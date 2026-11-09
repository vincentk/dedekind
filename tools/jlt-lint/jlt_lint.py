#!/usr/bin/env python3
"""Run the Jlt linter over tools/jlt-lint/scope (#1060).

A scope line is ``<rule> <file-glob> <namespace> <expected|report>``.  Every
file a glob names is parsed once by clang-query, with all its rules, each rule
matching beneath ``namespace`` (bound as ``scope``).  A number is a gate: the
file must have exactly that many findings.  ``report`` is counted and printed,
never failed.  A fixture (under tools/jlt-lint/fixtures) is standalone and is
parsed with explicit flags; every other file through the build's compilation
database.

A rule file may instead name a check of jlt-lint (``tool <check>``), the
checks that need code rather than a pattern; the tool prints one finding per
line as ``path:line:col``.  All of a file's checks run in one jlt-lint
process, over one parse, each check's findings in its own section.

Files are linted in parallel, ``NCPU`` at a time (default: the number of
cores); the report is printed in file order, as if they had run one by one.

A scope line belongs to a part: the layer its path names
(``src/main/modules/dedekind/<layer>/`` or ``src/test/cpp/modules/dedekind/<layer>/``),
``fixtures`` otherwise.  ``--part`` lints one part, so the build can lint a
layer as soon as that layer is built, and ``--out`` keeps its findings;
``--join`` reads them back, checks that every part of the scope ran, and
prints the report of the whole scope, as one run would have.

Usage: jlt_lint.py [CLANG_QUERY] [BUILD_DIR] [JLT_TOOL] [--part P --out DIR]
       jlt_lint.py --join DIR
"""

import argparse

import collections
import concurrent.futures
import glob
import json
import os
import pathlib
import re
import shlex
import signal
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parent.parent
COUNT = re.compile(r"^(\d+) match(?:es)?\.", re.M)
BINDS = re.compile(r'^(\S+:\d+:\d+): note: "root" binds here', re.M)
# The linter judges the code as written, not CI's coverage build: there the
# coverage bridges compile without constexpr on purpose, so their macro is
# undone for the parse (the prebuilt modules are unaffected).
UNDO_FOR_CHECK = ["--extra-arg=-UDEDEKIND_RUNTIME_COVERAGE_BRIDGES"]
# Clang's own headers (stdarg.h, …) as the compiler that built the tree finds
# them: a tool resolves them relative to its own binary, which on macOS with
# Homebrew's LLVM is not next to them.  Set by main() from the database.
RESOURCE_DIR = []
LAYER = re.compile(r"^src/(?:main|test/cpp)/modules/dedekind/([^/]+)/")


def part_of(path):
    """The part a file belongs to: its layer, or ``fixtures``."""
    m = LAYER.match(path)
    return m.group(1) if m else "fixtures"


def read_scope():
    """Yield (rule, file, namespace, expected) with globs expanded."""
    for n, line in enumerate((HERE / "scope").read_text().splitlines(), 1):
        line = line.split("#", 1)[0].split()
        if not line:
            continue
        if len(line) != 4:
            sys.exit(f"jlt-lint: scope:{n}: want <rule> <file-glob> <namespace> <expected|report>")
        rule, pattern, namespace, expected = line
        files = sorted(glob.glob(str(ROOT / pattern)))
        if not files:
            sys.exit(f"jlt-lint: scope:{n}: {pattern} names no file")
        for f in files:
            yield rule, pathlib.Path(f).relative_to(ROOT).as_posix(), namespace, expected


DEFAULT_TRAVERSAL = "set traversal IgnoreUnlessSpelledInSource"


def tool_check(rule):
    """The jlt-lint check a rule names, or None for a clang-query rule."""
    for l in (HERE / "rules" / f"{rule}.query").read_text().splitlines():
        if l.startswith("tool "):
            return l.split(None, 1)[1].strip()
    return None


def exit_status(p):
    """How a child ended, when it did not end well: its exit code, or the
    signal that killed it (a negative return code), so a crash is not read as
    a parse error."""
    if p.returncode == 0:
        return ""
    if p.returncode < 0:
        try:  # a real-time signal (SIGRTMIN + k) has no name
            name = signal.Signals(-p.returncode).name
        except ValueError:
            name = str(-p.returncode)
        return f"killed by signal {name}\n"
    return f"exit code {p.returncode}\n"


def run_tool(tool, build, path, namespace, checks):
    """The checks of jlt-lint on one file, over one parse: locations per
    check, or None and the output on an error."""
    args = [str(tool), f"--check={','.join(checks)}", f"--scope={namespace}"]
    if path.startswith("tools/jlt-lint/fixtures/"):
        args += [str(ROOT / path), "--", "-std=c++23", "-xc++", *RESOURCE_DIR]
    else:
        args += ["-p", str(build), *UNDO_FOR_CHECK,
                 *(f"--extra-arg={a}" for a in RESOURCE_DIR), str(ROOT / path)]
    p = subprocess.run(args, capture_output=True, text=True)
    if p.returncode != 0:
        return None, exit_status(p) + p.stdout + p.stderr
    lines = [l for l in p.stdout.splitlines() if l.strip()]
    if len(checks) == 1:  # one check prints no section header
        return {checks[0]: lines}, p.stdout + p.stderr
    found = {c: [] for c in checks}
    seen = []
    section = None
    for l in lines:
        if l.startswith("== "):
            section = l[3:].strip()
            if section not in found:
                return None, f"jlt-lint: unrequested section {section}\n" + p.stdout + p.stderr
            if section in seen:
                return None, f"jlt-lint: section {section} twice (one parse per file?)\n" + p.stdout + p.stderr
            seen.append(section)
        elif section is None:
            return None, "jlt-lint: a finding before any section\n" + p.stdout + p.stderr
        else:
            found[section].append(l)
    # Every check must have reported, even with no findings: a missing
    # section is a check that did not run, not a zero.
    if sorted(seen) != sorted(checks):
        return None, f"jlt-lint: sections {seen}, wanted {checks}\n" + p.stdout + p.stderr
    return found, p.stdout + p.stderr


def matchers(rule):
    """The rule's match lines; its findings are the sum of their counts (a
    rule may need one per kind of syntax node, e.g. a call and a member).  A
    rule may name its own traversal with a `set traversal` line (one that
    needs implicit nodes, such as a closure's fields); it is restored after."""
    text = (HERE / "rules" / f"{rule}.query").read_text().splitlines()
    lines = [l for l in text if l.startswith("match ")]
    if not lines:
        sys.exit(f"jlt-lint: rules/{rule}.query: want at least one match line")
    own = [l for l in text if l.startswith("set traversal ")]
    return lines, own[0] if own else None


def run(query, build, path, namespace, rules):
    """Parse one file once; return (counts per rule, locations per rule) or an error."""
    script = ["set output diag",
              DEFAULT_TRAVERSAL,
              f'let scope namespaceDecl(hasName("{namespace}"))']
    # The probe runs first: a scope that names no namespace in this file
    # would make every rule count zero, so its absence is a failure.
    script.append("match namespaceDecl(hasName(\"%s\"), isExpansionInMainFile())" % namespace)
    owner = []  # the rule each match line belongs to, in script order
    for r in rules:
        ms, traversal = matchers(r)
        if traversal:
            script.append(traversal)
        for m in ms:
            script.append(m)
            owner.append(r)
        if traversal:
            script.append(DEFAULT_TRAVERSAL)
    if path.startswith("tools/jlt-lint/fixtures/"):
        args = [str(ROOT / path), "--", "-std=c++23", "-xc++", *RESOURCE_DIR]
    else:
        args = ["-p", str(build), *UNDO_FOR_CHECK,
                *(f"--extra-arg={a}" for a in RESOURCE_DIR), str(ROOT / path)]
    p = subprocess.run([query] + [a for s in script for a in ("-c", s)] + args,
                       capture_output=True, text=True)
    out = p.stdout + p.stderr
    # clang-query prints a count after each match command, in order; the
    # findings of match k are the notes between count k-1 and count k.
    counts = [int(c) for c in COUNT.findall(out)]
    if p.returncode != 0 or len(counts) != len(owner) + 1:
        return None, None, exit_status(p) + out
    if counts[0] == 0:
        return None, None, f"namespace {namespace} does not occur in {path}"
    total = {r: 0 for r in rules}
    where = {r: [] for r in rules}
    marks = list(COUNT.finditer(out))
    start = marks[0].end()
    for r, n, m in zip(owner, counts[1:], marks[1:]):
        total[r] += n
        where[r] += BINDS.findall(out, start, m.start())
        start = m.end()
    return total, where, out


def one_entry_per_file(build, part):
    """A compilation database with one entry per file, in a directory of its
    own: a module partition has one per set of flags it is built with (CMake 4
    synthesises a BMI for every consumer with other flags, the Python
    bindings'), and a tool given several parses the file once each, which
    multiplies every count.  The library's own entry is kept, not a
    synthesised one (@synth), so the file is read as the library builds it."""
    entries = json.loads((build / "compile_commands.json").read_text())
    kept = {}
    for e in entries:
        synth = "@synth" in e.get("output", "") or "@synth" in e.get("command", "")
        old = kept.get(e["file"])
        if old is None or (old[0] and not synth):
            kept[e["file"]] = (synth, e)
    compiler = (entries[0].get("arguments") or shlex.split(entries[0]["command"]))[0] if entries else None
    if compiler:
        r = subprocess.run([compiler, "-print-resource-dir"], capture_output=True, text=True)
        if r.returncode == 0 and r.stdout.strip():
            RESOURCE_DIR[:] = [f"-resource-dir={r.stdout.strip()}"]
    db = build / "jlt-lint-db" / part  # a directory per part: parts run at once
    db.mkdir(parents=True, exist_ok=True)
    (db / "compile_commands.json").write_text(json.dumps([e for _, e in kept.values()], indent=1))
    return db


def lint_part(query, build, tool, part):
    """Lint the scope lines of one part (all of them for None); return one
    block per (file, namespace): its report lines, failures and coverage rows."""
    lines = [l for l in read_scope() if part is None or part_of(l[1]) == part]
    if any(not f.startswith("tools/") for _, f, _, _ in lines) and \
            not (build / "compile_commands.json").exists():
        sys.exit(f"jlt-lint: no {build}/compile_commands.json; build first")
    if (build / "compile_commands.json").exists():
        build = one_entry_per_file(build, part or "all")

    groups = collections.defaultdict(list)
    for rule, f, ns, expected in lines:
        groups[(f, ns)].append((rule, expected))

    def lint(group):
        """One file: its clang-query rules in one parse, its jlt-lint checks
        in another; (counts, locations, output) per rule, counts None on an
        error."""
        (f, ns), entries = group
        query_rules = [r for r, _ in entries if tool_check(r) is None]
        # Always through run(): its namespace probe guards tool-only groups too.
        counts, where, out = run(query, build, f, ns, query_rules)
        tool_rules = [r for r, _ in entries if tool_check(r) is not None]
        if counts is None or not tool_rules:
            return counts, where, out
        found, tout = run_tool(tool, build, f, ns, [tool_check(r) for r in tool_rules])
        if found is None:
            return None, None, "jlt-lint: " + tout
        for r in tool_rules:
            where[r] = found[tool_check(r)]
            counts[r] = len(where[r])
        return counts, where, out

    workers = int(os.environ.get("NCPU") or os.cpu_count() or 1)
    ordered = sorted(groups.items())
    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as pool:
        results = list(pool.map(lint, ordered))
    return [block(f, ns, entries, *result)
            for ((f, ns), entries), result in zip(ordered, results)]


def block(f, ns, entries, counts, where, out):
    """One file's part of the report: its lines, failures and coverage rows."""
    text, failures = [], 0
    rows = {}
    if counts is None:
        text.append(f"jlt-lint: {f}: does not parse, a matcher did not run, the scope is absent, or a tool failed")
        text += out.splitlines()[:30]
        return {"key": [f, ns], "text": text, "failures": 1, "rows": rows}
    for rule, expected in entries:
        n = counts[rule]
        row = rows.setdefault(rule, {"gated": 0, "reported": 0, "findings": 0})
        if expected == "report":
            row["reported"] += 1
            row["findings"] += n
            if n:
                text.append(f"jlt-lint: report {rule} {f}: {n}")
                text += [f"  {pathlib.Path(loc).relative_to(ROOT) if loc.startswith(str(ROOT)) else loc}"
                         for loc in where[rule]]
            continue
        row["gated"] += 1
        ok = n == int(expected)
        text.append(f"jlt-lint: gate   {rule} {f}: {n}, expected {expected}{'' if ok else '  FAIL'}")
        if not ok:
            failures += 1
            text += [f"  {loc}" for loc in where[rule]]
    return {"key": [f, ns], "text": text, "failures": failures, "rows": rows}


def report(blocks):
    """The report of some blocks, in file order, with the coverage table."""
    per_rule = collections.defaultdict(lambda: {"gated": 0, "reported": 0, "findings": 0})
    text, failures = [], 0
    for b in sorted(blocks, key=lambda b: b["key"]):
        text += b["text"]
        failures += b["failures"]
        for rule, row in b["rows"].items():
            for k, v in row.items():
                per_rule[rule][k] += v
    text.append("\njlt-lint: coverage")
    text.append(f"  {'rule':<34} {'files gated':>12} {'files reported':>15} {'findings reported':>18}")
    for rule in sorted(per_rule):
        r = per_rule[rule]
        text.append(f"  {rule:<34} {r['gated']:>12} {r['reported']:>15} {r['findings']:>18}")
    text.append(f"jlt-lint: {failures} failure(s)")
    return "\n".join(text) + "\n", failures


def join(out):
    """The report of the whole scope from its parts' findings, once every
    part of the scope has run."""
    wanted = {part_of(f) for _, f, _, _ in read_scope()}
    found = {p.stem: json.loads(p.read_text()) for p in out.glob("*.json")}
    missing = sorted(wanted - found.keys())
    if missing:
        sys.exit(f"jlt-lint: {out}: no findings of the part(s) {', '.join(missing)}")
    return [b for p in sorted(wanted) for b in found[p]]


def main():
    args = argparse.ArgumentParser(description=__doc__.split("\n", 1)[0])
    args.add_argument("query", nargs="?", default="clang-query")
    args.add_argument("build", nargs="?", type=pathlib.Path, default=ROOT / "build")
    args.add_argument("tool", nargs="?", type=pathlib.Path)
    args.add_argument("--part", help="lint only this part: a layer, or fixtures")
    args.add_argument("--out", type=pathlib.Path,
                      help="keep the findings in OUT/<part>.json and the report in OUT/<part>.txt")
    args.add_argument("--join", type=pathlib.Path, metavar="DIR",
                      help="the report of the whole scope from DIR's parts, into DIR/../report.txt")
    a = args.parse_args()
    if a.join:
        text, failures = report(join(a.join))
        (a.join.parent / "report.txt").write_text(text)
    else:
        blocks = lint_part(a.query, a.build, a.tool or a.build / "jlt-lint" / "jlt-lint", a.part)
        text, failures = report(blocks)
        if a.out:
            name = a.part or "all"
            a.out.mkdir(parents=True, exist_ok=True)
            (a.out / f"{name}.json").write_text(json.dumps(blocks))
            (a.out / f"{name}.txt").write_text(text)
    sys.stdout.write(text)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
