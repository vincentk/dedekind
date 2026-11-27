#!/usr/bin/env python3
"""Run the Jlt linter over tools/jlt-lint/scope (#1060).

A scope line is ``<rule> <file-glob> <namespace> <expected|report>``.  Every
file a glob names is parsed once, by the jlt-lint binary, which runs all of
the file's rules on that one parse, each rule matching beneath ``namespace``
(bound as ``scope``).  A number is a gate: the file must have exactly that
many findings.  ``report`` is counted and printed, never failed.  A fixture
(under tools/jlt-lint/fixtures) is standalone and is parsed with explicit
flags; every other file through the build's compilation database.

A rule file holds clang-query matchers (``match`` lines, and at most one
``set traversal``), which the binary parses with clang's dynamic matcher
parser, as clang-query would; or it names a check of the binary
(``tool <check>``), the checks that need code rather than a pattern.  The
binary prints one finding per line as ``path:line:col``, each rule's findings
in its own section, after the scope namespace's own (none: the scope is
absent, and the file fails rather than count zeros).

Files are linted in parallel, ``NCPU`` at a time (default: the number of
cores); the report is printed in file order, as if they had run one by one.

A scope line belongs to a part: the layer its path names
(``src/main/modules/dedekind/<layer>/`` or ``src/test/cpp/modules/dedekind/<layer>/``),
``fixtures`` otherwise.  ``--part`` lints one part, so the build can lint a
layer as soon as that layer is built, and ``--out`` keeps its findings;
``--join`` reads them back, checks that every part of the scope ran, and
prints the report of the whole scope, as one run would have.

Usage: jlt_lint.py [BUILD_DIR] [JLT_TOOL] [--part P --out DIR]
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
import time

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parent.parent
# The linter judges the code as written, not CI's coverage build: there the
# coverage bridges compile without constexpr on purpose, so their macro is
# undone for the parse (the prebuilt modules are unaffected).
UNDO_FOR_CHECK = ["--extra-arg=-UDEDEKIND_RUNTIME_COVERAGE_BRIDGES"]
# Clang's own headers (stdarg.h, …) as the compiler that built the tree finds
# them: a tool resolves them relative to its own binary, which on macOS with
# Homebrew's LLVM is not next to them.  Set by main() from the database.
RESOURCE_DIR = []
# How much of each file the matchers walk; set from --scan (see main).
SCAN = "shallow"
# Each file's definitions with their levels, "<file> <namespace>" to rows of
# levels.classify; written beside the report (summary.py reads it).
LEVELS = {}
# The longest one file may take, all its rules over one parse and the second
# stage's compiles (JLT_LINT_FILE_TIMEOUT seconds, default twenty minutes).
FILE_TIMEOUT = int(os.environ.get("JLT_LINT_FILE_TIMEOUT") or 1200)
LAYER = re.compile(r"^src/(?:main|test/cpp)/modules/dedekind/([^/]+)/")


def part_of(path):
    """The part a file belongs to: its layer, or ``fixtures``."""
    m = LAYER.match(path)
    return m.group(1) if m else "fixtures"


def read_scope(text=None, strict=True):
    """Yield (rule, file, namespace, expected) with globs expanded.  A line
    naming one file overrides a glob's line for the same rule, file and
    namespace, so a gate can take its own count while the glob stays the
    default for the rest (the ratchet edits one line per file)."""
    chosen = {}
    for rule, f, ns, expected, exact in _scope_lines(text, strict):
        key = (rule, f, ns)
        if key in chosen and chosen[key][1] and not exact:
            continue  # a file's own line beats a glob's
        if key in chosen and chosen[key][1] and exact:
            sys.exit(f"jlt-lint: scope: two lines name {rule} on {f} ({ns})")
        chosen[key] = (expected, exact)
    for (rule, f, ns), (expected, _) in chosen.items():
        yield rule, f, ns, expected


def _scope_lines(text=None, strict=True):
    """Yield (rule, file, namespace, expected, exact): exact when the line
    names the file itself rather than a glob.  `text` is a scope's text (the
    checked-in file by default); not `strict`, a line that names no file is
    skipped (another commit's scope, read against this tree)."""
    if text is None:
        text = (HERE / "scope").read_text()
    for n, line in enumerate(text.splitlines(), 1):
        line = line.split("#", 1)[0].split()
        if not line:
            continue
        if len(line) != 4:
            sys.exit(f"jlt-lint: scope:{n}: want <rule> <file-glob> <namespace> <expected|report>")
        rule, pattern, namespace, expected = line
        files = sorted(glob.glob(str(ROOT / pattern), recursive=True))
        if not files and expected == "report":
            # A control outside the tree (Catch2, under the build directory)
            # may be absent in another build or a layer-only run: nothing to
            # report.  A gate that names no file is still an error.
            print(f"jlt-lint: scope:{n}: {pattern} names no file, reported line skipped", file=sys.stderr)
            continue
        if not files and not strict:
            continue
        if not files:
            sys.exit(f"jlt-lint: scope:{n}: {pattern} names no file")
        exact = not any(ch in pattern for ch in "*?[")
        for f in files:
            yield rule, pathlib.Path(f).relative_to(ROOT).as_posix(), namespace, expected, exact


def tool_check(rule):
    """The jlt-lint check a rule names, or None for a rule of matchers."""
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


def run(tool, build, path, namespace, rules):
    """Parse one file once, with all its rules: (counts per rule, locations
    per rule, output), or None, None and the output on an error."""
    checks = [tool_check(r) for r in rules if tool_check(r) is not None]
    queries = [r for r in rules if tool_check(r) is None]
    args = [str(tool), f"--scope={namespace}",
            f"--archetypes={HERE / 'archetypes.hpp'}",
            # How much the matchers walk: shallow by default, deep on request
            # (README, "Choices the linter makes").
            f"--scan={SCAN}"]
    if checks:
        args.append(f"--check={','.join(checks)}")
    if queries:
        args.append(f"--query={','.join(str(HERE / 'rules' / f'{r}.query') for r in queries)}")
    if path.startswith("tools/jlt-lint/fixtures/"):
        args += [str(ROOT / path), "--", "-std=c++23", "-xc++", *RESOURCE_DIR]
    else:
        args += ["-p", str(build), *UNDO_FOR_CHECK,
                 *(f"--extra-arg={a}" for a in RESOURCE_DIR), str(ROOT / path)]
    try:
        p = subprocess.run(args, capture_output=True, text=True, timeout=FILE_TIMEOUT)
    except subprocess.TimeoutExpired:
        # A file that takes longer has a problem to fix: it fails by name
        # instead of holding the whole lint until the job's own limit.
        return None, None, f"jlt-lint: {path}: timed out after {FILE_TIMEOUT} s\n"
    out = p.stdout + p.stderr
    if p.returncode != 0:
        return None, None, exit_status(p) + out
    # The sections: the scope probe's, then one per rule file and per check.
    wanted = ["scope", *(f"query {r}" for r in queries), *checks]
    found = {w: [] for w in wanted}
    seen = []
    section = None
    for l in p.stdout.splitlines():
        if not l.strip():
            continue
        if l.startswith("== "):
            section = l[3:].strip()
            if section not in found:
                return None, None, f"jlt-lint: unrequested section {section}\n" + out
            if section in seen:
                return None, None, f"jlt-lint: section {section} twice (one parse per file?)\n" + out
            seen.append(section)
        elif section is None:
            return None, None, "jlt-lint: a finding before any section\n" + out
        else:
            found[section].append(l)
    # Every rule must have reported, even with no findings: a missing
    # section is a rule that did not run, not a zero.
    if sorted(seen) != sorted(wanted):
        return None, None, f"jlt-lint: sections {seen}, wanted {wanted}\n" + out
    if not found["scope"]:
        return "absent", None, f"namespace {namespace} does not occur in {path}"
    # A match whose root has no location ("-") counts, but is not listed.
    counts, where = {}, {}
    for r in rules:
        lines = found[f"query {r}" if tool_check(r) is None else tool_check(r)]
        counts[r] = len(lines)
        where[r] = [l for l in lines if l != "-"]
    return counts, where, out


def set_resource_dir(entries):
    """The compiler's own headers, as the build's compiler finds them: a tool
    linked against another LLVM (Homebrew's, on macOS) does not find them by
    itself, and a standalone file then fails to parse."""
    compiler = (entries[0].get("arguments") or shlex.split(entries[0]["command"]))[0] if entries else None
    if compiler:
        r = subprocess.run([compiler, "-print-resource-dir"], capture_output=True, text=True)
        if r.returncode == 0 and r.stdout.strip():
            RESOURCE_DIR[:] = [f"-resource-dir={r.stdout.strip()}"]


def header_entry(entries, header):
    """A compilation database entry for a header judged as a file of its own
    (a library's interface, which no translation unit of the build names):
    the flags of the source file nearest to it in the tree, with the header in
    the source's place, read as C++ and only parsed."""
    def shared(e):
        a, b = pathlib.Path(e["file"]).parts, pathlib.Path(header).parts
        return next((i for i, (x, y) in enumerate(zip(a, b)) if x != y), min(len(a), len(b)))
    near = max(entries, key=shared, default=None)
    if near is None:
        return None
    args = near.get("arguments") or shlex.split(near["command"])
    out, skip = [], False
    for a in args:
        if skip:
            skip = False
        elif a in ("-o", "-MF", "-MT", "-MQ"):
            skip = True
        elif a in ("-c", "-MD", "-MMD") or a == near["file"] or a.endswith("/" + pathlib.Path(near["file"]).name):
            continue
        else:
            out.append(a)
    # The control from the standard library is libc++ wherever the build runs
    # (stage_stdlib.py), so its headers are read against libc++.
    if "/_deps/stdlib-ranges/" in str(header):
        out = [a for a in out if not a.startswith("-stdlib=")] + ["-stdlib=libc++"]
    return {"directory": near["directory"], "file": header,
            "arguments": out + ["-xc++", "-fsyntax-only", header]}


def one_entry_per_file(build, part, files=()):
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
    set_resource_dir(entries)
    # A header in scope that the build compiles only through its includers.
    for f in files:
        f = str(ROOT / f)
        if f not in kept and f.endswith((".hpp", ".h")):
            e = header_entry([e for _, e in kept.values()], f)
            if e:
                kept[f] = (False, e)
    db = build / "jlt-lint-db" / part  # a directory per part: parts run at once
    db.mkdir(parents=True, exist_ok=True)
    (db / "compile_commands.json").write_text(json.dumps([e for _, e in kept.values()], indent=1))
    return db


def lint_part(build, tool, part):
    """Lint the scope lines of one part (all of them for None); return one
    block per (file, namespace): its report lines, failures and coverage rows."""
    lines = [l for l in read_scope() if part is None or part_of(l[1]) == part]
    if any(not f.startswith("tools/") for _, f, _, _ in lines) and \
            not (build / "compile_commands.json").exists():
        sys.exit(f"jlt-lint: no {build}/compile_commands.json; build first")
    if (build / "compile_commands.json").exists():
        build = one_entry_per_file(build, part or "all", sorted({f for _, f, _, _ in lines}))

    groups = collections.defaultdict(list)
    for rule, f, ns, expected in lines:
        groups[(f, ns)].append((rule, expected))

    costs, phases = [], {}

    def lint(group):
        """One file, all its rules over one parse: (counts, locations,
        output) per rule, counts None on an error.  Its wall time and the
        second stage's cost, as the tool reports it, go to the timing table."""
        (f, ns), entries = group
        started = time.monotonic()
        result = run(tool, build, f, ns, [r for r, _ in entries])
        # Each definition's level, by levels.py's own classify, from the
        # findings this parse already gave: the same computation as the
        # paper's Table 6, for every file that runs the definitions check.
        counts, where = result[0], result[1]
        if isinstance(counts, dict) and where and "definitions" in where:
            import admission
            import levels
            LEVELS[f"{f} {ns}"] = [list(r) for r in levels.classify(where, admission.admission())]
        m = re.search(r"jlt-lint-cost: templates (\d+) compiles (\d+) seconds ([\d.]+)", result[2] or "")
        costs.append((time.monotonic() - started, f, *(m.groups() if m else ("-", "-", "-"))))
        phases[f] = {name: float(secs) for name, secs in
                     re.findall(r"jlt-lint-phase: (.+?) ([\d.]+)$", result[2] or "", re.M)}
        # Progress as it happens, with the file's own phases, so a cancelled
        # or timed-out job still shows which files finished, which did not,
        # and where the finished ones spent their time.
        top = sorted(((n, s) for n, s in phases[f].items() if not n.startswith("matcher ")),
                     key=lambda kv: -kv[1])[:4]
        rules = sorted(((n[8:], s) for n, s in phases[f].items() if n.startswith("matcher ")),
                       key=lambda kv: -kv[1])[:2]
        print(f"jlt-lint: done {costs[-1][0]:7.1f} s  {f}"
              + ("" if result[0] is not None else "  (failed)")
              + ("  [" + ", ".join(f"{n} {s:.1f}" for n, s in top)
                 + ("; slowest rules: " + ", ".join(f"{n} {s:.1f}" for n, s in rules) if rules else "")
                 + "]" if top else ""),
              file=sys.stderr, flush=True)
        return result

    workers = int(os.environ.get("NCPU") or os.cpu_count() or 1)
    ordered = sorted(groups.items())
    started = time.monotonic()
    with concurrent.futures.ThreadPoolExecutor(max_workers=workers) as pool:
        results = list(pool.map(lint, ordered))
    # Where the time goes: the slowest files, and the second stage's share.
    total = sum(c[0] for c in costs)
    compiled = sum(int(c[3]) for c in costs if c[3] != "-")
    print(f"jlt-lint: timing, {len(costs)} files on {workers} workers: {time.monotonic() - started:.0f} s wall, "
          f"{total:.0f} s summed, {compiled} second-stage compiles", file=sys.stderr)
    for secs, f, k, n, s in sorted(costs, reverse=True)[:20]:
        print(f"jlt-lint: timing {secs:7.1f} s  {f}  (second stage: {k} templates, {n} compiles, {s} s)",
              file=sys.stderr)
    # Each phase summed over the files (the parse, every rule's matchers,
    # every check), and the slowest files phase by phase.
    summed = collections.Counter()
    for per in phases.values():
        summed.update(per)
    for name, secs in summed.most_common(15):
        print(f"jlt-lint: phase  {secs:8.1f} s summed  {name}", file=sys.stderr)
    for secs, f, *_ in sorted(costs, reverse=True)[:5]:
        top = sorted(phases.get(f, {}).items(), key=lambda kv: -kv[1])[:6]
        print(f"jlt-lint: phases of {f}: " + ", ".join(f"{n} {s:.1f} s" for n, s in top), file=sys.stderr)
    return [block(f, ns, entries, *result)
            for ((f, ns), entries), result in zip(ordered, results)]


def block(f, ns, entries, counts, where, out):
    """One file's part of the report: its lines, failures and coverage rows."""
    text, failures = [], 0
    rows = {}
    # A namespace absent from a file is an error where a line gates it, and
    # nothing to report where every line only reports (a control such as
    # Catch2, whose glob takes in a file or two without its namespace).
    if counts == "absent" and all(e == "report" for _, e in entries):
        text.append(f"jlt-lint: {f}: namespace {ns} absent, nothing to report")
        return {"key": [f, ns], "text": text, "failures": 0, "rows": rows}
    if counts is None or counts == "absent":
        text.append(f"jlt-lint: {f}: does not parse, a rule did not run, or the scope is absent")
        lines = out.splitlines()
        # What the compiler said, wherever it is in the output, and both ends.
        said = [l for l in lines if "error:" in l or "fatal" in l or "Assertion" in l][:20]
        text += lines[:10] + said + lines[-10:]
        # A file that only reports (a control, read as a header on its own
        # with a neighbour's flags) is named and left out, not a failure
        # here: on Linux the summary table's comparison then fails, with
        # this file's diagnostics above it, since the row no longer counts
        # the whole control.
        if all(e == "report" for _, e in entries):
            text.append(f"jlt-lint: {f}: reported only, left out of the counts")
            return {"key": [f, ns], "text": text, "failures": 0, "rows": rows}
        return {"key": [f, ns], "text": text, "failures": 1, "rows": rows}
    for rule, expected in entries:
        n = counts[rule]
        row = rows.setdefault(rule, {"gated": 0, "reported": 0, "findings": 0})
        if expected == "report":
            row["reported"] += 1
            row["findings"] += n
            if n and rule == "definitions":
                # Every declaration is one: the count here, the levels in
                # <out>/<part>-levels.json (summary.py).
                text.append(f"jlt-lint: report {rule} {f}: {n}")
                continue
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
    args.add_argument("build", nargs="?", type=pathlib.Path, default=ROOT / "build")
    args.add_argument("tool", nargs="?", type=pathlib.Path)
    args.add_argument("--part", help="lint only this part: a layer, or fixtures")
    args.add_argument("--out", type=pathlib.Path,
                      help="keep the findings in OUT/<part>.json and the report in OUT/<part>.txt")
    args.add_argument("--join", type=pathlib.Path, metavar="DIR",
                      help="the report of the whole scope from DIR's parts, into DIR/../report.txt")
    args.add_argument("--scan", choices=["shallow", "deep"],
                      default=os.environ.get("JLT_LINT_SCAN") or "shallow",
                      help="how much of each file the rules walk: shallow (default), the "
                           "declarations written in the file, a rule whose evidence leaves the "
                           "file following it there; deep, everything the file includes or "
                           "imports, to test that choice. Default from JLT_LINT_SCAN.")
    args.add_argument("--diagnostic", nargs="?", type=int, const=3, metavar="N",
                      help="after the report, every reason a definition is refused or undecided, "
                           "ranked by what fixing it alone would show, with N example "
                           "definitions each (default 3); see summary.py")
    a = args.parse_args()
    global SCAN
    SCAN = a.scan
    if a.join:
        text, failures = report(join(a.join))
        (a.join.parent / "report.txt").write_text(text)
    else:
        blocks = lint_part(a.build, a.tool or a.build / "jlt-lint" / "jlt-lint", a.part)
        text, failures = report(blocks)
        if a.out:
            name = a.part or "all"
            a.out.mkdir(parents=True, exist_ok=True)
            (a.out / f"{name}.json").write_text(json.dumps(blocks))
            (a.out / f"{name}-levels.json").write_text(json.dumps(LEVELS))
            (a.out / f"{name}.txt").write_text(text)
    sys.stdout.write(text)
    if a.diagnostic is not None and not a.join:
        import summary  # noqa: E402, beside this file
        sys.stdout.write("\njlt-lint: --diagnostic\n" + summary.diagnostic(LEVELS, examples=a.diagnostic))
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
