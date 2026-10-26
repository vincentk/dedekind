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

Usage: jlt_lint.py [CLANG_QUERY] [BUILD_DIR] [JLT_TOOL]
"""

import collections
import concurrent.futures
import glob
import os
import pathlib
import re
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


def run_tool(tool, build, path, namespace, checks):
    """The checks of jlt-lint on one file, over one parse: locations per
    check, or None and the output on an error."""
    args = [str(tool), f"--check={','.join(checks)}", f"--scope={namespace}"]
    if path.startswith("tools/jlt-lint/fixtures/"):
        args += [str(ROOT / path), "--", "-std=c++23", "-xc++"]
    else:
        args += ["-p", str(build), *UNDO_FOR_CHECK, str(ROOT / path)]
    p = subprocess.run(args, capture_output=True, text=True)
    if p.returncode != 0:
        return None, p.stdout + p.stderr
    lines = [l for l in p.stdout.splitlines() if l.strip()]
    if len(checks) == 1:  # one check prints no section header
        return {checks[0]: lines}, p.stdout + p.stderr
    found = {c: [] for c in checks}
    seen = []
    section = None
    for l in lines:
        if l.startswith("== "):
            section = l[3:].strip()
            if section not in found or section in seen:
                return None, p.stdout + p.stderr
            seen.append(section)
        elif section is None:
            return None, p.stdout + p.stderr
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
        args = [str(ROOT / path), "--", "-std=c++23", "-xc++"]
    else:
        args = ["-p", str(build), *UNDO_FOR_CHECK, str(ROOT / path)]
    p = subprocess.run([query] + [a for s in script for a in ("-c", s)] + args,
                       capture_output=True, text=True)
    out = p.stdout + p.stderr
    # clang-query prints a count after each match command, in order; the
    # findings of match k are the notes between count k-1 and count k.
    counts = [int(c) for c in COUNT.findall(out)]
    if p.returncode != 0 or len(counts) != len(owner) + 1:
        return None, None, out
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


def main():
    query = sys.argv[1] if len(sys.argv) > 1 else "clang-query"
    build = pathlib.Path(sys.argv[2] if len(sys.argv) > 2 else ROOT / "build")
    tool = pathlib.Path(sys.argv[3] if len(sys.argv) > 3 else build / "jlt-lint" / "jlt-lint")
    lines = list(read_scope())
    if any(not f.startswith("tools/") for _, f, _, _ in lines) and \
            not (build / "compile_commands.json").exists():
        sys.exit(f"jlt-lint: no {build}/compile_commands.json; build first")

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

    failures = 0
    per_rule = collections.defaultdict(lambda: {"gated": 0, "reported": 0, "findings": 0})
    for ((f, ns), entries), (counts, where, out) in zip(ordered, results):
        if counts is None:
            print(f"jlt-lint: {f}: does not parse, a matcher did not run, or the scope is absent", file=sys.stderr)
            print("\n".join(out.splitlines()[:30]), file=sys.stderr)
            failures += 1
            continue
        for rule, expected in entries:
            n, row = counts[rule], per_rule[rule]
            if expected == "report":
                row["reported"] += 1
                row["findings"] += n
                if n:
                    print(f"jlt-lint: report {rule} {f}: {n}")
                    for loc in where[rule]:
                        print(f"  {pathlib.Path(loc).relative_to(ROOT) if loc.startswith(str(ROOT)) else loc}")
                continue
            row["gated"] += 1
            ok = n == int(expected)
            print(f"jlt-lint: gate   {rule} {f}: {n}, expected {expected}{'' if ok else '  FAIL'}")
            if not ok:
                failures += 1
                for loc in where[rule]:
                    print(f"  {loc}")

    print("\njlt-lint: coverage")
    print(f"  {'rule':<34} {'files gated':>12} {'files reported':>15} {'findings reported':>18}")
    for rule in sorted(per_rule):
        r = per_rule[rule]
        print(f"  {rule:<34} {r['gated']:>12} {r['reported']:>15} {r['findings']:>18}")
    print(f"jlt-lint: {failures} failure(s)")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
