#!/usr/bin/env python3
"""Run the Jlt checker over tools/jlt-check/scope (#1060).

A scope line is ``<rule> <file-glob> <namespace> <expected|report>``.  Every
file a glob names is parsed once by clang-query, with all its rules, each rule
matching beneath ``namespace`` (bound as ``scope``).  A number is a gate: the
file must have exactly that many findings.  ``report`` is counted and printed,
never failed.  A fixture (under tools/jlt-check/fixtures) is standalone and is
parsed with explicit flags; every other file through the build's compilation
database.

Usage: jlt_check.py [CLANG_QUERY] [BUILD_DIR]
"""

import collections
import glob
import pathlib
import re
import subprocess
import sys

HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parent.parent
COUNT = re.compile(r"^(\d+) match(?:es)?\.", re.M)
BINDS = re.compile(r'^(\S+:\d+:\d+): note: "root" binds here', re.M)


def read_scope():
    """Yield (rule, file, namespace, expected) with globs expanded."""
    for n, line in enumerate((HERE / "scope").read_text().splitlines(), 1):
        line = line.split("#", 1)[0].split()
        if not line:
            continue
        if len(line) != 4:
            sys.exit(f"jlt-check: scope:{n}: want <rule> <file-glob> <namespace> <expected|report>")
        rule, pattern, namespace, expected = line
        files = sorted(glob.glob(str(ROOT / pattern)))
        if not files:
            sys.exit(f"jlt-check: scope:{n}: {pattern} names no file")
        for f in files:
            yield rule, pathlib.Path(f).relative_to(ROOT).as_posix(), namespace, expected


def matchers(rule):
    """The rule's match lines; its findings are the sum of their counts (a
    rule may need one per kind of syntax node, e.g. a call and a member)."""
    lines = [l for l in (HERE / "rules" / f"{rule}.query").read_text().splitlines()
             if l.startswith("match ")]
    if not lines:
        sys.exit(f"jlt-check: rules/{rule}.query: want at least one match line")
    return lines


def run(query, build, path, namespace, rules):
    """Parse one file once; return (counts per rule, locations per rule) or an error."""
    script = ["set output diag",
              "set traversal IgnoreUnlessSpelledInSource",
              f'let scope namespaceDecl(hasName("{namespace}"))']
    # The probe runs first: a scope that names no namespace in this file
    # would make every rule count zero, so its absence is a failure.
    script.append("match namespaceDecl(hasName(\"%s\"), isExpansionInMainFile())" % namespace)
    owner = []  # the rule each match line belongs to, in script order
    for r in rules:
        for m in matchers(r):
            script.append(m)
            owner.append(r)
    if path.startswith("tools/jlt-check/fixtures/"):
        args = [str(ROOT / path), "--", "-std=c++23", "-xc++"]
    else:
        args = ["-p", str(build), str(ROOT / path)]
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
    lines = list(read_scope())
    if any(not f.startswith("tools/") for _, f, _, _ in lines) and \
            not (build / "compile_commands.json").exists():
        sys.exit(f"jlt-check: no {build}/compile_commands.json; build first")

    groups = collections.defaultdict(list)
    for rule, f, ns, expected in lines:
        groups[(f, ns)].append((rule, expected))

    failures = 0
    per_rule = collections.defaultdict(lambda: {"gated": 0, "reported": 0, "findings": 0})
    for (f, ns), entries in sorted(groups.items()):
        rules = [r for r, _ in entries]
        counts, where, out = run(query, build, f, ns, rules)
        if counts is None:
            print(f"jlt-check: {f}: does not parse, a matcher did not run, or the scope is absent", file=sys.stderr)
            print("\n".join(out.splitlines()[:30]), file=sys.stderr)
            failures += 1
            continue
        for rule, expected in entries:
            n, row = counts[rule], per_rule[rule]
            if expected == "report":
                row["reported"] += 1
                row["findings"] += n
                if n:
                    print(f"jlt-check: report {rule} {f}: {n}")
                    for loc in where[rule]:
                        print(f"  {pathlib.Path(loc).relative_to(ROOT) if loc.startswith(str(ROOT)) else loc}")
                continue
            row["gated"] += 1
            ok = n == int(expected)
            print(f"jlt-check: gate   {rule} {f}: {n}, expected {expected}{'' if ok else '  FAIL'}")
            if not ok:
                failures += 1
                for loc in where[rule]:
                    print(f"  {loc}")

    print("\njlt-check: coverage")
    print(f"  {'rule':<34} {'files gated':>12} {'files reported':>15} {'findings reported':>18}")
    for rule in sorted(per_rule):
        r = per_rule[rule]
        print(f"  {rule:<34} {r['gated']:>12} {r['reported']:>15} {r['findings']:>18}")
    print(f"jlt-check: {failures} failure(s)")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
