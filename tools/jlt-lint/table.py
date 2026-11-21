"""The linter on the library, as a LaTeX table for the paper's appendix.

Reads the scope and a jlt-lint report (the driver's own lines, from
build/jlt-lint/report.txt or a CI log) on standard input, and prints one row
per rule: the library files where the rule is a gate (and so has no finding),
the files where it is reported, how many of those have findings, and how many
findings there are.  A reported file with no finding is not printed by the
driver, so it is counted as clean.

    python3 tools/jlt-lint/table.py < build/jlt-lint/report.txt
"""
import collections
import glob
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
LIB = "src/main/modules/dedekind/"


def scope():
    status = collections.defaultdict(dict)
    for line in (ROOT / "tools/jlt-lint/scope").read_text().splitlines():
        if not line.strip() or line.startswith("#"):
            continue
        rule, pattern, _, expected = line.split()
        for f in glob.glob(str(ROOT / pattern)):
            f = pathlib.Path(f).relative_to(ROOT).as_posix()
            if f.startswith(LIB):
                status[rule][f] = "report" if expected == "report" else "gate"
    return status


def findings(text):
    found = collections.defaultdict(dict)
    for rule, f, n in re.findall(r"jlt-lint: report (\S+) (" + re.escape(LIB) + r"\S+): (\d+)", text):
        found[rule][f] = int(n)
    return found


def main():
    status, found = scope(), findings(sys.stdin.read())
    library = sorted(pathlib.Path(f).relative_to(ROOT).as_posix()
                     for f in glob.glob(str(ROOT / LIB / "**/*.cppm"), recursive=True))
    linted = sorted({f for r in status.values() for f in r})
    lines = sum(len((ROOT / f).read_text().splitlines()) for f in linted)
    print(f"% {len(linted)} of {len(library)} library modules, {lines} lines")
    for rule in sorted(status, key=lambda r: (not r.startswith("rule"), r)):
        if rule == "inventory":
            continue
        gate = sum(1 for s in status[rule].values() if s == "gate")
        report = sum(1 for s in status[rule].values() if s == "report")
        hit = {f: n for f, n in found.get(rule, {}).items() if n > 0}
        number, _, name = rule.partition("-") if rule.startswith("rule") else ("", "", rule)
        print(f"{number.removeprefix('rule')} & \\texttt{{{name}}} & {gate} & {report} & {len(hit)} & {sum(hit.values())} \\\\")


if __name__ == "__main__":
    main()
