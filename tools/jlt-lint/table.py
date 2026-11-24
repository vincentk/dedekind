"""The linter on the library, as a LaTeX table for the paper's appendix.

Reads the scope and a jlt-lint report (the driver's own lines, from
build/jlt-lint/report.txt or a CI log) on standard input, and prints one row
per rule: the library files where the rule is a gate (an exact count, zero or
not: a gate pins what a file has, so that it cannot grow unseen), the files
where it is reported, how many of all those have findings, and how many
findings there are, read from gate and report lines alike.  A reported file
with no finding is not printed by the driver, so it is counted as clean.

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
        for f in glob.glob(str(ROOT / pattern), recursive=True):
            f = pathlib.Path(f).relative_to(ROOT).as_posix()
            if f.startswith(LIB):
                status[rule][f] = "report" if expected == "report" else "gate"
    return status


def grade(rule):
    """The rule's grade, from its own file: Π rejecting, Σ affirming, Δ both,
    "—" not a test (README, Choices the linter makes)."""
    for line in (ROOT / "tools/jlt-lint/rules" / f"{rule}.query").read_text().splitlines():
        if line.startswith("# grade: "):
            g = line.split()[2].rstrip(",")
            return "$\\" + {"Π": "Pi", "Σ": "Sigma", "Δ": "Delta"}[g] + "$" if g in "ΠΣΔ" else "---"
    sys.exit(f"table.py: {rule}.query states no grade")


def findings(text):
    found = collections.defaultdict(dict)
    for rule, f, n in re.findall(r"jlt-lint: (?:gate|report) +(\S+) (" + re.escape(LIB) + r"\S+): (\d+)", text):
        found[rule][f] = int(n)
    return found


def main():
    # --check FILE: print nothing, fail if FILE differs from the table this
    # report gives, so that the paper's table cannot drift from the code.
    check = sys.argv[sys.argv.index("--check") + 1] if "--check" in sys.argv else None
    # --report FILE: the report from a file rather than standard input, for
    # the build, whose commands do not redirect.
    report = sys.argv[sys.argv.index("--report") + 1] if "--report" in sys.argv else None
    text = pathlib.Path(report).read_text() if report else sys.stdin.read()
    if check:
        import contextlib
        import io
        buffer = io.StringIO()
        with contextlib.redirect_stdout(buffer):
            table(text)
        if pathlib.Path(check).read_text() != buffer.getvalue():
            sys.exit(f"table.py: {check} differs from the table the report gives; "
                     "regenerate it: python3 tools/jlt-lint/table.py < build/jlt-lint/report.txt")
        print(f"table.py: {check} matches")
        return
    table(text)


def table(text):
    status, found = scope(), findings(text)
    gated = {f for r in status.values() for f, s in r.items() if s == "gate"}
    seen = set(re.findall(r"jlt-lint: gate +\S+ (" + re.escape(LIB) + r"\S+): ", text))
    if not gated <= seen:
        sys.exit(f"table.py: the report lacks the gate lines of {len(gated - seen)} library files; "
                 "a whole, successful run is needed, not a part or a failed one")
    library = sorted(pathlib.Path(f).relative_to(ROOT).as_posix()
                     for f in glob.glob(str(ROOT / LIB / "**/*.cppm"), recursive=True))
    linted = sorted({f for r in status.values() for f in r})
    lines = sum(len((ROOT / f).read_text().splitlines()) for f in linted)
    print(f"% {len(linted)} of {len(library)} library modules, {lines} lines")
    groups = [
        ("Refused, outside Jlt: Rule 1, substitution", lambda r: r.startswith("rule1-")),
        ("Refused, outside Jlt: Rule 2, abstraction", lambda r: r.startswith("rule2-")),
        ("Refused, outside Jlt: Rule 3, descent", lambda r: r.startswith("rule3-")),
        ("Affirmed: the second stage, admission by type", lambda r: r == "admitted"),
        ("Listed, taken on trust", lambda r: r == "axioms"),
    ]
    for i, (title, member) in enumerate(groups):
        if i:
            print("\\midrule")
        print(f"\\multicolumn{{6}}{{@{{}}l}}{{\\emph{{{title}}}}} \\\\")
        for rule in sorted(r for r in status if member(r)):
            gate = sum(1 for s in status[rule].values() if s == "gate")
            report = sum(1 for s in status[rule].values() if s == "report")
            hit = {f: n for f, n in found.get(rule, {}).items() if n > 0}
            print(f"\\quad\\texttt{{{rule}}} & {grade(rule)} & {gate} & {report} & {len(hit)} & {sum(hit.values())} \\\\")


if __name__ == "__main__":
    main()
