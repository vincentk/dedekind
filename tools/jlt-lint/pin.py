"""Pin reported counts as gates: the first half of a ratchet.

Reads a jlt-lint report (build/jlt-lint/report.txt or a CI log) on standard
input.  For the given rules, every `report` line of the scope that names
library files becomes a gate at 0, and every file with findings gets a line
of its own with its count (a file's own line overrides a glob's).  Without
--write it prints the lines it would add; with --write it rewrites the scope.

    python3 tools/jlt-lint/pin.py RULE... < build/jlt-lint/report.txt
    python3 tools/jlt-lint/pin.py --write RULE... < report.txt

A reported file with no finding is not printed by the driver and is pinned
at 0, so the report must be of a whole, successful run: a partial one would
pin missing files at 0, and the next build's gates would show it.
"""
import pathlib
import re
import sys

HERE = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import jlt_lint  # noqa: E402

LIB = "src/main/modules/dedekind/"


def main():
    args = sys.argv[1:]
    write = "--write" in args
    rules = [a for a in args if not a.startswith("--")]
    if not rules:
        sys.exit(__doc__)
    text = sys.stdin.read()
    if "jlt-lint: " not in text:
        sys.exit("pin.py: no jlt-lint lines on standard input")
    counts = {}
    for rule, f, n in re.findall(r"jlt-lint: (?:gate|report) +(\S+) (" + re.escape(LIB) + r"\S+): (\d+)", text):
        counts[(rule, f)] = int(n)
    rows = [r for r in jlt_lint.read_scope() if r[0] in rules and r[1].startswith(LIB)]
    reported = sorted({(r[0], r[1], r[2]) for r in rows if r[3] == "report"})
    new = [f"{rule:<28} {f}  {ns}  {counts[(rule, f)]}"
           for rule, f, ns in reported if counts.get((rule, f), 0) > 0]
    if not write:
        print("\n".join(new))
        return
    scope = HERE / "scope"
    lines = scope.read_text().splitlines()
    out = []
    for line in lines:
        parts = line.split()
        if (len(parts) == 4 and parts[0] in rules and parts[1].startswith(LIB)
                and parts[3] == "report"):
            out.append(line[: line.rstrip().rfind("report")] + "0")
        else:
            out.append(line)
    out += new
    scope.write_text("\n".join(out) + "\n")
    print(f"pin.py: {sum(1 for r in reported)} reported (rule, file) pairs pinned, {len(new)} with findings")


if __name__ == "__main__":
    main()
