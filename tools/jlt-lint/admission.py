"""Admission by syntax kind, summed: the admit side of jlt-lint, a first cut.

Reads a jlt-lint report (build/jlt-lint/report.txt or a CI log) on standard
input, takes the inventory each file prints (one line per AST node kind with
its count), classes every kind by admission.txt, and prints the occurrences
per class and the files with no construct refused or open, for the library and
for the paper's listings.  A kind missing from admission.txt counts as open.

    python3 tools/jlt-lint/admission.py < build/jlt-lint/report.txt
    python3 tools/jlt-lint/admission.py --latex < build/jlt-lint/report.txt
"""
import collections
import pathlib
import re
import sys

HERE = pathlib.Path(__file__).resolve().parent
CLASSES = ["kernel", "derived", "scaffolding", "refused", "open"]
SCOPES = [
    ("library", "src/main/modules/dedekind/"),
    ("listings", "src/test/cpp/modules/dedekind/category/jlt_listings_test.cpp"),
    ("Catch2", "build/_deps/catch2-src/"),
]


def admission():
    table = {}
    for line in (HERE / "admission.txt").read_text().splitlines():
        if line.strip() and not line.startswith("#"):
            kind, cls = line.split()
            assert cls in CLASSES, line
            table[kind] = cls
    return table


def inventories(text):
    """{file: {kind: count}}, from each 'report inventory <file>: n' block."""
    out, current = {}, None
    for line in text.splitlines():
        line = re.sub(r"^.*?\d{4}-\d\d-\d\dT\S+Z ", "", line)  # a CI log's prefix
        head = re.match(r"jlt-lint: report inventory (\S+): \d+$", line)
        if head:
            current = out.setdefault(head.group(1), {})
            continue
        kind = re.match(r"\s+((?:Decl|Stmt|Type)\S*:\S+) (\d+)$", line)
        if kind and current is not None:
            current[kind.group(1)] = int(kind.group(2))
        else:
            current = None
    return out


def main():
    table, inv = admission(), inventories(sys.stdin.read())
    latex = "--latex" in sys.argv
    for name, prefix in SCOPES:
        files = {f: k for f, k in inv.items() if f.startswith(prefix)}
        if not files:
            continue
        total = collections.Counter()
        admitted_files = 0
        for kinds in files.values():
            per = collections.Counter()
            for kind, n in kinds.items():
                per[table.get(kind, "open")] += n
            total += per
            admitted_files += per["refused"] == 0 and per["open"] == 0
        n = sum(total.values())
        share = {c: 100 * total[c] / n for c in CLASSES}
        if latex:
            cells = " & ".join(f"{share[c]:.1f}" for c in CLASSES)
            print(f"{name} & {n} & {cells} \\\\")
        else:
            print(f"{name}: {len(files)} files, {admitted_files} with nothing refused or open; {n} constructs")
            for c in CLASSES:
                print(f"  {c:12s} {total[c]:6d}  {share[c]:5.1f} %")
            unknown = sorted({k for kinds in files.values() for k in kinds if k not in table})
            if unknown:
                print("  not in admission.txt (counted open):", " ".join(unknown))


if __name__ == "__main__":
    main()
