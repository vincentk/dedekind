"""The paper's summary table: Table 6's levels, summed over bodies of code.

Reads the levels the driver wrote beside its report (<out>/all-levels.json:
each file's definitions with their level, by levels.py's classify, the same
computation as Table 6) and sums them for each body of code: the listings,
the library, and two libraries not written under the rules: the ranges
library of the standard library in use, which constrains its templates by
concepts, and Catch2, which does not, its headers and its sources apart.  Per body:
the number of definitions, the share of them in FJlt, in Jlt, refused and
undecided, in per cent so that the rows compare, and of the function
templates how many the type check admits, does not admit, or leaves
undecided.

    python3 tools/jlt-lint/summary.py --levels build/jlt-lint/parts/all-levels.json [--latex OUT] [--check OUT]

--check regenerates the rows and fails, printing them, if OUT differs, so
that the paper's table cannot drift from the code.

--diagnostic [N] prints, for each body, every reason a definition is refused
or undecided, ranked by how many definitions it alone holds back (they would
be shown if it went), with N of them each as path:line name, and stops: where
to start when raising the coverage.  jlt_lint.py --diagnostic prints the same
for the part it lints.

Every run also prints, for each body, the most frequent reasons of its
refused and its undecided definitions (a definition with several reasons
counts once for each), and after each reason, in parentheses, how many
definitions have it as their only reason, with no refused or open kind of
syntax beside it: what the rows' two last levels are
made of, and how much of each one reason alone accounts for.
"""
import argparse
import collections
import json
import pathlib
import sys

BODIES = [
    ("listings", lambda key: key == "src/test/cpp/modules/dedekind/category/jlt_listings_test.cpp paper_listings"),
    ("library", lambda key: key.startswith("src/main/modules/dedekind/")),
    ("standard ranges", lambda key: key.startswith("build/_deps/stdlib-ranges/")),
    ("Catch2, headers", lambda key: key.startswith("build/_deps/catch2-src/") and key.split()[0].endswith(".hpp")),
    ("Catch2, sources", lambda key: key.startswith("build/_deps/catch2-src/") and key.split()[0].endswith(".cpp")),
]
LEVELS = ["FJlt", "Jlt", "refused", "undecided"]
BY_TYPE = ["admitted", "not admitted", "undecided"]


def rows(levels):
    out = []
    for name, member in BODIES:
        level, by_type, n = collections.Counter(), collections.Counter(), 0
        for key, defs in levels.items():
            if not member(key):
                continue
            for _name, _line, lv, _why, bt, *_also in defs:
                n += 1
                level[lv] += 1
                if bt:
                    by_type[bt] += 1
        if not n:
            sys.exit(f"summary.py: no definitions for {name}: a whole, successful run is needed")
        out.append(row(name, n, level, by_type))
    return "\n".join(out) + "\n"


def counts(levels):
    """The levels of each body as counts, one line each, for the ratchet:
    `<body>\t<definitions> <FJlt> <Jlt> <refused> <undecided>`."""
    out = ["# The levels of each body's definitions, counted: definitions, FJlt, Jlt, refused,",
           "# undecided.  Written by tools/jlt-lint/summary.py --counts, checked by every",
           "# build on Linux, held by ratchet.py: a body's definitions shown (FJlt and Jlt)",
           "# may not fall."]
    for name, member in BODIES:
        level, n = collections.Counter(), 0
        for key, defs in levels.items():
            if member(key):
                for _name, _line, lv, *_rest in defs:
                    n += 1
                    level[lv] += 1
        out.append(f"{name}\t{n} " + " ".join(str(level[lv]) for lv in LEVELS))
    return "\n".join(out) + "\n"


def percent(k, n):
    """A share in per cent, whole; a share that is not zero is not shown as 0."""
    p = 100 * k / n
    return "0" if k == 0 else "<1" if p < 1 else str(round(p))


def row(name, n, level, by_type):
    """One body of code: its number of definitions, each level as a share of
    them in per cent, so that rows compare, and its function templates by the
    type check's outcome, as counts."""
    return (f"{name} & {n} & " + " & ".join(percent(level[k], n) for k in LEVELS) + " & "
            + " / ".join(str(by_type[k]) for k in BY_TYPE) + " \\\\")


def reasons(levels, top=8):
    """For each body and for refused and undecided, the most frequent reasons."""
    out = []
    for name, member in BODIES:
        for level in ("refused", "undecided"):
            count, alone, n = collections.Counter(), collections.Counter(), 0
            for key, defs in levels.items():
                if not member(key):
                    continue
                for _name, _line, lv, why, _bt, *also in defs:
                    if lv == level:
                        n += 1
                        ws = [w.strip() for w in why.split(",") if w.strip()]
                        count.update(ws)
                        if len(ws) == 1 and not (also and also[0]):
                            alone[ws[0]] += 1
            out.append(f"summary.py: {name}, {level} ({n}): "
                       + ", ".join(f"{w} {c} ({alone[w]} alone)" for w, c in count.most_common(top)))
    return "\n".join(out) + "\n"


def diagnostic(levels, member=lambda key: True, examples=3):
    """Every reason a definition is refused or undecided, ranked by what
    fixing it alone would gain: first by how many definitions have it as
    their only reason (they would be shown if it went), then by how many
    have it at all; each with a few of the definitions it alone holds back,
    as path:line name, to start from.  For one body, or for whatever run of
    the linter wrote the levels (a part, a file)."""
    out = []
    for level in ("refused", "undecided"):
        count, alone, where, n = collections.Counter(), collections.Counter(), collections.defaultdict(list), 0
        for key, defs in levels.items():
            if not member(key):
                continue
            path = key.split()[0]
            for name, line, lv, why, _bt, *also in defs:
                if lv != level:
                    continue
                n += 1
                ws = [w.strip() for w in why.split(",") if w.strip()]
                count.update(ws)
                if len(ws) == 1 and not (also and also[0]):
                    alone[ws[0]] += 1
                    if len(where[ws[0]]) < examples:
                        where[ws[0]].append(f"{path}:{line} {name}")
        out.append(f"  {level} ({n}), by what fixing a reason alone would show:")
        for w, c in sorted(count.items(), key=lambda wc: (-alone[wc[0]], -wc[1], wc[0])):
            out.append(f"    {alone[w]:>5} alone of {c:>5}  {w}")
            out += [f"           {e}" for e in where[w]]
    return "\n".join(out) + "\n"


def diagnose(levels, examples=3):
    """The diagnostic of each body of code that has definitions."""
    out = []
    for name, member in BODIES:
        if any(member(key) for key in levels):
            out.append(f"summary.py: --diagnostic, {name}")
            out.append(diagnostic(levels, member, examples))
    return "\n".join(out)


FACTS = [("CatchSources", "Catch2, sources"), ("CatchHeaders", "Catch2, headers")]


def facts(levels):
    """Numbers the paper's prose states about the control, as TeX macros, so
    that a sentence cannot drift from the table: for Catch2's sources and its
    headers, the definitions, those shown to be in Jlt, those refused, those
    refused for a function not declared constexpr, and for that alone."""
    members = dict(BODIES)
    out = ["% regenerated by tools/jlt-lint/summary.py from the build; checked by every build"]
    for macro, body in FACTS:
        n = shown = refused = keyword = alone = 0
        for key, defs in levels.items():
            if not members[body](key):
                continue
            for _name, _line, lv, why, _bt, *also in defs:
                n += 1
                shown += lv in ("FJlt", "Jlt")
                if lv == "refused":
                    refused += 1
                    ws = [w.strip() for w in why.split(",") if w.strip()]
                    keyword += "rule1-constexpr" in ws
                    alone += ws == ["rule1-constexpr"] and not (also and also[0])
        for name, value in (("Definitions", n), ("Shown", shown), ("Refused", refused),
                            ("NotConstexpr", keyword), ("NotConstexprAlone", alone)):
            out.append(f"\\newcommand{{\\Jlt{macro}{name}}}{{{value}}}")
    return "\n".join(out) + "\n"


def main():
    args = argparse.ArgumentParser()
    args.add_argument("--levels", type=pathlib.Path, required=True)
    args.add_argument("--latex", type=pathlib.Path)
    args.add_argument("--check", type=pathlib.Path)
    args.add_argument("--facts", type=pathlib.Path, help="write the prose's numbers as TeX macros")
    args.add_argument("--check-facts", type=pathlib.Path, help="fail if the macros differ")
    args.add_argument("--counts", type=pathlib.Path, help="write each body's levels as counts")
    args.add_argument("--check-counts", type=pathlib.Path, help="fail if the counts differ")
    args.add_argument("--diagnostic", nargs="?", type=int, const=3, metavar="N",
                      help="print every reason a definition is refused or undecided, ranked by "
                           "what fixing it alone would show, with N example definitions each "
                           "(default 3), and stop")
    a = args.parse_args()
    levels = json.loads(a.levels.read_text())
    if a.diagnostic is not None:
        sys.stdout.write(diagnose(levels, a.diagnostic))
        return
    text = ("% regenerated by tools/jlt-lint/summary.py from the build; checked by every build\n"
            + rows(levels))
    sys.stdout.write(reasons(levels))
    sys.stdout.flush()
    macros = facts(levels)
    tally = counts(levels)
    # Every generated file that differs is reported in one run, so that one
    # log has all of them.
    failed = []
    if a.facts:
        a.facts.write_text(macros)
    if a.check_facts and a.check_facts.read_text() != macros:
        failed.append(f"summary.py: {a.check_facts} differs from the build's numbers; "
                      f"regenerate it with --facts.  The macros:\n{macros}")
    if a.counts:
        a.counts.write_text(tally)
    if a.check_counts and a.check_counts.read_text() != tally:
        failed.append(f"summary.py: {a.check_counts} differs from the build's levels; "
                      f"regenerate it with --counts.  The counts:\n{tally}")
    if a.latex:
        a.latex.write_text(text)
    if a.check:
        if a.check.read_text() != text:
            failed.append(f"summary.py: {a.check} differs from the levels the linter gives; "
                          f"regenerate it with --latex.  The rows:\n{text}")
        else:
            print(f"summary.py: {a.check} matches")
    if failed:
        sys.exit("\n".join(failed))
    if not a.latex and not a.check:
        sys.stdout.write(text)
        sys.stdout.write(macros)


if __name__ == "__main__":
    main()
