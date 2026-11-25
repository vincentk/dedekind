"""The level of every definition in one namespace of one file: the linter's
two stages and the kinds of syntax it uses, joined.

For each declaration the `definitions` check reports (its lines and the
syntax kinds it uses), the level is:

- refused, when a deny-rule finds something in its lines, or it uses a kind
  admission.txt refuses, or the type check does not admit it;
- undecided, when it uses a kind still open, or the type check cannot read
  its constraints;
- Jlt, when every kind is kernel, scaffolding or a derived form, and some is
  a derived form;
- FJlt, when every kind is kernel or scaffolding.

A function template also carries the type check's outcome; anything else is
not checked by type (a law's entry, a type, a concept: the archetypes are
built from these, not checked against them).

    python3 tools/jlt-lint/levels.py BUILD TOOL NAMESPACE FILE [--latex OUT] [--check OUT] [--expect FILE]

--latex writes the paper's table rows; --check regenerates them and fails if
the committed file differs, so the paper's table cannot drift from the code.
--expect compares every definition's level with FILE, lines `<name> <level>`
(a name defined twice has its levels in order), and fails on a definition
that differs, is missing or is not listed: a fixture's levels are then
pinned definition by definition, not by a count.
"""
import argparse
import pathlib
import re
import sys

HERE = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))
import jlt_lint  # noqa: E402
import admission  # noqa: E402

LOCATION = re.compile(r"^(?P<path>\S+?):(?P<line>\d+):(?P<col>\d+)(?:: (?P<rest>.*))?$")
# Reports, not deny-rules: rule2-constrained-templates is a proxy that only
# reports (README), so it refuses nothing.
NOT_DENY = {"axioms", "inventory", "definitions", "admitted", "undecided-by-type", "rule2-constrained-templates"}


def classify(where, classes):
    """{definition: (level, why, by_type)} from the tool's findings."""
    defs = []
    for l in where.get("definitions", []):
        m = LOCATION.match(l)
        d = re.match(r"(?P<name>.+) range (?P<a>\d+):(?P<ac>\d+)-(?P<b>\d+):(?P<bc>\d+)(?P<kinds>( \S+)*)$", m["rest"])
        defs.append((d["name"], (int(d["a"]), int(d["ac"])), (int(d["b"]), int(d["bc"])), d["kinds"].split()))
    findings = []  # (line, rule, text)
    for rule, lines in where.items():
        if rule in NOT_DENY:
            continue
        for l in lines:
            m = LOCATION.match(l)
            if m:
                findings.append(((int(m["line"]), int(m["col"])), rule, m["rest"] or ""))
    # Each outcome at the position of the template's name, line and column,
    # so two definitions on one line keep their own (Copilot on #1211).
    admitted = {}
    for l in where.get("admitted", []):
        m = LOCATION.match(l)
        admitted[(int(m["line"]), int(m["col"]))] = "admitted"
    for l in where.get("rule2-by-type", []):
        m = LOCATION.match(l)
        admitted[(int(m["line"]), int(m["col"]))] = "not admitted"
    for l in where.get("undecided-by-type", []):
        m = LOCATION.match(l)
        admitted[(int(m["line"]), int(m["col"]))] = "undecided"
    out = []
    for name, a, b, kinds in defs:
        cls = {k: classes.get(k, "open") for k in kinds}
        by_type = next((v for pos, v in admitted.items() if a <= pos <= b), None)
        deny = sorted({r for pos, r, _ in findings if a <= pos <= b and r != "rule2-by-type"})
        if deny:
            level, why = "refused", ", ".join(deny)
        elif by_type == "not admitted":
            level, why = "refused", "not admitted by type"
        elif "refused" in cls.values():
            level, why = "refused", ", ".join(k for k, c in cls.items() if c == "refused")
        elif "open" in cls.values() or by_type == "undecided":
            level, why = "undecided", ", ".join(k for k, c in cls.items() if c == "open") or "by type"
        elif "derived" in cls.values():
            level, why = "Jlt", ", ".join(k.split(":")[1] for k, c in cls.items() if c == "derived")
        else:
            level, why = "FJlt", ""
        out.append((name, a[0], level, why, by_type))
    return out


READABLE = {
    "rule2-definition": "an operation its constraints do not name",
    "rule2-decided-resolution": "a choice its signature does not decide",
    "rule3-no-loops": "a loop with no stated end",
    "range": "a range",
    "view": "a view",
    "CXXMemberCallExpr": "a member call",
    "LValueReference": "a reference parameter",
    "TypeAlias": "an alias",
    "TypeAliasTemplate": "an alias",
    "AliasTemplateSpecialization": "an alias",
}


def readable(why):
    words = [READABLE.get(w.strip(), w.strip()) for w in why.split(",")] if why else []
    return ", ".join(dict.fromkeys(words))


def latex(rows):
    """The paper's rows: each function template by name, with its admission
    by type and its level; every other definition (shapes, laws, types and
    their entries) summed by level, the names of those not in FJlt given."""
    lines, rest = [], {}
    for name, _, level, why, by_type in rows:
        if by_type is not None:
            lines.append(f"\\texttt{{{name.replace('_', '\\_')}}} & {by_type} & {level}"
                         + (f" ({readable(why).replace('_', '\\_')})" if why else "") + " \\\\")
        else:
            rest.setdefault(level, []).append(name)
    for level in ("FJlt", "Jlt", "undecided", "refused"):
        names = rest.get(level, [])
        if not names:
            continue
        shown = "" if level == "FJlt" else ": " + ", ".join(
            f"\\texttt{{{n.replace('_', '\\_')}}}" for n in names)
        lines.append(f"other definitions ({len(names)}){shown} & --- & {level} \\\\")
    return "\n".join(lines) + "\n"


def main():
    args = argparse.ArgumentParser()
    args.add_argument("build", type=pathlib.Path)
    args.add_argument("tool", type=pathlib.Path)
    args.add_argument("namespace")
    args.add_argument("file")
    args.add_argument("--latex", type=pathlib.Path)
    args.add_argument("--check", type=pathlib.Path)
    args.add_argument("--expect", type=pathlib.Path)
    a = args.parse_args()
    rules = sorted(p.stem for p in (HERE / "rules").glob("*.query"))
    build = jlt_lint.one_entry_per_file(a.build, "levels") if (a.build / "compile_commands.json").exists() else a.build
    counts, where, out = jlt_lint.run(a.tool, build, a.file, a.namespace, rules)
    if counts is None or counts == "absent":
        sys.exit(f"levels.py: the linter did not run on {a.file}:\n{out[:2000]}")
    rows = classify(where, admission.admission())
    for name, line, level, why, by_type in rows:
        print(f"{line:5d}  {level:10s} {by_type or '':13s} {name}" + (f"  ({why})" if why else ""))
    if a.expect:
        want = [tuple(l.split()) for l in a.expect.read_text().splitlines()
                if l.strip() and not l.startswith("#")]
        got = [(name, level) for name, _, level, _, _ in rows]
        if want != got:
            diff = [f"  {n}: {lv}, expected {dict(want).get(n, 'not listed')}"
                    for n, lv in got if (n, lv) not in want]
            diff += [f"  {n}: expected {lv}, not found" for n, lv in want
                     if n not in dict(got)]
            sys.exit(f"levels.py: {a.file} differs from {a.expect}:\n" + "\n".join(diff))
        print(f"levels.py: {a.file}: {len(got)} levels as {a.expect.name} expects")
    text = latex(rows)
    if a.latex:
        a.latex.write_text(text)
    if a.check:
        if a.check.read_text() != text:
            sys.exit(f"levels.py: {a.check} differs from the levels the linter gives; regenerate it with --latex")
        print(f"levels.py: {a.check} matches")


if __name__ == "__main__":
    main()
