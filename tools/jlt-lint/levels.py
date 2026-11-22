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

    python3 tools/jlt-lint/levels.py BUILD TOOL NAMESPACE FILE [--latex OUT] [--check OUT]

--latex writes the paper's table rows; --check regenerates them and fails if
the committed file differs, so the paper's table cannot drift from the code.
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
NOT_DENY = {"axioms", "inventory", "definitions", "admitted"}


def classify(where, classes):
    """{definition: (level, why, by_type)} from the tool's findings."""
    defs = []
    for l in where.get("definitions", []):
        m = LOCATION.match(l)
        d = re.match(r"(?P<name>.+) lines (?P<a>\d+)-(?P<b>\d+)(?P<kinds>( \S+)*)$", m["rest"])
        defs.append((d["name"], int(d["a"]), int(d["b"]), d["kinds"].split()))
    findings = []  # (line, rule, text)
    for rule, lines in where.items():
        if rule in NOT_DENY:
            continue
        for l in lines:
            m = LOCATION.match(l)
            if m:
                findings.append((int(m["line"]), rule, m["rest"] or ""))
    admitted = {}
    for l in where.get("admitted", []):
        m = LOCATION.match(l)
        admitted[int(m["line"])] = "admitted"
    for l in where.get("rule2-by-type", []):
        m = LOCATION.match(l)
        admitted[int(m["line"])] = "undecided" if " undecided: " in (m["rest"] or "") else "not admitted"
    out = []
    for name, a, b, kinds in defs:
        cls = {k: classes.get(k, "open") for k in kinds}
        by_type = next((v for line, v in admitted.items() if a <= line <= b), None)
        deny = sorted({r for line, r, _ in findings if a <= line <= b and r != "rule2-by-type"})
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
        out.append((name, a, level, why, by_type))
    return out


def latex(rows):
    """The paper's rows: a function template or a type by name; the entries of
    laws and associated types summed by level, since there are many."""
    lines, summed = [], {}
    for name, _, level, why, by_type in rows:
        if "<" in name:  # an entry of a law or of an associated type
            summed[level] = summed.get(level, 0) + 1
            continue
        name_tt = name.replace("_", "\\_")
        typed = by_type or "---"
        lines.append(f"\\texttt{{{name_tt}}} & {typed} & {level} \\\\")
    for level, n in sorted(summed.items()):
        lines.append(f"entries of laws and associated types ({n}) & --- & {level} \\\\")
    return "\n".join(lines) + "\n"


def main():
    args = argparse.ArgumentParser()
    args.add_argument("build", type=pathlib.Path)
    args.add_argument("tool", type=pathlib.Path)
    args.add_argument("namespace")
    args.add_argument("file")
    args.add_argument("--latex", type=pathlib.Path)
    args.add_argument("--check", type=pathlib.Path)
    a = args.parse_args()
    rules = sorted(p.stem for p in (HERE / "rules").glob("*.query"))
    build = jlt_lint.one_entry_per_file(a.build, "levels") if (a.build / "compile_commands.json").exists() else a.build
    counts, where, out = jlt_lint.run(a.tool, build, a.file, a.namespace, rules)
    if counts is None or counts == "absent":
        sys.exit(f"levels.py: the linter did not run on {a.file}:\n{out[:2000]}")
    rows = classify(where, admission.admission())
    for name, line, level, why, by_type in rows:
        print(f"{line:5d}  {level:10s} {by_type or '':13s} {name}" + (f"  ({why})" if why else ""))
    text = latex(rows)
    if a.latex:
        a.latex.write_text(text)
    if a.check:
        if a.check.read_text() != text:
            sys.exit(f"levels.py: {a.check} differs from the levels the linter gives; regenerate it with --latex")
        print(f"levels.py: {a.check} matches")


if __name__ == "__main__":
    main()
