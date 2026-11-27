"""Doxygen input filter for the linter's sources that are not C++.

A rule file (rules/<rule>.query) becomes a page named for the rule: its header
comment as the text, its matchers verbatim.  admission.txt becomes one page,
verbatim, its comments standing beside the kinds they explain.  So the
reference is generated from the files the linter reads, and has no second copy
to grow stale (#1231).

    python3 tools/jlt-lint/doxfilter.py <file>      (Doxygen's FILTER_PATTERNS)
"""
import pathlib
import sys


def escape(line):
    for a, b in (("\\", "\\\\"), ("@", "\\@"), ("<", "\\<"), (">", "\\>"),
                 ("#", "\\#"), ("%", "\\%"), ("*/", "*\\/")):
        line = line.replace(a, b)
    return line


def page(name, title, text, verbatim):
    body = "\n".join(verbatim).replace("*/", "* /")
    return (f"/** \\page {name} {title}\n" + "\n".join(text) +
            f"\n\\verbatim\n{body}\n\\endverbatim\n*/\n")


def main():
    path = pathlib.Path(sys.argv[1])
    lines = path.read_text().splitlines()
    name = path.stem.replace("-", "_").replace(".", "_")
    if path.suffix == ".query":
        text = [escape(l.lstrip("#").strip()) for l in lines if l.startswith("#")]
        rest = [l for l in lines if not l.startswith("#")]
        sys.stdout.write(page("rule_" + name, path.stem, text, rest))
    else:
        sys.stdout.write(page(name, path.name, [], lines))


if __name__ == "__main__":
    main()
