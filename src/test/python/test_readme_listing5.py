"""The README's Listing 5 panels are the checked sources, verbatim.

Each C++ panel is the region between ``// [listing5:<panel>]`` markers in the
tensor exhibit's test, which compiles it; each Python panel is the region
between ``.. [listing5:<panel>]`` markers in ``pynzli.jlt``'s docstring, which
``test_jlt_doctests`` runs.  And the Python column has the C++ column's
surface: every library name a C++ panel uses is ``pynzli.jlt``'s too.
"""

import re
import unittest
from pathlib import Path

import pynzli.jlt

ROOT = Path(__file__).resolve().parents[3]
README = ROOT / "README.md"
CPP = ROOT / "src/test/cpp/modules/dedekind/category/tensor_exhibit_test.cpp"
PAPER = ROOT / "docs/paper/jlt/sections/6-partial-evaluation.tex"
PANELS = ["all-genomes", "one-genome"]

# The host language's own words, not the library's.
CPP_WORDS = {
    "auto",
    "byte",
    "bytes",  # the library's in C++, Python's own there
    "const",
    "constexpr",
    "return",
    "size_t",
    "span",
    "static_assert",
    "std",
}


def between(text: str, begin: str, end: str) -> str:
    start = text.index(begin) + len(begin)
    return text[start : text.index(end, start)].strip("\n")


def cpp_panel(panel: str) -> str:
    return between(
        CPP.read_text(encoding="utf-8"),
        f"// [listing5:{panel}]\n",
        f"// [/listing5:{panel}]",
    )


def python_panel(panel: str) -> str:
    return between(
        pynzli.jlt.__doc__ or "",
        f".. [listing5:{panel}]\n",
        f".. [/listing5:{panel}]",
    )


def readme_panel(panel: str, language: str) -> str:
    return between(
        README.read_text(encoding="utf-8"),
        f"<!-- listing5:{panel}:{language} -->\n\n```{'cpp' if language == 'cpp' else 'pycon'}\n",
        "```",
    )


def code_of(cpp: str) -> str:
    """A panel as code alone: no comments, no std::, no whitespace."""
    return re.sub(r"\s+", "", re.sub(r"//[^\n]*", "", cpp).replace("std::", ""))


def definitions(cpp: str) -> set[str]:
    """What a panel defines: its constants, functions and parameters."""
    code = re.sub(r"'[^']*'|\"[^\"]*\"|//[^\n]*", "", cpp)
    defined = set(re.findall(r"(?:constexpr|const)\s+\w+\s+(\w+)\s*[=(]", code))
    parameters = set(re.findall(r"(?:size_t|std::byte>)\s+(\w+)\s*[,)]", code))
    return defined | parameters


def library_names(cpp: str, defined: set[str]) -> set[str]:
    """The names a panel takes from the library: its identifiers, less the
    host language's words and what the panels define (a later panel uses an
    earlier one's names, as the README shows them in order)."""
    code = re.sub(r"'[^']*'|\"[^\"]*\"|//[^\n]*", "", cpp)
    names = set(re.findall(r"\b[A-Za-z_]\w*\b", code))
    return names - CPP_WORDS - defined


class ReadmeListing5(unittest.TestCase):
    def test_the_readme_shows_the_checked_cpp(self) -> None:
        for panel in PANELS:
            with self.subTest(panel=panel):
                self.assertEqual(readme_panel(panel, "cpp"), cpp_panel(panel))

    def test_the_readme_shows_the_checked_python(self) -> None:
        for panel in PANELS:
            with self.subTest(panel=panel):
                self.assertEqual(readme_panel(panel, "python"), python_panel(panel))

    def test_the_paper_shows_the_same_code(self) -> None:
        # the paper breaks lines for its column and drops std::, so its panels
        # are compared as code, comments aside
        paper = PAPER.read_text(encoding="utf-8")
        listing = paper[paper.index(r"\label{sec:binding}") : paper.index(r"\label{lst:sites}")]
        panels = re.findall(r"\\begin\{cpp\}(.*?)\\end\{cpp\}", listing, re.S)
        self.assertEqual(len(panels), len(PANELS))
        for panel, text in zip(PANELS, panels):
            with self.subTest(panel=panel):
                self.assertEqual(code_of(text), code_of(cpp_panel(panel)))

    def test_python_has_the_listings_surface(self) -> None:
        defined = set().union(*(definitions(cpp_panel(p)) for p in PANELS))
        for panel in PANELS:
            for name in sorted(library_names(cpp_panel(panel), defined)):
                with self.subTest(panel=panel, name=name):
                    self.assertIn(name, pynzli.jlt.__all__)


if __name__ == "__main__":
    unittest.main()
