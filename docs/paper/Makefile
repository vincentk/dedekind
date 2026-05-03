.PHONY: doc ci-check

# `-shell-escape` is required by the `minted` package, which shells out to
# Pygments for syntax highlighting and native UTF-8 rendering of code
# listings (per #557).  Pygments is installed by the project-level
# `make ci-install-paper-deps` target.
LUALATEX := lualatex -shell-escape

doc: #dot
	$(LUALATEX) paper.tex
	biber paper
	$(LUALATEX) paper.tex

# Fast CI/local gate: detect TeX compile errors without bibliography pass.
# Mirrors the pattern in docs/report/Makefile.
ci-check:
	$(LUALATEX) -interaction=nonstopmode -halt-on-error paper.tex
