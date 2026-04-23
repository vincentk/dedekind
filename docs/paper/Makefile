.PHONY: doc ci-check

doc: #dot
	lualatex paper.tex
	biber paper
	lualatex paper.tex

# Fast CI/local gate: detect TeX compile errors without bibliography pass.
# Mirrors the pattern in docs/report/Makefile.
ci-check:
	lualatex -interaction=nonstopmode -halt-on-error paper.tex
