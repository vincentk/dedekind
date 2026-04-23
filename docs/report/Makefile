.PHONY: doc ci-check

doc:
	lualatex -interaction=nonstopmode report.tex
	biber report
	lualatex -interaction=nonstopmode report.tex

# Fast CI/local gate: detect TeX compile errors without bibliography pass.
ci-check:
	lualatex -interaction=nonstopmode -halt-on-error report.tex