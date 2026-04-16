.PHONY: doc ci-check

doc:
	pdflatex -interaction=nonstopmode report.tex
	biber report
	pdflatex -interaction=nonstopmode report.tex

# Fast CI/local gate: detect TeX compile errors without bibliography pass.
ci-check:
	pdflatex -interaction=nonstopmode -halt-on-error report.tex