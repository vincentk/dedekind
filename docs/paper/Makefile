.PHONY: doc ci-check jlt jlt-check clean

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
ci-check:
	$(LUALATEX) -interaction=nonstopmode -halt-on-error paper.tex

# The Jlt paper (docs/paper/jlt/jlt.tex, programming.cls): lualatex, biber,
# then lualatex twice, since cross-references and the table of contents
# settle only on the second pass after the bibliography.  Every pass halts
# on the first error so a broken build fails `make`.
JLT_LUALATEX := $(LUALATEX) -interaction=nonstopmode -halt-on-error

jlt:
	cd jlt && $(JLT_LUALATEX) jlt.tex && biber jlt && $(JLT_LUALATEX) jlt.tex && $(JLT_LUALATEX) jlt.tex

# Fast gate for the Jlt paper: one pass, no bibliography.
jlt-check:
	cd jlt && $(JLT_LUALATEX) jlt.tex

# Build products of both papers; the PDFs stay.
clean:
	rm -f paper.aux paper.bbl paper.bcf paper.blg paper.log paper.out paper.run.xml paper.toc paper.synctex.gz missfont.log
	rm -f jlt/jlt.aux jlt/jlt.bbl jlt/jlt.bcf jlt/jlt.blg jlt/jlt.log jlt/jlt.out jlt/jlt.run.xml jlt/jlt.toc jlt/jlt.synctex.gz jlt/missfont.log
	rm -rf _minted jlt/_minted
