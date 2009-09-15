
memoire: memoire.pdf

LATEX=pdflatex

memoire.pdf : memoire.tex memoire.bib r5rscommands.tex doublespace.sty udem_these_fr.sty
	$(LATEX) memoire
	bibtex memoire
	$(LATEX) memoire
	@echo ";;;;;;; Last Run (undef) ;;;;;;;;;;"
	$(LATEX) memoire

test : memoire.pdf
	xpdf memoire.pdf

clean :
	rm -f *~ *.aux .Z* memoire.dvi memoire.log _TZ_* memoire.lof memoire.lot memoire.pdf memoire.toc memoire.bbl memoire.ps memoire.blg
