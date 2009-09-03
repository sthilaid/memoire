
memoire: memoire.pdf

memoire.dvi : memoire.tex memoire.bib r5rscommands.tex doublespace.sty udem_these_fr.sty
	latex memoire
	bibtex memoire
	latex memoire
	latex memoire

again :
	latex memoire

memoire.ps : memoire.dvi
	dvips -t letter memoire.dvi -o memoire.ps

memoire.pdf : memoire.ps
	ps2pdf memoire.ps

test : memoire.pdf
	xpdf memoire.pdf

clean :
	rm -f *~ *.aux .Z* memoire.dvi memoire.log _TZ_* memoire.lof memoire.lot memoire.pdf memoire.toc memoire.bbl memoire.ps memoire.blg
