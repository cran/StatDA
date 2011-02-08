# Makefile for LaTeX files

LATEX	= pdflatex
#BIBTEX	= bibtex

SRC	= StatDA.tex

all 	: StatDA.pdf

%.pdf:	%.tex
	$(LATEX) $*
#	$(BIBTEX) $*
#	$(LATEX) $*
	$(LATEX) $*
	rm -f *.ps $*-* $*.dvi $*.log $*.bbl *.tex $*.aux $*.blg $*.log $*.toc
	rm Rplots.pdf
