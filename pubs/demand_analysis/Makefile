# Makefile to process LaTeX files for paper
PARTS =		commands.tex paper.tex

.SUFFIXES:	.mac .verb .tex .dvi .dvi2 .ps3 .ps4 .ps4 .ps6

.mac.verb:
	nl <$< >$@

.verb.tex:
	expand <$< | ./verbatim >$@

.tex.dvi:
	latex $<

.dvi.dvi2:
	dvi2 '2:0(3mm,-4mm),1(193mm,-4mm)' $< $@

.dvi2.ps3:
	dvips -t landscape -x 707 $< -o $@

.dvi.ps4:
	dvips -D400 -h 400dpi.pro -t portrait -x 1200 $< -o $@

.dvi.ps6:
	dvips -D600 -Z -t portrait -x 1200 $< -o $@

paper.dvi:	verbatim paper.tex ${PARTS}

verbatim:       verbatim.lex
	lex verbatim.lex
	cc lex.yy.c -ll -o verbatim
	rm lex.yy.c

all:
	touch paper.verb
	make paper.dvi

