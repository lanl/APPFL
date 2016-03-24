SHELL = /bin/sh

.PHONY: all clean

TARGET := paper

all: clean
	latexmk -pdf $(TARGET)
	# cat paper.tex | aspell -t --personal=okwords.txt list
clean:
	latexmk -C $(TARGET)
	rm -f *.bbl *.fls *.tex-e
