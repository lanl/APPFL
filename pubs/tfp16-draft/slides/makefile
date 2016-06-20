SHELL = /bin/sh

.PHONY: all clean

TARGET := slides

all: clean
	pdflatex $(TARGET)


print:
	# hacky way of faking command line arguments
	pdflatex "\def\printableSlides{1} \input{$(TARGET)}"

clean:
	latexmk -C $(TARGET)
	rm -f *.bbl *.fls *.tex-e
