include ../../webdir.mk
T = modelCap
HTML_DIR = html
WEB = $(WEBDIR)/v0.1/specs/$T
PNG = $(wildcard *.png)

all:	dvi html pdf

dvi:	$T.dvi

html:	$T.html

pdf:	$T.pdf

%.dvi : %.tex
	latex $<     # Call latex two times so that
	latex $<     # cross references are correct.

%.html : %.tex
	latex2html -dir $(HTML_DIR) -mkdir $<

%.ps : %.dvi
	dvips -Ppdf $<

%.pdf : %.ps
	ps2pdf $<
	rm -f $<

install:
	scp $(HTML_DIR)/*.html $(WEB)/$(HTML_DIR)
	scp $(HTML_DIR)/*.png $(WEB)/$(HTML_DIR)
	scp $T.pdf  $(WEB)
	for i in $(PNG); do \
	     scp $$i $(WEB); \
	done

clean:
	rm -f $T.dvi $T.aux $T.log $T.toc
	rm -f $T.pdf $T.ps
	rm -f -r $(HTML_DIR)

