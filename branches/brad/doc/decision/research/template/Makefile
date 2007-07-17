include ../../../webdir.mk
T = SUBJECT
HTML_OPS = --nochunks
STYLE = ../../../stylesheet
WEB = $(WEBDIR)/decision/research/$T
PNG = $(wildcard *.png)

all:	html pdf rtf

html:	$T.html

pdf:	$T.pdf

rtf:	$T.rtf

%.html : %.xml
	docbook2html --dsl $(STYLE)/local.dsl#html $(HTML_OPS)  $<

%.pdf : %.xml
	docbook2pdf --dsl $(STYLE)/local.dsl#print $<

%.rtf : %.xml
	docbook2rtf --dsl $(STYLE)/local.dsl#print $<

install:
	scp $T.html $(WEB)
	scp $T.pdf  $(WEB)
	for i in $(PNG); do \
	     scp $$i $(WEB); \
	done

clean:
	rm -f $T.pdf $T.rtf $T.html

