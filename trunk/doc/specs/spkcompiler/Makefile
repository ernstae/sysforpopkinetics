include ../../webdir.mk
T = spkcompiler
HTML_OPS = --nochunks
STYLE = ../../stylesheet
WEB = $(WEBDIR)/v0.1/specs/$T
PNG = $(wildcard *.png)
DRIVER_SRC = ../../../src/apps/spk/aspk/spkcompiler/driver

all:	man html

man:	$T.man

html:	$T.html

$T.man:	$(DRIVER_SRC)/spkcompiler.cpp
	doxygen $T.cfg 

%.html:	$T.xml
	docbook2html --dsl $(STYLE)/local.dsl#html $(HTML_OPS) $<

%.pdf:  $T.xml
	docbook2pdf  --dsl $(STYLE)/local.dsl#print $<

%.rtf:	$T.xml
	docbook2rtf  --dsl $(STYLE)/local.dsl#print $<

install:
	scp $T.1    $(WEB)
	scp $T.html $(WEB)
	scp $T.rtf  $(WEB)
	scp $T.pdf  $(WEB)
	for i in $(PNG); do \
		scp $$i $(WEB); \
	done

clean:
	rm -f $T.1 $T.html $T.pdf $T.rtf

