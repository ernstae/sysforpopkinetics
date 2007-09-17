include ../../webdir.mk
T = cspkOutXML
HTML_OPS = --nochunks
STYLE = ../../stylesheet
WEB = $(WEBDIR)/v0.1/specs/$T
PNG = $(wildcard *.png)
HTML = $(wildcard SpkML_html/*.html)

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

install: $(HTML)
	scp $T.html $(WEB)
	scp SpkML_nonmemOut.xml $(WEB)
	for i in $(HTML); do \
	     scp $$i $(WEB); \
	done
	scp $T.pdf  $(WEB)
	for i in $(PNG); do \
	     scp $$i $(WEB); \
	done

clean:
	rm -f $T.pdf $T.rtf $T.html

