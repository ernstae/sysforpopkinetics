T = SUBJECT-rfpk-HOWTO
HTML_OPS = --nochunks
STYLE = ../../stylesheet

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
	scp $T.html whitechuck:/var/www/html/soft/howto/html
	scp $T.pdf  whitechuck:/var/www/html/soft/howto/pdf

clean:
	rm -f $T.pdf $T.rtf $T.html

