T = CYGWIN-rfpk-HOWTO
#HTML_OPS = --nochunks --output $T.html
HTML_OPS = --output $T.html

all:	html pdf rtf

html:	$T.html

pdf:	$T.pdf

rtf:	$T.rtf

%.html : %.xml
	docbook2html --dsl ../stylesheet/local.dsl#html $(HTML_OPS)  $<

%.pdf : %.xml
	docbook2pdf --dsl ../stylesheet/local.dsl#print $<

%.rtf : %.xml
	docbook2rtf --dsl ../stylesheet/local.dsl#print $<

clean:
	rm -f $T.pdf $T.rtf *.html/*
	rmdir *.html 

