D = CVS-rfpk-HOWTO
JARS = "F:/Programs/java/xalan.jar;F:/Programs/java/xerces.jar"
PROC = org.apache.xalan.xslt.Process
S = f:/cygwin/usr/local/lib/xml/stylesheets/docbook-xsl
HTML = $S/html/docbook.xsl
FO = $S/fo/docbook.xsl

all:	html fo

html:	$D.html

fo:	$D.fo

$D.html:	$D.xml
	java -cp ${JARS} ${PROC} -in $D.xml -xsl ${HTML} -out $D.html

$D.fo:	$D.xml
	java -cp ${JARS} ${PROC} -in $D.xml -xsl ${FO} -out $D.fo

clean:
	rm -f $D.html $D.fo

