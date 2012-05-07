WEBDIR=.
T = spkcompiler
WEB = $(WEBDIR)/v0.1/specs/$T

dist_pkgdata_DATA = spkcompiler.cfg 

DRIVER_SRC = ../spkcompiler/main.cpp

html : components_html overall_html

overall_html : spkcompiler.xml
	docbook2html --dsl ~/develop/r2/doc/stylesheet/"local.dsl#html" --nochunks $<

components_html : $(DRIVER_SRC) 
	doxygen -u spkcompiler.cfg

clean : 
	rm -rf html
	rm -rf latex
	rm -rf man
	rm -rf rtf
	rm -f spkcompiler.html

all  : 
	rm -rf html
	rm -rf latex
	rm -rf man
	rm -rf rtf
	rm -f spkcompiler.html
	docbook2html --dsl ~/develop/r2/doc/stylesheet/"local.dsl#html" --nochunks spkcompiler.xml
	doxygen spkcompiler.cfg

install:
	scp *.html $(WEB)
	scp html/* $(WEB)/html
