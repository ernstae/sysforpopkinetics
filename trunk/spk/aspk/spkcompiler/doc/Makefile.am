include ../../../../../../doc/webdir.mk
T = spkcompiler
WEB = $(WEBDIR)/v0.1/specs/$T

SUBDIRS = common nonmem

dist_pkgdata_DATA = spkcompiler.cfg \
                    main_menu.html

DRIVER_SRC = ../spkcompiler/main.cpp

html : overall_html components_html

overall_html : spkcompiler.xml
	docbook2html --dsl ~/develop/r2/doc/stylesheet/"local.dsl#html" --nochunks $<

components_html : $(DRIVER_SRC) 
	doxygen -u spkcompiler.cfg

clean : 
	rm -rf html
	rm -rf latex
	rm -rf man

all  : 
	rm -rf html
	rm -rf latex
	rm -rf man
	doxygen spkcompiler.cfg
	docbook2html --dsl ~/develop/r2/doc/stylesheet/"local.dsl#html" --nochunks spkcompiler.xml

install:
	scp *.html $(WEB)
	scp html/*.html $(WEB)/html
	scp nonmem/html/*.html $(WEB)/nonmem/html
	scp common/html/*.html $(WEB)/common/html
