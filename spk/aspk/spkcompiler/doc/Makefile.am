include ../../../../../../doc/webdir.mk
T = spkcompiler
WEB = $(WEBDIR)/v0.1/specs/$T

SUBDIRS = common nonmem
SUFFIXES = .dox
.html.dox : 
	doxygen $<

dist_pkgdata_DATA = spkcompiler.cfg \
                    main_menu.html

DRIVER_SRC = ../spkcompiler/main.cpp

html : compiler_html 

compiler_html : $(DRIVER_SRC) 
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

install:
	scp html/*.html $(WEB)/html
	scp nonmem/html/*.html $(WEB)/nonmem/html
	scp common/html/*.html $(WEB)/common/html
