SUFFIXES = .dox
.html.dox : 
	doxygen $<

dist_pkgdata_DATA = nonmem.cfg \
                    nonmem_menu.html 

NONMEM_SRC = ../../spkcompiler/nonmem/explang.cpp \
	     ../../spkcompiler/nonmem/lex_explang.c \
	     ../../spkcompiler/nonmem/explang.ypp \
	     ../../spkcompiler/nonmem/lex_explang.l \
             ../../spkcompiler/nonmem/nonmem.h


html : nonmem_html 

nonmem_html: nonmem.cfg $(NONMEM_SRC)
	doxygen nonmem.cfg

clean : 
	rm -rf html
	rm -rf latex
	rm -rf man

all  : 
	rm -rf html
	rm -rf latex
	rm -rf man
	doxygen nonmem.cfg
