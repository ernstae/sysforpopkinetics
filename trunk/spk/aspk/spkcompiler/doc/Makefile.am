SUFFIXES = .dox
.html.dox : 
	doxygen $<

dist_pkgdata_DATA = core.cfg \
                    nonmem.cfg \
                    spkcompiler.cfg \
                    nonmem_menu.html \
                    core_menu.html \
                    menu.html

CORE_SRC   = ../spkcompiler/client.h \
             ../spkcompiler/Symbol.h \
             ../spkcompiler/SymbolTable.h \
             ../spkcompiler/ClientTranslator.h \
             ../spkcompiler/SpkCompilerError.h \
             ../spkcompiler/SpkCompilerException.h \
             ../spkcompiler/upper.h \
             ../spkcompiler/lower.h

NONMEM_SRC = ../spkcompiler/nonmem/explang.cpp \
	     ../spkcompiler/nonmem/lex_explang.c \
	     ../spkcompiler/nonmem/explang.ypp \
	     ../spkcompiler/nonmem/lex_explang.l \
             ../spkcompiler/nonmem/nonmem.h

DRIVER_SRC = ../spkcompiler/main.cpp

html : nonmem_html core_html compiler_html 

nonmem_html: nonmem.cfg $(NONMEM_SRC)
	doxygen nonmem.cfg

core_html: core.cfg $(CORE_SRC)
	doxygen core.cfg

compiler_html : $(DRIVER_SRC) nonmem_html core_html 
	doxygen spkcompiler.cfg

clean : 
	rm -rf html
	rm -rf latex
	rm -rf man
	rm -rf core/html
	rm -rf nonmem/html

all  : 
	rm -rf html
	rm -rf latex
	rm -rf man
	rm -rf core/html
	rm -rf nonmem/html
	doxygen nonmem.cfg
	doxygen core.cfg
	doxygen spkcompiler.cfg
