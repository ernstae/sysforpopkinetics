SUFFIXES = .dox
.html.dox : 
	doxygen $<

dist_pkgdata_DATA = common.cfg \
                    common_menu.html

CORE_SRC   = ../../spkcompiler/client.h \
             ../../spkcompiler/Symbol.h \
             ../../spkcompiler/SymbolTable.h \
             ../../spkcompiler/ClientTranslator.h \
             ../../spkcompiler/SpkCompilerError.h \
             ../../spkcompiler/SpkCompilerException.h \
             ../../spkcompiler/upper.h \
             ../../spkcompiler/lower.h

html : common_html  

common_html: common.cfg $(NONMEM_SRC)
	doxygen common.cfg

clean : 
	rm -rf html
	rm -rf latex
	rm -rf man

all  : 
	rm -rf html
	rm -rf latex
	rm -rf man
	doxygen common.cfg
