lib_LTLIBRARIES = libspkcompiler.la

NONMEM      = nonmem
INCLUDES    = -I@top_srcdir@/ -I$(NONMEM)/

AM_LFLAGS   = -i
AM_YFLAGS   = -d --verbose --debug
CXXFLAGS    = -g

MOSTLYCLEANFILES = explang.cpp explang.h explang.output lex_explang.c

libspkcompiler_la_SOURCES = \
        config.h \
	$(NONMEM)/nonmem.h \
	$(NONMEM)/nonmem.cpp \
	$(NONMEM)/explang.ypp \
	$(NONMEM)/lex_explang.l \
	$(NONMEM)/NonmemTranslator.cpp \
	$(NONMEM)/NonmemTranslator.h \
	$(NONMEM)/read_nonmem_driver.cpp \
	$(NONMEM)/read_nonmem_driver.h \
	$(NONMEM)/read_nonmem_data.cpp \
	$(NONMEM)/read_nonmem_data.h \
	$(NONMEM)/read_nonmem_model.cpp \
	$(NONMEM)/read_nonmem_model.h \
        $(NONMEM)/emit_nonmem_driver.cpp \
        $(NONMEM)/emit_nonmem_driver.h \
	read_content.h \
	read_content.cpp \
        ClientTranslator.h \
	SpkParameters.h \
	Symbol.cpp \
	Symbol.h \
	SymbolTable.cpp \
	SymbolTable.h \
	ExpTreeGenerator.cpp \
	ExpTreeGenerator.h \
	SpkMLToCpp.cpp \
	SpkMLToCpp.h \
	SpkCompilerUtil.cpp \
	SpkCompilerUtil.h \
        emit_IndData.cpp \
        emit_IndData.h

pkginclude_HEADERS = \
	$(NONMEM)/nonmem.h \
        $(NONMEM)/NonmemTranslator.h \
	$(NONMEM)/read_nonmem_driver.h \
	$(NONMEM)/read_nonmem_data.h \
	$(NONMEM)/read_nonmem_model.h \
        $(NONMEM)/emit_nonmem_driver.h \
	explang.h \
	read_content.h \
	ClientTranslator.h \
	SpkParameters.h \
	Symbol.h \
	SymbolTable.h \
	ExpTreeGenerator.h \
	ExpNodeCarrier.h \
	client.h \
	SpkMLToCpp.h \
	SpkCompilerUtil.h \
	emit_IndData.h 

