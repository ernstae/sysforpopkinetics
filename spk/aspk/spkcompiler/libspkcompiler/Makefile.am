lib_LTLIBRARIES = libspkcompiler.la

NONMEM      = nonmem
INCLUDES    = -I@top_srcdir@/ -I$(NONMEM)/

AM_YFLAGS   = -d --verbose --debug
AM_CFLAGS   = -d
AM_CPPFLAGS = -d

libspkcompiler_la_SOURCES = \
        config.h \
	$(NONMEM)/explang.ypp \
	$(NONMEM)/lex_explang.l \
	$(NONMEM)/NonmemTranslator.cpp \
	$(NONMEM)/NonmemTranslator.h \
	Symbol.cpp \
	Symbol.h \
	SymbolTable.cpp \
	SymbolTable.h \
	ExpTreeGenerator.cpp \
	ExpTreeGenerator.h \
	SpkMLToCpp.cpp \
	SpkMLToCpp.h \
	SpkCompilerUtil.cpp \
	SpkCompilerUtil.h

pkginclude_HEADERS = \
        $(NONMEM)/NonmemTranslator.h \
	explang.h \
	Symbol.h \
	SymbolTable.h \
	ExpTreeGenerator.h \
	ExpNodeCarrier.h \
	client.h \
	SpkMLToCpp.h \
	SpkCompilerUtil.h

