SUBDIRS = nonmem
lib_LTLIBRARIES = libspkcompiler.la

pkgincludedir = @includedir@/spkcompiler
NONMEM      = nonmem
INCLUDES    = -I@top_srcdir@/ -I$(NONMEM)

AM_LFLAGS   = -i
AM_YFLAGS   = -d --verbose --debug
CXXFLAGS    = -g
AM_LDFLAGS  = ./nonmem/libnonmem.la
MOSTLYCLEANFILES = explang.cpp explang.output lex_explang.c

libspkcompiler_la_SOURCES = \
        config.h \
	Symbol.cpp \
	Symbol.h \
	SymbolTable.cpp \
	SymbolTable.h \
	ClientTranslator.cpp \
	ClientTranslator.h 

nobase_pkginclude_HEADERS = \
	$(NONMEM)/nonmem.h \
	$(NONMEM)/explang.h \
	$(NONMEM)/NonmemTranslator.h

pkginclude_HEADERS = \
	Symbol.h \
	SymbolTable.h \
	client.h \
	ClientTranslator.h

