SUBDIRS = nonmem
lib_LTLIBRARIES = libspkcompiler.la

NONMEM        = nonmem
INST_BASE     = /usr/local
#INCLUDES      = -I@top_srcdir@/ -I$(NONMEM) -I$(INST_BASE)/include/spktest

libdir        = $(INST_BASE)/lib/spktest
pkgincludedir = $(INST_BASE)/include/spktest/spkcompiler

# Clear C/C++ compiler global flags
CXXFLAGS         = 

# Set C/C++ compiler flags to the values we intend to set.
AM_CXXFLAGS      = -g -I$(NONMEM) -I$(INST_BASE)/include/spktest

# Set linker flags
AM_LDFLAGS       = ./nonmem/libnonmem.la

# LEX flags
AM_LFLAGS        = -i

# YACC flags
AM_YFLAGS        = -d --verbose --debug


# List of extra files to be cleaned.
MOSTLYCLEANFILES = explang.cpp explang.output lex_explang.c

libspkcompiler_la_SOURCES = \
        config.h \
        series.h \
        series.cpp \
	lower.h \
	lower.cpp \
	upper.h \
	upper.cpp \
	Symbol.cpp \
	Symbol.h \
	SymbolTable.cpp \
	SymbolTable.h \
	ClientTranslator.cpp \
	ClientTranslator.h \
	SpkCompilerError.cpp \
	SpkCompilerError.h \
	SpkCompilerException.cpp \
	SpkCompilerException.h 

nobase_pkginclude_HEADERS = \
	$(NONMEM)/nonmem.h \
	$(NONMEM)/explang.h \
	$(NONMEM)/NonmemTranslator.h

pkginclude_HEADERS = \
        series.h \
	lower.h \
	upper.h \
	Symbol.h \
	SymbolTable.h \
	client.h \
	ClientTranslator.h \
	SpkCompilerError.h \
	SpkCompilerException.h

