noinst_LTLIBRARIES = libnonmem.la
pkgincludedir = @includedir@/spkcompiler
INCLUDES      = -I@top_srcdir@/ -I..

AM_LFLAGS     = -i -Pnm_ -olex.yy.c
AM_YFLAGS     = -d --verbose --debug -p nm_
CXXFLAGS      = -g -I..

MOSTLYCLEANFILES = explang.cpp explang.h explang.output lex_explang.c

libnonmem_la_SOURCES = \
	config.h \
	explang.ypp \
        lex_explang.l \
	NonmemTranslator.cpp \
	NonmemTranslator.h
