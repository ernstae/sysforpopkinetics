noinst_LIBRARIES = libnonmem.a
pkgincludedir = @includedir@/spkcompiler
INCLUDES      = -I@top_srcdir@/ -I..

AM_LFLAGS     = -i -Pnm_ -olex.yy.c
AM_YFLAGS     = -d --verbose --debug -p nm_
CXXFLAGS      = -g -I..

MOSTLYCLEANFILES = explang.cpp explang.h explang.output lex_explang.c

libnonmem_a_SOURCES = \
	config.h \
        explang.ypp \
        lex_explang.l \
	nonmem.cpp \
        nonmem.h \
	NonmemTranslator.cpp \
        NonmemTranslator.h \
	read_nonmem_driver.cpp \
        read_nonmem_driver.h \
	read_nonmem_data.cpp \
        read_nonmem_data.h \
	read_nonmem_model.cpp \
        read_nonmem_model.h \
	emit_nonmem_driver.cpp \
        emit_nonmem_driver.h \
	emit_nonmem_model.cpp \
        emit_nonmem_model.h
