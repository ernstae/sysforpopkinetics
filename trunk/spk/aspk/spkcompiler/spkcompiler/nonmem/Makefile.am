noinst_LIBRARIES = libnonmem.a
AM_LFLAGS        = -i -Pnm_ -olex.yy.c
AM_YFLAGS        = -d --verbose --debug -p nm_
AM_CPPFLAGS      = -I./ -I../

MOSTLYCLEANFILES = explang.output \
                   explang.cpp \
                   explang.h \
                   lex_explang.c \
	           libnonmem.a

libnonmem_a_SOURCES = \
        explang.ypp \
        lex_explang.l \
	nonmem.cpp \
	nonmem.h \
        NonmemTranslator.cpp \
        NonmemTranslator.h

