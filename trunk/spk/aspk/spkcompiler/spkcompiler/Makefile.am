SUBDIRS          = nonmem .
bin_PROGRAMS     = spkcompiler
bindir           = /usr/local/bin/spktest
noinst_LIBRARIES = libcommon.a
AM_CXXFLAGS      = -I./nonmem
AM_LDFLAGS       = -lxerces-c ./libcommon.a nonmem/libnonmem.a

MOSTLYCLEANFILES = libcommon.a 

spkcompiler_SOURCES = \
	main.cpp

libcommon_a_SOURCES = \
        client.h \
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
        SpkCompilerException.h \
        SpkCompilerException.cpp 
