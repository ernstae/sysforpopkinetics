SUBDIRS          = nonmem .
bin_PROGRAMS     = spkcompiler
bindir           = /usr/local/bin/spktest
noinst_LIBRARIES = libcommon.a

AM_CPPFLAGS = \
	-I. \
	-I./nonmem

if SPK_RELEASE
AM_CXXFLAGS = -O3 -Dspk_release -DNDEBUG
else
AM_CXXFLAGS = -g  -Dspk_debug
endif

AM_LDFLAGS       = -lxerces-c nonmem/libnonmem.a ./libcommon.a
spkcompiler_LDADD=@LEXLIB@

MOSTLYCLEANFILES = libcommon.a 

spkcompiler_SOURCES = \
	main.cpp

## This is to make sure that the program has dependencies on the sources under nonmem/.
spkcompiler_DEPENDENCIES = \
	nonmem/libnonmem.a

dist_libcommon_a_SOURCES = \
        DOMPrint.h \
        DOMPrint.cpp \
        DOMPrintErrorHandler.cpp \
        DOMPrintErrorHandler.hpp \
        DOMPrintFilter.cpp \
        DOMPrintFilter.hpp \
        DOMTreeErrorReporter.cpp \
        DOMTreeErrorReporter.hpp \
        client.h \
        series.h \
        series.cpp \
        Symbol.cpp \
        Symbol.h \
        SymbolTable.cpp \
        SymbolTable.h \
        ClientTranslator.cpp \
        ClientTranslator.h \
        SpkCompilerError.cpp \
        SpkCompilerError.h \
        SpkCompilerException.h \
        SpkCompilerException.cpp \
        $(top_builddir)/config.h
