bin_PROGRAMS = test_common

COMMON = common
MOSTLYCLEANFILES = IndData.h IndData.cpp
CXXFLAGS = -g

test_common_SOURCES = \
	../config.h \
	test_common.cpp \
        choleskyTest.cpp \
	choleskyTest.h \
	lowerTest.cpp \
        lowerTest.h \
        upperTest.cpp \
        upperTest.h \
	seriesTest.cpp \
	clientTest.cpp \
        clientTest.h \
        SymbolTest.cpp \
        SymbolTest.h \
        SymbolTableTest.cpp \
	SymbolTableTest.h \
	ClientTranslatorTest.cpp \
	ClientTranslatorTest.h

test_common_INCLUDES = \
	clientTest.h \
	choleksyTest.h \
        lowerTest.h \
        upperTest.h \
        seriesTest.h \
        SymbolTest.h \
	SymbolTableTest.h \
	ClientTranslatorTest.h

AM_LDFLAGS = -lxerces-c -lcppunit -lspkcompiler -latlas_lapack -lcblas -latlas -lm

INCLUDES = \
	-I@top_srcdir@/ \
	-I@top_srcdir@/spkcompiler/ \
	-I@top_srcdir@/spkcompiler/nonmem
