bin_PROGRAMS = test_common

COMMON = common
MOSTLYCLEANFILES = IndData.h IndData.cpp
CXXFLAGS = -g

test_common_SOURCES = \
	../config.h \
	test_common.cpp \
	clientTest.cpp \
        clientTest.h \
	SymbolTest.cpp \
	SymbolTest.h \
        SymbolTableTest.cpp \
        SymbolTabletest.h \
	ClientTranslatorTest.cpp \
	ClientTranslatorTest.h

test_common_INCLUDES = \
	clientTest.h \
	SymbolTest.h \
	SymbolTableTest.h \
	ClientTranslatorTest.h

AM_LDFLAGS = -lxerces-c -lcppunit -lspkcompiler

INCLUDES = \
	-I@top_srcdir@/ \
	-I@top_srcdir@/spkcompiler/ \
	-I@top_srcdir@/spkcompiler/nonmem
