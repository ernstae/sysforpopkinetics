bin_PROGRAMS     = test_common
noinst_PROGRAMS  = test_common
COMMON = common
INST_BASE        = /usr/local

MOSTLYCLEANFILES = IndData.h \
                   IndData.cpp \
                   convertDataTestInput.dat
CXXFLAGS = -g

test_common_SOURCES = \
	../config.h \
	test_common.cpp \
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
        lowerTest.h \
        upperTest.h \
        seriesTest.h \
        SymbolTest.h \
	SymbolTableTest.h \
	ClientTranslatorTest.h

AM_LDFLAGS = -lxerces-c -lcppunit -L$(INST_BASE)/lib/spktest -lspkcompiler

INCLUDES = \
	-I@top_srcdir@/ \
	-I$(INST_BASE)/include/spktest 
