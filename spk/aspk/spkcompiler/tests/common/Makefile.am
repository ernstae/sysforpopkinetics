bin_PROGRAMS     = testall
noinst_PROGRAMS  = testall
COMMON = common
INST_BASE        = /usr/local

MOSTLYCLEANFILES = IndData.h \
                   IndData.cpp \
                   convertDataTestInput.dat
CXXFLAGS = -g

testall_SOURCES = \
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
	ClientTranslatorTest.h \
	SpkCompilerErrorTest.h \
	SpkCompilerErrorTest.cpp \
	SpkCompilerExceptionTest.h \
	SpkCompilerExceptionTest.cpp

testall_INCLUDES = \
	clientTest.h \
        lowerTest.h \
        upperTest.h \
        seriesTest.h \
        SymbolTest.h \
	SymbolTableTest.h \
	ClientTranslatorTest.h \
	SpkCompilerErrorTest.h \
	SpkCompilerErrorTest.cpp

AM_LDFLAGS = -lxerces-c -lcppunit -L/usr/local/lib/ -L$(INST_BASE)/lib/spktest -lspkcompiler

INCLUDES = \
	-I@top_srcdir@/ \
	-I$(INST_BASE)/include/spktest 
