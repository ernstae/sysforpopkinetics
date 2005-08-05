noinst_PROGRAMS      = testall
AM_LDFLAGS           = -lxerces-c -lcppunit ../../spkcompiler/libcommon.a
AM_CPPFLAGS          = -g -I@top_srcdir@
MOSTLYCLEANFILES     = IndData.h 
MAINTEINERCLEANFILES = Makefile.in
testall_DEPENDENCIES = ../../spkcompiler/libcommon.a 

AM_CXXFLAGS = -g

testall_SOURCES = \
	test_common.cpp \
	lowerTest.cpp \
        lowerTest.h \
        upperTest.cpp \
        upperTest.h \
	seriesTest.cpp \
	seriesTest.h \
	clientTest.cpp \
        clientTest.h \
        SymbolTest.cpp \
        SymbolTest.h \
        SymbolTableTest.cpp \
	SymbolTableTest.h \
        SpkCompilerErrorTest.cpp \
        SpkCompilerErrorTest.h \
        SpkCompilerExceptionTest.cpp \
        SpkCompilerExceptionTest.h \
	ClientTranslatorTest.cpp \
	ClientTranslatorTest.h 

