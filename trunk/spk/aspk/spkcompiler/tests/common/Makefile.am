noinst_PROGRAMS      = testall
AM_LDFLAGS           = -lxerces-c -lcppunit ../../spkcompiler/libcommon.a
MOSTLYCLEANFILES     = IndData.h 
MAINTEINERCLEANFILES = Makefile.in
testall_DEPENDENCIES = ../../spkcompiler/libcommon.a 

AM_CXXFLAGS          = -I@top_srcdir@
if SPK_RELEASE
AM_CXXFLAGS          += -O3 -Dspk_release
else
AM_CXXFLAGS          += -g  -Dspk_debug
endif

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
	../../config.h

