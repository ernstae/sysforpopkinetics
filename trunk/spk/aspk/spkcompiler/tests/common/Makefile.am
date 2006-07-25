noinst_PROGRAMS      = testall

# Linker flags
AM_LDFLAGS           = -ldl -lxerces-c -lcppunit ../../spkcompiler/libcommon.a 

# Dependencies
testall_DEPENDENCIES = $(top_builddir)/spkcompiler/libcommon.a 

# C pre-processor flags
AM_CPPFLAGS          = \
	-I. \
	-I@top_builddir@

# C++ compiler flags
if SPK_RELEASE
AM_CXXFLAGS          = -O3 -Dspk_release -DNDEBUG
else
AM_CXXFLAGS          = -g  -Dspk_debug
endif

testall_SOURCES = \
	testall.cpp \
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
	$(top_builddir)/config.h

