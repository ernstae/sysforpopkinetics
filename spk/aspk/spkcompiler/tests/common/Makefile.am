bin_PROGRAMS = test_common

COMMON = common

AM_CFLAGS   = -g
AM_CPPFLAGS = -g

test_common_SOURCES = \
	../config.h \
	test_common.cpp \
	clientTest.cpp \
        clientTest.h \
	ExpTreeGeneratorTest.cpp \
	ExpTreeGeneratorTest.h \
	SpkCompilerUtilTest.cpp \
	SymbolTest.cpp \
	SymbolTest.h \
	SymbolTableTest.cpp \
	SymbolTableTest.h \
	read_contentTest.cpp \
	read_contentTest.h \
        emit_IndDataTest.cpp \
	emit_IndDataTest.h \
        emit_driverTest.cpp \
        emit_driverTest.h \
        SpkMLToCppTest.cpp \
        SpkMLToCppTest.h

test_common_INCLUDES = \
	clientTest.h \
	ExpTreeGeneratorTest.h \
	ExpNodeCarrierTest.h \
	SpkCompilerUtilTest.h \
	SymbolTest.h \
	SymbolTableTest.h \
	read_contentTest.h \
        emit_IndDataTest.h \
        emit_driverTest.h \
        SpkMLToCppTest.h

AM_LDFLAGS = -lxerces-c -lcppunit -lspkcompiler

INCLUDES = \
	-I@top_srcdir@/ \
	-I@top_srcdir@/libspkcompiler/ \
	-I@top_srcdir@/libspkcompiler/nonmem
