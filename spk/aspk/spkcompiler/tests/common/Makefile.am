bin_PROGRAMS = test_common

COMMON = common

AM_CFLAGS   = -g
AM_CPPFLAGS = -g

test_common_SOURCES = \
	../config.h \
	test_common.cpp \
	ExpTreeGeneratorTest.cpp \
	ExpTreeGeneratorTest.h \
	SpkCompilerUtilTest.cpp \
	SymbolTest.cpp \
	SymbolTest.h \
	SymbolTableTest.cpp \
	SymbolTableTest.h

test_common_INCLUDES = \
	ExpTreeGeneratorTest.h \
	ExpNodeCarrier.h \
	SpkCompilerUtilTest.h \
	SymbolTest.h \
	SymbolTableTest.h

AM_LDFLAGS = -lxerces-c -lcppunit -lspkcompiler

INCLUDES = \
	-I@top_srcdir@/ \
	-I@top_srcdir@/libspkcompiler/ \
	-I@top_srcdir@/libspkcompiler/nonmem
