bin_PROGRAMS = test_nonmem

NONMEM = nonmem

AM_CFLAGS   = -g
AM_CPPFLAGS = -g

test_nonmem_SOURCES = \
	../config.h \
	test_nonmem.cpp \
	NonmemExpXlatorTest.cpp \
	NonmemExpXlatorTest.h \
	NonmemTranslatorTest.cpp \
	NonmemTranslatorTest.h \
        NonmemDataReaderTest.cpp \
        NonmemDataReaderTest.h
test_nonmem_INCLUDES = \
	NonmemExpXlatorTest.h \
	NonmemTranslatorTest.h \
        NonmemDataReaderTest.h

AM_LDFLAGS = -lxerces-c -lcppunit -lspkcompiler

INCLUDES = \
	-I@top_srcdir@/ \
	-I@top_srcdir@/libspkcompiler/ \
	-I@top_srcdir@/libspkcompiler/nonmem
