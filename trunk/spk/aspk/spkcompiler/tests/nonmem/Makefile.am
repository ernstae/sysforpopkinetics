bin_PROGRAMS = test_nonmem

NONMEM = nonmem

AM_CFLAGS   = -g
AM_CPPFLAGS = -g

test_nonmem_SOURCES = \
	../config.h \
	test_nonmem.cpp \
	NonmemExpXlatorTest.cpp \
	NonmemExpXlatorTest.h \
	read_nonmem_driverTest.cpp \
	read_nonmem_driverTest.h \
	read_nonmem_dataTest.cpp \
	read_nonmem_dataTests.h \
	read_nonmem_modelTest.cpp \
	read_nonmem_modelTest.h

test_nonmem_INCLUDES = \
	NonmemExpXlatorTest.h \
	read_nonmem_driverTest.h \
	read_nonmem_dataTest.h \
	read_nonmem_modelTest.h

AM_LDFLAGS = -lxerces-c -lcppunit -lspkcompiler

INCLUDES = \
	-I@top_srcdir@/ \
	-I@top_srcdir@/libspkcompiler/ \
	-I@top_srcdir@/libspkcompiler/nonmem
