bin_PROGRAMS = test_nonmem

NONMEM = nonmem

CXXFLAGS = -g

test_nonmem_SOURCES = \
	../config.h \
	test_nonmem.cpp \
        FitPopulationTestModel.cpp \
        FitPopulationTestModel.h \
	NonmemExpXlatorTest.cpp \
	NonmemExpXlatorTest.h \
	read_nonmem_driverTest.cpp \
	read_nonmem_driverTest.h \
	read_nonmem_dataTest.cpp \
	read_nonmem_dataTests.h \
	read_nonmem_modelTest.cpp \
	read_nonmem_modelTest.h \
        emit_nonmem_driverTest.cpp \
	emit_nonmem_driverTest.h 

test_nonmem_INCLUDES = \
	FitPopulationTestModel.h \
	NonmemExpXlatorTest.h \
	read_nonmem_driverTest.h \
	read_nonmem_dataTest.h \
	read_nonmem_modelTest.h \
        emit_nonmem_driverTest.h

AM_LDFLAGS = -lspk /usr/local/lib/libnagc.a -lpthread -lm -lxerces-c -lcppunit -lspkcompiler

INCLUDES = \
	-I@top_srcdir@/ \
	-I@top_srcdir@/libspkcompiler/ \
	-I@top_srcdir@/libspkcompiler/nonmem
