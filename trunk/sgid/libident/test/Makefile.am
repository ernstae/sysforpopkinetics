\#*************************************************************************
#
# File: Makefile.am
#
# This file can be used with automake to generate the Makefile for the
# unit tests for the identifiability library.
#
# Author: Mitch Watrous
#
#*************************************************************************

# Set the executable to be built.  Note that it won't be installed.
noinst_PROGRAMS = test_libident

# Set the flags for the C pre-processor.
AM_CPPFLAGS = -I. -I../src

# Set the flags for the C++ compiler depending on whether or not the
# debug mode was been enabled when configure was called.
if !DEBUG
AM_CXXFLAGS = -O3 -DNDEBUG
else
AM_CXXFLAGS = -g
endif

# Set the flags for the linker.
AM_LDFLAGS = \
	-L/usr/lib \
	../src/libident.a \
	-lcln \
	-lginac \
	-lbad \
	-lbap \
	-lbav \
	-lba0 \
 	-lcppunit \
	-lxerces-c \
	-lm \
	-ldl

# Set the files that make up the set of unit tests.
test_libident_SOURCES = \
	calcexhaustsummaryTest.cpp \
	calcGroebnerBasisTest.cpp \
	checkParamIdentTest.cpp \
	calcexhaustsummaryTest.h \
	calcGroebnerBasisTest.h \
	checkParamIdentTest.h \
	test_libident.cpp

# Set the header files for the set of unit tests.
noinst_HEADERS = \
	${top_builddir}/config.h \
	calcexhaustsummaryTest.h \
	calcGroebnerBasisTest.h \
	checkParamIdentTest.h
