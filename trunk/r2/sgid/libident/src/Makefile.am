#*************************************************************************
#
# File: Makefile.am
#
# This file can be used with automake to generate the Makefile for the
# identifiability library.
#
# Author: Mitch Watrous
#
#*************************************************************************

# Set the libraries to be built.
lib_LIBRARIES = libident.a

# Set the directory where the library will be installed.
libdir    = ${prefix}/lib
pkglibdir = ${prefix}/lib

# Set the directory where the library's header files will be installed.
includedir =    ${prefix}/include
pkgincludedir = ${prefix}/include

# Set the flags for the C pre-processor.
AM_CPPFLAGS = -I.

# Set the flags for the C++ compiler depending on whether or not the
# debug mode was been enabled when configure was called.
if !DEBUG
AM_CXXFLAGS = -O3 -DNDEBUG
else
AM_CXXFLAGS = -g
endif

# Set the files that make up the library.
libident_a_SOURCES = \
	calcExhaustSummary.c \
	calcExhaustSummary.h \
	calcGroebnerBasis.c \
	calcGroebnerBasis.h \
	checkParamIdent.cpp \
	checkParamIdent.h \
	IdentException.h

# Set the header files the library.
pkginclude_HEADERS = \
	${top_builddir}/config.h \
	calcExhaustSummary.h \
	calcGroebnerBasis.h \
	checkParamIdent.h \
	IdentException.h
