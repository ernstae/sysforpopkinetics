#*************************************************************************
#
# File: Makefile.am
#
# This file can be used with automake to generate the Makefiles for the
# identifiability driver.
#
# Author: Mitch Watrous
#
#*************************************************************************

# Set the executable to be built.
bin_PROGRAMS = check_ident

# Set the directory where the executable will installed.
bindir = ${prefix}/bin

# Set the flags for the C pre-processor.
AM_CPPFLAGS = \
	-I. \
	-I${prefix}/include

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
	-L${prefix}/lib \
	-lcln \
	-lginac \
	-lbad \
	-lbap \
	-lbav \
	-lba0 \
	-lxerces-c \
	-lident

# Set the files that make up the identifiability driver.
check_ident_SOURCES = \
	check_ident.cpp


