bin_PROGRAMS = spkcompiler

CXXFLAGS = -g -dD

spkcompiler_SOURCES = \
	../config.h \
	compiler.cpp

AM_LDFLAGS = -lxerces-c -lspkcompiler

INCLUDES = \
	-I@top_srcdir@/ \
	-I@top_srcdir@/spkcompiler/ \
	-I@top_srcdir@/spkcompiler/nonmem
