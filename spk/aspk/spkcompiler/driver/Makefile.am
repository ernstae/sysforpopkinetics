bin_PROGRAMS = spkcompiler

CXXFLAGS = -g -dD

spkcompiler_SOURCES = \
	../config.h \
	compiler.cpp

AM_LDFLAGS = -lxerces-c -lspkcompiler -latlas_lapack -lcblas -latlas -lpthread -lm

INCLUDES = \
	-I@top_srcdir@/ \
	-I@top_srcdir@/spkcompiler/ \
	-I@top_srcdir@/spkcompiler/nonmem
