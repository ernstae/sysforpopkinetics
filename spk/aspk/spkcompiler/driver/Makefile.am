bin_PROGRAMS = spkcompiler

MOSTLYCLEANFILES = Pred.h predEqn.cpp result.xml driver.cpp IndData.h DataSet.h spk_error.tmp \
                   driver compilation_error.xml

CXXFLAGS = -g -dD

spkcompiler_SOURCES = \
	../config.h \
	spkcompiler.cpp

AM_LDFLAGS = -lxerces-c -lspkcompiler

INCLUDES = \
	-I@top_srcdir@/ \
	-I@top_srcdir@/spkcompiler/ \
	-I@top_srcdir@/spkcompiler/nonmem
