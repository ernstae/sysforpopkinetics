bin_PROGRAMS = spkcompiler
INST_BASE    = /usr/local
bindir       = $(INST_BASE)/bin/spktest

MOSTLYCLEANFILES = Pred.h \
                   predEqn.cpp \
                   result.xml \
                   driver.cpp \
                   IndData.h \
                   DataSet.h \
                   spk_error.tmp \
                   driver \
                   compilation_error.xml

CXXFLAGS   = -g

spkcompiler_SOURCES = \
	../config.h \
	spkcompiler.cpp

AM_LDFLAGS = -lxerces-c -L$(INST_BASE)/lib/spktest -lspkcompiler

INCLUDES = \
	-I@top_srcdir@/ \
	-I$(INST_BASE)/include/spktest
