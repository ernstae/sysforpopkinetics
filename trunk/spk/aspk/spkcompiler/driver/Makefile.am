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

# Unfortunately, perhaps due to autoconf's bug, a library path /usr/local/lib has to be
# explicitly passed to the linker by using -Wl option.  It is supposed to be infered
# by autoconf.
AM_LDFLAGS = -lspkcompiler -lxerces-c -L$(INST_BASE)/lib -L$(INST_BASE)/lib/spktest -Wl,--rpath -Wl,/usr/local/lib

INCLUDES = \
	-I@top_srcdir@/ \
        -I$(INST_BASE)/include
	-I$(INST_BASE)/include/spktest
