bin_PROGRAMS    = test_nonmem
noinst_PROGRAMS = test_nonmem
INST_BASE       = /usr/local
NONMEM          = nonmem

CXXFLAGS = -g
MOSTLYCLEANFILES = Pred.h \
                   predEqn.cpp \
                   predEqn.fortran \
                   IndData.h \
                   DataSet.h \
                   test_nonmem \
                   spkDriver.cpp \
                   spkDriver \
                   monteDriver.cpp \
                   monteDriver \
                   NonmemPars.h \
                   MontePars.h \
                   result.xml \
                   Makefile.SPK \
                   spk_error.tmp \
                   sotware_error 

test_nonmem_SOURCES = \
	../config.h \
	test_nonmem.cpp \
	lex_explangTest.cpp \
	lex_explangTest.h \
	explangTest.cpp \
	explangTest.h \
        ind_withIDTest.cpp \
        ind_withIDTest.h

test_nonmem_INCLUDES = \
	lex_explangTest.h \
	explangTest.h \
        ind_withIDTest.h

AM_LDFLAGS = -lxerces-c -lcppunit -L$(INST_BASE)/lib/spktest -lspkcompiler 

INCLUDES = \
	-I@top_srcdir@/ \
        -I$(INST_BASE)/include/spktest
