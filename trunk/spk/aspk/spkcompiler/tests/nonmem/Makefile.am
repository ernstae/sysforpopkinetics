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
                   driver.cpp \
                   driver \
                   result.xml \
                   generatedMakefile \
                   spk_error.tmp

test_nonmem_SOURCES = \
	../config.h \
	test_nonmem.cpp \
	lex_explangTest.cpp \
	lex_explangTest.h \
	explangTest.cpp \
	explangTest.h \
	NonmemTranslatorIndTest.cpp \
        NonmemTranslatorIndTest_onlysim.cpp \
	NonmemTranslatorIndTest.h \
	NonmemTranslatorPopTest.cpp \
	NonmemTranslatorPopTest.h

test_nonmem_INCLUDES = \
	lex_explangTest.h \
	explangTest.h \
	NonmemTranslatorIndTest.h \
	NonmemTranslatorPopTest.h

AM_LDFLAGS = -lxerces-c -lcppunit -L$(INST_BASE)/lib/spktest -lspkcompiler 

INCLUDES = \
	-I@top_srcdir@/ \
        -I$(INST_BASE)/include/spktest
