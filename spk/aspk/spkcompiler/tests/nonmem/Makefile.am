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
	ind_withID_NonmemTranslatorTest.cpp \
	ind_withID_NonmemTranslatorTest.h \
        ind_noID_NonmemTranslatorTest.cpp \
	ind_noID_NonmemTranslatorTest.h \
        ind_onlysim_NonmemTranslatorTest.cpp \
	ind_onlysim_NonmemTranslatorTest.h \
	NonmemTranslatorPopTest.cpp \
	NonmemTranslatorPopTest.h

test_nonmem_INCLUDES = \
	lex_explangTest.h \
	explangTest.h \
	ind_withID_NonmemTranslatorTest.h \
	ind_noID_NonmemTranslatorTest.h \
	ind_onlysim_NonmemTranslatorTest.h \
	NonmemTranslatorPopTest.h

AM_LDFLAGS = -lxerces-c -lcppunit -L$(INST_BASE)/lib/spktest -lspkcompiler 

INCLUDES = \
	-I@top_srcdir@/ \
        -I$(INST_BASE)/include/spktest
