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
                   NonmemPars.h \
                   MontePars.h \
                   result.xml \
                   Makefile.SPK \
                   spk_error.tmp \
                   sotware_error \
                   ind_noID_dataML.xml \
                   ind_noID_sourceML.xml \
                   ind_noID_DataSetDriver \
                   ind_noID_DataSetDriver.cpp \
                   ind_noID_IndDataDriver \
                   ind_noID_IndDataDriver.cpp \
                   ind_noID_PredDriver \
                   ind_noID_PredDriver.cpp \
                   ind_withID_dataML.xml \
                   ind_withID_sourceML.xml \
                   ind_withID_DataSetDriver \
                   ind_withID_DataSetDriver.cpp \
                   ind_withID_IndDataDriver \
                   ind_withID_IndDataDriver.cpp \
                   ind_withID_PredDriver \
                   ind_withID_PredDriver.cpp \
                   pop_dataML.xml \
                   pop_sourceML.xml \
                   pop_DataSetDriver \
                   pop_DataSetDriver.cpp \
                   pop_IndDataDriver \
                   pop_IndDataDriver.cpp \
                   pop_PredDriver \
                   pop_PredDriver.cpp

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
