bin_PROGRAMS    = testall
noinst_PROGRAMS = testall
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

testall_SOURCES = \
	../config.h \
	test_nonmem.cpp \
	lex_explangTest.cpp \
	lex_explangTest.h \
	explangTest.cpp \
	explangTest.h \
	ind_fixedParaTest.cpp \
	ind_fixedParaTest.h \
        ind_withIDTest.cpp \
        ind_withIDTest.h \
	ind_withoutIDTest.cpp \
	ind_withoutIDTest.h \
	ind_simTest.cpp \
        ind_simTest.h \
	ind_simNoEstTest.cpp \
	ind_simNoEstTest.h \
	pop_basicTest.cpp \
	pop_basicTest.h \
	pop_monteTest.cpp \
	pop_monteTest.h \
	pop_fixedParaTest.cpp \
	pop_fixedParaTest.h

testall_INCLUDES = \
	lex_explangTest.h \
	explangTest.h \
	ind_fixedParaTest.h \
        ind_withIDTest.h \
	ind_withoutIDTest.h \
	ind_simTest.h \
	ind_simNoEstTest.h \
	pop_basicTest.h \
	pop_monteTest.h \
	pop_fixedParaTest.h 

AM_LDFLAGS = -lxerces-c -lcppunit -L$(INST_BASE)/lib/spktest -lspkcompiler 

INCLUDES = \
	-I@top_srcdir@/ \
        -I$(INST_BASE)/include/spktest
