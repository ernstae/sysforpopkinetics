bin_PROGRAMS = test_nonmem

NONMEM = nonmem

CXXFLAGS = -g
MOSTLYCLEANFILES = Pred.h predEqn.cpp predEqn.fortran IndData.h DataSet.h testInd testPop test_nonmem testPopPred.cpp testPopPred testIndPred.cpp testIndPred driver.cpp driver result.xml generatedMakefile NonmemTranslatorIndTest.sourceML NonmemTranslatorPopTest.sourceML popDriver.cpp popDriver indDriver.cpp indDriver spk_error.tmp indDriverNoID testIndPredNoID

test_nonmem_SOURCES = \
	../config.h \
	test_nonmem.cpp \
	lex_explangTest.cpp \
	lex_explangTest.h \
	explangTest.cpp \
	explangTest.h \
	NonmemTranslatorIndTest.cpp \
	NonmemTranslatorIndTest.h \
	NonmemTranslatorPopTest.cpp \
	NonmemTranslatorPopTest.h

test_nonmem_INCLUDES = \
	lex_explangTest.h \
	explangTest.h \
	NonmemTranslatorIndTest.h \
	NonmemTranslatorPopTest.h

AM_LDFLAGS = -lxerces-c -lcppunit -lspkcompiler 

INCLUDES = \
	-I@top_srcdir@/ \
	-I@top_srcdir@/spkcompiler/ \
	-I@top_srcdir@/spkcompiler/nonmem
