bin_PROGRAMS = test_nonmem

NONMEM = nonmem

CXXFLAGS = -g
MOSTLYCLEANFILES = Pred.h predEqn.cpp predEqn.fortran IndData.h DataSet.h testInd testPop test_nonmem

test_nonmem_SOURCES = \
	../config.h \
	test_nonmem.cpp \
	lex_explangTest.cpp \
	lex_explangTest.h \
	explangTest.cpp \
	explangTest.h \
	NonmemTranslatorTest.cpp \
	NonmemTranslatorTest.h

test_nonmem_INCLUDES = \
	lex_explangTest.h \
	explangTest.h \
	NonmemTranslatorTest.h

AM_LDFLAGS = -lxerces-c -lcppunit -lspkcompiler

INCLUDES = \
	-I@top_srcdir@/ \
	-I@top_srcdir@/spkcompiler/ \
	-I@top_srcdir@/spkcompiler/nonmem
