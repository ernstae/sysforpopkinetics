bin_PROGRAMS = testall

INST_BASE = /usr/local

libdir = $(INST_BASE)/lib/spktest

AM_CPPFLAGS = -I$(INST_BASE)/include/spktest -I/$(INST_BASE)/cppunit/include
AM_LDFLAGS  = -L$(INST_BASE)/lib/spktest -lspkpred -lspk -lspkopt -llapack -latlas_lapack -lcblas -latlas -lpthread -lm -lcppunit

testall_SOURCES = \
	compareToKnown.cpp \
	DiagCovTest.cpp \
	FullCovTest.cpp \
	IndPredModelTest.cpp \
	PopPredModelTest.cpp \
	testall.cpp \
	compareToKnown.h \
	DiagCovTest.h \
	FullCovTest.h \
	IndPredModelTest.h \
	PopPredModelTest.h \
	../config.h

pkginclude_HEADERS = \
	compareToKnown.h \
	DiagCovTest.h \
	FullCovTest.h \
	IndPredModelTest.h \
	PopPredModelTest.h
