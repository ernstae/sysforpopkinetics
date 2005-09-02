bin_PROGRAMS = testall

INST_BASE = /usr/local

libdir = $(INST_BASE)/lib/spktest

AM_CPPFLAGS = -I$(INST_BASE)/include/spktest -I$(INST_BASE)/cppunit/include -I$(INST_BASE)/include/spktest/CppAD
AM_LDFLAGS  = -L/usr/local/lib -L$(INST_BASE)/lib/spktest -lspkpred -lCppAD -lspk -lspkopt -llapack -latlas_lapack -lcblas -latlas -lpthread -lm -lcppunit -lxerces-c

testall_SOURCES = \
	compareToKnown.cpp \
	DiagCovTest.cpp \
	FullCovTest.cpp \
	IndPredModelTest.cpp \
	OdeBreakTest.cpp \
	OdePredBaseTest.cpp \
	PopPredModelTest.cpp \
	testall.cpp \
	compareToKnown.h \
	DiagCovTest.h \
	FullCovTest.h \
	IndPredModelTest.h \
	OdeBreakTest.h \
	OdePredBaseTest.h \
	PopPredModelTest.h \
	../config.h

pkginclude_HEADERS = \
	compareToKnown.h \
	DiagCovTest.h \
	FullCovTest.h \
	IndPredModelTest.h \
	OdeBreakTest.h \
	OdePredBaseTest.h \
	PopPredModelTest.h \
