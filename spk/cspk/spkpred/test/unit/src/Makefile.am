bin_PROGRAMS = testall

INST_BASE = /usr/local

libdir = $(INST_BASE)/lib/spktest

AM_LDFLAGS  = \
	-L/usr/local/lib \
	-L$(INST_BASE)/lib/spktest \
	 ../../../spkpred/libspkpred.la \
	-lCppAD \
	-lspk \
	-lQN01Box \
	-llapack \
	-latlas_lapack \
	-lcblas \
	-latlas \
	-lpthread \
	-lm \
	-lcppunit \
	-lxerces-c

testall_DEPENDENCIES = ../../../spkpred/libspkpred.la 

# Set C++ compiler flags
AM_CPPFLAGS = -I@top_srcdir@ -I$(INST_BASE)/include/spktest -I$(INST_BASE)/cppunit/include 
AM_CXXFLAGS = -I@top_srcdir@ -I$(INST_BASE)/include/spktest
                                                                               
if SPK_RELEASE
AM_CXXFLAGS += -O3 -Dspk_release
else
AM_CXXFLAGS += -g  -Dspk_debug
endif


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
	$(top_builddir)/config.h

pkginclude_HEADERS = \
	compareToKnown.h \
	DiagCovTest.h \
	FullCovTest.h \
	IndPredModelTest.h \
	OdeBreakTest.h \
	OdePredBaseTest.h \
	PopPredModelTest.h \
	@top_builddir@/config.h
