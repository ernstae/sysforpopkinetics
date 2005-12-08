bin_PROGRAMS = testall

INST_BASE = /usr/local

libdir = $(INST_BASE)/lib/spktest

# Set C pre-processor flags
AM_CPPFLAGS = \
	-I. \
	-I$(INST_BASE)/include/spktest \
	-I$(INST_BASE)/cppunit/include 

# Set C++ compiler flags
if SPK_RELEASE
AM_CXXFLAGS = -O3 -Dspk_release -DNDEBUG
else
AM_CXXFLAGS = -g  -Dspk_debug
endif
if SPK_PROFILE
AM_CXXFLAGS += -pg -Dspk_profiling
endif

AM_LDFLAGS  = \
	-L/usr/local/lib \
	-L$(INST_BASE)/lib/spktest \
	$(top_builddir)/spkpred/libspkpred.a \
	-lCppAD \
	-lspk \
	-lQN01Box \
	-llapack \
	-lg2c \
	-latlas_lapack \
	-lcblas \
	-latlas \
	-lpthread \
	-lm \
	-lcppunit \
	-lxerces-c

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
