bin_PROGRAMS = testall

INST_BASE = /usr/local

libdir = $(INST_BASE)/lib/spktest

# Set C pre-processor flags
AM_CPPFLAGS = \
	-I. \
	-I$(INST_BASE)/include/spktest \
	-I$(INST_BASE)/cppunit/include 

# Set C++ compiler flages to build a 64-bit version
CXX_FLAGS_64_BIT = 
if [ `uname -i` = "x86_64" ]; then  CXX_FLAGS_64_BIT = -DBA0_64BITS; fi

# Set C++ compiler flags
if SPK_RELEASE
AM_CXXFLAGS = -O3 -Dspk_release -DNDEBUG $(CXX_FLAGS_64_BIT)
else
AM_CXXFLAGS = -g  -Dspk_debug $(CXX_FLAGS_64_BIT)
endif
if SPK_PROFILE
AM_CXXFLAGS += -pg -Dspk_profiling $(CXX_FLAGS_64_BIT)
endif

AM_LDFLAGS  = \
	-L/usr/local/lib \
	-L/usr/lib/atlas \
	-L$(INST_BASE)/lib/spktest \
	$(top_builddir)/spkpred/libspkpred.a \
	-lspk \
	-lQN01Box \
	-llapack \
	-llapack_atlas \
	-lcblas \
	-latlas \
	-ldl \
	-lpthread \
	-lm \
	-lcppunit \
	-lxerces-c \
	-lcln \
	-lginac \
	-lbad \
	-lbap \
	-lbav \
	-lba0 \
	-lgsl \
	-lgslcblas

testall_SOURCES = \
	calcGroebnerBasisTest.cpp \
	compareToKnown.cpp \
	DiagCovTest.cpp \
	FullCovTest.cpp \
	BlkDiagCovTest.cpp \
	IdentPredBaseTest.cpp \
	IndPredModelTest.cpp \
	IndPredModelBaseTest.cpp \
	OdeBreakTest.cpp \
	OdePredBaseTest.cpp \
	PopPredModelTest.cpp \
	PopPredModelBaseTest.cpp \
	predTwoStageMethodTest.cpp \
	testall.cpp \
	calcGroebnerBasisTest.h \
	compareToKnown.h \
	DiagCovTest.h \
	FullCovTest.h \
	BlkDiagCovTest.h \
	IdentPredBaseTest.h \
	IndPredModelTest.h \
	IndPredModelBaseTest.h \
	OdeBreakTest.h \
	OdePredBaseTest.h \
	PopPredModelTest.h \
	PopPredModelBaseTest.h \
	predTwoStageMethodTest.h \
	$(top_builddir)/config.h

pkginclude_HEADERS = \
	calcGroebnerBasisTest.h \
	compareToKnown.h \
	DiagCovTest.h \
	FullCovTest.h \
	BlkDiagCovTest.h \
	IdentPredBaseTest.h \
	IndPredModelTest.h \
	IndPredModelBaseTest.h \
	OdeBreakTest.h \
	OdePredBaseTest.h \
	PopPredModelTest.h \
	PopPredModelBaseTest.h \
	predTwoStageMethodTest.h \
	@top_builddir@/config.h
