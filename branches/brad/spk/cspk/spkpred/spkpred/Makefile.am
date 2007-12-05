# Static library

lib_LIBRARIES = libspkpred.a 

INST_BASE = /usr/local

libdir        = $(INST_BASE)/lib/spktest
pkgincludedir = $(INST_BASE)/include/spktest/spkpred

# Set C pre-processor flags
AM_CPPFLAGS = \
	-I. \
	-I$(top_builddir) \
	-I$(INST_BASE)/include/spktest \
	-I/usr/share/pvm3/include
                     
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

libspkpred_a_SOURCES = \
	Cov.cpp \
	DiagCov.cpp \
	DiagCovBase.cpp \
	FullCov.cpp \
	FullCovBase.cpp \
	BlkDiagCov.cpp \
	BlkDiagCovBase.cpp \
	IndPredModel.cpp \
	IndPredModelBase.cpp \
	isEqual.cpp \
	PopPredModel.cpp \
	PopPredModelBase.cpp \
	predNonparamMethod.cpp \
	predTwoStageMethod.cpp \
	Cov.h \
	DiagCov.h \
	DiagCovBase.h \
	FullCov.h \
	FullCovBase.h \
	BlkDiagCov.h \
	BlkDiagCovBase.h \
	IndPredModel.h \
	IndPredModelBase.h \
	isEqual.h \
	OdeBreak.h \
	OdePredBase.h \
	PopPredModel.h \
	PopPredModelBase.h \
	PredBase.h \
	predNonparamMethod.h \
	predTwoStageMethod.h \
	$(top_builddir)/config.h
# [Revisit - Identifiability Files Should not be Included in the Release - Mitch]
#	calcGroebnerBasis.h \
#	calcGroebnerBasis.c \
#	IdentPredBase.h \
# [Revisit - Identifiability Files Should not be Included in the Release - Mitch]

pkginclude_HEADERS = \
	Cov.h \
	DiagCov.h \
	DiagCovBase.h \
	FullCov.h \
	FullCovBase.h \
	BlkDiagCov.h \
	BlkDiagCovBase.h \
	IndPredModel.h \
	IndPredModelBase.h \
	isEqual.h \
	OdeBreak.h \
	OdePredBase.h \
	PopPredModel.h \
	PopPredModelBase.h \
	PredBase.h \
	predNonparamMethod.h \
	predTwoStageMethod.h
# [Revisit - Identifiability Files Should not be Included in the Release - Mitch]
#	calcGroebnerBasis.h \
#	IdentPredBase.h \
# [Revisit - Identifiability Files Should not be Included in the Release - Mitch]

