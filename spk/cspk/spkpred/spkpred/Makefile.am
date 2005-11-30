lib_LTLIBRARIES = libspkpred.la 

INST_BASE = /usr/local

libdir        = $(INST_BASE)/lib/spktest
pkgincludedir = $(INST_BASE)/include/spktest/spkpred

# Set C++ compiler flags
AM_CXXFLAGS = -I$(INST_BASE)/include/spktest
                                                                                
if SPK_RELEASE
AM_CXXFLAGS += -O3 -Dspk_release -DNDEBUG
else
AM_CXXFLAGS += -g  -Dspk_debug
endif

libspkpred_la_SOURCES = \
	Cov.cpp \
	DiagCov.cpp \
	FullCov.cpp \
	IndPredModel.cpp \
	isEqual.cpp \
	PopPredModel.cpp \
	Cov.h \
	DiagCov.h \
	FullCov.h \
	IndPredModel.h \
	isEqual.h \
	OdeBreak.h \
	OdePredBase.h \
	PopPredModel.h \
	PredBase.h \
	OdePredBase.h \
	OdeBreak.h \
	../config.h

pkginclude_HEADERS = \
	Cov.h \
	DiagCov.h \
	FullCov.h \
	IndPredModel.h \
	isEqual.h \
	OdeBreak.h \
	OdePredBase.h \
	PopPredModel.h \
	PredBase.h \
	OdePredBase.h \
	OdeBreak.h

