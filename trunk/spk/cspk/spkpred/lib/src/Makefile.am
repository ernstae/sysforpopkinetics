lib_LTLIBRARIES = libspkpred.la libspkpred_release.la

INST_BASE = /usr/local

libdir        = $(INST_BASE)/lib/spktest
pkgincludedir = $(INST_BASE)/include/spktest/spkpred

AM_CPPFLAGS = -I$(INST_BASE)/include/spktest 

libspkpred_la_CXXFLAGS         = -g
libspkpred_release_la_CXXFLAGS = -O3

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
	OdeBreak.h

libspkpred_release_la_SOURCES = \
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
	OdeBreak.h

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

