#!/bin/bash

include ../../../../doc/webdir.mk

T        = spkpredLib
HTML_DIR = html
WEB      = $(WEBDIR)/v0.1/specs/$T

VPATH = ../spkpred

SPKPRED_SRC = \
	BlkDiagCov.cpp \
	BlkDiagCovBase.cpp \
	Cov.cpp \
	DiagCov.cpp \
	DiagCovBase.cpp \
	FullCov.cpp \
	FullCovBase.cpp \
	IndPredModel.cpp \
	IndPredModelBase.cpp \
	isEqual.cpp \
	PopPredModel.cpp \
	PopPredModelBase.cpp \
	predTwoStageMethod.cpp \
	BlkDiagCov.h \
	BlkDiagCovBase.h \
	Cov.h \
	DiagCov.h \
	DiagCovBase.h \
	FullCov.h \
	FullCovBase.h \
	IndPredModel.h \
	IndPredModelBase.h \
	isEqual.h \
	OdeBreak.h \
	OdePredBase.h \
	PopPredModel.h \
	PopPredModelBase.h \
	PredBase.h \
	predTwoStageMethod.h

all:	html

html: Doxyfile $(SPKPRED_SRC) 
	rm -f *.cpp *.h
	rm -f $(HTML_DIR)/*.png
	doxygen Doxyfile
	./RunOMhelp

install:
	scp $(HTML_DIR)/* $(WEB)

clean:
	rm -f -r $(HTML_DIR)
	rm -f -r *.tag
	rm -f *.cpp *.h

