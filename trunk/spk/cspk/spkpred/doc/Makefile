#!/bin/bash

include ../../../../../../doc/webdir.mk

T        = spkpredLib
HTML_DIR = html
WEB      = $(WEBDIR)/v0.1/specs/$T

VPATH = ../spkpred

SPKPRED_SRC = \
	BlkDiagCov.cpp \
	Cov.cpp \
	DiagCov.cpp \
	FullCov.cpp \
	IndPredModel.cpp \
	isEqual.cpp \
	PopPredModel.cpp \
	predTwoStageMethod.cpp \
	BlkDiagCov.h \
	Cov.h \
	DiagCov.h \
	FullCov.h \
	IndPredModel.h \
	isEqual.h \
	OdeBreak.h \
	OdePredBase.h \
	PopPredModel.h \
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

