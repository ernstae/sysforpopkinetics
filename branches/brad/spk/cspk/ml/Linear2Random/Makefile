# This makefile is counting on the files 
# Pred.h, DataSet.h, IndData.h and NonmemPars.h 
# being properly generated in some separate fashion.
#
# search parent directory for source files
VPATH = ..
#
CPPFLAGS = \
	-g \
	-c \
	-I/usr/local/include/spktest/ \
	-I. \
	-I..

INCLUDE = \
	AnalyticIntegral.h \
	GridIntegral.h \
	MontePopObj.h \
	MapBay.h \
	MapMonte.h \
	Gsl2SpkError.h \
	Pred.h \
	DataSet.h \
	IndData.h \
	NonmemPars.h \
	MontePars.h
#
OBJECT = \
	monteDriver.o \
	AnalyticIntegral.o \
	GridIntegral.o \
	MontePopObj.o \
	MapBay.o \
	Gsl2SpkError.o \
	MapMonte.o \
	AdaptIntegral.o \
	adapt.o

All : monteDriver

Sim : Sim.cpp
	g++ \
		-g \
		-I/usr/local/include/spktest \
		-L/usr/local/lib/spktest \
		Sim.cpp \
		-lQN01Box \
		-lCppAD \
		-lgsl \
		-llapack \
		-lcblas \
		-latlas \
		-lg2c \
		-o Sim

monteDriver : monteDriver.cpp $(OBJECT) $(INCLUDE)
	g++ -g $(OBJECT) -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -Wl,--rpath -Wl,/usr/local/lib -L/usr/local/lib -o monteDriver -lspk -lspkpred -lQN01Box -lCppAD -lgsl -latlas_lapack -lcblas -latlas -lpthread -lm -lxerces-c
