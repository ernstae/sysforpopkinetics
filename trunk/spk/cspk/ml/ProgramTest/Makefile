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
	MapMonte.o

All : Sim monteDriver

Sim : Sim.cpp
	g++ -g Sim.cpp -lgsl -lcblas -latlas -o Sim

monteDriver : monteDriver.cpp $(OBJECT) $(INCLUDE)
	g++ -g $(OBJECT) -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -o monteDriver -lspk -lxerces-c -lspkopt -lspkpred -lgsl -latlas_lapack -lcblas -latlas -lpthread -lm

# Rule for compiling C++ source files
monteDriver.o : ../monteDriver.cpp $(INCLUDE)
	g++ $(CPPFLAGS) ../monteDriver.cpp 
