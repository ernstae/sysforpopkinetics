# This makefile is counting on the files 
# Pred.h, DataSet.h, IndData.h and NonmemPars.h 
# being properly generated in some separate fashion.
#
CPPFLAGS = \
	-g \
	-c \
	-I/usr/local/include/spktest/ \
	-I. \
	-I..

INCLUDE = \
	../AnalyticIntegral.h \
	../GridIntegral.h \
	../MontePopObj.h \
	../MapBay.h \
	../MapMonte.h \
	Pred.h \
	DataSet.h \
	IndData.h \
	NonmemPars.h \
	MontePars.h
#
OBJECT = \
	../monteDriver.o \
	../AnalyticIntegral.o \
	../GridIntegral.o \
	../MontePopObj.o \
	../MapBay.o \
	../MapMonte.o

monteDriver : $(OBJECT) $(INCLUDE)
	g++ -g $(OBJECT) -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -o monteDriver -lspk -lspkopt -lspkpred -lgsl -latlas_lapack -lcblas -latlas -lpthread -lm

# Rule for compiling C++ soruce files
%.o : %.cpp $(INCLUDE) 
	g++ $(CPPFLAGS) $< -o $@

# Rule for compiling C soruce files
%.o : %.c $(INCLUDE) 
	gcc $(CPPFLAGS) $< -o $@
