# This makefile is counting on the files 
# Pred.h, DataSet.h, IndData.h and NonmemPars.h 
# being properly generated in some separate fashion.
#
CPPFLAGS = \
	-g \
	-c 

INCLUDE = \
	../AnalyticIntegral.h \
	../GridIntegral.h \
	../MontePopObj.h \
	../MapBay.h \
	../MapMonte.h \
	Pred.h \
	DataSet.h \
	IndData.h \
	NonmemPars.h
#
OBJECT = \
	monteDriver.o \
	../AnalyticIntegral.o \
	../GridIntegral.o \
	../MontePopObj.o \
	../MapBay.o \
	../MapMonte.o

# Rule for compiling C++ soruce files
%.o : %.cpp $(INCLUDES) 
	g++ $(CPPFLAGS) $< -o $@

# Rule for compiling C soruce files
%.o : %.c $(INCLUDE) 
	gcc $(CPPFLAGS) $< -o $@

monteDriver : $(OBJECT)
	g++ -g $(OBJECT) -o monteDriver -lspk -lspkopt -lspkpred -lgsl -latlas_lapack -lcblas -latlas -lpthread -lm

# Rule for local copy of monteDriver.cpp
monteDriver.cpp: ../monteDriver.cpp
	sed < ../monteDriver.cpp > monteDriver.cpp \
		-e "s%AnalyticIntegral.h%../&%" \
		-e "s%GridIntegral.h%../&%" \
		-e "s%MontePopObj.h%../&%" \
		-e "s%MapBay.h%../&%"
