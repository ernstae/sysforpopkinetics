# This makefile is counting on the files 
# Pred.h, DataSet.h, IndData.h and NonmemPars.h 
# being properly generated in some separate fashion.
#
CPPFLAGS = \
	-g \
	-c \
        -I/usr/local/include/spktest

INSTALL_DIR = \
	/usr/local/src/spktest/ml

INCLUDE = \
	AnalyticIntegral.h \
	GridIntegral.h \
	MontePopObj.h \
	MapBay.h \
	MapMonte.h \
	Pred.h \
	DataSet.h \
	IndData.h \
	NonmemPars.h
#
OBJECT = \
	monteDriver.o \
	AnalyticIntegral.o \
	GridIntegral.o \
	MontePopObj.o \
	MapBay.o \
	MapMonte.o

# Rule for compiling C++ soruce files
%.o : %.cpp $(INCLUDES) 
	g++ $(CPPFLAGS) $< -o $@

# Rule for compiling C soruce files
%.o : %.c $(INCLUDE) 
	gcc $(CPPFLAGS) $< -o $@

monteDriver : $(OBJECT)
	g++ -g $(OBJECT) -o monteDriver -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -lspk -lspkopt -lspkpred -lgsl -latlas_lapack -lcblas -latlas -lpthread -lm

install : driver \
	  AnalyticIntegral.h \
	  AnalyticIntegral.cpp \
	  GridIntegral.h \
	  GridIntegral.cpp \
	  MontePopObj.h \
	  MontePopObj.cpp \
	  MapBay.h \
	  MapBay.cpp \
	  MapMonte.h \
	  MapMonte.cpp \
	  monteDriver.cpp
	rm -rf $(INSTALL_DIR)
	mkdir -p $(INSTALL_DIR)
	cp $? $(INSTALL_DIR)

