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
	adapt.h \
	AdaptIntegral.h \
	DataSet.h \
	f2c.h \
	GridIntegral.h \
        Gsl2SpkError.h \
	IndData.h \
	MapBay.h \
	MapMonte.h \
	MontePopObj.h \
	Pred.h \
	NonmemPars.h
#
OBJECT = \
	AdaptIntegral.o \
	adapt.o \
	GridIntegral.o \
	Gsl2SpkError.o \
	MapBay.o \
	MapMonte.o \
	monteDriver.o \
	MontePopObj.o \
	pow_ii.o

# Rule for compiling C++ soruce files
%.o : %.cpp $(INCLUDES) 
	g++ $(CPPFLAGS) $< -o $@

# Rule for compiling C soruce files
%.o : %.c $(INCLUDE) 
	gcc $(CPPFLAGS) $< -o $@

monteDriver : $(OBJECT)
	g++ -g $(OBJECT) -o monteDriver -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -lspk -lspkopt -lspkpred -lgsl -latlas_lapack -lcblas -latlas -lpthread -lm -lxerces-c

install : \
	driver \
	\
	adapt.h \
	AdaptIntegral.h \
	f2c.h \
	GridIntegral.h \
        Gsl2SpkError.h \
	MapBay.h \
	MapMonte.h \
	MontePopObj.h \
	\
	AdaptIntegral.cpp \
	adapt.c \
	GridIntegral.cpp \
	Gsl2SpkError.cpp \
	MapBay.cpp \
	MapMonte.cpp \
	monteDriver.cpp \
	MontePopObj.cpp \
	pow_ii.c

	rm -rf $(INSTALL_DIR)
	mkdir -p $(INSTALL_DIR)
	cp $? $(INSTALL_DIR)
