#
# search the directory above for source files
VPATH = ..
#
CPPFLAGS = \
	-g \
	-c \
	-I/usr/local/include/spktest/  \
	-I. \
	-I..

INCLUDES = \
	GridIntegral.h \
	MapBay.h \
	MapMonte.h \
	MontePars.h \
	LinearModel.h \
	Gsl2SpkError.h \
	ExpModel.h

OBJECTS = \
	GridIntegral.o \
	AnalyticIntegral.o \
	MapMonte.o \
	MapBay.o \
	MontePopObj.o \
	MapBayTest.o \
	GridIntegralTest.o \
	MapMonteTest.o \
	MontePopObjTest.o \
	NearEqual.o \
	Gsl2SpkError.o \
	ExpModelTest.o \
	RunTest.o


# Rule for compiling C++ soruce files
%.o : %.cpp $(INCLUDES) 
	g++ $(CPPFLAGS) $< -o $@

# Rule for compiling C soruce files
%.o : %.c $(INCLUDES) 
	gcc $(CPPFLAGS) $< -o $@

RunTest: $(OBJECTS) $(INCLUDES)
	g++ $(OBJECTS) -Wl,--rpath -Wl,/usr/local/lib -L/usr/local/lib -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -lCppAD -lspk -lspkopt -lgsl -latlas_lapack -lcblas -latlas -lxerces-c -o RunTest
