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
	NearEqual.h

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
	RunTest.o


# Rule for compiling C++ soruce files
%.o : %.cpp $(INCLUDES) 
	g++ $(CPPFLAGS) $< -o $@

# Rule for compiling C soruce files
%.o : %.c $(INCLUDES) 
	gcc $(CPPFLAGS) $< -o $@

RunTest: $(OBJECTS) $(INCLUDES)
	g++ $(OBJECTS) -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -lspk -lspkopt -lgsl -latlas_lapack -lcblas -latlas -o RunTest
