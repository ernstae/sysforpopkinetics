#
CPPFLAGS = \
	-g \
	-c 

INCLUDES = \
	../GridIntegral.h \
	../MapBay.h \
	../MapMonte.h \
	LinearModel.h \
	NearEqual.h

OBJECTS = \
	../GridIntegral.o \
	../AnalyticIntegral.o \
	../MapMonte.o \
	../MapBay.o \
	../MontePopObj.o \
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
	g++ $(OBJECTS)  -lspk -lspkopt -lgsl -latlas_lapack -lcblas -latlas -o RunTest
