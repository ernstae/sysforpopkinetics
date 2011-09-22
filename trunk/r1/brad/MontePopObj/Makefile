#
CPPFLAGS = \
	-g \
	-c 

INCLUDES = \
	GridIntegral.h \
	NearEqual.h \
	LinearModel.h \
	MapBay.h \
	MapMonte.h \
	NearEqual.h

OBJECTS = \
	GridIntegral.o \
	AnalyticIntegral.o \
	MapMonte.o \
	MapMonteTest.o \
	MapBay.o \
	GridIntegralTest.o \
	MapBayTest.o \
	MontePopObj.o \
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
#
DRIVER_INC = \
	MapMonte.h \
	MapBay.h \
	Driver/Pred.h \
	Driver/DataSet.h \
	Driver/IndData.h
#
DRIVER_OBJ = \
	GridIntegral.o \
	MontePopObj.o \
	AnalyticIntegral.o \
	monteDriver.o \
	MapMonte.o \
	MapBay.o

monteDriver : $(DRIVER_INC) $(DRIVER_OBJ)
	g++ -g $(DRIVER_OBJ) -o monteDriver -lspk -lspkopt -lspkpred -lgsl -latlas_lapack -lcblas -latlas -lpthread -lm
