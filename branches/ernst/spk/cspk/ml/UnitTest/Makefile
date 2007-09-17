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
	../adapt.h \
	../AdaptIntegral.h \
	../f2c.h \
	../GridIntegral.h \
	../Gsl2SpkError.h \
	../MapBay.h \
	../MapMonte.h \
	../MontePopObj.h \
	AnalyticIntegral.h \
	MontePars.h 

OBJECTS = \
	pow_ii.o \
	adapt.o \
	\
	AdaptIntegral.o \
	GridIntegral.o \
	Gsl2SpkError.o \
	MapBay.o \
	MapMonte.o \
	MontePopObj.o \
	\
	AnalyticIntegral.o \
	ExpModelTest.o \
	GridIntegralTest.o \
	MapBayTest.o \
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
	g++ $(OBJECTS) -Wl,--rpath -Wl,/usr/local/lib -L/usr/local/lib -L/usr/lib/atlas -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/local/lib/spktest -lspk -lspkpred -lQN01Box -lgsl -llapack_atlas -lcblas -latlas -lpthread -lm -lxerces-c -o RunTest
