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
	../MontePopObj.h 

OBJECTS = \
	pow_ii.o \
	adapt.o \
	\
	AdaptIntegral.o \
	GridIntegral.o \
	Gsl2SpkError.o \
	MapBay.o \
	MapMonte.o \
	monteDriver.o \
	MontePopObj.o 

Compare: NearEqual 
	./RunTest

NearEqual: NearEqual.cpp
	g++ -g NearEqual.cpp -o NearEqual

OneTest: $(OBJECTS) $(INCLUDES)
	g++ $(OBJECTS) -Wl,--rpath -Wl,/usr/local/lib -L/usr/local/lib -Wl,--rpath -Wl,/usr/local/lib/spktest -L/usr/lib/atlas -L/usr/local/lib/spktest -lspkpred -lspk -lQN01Box -lgsl -llapack_atlas -lcblas -latlas -lpthread -lm -lxerces-c -o OneTest

# Rule for compiling monteDriver, only file that really depends on MontePars.h
# in the sense as it is changed by RunTest.sh
monteDriver.o : ../monteDriver.cpp MontePars.h $(INCLUDES)
	g++ $(CPPFLAGS) $< -o $@

# Rule for compiling C++ soruce files
%.o : %.cpp $(INCLUDES) 
	g++ $(CPPFLAGS) $< -o $@

# Rule for compiling C soruce files
%.o : %.c $(INCLUDES) 
	gcc $(CPPFLAGS) $< -o $@
