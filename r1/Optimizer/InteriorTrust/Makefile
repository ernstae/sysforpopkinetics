CPPFLAGS = \
	-g \
	-c
INCLUDES = \
	Bfgs.h \
	c2dsyev.h \
	CppADUtil.h \
	Delta.h \
	MaxAbs.h \
	max.h \
	Memory.h \
	min.h \
	NearEqual.h \
	Next.h \
	PlusInfinity.h \
	QuadBox.h \
	QuadFixed.h \
	QuasiNewton01Box.h \
	Residual.h

OBJECTS = \
	MaxAbs.o \
	NearEqual.o \
	Delta.o \
	Residual.o \
	Next.o \
	QuadBox.o \
	QuadFixed.o \
	Bfgs.o \
	QuasiNewton01Box.o \
	Degenerate.o \
	Test_03_12_16.o \
	ExpLeastSquares.o \
	Test_04_01_21.o \
	Test_04_02_18.o \
	LinearObjective.o \
	SingularUpdate.o \
	ResidualIncrease.o \
	c2dsyev.o \
	PlusInfinity.o \
	RunTest.o


# Rule for compiling C++ soruce files
%.o : %.cpp $(INCLUDES)
	g++ $(CPPFLAGS) $< -o $@

# Rule for compiling F77 soruce files
%.o : %.f 
	f77 -c -g $< -o $@

RunTest: $(OBJECTS) $(INCLUDES)
	g++ $(OBJECTS)  -llapack -lg2c -o RunTest

