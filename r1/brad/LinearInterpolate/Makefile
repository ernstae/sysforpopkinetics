LinearInterpolateOk: LinearInterpolate.o
	g++ -L $(HOME)/lib LinearInterpolateOk.o -lCppAD -o LinearInterpolateOk

LinearInterpolate.o: LinearInterpolateOk.cpp LinearInterpolate.h
	g++ -I $(HOME)/include -g -c LinearInterpolateOk.cpp
