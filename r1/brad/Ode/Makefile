CppADInstallPrefix=/usr/local

Main: Main.o OdeBreakOk.o OneBreakOk.o ZeroBreakOk.o
	g++ -L $(CppADInstallPrefix)/lib \
		Main.o \
		OdeBreakOk.o \
		OneBreakOk.o \
		ZeroBreakOk.o \
		-lCppAD -o Main

OdeBreakOk.o : OdeBreakOk.cpp OdeBreak.h
	g++ -I $(CppADInstallPrefix)/include -g -c OdeBreakOk.cpp

OneBreakOk.o : OneBreakOk.cpp OdeBreak.h
	g++ -I $(CppADInstallPrefix)/include -g -c OneBreakOk.cpp

ZeroBreakOk.o : ZeroBreakOk.cpp OdeBreak.h
	g++ -I $(CppADInstallPrefix)/include -g -c ZeroBreakOk.cpp

Main.o : Main.cpp
	g++ -I $(CppADInstallPrefix)/include -g -c Main.cpp
