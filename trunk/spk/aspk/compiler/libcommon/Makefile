LD         = ld
LDFLAGS    =

C          = gcc
CFLAGS     = -g
COMPILE.c  = $(C) $(CFLAGS) -c
LINK.c     = $(C) $(CFLAGS) $(LDFLAGS)

CPP        = g++
CPPFLAGS   = -g
COMPILE.cc = $(CPP) $(CPPFLAGS) -c
LINK.cc    = $(CPP) $(CPPFLAGS) $(LDFLAGS) $(LDPATH)

XERCESLIB  = xerces-c

OBJECTS    = SymbolTable.o Symbol.o SpkCompilerUtil.o ParseTree.o SpkCompiler.o 

HEADERS    = Symbol.h SymbolTable.h ParseTree.h client.h NodeCarrier.h SpkCompilerUtil.h SpkCompiler.h

CPPS       = Symbol.cpp SymbolTable.cpp ParseTree.cpp SpkCompilerUtil.cpp SpkComiler.cpp

all: $(OBJECTS)

SpkCompiler.o : SpkCompiler.cpp SpkCompiler.h SymbolTable.h Symbol.h SpkCompilerUtil.h ParseTree.h
	$(COMPILE.cc) SpkCompiler.cpp

ParseTree.o : ParseTree.cpp ParseTree.h SpkCompilerUtil.h
	$(COMPILE.cc) ParseTree.cpp

SpkCompilerUtil.o : SpkCompilerUtil.h SpkCompilerUtil.cpp
	$(COMPILE.cc) SpkCompilerUtil.cpp

SymbolTable.o : SymbolTable.cpp SymbolTable.h Symbol.h
	$(COMPILE.cc) SymbolTable.cpp

Symbol.o : Symbol.cpp Symbol.h
	$(COMPILE.cc) Symbol.cpp

clean:
	rm $(OBJECTS)
