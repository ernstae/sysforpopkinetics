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

LIBCOMM    = ../libcommon
LIBNONMEM  = ../libnonmem

OBJECTS    = $(LIBCOMM)/SymbolTable.o \
             $(LIBCOMM)/Symbol.o \
             $(LIBCOMM)/SpkCompilerUtil.o \
             $(LIBCOMM)/ParseTree.o \
             $(LIBCOMM)/SpkCompiler.o \
             $(LIBNONMEM)/NonmemCompiler.o \
             $(LIBNONMEM)/nmabb.tab.o \
             $(LIBNONMEM)/lex.yy.o

HEADERS    = $(LIBCOMM)/Symbol.h \
             $(LIBCOMM)/SymbolTable.h \
             $(LIBCOMM)/ParseTree.h \
             $(LIBCOMM)/client.h \
             $(LIBCOMM)/NodeCarrier.h \
             $(LIBCOMM)/SpkCompilerUtil.h \
             $(LIBCOMM)/SpkCompiler.h \
	     $(LIBNONMEM)/NonmemCompiler.h \
             $(LIBNONMEM)/nmabb.tab.h

SOURCES    = $(LIBCOMM)/Symbol.cpp \
             $(LIBCOMM)/SymbolTable.cpp \
             $(LIBCOMM)/ParseTree.cpp \
             $(LIBCOMM)/SpkCompilerUtil.cpp \
             $(LIBCOMM)/SpkCompiler.cpp \
             $(LIBNONMEM)/NonmemCompiler.cpp \
             $(LIBNONMEM)/nmabb.tab.cpp \
             $(LIBNONMEM)/lex.yy.c

all: $(OBJECTS) compiler

compiler : $(HEADERS) $(SOURCES) compiler.cpp
	$(LINK.cc) -I../libcommon/ -I../libnonmem/ -l$(XERCESLIB) compiler.cpp $(OBJECTS) -o compiler

clean:
	rm $(OBJECTS)
