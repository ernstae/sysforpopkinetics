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

LIBCOMM    = ../../libcommon

XERCESLIB  = xerces-c
CPPUNITLIB = cppunit

CORE_OBJS   = $(LIBCOMM)/SpkCompiler.o \
	      $(LIBCOMM)/ParseTree.o \
              $(LIBCOMM)/SpkCompilerUtil.o \
              $(LIBCOMM)/SymbolTable.o \
              $(LIBCOMM)/Symbol.o

TEST_OBJS   = ParseTreeTest.o SpkCompilerUtilTest.o SymbolTableTest.o SymbolTest.o

CORE_H      = $(LIBCOMM)/SpkCompiler.h \
              $(LIBCOMM)/ParseTree.h \
              $(LIBCOMM)/SpkCompilerUtil.h \
              $(LIBCOMM)/SymbolTable.h \
              $(LIBCOMM)/Symbol.h

TEST_H      = ParseTreeTest.h SpkCompilerUtilTest.h SymbolTableTest.h SymbolTest.h

testall : testall.cpp $(TEST_OBJS) $(CORE_OBJS) $(TEST_H) $(CORE_H)
	$(LINK.cc) -I$(LIBCOMM) testall.cpp $(TEST_OBJS) $(CORE_OBJS) -l$(XERCESLIB) -l$(CPPUNITLIB) -o $@

ParseTreeTest.o : ParseTreeTest.cpp ParseTreeTest.h $(LIBCOMM)/ParseTree.o
	$(COMPILE.cc) -I$(LIBCOMM) ParseTreeTest.cpp

SpkCompilerUtilTest.o : SpkCompilerUtilTest.cpp SpkCompilerUtilTest.h $(LIBCOMM)/SpkCompilerUtil.o
	$(COMPILE.cc) -I$(LIBCOMM)  SpkCompilerUtilTest.cpp

SymbolTableTest.o : SymbolTableTest.cpp SymbolTableTest.h $(LIBCOMM)/SymbolTable.o $(LIBCOMM)/Symbol.o
	$(COMPILE.cc) -I$(LIBCOMM) SymbolTableTest.cpp

SymbolTest.o : SymbolTest.cpp SymbolTest.h $(LIBCOMM)/Symbol.o
	$(COMPILE.cc) -I$(LIBCOMM) SymbolTest.cpp


clean:
	rm $(TEST_OBJS)
