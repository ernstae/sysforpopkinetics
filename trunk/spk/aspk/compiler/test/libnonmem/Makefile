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
LIBNONMEM  = ../../libnonmem

XERCESLIB  = xerces-c
CPPUNITLIB = cppunit

TEST_OBJS  = NonmemCompilerTest.o

TEST_HS    = NonmemCompilerTest.h

TEST_CPPS  = NonmemCompilerTest.cpp

CORE_OBJS  = $(LIBNONMEM)/NonmemCompiler.o \
             $(LIBNONMEM)/nmabb.tab.o \
             $(LIBNONMEM)/lex.yy.o \
             $(LIBCOMM)/SpkCompiler.o \
	     $(LIBCOMM)/ParseTree.o \
             $(LIBCOMM)/SpkCompilerUtil.o \
             $(LIBCOMM)/SymbolTable.o \
             $(LIBCOMM)/Symbol.o

CORE_HS    = $(LIBNONMEM)/NonmemCompiler.h \
             $(LIBNONMEM)/nmabb.tab.h \
             $(LIBCOMM)/SpkCompiler.h \
             $(LIBCOMM)/ParseTree.h \
             $(LIBCOMM)/SpkCompilerUtil.h \
             $(LIBCOMM)/SymbolTable.h \
             $(LIBCOMM)/Symbol.h \
             $(LIBCOMM)/client.h

CORE_CPPS  = $(LIBNONMEM)/NonmemCompiler.cpp \
             $(LIBNONMEM)/nmabb.tab.cpp \
             $(LIBNONMEM)/lex.yy.c \
             $(LIBCOMM)/SpkCompiler.cpp \
             $(LIBCOMM)/ParseTree.cpp \
             $(LIBCOMM)/SpkCompilerUtil.cpp \
             $(LIBCOMM)/SymbolTable.cpp \
             $(LIBCOMM)/Symbol.cpp

TEST_HS    = NonmemCompilerTest.h

testall : testall.cpp $(CORE_OBJS) $(TEST_OBJS)
	$(LINK.cc) -I$(LIBCOMM) -I$(LIBNONMEM) testall.cpp $(TEST_OBJS) $(CORE_OBJS) -l$(XERCESLIB) -l$(CPPUNITLIB) -o $@

NonmemCompilerTest.o : NonmemCompilerTest.cpp NonmemCompilerTest.h $(CORE_HS) $(CORE_CPPS)
	$(COMPILE.cc) -I$(LIBCOMM)/ -I$(LIBNONMEM)/ NonmemCompilerTest.cpp

clean:
	rm $(TEST_OBJS)
