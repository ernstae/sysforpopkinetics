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

TEST_OBJS  = NonmemCompilerTest.o NonmemExpXlatorTest.o

TEST_HS    = NonmemCompilerTest.h NonmemExpXlatorTest.h

TEST_CPPS  = NonmemCompilerTest.cpp NonmemExpXlatorTest.cpp

CORE_OBJS  = $(LIBNONMEM)/NonmemSpkMLToCpp.o \
             $(LIBNONMEM)/explang.tab.o \
             $(LIBNONMEM)/lex.yy.o \
             $(LIBCOMM)/SpkMLToCpp.o \
	     $(LIBCOMM)/ExpTreeGenerator.o \
             $(LIBCOMM)/SpkCompilerUtil.o \
             $(LIBCOMM)/SymbolTable.o \
             $(LIBCOMM)/Symbol.o

CORE_HS    = $(LIBNONMEM)/NonmemSpkMLToCpp.h \
             $(LIBNONMEM)/explang.tab.h \
             $(LIBCOMM)/SpkMLToCpp.h \
             $(LIBCOMM)/ExpTreeGenerator.h \
             $(LIBCOMM)/SpkCompilerUtil.h \
             $(LIBCOMM)/SymbolTable.h \
             $(LIBCOMM)/Symbol.h \
             $(LIBCOMM)/client.h

CORE_CPPS  = $(LIBNONMEM)/NonmemSpkMLToCpp.cpp \
             $(LIBNONMEM)/explang.tab.cpp \
             $(LIBNONMEM)/lex.yy.c \
             $(LIBCOMM)/SpkMLToCpp.cpp \
             $(LIBCOMM)/ExpTreeGenerator.cpp \
             $(LIBCOMM)/SpkCompilerUtil.cpp \
             $(LIBCOMM)/SymbolTable.cpp \
             $(LIBCOMM)/Symbol.cpp

testall : testall.cpp $(CORE_OBJS) $(TEST_OBJS)
	$(LINK.cc) -I$(LIBCOMM) -I$(LIBNONMEM) testall.cpp $(TEST_OBJS) $(CORE_OBJS) -l$(XERCESLIB) -l$(CPPUNITLIB) -o $@

NonmemCompilerTest.o : NonmemCompilerTest.cpp NonmemCompilerTest.h $(CORE_HS) $(CORE_CPPS)
	$(COMPILE.cc) -I$(LIBCOMM)/ -I$(LIBNONMEM)/ NonmemCompilerTest.cpp

NonmemExpXlatorTest.o : NonmemExpXlatorTest.cpp $(CORE_HS) $(CORE_CPPS) $(LIBNONMEM)/explang.l $(LIBNONMEM)/explang.y
	$(COMPILE.cc) -I$(LIBCOMM) -I$(LIBNONMEM)/ NonmemExpXlatorTest.cpp

clean:
	rm $(TEST_OBJS)
