COMDIR     = ..

YACC       = bison
YFLAGS     = -d --verbose --debug

LEX        = flex
LFLAGS     = -i
LEX.l      = $(LEX) $(LFLAGS)
LLIB       = fl

# C compiler
C          = gcc
CFLAGS     = -g
COMPILE.c  = $(C) $(CFLAGS) -c

# C++ compiler
CPP        = g++
CPPFLAGS   = -g
COMPILE.cc = $(CPP) $(CPPGLAGS) -c

# linker
LD   = ld
#LDFLAGS   = 

FLEX_OBJS = lex.yy.c

BISON_OBJS = explang.tab.cpp explang.tab.h explang.output

OBJECTS = explang.tab.o lex.yy.o

all:	explang.tab.o lex.yy.o NonmemCompiler.o

NonmemCompiler.o : NonmemCompiler.h NonmemCompiler.cpp explang.tab.h
	$(COMPILE.cc) $(CPPFLAGS) -I../libcommon/ NonmemCompiler.cpp

explang.tab.o : explang.tab.cpp explang.tab.h explang.y $(COMMON_HEADERS)
	$(COMPILE.cc) $(CPPFLAGS) -I../libcommon/ explang.tab.cpp

lex.yy.o : lex.yy.c explang.tab.h explang.l
	$(COMPILE.c) $(CFLAGS) lex.yy.c

lex.yy.c : explang.l explang.tab.h
	$(LEX.l) explang.l

explang.tab.h explang.tab.cpp : explang.y
	$(YACC.y) explang.y
	mv explang.tab.c explang.tab.cpp

clean:
	rm $(OBJECTS) $(FLEX_OBJS) $(BISON_OBJS)
