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

BISON_OBJS = nmabb.tab.cpp nmabb.tab.h nmabb.output

OBJECTS = nmabb.tab.o lex.yy.o

all:	nmabb.tab.o lex.yy.o NonmemCompiler.o

NonmemCompiler.o : NonmemCompiler.h NonmemCompiler.cpp nmabb.tab.h
	$(COMPILE.cc) $(CPPFLAGS) -I../libcommon/ NonmemCompiler.cpp

nmabb.tab.o : nmabb.tab.cpp nmabb.tab.h nmabb.y $(COMMON_HEADERS)
	$(COMPILE.cc) $(CPPFLAGS) -I../libcommon/ nmabb.tab.cpp

lex.yy.o : lex.yy.c nmabb.tab.h nmabb.l
	$(COMPILE.c) $(CFLAGS) lex.yy.c

lex.yy.c : nmabb.l nmabb.tab.h
	$(LEX.l) nmabb.l

nmabb.tab.h nmabb.tab.cpp : nmabb.y
	$(YACC.y) nmabb.y
	mv nmabb.tab.c nmabb.tab.cpp

clean:
	rm $(OBJECTS) $(FLEX_OBJS) $(BISON_OBJS)
