YACC       = bison
YFLAGS     = -d --verbose --debug

LEX        = flex
LFLAGS     = -i
#LLIB       = fl

# C compiler
CFLAGS     = -g

FLEX_OBJS = lexer.c

BISON_OBJS = explang.cpp explang.tab.h explang.output

OBJECTS = explang.o lexer.o NonmemTranslator.o

.y.cpp:
	$(YACC) $(YFLAGS) $<
	mv $*.tab.c $*.cpp

.y.o:
	$(YACC) $(YFLAGS) $<
	mv $*.tab.c $*.cpp
	$(CC) $(CFLAGS) -c $*.cpp -o $*.o

all: $(OBJECTS)

lexer.c : explang.l explang.cpp
	$(LEX) $(LFLAGS) explang.l
	mv lex.yy.c lexer.c

clean:
	rm $(OBJECTS) $(FLEX_OBJS) $(BISON_OBJS)
