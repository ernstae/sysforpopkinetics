YACC       = bison
YFLAGS     = -d --verbose --debug

LEX        = flex
LFLAGS     = -i
#LLIB       = fl

# C compiler
CFLAGS     = -g

FLEX_OBJS = lex.yy.c

BISON_OBJS = explang.tab.cpp explang.tab.h explang.output

OBJECTS = explang.tab.o lex.yy.o NonmemTranslator.o


all: $(OBJECTS)

NonmemTranslator.o : NonmemTranslator.h NonmemTranslator.cpp ../libcommon/SpkMLToCpp.h ../libcommon/SpkMLToCpp.cpp ../libcommon/client.h explang.tab.h
	$(CC) $(CPPFLAGS) -c -I../libcommon/ NonmemTranslator.cpp

explang.tab.o : explang.tab.cpp explang.tab.h
	$(CC) $(CPPFLAGS)  -c explang.tab.cpp

lex.yy.o : lex.yy.c explang.tab.h explang.l
	$(CC) $(CFLAGS) -c lex.yy.c

lex.yy.c : explang.l explang.tab.h
	$(LEX) $(LFLAGS) explang.l

explang.tab.h explang.tab.cpp : explang.y
	$(YACC) $(YFLAGS) explang.y
	mv explang.tab.c explang.tab.cpp

clean:
	rm $(OBJECTS) $(FLEX_OBJS) $(BISON_OBJS)
