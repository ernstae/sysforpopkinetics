CC=g++
CFLAGS=-g
MODE=test
SPK_SHARE=/usr/local/spk/share

includes = spkpvm.h 

objects = spkpvm.o

all:	spkjob spkpop spkind

spkjob:	spkjob.o $(objects)
	$(CC) -o spkjob spkjob.o $(objects) -lpvm3 -L$(SPK_SHARE)/lib/$(PVM_ARCH)/spktest

spkpop: spkpop.o $(objects)
	$(CC) -o spkpop spkpop.o $(objects) -lpvm3 -L$(SPK_SHARE)/lib/$(PVM_ARCH)/spktest

spkind: spkind.o $(objects)
	$(CC) -o spkind spkind.o $(objects) -lpvm3 -L$(SPK_SHARE)/lib/$(PVM_ARCH)/spktest

%.o : %.cpp $(includes)
	$(CC) -c $(CFLAGS) $< -o $@ -DARCH=\"$(PVM_ARCH)\"

install:
	cp spkjob spkpop spkind $(SPK_SHARE)/arch/$(ARCH)/bin/spk$(MODE)

clean:
	rm -f *.o
	rm -f spkjob
	rm -f spkpop
	rm -f spkind
