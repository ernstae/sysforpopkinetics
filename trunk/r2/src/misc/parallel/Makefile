CC=g++
CFLAGS=-g
MODE=test
SPK_SHARE=/usr/local/spk/share
MODE=test

includes = spkpvm.h 

objects = spkpvm.o

all:	spkjob spkpop spkind

spkjob:	spkjob.o $(objects)
	$(CC) -o spkjob spkjob.o $(objects) -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/

spkpop: spkpop.o $(objects)
	$(CC) -o spkpop spkpop.o $(objects) -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/

spkind: spkind.o $(objects)
	$(CC) -o spkind spkind.o $(objects) -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/

%.o : %.cpp $(includes)
	$(CC) -c $(CFLAGS) $< -o $@ -DARCH=\"$(PVM_ARCH)\" -I$(PVM_ROOT)/include -I.

install:
	cp spkjob spkpop spkind newjob.sh $(SPK_SHARE)/arch/bin/$(PVM_ARCH)/spk$(MODE)
	cp pvmhosts $(SPK_SHARE)/pvmhosts

clean:
	rm -f *.o
	rm -f spkjob
	rm -f spkpop
	rm -f spkind
