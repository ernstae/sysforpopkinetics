CC=g++
CFLAGS=-g
SPK_SHARE=/usr/local/spk/share
ARCH=i686
MODE=test

includes = spkpvm.h 

objects = spkpvm.o

all:	spkjob spkpop spkind

spkjob:	spkjob.o $(objects)
	$(CC) -o spkjob spkjob.o $(objects) -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/ 

spkpop: spkpop.o spkpvm.o
	$(CC) -o spkpop spkpop.o $(objects) -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/ 

spkind: spkind.o 
	$(CC) -o spkind spkind.o $(objects) -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/ 

example: master1 slave1

master1:	master1.o 
	$(CC) -o master1 master1.o -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/ 

slave1:	slave1.o 
	$(CC) -o slave1 slave1.o -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/ 


cspk: cspk.o 
	$(CC) -o cspk cspk.o $(objects) -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/ 

sig:	sig.o
	$(CC) -o sig sig.o

%.o : %.cpp $(includes)
	$(CC) -c $(CFLAGS) $< -o $@ -DARCH=$(ARCH) -I$(PVM_ROOT)/include -I.

install:
	cp spkjob spkpop spkind $(SPK_SHARE)/arch/$(ARCH)/bin/spk$(MODE)

clean:
	rm -f *.o
	rm -f spkjob
	rm -f spkpop
	rm -f spkind
