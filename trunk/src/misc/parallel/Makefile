CC=g++
CFLAGS=-g
SPK_SHARE=/usr/local/spk/share
ARCH=i686

includes = spkjob.h 

all:	spkjob spkpop

example: master1 slave1

master1:	master1.o 
	$(CC) -o master1 master1.o -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/ 

slave1:	slave1.o 
	$(CC) -o slave1 slave1.o -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/ 

spkjob:	spkjob.o 
	$(CC) -o spkjob spkjob.o -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/ 

spkpop: spkpop.o 
	$(CC) -o spkpop spkpop.o $(objects) -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/ 

cspk: cspk.o 
	$(CC) -o cspk cspk.o $(objects) -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/ 

sig:	sig.o
	$(CC) -o sig sig.o

%.o : %.cpp $(includes)
	$(CC) -c $(CFLAGS) $< -o $@ -DARCH=$(ARCH) -I$(PVM_ROOT)/include -I.

install:
	cp spkjob spkpop $(SPK_SHARE)/arch/$(ARCH)/bin

install_example:
	cp master1 slave1 $(SPK_SHARE)/arch/$(ARCH)/bin

clean:
	rm -f *.o
	rm -f spkjob
	rm -f cspk
