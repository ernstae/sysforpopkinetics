CC=g++
CFLAGS=-g

includes = spkjob.h 

all:	spkjob spkpop

spkjob:	spkjob.o 
	$(CC) -o spkjob spkjob.o -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/ 

spkpop: spkpop.o 
	$(CC) -o spkpop spkpop.o $(objects) -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/ 

cspk: cspk.o 
	$(CC) -o cspk cspk.o $(objects) -lpvm3 -L$(PVM_ROOT)/lib/$(PVM_ARCH)/ 

sig:	sig.o
	$(CC) -o sig sig.o

%.o : %.cpp $(includes)
	$(CC) -c $(CFLAGS) $< -o $@ -I$(PVM_ROOT)/include -I.

install:
	scp spk_parallel_demo aspkserver:dvl/r2/src/misc/parallel

clean:
	rm -f *.o
	rm -f spkjob
	rm -f cspk
