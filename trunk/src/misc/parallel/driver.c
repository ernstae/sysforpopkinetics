/*

Simulation of parallel behavior of an SPK driver

*/

static int get_individuals();

#include <stdio.h>
#include <pvm3.h>

int main(int argc, char** argv) {
  size_t ntasks = get_individuals();
  int mytid = pvm_mytid();
  printf("mytid=%d\n", mytid);
  if (mytid < 0) {
    pvm_perror(argv[0]);
    return -1;
  }
  int myparent = pvm_parent();
  printf("myparent=%d\n", myparent);
  if ((myparent < 0) && (myparent != PvmNoParent)) {
    pvm_perror(argv[0]);
    pvm_exit();
    return -1;
  }
  if (myparent == PvmNoParent) {
    int* child = (int*)calloc(ntasks, sizeof(int));
    int rval = pvm_spawn(argv[0], NULL, 0, "jambutty", ntasks, child);
    printf("rval=%d\n", rval);
    
    free(child);
			 
  } 
  else {
    sleep(30);
  }
  return 0;
}

static int get_individuals() {
  return 5;
}
