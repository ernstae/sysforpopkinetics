/*
  NAME
  spkind -- individual level of SPK computation

  SYNOPSIS
  spkind job_id individual_id spkjob_tid

  DESCRIPTION

  This program performs the SPK computation for a single individual.
  It is started by the population level, spkpop, and runs the
  individual optimization. It can run on any node of a parallel
  virtual machine (pvm).

  As a unix process, this program is the child of the pvm daemon
  running on its node.  As a pvm task, it is the child of spkpop,
  which may be running on the same node or on any other node of the
  pvm.

  Pvm connects the standard output stream of this program to the spk
  log file, which resides on the master node.  The program should not
  write directly to either standard output or standard input, Instead,
  all log messages should be posted using the "spklog" function, which
  prepends identification information to the message, then writes it
  to standard output. (Note that the logging system does not rely on
  NFS.)

  The program runs in its own unique working directory, which is
  established by spkrund, the SPK runtime daemon, which is the unix
  parent of spkjob. It acesses the working directory via the Network
  File System (NFS).

  ARGUMENTS

  job_id

      The job_id of the job for which this is a part.  This is the
      string which identifies the job in the spkdb database, and also
      in the spk runtime log.

  individual_id

      If a job models a population of n individuals, they can be
      numbered 0,1,2,...n-1.  The individual_id is a number that set
      and serves to distinguish this instance of spkind from other
      instances associated with the same job.

  spkjob_tid

      This is the tid of the instance of spkjob which is working on
      the given job and is, with respect to the parallel virtual
      machine (PVM) the grandparent of this task.
 
  SEE ALSO
  spkjob
  spkpop
  spkrund
  pvm
*/

#include <unistd.h>
#include <sys/utsname.h>
#include <pvm3.h>
#include <cstdlib>
#include <cstdio>
#include <time.h>
#include <cstring>
#include <csignal>
#include <spkpvm.h>

static int my_tid = 0;
static char *job_id = "?????";
static char *ind_id = "?????";
static int parent_tid = 0;
static int spkjob_tid = 0;

static void finish(int);
static void signal_handler(int);
static void spklog(const char*);

// call this function in case of fatal error
static void die(char* message) {
  spklog(message);
  finish(SpkPvmDied);
  exit(1);
}
// call this function to clean up before ending
static void finish(int exit_value) {
  pvm_initsend(PvmDataDefault);
  pvm_pkint(&exit_value, 1, 1);
  pvm_send(parent_tid, SpkPvmExitValue);
  spklog("stop");
  pvm_exit();
}
// handle a signal
static void signal_handler(int signum) {
  spklog("caught a termination signal");
  finish(signum);
  exit(signum);
}
// Send a log message to spkjob
static void spklog(const char* message) {
  char buf[100];
  sprintf(buf, "j%s\tt%0x\tspkind(%s):\t%s", job_id, my_tid, ind_id, message);
  pvm_initsend(PvmDataDefault);
  pvm_pkstr(buf);
  pvm_send(spkjob_tid, SpkPvmLogMessage);
}

int main(int argc, char** argv) {
  pvm_setopt(PvmRoute, PvmDontRoute);
  my_tid = pvm_mytid();          // attach to pvm
  char *usage = "spkind job_id individual_id";
  char buf[100];
  int bufid;

  // process arguments  
  if (argc != 4)
    die(usage);
  job_id     = argv[1];
  ind_id     = argv[2];
  spkjob_tid = atoi(argv[3]);

  int iid = atoi(ind_id);
  
  // set up signal handling
  struct sigaction signal_action;
  sigset_t block_mask;
  sigemptyset(&block_mask);
  for (int i = 0; i < NSIG; i++)
    sigaddset(&block_mask, i);
  signal_action.sa_handler = signal_handler;
  signal_action.sa_flags = 0;
  signal_action.sa_mask = block_mask;
  for (int i = 0; i < NSIG; i++)
    sigaction(i,  &signal_action, NULL);

  parent_tid = pvm_parent();

  // write our host name to the log
  struct utsname un;
  uname(&un);
  sprintf(buf, "start: on host %s", un.nodename);
  spklog(buf);

  // change working directory
  sprintf(buf, "%s/spkjob-%s/%s", SPK_WORKING, job_id, ind_id);
  if (chdir(buf) != 0)
    die("could not change working directory");

  if (strcmp(ind_id, "1") == 0) {
    spklog("going to cause a segmentation fault");
    sleep(17);
    char *x;
    strcpy(x, "hello");
  }
  srand(1023);
  for (int i = 0; i < 10 * iid; i++)
    rand();
  sleep(1 + (int)(20.0*rand()/(RAND_MAX+1.0)));

  sleep(10);
  finish(0);
  return 0;
}

