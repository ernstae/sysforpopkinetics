/*
  NAME
  spkind -- individual level of spk parallel prototype

  SYNOPSIS
  spkind job_id individual_id spkjob_tid mode

  DESCRIPTION
  This program is a prototype for the individual level of the parallel
  spk.  It is started by the population level, spkpop, and runs the
  individual optimization. It can run on any node of a parallel
  virtual machine (pvm).

  As a unix process, this program is the child of the pvm daemon
  running on its node.  As a pvm task, it is the child of spkpop,
  which may be running on the same node or on any other node of the
  pvm.

  The program runs in its own unique working directory, which is
  a subdirectory of the working directory for the job.  

  RUNNING
  This program should only be run by spkpop in a parallel vitual 
  machine (pvm).

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
  mode
      mode is either "test" or "prod" depending on whether the job is 
      running in the test environment or the production environment

  DESIGN CONSTRAINTS AND GOALS
  See the similar documentation for spkjob.

  FILES
  See the similar documentation for spkjob
 
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
#include <spkpvm.h>

#include <csignal>

static int my_tid = 0;
static char *job_id = "?????";
static char *ind_id = "?????";
static int parent_tid = 0;
static int spkjob_tid = 0;

static void finish(int);
static void spklog(const char*);

static void signal_handler(int);
static void signal_initialize(void);

// perform the computation
static void compute(int iid) {
  const int t = 10;
  if (iid == 1 && (time(NULL) % 10L) == 0) {
    char buf[100];
    sprintf(buf, "going to cause a fault in %d sec.", t);
    spklog(buf);
    sleep(t);
    char *p = 0;
    strcpy(p, "x");
  }
  srand(time(NULL));
  sleep(5 + (int)(10.0*rand()/(RAND_MAX+1.0)));
}

// call this function in case of fatal error
static void die(char* message) {
  spklog(message);
  finish(SpkPvmDied);
  exit(1);
  sleep(10);
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
  char buf[100];
  sprintf(buf, "caught termination signal %d", signum);
  signal(signum, SIG_IGN);
  spklog(buf);
  finish(signum);
  exit(signum);
}
// initialize signal handling
static void signal_initialize() {
  struct sigaction signal_action;
  sigset_t block_mask;
  sigemptyset(&block_mask);
  for (int i = 0; i < spkpvm_siglist_length; i++)
    sigaddset(&block_mask, spkpvm_siglist[i]);
  signal_action.sa_handler = signal_handler;
  signal_action.sa_flags = 0;
  signal_action.sa_mask = block_mask;
  for (int i = 0; i < spkpvm_siglist_length; i++)
    sigaction(spkpvm_siglist[i],  &signal_action, NULL);
}
// send a log message to spkjob
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
  char *usage = "spkind job_id individual_id mode";
  char buf[100];
  int bufid;

  // Process arguments.
  if (argc != 5)
    die(usage);
  job_id     = argv[1];
  ind_id     = argv[2];
  spkjob_tid = atoi(argv[3]);
  char *mode = argv[4];

  int iid = atoi(ind_id);
  
  // Set up signal handling.
  signal_initialize();

  parent_tid = pvm_parent();

  // Write my host name to the log.
  struct utsname un;
  uname(&un);
  sprintf(buf, "start: on host %s", un.nodename);
  spklog(buf);

  // Change working directory.
  sprintf(buf, "%s/working/spk%s/spkjob-%s/%s", SPK_SHARE, mode, job_id, ind_id);
  if (chdir(buf) != 0)
    die("could not change working directory");

  // Perform the compution.
  compute(iid);

  // Exit.
  finish(0);
  return 0;
}

