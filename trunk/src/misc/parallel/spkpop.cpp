/*
  NAME
  spkpop -- population level of SPK computation

  SYNOPSIS
  spkpop job_id individual_count

  DESCRIPTION

  This program performs the SPK computation for the job given by its
  "job_id" argument.  It can run on any node of a parallel virtual
  machine (pvm).  It calls pvm_spawn to create an instance of the
  spkind program for each individual in the population being
  modeled. The number of such child tasks is given by the
  individual_count argument.  The children can run on the same node or
  on any other node of the pvm.

  As a unix process, this program is the child of the pvm daemon
  running on its node.  As a pvm task, it is the child of spkjob,
  which always runs on the head node of the pvm.

  Pvm connects the standard output stream of this program to the spk
  log file, which resides on the master node.  The program should not
  write directly to either standard output or standard input, Instead,
  all log messages should be posted using the "spklog" function, which
  prepends identification information to the message, then writes it
  to standard output. (Note that the logging system does not rely on
  NFS.)

  The program runs in its own unique working directory, which is
  established by spkrund, the SPK runtime daemon, which is the unix
  parent of spkjob.  It accesses the directory via the Network Files
  System (NFS).

  SEE ALSO
  spkjob
  spkind
  spkrund
  pvm
*/

#include <unistd.h>
#include <sys/utsname.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <errno.h>
#include <cstdlib>
#include <cstdio>
#include <time.h>
#include <cstring>
#include <csignal>
#include <spkpvm.h>

static char *job_id = "?????";
static int ntasks = 0;
static int parent_tid = 0;
static sigset_t block_set;
static int *ind_tid;
static int *host_tid;
static bool *done;
static int *exit_val;
static int my_exit_value = 0;
static int my_tid = 0;
static volatile int ndone = 0;

static int tid2iid(int);
static void finish(int);
static void signal_block(void);
static void signal_handler(int);
static void signal_initialize(void);
static void signal_unblock(void);
static void spawn_individual(int);
static void spklog(const char*);

// call this function in case of fatal error
static void die(char* message) {
  spklog(message);
  finish(SpkPvmDied);
  exit(1);
}
// call this function to clean up before ending
static void finish(int exit_value) {
  char buf[100];
  pvm_initsend(PvmDataDefault);
  pvm_pkint(&exit_value, 1, 1);
  pvm_send(parent_tid, SpkPvmExitValue);
  spklog("stop");
  pvm_exit();
}
static int tid2iid(int tid) {
  for (int iid = 0; iid < ntasks; iid++) {
    if (ind_tid[iid] == tid)
      return iid;
  }
  return -1;
}
// block termination signals
static void signal_block() {
  sigprocmask(SIG_BLOCK, &block_set, NULL);
}
// handle signals (all except alarm)
static void signal_handler(int signum) {
  char buf[100];
  sprintf(buf, "received signal number %d", signum);
  spklog(buf);
  if (signum == SIGALRM) {
    /* 
       SIGALRM should only be activated when PvmTaskExit has been
       received for the last individual still running, and we need to
       delay termination of spkpop to give a PvmHostDelete time to
       arrive if that was the condition that caused the individual to
       exit.
    */
    ndone++;
  }
  else {
    for (int iid = 0; iid < ntasks; iid++)
      pvm_kill(ind_tid[iid]);
    my_exit_value = signum;
  }
}
// initialize signal handling
static void signal_initialize() {

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
  // set up mask of signals to be blocked while executing critical sections
  sigemptyset(&block_set);
  for (int i =  0; i < NSIG; i++)
    sigaddset(&block_set, i);
}
// unblock termination signals
static void signal_unblock() {
  sigprocmask(SIG_UNBLOCK, &block_set, NULL);
}
// spawn an individual
static void spawn_individual(int iid) {
  int tid;
  char* arg[4]; 
  char buf[100];
  char ind_id[6];
  char spkjob_tid[30];
  sprintf(ind_id, "%d", iid);
  sprintf(spkjob_tid, "%d", parent_tid);
  arg[0] = job_id;
  arg[1] = ind_id;
  arg[2] = spkjob_tid;
  arg[3] = NULL;
  sprintf(buf, "%s/arch/%s/bin/spkind", SPK_SHARE, ARCH);
  int rval = pvm_spawn(buf, arg, 0, NULL, 1, &tid);
  if (rval < 0) 
    die("could not spawn individual level due to system error");
  if (rval == 0) {
    const char *err_string = spkpvm_spawn_error(tid);
    sprintf(buf, "could not spawn individual level: %s", err_string);
    die(buf);
  }
  ind_tid[iid]  = tid;
  host_tid[iid] = pvm_tidtohost(tid);
  exit_val[iid] = SpkPvmUnreported;
  done[iid] = false;

  // Establish notifications for this task and its host
  if (pvm_notify(PvmTaskExit, PvmTaskExit, 1, ind_tid + iid) < 0) {
    sprintf(buf, "pvm_notify failed for PvmTaskExit for individual %d", iid);
    die(buf);
  }
  bool host_notified = false;
  for (int i = 0; i < ntasks; i++) {
    if (i != iid && host_tid[i] == host_tid[iid]) {
      host_notified = true;
      break;
    }
  }
  if (!host_notified && pvm_notify(PvmHostDelete, PvmHostDelete, 1, host_tid + iid) < 0) {
    sprintf(buf, "pvm_notify failed for PvmHostExit for individual %d", iid);
    die(buf);
  }
}
// Send a log message to spkjob
static void spklog(const char* message) {
  char buf[100];
  sprintf(buf, "j%s\tt%0x\tspkpop:\t\t%s", job_id, my_tid, message);
  pvm_initsend(PvmDataDefault);
  pvm_pkstr(buf);
  pvm_send(parent_tid, SpkPvmLogMessage);
}


int main(int argc, char** argv) {
  pvm_setopt(PvmRoute, PvmDontRoute);
  my_tid  = pvm_mytid();          // attach to pvm
  int my_host = pvm_tidtohost(my_tid);
  char *usage = "usage: spkpop job_id individual_count";
  char buf[100];
  int bufid;

  // Process arguments  
  if (argc != 3)
    die(usage);
  job_id = argv[1];
  ntasks = atoi(argv[2]);

  // Set up signal handling for the signals that might be used
  // by human operators to terminate us
  signal_initialize();

  parent_tid = pvm_parent();

  // write our host name to the log
  struct utsname un;
  uname(&un);
  sprintf(buf, "start: on host %s", un.nodename);
  spklog(buf);


  if (! (1 <= ntasks && ntasks <= SPK_MAX_INDIVIDUALS)) {
    sprintf(buf, "individual_count = %d > %d individuals (program  limit) -- %s",
	    ntasks, SPK_MAX_INDIVIDUALS, usage);
    die(buf);
  }

  // change working directory
  sprintf(buf, "%s/spkjob-%s", SPK_WORKING, job_id);
  if (chdir(buf) != 0)
    die("could not change working directory");

  // create working directories for individuals, renaming them if they
  // already exist
  time_t ut = time(NULL);       // get a unique number
  for (int iid = 0; iid < ntasks; iid++) {
    char wdir_name[100], junk_name[100];
    sprintf(wdir_name, "%d", iid);
    sprintf(junk_name, "junk.%d.%d", iid, ut);
    rename(wdir_name, junk_name);
    if (mkdir(wdir_name, 02775) != 0) {
      const char *s = iid == 1 ? "st" : "th";
      sprintf(buf, "couldn't make the %d-%s individual working directory", iid, s);
      die(buf);
    }
  }

  ind_tid  = new int[ntasks];
  host_tid = new int[ntasks];
  exit_val = new int[ntasks];
  done     = new bool[ntasks];

  // spawn the individuals
  for (int iid =  0; iid < ntasks; iid++)
    spawn_individual(iid);

  // loop until the population level is done
  ndone = 0;
  alarm(1);
  int iid;
  int exit_value = -1;
  while (ndone < ntasks && (bufid = pvm_recv(-1, -1)) > 0) {
    int bytes, msgtag, tid, source;
    //signal_block();
    if (pvm_bufinfo(bufid, &bytes, &msgtag, &source) < 0)
      die("pvm_bufinfo failed");
    switch (msgtag) {
    case SpkPvmExitValue:
      tid = source;
      if ((iid = tid2iid(tid)) < 0) {
	sprintf(buf, "received SpkPvmExitValue from unexpected tid %0x", tid);
	spklog(buf);
	break;
      }
      if (pvm_upkint(&exit_value, 1, 1) < 0)
	die("pvm_upkint returned an error at SpkPvmExitValue");
      exit_val[iid] = exit_value;
      sprintf(buf, "received exit value = %d from spkind(%d)", exit_value, iid);
      spklog(buf);
	
      if (my_exit_value == 0 && exit_value != 0)
	my_exit_value = SpkPvmChildError;
      break;
    case PvmTaskExit:
      if (pvm_upkint(&tid, 1, 1) < 0)
	die("pvm_upkint failed to unpack tid from PvmTaskExit message");
      if ((iid = tid2iid(tid)) < 0) {
	sprintf(buf, "received PvmTaskExit from tid %0x", tid);
	spklog(buf);
      }
      else {
	sprintf(buf, "received PvmTaskExit from spkind(%d)", iid);
	spklog(buf);
	done[iid] = true;
      }
      if (ndone + 1 == ntasks) {
	alarm(10);  // give PvmHostDelete time to arrive
	spklog("set alarm");
      }
      else
	ndone++;
      break;
    case PvmHostDelete:
      // Treat the case where the incrementing of ndone was delayed
      // to give PvmHostDelete time to arrive
      if (ndone + 1 == ntasks) {
	alarm(0);  // cancel alarm
	ndone++;
      }
      if (pvm_upkint(&tid, 1, 1) < 0)
	die("pvm_upkint failed to unpack tid from PvmTaskExit message");
      sprintf(buf, "received PvmHostDelete, source=%0x, tid=%0x", source, tid);
      spklog(buf);
      // respawn individuals that were on the deleted host
      if (my_exit_value == 0) 
	for (int iid = 0; iid < ntasks; iid++) 
	  if (host_tid[iid] == tid && exit_val[iid] == SpkPvmUnreported) {
	    spawn_individual(iid);
	    ndone--;
	  }
      break;
    default:
      spklog("unknown message tag");
      break;
    }
    //signal_unblock();
  }
  for (int iid =  0; iid < ntasks && my_exit_value == 0; iid++) 
    if (exit_val[iid] != 0) 
      my_exit_value = -1;

  finish(my_exit_value);
  return my_exit_value;
}

