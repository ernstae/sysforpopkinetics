/*
  NAME
  spkpop -- population level of SPK computation

  SYNOPSIS
  spkpop job_id individual_count mode

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

  This program should not write directly to either stdout or stderr.
  Instead, all event messages should be posted to the spk log using
  the spklog function.  This function prepends identification
  information to the message, then uses pvm_send to transmit it to the
  spkjob program running on the front end which, in turn, writes it to
  the log.

  The program runs in its own unique working directory, which is
  established by spkrund, the SPK runtime daemon, which is the unix
  parent of spkjob. It accesses the directory via the Network Files
  System (NFS).

  job_id is a whole number which is the key in the database identifying
  the job that is being worked on

  individual_count is the number of individuals being modeled

  mode is either "test" or "prod" depending on whether the job is 
  running in the test environment or the production environment

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

// note: ARCH is defined on the command line of the make file

// file level variables
static char *job_id = "?????";
static char *mode;
static int ntasks = 0;
static int parent_tid = 0;
static sigset_t block_set;
static const int iterations = 3;
static int my_exit_value = 0;
static int my_tid = 0;
static volatile int ndone = 0;

// file level dynamic arrays
static int  *exit_val;
static int  *ind_tid;
static int  *old_ind_tid;
static int  *host_tid;
static bool *task_exit;
static bool *respawned;

// function forward references
static void compute(int);
static void finish(int);
static void kill_all(void);
static void signal_block(void);
static void signal_handler(int);
static void signal_initialize(void);
static void signal_unblock(void);
static void spawn_individual(int);
static void spklog(const char*);
static int  tid2iid(int);
static int  old_tid2iid(int);

// perform the population calculation for an iteration
static void compute(int iter) {
  char buf[100];
  sprintf(buf, "performing population computation for iteration %d", iter);
  spklog(buf);
  sleep(1);
}

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
// kill all skpkind children still running
void kill_all() {
  int killed = 0;
  for (int iid = 0; iid < ntasks; iid++)
    if (!task_exit[iid]) {
      pvm_kill(ind_tid[iid]);
      killed++;
    }
  if (killed) {
    spklog("killed all skpkind children still running");
  }
}
// return the iid of an old spkind tid
// This is used to handle the case where an message is received from 
// an individual that has already been respawned.  
static int old_tid2iid(int tid) {
  for (int iid = 0; iid < ntasks; iid++) {
    if (old_ind_tid[iid] == tid) {
      return iid;
    }
  }
  return -1;

}
// block termination signals
static void signal_block() {
  sigprocmask(SIG_BLOCK, &block_set, NULL);
}
// handle signals 
static void signal_handler(int signum) {
  char buf[100];
  int killed = 0;
  signal_block();
  sprintf(buf, "received signal number %d", signum);
  spklog(buf);
  my_exit_value = signum;
  kill_all();
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
  char* arg[5]; 
  char buf[100];
  char ind_id[6];
  char spkjob_tid[30];
  sprintf(ind_id, "%d", iid);
  sprintf(spkjob_tid, "%d", parent_tid);
  arg[0] = job_id;
  arg[1] = ind_id;
  arg[2] = spkjob_tid;
  arg[3] = mode;
  arg[4] = NULL;
  sprintf(buf, "%s/arch/%s/bin/spk%s/spkind", SPK_SHARE, ARCH, mode);
  int rval = pvm_spawn(buf, arg, 0, NULL, 1, &tid);
  if (rval < 0) 
    die("could not spawn individual level due to system error");
  if (rval == 0) {
    const char *err_string = spkpvm_spawn_error(tid);
    sprintf(buf, "could not spawn individual level: %s", err_string);
    die(buf);
  }
  old_ind_tid[iid] = ind_tid[iid];
  ind_tid[iid]     = tid;
  host_tid[iid]    = pvm_tidtohost(tid);
  exit_val[iid]    = SpkPvmUnreported;
  task_exit[iid]   = false;
  respawned[iid]   = false;

  // Establish notification for task exit of the task we have just spawned
  if (pvm_notify(PvmTaskExit, PvmTaskExit, 1, ind_tid + iid) < 0) {
    sprintf(buf, "pvm_notify failed for PvmTaskExit for individual %d", iid);
    die(buf);
  }
  // Establish notification of deletion of the host of the task we have just
  // spawned, if that we have not previously established that notification
  bool host_notified = false;
  for (int i = 0; i < ntasks; i++) {
    if (i != iid && host_tid[i] == host_tid[iid]) {
      host_notified = true;
      break;
    }
  }
  if (!host_notified && pvm_notify(PvmHostDelete, PvmHostDelete, 1, host_tid + iid) < 0) {
    sprintf(buf, "pvm_notify failed for PvmHostDelete for individual %d", iid);
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
// return the tid of the iid-th skpkind
static int tid2iid(int tid) {
  for (int iid = 0; iid < ntasks; iid++) {
    if (ind_tid[iid] == tid)
      return iid;
  }
  return -1;
}

int main(int argc, char** argv) {
  // Disallow direct routing of messages between tasks;
  // otherwise messages will arrive out of sequence. 
  pvm_setopt(PvmRoute, PvmDontRoute);

  my_tid  = pvm_mytid();          // attach to pvm
  int my_host = pvm_tidtohost(my_tid);
  char *usage = "usage: spkpop job_id individual_count mode";
  char buf[100];
  int bufid;

  // Process arguments  
  if (argc != 4)
    die(usage);
  job_id = argv[1];
  ntasks = atoi(argv[2]);
  mode = argv[3];

  // Set up signal handling for the signals that might be used
  // by human operators to terminate us
  //signal_initialize();

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
  sprintf(buf, "%s/working/spk%s/spkjob-%s", SPK_SHARE, mode, job_id);
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

  // loop through the iterations
  int iid;
  int exit_value = 0;
  int bytes, msgtag, tid, source;

  exit_val    = new int[ntasks];
  host_tid    = new int[ntasks];
  ind_tid     = new int[ntasks];
  old_ind_tid = new int[ntasks];
  task_exit   = new bool[ntasks];
  respawned   = new bool[ntasks];
  for (iid = 0; iid < ntasks; iid++)
    ind_tid[iid] = 0;
  
  for (int iter = 1; exit_value == 0 && iter <= iterations; iter++) {
    int ndone = 0;
    bool premature_child_exit = false;
    sprintf(buf, "starting iteration %d", iter);
    spklog(buf);

    // spawn the individuals
    for (iid =  0; iid < ntasks; iid++)
      spawn_individual(iid);

    // loop while processing notification messages, until a PvmTaskExit message
    // has been received for each individual
  loop:
    while (ndone < ntasks && (bufid = pvm_recv(-1, -1)) > 0) {
      //signal_block();
      if (pvm_bufinfo(bufid, &bytes, &msgtag, &source) < 0)
	die("pvm_bufinfo failed");
      switch (msgtag) {
      case SpkPvmExitValue:
	tid = source;
	if ((iid = tid2iid(tid)) < 0) {
	  if ((iid = old_tid2iid(tid)) >= 0 && respawned[iid]) {
	    sprintf(buf, "received SpkPvmExitValue = %d from superceded spkind(%d)", exit_value, iid);
	    spklog(buf);
	    break;
	  }
	  // This should never happen.
	  sprintf(buf, "error: received SpkPvmExitValue from unexpected tid %0x", tid);
	  die(buf);
	}
	if (pvm_upkint(&exit_value, 1, 1) < 0)
	  die("pvm_upkint returned an error at SpkPvmExitValue");
	sprintf(buf, "received SpkPvmExitValue = %d from spkind(%d)", exit_value, iid);
	spklog(buf);
	if ((exit_val[iid] = exit_value) != 0)
	  die("received an error exit value from a child");
	break;
      case PvmTaskExit:
 	if (pvm_upkint(&tid, 1, 1) < 0)
	  die("pvm_upkint failed to unpack tid from PvmTaskExit message");
	if ((iid = tid2iid(tid)) < 0) {
	  if ((iid = old_tid2iid(tid)) >= 0 && respawned[iid]) {
	    sprintf(buf, "received PvmTaskExit for superceded spkind(%d)", iid);
	    spklog(buf);
	    ndone++;
	    break;
	  }
	  // This should never happen.
	  sprintf(buf, "error: received unexpected PvmTaskExit for tid %0x", tid);
	  die(buf);
 	}
	if (exit_val[iid] == SpkPvmUnreported) {
	  /*
	    The PvmTaskExit was not preceded (as it normally should be)
	    by an SpkPvmExitValue.  This means one of the following:
	    1. The SpkPvmExitValue message was lost. (should never happen)
	    2. The skpkind task died suddenly, without reporting its exit value.
	    This is a fatal error, unless caused by a host delete.
	    3. A host was deleted.  We will know this if we get a PvmHostDelete
	    message for the host that this skpkind was running on

	    For the time being, we assume 2.   
	  */
	  exit_val[iid]  = SpkPvmErrorPending;
	}
	sprintf(buf, "received PvmTaskExit for spkind(%d)", iid);
	spklog(buf);
	task_exit[iid] = true;
	ndone++;
	break;
      case PvmHostDelete:
	if (pvm_upkint(&tid, 1, 1) < 0)
	  die("pvm_upkint failed to unpack tid from PvmTaskExit message");
	sprintf(buf, "received PvmHostDelete, source=%0x, tid=%0x", source, tid);
	spklog(buf);
	/*
	  If my own host was deleted, all my children must die and I
	  must exit.  This is taken care of automatically by the pvm
	  daemon running on my host, hence there is not for me to do.
	  My parent task, spkjob, will receive a PvmHostDelete
	  notification and will respawn me on another host.

	  If another host was deleted, all of my children on that host
	  must die and I must respawn them on other hosts.  Killing
	  the children is done automatically by the pvm daemon running
	  on the deleted host.  My job is to respawn the killed
	  processes and proceed as if nothing occurred.
	*/
	for (iid = 0; iid < ntasks; iid++) {
	  if (host_tid[iid] == tid) {
	    if (exit_val[iid] == SpkPvmErrorPending || exit_val[iid] == SpkPvmUnreported) {
	      sprintf(buf, "respawned spkind[%d]", iid);
	      spklog(buf);
	      spawn_individual(iid);
	      respawned[iid] = true;
	      ndone--;
	    }
	  }
	}
	break;
      default:
	spklog("unknown message tag");
	break;
      }
    }
    /*
      At this point, a PvmTaskExit has been received for each individual.  
      If any individual task exited without having first sent its exit value
      in a SpkPvmExitValue message, it is possible that a PvmHostDelete
      message still coming to us.
     */
    bool should_wait = false;
    int  n_respawned = 0;
    for (iid = 0; iid < ntasks; iid++) {
      if (exit_val[iid] == SpkPvmErrorPending) {
	should_wait = true;
	break;
      }
    }
    if (should_wait) {
      sleep(10);
      while ((bufid = pvm_nrecv(-1, -1)) != 0) {
	if (bufid == -1)
	  die("pvm_nrecv failed");
	if (pvm_bufinfo(bufid, &bytes, &msgtag, &source) < 0)
	  die("pvm_bufinfo failed");
	if (msgtag == PvmHostDelete) {
	  if (pvm_upkint(&tid, 1, 1) < 0)
	    die("pvm_upkint failed to unpack tid from PvmTaskExit message");
	  sprintf(buf, "received PvmHostDelete, source=%0x, tid=%0x", source, tid);
	  spklog(buf);
	  // respawn individuals that were on the deleted host
	  if (my_exit_value == 0) {
		    for (int iid = 0; iid < ntasks; iid++) 
	      if (host_tid[iid] == tid && exit_val[iid] == SpkPvmErrorPending) {
		sprintf(buf, "respawned spkind[%d]", iid);
		spklog(buf);
		spawn_individual(iid);
		ndone--;
		n_respawned++;
	      }
	  }
	}   
      }
      if (n_respawned > 0)
	goto loop;
      else
	die("a child exited without first reporting an exit value");
    }
    
    // perform the population computation
    compute(iter);
  }
  finish(my_exit_value);
  return my_exit_value;
}

