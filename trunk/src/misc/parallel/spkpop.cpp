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

static char *job_id = "?????";
static char *mode;
static int ntasks = 0;
static int parent_tid = 0;
static sigset_t block_set;
static const int iterations = 3;
static int *ind_tid;
static int *host_tid;
static bool *task_exit;
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
// handle signals 
static void signal_handler(int signum) {
  char buf[100];
  int killed = 0;
  signal_block();
  sprintf(buf, "received signal number %d", signum);
  spklog(buf);
  my_exit_value = signum;
  for (int iid = 0; iid < ntasks; iid++)
    if (exit_val[iid] == SpkPvmUnreported) {
      pvm_kill(ind_tid[iid]);
      killed++;
    }
  if (killed > 0)
    spklog("killed remaining spkind tasks");
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
  ind_tid[iid]  = tid;
  host_tid[iid] = pvm_tidtohost(tid);
  exit_val[iid] = SpkPvmUnreported;
  task_exit[iid] = false;

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

  // loop until the population level is done
  int iid;
  int exit_value = 0;
  int bytes, msgtag, tid, source;

  for (int iter = 1; exit_value == 0 && iter <= iterations; iter++) {
    int ndone = 0;
    bool premature_child_exit = false;
    bool respawned = false;
    sprintf(buf, "starting iteration %d", iter);
    spklog(buf);
    ind_tid   = new int[ntasks];
    host_tid  = new int[ntasks];
    exit_val  = new int[ntasks];
    task_exit = new bool[ntasks];

    // spawn the individuals
    for (iid =  0; iid < ntasks; iid++)
      spawn_individual(iid);
    while (ndone < ntasks && (bufid = pvm_recv(-1, -1)) > 0) {
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
	if (my_exit_value == 0 && exit_value != 0) {
	  int killed = 0;
	  signal_block();
	  my_exit_value = SpkPvmChildError;
	  ndone++;
	  for (int iid = 0; iid < ntasks; iid++)
	    if (exit_val[iid] == SpkPvmUnreported) {
	      pvm_kill(ind_tid[iid]);
	      killed++;
	      ndone++;
	    }
	  if (killed > 0)
	    spklog("killed remaining spkind tasks");
	} else
	  ndone++;
	break;
       case PvmTaskExit:
 	if (pvm_upkint(&tid, 1, 1) < 0)
 	  die("pvm_upkint failed to unpack tid from PvmTaskExit message");
 	if ((iid = tid2iid(tid)) < 0) {
	  // This happens at the beginning of one of the iterations
	  // 2, 3, ... because the reception of exit values completes the 
	  // previous iteration rather than task exit.  The task is now
	  // unknown, because the tid's for the new iteration are different.
	  // We just ignore this.
 	}
	else {
	  if (exit_val[iid] == SpkPvmUnreported) {
	    sleep(10);  // give PvmHostDelete time to arrive if such a message is en route
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
		    if (host_tid[iid] == tid && exit_val[iid] == SpkPvmUnreported) {
		      spawn_individual(iid);
		      respawned++;
		    }
		  if (respawned) {
		    sprintf(buf, "respawned %d individuals", respawned);
		    spklog(buf);
		  }
		}
	      }
	    }
	    if (!respawned) {
	      sprintf(buf, "spkind(%d) died without returning a value", iid);
	      spklog(buf);
	      exit_val[iid] = SpkPvmDied;
	      ndone++;
	    }
	  }
	  task_exit[iid] = true;
	}
 	break;
      case PvmHostDelete:
	if (pvm_upkint(&tid, 1, 1) < 0)
	  die("pvm_upkint failed to unpack tid from PvmTaskExit message");
	sprintf(buf, "received PvmHostDelete, source=%0x, tid=%0x", source, tid);
	spklog(buf);

	// If my host has been deleted,
	// 1. All my spkint children must die (so as not to waste resources)
	// 2. I must exit
	// The pvm daemon running on my host kills all pvm tasks, hence there
	// is nothing more for me to do.
	//
	// If another host has been deleted,
	// 1. All my spkint children running on that host must die
	// 2. I must respawn all individuals that were on the deleted host
	// respawn individuals that were on the deleted host
	// The pvm daemon running on the deleted host takes care of 1) for me
	// 
	// respawn spkint children that were running on the deleted host
	if (my_exit_value == 0) 
	  for (int iid = 0; iid < ntasks; iid++) 
	    //	    if (host_tid[iid] == tid && exit_val[iid] == SpkPvmUnreported) {
	    if (host_tid[iid] == tid && !task_exit[iid]) {
	      spawn_individual(iid);
	      ndone--;
	    }
	break;
      default:
	spklog("unknown message tag");
	break;
      }
    }
    // At this point, all of the individuals should have completed their work
    // and reported their exit values.  If, however, one or more individuals 
    // exited prematurely, it is possible that this occurred because the 
    // host on which they were running was deleted from the pvm.  We need to
    // check for this condition, and if true, restart the individuals on 
    // another host.
//    if (premature_child_exit) {
//      sleep(10);  // give PvmHostDelete time to arrive if such a message is en route
//      while ((bufid = pvm_nrecv(-1, -1)) != 0) {
//	if (bufid == -1)
//	  die("pvm_nrecv failed");
//	if (pvm_bufinfo(bufid, &bytes, &msgtag, &source) < 0)
//	  die("pvm_bufinfo failed");
//	if (msgtag == PvmHostDelete) {
//	  if (pvm_upkint(&tid, 1, 1) < 0)
//	    die("pvm_upkint failed to unpack tid from PvmTaskExit message");
//	  sprintf(buf, "received PvmHostDelete, source=%0x, tid=%0x", source, tid);
//	  spklog(buf);
//	  // respawn individuals that were on the deleted host
//	  if (my_exit_value == 0) {
//	    int respawned = 0;
//	    for (int iid = 0; iid < ntasks; iid++) 
//	      if (host_tid[iid] == tid && exit_val[iid] == SpkPvmUnreported) {
//		spawn_individual(iid);
//		ndone--;
//	      }
//	    if (respawned) {
//	      sprintf(buf, "respawned %d individuals", respawned);
//	      spklog(buf);
//	      continue;
//	    }
//	  }
//	}
//      }
//    }
    for (int iid =  0; iid < ntasks && my_exit_value == 0; iid++) 
      if (exit_val[iid] != 0) 
	my_exit_value = SpkPvmChildError;

    if (my_exit_value == 0)
      compute(iter);
  }
  finish(my_exit_value);
  return my_exit_value;
}

