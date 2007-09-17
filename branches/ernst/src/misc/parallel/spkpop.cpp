/*
  NAME
  spkpop -- population level of the spk parallel prototype

  SYNOPSIS
  spkpop job_id individual_count mode

  DESCRIPTION

  This program is the population level of the SPK parallel prototype.
  It is the pvm child task of spkjob. It runs through a fixed number
  of iterations (the final product will have more sophisticated means
  of determining when a sufficient number of iterations have
  occurred.)  With each iteration, it runs a number of instances of
  spkind, one for each individual in the model. If all spkind
  instances perform their calculations successfully, spkpop performs a
  population-level calculation to complete the iteration.

  Spkpop runs in a working directory created for it by spkjob. 
  For its part, it creates a sub-directory for each of the spkind
  instances that it spawns.

  Spkpop can run on any node of the pvm. The particular node it
  happens to be on depends on the load distribution algorithms of pvm.
  As a unix-style process, it is the child of the pvm daemon running
  on the node. As a pvm task, it is the child of spkjob.

  RUNNING
  This program should only be run by spkjob in a parallel virtual
  machine (pvm).

  DESIGN CONSTRAINTS AND GOALS
  See the similar documentation for spkjob.

  FILES
  See the similar documentation for spkjob

  BUGS 
  The code in this program assumes that when an event occurs, pvm
  sends event notice messages in the order notification requests were
  originally registered by calls to pvm_notify().  In particular, when
  the host on which one or more spkind instances are running is
  deleted, the PvmHostDelete message arrives before any of the
  PvmTaskExit messages.  This behavior appears to be an undocumented
  implementation artifact.  If it changes in some future version of
  pvm, the message processing logic of this program will break.

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
#include <spkpvm.h>
#include <csignal>

// note: ARCH is defined in Makefile

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

// file level dynamic arrays; storage to be defined later
static int  *exit_val;
static int  *ind_tid;
static int  *old_ind_tid;
static int  *host_tid;
static bool *task_exit;
static int  *respawn_state;

// function forward references
static void compute(int);
static void finish(int);
static void kill_all(void);
static int  respawn(int);
static void spawn_individual(int);
static void spklog(const char*);
static int  tid2iid(int);
static int  old_tid2iid(int);
static void signal_handler(int);
static void signal_initialize(void);

static const int NoRespawn     = 0;
static const int RespawnNeeded = 1;
static const int Respawned     = 2;

// perform the population calculation for an iteration (dummy routine)
static void compute(int iter) {
  char buf[100];
  sprintf(buf, "performing population computation for iteration %d", iter);
  spklog(buf);
  sleep(1);
}
// call this function in case of fatal error
static void die(char* message) {
  spklog(message);
  if (my_exit_value == 0)
    my_exit_value = SpkPvmDied;
  finish(my_exit_value);
  exit(my_exit_value);
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
// This is used to handle the case where a message is received from 
// an individual that has already been respawned.  
static int old_tid2iid(int tid) {
  for (int iid = 0; iid < ntasks; iid++) {
    if (old_ind_tid[iid] == tid) {
      return iid;
    }
  }
  return -1;
}
// respawn all individuals which were running on a given host
static int respawn(int host) {
  int iid, n = 0;
  char buf[100];
  /*
    In this code, setting host_tid[iid]=0 for an individual that is
    going to be respawned is critical to avoid a kind of race
    condition.  If we did not do this, it is possible that the deleted
    host would be added back and one of the respawned spkind processes
    would be started on it.  The fact that the tid of this host was
    still in the host_tid array would keep us from setting up a
    PvmHostDelete notification for it (we only want one notification
    per host).
  */
  for (iid = 0; iid < ntasks; iid++)
    if (host_tid[iid] == host && exit_val[iid] == SpkPvmUnreported) {
      respawn_state[iid] = RespawnNeeded;
      host_tid[iid] = 0;
    }
  /*
    Now that we have cleared the host from host_tid[], we can do the
    spawning.
  */
  for (iid = 0; iid < ntasks; iid++)
    if (respawn_state[iid] == RespawnNeeded) {
      sprintf(buf, "respawned spkind[%d]", iid);
      spklog(buf);
      spawn_individual(iid);
      respawn_state[iid] = Respawned;
      n++;
    }
  return n;
}
// handle signals 
static void signal_handler(int signum) {
  char buf[100];
  int killed = 0;
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
  for (int i = 0; i < spkpvm_siglist_length; i++)
    sigaddset(&block_mask, spkpvm_siglist[i]);
  signal_action.sa_handler = signal_handler;
  signal_action.sa_flags = 0;
  signal_action.sa_mask = block_mask;
  for (int i = 0; i < spkpvm_siglist_length; i++)
    sigaction(spkpvm_siglist[i],  &signal_action, NULL);
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
  //  sprintf(buf, "%s/arch/bin/%s/spk%s/spkind", SPK_SHARE, ARCH, mode);
  sprintf(buf, "spkind");
  int rval = pvm_spawn(buf, arg, 0, NULL, 1, &tid);
  if (rval < 0) 
    die("could not spawn individual level due to system error");
  if (rval == 0) {
    const char *err_string = spkpvm_spawn_error(tid);
    sprintf(buf, "could not spawn individual level: %s", err_string);
    die(buf);
  }
  old_ind_tid[iid]     = ind_tid[iid];  // remember tid after respawn
  ind_tid[iid]         = tid;
  host_tid[iid]        = pvm_tidtohost(tid);
  exit_val[iid]        = SpkPvmUnreported;
  task_exit[iid]       = false;
  respawn_state[iid]   = NoRespawn;
  // Establish notification of deletion of the host of the task we have just
  // spawned, if we haven't already asked to be notified for this host
  // WARNING! Do not change the order of these notifications.
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
  // Establish notification for task exit of the task we have just spawned
  if (pvm_notify(PvmTaskExit, PvmTaskExit, 1, ind_tid + iid) < 0) {
    sprintf(buf, "pvm_notify failed for PvmTaskExit for individual %d", iid);
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
// return the tid of the iid-th spkind
static int tid2iid(int tid) {
  for (int iid = 0; iid < ntasks; iid++) {
    if (ind_tid[iid] == tid)
      return iid;
  }
  return -1;
}

int main(int argc, char** argv) {
  // Disallow direct routing of messages between tasks; otherwise
  // messages will arrive out of sequence.
  pvm_setopt(PvmRoute, PvmDontRoute);

  my_tid  = pvm_mytid();
  int my_host = pvm_tidtohost(my_tid);
  char *usage = "usage: spkpop job_id individual_count mode";
  char buf[100];
  int bufid;

  // Process arguments  
  if (argc != 4)
    die(usage);
  job_id = argv[1];
  ntasks = atoi(argv[2]);
  mode   = argv[3];

  // Set up signal handling
  signal_initialize();

  parent_tid = pvm_parent();

  // Write my host name to the log
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
  int iid;
  int exit_value = 0;
  int bytes, msgtag, tid, source;

  exit_val        = new int[ntasks];
  host_tid        = new int[ntasks];
  ind_tid         = new int[ntasks];
  old_ind_tid     = new int[ntasks];
  task_exit       = new bool[ntasks];
  respawn_state   = new int[ntasks];
  for (iid = 0; iid < ntasks; iid++)
    ind_tid[iid] = 0;

  // ITERATION LOOP

  for (int iter = 1; exit_value == 0 && iter <= iterations; iter++) {
    int ndone = 0;
    bool premature_child_exit = false;
    sprintf(buf, "starting iteration %d", iter);
    spklog(buf);

    // spawn the individuals
    for (iid =  0; iid < ntasks; iid++)
      spawn_individual(iid);

    // MESSAGE PROCESSING LOOP
    // loop while processing notification messages, until a PvmTaskExit message
    // has been received for each individual
  loop:
    while (ndone < ntasks && (bufid = pvm_recv(-1, -1)) > 0) {
      if (pvm_bufinfo(bufid, &bytes, &msgtag, &source) < 0)
	die("pvm_bufinfo failed");
      if (bufid < 0)
	die("pvm_recv returned an error");
      switch (msgtag) {
      case SpkPvmExitValue:   
	// PROCESS EXIT VALUE MESSAGE FROM AN SPKIND INSTANCE
	tid = source;
	// Test to see if the message came from one of my children. If not, die.
	if ((iid = tid2iid(tid)) < 0) { 
	  sprintf(buf, "error: received SpkPvmExitValue from unexpected tid %0x", tid);
	  die(buf);
	}
	// It's one of mine.  Unpack the exit value from the message and log it.
	if (pvm_upkint(&exit_value, 1, 1) < 0)
	  die("pvm_upkint returned an error at SpkPvmExitValue");
	sprintf(buf, "received SpkPvmExitValue = %d from spkind(%d)", exit_value, iid);
	spklog(buf);
	// If my spkind child died with an error, so must I.
	if ((exit_val[iid] = exit_value) != 0)
	  die("received an error exit value from a child");
	break;
      case PvmTaskExit: 
	// PROCESS A TASK EXIT NOTIFICATION FROM PVM
 	if (pvm_upkint(&tid, 1, 1) < 0)
	  die("pvm_upkint failed to unpack tid from PvmTaskExit message");
	// Test for the case where one or more spkind children have
	// just been respawned to recover from a host deletion and
	// this is the task exit for one of the superceded children.
	if ((iid = old_tid2iid(tid)) >= 0 && respawn_state[iid] == Respawned) {
	  sprintf(buf, "received PvmTaskExit for superceded spkind(%d)", iid);
	  spklog(buf);
	  ndone++;
	  break;
	}
	// Test that the notification is really for one of my spkind
	// children; if not, die.
	if ((iid = tid2iid(tid)) < 0) {
	  sprintf(buf, "error: received unexpected PvmTaskExit for tid %0x", tid);
	  die(buf);
 	}
	// Yes, it's a child of mine.  I should have already received
	// its exit value.  If not, die.
	if (exit_val[iid] == SpkPvmUnreported) {
	  sprintf(buf, "spkind(%d) died without returning an exit valule", iid);
	  die(buf);
	}
	// Everything in order.  This child completed successfully.
	sprintf(buf, "received PvmTaskExit for spkind(%d)", iid);
	spklog(buf);
	task_exit[iid] = true;
	ndone++;
	break;
      case PvmHostDelete:
	// PROCESS A HOST DELETE NOTIFICATION FROM PVM
	if (pvm_upkint(&tid, 1, 1) < 0)
	  die("pvm_upkint failed to unpack tid from PvmTaskExit message");
	sprintf(buf, "received PvmHostDelete, source=%0x, tid=%0x", source, tid);
	spklog(buf);
	// If my own host was deleted, pvm will kill me and all of my
	// children, and then spkjob will respawn me.  If another host
	// was deleted, I need to respawn all of my children that were
	// running on that host.
	ndone -= respawn(tid);
	break;
      default:
	spklog("unknown message tag");
	break;
      }
    }
    // PERFORM A POPULATION-LEVEL COMPUTATION
    compute(iter);
  }
  finish(my_exit_value);
  return my_exit_value;
}
