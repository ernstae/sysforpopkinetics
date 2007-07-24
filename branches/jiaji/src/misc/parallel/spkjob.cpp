/*
  NAME
  spkjob -- drive the spk parallel protype
      
  SYNOPSIS
  spkjob job_id individual-count mode

  DESCRIPTION
  This program is the root process in the SPK parallel prototype.
  It runs on a parallel virtual machine (pvm), which is a set of linux
  computers clustered together by the pvm software.  It has
  responsibility for a single SPK job, including the initialization,
  logging messages, spawning the population level and cleanup. It
  always runs on the head node of the pvm.  It writes messages
  directly to the unified SPK log file, which is located on the same
  node. It assists descendant tasks, running on the same or different
  nodes, to write to the SPK log, by receiving their messages and
  copying them to that log.

  RUNNING
  In the production environment, the program that will be developed
  from this prototype, will be executed as the (unix) child process of
  the runtime daemon.  As a prototype, it can be run from the command
  line.  To assist in running the prototype from the command line,
  there is a script named newjob.sh in the same cvs directory as the
  rest of the source code.

  ACCESS PRIVILEGE
  Spkjob is designed to be run by an ordinary user rather than
  root. If root privilege were used to run spkjob on the head node,
  root privilege would also be required on all of the other nodes.
  This would be undesirable from a security point of view.  Instead,
  the ordinary user must be a member of the cspk group, which must
  exist on all nodes and have the same GID on all nodes.

  ARGUMENTS
  job_id is a whole number which is the key to the job table of the
  spkdb database, identifying the job to be run.  The prototype does
  not access the database, however, so this can be any number.  Each
  time that the prototype is run, spkjob creates a new working
  directory called "spkjob-nnn", where nnn is the job_id.  If that
  directory already exists, spkjob takes an error exit. If you use
  the newjob.sh script to run prototype jobs, this error will be
  avoided because the script uses a different job_id each time that
  it runs.

  individual-count is the number of individuals being modeled.

  mode is either "test" or "prod" depending on whether the job is 
  running in the test environment or the production environment

  DESIGN CONSTRAINTS AND GOALS
  1. Any cluster management system can be used, as long as pvm
     is supported.
  2. Fault tolerance is required. The prototype must automatically
     handle the addition and deletion of nodes; it must terminate
     cleanly when any of the tasks is killed by the pvm console
     operator.
  3. Gnu C++ libraries are assumed.
  4. A distributed file system is required. NFS is satisfactory 
     in this regard.

  FILES
  The directory /usr/local/spk/share must be mounted on each node of
  the pvm. There are a number of distributed file systems that can be
  used for this, including the ancient but ubiquitous NFS, which is
  known not to handle write concurrency very well but is adequate for
  SPK, because it has been designed so that write concurrency is not
  required.

  The file hierarchy looks like this:

      /usr/local/spk/share/working/spkprod
      ............................/spktest
      ..................../log/spkprod
      ......................../spktest
      ..................../include/spkprod
      ............................/spktest
      ..................../arch/LINUXI386/lib/spkprod
      ......................................./spktest
      ........................./LINUXX86_64/lib/spkprod
      ........................................./spktest
      ..................../arch/LINUXI386/bin/spkprod
      ......................................./spktest
      ........................./LINUXX86_64/bin/spkprod
      ........................................./spktest
	    

  BUGS 
  The code in this program assumes that when an event occurs, pvm
  sends event notice messages in the order notification requests were
  originally registered by calls to pvm_notify().  In particular, when
  the host on which spkpop is running is deleted, the PvmHostDelete
  message arrives before the PvmTaskExit message.  This behavior
  appears to be an undocumented implementation artifact.  If it
  changes in some future version of pvm, the message processing logic
  of this program will break.

  SEE ALSO
  spkjob
  spkind
  spkrund
  pvm

 */

#include <sys/types.h>
#include <sys/utsname.h>
#include <unistd.h>
#include <csignal>

#include <time.h>
#include <string>
#include <cstdio>
#include <cstring>
#include <map>
#include <spkpvm.h>

// note: ARCH is defined in the Makefile

static int my_tid;
static int pop_tid = 0;
static int old_pop_tid;
static int pop_host;
static int pop_exit_value = SpkPvmUnreported;
static FILE *logfile;
static void die(char*);
static char *job_id;
static sigset_t block_set;
static volatile bool terminating = false;

static void finish(int);
static void spklog(const char*);
static void writelog(const char*);

static void signal_handler(int);
static void signal_initialize(void);

// call this function in case of fatal error
static void die(char* message) {
  spklog(message);
  finish(SpkPvmDied);
  exit(SpkPvmDied);
}
// call this function to clean up before ending
static void finish(int exit_value) {
  char buf[100];
  sprintf(buf, "stop; exit value = %d", exit_value);
  spklog(buf);
  fclose(logfile);
  pvm_exit();
}
// handle a termination signal
static void signal_handler(int signum) {
  if (!terminating) {
    spklog("termination signal received");    
    char buf[100];
    pvm_kill(pop_tid);
  } 
  else {
    spklog("a second termination signal received");    
    finish(signum);
    exit(1);
  }
  terminating = true;
}
// initialize signal handling
static void signal_initialize() {
  // set up the normal signal action
  struct sigaction signal_action;
  signal_action.sa_handler = signal_handler;
  signal_action.sa_flags = 0;
  for (int i; i < spkpvm_siglist_length; i++)
    sigaction(spkpvm_siglist[i],  &signal_action, NULL);
}
// format a message for the spk log
static void spklog(const char *message) {
  char buf[100];
  sprintf(buf, "j%s\tt%0x\tspkjob:\t\t%s", job_id, my_tid, message);
  writelog(buf);
}
// timestamp and write a message to the spk log
static void writelog(const char *message) {
  static char timestamp[100];
  time_t t = time(NULL);
  struct tm* tm = localtime(&t);
  sprintf(timestamp, "%04d:%02d:%02d:%02d:%02d:%02d",
	  tm->tm_year + 1900,
	  tm->tm_mon  + 1,
	  tm->tm_mday,
	  tm->tm_hour,
	  tm->tm_min,
	  tm->tm_sec);
  fprintf(logfile, "%s %s\n", timestamp, message);
  fflush(logfile);
}
int main(int argc, char** argv) {
  char *usage = "usage: spkjob job_id individual_count mode";
  char buf[100];
  pid_t my_pid = getpid();
  int bufid;
    
  // Process arguments
  if (argc != 4) {
    fprintf(stderr, "%s\n", usage);
    return 1;
  }
  job_id = argv[1];
  char *individual_count = argv[2];
  char *mode = argv[3];

  // Enroll in pvm
  if ((my_tid = pvm_mytid()) < 0) {
    die("pvm_mytid failed");
  }  
  // Disallow direct routing of messages between tasks; otherwise
  // messages would be able to arrive out of sequence, which would
  // greatly complicate the logic of this program as well as the 
  // time-stamping of the log.
  pvm_setopt(PvmRoute, PvmDontRoute);

  // Open the log file
  sprintf(buf, "%s/log/spk%s/messages", SPK_SHARE, mode);
  if ((logfile = fopen(buf, "a")) == NULL) {
    char msg[100];
    logfile = stdout;
    sprintf(msg, "could not open spk log file: %s", buf);
    die(msg);
  }
  // Write the name of my host and my pid to the log
  struct utsname un;
  uname(&un);
  sprintf(buf, "start pid %d on host %s", my_pid, un.nodename);
  spklog(buf);

  // Set up signal handling, so that I can trap signals that might
  // otherwise cause me to terminate without cleaning up.
  signal_initialize();

 pop_start:  // label for restarting in the case that the host on
             // which spkpop was running was deleted from the pvm
  old_pop_tid = pop_tid;

  // Spawn the population level
  char* arg[4]; 
  arg[0] = job_id;
  arg[1] = individual_count;
  arg[2] = mode;
  arg[3] = NULL;
  //  sprintf(buf, "%s/arch/bin/%s/spk%s/spkpop", SPK_SHARE, ARCH, mode); 
  sprintf(buf, "spkpop");
  int rval = pvm_spawn(buf, arg, 0, NULL, 1, &pop_tid);
  if (rval < 0) 
    die("could not spawn population level due to system error");
  if (rval == 0) {
    const char *err_string = spkpvm_spawn_error(pop_tid);
    sprintf(buf, "could not spawn population level: %s", err_string);
    die(buf);
  }
  if ((pop_host = pvm_tidtohost(pop_tid)) < 0)
    die("can't determine which host spkpop started on");

  // Change working directory
  sprintf(buf, "%s/working/spk%s/spkjob-%s", SPK_SHARE, mode, job_id);
  if (chdir(buf) != 0)
    die("could not change working directory");

  // Register to have pvm notify me if my host is deleted and when
  // spkpop exits.
  // WARNING! Do not change the order of these notifications.
  if (pvm_notify(PvmHostDelete, PvmHostDelete, 1, &pop_host) < 0)
    die("pvm_notify failed for PvmHostDelete of the host for the population task");
  if (pvm_notify(PvmTaskExit, PvmTaskExit, 1, &pop_tid) < 0)
    die("pvm_notify failed for PvmTaskExit of the population task");

  // MESSAGE PROCESSING LOOP.  I read messages from pmv and from
  // spkpop until spkpop sends me its exit value.  Note that the
  // blocking form of the pvm message input function, pvm_recv(), is
  // used.
  while (pop_exit_value == SpkPvmUnreported && ((bufid = pvm_recv(-1, -1)) > 0)) {
    int bytes, msgtag, junk, tid, source;
    if (pvm_bufinfo(bufid, &bytes, &msgtag, &source) < 0)
      die("pvm_bufinfo failed");
    if (bufid < 0)
      die("pvm_recv returned an error");
    switch (msgtag) {
    case SpkPvmExitValue:   
      // PROCESS EXIT VALUE MESSAGE FROM SPKPOP
      tid = source;
      if (tid != pop_tid) {
	sprintf(buf, "received SpkPvmExitValue from unexpected tid %0x", tid);
	die(buf);            
      }
      // Unpack the exit value from the message.
      if (pvm_upkint(&pop_exit_value, 1, 1) < 0)
	die("pvm_upkint returned an error at SpkPvmExitValue");
      sprintf(buf, "received exit value = %d from spkpop", pop_exit_value);
      spklog(buf);
      break;
    case PvmTaskExit:  
      // PROCESS TASK EXIT NOTIFICATION FROM PVM
      // Unpack the tid from the message.
      if (pvm_upkint(&tid, 1, 1) < 0)
	die("pvm_upkint returned an error at PvmTaskExit");
      // Test for the case where spkpop has just been respawned to
      // recover from a host deletion and this is the task exit for
      // the previous instance.
      if (tid == old_pop_tid) {
	spklog("received PvmTaskExit for superceded spkpop");
	break;
      }
      // Test that the notification is really for spkpop
      if (tid != pop_tid) {
	sprintf(buf, "received PvmTaskExit from unexpected tid %0x", tid);
	die(buf);            
      }
      // Yes, it is my spkpop that exited.
      spklog("received PvmTaskExit for spkpop");

      // I should have already received its exit value. If not, die.
      if (pop_exit_value == SpkPvmUnreported) {
	sprintf(buf, "spkpop died without returning a value");
	spklog(buf);
	die(buf);
      }
      break;
    case PvmHostDelete:  
      // The host on which spkpop was running has been deleted. I can 
      // spawn spkpop again and let pvm run it on a different host.
      spklog("PvmHostDelete received; restarting spkpop");
      spklog("respawned spkpop");
      goto pop_start;
      break;
    case SpkPvmLogMessage:            
      // Received a message from one of my decendents.  Log it.
      if (pvm_upkstr(buf) < 0)
	die("pvm_upkstr return an error at SpkPvmLogMessage");
      writelog(buf);
      break;
    default:
      spklog("unknown message tag");
      break;
    }
  }
  // Finish up
  finish(pop_exit_value);
  return pop_exit_value;
}
