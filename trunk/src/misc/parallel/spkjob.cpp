/*
  NAME
  spook -- perform the computation for an SPK job
      
  SYNOPSIS
  spkjob job_id individual-count

  DESCRIPTION 
  This program should run on the head node of the cluster.  It has
  reponsibility for initialization, for spawning the population level
  of the computation, and for cleanup.  It writes messages directly to
  the spk log file, which is on the same node.  The standard output of
  descendent nodes is directed to this same file, through the magic of
  pvm.

  job_id is the whole number which is the key to the job in the spk
  database.

  individual-count is the number of individuals being modelled.
*/

#include <sys/types.h>
#include <sys/utsname.h>
#include <unistd.h>
#include <time.h>
#include <csignal>
#include <string>
#include <cstdio>
#include <cstring>
#include <map>
#include <spkpvm.h>

static int my_tid;
static int pop_tid;
static int pop_host;
static bool pop_done = false;
static int pop_exit_value = SpkPvmUnreported;
static FILE *logfile;
static void die(char*);
static char *job_id;
static sigset_t block_set;
static volatile bool terminating = false;

static void finish(void);
static void signal_block(void);
static void signal_handler(int);
static void signal_initialize(void);
static void signal_unblock(void);
static void spklog(const char*);
static void writelog(const char*);

// call this function in case of fatal error
static void die(char* message) {
  spklog(message);
  finish();
  exit(SpkPvmDied);
}
// call this function to clean up before ending
static void finish() {
  spklog("stop");
  fclose(logfile);
}
// block termination signals
static void signal_block() {
  sigprocmask(SIG_BLOCK, &block_set, NULL);
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
    finish();
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
  sigaction(SIGINT,  &signal_action, NULL);
  sigaction(SIGHUP,  &signal_action, NULL);
  sigaction(SIGTERM, &signal_action, NULL);
  // set up mask of signals to be blocked while executing critical sections
  sigemptyset(&block_set);
  sigaddset(&block_set, SIGINT);
  sigaddset(&block_set, SIGHUP);
  sigaddset(&block_set, SIGTERM);
}
// unblock termination signals
static void signal_unblock() {
  sigprocmask(SIG_UNBLOCK, &block_set, NULL);
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
  char *usage = "usage: spkjob job_id individual_count";
  char buf[100];
  pid_t my_pid = getpid();
  int bufid;
    
  // Process arguments
  if (argc != 3) {
    fprintf(stderr, "%s\n", usage);
    return 1;
  }
  job_id = argv[1];
  char *individual_count = argv[2];

  // Enroll in pvm
  if ((my_tid = pvm_mytid()) < 0) {
    die("pvm_tid failed");
  }
  // Disallow direct routing of messages between tasks;
  // otherwise messages will arrive out of sequence. 
  pvm_setopt(PvmRoute, PvmDontRoute);

  // Open the log file
  if ((logfile = fopen(SPKLOG_PATH, "a")) == NULL) {
    logfile = stdout;
    die("could not open spk log file");
  }
  // Attach the standard output of (yet-to-be-spawned) descendent
  // tasks to the log file.
  //(void)pvm_catchout(logfile);

  // Write our host name and pid to the log
  struct utsname un;
  uname(&un);
  sprintf(buf, "start pid %d on host %s", my_pid, un.nodename);
  spklog(buf);

  // Set up signal handling for the signals that might be used
  // by human operators to terminate us.
  signal_initialize();

 pop_start:
  // Spawn the population level
  char* arg[3]; 
  arg[0] = job_id;
  arg[1] = individual_count;
  arg[2] = NULL;
  sprintf(buf, "%s/arch/%s/bin/spkpop", SPK_SHARE, ARCH);
  int rval = pvm_spawn(buf, arg, 0, NULL, 1, &pop_tid);
   if (rval < 0) 
    die("could not spawn population level due to system error");
  if (rval == 0) {
    const char *err_string = spkpvm_spawn_error(pop_tid);
    sprintf(buf, "could not spawn population level: %s", err_string);
    die(buf);
  }
  if ((pop_host = pvm_tidtohost(pop_tid)) < 0)
    die("can't get host for spkpop");
  // Change working directory
  sprintf(buf, "%s/spkjob-%s", SPK_WORKING, job_id);
  if (chdir(buf) != 0)
    die("could not change working directory");

  // Establish pvm notifications
  if (pvm_notify(PvmTaskExit, PvmTaskExit, 1, &pop_tid) < 0)
    die("pvm_notify failed for PvmTaskExit of the population task");
  if (pvm_notify(PvmHostDelete, PvmHostDelete, 1, &pop_host) < 0)
    die("pvm_notify failed for PvmHostDelete of the host for the population task");

  // Loop until the population level is done

  while (!pop_done && ((bufid = pvm_recv(-1, -1)) > 0)) {
    int bytes, msgtag, junk, tid[1];
    if (pvm_bufinfo(bufid, &bytes, &msgtag, tid) < 0)
      die("pvm_bufinfo failed");
    if (bufid < 0)
      die("pvm_nrecv returned an error");
    switch (msgtag) {
    case SpkPvmExitValue:
      if (*tid != pop_tid) {
	sprintf(buf, "received SpkPvmExitValue from unexpected tid %0x", *tid);
	spklog(buf);
      }
      if (pvm_upkint(&pop_exit_value, 1, 1) < 0)
	die("pvm_upkint returned an error at SpkPvmExitValue");
      sprintf(buf, "received exit value = %d from spkpop", pop_exit_value);
      spklog(buf);
      break;
    case PvmTaskExit:
      if (pvm_upkint(tid, 1, 1) < 0)
	die("pvm_upkint returned an error at PvmTaskExit");
      spklog("received PvmTaskExit for spkpop");
      pop_done = pop_exit_value != SpkPvmUnreported;
      break;
    case PvmHostDelete:
      spklog("PvmHostDelete received; restarting spkpop");
      goto pop_start;
      break;
    case SpkPvmLogMessage:
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
  finish();
  return pop_exit_value;
}
