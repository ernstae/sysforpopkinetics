/*
  NAME
  spkjob -- perform the computation for an SPK job
      
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
#include <spkjob.h>
#include <spklog.h>
#include <csignal>
#include <string>
#include <cstdio>
#include <cstring>
#include <map>
#include <pvm3.h>
#include <spkpvm.h>

static int my_tid;
static FILE *logfile;
static void die(char*);
static char *job_id;

static void finish(void);
static void signal_handler(int);
static void spklog(const char*);

// call this function in case of fatal error
static void die(char* message) {
  spklog(message);
  finish();
  exit(1);
}
// call this function to clean up before ending
static void finish() {
  spklog("stop");
  fclose(logfile);
}
// write a message to the spk log
static void spklog(const char *message) {
  time_t t = time(NULL);
  char* timestamp = ctime(&t);
  int len = strlen(timestamp);
  if (len > 6)
    timestamp[len - 6] = '\0';
  fprintf(logfile, "[t%0X][j%s](%s)spkjob %s\n", my_tid, job_id, timestamp, message);
  fflush(logfile);
}

// handle a termination signal
static void signal_handler(int signum) {
  spklog("job terminated by operator");
  finish();
  struct sigaction signal_action;
  signal_action.sa_handler = SIG_DFL;
  sigaction(signum,  &signal_action, NULL);
  raise(signum);
}
int main(int argc, char** argv) {
  char *usage = "usage: spkjob job_id individual_count";
  char buf[100];
  pid_t my_pid = getpid();

  // process arguments
  if (argc != 3) {
    fprintf(stderr, "%s\n", usage);
    return 1;
  }
  job_id = argv[1];
  char *individual_count = argv[2];

  // attach to pvm 
  if ((my_tid = pvm_mytid()) < 0) {
    die("pvm_tid failed");
  }

  // open the log file
  if ((logfile = fopen(SPKLOG_PATH, "a")) == NULL) {
    logfile = stdout;
    die("could not open spk log file");
  }
  // attach the standard output of (yet-to-be-spawned) descendent
  // tasks to the log file
  (void)pvm_catchout(logfile);

  // write our host name and pid to the log
  struct utsname un;
  uname(&un);
  sprintf(buf, "start: pid %d on host %s", my_pid, un.nodename);
  spklog(buf);

  // set up signal handling for the signals that might be used
  // by human operators to terminate us
  struct sigaction signal_action;
  signal_action.sa_handler = signal_handler;
  sigemptyset(&signal_action.sa_mask);
  signal_action.sa_flags = 0;
  sigaction(SIGINT,  &signal_action, NULL);
  sigaction(SIGHUP,  &signal_action, NULL);
  sigaction(SIGTERM, &signal_action, NULL);

  // spawn the population level
  char* arg[3]; 
  arg[0] = job_id;
  arg[1] = individual_count;
  arg[2] = 0;
  sprintf(buf, "%s/spkpop", codeDir);
  spklog(buf);
  int popTid[1];
  int rval = pvm_spawn(buf, arg, 0, NULL, 1, popTid);
  if (rval < 0) 
    die("could not spawn population level due to system error");
  char *err_string = "unknown";
  if (rval == 0) {
    switch (popTid[0]) {
    case PvmBadParam:
      err_string = "bad parameter";
      break;
    case PvmNoHost:
      err_string = "host not not in virtual machine";
      break;
    case PvmNoFile:
      err_string = "code file can't be found";
      break;
    case PvmNoMem:
      err_string = "malloc failed";
      break;
    case PvmSysErr:
      err_string = "pvmd not responding";
      break;
    case PvmOutOfRes:
      err_string = "out of resources";
      break;
    default:
      break;
    }
    sprintf(buf, "could not spawn population level: %s", err_string);
    die(buf);
  }
    

  // establish communications with the population level
  if (pvm_notify(PvmTaskExit, PvmC::taskExit, 1, popTid) < 0)
    die("pvm_notify failed for PvmTaskExit of the population task");
  if (pvm_notify(PvmHostDelete, PvmC::hostDelete, 1, popTid) < 0)
    die("pvm_notify failed for PvmHostDelete of the host for the population task");
  
  // loop until the population level exits or we are terminated
  int bufid;
  while ((bufid = pvm_recv(-1, -1)) >= 0) {
    int bytes, msgtag, tid;
    if (pvm_bufinfo(bufid, &bytes, &msgtag, &tid) < 0)
      die("pvm_bufinfo failed");
    printf("received message from tid=%0X\n", tid);
    switch (msgtag) {
    case PvmC::taskExit:
      if (pvm_upkint(&tid, 1, 1) < 0)
	die("pvm_upkint returned an error at taskExit");
      break;
    case PvmC::hostDelete:
      break;
    default:
      break;
    }
  }
  if (bufid < 0)
    die("pvm_recv returned an error");

  // finish up
  spklog("job finished");
  finish();
  return 0;
}
