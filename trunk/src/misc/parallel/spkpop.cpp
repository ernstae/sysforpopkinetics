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
#include <pvm3.h>
#include <cstdlib>
#include <cstdio>
#include <time.h>
#include <cstring>
#include <csignal>
#include <spkjob.h>

static int my_tid = 0;
static char *job_id = "?????";
static int ntasks = 0;
static int parent_tid = 0;
static sigset_t block_set;

static void finish(int);
static void signal_block(void);
static void signal_handler(int);
static void signal_initialize(void);
static void signal_unblock(void);

static void spklog(const char*);

// call this function in case of fatal error
static void die(char* message) {
  spklog(message);
  finish(1);
  exit(1);
}
// call this function to clean up before ending
static void finish(int exit_value) {
  char buf[100];
  pvm_initsend(PvmDataDefault);
  pvm_pkint(&exit_value, 1, 1);
  pvm_send(parent_tid, SpkPvmExitValue);
  spklog("stop");
}
// block termination signals
static void signal_block() {
  sigprocmask(SIG_BLOCK, &block_set, NULL);
}
// handle a termination signal
static void signal_handler(int signum) {
  spklog("terminated by spkjob");
  finish(1);
  exit(1);
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

/*
  Write a message to the SPK log.  It is assumed that spkjob, which is
  either the pvm parent or grandparent of this instance, set up the
  connection of our standard output to the log file, which resides on
  the head node, by invoking pvm_catchout.
*/
static void spklog(const char* message) {
  time_t t = time(NULL);
  char* timestamp = ctime(&t);
  int len = strlen(timestamp);
  if (len > 6)
    timestamp[len - 6] = '\0';
  printf("[j%s](%s)spkpop: %s\n", job_id, timestamp, message);
  fflush(stdout);
}

int main(int argc, char** argv) {
  pvm_setopt(PvmRoute, PvmRouteDirect);
  int my_tid = pvm_mytid();          // attach to pvm
  char *usage = "spkpop job_id individual_count";
  char buf[100];
  int bufid;

  // process arguments  
  if (argc != 3)
    die(usage);
  job_id = argv[1];
  ntasks = atoi(argv[2]);
  
  // set up signal handling for the signals that might be used
  // by human operators to terminate us
  signal_initialize();

  parent_tid = pvm_parent();

  // write our host name to the log
  struct utsname un;
  uname(&un);
  sprintf(buf, "start: on host %s", un.nodename);
  spklog(buf);

  // loop while waiting for messages
  /*
  while ((bufid = pvm_recv(parent_tid, -1)) >= 0) {
    spklog("spkpop received a message");
    int bytes, msgtag, junk, tid[1];
    if (pvm_bufinfo(bufid, &bytes, &msgtag, tid) < 0)
      die("pvm_bufinfo failed");
    switch (msgtag) {
    case SpkPvmKill:
      if (*tid != parent_tid) {
	sprintf(buf, "received SpkPvmKill from unexpected tid %0x", *tid);
	spklog(buf);
      } 
      else {
	die("killed by spkjob");
      }
      break;
    default:
      break;
    }
  }
  */
  for (int i = 0; i < 10; i++) {
    sleep(1);
  }
  finish(0);
  return 0;
}

