/*
  NAME
  spkind -- individual level of SPK computation

  SYNOPSIS
  spkpop job_id individual_id

  DESCRIPTION

  This program performs the SPK computation for a single individual.
  It is started by the population level, spkpop, and runs the
  individual optimization repeatedly, once for each iteration of the
  population level.  It can run on any node of a parallel virtual
  machine (pvm).

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

static void finish(int);
static void signal_handler(int);
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
// handle a termination signal
static void signal_handler(int signum) {
  spklog("terminated by spkpop");
  finish(1);
  exit(1);
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
  printf("[j%s] (%s) 2 spkind(%s): %s\n", job_id, timestamp, ind_id, message);
  //  fflush(stdout);
}

int main(int argc, char** argv) {
  pvm_setopt(PvmRoute, PvmRouteDirect);
  int my_tid = pvm_mytid();          // attach to pvm
  char *usage = "spkind job_id individual_id";
  char buf[100];
  int bufid;

  // process arguments  
  if (argc != 3)
    die(usage);
  job_id = argv[1];
  ind_id = argv[2];
  
  // set up signal handling for the signals that might be used
  // by human operators to terminate us
  struct sigaction signal_action;
  signal_action.sa_handler = signal_handler;
  sigemptyset(&signal_action.sa_mask);
  signal_action.sa_flags = 0;
  sigaction(SIGINT,  &signal_action, NULL);
  sigaction(SIGHUP,  &signal_action, NULL);
  sigaction(SIGTERM, &signal_action, NULL);

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


  for (int i = 0; i < 10; i++) {
    sleep(1);
  }
  finish(0);
  return 0;
}

