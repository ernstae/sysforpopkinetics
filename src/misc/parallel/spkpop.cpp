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
  parent of spkjob.

  SEE ALSO
  spkjob
  spkrund
  pvm
*/

#include <unistd.h>
#include <sys/utsname.h>
#include <spklog.h>
#include <pvm3.h>
#include <cstdlib>
#include <cstdio>
#include <time.h>
#include <cstring>
#include <csignal>

static int my_tid;
static char *job_id = "?????";
static int ntasks = 0;

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
  pvm_exit();
}
// handle a termination signal
static void signal_handler(int signum) {
  spklog("job terminated by operator -- this is abnormal!");
  finish();
  struct sigaction signal_action;
  signal_action.sa_handler = SIG_DFL;
  sigaction(signum,  &signal_action, NULL);
  raise(signum);
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
  printf("[j%s](%s)spkpop %s)\n", job_id, timestamp, message);
}

int main(int argc, char** argv) {
  int my_tid = pvm_mytid();          // attach to pvm
  char *usage = "spkpop job_id individual_count";
  char buf[100];

  // process arguments  
  if (argc != 3)
    die(usage);
  job_id = argv[1];
  ntasks = atoi(argv[2]);
  
  // set up signal handling for the signals that might be used
  // by human operators to terminate us
  struct sigaction signal_action;
  signal_action.sa_handler = signal_handler;
  sigemptyset(&signal_action.sa_mask);
  signal_action.sa_flags = 0;
  sigaction(SIGINT,  &signal_action, NULL);
  sigaction(SIGHUP,  &signal_action, NULL);
  sigaction(SIGTERM, &signal_action, NULL);

  // write our host name to the log
  struct utsname un;
  uname(&un);
  sprintf(buf, "start: on host %s", un.nodename);
  spklog(buf);
		
  sleep(10);
  spklog("stop");
  finish();
  return 0;
}

