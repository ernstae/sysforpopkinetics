#!/usr/bin/perl -w

use strict;

use Fcntl qw(:DEFAULT :flock);
use Proc::Daemon;
use Spkdb('connect', 'disconnect', 'de_q2c', 'en_q2r');
use Sys::Syslog('openlog', 'syslog', 'closelog');

my $database = shift;
my $host     = shift;
my $dbuser   = shift;
my $dbpasswd = shift;

my $compiler_path = "/usr/local/bin/spkcompiler";
my $dbh;
my $database_open = 0;
my $service_name = "spkcmpd";
my $lockfile_path = "/tmp/lock_$service_name";
my $lockfile_exists = 0;
my $row;
my $row_array;
my $tmp_dir = "/tmp";
my $working_dir;

sub death {
    my $level = shift;
    my $msg = shift;

    # log the reason for termination
    syslog($level, $msg);

    # close the connection to the database
    if ($database_open) {
	&disconnect($dbh)
    }

    # remove the lockfile
    if ($lockfile_exists) {
	unlink($lockfile_path);
    }
    # log final message, then close the system log
    syslog("info", "stop");
    closelog();

    die;
}
sub fork_compiler {
    use Errno qw(EAGAIN);

    my $job_id = shift;
    my $recovering = shift;
    my $pid;
  FORK: {
      if ($pid = fork) {
	  # this is parent
	  syslog("info", "forked process with pid=$pid for job_id=$job_id");
      }
      elsif (defined $pid) {
	  # this is the child
	  $working_dir = "$tmp_dir/spkcompiler-$job_id";

	  if (!$recovering && !mkdir $working_dir, 0777) {
	      syslog("emerg", "couldn't create working directory: $working_dir");
	      die;
	  }
	  chdir $working_dir;
	  my $e = exec $compiler_path "spk-compiler", ($job_id);
	  if (!$e) {
	      syslog("emerg", "couldn't exec $compiler_path");
	      die;
	  }
      }
      elsif ($! == EAGAIN) {
	  # EAGAIN indicates a fork error which may be recoverable
	  sleep(5);
	  redo FORK;
      }
      else {
	  death("emerg", "fork of compiler failed");
      }
  }

    
}
sub start {
    # open the system log and record that we have started
    openlog("$service_name, pid=$$", 'cons', 'daemon');
    syslog('info', 'start');

    # create a lockfile and store our pid, to be used later by stop()
    sysopen(FH, $lockfile_path, O_RDWR | O_CREAT)
	or death("emerg", "can't start -- could not create lockfile");
    my @info = stat(FH);
    $info[7] == 0 
	or death("emerg", "can't start -- $lockfile_path already exists");
    print FH $$;
    close(FH);
    $lockfile_exists = 1;

    # open a connection to the database
    $dbh = &connect($database, $host, $dbuser, $dbpasswd)
	or death("emerg", "can't connect to database=$database, host=$host");
    syslog("info", "connected to database=$database, host=$host");
    $database_open = 1;
}
sub stop {
    # We have received the TERM signal.  So have our children. 
    # Wait as them terminate, and clean up after them.
    
    my $child_pid;

    while (($child_pid = wait()) != -1) {
	syslog('info', "process $child_pid received the TERM signal");
    }
    death('info', 'received the TERM signal (normal mode of termination)');
}
# designate stop subroutine to catch the Unix TERM signal
$SIG{"TERM"} = \&stop;

# become a daemon
Proc::Daemon::Init();

# initialize
start();

# rerun any compiles that were interrupted when we last terminated
$row_array = &Spkdb::get_cmp_jobs($dbh);
syslog('info', "looking for interrupted compiler jobs");
if (defined $row_array) {
    foreach $row (@$row_array) {
	&fork_compiler($row->{"job_id"}, 1);
    }
}
else {
    death("emerg", "error reading database: $Spkdb::errstr");
}

# loop until interrupted by a signal
use POSIX ":sys_wait_h";
my $pid_of_deceased_child;

while(1) {
    # if there is a job queued-to-compile, fork the compiler
    $row = &de_q2c($dbh);
    if (defined $row) {
	if ($row) {
	    &fork_compiler($row->{"job_id"}, 0);
	}
    }
    else {
	death("emerg", "error reading database: $Spkdb::errstr");
    }
    # check for the death of our compiler child processes
    $pid_of_deceased_child = waitpid(-1, &WNOHANG);
    if ($pid_of_deceased_child > 0) {
	syslog("info", "process with pid=$pid_of_deceased_child terminated");
    }
    # sleep for a second
    sleep(1);
};


