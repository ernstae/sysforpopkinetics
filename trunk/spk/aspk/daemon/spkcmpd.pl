#!/usr/bin/perl -w
########################################################################
#
# This file is part of the System for Population Kinetics (SPK), which
# was developed with support from NIH grants RR-12609 and P41-
# EB001975. Please cite these grants in any publication for which this
# software is used and send a notification to the address given above.
#
# SPK is Copyright (C) 1998-2003, by the University of Washington,
# Resource Facility for Population Kinetics, and is made available as
# free open source software under the terms of the University of
# Washington Free-Fork License as a public service.  A copy of the
# License can be found in the COPYING file in the root directory of this
# distribution or can be obtained from
#     Resource Facility for Population Kinetics
#     Department of Bioengineering Box 352255
#     University of Washington
#     Seattle, WA 98195-2255
########################################################################

=head1 NAME

spkcmpd.pl -- the SPK Compiler Daemon

=head1 SYNOPSIS

spkcmpd.pl database host dbuser dbpasswd

=head1 ABSTRACT

The SPK Compiler Daemon runs continuously on the ASPK server. It
selects jobs from the compiler queue, forks the spkcompiler to
process them, and inserts them into the run qeueue,  In cases where
a job fails to compile, the daemon records the error report and
the 'end' status in the database.

=head1 DESCRIPTION

The program expects the following arguments:

    $database
        The name of the SPK database
    $host
        The host on which the SPK database resides
    $dbuser
        A database username which has read/write access to the 
        job table
    $dbpasswd
        The password associated with the username

The first think that spkcmp.pl does after starting up is to call
Proc::Daemon::Init to make it into a daemon, by shedding its 
inheirited environment and becoming a direct child of the system
init process.

It then opens the system log so that it has a place to record progress
and error messages.

It attempts to create a lock-file if one does not already exist. The
method that is used assures that the process of query and creation
is atomic.  If the lock-file already exists, the program writes an
error message to the system log and terminates, because only one 
copy of spkcmpd.pl can be allowed run at any given time.

Next, it opens the database.

The program designates itself to be a process group leader. This
way it will be able to send signals to all of its descendents without
having to know their PIDs. 

The "stop" subroutine is designated to catch the TERM signal,
when it is received. As explained below, this will allow for
an orderly shutdown of the daemon and its sub-processes.

The last major step in the start-up sequence is to select from the
database all jobs with a state_code field set to 'cmp'.  These
jobs, if any exist, had been in the process of being compiled when 
the daemon last shut down.  All such jobs are rerun.

At this point, the program enters an endless loop from which it will
escape only upon receipt of a signal. It queries the
database to discover whether or not a job has been added to the compiler
queue.  If so, a copy of spkcompiler is started as an independent
sub-process, working in its own directory on input provided by the
job. The daemon then checks to see if any child processes have 
terminated. If so, it moves them either to the run queue or to 
"end" status, depending on whether the compilation was successful
or not.  The daemon sleeps a second, before continuing the
loop.

The normal way in which spkcmp.pl is terminated is by using the
Unix "kill" command to send it the TERM signal.  When TERM is received,
execution is passed to the 'stop' subroutine, which had previously
been designated to catch this signal.

To avoid a loop, 'stop' sets the signal mask to ignore any
subsequent TERM signals. It then sends the TERM signal to every process
in its process group, which consists of the daemon itself and all of its
descendents. It waits for all sub-processes (which are 
instances of the spkcompiler) to terminate.  It closes the database
and the system log, then dies.

=head1 RETURNS

Nothing, because it has no parent (other than init) to which an exit
code might be returned.  The program does, however, write event messages
to the system log.

=cut

use strict;

use Fcntl qw(:DEFAULT :flock);
use Proc::Daemon;
use Spkdb('connect', 'disconnect', 'de_q2c', 'en_q2r');
use Sys::Syslog('openlog', 'syslog', 'closelog');

my $database = shift;
my $host     = shift;
my $dbuser   = shift;
my $dbpasswd = shift;

#my $compiler_path = "/usr/local/bin/spkcompiler";
my $compiler_path = "/usr/local/bin/spkcompilerx";
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
    
    local $SIG{'TERM'} = 'IGNORE';
    kill('TERM', -$$);

    my $child_pid;

    while (($child_pid = wait()) != -1) {
	syslog('info', "process $child_pid received the TERM signal");
    }
    death('info', 'received the TERM signal (normal mode of termination)');
}

# become a daemon
Proc::Daemon::Init();

# initialize
start();

setpgrp(0, 0);

# designate stop subroutine to catch the Unix TERM signal
$SIG{"TERM"} = \&stop;

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
    while (($pid_of_deceased_child = waitpid(-1, &WNOHANG)) > 0) {
	syslog("info", "process with pid=$pid_of_deceased_child terminated normally");
    }
    # sleep for a second
    sleep(1);
};


