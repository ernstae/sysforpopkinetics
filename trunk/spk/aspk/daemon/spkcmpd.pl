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
use File::Path;
use Proc::Daemon;
use Spkdb('connect', 'disconnect', 'de_q2c', 'en_q2r', 'get_cmp_jobs', 'get_dataset');
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
my $pathname_co  = "/usr/bin/co";   # rcs checkout utility
my $pathname_tar = "/bin/tar";
my $row;
my $row_array;
my $tmp_dir = "/tmp";
my $working_dir_prefix = "spkjob-";
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
    my $jrow = shift;
    my $drow;
    my $dataset_id = $jrow->{"dataset_id"};
    my $job_id     = $jrow->{"job_id"};

    my $pid;
  FORK: {
      if ($pid = fork) {
	  # this is parent
	  syslog("info", "forked process with pid=$pid for job_id=$job_id");
      }
      elsif (defined $pid) {
	  # this is the child; create a working directory for the compiler
	  $working_dir = "$tmp_dir/$working_dir_prefix$$";

	  if (!mkdir $working_dir, 0777) {
	      syslog("emerg", "couldn't create working directory: $working_dir");
	      die;
	  }
	  chdir $working_dir;

	  # write our job_id to a file called "job_id" in the working directory
	  open(FH, ">job_id")
	      or death('emerg', "could not create the job_id file in $working_dir");
	  print FH "$job_id";
	  close(FH);

	  # write xml_source field to a file called source.xml
	  open(FH, ">source.xml")
	      or death('emerg', "could not create the source.xml file in $working_dir");
	  print FH $jrow->{"xml_source"};
	  close(FH);

	  # write dataset archive corresponding to dataset_id to a file called data.xml,v
	  $drow = &get_dataset($dbh, $dataset_id)
	      or death('emerg', "could not read dataset $dataset_id from database");
	  open(FH, ">data.xml,v")
	      or death('emerg', "could not create the data.xml,v file in $working_dir");
	  print FH $drow->{"archive"};
	  close(FH);

	  # execute co, the rcs check-out command, to extract the version from the archive
	  my @args = ($pathname_co, "-u$jrow->{'dataset_version'}", "data.xml");
	  system(@args);
	  my $exit_status = $? >> 8;
	  if ($exit_status != 0) {
	      death('emerg', "$pathname_co failed for job $job_id; exit_status=$exit_status");
	  }

	  # remove the archive (leaving the version in file data.xml)
	  File::Path::rmtree("data.xml,v", 0, 0);

	  # execute the spk compiler
	  my $e = exec $compiler_path "spkcompiler", ("source.xml, data.xml");

	  # this statement will never be reached, unless the exec failed
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
    # We have received the TERM signal. 
    
    # become insensitive to the TERM signal we are about to send
    local $SIG{'TERM'} = 'IGNORE';

    # send the TERM signal to every member of our process group
    kill('TERM', -$$);

    # wait for all of our children to die
    my $child_pid;
    while (($child_pid = wait()) != -1) {
	syslog('info', "process $child_pid received the TERM signal");
    }
    # now we can die
    death('info', 'received the TERM signal (normal mode of termination)');
}

# become a daemon
Proc::Daemon::Init();

# initialize
start();

# create a new process group, with this process as leader
setpgrp(0, 0);

# designate the stop subroutine (above) to catch the Unix TERM signal
$SIG{"TERM"} = \&stop;

# rerun any compiles that were interrupted when we last terminated
$row_array = &get_cmp_jobs($dbh);
syslog('info', "looking for interrupted compiler jobs");
if (defined $row_array) {
    foreach $row (@$row_array) {
	&fork_compiler($row);
    }
}
else {
    death("emerg", "error reading database: $Spkdb::errstr");
}

# loop until interrupted by a signal
use POSIX ":sys_wait_h";
my $pid_of_deceased_child;
my @args;
my $job_id;

while(1) {
    # if there is a job queued-to-compile, fork the compiler
    $row = &de_q2c($dbh);
    if (defined $row) {
	if ($row) {
	    &fork_compiler($row);
	}
    }
    else {
	death("emerg", "error reading database: $Spkdb::errstr");
    }
    # process any children that have terminated
    while (($pid_of_deceased_child = waitpid(-1, &WNOHANG)) > 0) {
	syslog("info", "process with pid=$pid_of_deceased_child terminated normally");

	# get the job_id from the file we wrote to working directory of this process
	$working_dir = "$working_dir_prefix$pid_of_deceased_child";
	open(FH, "$tmp_dir/$working_dir/job_id")
	    or death('emerg', "can't open $tmp_dir/$working_dir");
	read(FH, $job_id, -s FH);

	# make a compressed tar file from the working directory
	chdir $tmp_dir;
	@args = ($pathname_tar, 'cvzf', "$working_dir.tgz", $working_dir);
	system(@args);
	my $exit_status = $? >> 8;
	if ($exit_status != 0) {
	    death('emerg', "$pathname_tar failed creating file $working_dir.tgz; exit_status=$exit_status");
	}
	# remove the working directory
	File::Path::rmtree($working_dir, 0, 0);

	# read the tar file into memory
	open(FH, "$working_dir.tgz")
	    or death('emerg', "failed to open $working_dir.tgz");
	read(FH, my $buf, -s FH)
	    or death('emerg', "failed to read $working_dir.tgz");
	close(FH);

	# place the job in the run queue, storing the contents of the compressed
	# tar file in the cpp_source field of the job table entry
	&en_q2r($dbh, $job_id, $buf)
	    or death('emerg', "can't move job $job_id to the run queue; $Spkdb::errstr");
    }
    # sleep for a second
    sleep(1);
};


