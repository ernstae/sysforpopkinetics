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
use bytes;

use Fcntl qw(:DEFAULT :flock);
use File::Path;
use POSIX qw(:signal_h);
use Proc::Daemon;
use Spkdb('connect', 'disconnect', 'de_q2c', 'en_q2r', 'end_job',
	  'get_cmp_jobs', 'get_dataset');
use Sys::Syslog('openlog', 'syslog', 'closelog');

my $database = shift;
my $host     = shift;
my $dbuser   = shift;
my $dbpasswd = shift;

my $dbh;
my $database_open = 0;
my $filename_cerr_report = "compilation_error.xml";
my $filename_serr_report = "software_error.xml";
my $service_name = "spkcmpd";
my $lockfile_path = "/tmp/lock_$service_name";
my $lockfile_exists = 0;
my $pathname_co  = "/usr/bin/co";   # rcs checkout utility
my $pathname_compiler = "/usr/local/bin/spkcompiler";
my $pathname_tar = "/bin/tar";
my $prefix_working_dir = "spkcmp-";
my $tmp_dir = "/tmp";

sub death {
    # Call this subroutine only from parent, never from child
    my $level = shift;
    my $msg = shift;

    # Log the reason for termination
    syslog($level, $msg);

    # Close the connection to the database
    if ($database_open) {
	&disconnect($dbh)
    }

    # Remove the lockfile
    if ($lockfile_exists) {
	unlink($lockfile_path);
    }
    # Log final message, then close the system log
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

    # Block SIGCHLD while we are in this subroutine
    my $sigset   = POSIX::SigSet->new;
    my $blockset = POSIX::SigSet->new(SIGCHLD);
    sigprocmask(SIG_BLOCK, $blockset, $sigset)
	or death('emerg', "could not block SIGCHLD in repear");

    # NOTE: Working Directory Name
    #
    # The name of the compiler working directory can change over
    # the life of the job:
    # 1. A working directory must be created before the compiler is
    #    forked as a child of this daemon, so that the compiler
    #    will start up in a directory unique to itself in which it
    #    will find its input files and into which it can write
    #    its output.  So that the name is unique, the unique job_id
    #    is suffixed to a prefix indicating that this is a compile.
    #    This daemon writes the job_id to a file within the working
    #    directory called "job_id", for later reference.
    # 2. After the fork, the pid of the child is known.  At that 
    #    point, the name of the directory is changed so that the
    #    suffix is the job_id.  This will be needed later when the
    #    child dies, because at that point the parent (this daemon)
    #    will be provided with the pid but will not know the job_id.
    #    After going to the working directory, the parent will
    #    discover the job_id by reading the contents of the "job_id"
    #    file, written in step 1.
    # 3. If a core dump was created when the child died, the 
    #    name of the working directory is changed so that, once
    #    again, the suffix is the job_id. This makes it easy for
    #    software engineers to find the dump in order to analyze
    #    it.  All other working directories are removed, when 
    #    the compiler terminates.

    # Create a working directory
    my $unique_name = "$prefix_working_dir$job_id";
    my $working_dir = "$tmp_dir/$unique_name";
    mkdir($working_dir, 0777) 
	or death("emerg", "couldn't create working directory: $working_dir");

    # Write the job_id to a file name "job_id"
    chdir $working_dir;
    open(FH, ">job_id")
	or death('emerg', "could not create the job_id file in $working_dir");
    print FH "$job_id";
    close(FH);

    # Write the xml_source field from the job row in the database to
    # a file called "source.xml"
    open(FH, ">source.xml")
	or death('emerg',
                 "could not create the source.xml file in $working_dir");
    print FH $jrow->{"xml_source"};
    close(FH);

    # Write the archive field of the dataset row corresponding to
    # the dataset_id from the job row to a filed called data.xml,v
    # (Note, this is the filename format expected by rcs)
    $drow = &get_dataset($dbh, $dataset_id)
	or death('emerg', "could not read dataset $dataset_id from database");
    open(FH, ">data.xml,v")
	or death('emerg', 
                 "could not create the data.xml,v file in $working_dir");
    print FH $drow->{"archive"};
    close(FH);

    # Execute co, the rcs check-out command, to extract the version
    # from the archive, creating a file called "data.xml"
    my @args = ($pathname_co, "-u$jrow->{'dataset_version'}", "data.xml");
    system(@args);
    my $exit_status = $? >> 8;
    if ($exit_status != 0) {
	death('emerg', 
              "$pathname_co failed for job $job_id; exit_status=$exit_status");
    }

    # Remove the archive file, leaving just the extracted dataset.
    File::Path::rmtree("data.xml,v", 0, 0);

    # Fork into parent and child
  FORK: {
      if ($pid = fork) {
	  # This is the parent
	  syslog("info", "forked process with pid=$pid for job_id=$job_id");
      }
      elsif (defined $pid) {
	  # This is the child.  NOTE: do not call death() in this block.
	  # Open the system log for the child, so that messages are properly
	  # identified as coming from the child.
	  closelog();
	  openlog("$service_name-child, pid=$$", 'cons', 'daemon');

	  # Change the name of the working directory to reflect the child pid.
	  # When the child terminates, the parent will be provided with this pid,
	  # and hence will be able to identify the working directory of the
	  # child.
	  rename "$working_dir", "$tmp_dir/$prefix_working_dir$$"
	      or do {
		  syslog("emerg", "couldn't rename working directory");
		  die;
	      };
	  # Execute the spk compiler, overlaying the child
	  @args = ($pathname_compiler, "source.xml", "data.xml");
	  my $e = exec(@args);

	  # This statement will never be reached, unless the exec failed
	  if (!$e) {
	      syslog("emerg", "couldn't exec $pathname_compiler");
	      die;
	  };
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
    sigprocmask(SIG_SETMASK, $sigset)
	or death('emerg', "could not restore SIGCHLD in reaper");
}
sub format_error_report {
    my $content = shift;
    my $report = "<spkreportML>\n";
    $report .= "  <error_message>\n";
    $report .= "    $content\n";
    $report .= "  </error_message>\n";
    $report .= "</spkreportML>\n";
}
sub reaper {
    # Handler for SIGCHLD which processes any children which have terminated
    # Block SIGCHLD while we are in here 
    my $sigset   = POSIX::SigSet->new;
    my $blockset = POSIX::SigSet->new(SIGCHLD);
    sigprocmask(SIG_BLOCK, $blockset, $sigset)
	or death('emerg', "could not block SIGCHLD in repear");

    while ((my $child_pid = waitpid(-1, &WNOHANG)) > 0) {
	my $child_exit_value    = $? >> 8;
	my $child_signal_number = $? & 0x7f;
	my $child_dumped_core   = $? & 0x80;
	my $err_msg;
	my $job_id;
	my $report;
	my $status_msg = "";

	# Get the job_id from the file we wrote to working directory
	# of this process
	my $working_dir = "$prefix_working_dir$child_pid";
	chdir "$tmp_dir/$working_dir";
	open(FH, "job_id")
	    or death('emerg', "can't open $tmp_dir/$working_dir/job_id");
	read(FH, $job_id, -s FH);
	close(FH);

	# Prepare part of an error message, which might be needed
	if ($child_exit_value != 0) {
	    $status_msg .= "exit value = $child_exit_value; ";
	}
	if ($child_signal_number == SIGSEGV) {
	    $status_msg .= "segmentation fault; ";
	}
	elsif($child_signal_number != 0) {
	    $status_msg .= "received signal number $child_signal_number";
	}
	if ($child_dumped_core) {
	    $status_msg .= "core dump in $tmp_dir/$prefix_working_dir$job_id";
	}

	# Did the compiler exit normally?
	if ($child_exit_value == 0 && $child_signal_number == 0) {
	    # Make a tar file from the working directory
	    my @args = ($pathname_tar, 'cf', "cpp_source.tar");
	    push @args, glob("*");
	    system(@args);
	    my $exit_status = $? >> 8;
	    if ($exit_status != 0) {
		death('emerg', "tar failed creating file $working_dir.tar;"
                             . " exit_status=$exit_status");
	    }
	    # Read the tar file into memory
	    my $buf;
	    open(FH, "cpp_source.tar")
		or death('emerg', "failed to open cpp_source.tar");
	    read(FH, $buf, -s FH)
		or death('emerg', "failed to read cpp_source.tar");
	    close(FH);

	    # Place the job in the run queue, storing the contents of the
	    # tar file in the cpp_source field of the job table entry
	    &en_q2r($dbh, $job_id, $buf)
		or death('emerg', "job_id=$job_id: errstr: $Spkdb::errstr");
	    syslog('info', "job_id=$job_id compiled and has moved to run queue");
	}
	# Did the compiler find errors in the user's source?
	elsif (-f $filename_cerr_report && -s $filename_cerr_report != 0) {
	    open(FH, $filename_cerr_report);
	    read(FH, $err_msg, -s FH);
	    close(FH);
	    $report = format_error_report($err_msg);
	    &end_job($dbh, $job_id, "cerr", $report)
		or death('emerg', "job_id=$job_id: $Spkdb::errstr");
	    syslog('info',
                   "job_id=$job_id failed compilation due to source errors");
	}
	# Did the compiler die because of a software fault?
	elsif (-f $filename_serr_report){
	    open(FH, $filename_serr_report);
	    read(FH, $err_msg, -s FH);
	    close(FH);
	    $report = format_error_report($err_msg);
	    &end_job($dbh, $job_id, "serr", $report)
		or death('emerg', "job_id=$job_id: $Spkdb::errstr");
	    syslog('info', "job_id=$job_id failed due to a compiler bug");
	} 
	# Did the compiler die because it was aborted by an operator
        # suspecting a bug?
	elsif ($child_signal_number == SIGABRT
            || $child_signal_number == SIGTERM) {
	    $err_msg = "Compiler killed by operator: $status_msg";
	    $report = format_error_report($err_msg);

	    &end_job($dbh, $job_id, "serr", $report)
		or death('emerg', "job_id=$job_id: $Spkdb::errstr");
	    syslog('info', "job_id=$job_id terminated by operator who "
                          ."suspected a compiler bug");
	}
	else {
	    $err_msg = "Compiler died: $status_msg";
	    $report = format_error_report($err_msg);
	    &end_job($dbh, $job_id, "herr", $report)
		or death('emerg', "job_id=$job_id: $Spkdb::errstr");
	    syslog('info', "job_id=$job_id died unexpectedly during "
		          ."compile: $status_msg");
	}
	if ($child_dumped_core) {
	    # Rename the working directory with the job_id to make core
            # dump easy to find
	    rename "$tmp_dir/$working_dir", "$tmp_dir/$prefix_working_dir$job_id"
		or death('emerg', "couldn't rename working directory");
	}
	else {
	    # Remove the working directory
	    File::Path::rmtree("$tmp_dir/$working_dir");
	}
    }
    sigprocmask(SIG_SETMASK, $sigset)
	or death('emerg', "could not restore SIGCHLD in reaper");
}
sub start {
    # Open the system log and record that we have started
    openlog("$service_name, pid=$$", 'cons', 'daemon');
    syslog('info', 'start');

    # Create a lockfile and store our pid, so only one copy of parent can run
    sysopen(FH, $lockfile_path, O_RDWR | O_CREAT)
	or death("emerg", "can't start -- could not create lockfile");
    my @info = stat(FH);
    $info[7] == 0 
	or death("emerg", "can't start -- $lockfile_path already exists");
    print FH $$;
    close(FH);
    $lockfile_exists = 1;

    # Open a connection to the database
    $dbh = &connect($database, $host, $dbuser, $dbpasswd)
	or death("emerg", "can't connect to database=$database, host=$host");
    syslog("info", "connected to database=$database, host=$host");
    $database_open = 1;
}
sub stop {
    # We have received the TERM signal. 
    
    # Become insensitive to the TERM signal we are about to send
    local $SIG{'TERM'} = 'IGNORE';

    # Remove reaper as catcher of SIGCHLD
    $SIG{'CHLD'} = 'DEFAULT';

    # Send the TERM signal to every member of our process group
    kill('TERM', -$$);

    # Wait for all of our children to die
    my $child_pid;
    while (($child_pid = wait()) != -1) {
	syslog('info', "process $child_pid received the TERM signal");
    }
    # Now we must die
    death('info', 'received the TERM signal (normal mode of termination)');
}
my $row;
my $row_array;

# Become a daemon
Proc::Daemon::Init();

# Initialize
start();

# Create a new process group, with this process as leader.  This will
# allow us to send the TERM signal to all of our children with a single
# command.
setpgrp(0, 0);

# Block certain signals
$SIG{'ABRT'} = 'IGNORE';
$SIG{'HUP'}  = 'IGNORE';
$SIG{'INT'}  = 'IGNORE';
$SIG{'QUIT'} = 'IGNORE';

# Designate handlers for certain signals
$SIG{'CHLD'} = \&reaper;
$SIG{'TERM'} = \&stop;

# Rerun any compiles that were interrupted when we last terminated
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

# Loop until interrupted by a signal
use POSIX ":sys_wait_h";

while(1) {
    # If there is a job queued-to-compile, fork the compiler
    $row = &de_q2c($dbh);
    if (defined $row) {
	if ($row) {
	    &fork_compiler($row);
	}
    }
    else {
	death("emerg", "error reading database: $Spkdb::errstr");
    }
    # sleep for a second
    sleep(1); # DO NOT REMOVE THIS LINE 
              # or else this daemon will burn all your CPU cycles!
};
