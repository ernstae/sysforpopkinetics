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

spkrund.pl -- the SPK Run Time Daemon

=head1 SYNOPSIS

spkrund.pl database host dbuser dbpasswd

=head1 ABSTRACT

The SPK run daemon executes continuously on the CSPK server. It
selects jobs from the run queue, forks processes to comopile, link
and run them and inserts results into the database.

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
copy of spkrund.pl can be allowed run at any given time.

Next, it opens the database.

The program designates itself to be a process group leader. This
way it will be able to send signals to all of its descendents without
having to know their PIDs. 

The "stop" subroutine is designated to catch the TERM signal,
when it is received. As explained below, this will allow for
an orderly shutdown of the daemon and its sub-processes.

The last major step in the start-up sequence is to select from the
database all jobs with a state_code field set to 'run'.  These
jobs, if any exist, had been in the process of running when 
the daemon last shut down.  All such jobs are rerun.

At this point, the program enters an endless loop from which it will
escape only upon receipt of a signal. It queries the
database to discover whether or not a job has been added to the run
queue.  If so, a copy of runner is started as an independent
sub-process, working in its own directory on input provided by the
job. The daemon then checks to see if any child processes have 
terminated. If so, it moves them to "end" status and stores the
results in the database. The daemon sleeps a second, before continuing
its loop.

The normal way in which spkcmp.pl is terminated is by using the
Unix "kill" command to send it the TERM signal.  When TERM is received,
execution is passed to the 'stop' subroutine, which had previously
been designated to catch this signal.

To avoid a loop, 'stop' sets the signal mask to ignore any
subsequent TERM signals. It then sends the TERM signal to every process
in its process group, which consists of the daemon itself and all of its
descendents. It waits for all sub-processes (which are 
instances of the runner) to terminate.  It closes the database
and the system log, then dies.

NOTE: Life-Cycle of the Working Directory Name

  1. A working directory must be created before each run-time is 
     forked so that the process will start up in its own unique space.
     There it will find all of the files created for it by the SPK
     compiler and there it will write its output.  To insure that the
     name of the working directory is unique, a suffix of the form
     "-job-jjjj" is appended to the name, where "jjjj" is the unique
     job_id that was assigned by the database management system when
     the job was created. 
  2. Once the working directory has been created, the daemon uses the
     tar application to expand the contents of the "cpp_source" field
     of the row for this job in the job table of the database. In effect,
     it recreates the working directory as it was on the server which
     hosts the SPK compiler at the time that compiler finished its
     work with the job. In addition to the source code and data needed
     for the run, there is a file called "job_id" which only the job_id,
     placed there for convenient future reference.
  3. When the process is forked into a parent process and a child
     process, the Linux or Unix operating system assigns a process
     identifier (pid) number to the child.  Process identifiers are
     recycled, but only after a long time or when the system is 
     rebooted.  The parent could find the working directories of its
     children by maintaining a table relating pid to job_id. We take
     a slightly different approach, which is simple and robust. As
     soon as it is forked, the child changes the name of its working
     directory to reflect its pid rather than its job_id.  The 
     directory is renamed so that the name suffix is "-pid-pppp" where
     "pppp" is the pid.
  4. When a child process dies, the parent receives its pid as the 
     value returned from the waitpid system call.  Using this, it
     easily constructs the name of the working directory. After 
     extracting results and placing them in the database, the 
     working directory is normally removed, unless the constant 
     $retain_working_directory has been initialized to be true, or
     in case the run died due to an internal error. So that the
     retained working directory can be easily identified by software
     maintainers, the directory name is once again changed to 
     have "-job-jjjj" as its suffix.  The parent gets the job_id by
     reading the contents of a file in the working directory, called
     job_id, which was placed there by the SPK compiler daemon.

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
use Spkdb('connect', 'disconnect', 'de_q2r', 'en_q2r', 'get_run_jobs', 'end_job', 'email_for_job');
use Sys::Syslog('openlog', 'syslog', 'closelog');

my $database = shift;
my $host     = shift;
my $dbuser   = shift;
my $dbpasswd = shift;
my $mode     = shift;

my $bugzilla_production_only = 1;
my $bugzilla_url = "http://192.168.2.2:8081/";

my $max_concurrent = 1;
my $concurrent = 0;

my $service_root = "spkrun";
my $bugzilla_product = "SPK";
my $submit_to_bugzilla = 1;
my $retain_working_dir = 0;

my $dbh;
my $build_failure_exit_value = 101;
my $database_open = 0;

my $filename_data = "data.xml";
my $filename_job_id = "job_id";
my $filename_makefile = "generatedMakefile";
my $filename_optimizer_trace = "optimizer_trace.txt";
my $filename_results = "result.xml";
my $filename_runner = "driver";
my $filename_serr = "software_error";
my $filename_source = "source.xml";

my $spk_library_path = "/usr/local/lib/spkprod";
my $cpath = "/usr/local/include/spkprod";


if ($mode =~ "test") {
    $submit_to_bugzilla = !$bugzilla_production_only;
    $service_root .= "test";
    $bugzilla_product = "TestProduct";
    $retain_working_dir = 1;
    $spk_library_path = "/usr/local/lib/spktest";
    $cpath = "/usr/local/include/spktest";
}
my $service_name = "$service_root" . "d";
my $prefix_working_dir = $service_root;

my $pathname_bugzilla_submit = "/usr/local/bin/bugzilla-submit";
my $pathname_make = "/usr/bin/make";
my $pathname_tar = "/bin/tar";

my $lockfile_path = "/tmp/lock_$service_name";

my $lockfile_exists = 0;
my $spk_version = "0.1";
my $tmp_dir = "/tmp";

sub death {
    #
    # Only call this from the parent, never from the child
    #
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
sub fork_runner {
    use Errno qw(EAGAIN);

    my $jrow = shift;
    my $job_id = $jrow->{'job_id'};
    my $cpp_source = $jrow->{'cpp_source'};
    my $pid;


    # Create a working directory
    my $unique_name = $prefix_working_dir . "-job-" . $job_id;
    my $working_dir = "$tmp_dir/$unique_name";
    if (-d $working_dir) {
	File::Path::rmtree($working_dir, 0, 0);
    }
    mkdir($working_dir, 0700) 
	or death("emerg", "couldn't create working directory: $working_dir");

    # Expand the cpp_source archive in the working directory
    chdir $working_dir
	or death('emerg', "couldn't change directory to $working_dir");
    my $archive_name = "cpp_source.tar";
    open(FH, ">$archive_name")
	or death('emerg', "could not open the $archive_name file in $working_dir");
    print FH $cpp_source;
    close(FH);
    if (-s $archive_name != length $cpp_source) {
	death('emerg', "write of $archive_name failed");
    }
    my @args = ($pathname_tar, "xf", $archive_name);
    unless (system(@args) == 0)  {
	death("emerg", "couldn't expand $archive_name");
    }
    File::Path::rmtree($archive_name);

    # Fork the process into parent and child

  FORK: {
      if ($pid = fork) {
	  # This is the parent (fork returned a nonzero value)
	  $concurrent++;
	  syslog("info", "forked process with pid=$pid for job_id=$job_id");
      }
      elsif (defined $pid) {
	  # This is the child (fork returned zero).
	  # NOTE: do not call death() in this block.
	  #
	  # Open the system log for the child, so that messages are properly
	  # identified as coming from the child.
	  closelog();
	  openlog("$service_name-child, pid=$$", 'cons', 'daemon');

	  # Change the name of the working directory to reflect the child pid.
	  # When the child terminates, the parent will be provided with this pid,
	  # and hence will be able to identify the working directory of the
	  # child.
	  my $old_working_dir = $working_dir;
	  $working_dir = "$tmp_dir/$prefix_working_dir" . "-pid-" . $$;
	  if (-d $working_dir) {
	      File::Path::rmtree($working_dir, 0, 0);
	  }
	  rename $old_working_dir, $working_dir
	      or do {
		  syslog('emerg', "can't rename working directory");
		  die;
	      };
	  # Compile and link the runner
	  @args = ($pathname_make, "-f", $filename_makefile);
	  unless (system(@args) == 0)  {
	      $! = 101;
	      #$? = $build_failure_exit_value << 8;
	      die;
	  }
	  # Redirect Standard output to a file
	  open STDOUT, ">$filename_optimizer_trace";

	  # Redirect Standard Error to a file
	  open STDERR, ">$filename_serr";

	  # execute the spkrunner
	  @args = ("./$filename_runner", $filename_source, $filename_data);
	  my $e = exec(@args);

	  # this statement will never be reached, unless the exec failed
	  if (!$e) {
	      syslog("emerg", "couldn't exec $filename_runner for $job_id");
	      die;
	  }
      }
      elsif ($! == EAGAIN) {
	  # Error (fork returned an undefined value).
	  # If the Unix errno is EAGAIN, the fork may work next time if we
	  # retry it.
	  sleep(5);
	  redo FORK;
      }
      else {
	  # Unrecoverable error.  Something is seriously wrong.
	  death("emerg", "fork of run-time driver failed");
      }
  }
}
sub format_error_report {
    my $content = shift;
    my $report = "<spkreport>\n";
    $report   .= "  <error_message>\n";
    $report   .= "    $content\n";
    $report   .= "  </error_message>\n";
    $report   .= "</spkreport>\n";
    return $report;
}
sub insert_optimizer_trace {
    my $trace = shift;
    my $report = shift;

    $trace = "  <opt_trace_out>\n" . $trace . "\n  </opt_trace_out>\n";
    $report =~ s/<\/spkreport>/$trace<\/spkreport>\n/;
    return $report;
}
sub reaper {
    my $child_pid = shift;
    my $value = shift;

    my $child_exit_value    = $value >> 8;
    my $child_signal_number = $value & 0x7f;
    my $child_dumped_core   = $value & 0x80;
    my $remove_working_dir = 0;
    my $job_id;
    my $optimizer_trace;
    my $report;
    my $end_code;

    $concurrent--;

    # Get the job_id from the file spkcmpd.pl wrote to the working
    # directory of this process
    my $unique_name = "$prefix_working_dir" . "-pid-" . $child_pid;
    my $working_dir = "$tmp_dir/$unique_name";
    chdir $working_dir;
    open(FH, $filename_job_id)
	or death('emerg', "can't open $working_dir/job_id");
    read(FH, $job_id, -s FH);
    close(FH);

    # Rename working directory to make evidence easier to find
    my $old_working_dir = $working_dir;
    $unique_name = $prefix_working_dir . "-job-" . $job_id;
    $working_dir = "$tmp_dir/$unique_name";
    if (-d $working_dir) {
	File::Path::rmtree($working_dir, 0, 0);
    }
    rename $old_working_dir, $working_dir
	or death('emerg', "couldn't rename working directory");

    # Get optimizer trace 
    if (-f $filename_optimizer_trace) {
	open(FH, $filename_optimizer_trace)
	    or death('emerg', "can't open $working_dir/$filename_optimizer_trace");
	read(FH, $optimizer_trace, -s FH);
	close(FH);
    }

    # Normal termination at end of run
    if (-f $filename_results) {
	$end_code = "srun";

	# Read the results file into the report variable
	open(FH, $filename_results)
	    or death('emerg', "can't open $$working_dir/$filename_results");
	read(FH, $report, -s FH);
	close(FH);

	# Place a message in the system log
	syslog('info', "job_id=$job_id terminated normally");

	# We will remove the working directory
	$remove_working_dir = 1;
    }
    # Error termination
    else {
	$end_code = "unknown";
	my $err_msg = "";
	my $err_rpt = "";
	if ($child_exit_value == $build_failure_exit_value) {
	    $end_code = "serr";
	    $err_msg .= "c++ program generated by SPK failed to build; ";
	}
	elsif ($child_exit_value != 0) {
	    $end_code = "serr";
	    $err_msg .= "exit value = $child_exit_value; ";
	}
	if ($child_signal_number == SIGABRT) {
	    $end_code = "serr";
	    $err_msg .= "software bug asserted; ";
	}
	elsif ($child_signal_number == SIGTERM) {
	    $end_code = "serr";
	    $err_msg .= "killed by operator; ";
	}
	elsif ($child_signal_number == SIGSEGV) {
	    $end_code = "herr";
	    $err_msg .= "segmentation fault; ";
	}
	elsif($child_signal_number != 0) {
	    $end_code = "herr";
	    $err_msg .= "killed with signal $child_signal_number; ";
	}
	if (-f $filename_serr && -s $filename_serr > 0) {
	    open(FH, $filename_serr)
		or death('emerg', "can't open $working_dir/$filename_serr");
	    read(FH, $err_rpt, -s FH);
	    close(FH);
	    $end_code = "serr";
	    $err_msg .= "software bug caught as exception; ";
	}
	if ($end_code =~ "unknown") {
	    $end_code = "herr";
	    $err_msg = "run died unexpectedly; ";
	}
	if ($child_dumped_core) {
	    $err_msg .= "core dump in $working_dir; ";
	}

	# Get email address of user
	my $email = &email_for_job($dbh, $job_id);

	# Format error report and place a message in system log
	$report = format_error_report("$err_msg $err_rpt");
	syslog('info', "job_id=$job_id: $err_msg");

	# Submit runtime bugs to bugzilla
	if ($submit_to_bugzilla && ($end_code == "serr" || $end_code == "herr")) {
	    my $summary = $end_code == "serr" ? "soft" : "hard";
	    my @args = ($pathname_bugzilla_submit);
	    push @args, "--product",     $bugzilla_product;
	    push @args, "--version",     $spk_version;
	    push @args, "--component",   "CSPK";
	    push @args, "--priority",    "P4";
	    push @args, "--severity",    "critical";
	    push @args, "--summary",     "'job_id=$job_id, runtime $summary error'";
	    if (defined $email) {
		push @args, "--cc", $email;
	    }
	    push @args, "--description", $err_msg;
	    push @args, "--no-stdin";
	    push @args, "$bugzilla_url";
	    system(@args);
	    my $exit_status = $? >> 8;
	    if ($exit_status != 0) {
		syslog('emerg', "bugzilla-submit failed with exit_status=$exit_status");
	    }
	}
    }

    # Remove working directory if not needed
    if ($remove_working_dir && !$retain_working_dir) {
	File::Path::rmtree($working_dir);
    }
    
    if (length($optimizer_trace) > 0) {
	$report = insert_optimizer_trace($optimizer_trace, $report);
    }
    &end_job($dbh, $job_id, $end_code, $report)
	or death('emerg', "job_id=$job_id: $Spkdb::errstr");
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
    
    # Become insensitive to TERM signal we are about to broadcast
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
my $row;
my $row_array;

# become a daemon
Proc::Daemon::Init();

# initialize
start();

# Add directories of shared libraries to the load path
$ENV{LD_LIBRARY_PATH} = "/usr/lib:" . $spk_library_path;
$ENV{CPATH} = $cpath;

# Create a new process group, with this process as leader.  This will
# allow us to send the TERM signal to all of our children with a single
# command.
setpgrp(0, 0);

# Block certain signals
$SIG{'ABRT'} = 'IGNORE';
$SIG{'HUP'}  = 'IGNORE';
$SIG{'INT'}  = 'IGNORE';
$SIG{'QUIT'} = 'IGNORE';

# Designate a handler for the "terminate" signal
$SIG{'TERM'} = \&stop;

# rerun any runs that were interrupted when we last terminated
$row_array = &get_run_jobs($dbh);
syslog('info', "looking for interrupted computational runs");
if (defined $row_array) {
    foreach $row (@$row_array) {
	&fork_runner($row);
    }
}
else {
    death("emerg", "error reading database: $Spkdb::errstr");
}

# loop until interrupted by a signal
use POSIX ":sys_wait_h";
my $child_pid;
syslog('info', "processing new computational runs");

while(1) {
    # if there is a job queued-to-run, fork the runner
    if ($concurrent < $max_concurrent) {
	$row = &de_q2r($dbh);
	if (defined $row) {
	    if ($row) {
		&fork_runner($row);
	    }
	}
	else {
	    death("emerg", "error reading database: $Spkdb::errstr");
	}
    }
    # process child processes that have terminated
    while (($child_pid = waitpid(-1, &WNOHANG)) > 0) {    
	reaper($child_pid, $?);
    }
    # sleep for a second
    sleep(1); # DO NOT REMOVE THIS LINE
              # or else this daemon will burn all your CPU cycles!
};
