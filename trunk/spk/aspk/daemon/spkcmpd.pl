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

NOTE: Life-Cycle of the Working Directory Name

  1. A working directory must be created before each SPK compile is 
     forked so that the process will start up in its own unique space.
     There it will write its output.  To insure that the
     name of the working directory is unique, a suffix of the form
     "-job-jjjj" is appended to the name, where "jjjj" is the unique
     job_id that was assigned by the database management system when
     the job was created. 
  2. Once the working directory has been created, the daemon writes
     a file called "job_id" containing only the job_id number. This
     will be useful later on to determine what job this directory
     was created for.
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
     reading the contents of the file named "job_id", which it created
     shortly after it created the directory.

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
	  'get_cmp_jobs', 'get_dataset', 'email_for_job');
use Sys::Syslog('openlog', 'syslog', 'closelog');


my $database = shift;
my $host     = shift;
my $dbuser   = shift;
my $dbpasswd = shift;
my $mode     = shift;

my $bugzilla_production_only = 1;
my $bugzilla_url = "http://192.168.2.3:8081/";

my $service_root = "spkcmp";
my $bugzilla_product = "SPK";
my $submit_to_bugzilla = 1;
my $retain_working_dir = 0;
if ($mode =~ "test") {
    $submit_to_bugzilla = !$bugzilla_production_only;
    $service_root .= "test";
    $bugzilla_product = "SPKtest";
    $retain_working_dir = 1;
}

my $service_name = "$service_root" . "d";
my $prefix_working_dir = $service_root;

my $dbh;
my $database_open = 0;
my $filename_cerr_report = "compilation_error.xml";
my $filename_job_id = "job_id";
my $filename_serr = "software_error";
my $lockfile_path = "/tmp/lock_$service_name";
my $lockfile_exists = 0;
my $pathname_bugzilla_submit = "/usr/local/bin/bugzilla-submit";
my $pathname_co  = "/usr/bin/co";   # rcs checkout utility
my $pathname_compiler = "/usr/local/bin/spkcompiler";
my $pathname_tar = "/bin/tar";
my $spk_version = "0.1";
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
    #    it.  If there is no core dump, the working directory is
    #    removed when the child terminates.

    # Create a working directory
    my $unique_name = $prefix_working_dir . "-job-" . $job_id;
    my $working_dir = "$tmp_dir/$unique_name";
    if (-d $working_dir) {
	File::Path::rmtree($working_dir, 0, 0);
    }
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
    # the dataset_id from the job row to a file called data.xml,v
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
	  # This is the parent (fork returned a nonzero value)
	  syslog("info", "forked process with pid=$pid for job_id=$job_id");
      }
      elsif (defined $pid) {
	  # This is the child. (fork returned zero)
	  # NOTE: do not call death() in this block.
	  #
	  # Open the system log for the child, so that messages are properly
	  # identified as coming from the child.
	  # Note that $$ is the process-id of the child.
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
		  syslog("emerg", "couldn't rename working directory");
		  die;
	      };
	  # Redirect Standard Error to a file
	  open STDERR, ">$filename_serr";

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
	  # Error (fork returned an undefined value).
	  # If the Unix errno is EAGAIN, the fork may work next time if
	  # we retry it.
	  sleep(5);
	  redo FORK;
      }
      else {
	  # Unrecoverable error.  Something is seriously wrong.
	  death("emerg", "fork of compiler failed");
      }
  }
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

    my $child_pid = shift;
    my $value = shift;
    my $child_exit_value    = $value >> 8;
    my $child_signal_number = $value & 0x7f;
    my $child_dumped_core   = $value & 0x80;
    my $job_id;
    my $remove_working_dir = 0;

    # Get the job_id from the file we wrote to the working directory
    # of this process
    my $unique_name = $prefix_working_dir . "-pid-" . $child_pid;
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

    # Normal termination
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

	#remove the (now redundant) tar file from the working directory
	File::Path::rmtree("cpp_source.tar");

	$remove_working_dir = 1;
    }
    # Error termination
    else {
	my $err_msg = "";
	my $err_rpt = "";
	my $report = "";
	my $end_code = "unkown";

	if ($child_exit_value != 0) {
	    $end_code = "serr";
	    $err_msg .= "exit value = $child_exit_value; ";
	}
	if ($child_signal_number == SIGABRT) {
	    $end_code = "serr";
	    $err_msg .= "compiler bug asserted; ";
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

	# Did the compiler find errors in the user's source?
	if (-f $filename_cerr_report && -s $filename_cerr_report != 0) {
	    open(FH, $filename_cerr_report);
	    read(FH, $err_rpt, -s FH);
	    close(FH);
	    $err_msg .= "failed compilation do to source errors; ";
	    $end_code = "cerr";
	    $remove_working_dir = 1;
	}
	# Did the compiler die because of a software fault?
	elsif (-f $filename_serr){
	    open(FH, $filename_serr);
	    read(FH, $err_rpt, -s FH);
	    close(FH);
	    $end_code = "serr";
	    $err_msg .= "compiler bug caught as exception; ";
	} 
	# Get email address of user
	my $email = &email_for_job($dbh, $job_id);
	
	#format error report and place a message in the system log
	$report = format_error_report("$err_msg $err_rpt");
	&end_job($dbh, $job_id, $end_code, $report)
	    or death('emerg', "job_id=$job_id: $Spkdb::errstr");
	syslog('info', "job_id=$job_id $err_msg");

	# Submit compiler bugs to bugzilla
	if ($submit_to_bugzilla && ($end_code == "serr" || $end_code == "herr")) {
	    my $summary = $end_code == "serr" ? "soft" : "hard";
	    my @args = ($pathname_bugzilla_submit);
	    push @args, "--product",     $bugzilla_product;
	    push @args, "--version",     $spk_version;
	    push @args, "--component",   "ASPK";
	    push @args, "--priority",    "P4";
	    push @args, "--severity",    "critical";
	    push @args, "--summary",     "'job_id=$job_id, compiler $summary error'";
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
}
sub start {
    # Open the system log and record that we have started
    openlog("$service_name, pid=$$", 'cons', 'daemon');

    # Record our start-up in the system log
    syslog('info', "start of $service_name");

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

# Add directories of shared libraries to the load path
$ENV{LD_LIBRARY_PATH} = "/usr/lib:/usr/local/lib";
$ENV{HOME} = "/root";

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

my $child_pid;

syslog('info', "compiling jobs from the queue");

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
    # Process any child processes which have terminated
    while (($child_pid = waitpid(-1, &WNOHANG)) > 0) {    
	reaper($child_pid, $?);
    }
    # Sleep for a second
    sleep(1); # DO NOT REMOVE THIS LINE 
              # or else this daemon will burn all your CPU cycles!
};
