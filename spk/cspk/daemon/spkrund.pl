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
selects jobs from the run queue, forks processes to compile, link
and run them and, finally, inserts results into the database.

=head1 DESCRIPTION

=head2 ARGUMENTS

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

=head2 OPERATION

The first think that spkcmp.pl does after starting up is to call
Proc::Daemon::Init to make it into a daemon, by shedding its
inheirited environment and becoming a direct child of the system init
process.

It then opens the system log so that it has a place to record progress
and error messages.

It attempts to create a lock-file if one does not already exist. The
method that is used assures that the process of query and creation is
atomic.  If the lock-file already exists, the program writes an error
message to the system log and terminates, because only one copy of
spkrund.pl can be allowed to run at any given time.

Next, it opens the database and attach a sensor for monitoring. The 
sensor is a server written in java.

The program designates itself to be a process group leader. This way
it will be able to send signals to all of its descendents without
having to know their PIDs.

The "stop" subroutine is designated to catch the TERM signal, when it
is received. As explained below, this will allow for an orderly
shutdown of the daemon and its sub-processes.

The next step is to select from the database all jobs with a state 
code field set to 'q2ar'.  These jobs, if any exist, had been in the 
abort-run queue when the daemon last shut down. All such jobs are 
set to be aborted by setting the state code field to 'end' and the 
end code field to 'abrt'.

The next step is to select from the database all jobs with a state 
code field set to 'arun'.  These jobs, if any exist, had been in the 
process of aborting run when the daemon last shut down. All such jobs 
are set to be aborted by setting the state code field to 'end' and the 
end code field to 'abrt'.  If checkpoint file exists in any of these 
jobs' working directory, its text content is copied to the database.

The last major step in the start-up sequence is to select from the
database all jobs with a state_code field set to 'run'.  These jobs,
if any exist, had been in the process of running when the daemon last
shut down.  All such jobs are rerun.

At this point, the program enters an endless loop from which it will
escape only upon receipt of a signal. It queries the database to
discover whether or not a job has been added to the run queue.  If so,
a copy of the job's driver is started as an independent sub-process,
working in its own directory.  

The program also queries the database to discover whether or not a
job has been added to the abort-run queue.  If so, a 'TERM' signal is
sent to the child process of the job to terminate the child process. 
To avoid the "stop" subroutine, which is for the termination of the 
daemon, being called, the signal mask is temporarily set.

The daemon then checks to see if any child processes have terminated.
If so, it moves them to "end" status and stores the results in the 
database. The daemon sleeps a second, before continuing its loop.

The normal way in which spkcmp.pl is terminated is by using the Unix
"kill" command to send it the TERM signal.  When TERM is received,
execution is passed to the 'stop' subroutine, which had previously
been designated to catch this signal.

To avoid a loop, 'stop' sets the signal mask to ignore any subsequent
TERM signals. It then sends the TERM signal to every process in its
process group, which consists of the daemon itself and all of its
descendents. It waits for all descendents (which are job drivers) to
terminate.  It closes the database and the system log, then dies.

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
     work with the job. The source code and data needed for the run
     are placed there.
  3. When the process is forked into a parent process and a child
     process, the Linux or Unix operating system assigns a process
     identifier (pid) number to the child.  Process identifiers are
     recycled, but only after a long time or when the system is 
     rebooted.  The parent finds the working directories of its
     children by maintaining a table relating pid to job_id. 
  4. When a child process dies, the parent receives its pid as the 
     value returned from the waitpid system call.  Using this, it
     easily constructs the name of the working directory. After 
     extracting results and placing them in the database, the 
     working directory is normally removed, unless the constant 
     $retain_working_directory has been initialized to be true, or
     in case the run died due to an internal error. So that the
     retained working directory can be easily identified by software
     maintainers.

=head2 END_CODE AND ERROR REPORTING

Because the deamon is the parent of all jobs, it is informed as soon
as a job driver terminates.  The operating system provides the daemon
with three very useful numbers describing the deceased driver: the
process identifier (pid), the exit status and, in the case where the
driver is terminated with a kill signal, the signal number.

The end-code will be set to "srun", which indicates a successful run,
only if the exit status and the signal number are both zero and if, in
addition, no file called "software error" is found in the job's
working directory.

If the signal number is SIGTERM, which indicates that the job was
terminated by an operator, a software error of some sort is assumed
and end_code is set to "serr".  It may be that the only error is that
the optimization failed to converge in a reasonable period of time.
Only subsequent analysis will determine what the problem was.

If the signal number is SIGABRT, this indicates that the software
itself discovered some internal inconsistency and terminated by
raising an "assertion".  In this case, end_code is also set to "serr".

If the job terminated with any signal other than SIGTERM or SIGABRT, a
hardware error is assumed, and the end_code is set to "herr". Of
course, many hardware errors are really caused by software, such as
when the software attempts to divide a number by zero or when it
attempts a reference via an invalid pointer.

If the signal number is zero, indicating that the job did not
terminate as the result of a signal, but the exit status is not zero,
end_code is set to "serr".

If the daemon finds a file called "software error", end_code is always
set to "serr" and it is assumed that an assertion was raised.  In this
case, the SIGABRT signal was probably also reported.

The daemon analyzes the various cases described above, and generates
an error messages which is written to the report which goes into the
database, to be made available to the end user, as well as to the
system log, where it is available to developers and system
adminstrators.

If the end_code is not "abrt", the daemon sends an end-job email notice 
to the user on the user's request.

=head1 RETURNS

Nothing, because it has no parent (other than init) to which an exit
code might be returned.  The program does, however, write event
messages to the system log.

=cut

use strict;
use bytes;

use Fcntl qw(:DEFAULT :flock);
use File::Path;
use POSIX qw(:signal_h);
use Proc::Daemon;
use Spkdb('connect', 'disconnect', 'de_q2r', 'en_q2r', 'get_run_jobs', 'end_job',
	  'job_history', 'email_for_job', 'de_q2ar', 'get_job_ids', 'get_mail_notice');
use Sys::Syslog('openlog', 'syslog', 'closelog');
use IO::Socket::INET;
use Sys::Hostname;

my $database = shift;
my $host     = shift;
my $dbuser   = shift;
my $dbpasswd = shift;
my $mode     = shift;

my $mailserver = "smtp.washington.edu:25";
my $hostname = hostname();
my $from = "rfpksoft\@u.washington.edu";

my $bugzilla_production_only = 1;
my $bugzilla_url = "http://192.168.2.2:8081/";

my $max_concurrent = 2;
my $concurrent = 0;

my $service_root = "spkrun";
my $bugzilla_product = "SPK";
my $submit_to_bugzilla = 1;
my $retain_working_dir = 0;
my $attach_sensor = 1;

my $dbh;
my $build_failure_exit_value = 101;
my $database_open = 0;

my $filename_checkpoint = "checkpoint.xml";
my $filename_data = "data.xml";
my $filename_driver = "driver";
my $filename_job_id = "job_id";
my %jobid_pid = ();
my $filename_makefile = "Makefile.SPK";
my $filename_optimizer_trace = "optimizer_trace.txt";
my $filename_results = "result.xml";
my $filename_serr = "software_error";
my $filename_source = "source.xml";

my $spk_library_path = "/usr/local/lib/spkprod";
my $cpath = "/usr/local/include/spkprod";
my $sensor_classpath = "-cp /usr/local/bin/spkmonitor:.";
my $sensor_port = "9001";
my $sensor_pid;

if ($mode =~ "test") {
    $submit_to_bugzilla = !$bugzilla_production_only;
    $service_root .= "test";
    $bugzilla_product = "TestProduct";
    $retain_working_dir = 1;
    $spk_library_path = "/usr/local/lib/spktest";
    $cpath = "/usr/local/include/spktest";
    $attach_sensor = 0;
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

    # stop the sensor
    if(defined $sensor_pid) {
        $SIG{'TERM'} = 'IGNORE';
        kill('TERM', $sensor_pid);
    }

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
sub fork_driver {
    use Errno qw(EAGAIN);

    my $jrow = shift;
    my $job_id = $jrow->{'job_id'};
    my $cpp_source = $jrow->{'cpp_source'};
    my $checkpoint = $jrow->{'checkpoint'};
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

    # Create checkpoint.xml if checkpoint field in job table is not NULL and checkpoint.xml
    # does not already exist

    if (defined $checkpoint && length $checkpoint && ! -e $filename_checkpoint) {
	open(FH, ">$filename_checkpoint")
	    or death('emerg', "could not create $filename_checkpoint file in $working_dir");
	print FH $checkpoint;
	close(FH);
	if (-s $filename_checkpoint != length $checkpoint) {
	    death('emerg', "write of $filename_checkpoint failed");
	}
    }
    # Fork the process into parent and child

  FORK: {
      if ($pid = fork) {
	  # This is the parent (fork returned a nonzero value)
          syslog("info", "forked process with pid=$pid for job_id=$job_id");
	  $concurrent++;

          # Add the child process to jobid_pid hash
          $jobid_pid{$job_id} = $pid;
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
#	  my $old_working_dir = $working_dir;
#	  $working_dir = "$tmp_dir/$prefix_working_dir" . "-pid-" . $$;
#	  if (-d $working_dir) {
#	      File::Path::rmtree($working_dir, 0, 0);
#	  }
#	  rename $old_working_dir, $working_dir
#	      or do {
#		  syslog('emerg', "can't rename working directory");
#		  die;
#	      };
	  # Compile and link the runner

          if ($mode =~ "test"){
	     @args = ($pathname_make, "-f", $filename_makefile, "test");
          }
          else{
             @args = ($pathname_make, "-f", $filename_makefile);
          }
	  unless (system(@args) == 0)  {
	      $! = 101;
	      die;
	  }
	  # Redirect Standard output to a file
	  open STDOUT, ">$filename_optimizer_trace";

	  # Redirect Standard Error to a file
	  open STDERR, ">$filename_serr";
         
	  # execute the job driver
	  @args = ("./$filename_driver", $filename_source, $filename_data);
	  my $e = exec(@args);

	  # this statement will never be reached, unless the exec failed
	  if (!$e) {
	      syslog("emerg", "couldn't exec $filename_driver for $job_id");
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
sub insert_error {
    my $daemon_text = shift;
    my $driver_text = shift;
    my $report      = shift;

    my $daemon_text_xml  = '<error>';
    $daemon_text_xml    .= "\n<description>Exit status from the driver</description>\n";
    $daemon_text_xml    .= "<file_name>N/A</file_name>\n";
    $daemon_text_xml    .= "<line_number>N/A</line_number>\n";
    $daemon_text_xml    .= "<message>";
    if ( $daemon_text ){
       $daemon_text_xml .= $daemon_text;
    }
    else{
       $daemon_text_xml .= "N/A";
    }
    $daemon_text_xml    .= "</message>\n";
    $daemon_text_xml    .= "</error>\n";
 
    my $driver_text_xml  = "";
    if( $driver_text ){
       $driver_text_xml .= '<error>';
       $driver_text_xml .= "\n<description>Assertion text from driver</description>\n";
       $driver_text_xml .= "<file_name>N/A</file_name>\n";
       $driver_text_xml .= "<line_number>N/A</line_number>\n";
       $driver_text_xml .= "<message>$driver_text</message>\n";
       $driver_text_xml .= "</error>\n";
    }

    $report =~ s/<\/error_list>/$daemon_text_xml $driver_text_xml<\/error_list>\n/;
    return $report;
}
sub format_error_report {
    my $content = shift;
    my $report = '<?xml version="1.0"?>';
    $report   .= "\n<spkreport>\n";
    $report   .= "<error_list>\n";
    $report   .= "</error_list>\n";
    $report   .= "</spkreport>\n";

    $report = insert_error($content, "", $report);
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
    my $checkpoint;
    my $end_code;
    my $email;

    $concurrent--;

    # Get the job_id from the file spkcmpd.pl wrote to the working
    # directory of this process
#    my $unique_name = "$prefix_working_dir" . "-pid-" . $child_pid;
#    my $working_dir = "$tmp_dir/$unique_name";
#    chdir $working_dir;
#    open(FH, $filename_job_id)
#	or death('emerg', "can't open $working_dir/job_id");
#    read(FH, $job_id, -s FH);
#    close(FH);

    # Rename working directory to make evidence easier to find
#    my $old_working_dir = $working_dir;
#    $unique_name = $prefix_working_dir . "-job-" . $job_id;
#    $working_dir = "$tmp_dir/$unique_name";
#    if (-d $working_dir) {
#	File::Path::rmtree($working_dir, 0, 0);
#    }
#    rename $old_working_dir, $working_dir
#	or death('emerg', "couldn't rename working directory");

    # Get the job_id from the child pid
    my %pid_jobid = reverse %jobid_pid;
    $job_id = $pid_jobid{$child_pid};

    # Change to working directory
    my $unique_name = "$prefix_working_dir" . "-job-" . $job_id;
    my $working_dir = "$tmp_dir/$unique_name";
    chdir $working_dir;

    # Get optimizer trace 
    if (-f $filename_optimizer_trace) {
	open(FH, $filename_optimizer_trace)
	    or death('emerg', "can't open $working_dir/$filename_optimizer_trace");
	read(FH, $optimizer_trace, -s FH);
	close(FH);
    }

    # Assume success, then look for errors
    $end_code = "srun";  
    my $err_msg = "";
    my $err_rpt = "";
    if ($child_exit_value == $build_failure_exit_value) {
	$end_code = "serr";
	$err_msg .= "c++ program generated by SPK failed to build; ";
    }
    elsif($child_exit_value == 1) {
        $end_code = "serr";
        $err_msg .= "optimization failed; ";
    }
    elsif($child_exit_value == 2) {
        $end_code = "serr";
        $err_msg .= "file/directory access error; ";
    }
    elsif($child_exit_value == 3) {
        $end_code = "serr";
        $err_msg .= "post-integration method failed; ";
    }
    elsif($child_exit_value == 4) {
        $end_code = "serr";
        $err_msg .= "statistics failed; ";
    }
    elsif($child_exit_value == 5) {
        $end_code = "serr";
        $err_msg .= "data simulation failed; ";
    }
    elsif($child_exit_value >  5) {
        $end_code = "serr";
        $err_msg .= "exit value = $child_exit_value; ";
    }

    if ($child_signal_number == SIGABRT) {
	$end_code = "serr";
	$err_msg .= "software bug asserted; ";
    }
    elsif ($child_signal_number == SIGTERM) {
	$end_code = "abrt";
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
	$err_msg .= "software bug caught as assertion; ";
    }
    # Remove the empty STDERR captured file
    else {
        unlink($filename_serr);
    }
    if ($child_dumped_core) {
	$err_msg .= "core dump in $working_dir; ";
    }
    if( -f $filename_checkpoint && -s $filename_checkpoint > 0 ){
	# Read the checkpoint file into the checkpoint variable
	open(FH, $filename_checkpoint)
	    or death('emerg', "can't open $working_dir/$filename_checkpoint");
	read(FH, $checkpoint, -s FH);
	close(FH);
    }
    if ($end_code =~ "srun") {
	# Read the results file into the report variable
	open(FH, $filename_results)
	    or death('emerg', "can't open $working_dir/$filename_results");
	read(FH, $report, -s FH);
	close(FH);

	# Place a message in the system log
	syslog('info', "job_id=$job_id terminated normally");

	# We will remove the working directory
	$remove_working_dir = 1;
    } 
    else {
	# Get email address of user
	$email = &email_for_job($dbh, $job_id);

        if( -f $filename_results && -s $filename_results > 0 ){
           # Read the results file, if it exists, into the report variable.
           open(FH, $filename_results)
              or death( 'emerg', "can't open $working_dir/$filename_results");
	   read(FH, $report, -s FH );
           close(FH);
           # Insert the return value and its description in the results file.
	   $report = insert_error($err_msg, $err_rpt, $report);
        }
        else{
           # Format error report.
           $report = format_error_report("$err_msg $err_rpt");
        }

	# Place a message in system log
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
    # Replace/write results in report file
    open(FH, ">$filename_results")
       or death( 'emerg', "can't open $working_dir/$filename_results");
    print FH $report;
    close(FH);

    # Remove working directory if not needed
    if ($remove_working_dir && !$retain_working_dir) {
	File::Path::rmtree($working_dir);
    }
    if (length($optimizer_trace) > 0) {
	$report = insert_optimizer_trace($optimizer_trace, $report);
    }
    &end_job($dbh, $job_id, $end_code, $report, $checkpoint)
	or death('emerg', "job_id=$job_id: $Spkdb::errstr");

    # Send end-job email notice to the user if it is requested
    if ($end_code ne "abrt") {
        my $mail_notice = &get_mail_notice($dbh, $job_id);
        if (defined $mail_notice && $mail_notice == 1) {
            if(!defined $email) {
                $email = &email_for_job($dbh, $job_id);
            }
            sendmail($job_id, $email, $end_code);
            syslog('info', "end-job email notice sent for job_id=$job_id from spkrund");
        }
    }
}
sub sendmail {
    my $job_id = shift;
    my $to = shift;
    my $end_code = shift;
    my $status;
    if ($end_code =~ "srun") {
        $status = "Your job has run to completion.";
    }
    else {
        $status = "Your job has not run to completion due to an error.";
    }
    my $subject = "SPK job finished - Job ID: $job_id";
    my $message = "This message was sent by the SPK service provider.\n$status";
    my $socket = IO::Socket::INET->new($mailserver);
    print $socket "HELO $hostname\r\n";
    print $socket "MAIL FROM: <$from>\r\n";
    print $socket "RCPT TO: <$to>\r\n";
    print $socket "DATA\r\n";
    print $socket "To: $to\nSubject: $subject\n$message\r\n";
    print $socket ".\r\n";
    close($socket);
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

    # Attach a sensor
    if($attach_sensor == 1)
    {
        FORK: {
            if ($sensor_pid = fork) {
            }
            elsif (defined $sensor_pid) {
	        my @args = ("java $sensor_classpath uw.rfpk.monitor.Sensor $sensor_port");
	        my $e = exec(@args);

	        # This statement will never be reached, unless the exec failed
	        if (!$e) {
	            syslog("emerg", "couldn't attach a sensor");
	            die;
	        };
            }
            elsif ($! == EAGAIN) {
                sleep 5;
                redo FORK;
            }
            else { 
                die "can't fork: $! to attach a sensor\n";
            }
        }
    }
}
sub stop {
    # We have received the TERM signal. 
    
    # Become insensitive to TERM signal we are about to broadcast
    local $SIG{'TERM'} = 'IGNORE';

    # Remove the lockfile. We may not get a chance to wait for all 
    # the children to terminate.
    if ($lockfile_exists) {
	unlink($lockfile_path);
	$lockfile_exists = 0;
    } 

    # send the TERM signal to every member of our process group
    kill('TERM', -$$);
    kill('TERM', $sensor_pid);

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

# Add directories of shared libraries to the load path
$ENV{LD_LIBRARY_PATH} = "/usr/lib:/usr/local/lib:" . $spk_library_path;
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

# Abort any jobs of queued to abort run or aborting run
# state, which was left by the last termination of this daemon.
my @jobs = &get_job_ids($dbh, "q2ar");
my $jobid;
if(!(@jobs == 1 && not defined $jobs[0])) {
    foreach $jobid (@jobs) {
        &end_job($dbh, $jobid, "abrt", undef, undef)
            or death('emerg', "job_id=$jobid: $Spkdb::errstr");
    }
}
else {
    death("emerg", "error reading database: $Spkdb::errstr");
}
@jobs = &get_job_ids($dbh, "arun");
if(!(@jobs == 1 && not defined $jobs[0])) {
    my $checkpoint;
    foreach $jobid (@jobs) {
        # Change to the working directory of the job
        my $unique_name = "$prefix_working_dir" . "-job-" . $jobid;
        my $working_dir = "$tmp_dir/$unique_name";
        chdir $working_dir;

        # Read checkpoint file to $checkpoint variable
        if( -f $filename_checkpoint && -s $filename_checkpoint > 0 ){
	    # Read the checkpoint file into the checkpoint variable
	    open(FH, $filename_checkpoint)
	        or death('emerg', "can't open $working_dir/$filename_checkpoint");
	    read(FH, $checkpoint, -s FH);
	    close(FH);
        }

        # Remove working directory if it is not needed
        if (!$retain_working_dir) {
            File::Path::rmtree($working_dir);
        }

        &end_job($dbh, $jobid, "abrt", undef, $checkpoint)
            or death('emerg', "job_id=$jobid: $Spkdb::errstr");
    }
}
else {
    death("emerg", "error reading database: $Spkdb::errstr");
}

# rerun any jobs that were interrupted when we last terminated
my $job_array = &get_run_jobs($dbh);
syslog('info', "looking for interrupted computational runs");
if (defined $job_array) {
    foreach my $job_row (@$job_array) {
	my $history_array = &job_history($dbh, $job_row->{'job_id'});
	my $history_row = $history_array->[@$history_array - 1];
	print "host = $history_row->{'host'}\n";
	if ($history_row->{'host'} eq hostname) {
	    &fork_driver($job_row);
	}
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
    # if there is a job queued-to-run, fork the driver
    if ($concurrent < $max_concurrent) {
	my $job_row = &de_q2r($dbh);
	if (defined $job_row) {
	    if ($job_row) {
		&fork_driver($job_row);
	    }
	}
	else {
	    death("emerg", "error reading database: $Spkdb::errstr");
	}
    }

    # If there is a job queued-to-abort-run, kill the process
    my $jobid = &de_q2ar($dbh);
    if (defined $jobid) {
	if ($jobid) {
            my $cpid = $jobid_pid{jobid};
            $SIG{'TERM'} = 'IGNORE';
	    kill('TERM', $cpid);
            $SIG{'TERM'} = \&stop;
        }
    }
    else {
	death("emerg", "error reading database: $Spkdb::errstr");
    }

    # process child processes that have terminated
    while (($child_pid = waitpid(-1, &WNOHANG)) > 0) { 
	reaper($child_pid, $?);
        delete($jobid_pid{'jobid'});
    }
    # sleep for a second
    sleep(1); # DO NOT REMOVE THIS LINE
              # or else this daemon will burn all your CPU cycles!
};
