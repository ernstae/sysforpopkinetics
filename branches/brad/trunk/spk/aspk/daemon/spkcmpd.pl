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

spkcmpd.pl database dbhost dbuser dbpasswd shost sport

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
    $dbhost
        The host on which the SPK database resides
    $dbuser
        A database username which has read/write access to the 
        job table
    $dbpasswd
        The password associated with the username
    $mode
        The test mode indicator being "test" for test mode
    $shost
        The host on which the job-queue server resides
    $sport
        The port number of the job-queue server uses 

The first thing that spkcmp.pl does after starting up is to call
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

Next, it opens the database and connects to the job-queue server.

The program designates itself to be a process group leader. This
way it will be able to send signals to all of its descendents without
having to know their PIDs. 

The "stop" subroutine is designated to catch the TERM signal,
when it is received. As explained below, this will allow for
an orderly shutdown of the daemon and its sub-processes.  It closes 
the connection to the database and the connection to the job-queue 
server.

The last major step in the start-up sequence is to talk to the 
job-queue server to put any compiling jobs back to compiler queue 
that were interupted by the last termination of this daemon, and 
to put any aborting jobs back to aborting compiler queue that were 
interupted by the last termination of this daemon

At this point, the program enters an endless loop from which it will
escape only upon receipt of a signal. It queries the job-queue server
to discover whether or not a job has been added to the compiler
queue.  If so, a copy of spkcompiler is started as an independent
sub-process, working in its own directory on input provided by the
job. 

The program also queries the job-queue server to discover whether or not 
a job has been added to the aborting compiler queue.  If so, a 'TERM' 
signal is sent to the child process of the job to terminate the child 
process.  To avoid the "stop" subroutine, which is for the termination 
of the daemon, being called, the signal mask is temporarily set.

The daemon then checks to see if any child processes have 
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

If the signal number is SIGKILL, this indicates that the job is set to
abort.

NOTE: Life-Cycle of the Working Directory Name

  1. A working directory must be created before each SPK compile is 
     forked so that the process will start up in its own unique space.
     There it will write its output.  To insure that the
     name of the working directory is unique, a suffix of the form
     "-job-jjjj" is appended to the name, where "jjjj" is the unique
     job_id that was assigned by the database management system when
     the job was created. 
  2. When the process is forked into a parent process and a child
     process, the Linux or Unix operating system assigns a process
     identifier (pid) number to the child.  Process identifiers are
     recycled, but only after a long time or when the system is 
     rebooted.  The parent finds the working directories of its
     children by maintaining a table relating pid to job_id.
  3. When a child process dies, the parent receives its pid as the 
     value returned from the waitpid system call.  Using this, it
     easily constructs the name of the working directory. After 
     extracting results and placing them in the database, the 
     working directory is normally removed, unless the constant 
     $retain_working_directory has been initialized to be true, or
     in case the run died due to an internal error. 

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
use Spkdb('connect', 'disconnect', 'get_q2c_job', 'set_state_code', 'en_q2r',
          'end_job', 'get_dataset', 'email_for_job', 'job_status');
use Sys::Syslog('openlog', 'syslog', 'closelog');
use IO::Socket;

my $database = shift;
my $dbhost   = shift;
my $dbuser   = shift;
my $dbpasswd = shift;
my $mode     = shift;
my $shost    = shift;
my $sport    = shift;

my $bugzilla_production_only = 1;
my $bugzilla_url = "http://192.168.1.101:8081/";

my $service_root = "spkcmp";
my $bugzilla_product = "SPK";
my $submit_to_bugzilla = 1;
my $retain_working_dir = 0;

my $dbh;
my $database_open = 0;
my $sh;
my $server_open = 0;

my $filename_cerr_report = "compilation_error.xml";
my $filename_job_id = "job_id";
my %jobid_pid = ();
my $filename_serr = "software_error";
my $filename_results = "result.xml";

my $lockfile_exists = 0;
my $pathname_bugzilla_submit = "/usr/local/bin/bugzilla-submit";
my $pathname_co  = "/usr/bin/co";   # rcs checkout utility
my $pathname_compiler = "/usr/local/bin/spkprod/spkcompiler";
my $pathname_tar = "/bin/tar";
my $spk_version = "0.1";
my $tmp_dir = "/tmp";
my $spk_library_path = "/usr/local/lib/spkprod";
my $cpath = "/usr/local/include/spkprod";

if ($mode =~ "test") {
    $submit_to_bugzilla = !$bugzilla_production_only;
    $service_root .= "test";
    $bugzilla_product = "TestProduct";
    $retain_working_dir = 1;
    $pathname_compiler = "/usr/local/bin/spktest/spkcompiler";
    $spk_library_path = "/usr/local/lib/spktest";
    $cpath = "/usr/local/include/spktest";
}

my $service_name = "$service_root" . "d";
#my $prefix_working_dir = $service_root;
my $lockfile_path = "/tmp/lock_$service_name";

sub death {
    # Call this subroutine only from parent, never from child
    my $level = shift;
    my $msg = shift;

    # Log the reason for termination
    syslog($level, $msg);

    # send an e-mail indicating failure
    # added by:  andrew 05/19/2006
    if ( ! ($mode =~ "test") ) {
	my $mail_from = 'rfpk@spk.washington.edu';
	my $mail_subject = "spkcmpd shut down: $mode";
	my $mail_body = "$msg\n\n$level\n\n$mode";
	my $alert = 'jjdu@u.washington.edu,ernst@u.washington.edu';

	use MIME::Lite;
	$msg = MIME::Lite->new(
			       From     => $mail_from,
			       To	 => $alert,
			       Subject  => $mail_subject,
			       Data     => $mail_body
			       );
	$msg->send; # send via default
    }


    # Close the connection to the database
    if ($database_open) {
	&disconnect($dbh)
    }

    # Close the connection to the job-queue server
    if ($server_open) {
	close($sh);
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
    my $job_id = shift;

    my $drow;
    my $dataset_id = $jrow->{"dataset_id"};
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
#    my $unique_name = $prefix_working_dir . "-job-" . $job_id;
#    my $working_dir = "$tmp_dir/$unique_name";
    my $working_dir = "/usr/local/spk/share/working/spk$mode/spkjob-$job_id";
    if (-d $working_dir) {
	File::Path::rmtree($working_dir, 0, 0);
    }
    mkdir($working_dir, 0777) 
	or death("emerg", "couldn't create working directory: $working_dir");

    # Write the job_id to a file name "job_id"
    chdir $working_dir;
#    open(FH, ">job_id")
#	or death('emerg', "could not create the job_id file in $working_dir");
#    print FH "$job_id";
#    close(FH);

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

    # Change the dataset file permission
    chmod 0666, 'data.xml';

    # Fork into parent and child
  FORK: {
      if ($pid = fork) {
	  # This is the parent (fork returned a nonzero value)
	  syslog("info", "forked process with pid=$pid for job_id=$job_id");

          # Add the child process to jobid_pid hash
          $jobid_pid{$job_id} = $pid;
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
#	  my $old_working_dir = $working_dir;
#	  $working_dir = "$tmp_dir/$prefix_working_dir" . "-pid-" . $$;
#	  if (-d $working_dir) {
#	      File::Path::rmtree($working_dir, 0, 0);
#	  }
#	  rename $old_working_dir, $working_dir
#	      or do {
#		  syslog("emerg", "couldn't rename working directory");
#		  die;
#	      };
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
#    my $unique_name = "$prefix_working_dir" . "-job-" . $job_id;
#    my $working_dir = "$tmp_dir/$unique_name";
    my $working_dir = "/usr/local/spk/share/working/spk$mode/spkjob-$job_id";
    chdir $working_dir;

    # Normal termination
    if ($child_exit_value == 0 && $child_signal_number == 0 && !$child_dumped_core) {
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
        print $sh "add-q2r-$job_id\n";
        my $answer = <$sh>;
        if (defined $answer) {
            chop($answer);
            if ($answer eq "done") {
                syslog('info', "job_id=$job_id compiled and has moved to run queue");
            }
            else {
                death('emerg', "job_id=$job_id: could not add q2r to job-queue");
            }
	}
        else {
            death("emerg", "job_id=$job_id: could not add q2r to job-queue");
        }

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

        $submit_to_bugzilla = 1;
        if ($mode =~ "test") {
           $submit_to_bugzilla = !$bugzilla_production_only;
        }
	if ($child_exit_value != 0) {
	    $end_code = "cerr";
	    $err_msg .= "exit value = $child_exit_value; ";
            $submit_to_bugzilla &= 0;
	}
	if ($child_signal_number == SIGABRT) {
	    $end_code = "serr";
	    $err_msg .= "compiler bug asserted; ";
            $submit_to_bugzilla &= 1;
	}
        elsif ($child_signal_number == SIGTERM) {
	    $end_code = "serr";
	    $err_msg .= "killed by operator; ";
            $submit_to_bugzilla &= 0;
        }
	elsif ($child_signal_number == SIGKILL) {
	    $end_code = "abrt";
	    $err_msg .= "aborted by user; ";
            $submit_to_bugzilla &= 0;
	}
	elsif ($child_signal_number == SIGSEGV) {
	    $end_code = "herr";
	    $err_msg .= "segmentation fault; ";
            $submit_to_bugzilla &= 1;
	}
	elsif($child_signal_number != 0) {
	    $end_code = "herr";
	    $err_msg .= "killed with signal $child_signal_number; ";
            $submit_to_bugzilla &= 1;
	}

	# Did the compiler find errors in the user's source?
	if (-f $filename_cerr_report && -s $filename_cerr_report != 0) {
	    open(FH, $filename_cerr_report)
               or death('emerg', "can't open $working_dir/$filename_cerr_report");
	    read(FH, $report, -s FH);
	    close(FH);
	    $err_msg .= "compiler bug caught as exception; ";
	    $end_code = "cerr";
	    $remove_working_dir = 0;
            $submit_to_bugzilla &= 0;
            #$report = insert_error($err_msg, "", $report);
	}
	# Did the compiler die because of a software fault?
	elsif (-f $filename_serr && -s $filename_serr > 0){
	    open(FH, $filename_serr);
	    read(FH, $err_rpt, -s FH);
	    close(FH);
	    $end_code = "serr";
	    $err_msg .= "software bug caught as assertion; ";
            $remove_working_dir = 0;
            $submit_to_bugzilla &= 1;
            $report = format_error_report( "$err_msg $err_rpt" );     
	} 
        # Core dump?
        if( $child_dumped_core ){
           $end_code = "serr";
           $err_msg .= "core dump in $working_dir; ";
           $remove_working_dir = 0;
           $submit_to_bugzilla &= 1;
           $report = format_error_report( "$err_msg $err_rpt" );     
        }
        # Remove the empty STDERR captured file
        else {
           unlink($filename_serr);
        }
       
        if ($end_code ne "abrt") {
            open(FH, ">result.xml")
                or death( 'emerg', "can't open $working_dir/result.xml");
            print FH $report;
            close(FH);
        }

	# Get email address of user
	my $email;
        if ($end_code ne "abrt") {
            $email = &email_for_job($dbh, $job_id);	
        }

        $report = compress($report);

        # End the job
        print $sh "set-end-$job_id\n";
        my $answer = <$sh>;
        chop($answer);
        if (defined $answer && $answer eq "done") {
            &end_job($dbh, $job_id, $end_code, $report)
               or death('emerg', "job_id=$job_id: $Spkdb::errstr");
        }
        else {
            death('emerg', "error ending job in job-queue: job_id=$job_id");
        }
	syslog('info', "job_id=$job_id $err_msg ($end_code)");

	# Submit compiler bugs to bugzilla
	if ($submit_to_bugzilla) {
            #only serr and herr should be caught here.
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
            else {
                syslog('info', "submited a bugzilla report for job $job_id");
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
    $dbh = &connect($database, $dbhost, $dbuser, $dbpasswd)
	or death("emerg", "can't connect to database=$database, host=$dbhost");
    syslog("info", "connected to database=$database, host=$dbhost");
    $database_open = 1;

    # Open a connection to the job-queue server
    $sh = IO::Socket::INET->new(Proto => "tcp", PeerAddr => $shost, PeerPort => $sport)
        or death("emerg", "can't connect to port $sport on $shost: $!");
    $sh->autoflush(1);
    $server_open = 1;
}
sub stop {
    # We have received the TERM signal. 
    
    # Become insensitive to the TERM signal we are about to send
    local $SIG{'TERM'} = 'IGNORE';

    # Remove reaper as catcher of SIGCHLD
    $SIG{'CHLD'} = 'DEFAULT';

    # Remove the lockfile
    if ($lockfile_exists) {
	unlink($lockfile_path);
	$lockfile_exists = 0;
    }

    # Close the connection to the database
    if ($database_open) {
	&disconnect($dbh)
    }

    # Close the connection to the job-queue server
    if ($server_open) {
	close($sh);
    }

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
# Compress the report
sub compress {
    my $report = shift;
    open(FH, ">result.xml");
    print FH $report;
    close(FH);
    my @args = ($pathname_tar, 'czf', "result.tar.gz", $filename_results);
    system(@args);
    my $exit_status = $? >> 8;
    if ($exit_status != 0) {
    death('emerg', "tar failed creating result.tar.gz file."
	  . " exit_status=$exit_status");
    }
    # Read the tar file into memory
    open(FH, "result.tar.gz")
        or death('emerg', "failed to open result.tar.gz");
    read(FH, $report, -s FH)
        or death('emerg', "failed to read result.tar.gz");
    close(FH);
    return $report;
}
# Abort a non-compiling job, which was queued to abort cmp or aborting cmp
# state left by the last termination of this daemon).
sub abort_job {
    my $jobid = shift;

    # Form the working directory path of the job
#    my $unique_name = "$prefix_working_dir" . "-job-" . $jobid;
#    my $working_dir = "$tmp_dir/$unique_name";
    my $working_dir = "/usr/local/spk/share/working/spk$mode/spkjob-$jobid";
    if ( -e $working_dir ) {
        # Remove working directory if it is not needed
        if (!$retain_working_dir) {
            File::Path::rmtree($working_dir);
        }
    }

    # End the job
    print $sh "set-end-$jobid\n";
    my $answer = <$sh>;
    chop($answer);
    if (defined $answer && $answer eq "done") {
        &end_job($dbh, $jobid, "abrt", undef, undef)
            or death('emerg', "job_id=$jobid: $Spkdb::errstr");
    }
    else {
        death('emerg', "error ending job in job-queue: job_id=$jobid");
    }
}
my $row;
my $row_array;

# Become a daemon
Proc::Daemon::Init();

# Initialize
start();

# Add directories of shared libraries to the load path
$ENV{LD_LIBRARY_PATH} = "/usr/lib:/usr/local/lib:" . $spk_library_path;
$ENV{CPATH} = $cpath;
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
#$row_array = &get_cmp_jobs($dbh);
#syslog('info', "looking for interrupted compiler jobs");
#if (defined $row_array) {
#    foreach $row (@$row_array) {
#	&fork_compiler($row);
#    }
#}
#else {
#    death("emerg", "error reading database: $Spkdb::errstr");
#}

# Initialize this daemon with the job-queue server
print $sh "init-cmpd\n";
my $answer = <$sh>;
chop($answer);
unless(defined $answer && $answer eq "done") {
    death("emerg", "error reading job-queue to initialize");
}

# Loop until interrupted by a signal
use POSIX ":sys_wait_h";

my $child_pid;
my $jobid;
my $time = 0;
syslog('info', "compiling jobs from the queue");

while(1) {
    # If there is a job queued-to-compile, fork the compiler
    print $sh "get-q2c\n";
    $jobid = <$sh>;
    if (defined $jobid) {
        chop($jobid);
        if ($jobid ne "none") {
            $row = &get_q2c_job($dbh, $jobid);
            if (defined $row && $row) {
	        &fork_compiler($row, $jobid)
            }
            else {
	        death("emerg", "error reading database: $Spkdb::errstr");
            }
        }
    }
    else {
        death("emerg", "error reading job-queue to get q2c job");
    }

    # If there is a job queued-to-abort-compile, kill the process
    print $sh "get-q2ac\n";
    $jobid = <$sh>;
    if (defined $jobid) {
        chop($jobid);
        if ($jobid ne "none") {
            if (&set_state_code($dbh, $jobid, "acmp") == 1) {
                if (exists $jobid_pid{$jobid}) {
                    my $cpid = $jobid_pid{$jobid};
                    kill('KILL', $cpid);
                }
                else {
                    abort_job($jobid);
                }
            }
            else {
	        death("emerg", "error reading database: $Spkdb::errstr");
            }
        }
    }
    else {
        death("emerg", "error reading job-queue to get q2ac job");
    }

    # Process any child processes which have terminated
    while (($child_pid = waitpid(-1, &WNOHANG)) > 0) {   
	reaper($child_pid, $?);

        # Get the job_id from the child pid
        my %pid_jobid = reverse %jobid_pid;
        $jobid = $pid_jobid{$child_pid};
        delete($jobid_pid{$jobid});
    }
    # Sleep for a second
    sleep(1); # DO NOT REMOVE THIS LINE 
              # or else this daemon will burn all your CPU cycles!
    $time++;
    if ($time == 3600) {
        &job_status($dbh, 1);
        $time = 0;
    }
};
