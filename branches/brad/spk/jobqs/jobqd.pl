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

jobqd.pl -- the Job-queue Daemon

=head1 SYNOPSIS

jobqd.pl database dbhost dbuser dbpasswd shost sport

=head1 ABSTRACT

The Job-queue Daemon runs continuously on the Job-queue server. It
first makes it into a deamon and creates a log file.  Then it starts 
the Job-queue Server which written in java.

=head1 DESCRIPTION

The program expects the following arguments:

    $dbhost
        The host on which the SPK database resides
    $database
        The name of the SPK database
    $dbuser
        A database username which has read/write access to the 
        job table
    $dbpasswd
        The password associated with the username
    $mode
        The test mode indicator being "test" for test mode and "prod" for production mode
    $startjob
        The starting job_id that the job-queue server uses to initilize itself with the 
        database

The first thing that jobq.pl does after starting up is to call
Proc::Daemon::Init to make it into a daemon, by shedding its 
inheirited environment and becoming a direct child of the system
init process.

It then opens the system log so that it has a place to record progress
and error messages.

It attempts to create a lock-file if one does not already exist. The
method that is used assures that the process of query and creation
is atomic.  If the lock-file already exists, the program writes an
error message to the system log and terminates, because only one 
copy of jobqd.pl can be allowed run at any given time.

Next, it starts the job-queue server using the daemon process.

=head1 RETURNS

Nothing, because it has no parent (other than init) to which an exit
code might be returned.  The program does, however, write event messages
to the system log.

=cut

use Sys::Syslog('openlog', 'syslog', 'closelog');
use Proc::Daemon;
use File::Path;
use Fcntl qw(:DEFAULT :flock);

my $dbhost = shift;
my $dbname = shift;
my $dbuser = shift;
my $dbpasswd = shift;
my $mode = shift;

my @a = ("ps -elf|grep jobqs");
my $stdout = `@a`;

if(index($stdout, "spk$mode") != -1) {
    print "A jobqserver is alreading running. Another instance cannot be ";
    die;
}

my $command = "/usr/java/jdk1.5.0_07/bin/java";
my $classpath = "-cp /usr/local/lib/mysql-connector-java-3.0.10-stable-bin.jar:/usr/local/bin/spk$mode/:.";
my $lockfile_exists = 0;
my $jobqs_port = 9000;
my $service_root = "jobq";
if ($mode eq "test") {
   $service_root .= "test";
   $jobqs_port = 9001;
}  
my $service_name = "$service_root" . "d";
my $lockfile_path = "/tmp/lock_$service_name";

Proc::Daemon::Init();    
start();
my @args = ("$command $classpath uw.rfpk.jobqs.JobQueue $dbhost $dbname $dbuser $dbpasswd $jobqs_port $mode");
my $e = exec(@args);

# This statement will never be reached, unless the exec failed
if (!$e) {
    stop("emerg", "could not start JobQueue server"); 
}
sub start {
    # Open the system log and record that we have started
    openlog("$service_name, pid=$$", 'cons', 'daemon');

    # Record our start-up in the system log
    syslog('info', "start of $service_name");

    # Create a lockfile and store our pid, so only one copy can run
    sysopen(FH, $lockfile_path, O_RDWR | O_CREAT)
        or stop("emerg", "can't start -- could not create lockfile");
    my @info = stat(FH);
    $info[7] == 0 
	or stop("emerg", "can't start -- $lockfile_path already exists");
    print FH $$;
    close(FH);
    $lockfile_exists = 1;
}
sub stop {
    my $level = shift;
    my $msg = shift;

    # Log the reason for termination
    syslog($level, $msg);

    # Remove the lockfile
    if ($lockfile_exists) {
	unlink($lockfile_path);
    }

    # Log final message, then close the system log
    syslog("info", "stop");
    closelog();

    die;
}
