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

traceserver.pl -- the SPK trace server

=head1 SYNOPSIS

traceserver.pl mode

=head1 ABSTRACT

The SPK trace server accepts request: job identification number,
and sends respond: the text of the optimization trace file in the 
job's working directory if the file exists, or an empty string if 
the file does not exist.  

=head1 DESCRIPTION

=head2 ARGUMENTS

The program expects the following arguments:

    $mode
        The test mode indicator being "test" for test mode, "prod" for production mode 
    
=head2 OPERATION

The first thing that traceserver.pl does after starting up is to call
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
use strict;

Then, it waits for incoming requests.  The request should be a job's 
identification number.  The program forms the path-name of the job's
optimization trace file in the job's working directory.  If the file
exists, the program opens the file, and send the content of the file 
to the client as the response.  If the file does not exist, an empty 
string is sent as the response. 

=head1 RETURNS

Nothing, because it has no parent (other than init) to which an exit
code might be returned.  The program does, however, write event messages
to the system log.

=cut

use Sys::Syslog('openlog', 'syslog', 'closelog');
use Sys::Hostname;
use IO::Socket;
use Proc::Daemon;
use File::Path;
use Fcntl qw(:DEFAULT :flock);

my $mode = shift;
my $working_dir = "/usr/local/spk/share/working/spk$mode/spkjob-";
my $filename = "/optimizer_trace.txt";
my $lockfile_exists = 0;
my $host = hostname();
my $port = 9002;
my $service_root = "trace";
if ($mode eq "test") {
   $service_root .= "test";
   $port = 9003;
}  
my $service_name = "$service_root" . "d";
my $lockfile_path = "/tmp/lock_$service_name";

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

# TERM signal handler
sub stop {
    my $level = shift;
    my $msg = shift;

    # Log the reason for termination
    syslog($level, $msg);

    # Stop all sub-processes
    kill('KILL', -$$);

    # Remove the lockfile 
    if ($lockfile_exists) {
	unlink($lockfile_path);
    }

    # Log final message, then close the system log
    syslog("info", "stop");
    closelog();

    die;
}

# become a daemon
Proc::Daemon::Init();

# Initialize
start();

# set children group
setpgrp(0, 0);

# designate a handler for the TERM signal
$SIG{'TERM'} = \&stop;
$SIG{CHLD} = 'IGNORE';

# become a server
my $sock = new IO::Socket::INET(
                   LocalHost => $host,
                   LocalPort => $port,
                   Proto     => 'tcp',
                   Listen    => SOMAXCONN,
                   Reuse     => 1);
$sock or die "no socket :$!";

# wait for and response to request
my($new_sock, $buf, $pid);
while ($new_sock = $sock->accept())
{
    if ($pid = fork)
    {
    }
    elsif (defined $pid)
    {
        while (defined($buf = <$new_sock>))
        {
            chop $buf;
            my $trace = "";
            if( $buf =~ /^\d+$/) { 
                my $filepath = $working_dir . $buf . $filename;
                if( -f $filepath && -s $filepath > 0 ) {
                    open(FH, $filepath);
                    read(FH, $trace, -s FH);
                    close(FH);
                }
            }
            print $new_sock $trace;            
            shutdown $new_sock, 2;
            exit;
        }
    }
}
