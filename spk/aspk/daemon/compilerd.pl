#!/usr/bin/perl -w

use strict;

use Fcntl qw(:DEFAULT :flock);
use Proc::Daemon;
use Sys::Syslog (
		 'openlog', 'syslog', 'closelog'
);

my $lockfile_path = "/tmp/lock_compilerd";

sub death {
    my $msg = shift;
    syslog('emerg', $msg);
    closelog();
    die;
}

sub start {
    openlog('compilerd', 'cons', 'daemon');
    syslog('info', 'start');
    sysopen(FH, $lockfile_path, O_RDWR | O_CREAT)
	or death("can't start -- could not create lockfile");
    my @info = stat(FH);
    $info[7] == 0 
	or death("can't start -- $lockfile_path already exists");
    print FH $$;
    close(FH);
}
sub stop {
    unlink($lockfile_path);
    closelog();
    syslog('info', 'stop');
    closelog();
    die;
}
$SIG{"TERM"} = \&stop;

Proc::Daemon::Init();
start();
while(1) {};

exit 0;
