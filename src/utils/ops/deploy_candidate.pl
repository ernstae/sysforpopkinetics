#!/usr/bin/perl -w

push @INC, "/usr/local/bin";
use strict;
use English;
use File::Path;
use Candidate('make_directory');

=head1 NAME

    deploy_candidate.pl -- deploy an SPK candidate system to production

=head1 SYNPOSIS

    deploy_candidate.pl

=head1 ABSTRACT

    The deploy_candidate.pl operations utility is part of the process of
    moving an SPK system from test into production. It takes the most
    recent candidate and moves it to production.

=head1 RUNNING

    To run this program, you must satisfy these conditions:
    -  Have a terminal window or ssh window on whitechuck
    -  Run as root in the root environment. One way to achieve this is
       to log in as an ordinary user, then upgrade with 
            su -
        Note that the "-" sign is necessary. Without it, you will be root
        but you will be running in the ordinary user's environment.

=head1 DESCRIPTION

    See documentation for stage_candidate.pl for an overview of the test
    and installation platforms for the RFPK installation of SPK, and the
    process of moving systems from test to production, the directory
    structure used for storing candidate systems, the contents and the
    format of files.

=cut


my $ops_root = "/usr/local/spk/ops";
my $candidate_dir = "$ops_root/candidate";

my $candidate = "candidate";
my $dir_prefix = "spk-";
my $log_file_dir = "/etc/spk";
my $log_file = "$log_file_dir/deployment_log";
my $rotate_conf = "rotate.conf";

my $mkdir_command = "/bin/mkdir";
my $scp_command = "/usr/bin/scp";
my $logrotate_command = "/usr/sbin/logrotate";

if (@ARGV > 1 || (@ARGV == 1 && $ARGV[0] =~ "--help")) {
    die "usage: $0 [ candidate.n ]\n";
}

$candidate = $ARGV[0] if @ARGV == 1;

$EFFECTIVE_USER_ID == 0 
    or die "You must be root to run this program\n";

-d $log_file_dir
    or &make_directory("$log_file_dir");

-d $candidate_dir
    or die "The candidate directory, $candidate_dir, appears not to exist.\n";

chdir $candidate_dir;

-f $candidate
    or die "Your candidate file, $candidate_dir/$candidate, appears not to exist.\n";

open FH, $candidate;
my $name = <FH>;
chomp $name;

-d $name
    or die "Candidate tree, $candidate_dir/$name, appears not to exist.\n";

chdir $name;

foreach my $d ("aspkserver", "cspkserver") {
    my $sdir = "$candidate_dir/$name/$d";
    -d $sdir or die "Candidate subtree\n$sdir\nappears not to exist.\n";
    $sdir .= "/usr/local";
    my $ddir = $d eq "aspkserver" ? "/usr/local" : "$d:/usr/local";

    foreach my $f (<$sdir/*>) {
	my @args = ($scp_command, "-r", "$f", $ddir);
        system(@args);
        my $exit_status = $? >> 8;
	if ($exit_status != 0) {
	    die "'scp -r $sdir/$f $ddir' failed\n";
	}
    }
}



