#!/usr/bin/perl -w

use strict;
use English;
use File::Path;

use cand("make_directory");

my $ops_root = "/usr/local/spk/ops";
my $candidate_dir = "$ops_root/candidate";

my $candidate = "candidate";
my $dir_prefix = "spk-";
my $log_file_dir = "/etc/spk";
my $log_file = "$log_file_dir/deployment_log";
my $rotate_conf = "rotate.conf";

my $scp_command = "/usr/bin/scp";

my $logrotate_command = "/usr/sbin/logrotate";

if (@ARGV > 1 || (@ARGV == 1 && $ARGV[0] =~ "--help")) {
    die "usage: $0 [ candidate.n ]\n";
}

$candidate = $ARGV[0] if @ARGV == 1;

$EFFECTIVE_USER_ID == 0 
    or die "You must be root to run this program\n";

-d $log_file_dir
    or make_directory $log_file_dir;

-d $candidate_dir
    or die "The candidate directory, $candidate_dir, appears not to exist.\n";

my $date;
#my ($sec, $min, $hour, $mday, $mon, $year) = localtime;
#my $date = sprintf "%04d-%02d-%02d-%02d%02d-%02d", $year+1900, $mon+1, $mday, $hour, $min, $sec;

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
    my $ddir = "$d:/tmp/usr/local";
    foreach my $f (<$sdir/*>) {
	my @args = ($scp_command, "-r", "$f", $ddir);
	print "@args", "\n";
        system(@args);
        my $exit_status = $? >> 8;
	if ($exit_status != 0) {
	    die "'scp -r $sdir/$f $ddir' failed\n";
	}
    }
}



