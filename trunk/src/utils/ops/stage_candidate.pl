#!/usr/bin/perl -w

use strict;
use English;
use File::Path;

my $ops_root = "/usr/local/spk/ops";
my $candidate_dir = "$ops_root/candidate";
my $candidate = "candidate";
my $rotate_conf = "rotate.conf";
my $dir_prefix = "spk-";

my $logrotate_command = "/usr/sbin/logrotate";
my $mkdir_command = "/bin/mkdir";
my $scp_command = "/usr/bin/scp";

$EFFECTIVE_USER_ID == 0 
    or die "You must be root to run this program\n";

-d $candidate_dir
    or die "Please create the directory '$candidate_dir', then try again\n";

-f "$candidate_dir/$rotate_conf"
    or die "\nPlease provide $candidate_dir/$rotate_conf;\n"
           . 'it must include an entry for "candidate"'
           . "\n(see unix manual page LOGROTATE(8))\n\n";

sub copy_directory {
    my $host = shift;
    my $path = shift;
    my $source = "$host:$path/spktest";
    my $dest = "$host/$path";
    my @args = ($scp_command, "-r", $source, $dest);
    system(@args);
    my $exit_status = $? >> 8;
    if ($exit_status != 0) {
	die "'scp -r $source $dest' failed\n";
    }
}

sub make_directory {
    my $path = shift;
    my @args = ($mkdir_command, "-p", $path);
     system(@args);
    my $exit_status = $? >> 8;
    if ($exit_status != 0) {
	die "Could not make directory '$path'\n";
    }
}

my ($sec, $min, $hour, $mday, $mon, $year) = localtime;

my $date = sprintf "%04d-%02d-%02d-%02d%02d-%02d", $year+1900, $mon+1, $mday, $hour, $min, $sec;

chdir $candidate_dir;

if (-f $candidate) {
    my @args = ($logrotate_command, "--force", "--state", "rotate.status", $rotate_conf );
    system(@args);
    my $exit_status = $? >> 8;
    if ($exit_status != 0) {
	die "logrotate failed\n";
    }
}
my @dirs = <$dir_prefix*>;
my %referenced;
my ($f, $d);
foreach $f (<$candidate*>) {
    open FH, $f;
    my $name = <FH>;
    chomp $name;
    $referenced{$name} = 1;
}

foreach $d (@dirs) {
    $referenced{$d} || File::Path::rmtree $d, 0, 0;
}

open FH, ">$candidate"
    or die "Could not create $candidate_dir/$candidate\n";

print FH "$dir_prefix$date\n";
close FH;

my $new_dir = $dir_prefix . $date;

make_directory "$new_dir/cspkserver/usr/local/bin/spktest";
make_directory "$new_dir/cspkserver/usr/local/include/spktest";
make_directory "$new_dir/cspkserver/usr/local/lib/spktest";
make_directory "$new_dir/aspkserver/usr/local/bin/spktest";
make_directory "$new_dir/aspkserver/usr/local/include/spktest";
make_directory "$new_dir/aspkserver/usr/local/lib/spktest";

exit;

chdir $new_dir;

copy_directory "aspkserver", "/usr/local/bin";
copy_directory "aspkserver", "/usr/local/include";
copy_directory "aspkserver", "/usr/local/lib";

copy_directory "cspkserver", "/usr/local/bin";
copy_directory "cspkserver", "/usr/local/include";
copy_directory "cspkserver", "/usr/local/lib";



