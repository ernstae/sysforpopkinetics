#!/usr/bin/perl -w

use strict;
use English;
use File::Path;

=head1 NAME

    stage_candidate.pl -- stage an SPK for subsequent deployment

=head1 SYNPOSIS

    stage_candidate.pl

=head1 ABSTRACT

    The stage_candidate.pl operations utility is part of the process of
    moving an SPK system from test into production. It captures software
    that has passed system test, and stores it for subsequent deployment.

=head1 RUNNING

    To run this program, you must satisfy these conditions:
    -  Have a terminal window or ssh window on whitechuck
    -  Run as root in the root environment. One way to achieve this is
       to log in as an ordinary user, then upgrade with 
            su -
        Note that the "-" sign is necessary. Without it, you will be root
        but you will be running in the ordinary user's environment.

=head1 DESCRIPTION

    In the RFPK installation of SPK, two identical platforms are maintained:
    one for system test and the other for production. The migration of new
    versions into production is the following:
    1.  All of the software needed to run the server side of SPK is assembled
        on the system test platform.
    2.  The system is tested, using test sets with known results.
    3.  If problems are detected, they are fixed, and steps 1 and 2 
        repeated.  This occurs as many times as necessary, until system
        test results are satisfactory.
    4.  stage_candidate.pl is run, making a complete copy of the tested
        system, in a form that can be deployed by a companion utility
        called deploy_candidate.pl
    5.  A system is not necessarily deployed immediately, unless it contains
        fixes to bugs with serious impact on users.  Instead, deployments
        are scheduled on a regular basis.  Therefore, it is normal for
        several candidates to be assembled and tested, before deployment
        of the most recent one takes place. stage_candidate can stage a number 
        of candidates (the default is four).
    6.  The companion utility, deploy_candidate.pl, copies the candidate
        onto the production platform.

=head2 DIRECTORY STRUCTURE

    stage_candidate.pl stores candidates in a directory, which we will refer
    to as the candidate directory.  Each candidate is represented by a
    candidate reference file and a candidate data tree.

=head3 CANDIDATE REFERENCE FILE

    A candidate reference file (CRF) is a file that contains the directory
    name of a candidate data tree, followed by a newline.  The CRF for the
    newest candidate is named "candidate", the next most recent candidate
    is called "candidate.1", the one before that "candidate.2", etc.

=head3 CANDIDATE REFERENCE TREE

    Each candidate reference tree (CRT) has a name that identifies it as
    belonging to SPK and the date and time at which it was created.  The
    format of the name is: spk-yyyy-mm-dd-hh-mm-ss.

    The structure of each CRT is the following:

    aspkserver
        usr
            local
                bin
                    spkprod
                include
                    spkprod
                lib
                    spkprod
    cspkserver
        usr
            local
                bin
                    spkprod
                include
                    spkprod
                lib
                    spkprod

=head1 FILES

    /usr/local/spk/ops/candidate/rotate.conf
 
        In order to rotate the candidate names (candidate -> candidate.1->
        candidate.2 -> ...), stage_candidate calls the logrotate unix
        utility for assistance.  The file rotate.conf is required by 
        logrotate.  Here is an example of such a file:

        "candidate" {
            rotate 3
            weekly
        }

        The value of the "rotate" directive tells the maximum depth of the
        sequence of names.  In this case, there will be a "candidate.3"
        but no "candidate.4".  The "weekly" directive has no effect in
        this application, but is required by logrotate.
        
    /usr/local/spk/ops/candidate/rotate.status

        This file is written by logrotate and simply tells the name of
        the latest file in the rotation (which is alwyas "candidate") and
        the date on which the most recent rotation occurred.  This file
        is not of any use in this application, but is simply a side-effect
        of the use of logrotate.

=cut

my $ops_root = "/usr/local/spk/ops";
my $candidate_dir = "$ops_root/candidate";
my $candidate = "candidate";
my $rotate_conf = "rotate.conf";
my $dir_prefix = "spk-";

my $logrotate_command = "/usr/sbin/logrotate";

my $mkdir_command = "/bin/mkdir";
my $scp_command = "/usr/bin/scp";

sub stage_directory {
    my $host = shift;
    my $path = shift;
    my $source = "$host:$path/spktest";
    my $dest = "$host$path";
    my @args = ($scp_command, "-r", $source, $dest);
    print "@args", "\n";
    system(@args);
    my $exit_status = $? >> 8;
    if ($exit_status != 0) {
	die "'scp -r $source $dest' failed\n";
    }
   rename "$dest/spktest", "$dest/spkprod";
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

$EFFECTIVE_USER_ID == 0 
    or die "You must be root to run this program\n";

-d $candidate_dir
    or die "Please create the directory '$candidate_dir', then try again\n";

-f "$candidate_dir/$rotate_conf"
    or die "\nPlease provide $candidate_dir/$rotate_conf;\n"
           . 'it must include an entry for "candidate"'
           . "\n(see unix manual page LOGROTATE(8))\n\n";


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

chdir $new_dir;

stage_directory "aspkserver", "/usr/local/bin";
stage_directory "aspkserver", "/usr/local/include";
stage_directory "aspkserver", "/usr/local/lib";
stage_directory "cspkserver", "/usr/local/bin";
stage_directory "cspkserver", "/usr/local/include";
stage_directory "cspkserver", "/usr/local/lib";

