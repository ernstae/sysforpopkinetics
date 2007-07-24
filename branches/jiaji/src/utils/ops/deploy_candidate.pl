#!/usr/bin/perl -w

use lib "/usr/local/bin";
use strict;
use Getopt::Long;
use English;
use File::Path;
use Candidate('make_directory');

=head1 NAME

    deploy_candidate.pl -- deploy an SPK candidate system to production

=head1 SYNPOSIS

    deploy_candidate.pl [--test] [--help]

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

=head1 OPTIONS

    --prod    If this option is provided, the most recent candidate will 
              be copied into the production environment.

    --test    This is the default option.  
              If this option is provided, the most recent candidate will 
              be copied into the test environment, rather than the 
              production environment.  This should be used to restore the
              test environment after a failed session of system testing.

=cut

my $usage = "usage: deploy_candidate.pl [--prod or --test] --[help]";

my $ops_root = "/usr/local/spk/ops";
my $candidate_dir = "$ops_root/candidate";

my $candidate = "candidate";
my $dir_prefix = "spk-";
my $log_file_dir = "/etc/spk";
my $log_file = "$log_file_dir/deployment_log";
my $rotate_conf = "rotate.conf";

my $scp_command = "/usr/bin/scp";
my $logrotate_command = "/usr/sbin/logrotate";

my $tmp_dir = "/tmp/deploy_candidate-$$";

my $test = 1;

my %opt = ();
GetOptions (\%opt, 'prod', 'test', 'help') 
    or die "$usage\n";

defined $opt{'help'} == 0
    or die "$usage\n";

$test = 0 if defined $opt{'prod'};

# check for RHEL 4 bug in OpenSSH
my $rhel_problem = 0;
my $rhel_ssh = `/bin/rpm -q openssh`;
chomp($rhel_ssh);
if ( $rhel_ssh =~ /RHEL4/ig ) {
  $rhel_problem = 1;
}

print("Deploying into ");
if( $test ) {
  print("test");
}
else {
  print("production");
}
print(" environment.\n");

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

my $ddir = '';
my @args = ("");

foreach my $s ("aspkserver", "cspkserver") {
    my $sdir = "$candidate_dir/$name/$s";
    -d $sdir or die "Candidate subtree\n$sdir\nappears not to exist.\n";
    $sdir .= "/usr/local";
    if ($test) {
        $ddir = "$tmp_dir/$s";
        &make_directory($ddir);
    }
    else {
       $ddir = $s eq "aspkserver" ? "/usr/local" : "$s:/usr/local";
    }

    # fix for RHEL 4 scp problem
    if ( ($ddir !~ /\:/ig) && $rhel_problem == 1) {
        $ddir = "localhost:" . $ddir;
    }

    foreach my $f (<$sdir/*>) {
	@args = ($scp_command, "-r", "$f", $ddir);
        system(@args);
        my $exit_status = $? >> 8;
	if ($exit_status != 0) {
	    die "'scp -r $f $ddir' failed: $!\n";
	}
    }
}
#my $sdir =  "webserver/usr/local/tomcat/instance/prodssl/webapps";
#if ($test) {
#   $ddir = "$tmp_dir/webserver/usr/local/tomcat/instance/prodssl/webapps";
#   &make_directory($ddir);
#}
#else {
#   $ddir = "webserver:/usr/local/tomcat/instance/prodssl/webapps";
#}
#my @args = ($scp_command, "-r", "$sdir/user.war", "$ddir/user.war");
#system(@args);
#my $exit_status = $? >> 8;
#if ($exit_status != 0) {
#    die "'scp -r $sdir/user.war $ddir/user.war' failed\n";
#}

exit 0 if (not $test);

# Sachiko 10/9/2005
# The originating directory name was constructed incorrectly.  Fixed it.
# Added "or die" in case renaming fails.
foreach my $s ("aspkserver", "cspkserver") {
   foreach my $d ("bin", "lib", "include", "src") {
       rename( "$tmp_dir/$s/$d/spkprod", "$tmp_dir/$s/$d/spktest")
         or die "'renaming from $tmp_dir/$s/$d/spkprod' to '$tmp_dir/$s/$d/spktest' failed";
    }
}

#clear out arguments
@args = ("");

foreach my $s ("aspkserver", "cspkserver") {
    my $sdir = "$tmp_dir/$s";
    $ddir = $s eq "aspkserver" ? "/usr/local" : "$s:/usr/local";

    # fix for RHEL 4 scp problem
    if ( ($ddir !~ /\:/ig) && $rhel_problem == 1) {
        $ddir = "localhost:" . $ddir;
    }

    foreach my $f (<$sdir/*>) {
        @args = ($scp_command, "-r", "$f", $ddir);
        system(@args);
        my $exit_status = $? >> 8;
	if ($exit_status != 0) {
	    die "'scp -r $f $ddir' failed\n";
	}
    }
}

#$sdir = "$tmp_dir/webserver/usr/local/tomcat/instance/prodssl/webapps";
#$ddir =  "webserver/usr/local/tomcat/instance/prodssl/webapps";
#@args = ($scp_command, "-r", "$sdir/user.war", "$ddir/user.war");
#system(@args);
#$exit_status = $? >> 8;
#if ($exit_status != 0) {
#    die "'scp -r $sdir/user.war $ddir/user.war' failed\n";
#}

File::Path::rmtree($tmp_dir, 0, 0);

exit 0;
