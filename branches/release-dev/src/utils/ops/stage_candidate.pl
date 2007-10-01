#!/usr/bin/perl -w

use lib "/usr/local/bin";
use strict;
use English;
use File::Path;
use Candidate('make_directory', 'stage_directory');
use Fcntl qw(:DEFAULT :flock);

my $ops_root = "/usr/local/spk/ops";
my $candidate_dir = "$ops_root/candidate";
my $candidate = "candidate";
my $rotate_conf = "rotate.conf";
my $dir_prefix = "spk-";
my $version_file = "$candidate_dir/version";

my $scp_command = "/usr/bin/scp";

my $logrotate_command = "/usr/sbin/logrotate";

$EFFECTIVE_USER_ID == 0 
    or die "You must be root to run this program\n";

-d $candidate_dir
    or die "Please create the directory '$candidate_dir', then try again\n";

-f "$candidate_dir/$rotate_conf"
    or die "\nPlease provide $candidate_dir/$rotate_conf;\n"
           . 'it must include an entry for "candidate"'
           . "\n(see unix manual page LOGROTATE(8))\n\n";

# read version and update it atomically
open(FH, "+< $version_file") or die "can't open $version_file\n";

unless (flock(FH, LOCK_EX | LOCK_NB)) {
    local $| = 1;
    print "waiting for lock on $version_file...";
    flock(FH, LOCK_EX) or die "can't lock $version_file\n";
    print "\tOK\n";
}
my $version = <FH>;
chomp $version;
my $minor_version = $version;
$minor_version =~ s/^.*\.//;
$minor_version++;
$version =~ s/[0-9]*$/$minor_version/;
truncate(FH, 0) or die "couldn't truncate $version_file\n";
seek(FH, 0, 0)  or die "couldn't seek to the beginning of $version_file\n";
print FH "$version\n";
flock(FH, LOCK_UN);
close FH;

# rotate candidates
chdir $candidate_dir;

if (-f $candidate) {
    my @args = ($logrotate_command, "--force", "--state", "rotate.status", $rotate_conf );
    system(@args);
    my $exit_status = $? >> 8;
    if ($exit_status != 0) {
	die "logrotate failed\n";
    }
}
#after rotation, determine which CRTs are still referenced
my @dirs = <$dir_prefix*>;
my %referenced;
my ($f, $d);
foreach $f (<$candidate*>) {
    open FH, $f;
    my $name = <FH>;
    chomp $name;
    $referenced{$name} = 1;
}
#remove any CRTs which are no longer referenced
foreach $d (@dirs) {
    $referenced{$d} || File::Path::rmtree $d, 0, 0;
}
# create the new candidate and copy files to it
open FH, ">$candidate"
    or die "Could not create $candidate_dir/$candidate\n";

print FH "$dir_prefix$version\n";
close FH;

my $new_dir = $dir_prefix . $version;

&make_directory("$new_dir/cspkserver/usr/local/bin/spktest");
&make_directory("$new_dir/cspkserver/usr/local/include/spktest");
&make_directory("$new_dir/cspkserver/usr/local/lib/spktest");
&make_directory("$new_dir/cspkserver/usr/local/src/spktest");
&make_directory("$new_dir/aspkserver/usr/local/bin/spktest");
&make_directory("$new_dir/aspkserver/usr/local/include/spktest");
&make_directory("$new_dir/aspkserver/usr/local/lib/spktest");
&make_directory("$new_dir/aspkserver/usr/local/src/spktest");
&make_directory("$new_dir/webserver/usr/local/tomcat/instance/prodssl/webapps");

chdir $new_dir;

&stage_directory("aspkserver", "/usr/local/bin");
&stage_directory("aspkserver", "/usr/local/include");
&stage_directory("aspkserver", "/usr/local/lib");
&stage_directory("aspkserver", "/usr/local/src");
&stage_directory("cspkserver", "/usr/local/bin");
&stage_directory("cspkserver", "/usr/local/include");
&stage_directory("cspkserver", "/usr/local/lib");
&stage_directory("cspkserver", "/usr/local/src");

#my $srce = "webserver:/usr/local/tomcat/instance/testssl/webapps/user.war";
#my $dest =  "webserver/usr/local/tomcat/instance/prodssl/webapps/user.war";
#my @args = ($scp_command, "-r", $srce, $dest);
#system(@args);
#my $exit_status = $? >> 8;
#if ($exit_status != 0) {
#    die "'scp -r $srce $dest' failed\n";
#}
# create a notes file to be edited by the developer
my $notes = "$candidate_dir/notes-$version";
my $template = "$candidate_dir/notes_template.txt";
my $dashes = "----------------------------------";

open FH, "> $notes" or die "Could not create $notes\n";
open TH, $template  or die "Could not open $template\n";
print FH "$dashes\n";
print FH "RELEASE NOTES FOR VERSION $version\n";
print FH  "$dashes\n";
while (<TH>) {
    print FH;
}
close TH;
close FH;
chmod 0666, $notes;
print "\nPlease provide release notes for this candidate.\n";
print "\nAs an ordinary user, edit the file\n";
print "\n\t$notes\n\n";

__END__


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
    being a version of spk. The format of the names is: spk-version

    The structure of each CRT is the following:

    aspkserver
        usr
            local
                bin
                    spkprod
                        (files)
                include
                    spkprod
                        (files)
                lib
                    spkprod
                        (files)
    cspkserver
        usr
            local
                bin
                    spkprod
                        (files)
                include
                    spkprod
                        (files)
                lib
                    spkprod
                        (files)
                src
                    spkprod
                        (files)
    webserver
        usr
            local
                tomcat
                    prodssl
                        webapps
                            user.war

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

    /usr/local/spk/ops/candidate/spk-version

        The actual candidate reference trees (CRTs) are directories
        with path names that conform to the above format.

    /usr/local/spk/ops/candidate/notes-version

        A file to be edited by the developer of the candidate so that it 
        contains release notes describing the changes.  At deployment,
        all the release notes files created since the last deployment are
        concatenated to form the release notes for the deployed version.

    /usr/local/spk/ops/candidate/candidate
     
        Contains the file name of the most recently staged CRT

    /usr/local/spk/ops/candidate/candidate.1

        Contains the file name of the CRT which is the most recent save 1.

     ...

    /usr/local/spk/ops/candidate/candidate.n

       Contains the file name of the CRT which is the most recent save n.

    /usr/local/spk/ops/candidate/version

       Contains the version number of the must recently staged candidate.

=cut
