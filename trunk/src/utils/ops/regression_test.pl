#!/usr/bin/perl -w

use lib "/usr/local/bin";
use strict;
use English;
use File::Path;
use XML::Simple;
use Data::Dumper;
use Getopt::Long qw(:config no_auto_abbrev);
use Pod::Usage;

use Spkdb ('connect', 'disconnect', 'job_status');

my $base_dir = "/usr/local/spk/ops/regression_test";
my %file_to_compare = ( 'cerr' => 'compilation_error.xml',
		        'srun' => 'result.xml');

my $config_file = "regression_test.xml";

my %opt = ();
GetOptions (\%opt, 'help', 'man', 'dump-config', 'ignore-candidate', 'config-file=s') 
    or pod2usage(-verbose => 0);
pod2usage(-verbose => 1)  if (defined $opt{'help'});
pod2usage(-verbose => 2)  if (defined $opt{'man'});

$EFFECTIVE_USER_ID == 0 
    or die "You must be root to run this program\n";

$ENV{'LOGNAME'} eq 'root' 
    or die "You must execute $ENV{'HOME'}/.bash_profile before running this program.\n"
    .  "If you become root with the command 'su -', that will happen automatically.\n";
 
$config_file = $opt{'config-file'} if (defined $opt{'config-file'});

my $config = XMLin($config_file, ForceArray => 1);

if (defined $opt{'dump-config'}) {
    print Dumper($config);
    exit 0;
}
$| = 1;
my $bit_bucket;
if (! defined $opt{'ignore-candidate'}) {
    print "executing 'deploy_candidate.pl --test' (this will take a while)";
    $bit_bucket = `deploy_candidate.pl --test`;
    $? == 0
	or die "could not execute 'deploy_candidate.pl --test'\n";
    print "\t\tOK\n";
}

print "stopping the aspkserver test daemon";
$bit_bucket = `/etc/rc.d/init.d/spkcmptestd stop`;
$? == 0
    or die "could not execute '/etc/rc.d/init.d/spkcmptestd stop'\n";
print "\t\t\t\t\tOK\n";

print "stopping the cspkserver test daemon:";
$bit_bucket = `ssh cspkserver '/etc/rc.d/init.d/spkruntestd stop'`;
$? == 0
    or die "could not execute '/etc/rc.d/init.d/spkruntestd stop'\n";
print "\t\t\t\t\tOK\n";

my $job =  $config->{'cerr'}[0]{'job'};
my @alljobs = @$job;
$job =  $config->{'srun'}[0]{'job'};
push @alljobs, @$job;

my $cmd = "take_snapshot.pl " . join " ", @alljobs;
print "taking a snapshot of selected jobs in the production database";
$bit_bucket = `$cmd`;
$? == 0
    or die "could not execute '$cmd'\n";
print "\t\tOK\n";

print "loading test database with snapshot";
$cmd = "load_spktest.pl";
$bit_bucket = `$cmd`;
print "\t\t\t\t\tOK\n";

print "starting the aspkserver test daemon";
$bit_bucket = `/etc/rc.d/init.d/spkcmptestd start`;
$? == 0
    or die "could not execute '/etc/rc.d/init.d/spkcmptestd stop'\n";
print "\t\t\t\t\tOK\n";

print "starting the cspkserver test daemon:";
$bit_bucket = `ssh cspkserver '/etc/rc.d/init.d/spkruntestd start'`;
$? == 0
    or die "could not execute '/etc/rc.d/init.d/spkruntestd stop'\n";
print "\t\t\t\t\tOK\n";

my %job_done;

for my $job_id (@alljobs) {
    $job_done{$job_id} = 0;
}

print "waiting for jobs to complete -- (this will take a while) ";

my $dbh = &connect("spktest", "localhost", "reader", "reader");

my $active_jobs = @alljobs;

while ($active_jobs) {
    JOB: for my $job_id (keys %job_done) {
	if (! $job_done{$job_id}) {
	    my $row = &job_status($dbh, $job_id);
	    die "\njob_status call for job_id $job_id failed\n" if (!$row);
	    next JOB if $row->{"state_code"} !~ /^end$/;
	    print "\n\tjob $job_id has reached 'end'\t\t\t\t\tOK";
	    $active_jobs--;
	    $job_done{$job_id} = 1;
	}
    }
    sleep 5;
};
&disconnect($dbh);

my @args;

my $jobs_that_differed = 0;

for ('cerr', 'srun') {
    my $job    =  $config->{$_}[0]{'job'};
    my $ignore =  $config->{$_}[0]{'ignore'};
    my $file = $file_to_compare{$_};
    /cerr/ and do {
	print "\nchecking differences for jobs with end_code 'cerr'";
	for my $job_id (@$job) {
	    print "\n\tjob $job_id";
	    @args = ("diff", "-bB");
	    for my $regexp (@$ignore) {
		push @args, ("--ignore-matching-lines", "\"$regexp\"");
	    }
	    push @args, "$base_dir/$_/spkcmptest-job-$job_id/compilation_error.xml";
	    push @args, "/tmp/spkcmptest-job-$job_id/compilation_error.xml";
	    if (system(@args) == 0) {
		print "\t\t\t\t\t\t\t\tOK";
	    }
	    else {
		$jobs_that_differed++;
	    }
	}
    };
    /srun/ and do {
	print "\nchecking differences for jobs with end_code 'srun'";
	for my $job_id (@$job) {
	    print "\n\tjob $job_id";
	    @args = ("ssh", "cluster", "diff", "-bB");
	    for my $regexp (@$ignore) {
		push @args, ("--ignore-matching-lines", "\"$regexp\"");
	    }
	    push @args, "$base_dir/$_/spkruntest-job-$job_id/result.xml";
	    push @args, "/tmp/spkruntest-job-$job_id/result.xml";
	    system(@args);
	    if (system(@args) == 0) {
		print "\t\t\t\t\t\t\t\tOK";
	    }
	    else {
		$jobs_that_differed++;
	    }
	}
    };
}
print "\nNumber of jobs that differed: $jobs_that_differed\n";

exit $jobs_that_differed;

__END__


=head1 NAME

regression_test.pl -- test a candidate before deployment

=head1 SYNOPSIS

regression_test.pl [--help] [--man] [--dump-config] [--ignore-candidate] [--config-file=file]

=head1 ABSTRACT

B<regression_test.pl> is an operations utility which is part of the
process of moving an SPK system from test into production. It tests
the current deployment candidate against a set of jobs with known
outcomes, to insure that the candidate does not "break" features that
previously worked.

=head1 RUNNING

To run B<regression_test.pl>, you must satisfy these conditions:

=over 2

=item

Have exclusive use of the test environment.  This will have to be
coordinated with the other developers and testers.

=item

Have a terminal window or ssh window on whitechuck

=item

Log in as root, so that root's B<.bash_profile> is executed, which
runs B<keychain> and starts B<ssh_agent>.  One way to achieve this is
to log in as an ordinary user, then execute B<su -> to upgrade.
(Note that the B<-> sign is necessary. Without it, root's 
B<.bash_profile> will not be executed.)

=back

=head1 DESCRIPTION

B<regression_test.pl> expects two sets of job_id numbers to be
specified in input file.  One set references jobs which ran
successfully in the production environment, and the other references
jobs which were rejected by the SPK compiler due to syntax errors.
These jobs are rerun in the test environment and then the results are
checked against known results.

=head1 OPTIONS AND ARGUMENTS

=over 8

=item B<--help>

Print a brief help message and exit.

=item B<--man>

Print the manual page and exit.

=item B<--dump-config>

Dump out the data structure parsed from the xml configuration file 
and exit.

=item B<--ignore-candidate>

Useful if this program is used for system testing rather than regression
testing.  In regression testing, when used as part of the deployment 
process, the most recent deployment candidate is copied into the test
environment.  With this option, that copy does not occur.

=item B<--config-file=file>

Specify the name of the configuration file.  If this argument is not
present, the configuration file name is assumed to be
"regression_test.xml".  See the CONFIGURATION section for the format
of this file.

=back

=head1 CONFIGURATION FILE

The configuration file is in xml format. Its default name is 
"regression_test.xml", but a file of another name can be used if
that name is provided on the command-line as the value for the
B<--config-file> argument.

=head2 SYNTAX

The configuration file has the following syntax:

    <regression_test>
        <cerr>
            <job> job_id number </job>
            <job> job_id number </job>
            ...
            <ignore> regexp </ignore>
            <ignore> regexp </ignore>
            ...
        </cerr>
        <srun>
            <job> job_id number </job>
            <job> job_id number </job>
            ...
            <ignore> regexp </ignore>
            <ignore> regexp </ignore>
            ...
        </srun>
    </regression_test>

=head1 TEST SET

The test set consists of working directories for a set of jobs.  The
set is divided into two subsets.  One subset consists of jobs which
failed in the SPK compilation phase and, hence, have "cerr" for
end_code.  The other subset consists of jobs which were successful
and, thus, have "srun" for end_code.

The working directories of the "cerr" subset reside on aspkserver in
the directory B</usr/local/spk/ops/regression_test/cerr>.  Each working
directory has a name with the format B<spkcmptest-job-n>, where B<n>
is a job_id. 

The working directories of the "srun" subset reside on cspkserver in
the directory B</usr/local/spk/ops/regression_test/srun>.  Each working
directory has a name with the format B<spkruntest-job-n>, where B<n>
is a job_id. 

To add jobs to the test set, 

=over 4

=item 1.

Run the jobs in the test environment, following the procedure outlined in the
B<SPK System Testing Guide>.  

=item 2

Depending on whether a job terminated with "cerr" or "srun", copy 
its working directory to the appropriate directory on either aspkserver
or cspkserver.

=item 3

Add the jobs to their appropriate subsets in the configuration file.

=back

=head1 EXIT STATUS

Returns 0 as exit status if the regression test was successful; otherwise
a non-zero value is returned.

=cut