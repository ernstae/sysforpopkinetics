#!/usr/bin/perl -w

use strict;
use Cwd;
use Spkdb (
    'connect', 'disconnect', 'new_job', 'get_job', 'job_status', 
    'de_q2c', 
    'en_q2r', 'de_q2r', 'end_job', 'job_report',
    'new_dataset', 'get_dataset', 'update_dataset', 'user_datasets',
    'new_model', 'get_model', 'update_model', 'user_models',
    'new_user', 'update_user', 'get_user', 'email_for_job'
	   );
use Fcntl qw(:DEFAULT :flock);

=head1 NAME

    take_snapshot.pl -- take a snapshot of the production database

=head1 SYNOPSIS

    take_snapshot.pl job_id ...

=head1 DESCRIPTION

    Given a list of job_id numbers, this program extracts the corresponding
    rows from the job table of the production database, spkdb, and all related 
    rows from the job, history, user, model and dataset tables, as if the
    selected jobs had been submitted but had not yet been compiled.  The output
    consists of the three files expected by the load_spktest.pl utility; namely
    schema.sql, basedata.sql, and userdata.sql.

    In conjunction with load_spktest,pl, this program can be used to build the
    spktest with a specific set of jobs, prior to testing.  Here is a simple
    shell script that demonstrates the principle.  It extracts jobs 5, 11, and
    15 from spkdb and uses them and related rows to reinitialize and load spktest:

    #!/bin/bash
    cd /tmp
    take_snapshot.pl 5 11 15
    load_spktest.pl

=head1 DEPENDENCIES

    This program uses the following utility programs to do much of its work:
    
        dump_spkdb.pl
        load_spktest.pl
        mysqldump

    It also uses the SPK Perl API module:

        Spkdb.pm

=head1 BUGS

    The output files, schema.sql, basedata.sql, and userdata.sql are created
    in the current direction.  If files with those same names already exist
    in the current directory, they will be overwritten.

    The utility makes use of a temporary database called spktmp, which is 
    assumed to be located on the local system.  The utility can be run on any
    system which has a local spktmp database.  In particular, it can be run on
    dbserver, where such a database is known to exist.  To make it run on another
    machine, an spktmp database must be created, along with a user called
    "extractor@localhost identified by 'extractor'". with table drop and create
    privileges.

    Because the utility drops all tables in spktmp, then recreates them, only one
    copy of the program can run at a time.  A lock file is used to enforce this
    discipline.  If the lock file already exists, the program will refuse run.
    If no lock file can be found, the program will create one.  When the program
    terminates, whether successfully or with error, it removes the lock file.
    If, however, the program is killed, the lock file will not be removed, and
    the utility will refuse to run until the lock file is removed manually.

=cut

my $spkdbname   = "spkdb";
my $spktmpname  = "spktmp";

my $dbhost = "dbserver";
my $dbuser = "extractor";
my $dbpass = "extractor";

my $lockfile_exists = 0;
my $lockfile_path = "/tmp/lock-take_snapshot";

sub extract_model_or_dataset(@);

my ($sec, $min, $hour, $mday, $mon, $year) = localtime;
my $date = sprintf "%04d-%02d-%02d-%02d%02d-%02d", $year+1900, $mon+1, $mday, $hour, $min, $sec;

my $dump_spkdb = "/usr/local/bin/dump_spkdb.pl";
my $load_spktest = "/usr/local/bin/load_spktest.pl";
my $mysqldump = "/usr/bin/mysqldump";

my $spkdb_dbh = &connect($spkdbname, $dbhost, $dbuser, $dbpass)
    or die("Could not connect to database $spkdbname\n");

my %model_list;
my %dataset_list;
my %user_list;
my @job_list = add_parents_to_list(@ARGV);

system("$dump_spkdb --noprefix") == 0 
    or death("$dump_spkdb failed");

open FL, "> userdata.sql";  # truncate userdata.sql file
close FL;

my $cmd = "$load_spktest --host $dbhost --database $spktmpname --user $dbuser --password $dbpass";
system($cmd) == 0
    or death("'$cmd' failed\n");

# Create a lockfile and store our pid, so only one copy of parent can run
sysopen(FH, $lockfile_path, O_RDWR | O_CREAT)
    or death("emerg", "could not create lockfile");
my @info = stat(FH);
$info[7] == 0 
    or death("emerg", "$lockfile_path already exists");
print FH $$;
close(FH);
$lockfile_exists = 1;

my $spktmp_dbh = &connect($spktmpname, "localhost", $dbuser, $dbpass)
    or die("Could not connect to database $spktmpname\n");

my $history_sth = $spkdb_dbh->prepare("select * from history where job_id=? and state_code='q2c';")
    or death("prepare of 'select * from history' failed");

for my $job_id (@job_list) {
    $history_sth->execute($job_id)
	or death("execute of 'select * from history' failed");
    my $row = $history_sth->fetchrow_hashref();
    my $event_time = $row->{'event_time'};
    my $sql = "insert into history ("
	. (join ",", keys %$row)
	. ") values ('"
	. (join "','", values %$row)
	."');";
    $spktmp_dbh->do($sql)
	or death("couldn't do '$sql'");
    
    $row = &get_job($spkdb_dbh, $job_id)
	or death("No job with job_id: $job_id\ exists\n");
    
    $model_list{$row->{'model_id'}}     = 0;
    $dataset_list{$row->{'dataset_id'}} = 0;
    $user_list{$row->{'user_id'}}     = 0;

    $row->{'state_code'} = 'q2r';
    my $xml_source = $row->{'xml_source'};
    my $model_id   = $row->{'model_id'};
    my $dataset_id = $row->{'dataset_id'};
    delete $row->{'xml_source'};
    delete $row->{'cpp_source'};
    delete $row->{'report'};
    delete $row->{'end_code'};
    $row->{'start_time'} = $row->{'event_time'} = $event_time;
    $row->{'state_code'} = 'q2c';
    $sql = "insert into job (xml_source,cpp_source,report,end_code,"
	. (join ",", keys %$row)
	. ") values (?,NULL,NULL,NULL,'"
	. (join "','", values %$row)
	. "');";
    my $job_sth = $spktmp_dbh->prepare($sql)
	or death("prepare of '$sql' failed");
    $job_sth->execute($xml_source)
	or death("execute of '$sql' failed");

}
$history_sth->finish;

extract_model_or_dataset("model",   keys %model_list);
extract_model_or_dataset("dataset", keys %dataset_list);

my $sth_in = $spkdb_dbh->prepare("select * from user where user_id=?;")
    or death("prepare of 'select * from user' failed");

for my $user_id (keys %user_list) {
    # Get a row from spkdb
    $sth_in->execute($user_id)
	or death("execute of 'select * from user' failed");
    my $row = $sth_in->fetchrow_hashref();
    my $sql = "insert into user ("
	. (join ",", keys %$row)
	. ") values ('"
	. (join "','", values %$row)
	. "');";
    $spktmp_dbh->do($sql)
	or death("failed to do '$sql'");
}
$sth_in->finish;

system("$mysqldump -h$dbhost -u$dbuser -p$dbpass -tc $spktmpname --tables "
       . "dataset history job model user > userdata.sql") == 0
    or death("mysqldump failed");

$spkdb_dbh->disconnect;
$spktmp_dbh->disconnect;

# Remove the lockfile
if ($lockfile_exists) {
    unlink($lockfile_path);
}

exit 0;


sub extract_model_or_dataset(@) {
    my $name = shift;
    my @list = @_;

    my $sth_in = $spkdb_dbh->prepare("select * from $name where ${name}_id=?;")
	or death("prepare of 'select * from $name' failed");

    for my $id (@list) {
	# Get a row from spkdb
	$sth_in->execute($id)
	    or death("execute of 'select * from $name' failed");
	my $row = $sth_in->fetchrow_hashref();

	# The user may be different than the user of the job
	$user_list{$row->{'user_id'}} = 0;

	# Create a row in spktmp
	my $archive = $row->{'archive'};
	delete $row->{'archive'};
	my $sql = "insert into $name (archive,"
	    . (join ",", keys %$row)
	    . ") values (?,'"
	    . (join "','", values %$row)
	    . "');";
	my $sth_out = $spktmp_dbh->prepare($sql)
	    or death("prepare of 'insert into $name' failed");

	$sth_out->execute($archive)
	    or death("execute of 'insert into $name' failed");
    }    
}

sub add_parents_to_list {
    my @argv = @_;
    my %job_list;
    my $job_id;
    for $job_id (@argv) {
	$job_list{$job_id} = 0;
    }
    my $row;
    my $repeat = 0;
    do {
	$repeat = 0;
	foreach $job_id (keys %job_list) {
	    if ($job_list{$job_id} == 0) {
		$row = &get_job($spkdb_dbh, $job_id)
		    or death("No job with job_id: $job_id\ exists\n");
		$job_list{$job_id} = 1;
		my $parent = $row->{"parent"};
		if ($parent && ! exists $job_list{$parent}) {
		    $job_list{$parent} = 0;
		    $repeat = 1;
		}
	    }
	}
    } while $repeat;
    return keys %job_list;
}
sub death  {
    my $msg = shift;
    # Remove the lockfile
    if ($lockfile_exists) {
	unlink($lockfile_path);
    }
    $spkdb_dbh->disconnect  if $spkdb_dbh;
    $spktmp_dbh->disconnect if $spktmp_dbh;
    die("$msg\n");
}
