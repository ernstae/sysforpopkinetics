#!/usr/bin/perl -w

use strict;
use Getopt::Long;

=head1 NAME
    
    dump_spkdb.pl -- take a symbolic dump of the production database

=head1 ABSTRACT

    This program takes a symbolic dump of the production database, so that
    the a clone of the database can be constructed in the test database 
    using load_spktest.pl.

    Warning: the output of this program may contain inconsistencies if a transaction
    was in progress at the time the program was run.  The output should not, therefore,
    be relied upon for rebuilding the production database in case it is damaged.
    The principal use of this program is to create test databases as clones of 
    the production database, using the program load_spktest.pl

=head1 SYNOPSIS

    dump_spkdb.pl [ --[no]prefix ][ --[no]userdata ][--spktest]

=head1 DESCRIPTION

    This program is designed to be run from the database server. It is not necessary
    for the user have root privileges.  It does not alter the production database
    in any way.  It produces its output in the current directory.

=head2 OUTPUT FILE CONTENTS

    The output of this program consists of three files of sql statements:
    - schema:   'create table' statements
    - basedata: 'insert' statements for inserting all data into the non-transactional
                table; namely, class, end, method, and state
    - userdata: 'insert' statements for inserting all data into the rest of the tables

    The output of the userdata file can be suppressed by specifying --nouserdata on the
    command line.

=head2 OUTPUT FILE NAMES

    By default, or if the --prefix argument is present, the output files have a prefix
    added to their names, which includes the name of the database, the date and time:

    spkdb-yyyy-mm-dd-hhmm-ss-schema.sql
    spkdb-yyyy-mm-dd-hhmm-ss-basedata.sql
    spkdb-yyyy-mm-dd-hhmm-ss-userdata.sql

    To omit the prefix, use the --noprefix argument.

=head2 ARGUMENTS

    --noprefix          - do not prefix the output file names with database, date and time
    --nouserdata        - do not output a userdata file
    --spktest           - dump spktest database instead of spkdb

=head2 DEPENDENCIES

    The program depends on a MySQL utility, mysqldump, to do the hard work.

=head1 BUGS

    As stated above, there may be inconsistencies in the output if transactions
    are in progress at the time the dump is taken.

=cut

my $mysqldump = "/usr/bin/mysqldump";

my $db = "spkdb";
my $dbhost = "dbserver";
my $dbuser = "reader";
my $dbpass = "reader";

my $usage = "usage: dump_spkdb.pl  [--[no]prefix] [--[no]userdata] [--spktest]";


my ($sec, $min, $hour, $mday, $mon, $year) = localtime;
my $date = sprintf "%04d-%02d-%02d-%02d%02d-%02d", $year+1900, $mon+1, $mday, $hour, $min, $sec;

my %opt = ();
GetOptions (\%opt, 'prefix!', 'userdata!', 'spktest') 
    or die "$usage\n";
my $prefixed_file_names = 1;
$prefixed_file_names = $opt{'prefix'} if (defined $opt{'prefix'});
my $userdata = 1;
$userdata = $opt{'userdata'} if (defined $opt{'userdata'});
$db = "spktest" if (defined $opt{'spktest'});

my $prefix = $prefixed_file_names ? "$db-$date-" : "";
my $schema_name   = "${prefix}schema.sql";
my $basedata_name = "${prefix}basedata.sql";
my $userdata_name = "${prefix}userdata.sql";

system("$mysqldump -h$dbhost -u$dbuser -p$dbpass -d $db --single-transaction --hex-blob > $schema_name") == 0
    or die "Could not dump the schema\n";
system("$mysqldump -h$dbhost -u$dbuser -p$dbpass -tc $db --single-transaction --hex-blob --tables class end method state > $basedata_name") == 0
    or die "Could not dump the basedata tables\n";
if ($userdata) {
    system("$mysqldump -h$dbhost -u$dbuser -p$dbpass -tc $db --single-transaction --hex-blob --tables dataset history job model user team email > $userdata_name") == 0
	or die "Could not dump the userdata tables\n";
}



