#!/usr/bin/perl -w

use strict;

=head1 NAME
    
    dump_spkdb.pl -- take a symbolic dump of the production database

=head1 SYNOPSIS

    dump_spkdb.pl

=head1 ABSTRACT

    This program takes a symbolic dump of the production database, so that
    the a clone of the database can be constructed in the test database 
    usiing load_spktest.pl.

    Warning: the output of this program may contain inconsistencies if a transaction
    was in progress at the time the program was run.  The output should not, therefore,
    be relied upon for rebuilding the production database in case it is damaged.
    The principal use of this program is to create test databases as clones of 
    the production database, using the program load_spktest.pl

=head1 SYNOPSIS

    dump_spkdb.pl

=head1 DESCRIPTION

    This program is designed to be run from the database server. It is not necessary
    for the user have root privileges.  It does not alter the production database
    in any way.  It produces its output in the current directory.

=head2 OUTPUT FILE CONTENTS

    The output of this program consists of three files of sql statements:
    - schema:   'create table' statements
    - basedata: 'insert' statements for inserting all data into the non-transactional
                table; namely, end, method, and state
    - userdata: 'insert' statements for inserting all data into the rest of the tables

=head2 OUTPUT FILE NAMES

    The output files have names with the following pattern:

    spkdb-yyyy-mm-dd-hhmm-ss-schema.sql
    spkdb-yyyy-mm-dd-hhmm-ss-basedata.sql
    spkdb-yyyy-mm-dd-hhmm-ss-userdata.sql

=head2 DEPENDENCIES

    The program depends on a MySQL utility, mysqldump, to do the hard work.

=head1 BUGS

    As stated above, there may be inconsistencies in the output if transactions
    are in progress at the time the dump is taken.

=cut

my $mysqldump = "/usr/bin/mysqldump";
my $user = "tester";
my $pass = "tester";
#my $db = "spkdb";
my $db = "spktest";

my ($sec, $min, $hour, $mday, $mon, $year) = localtime;
my $date = sprintf "%04d-%02d-%02d-%02d%02d-%02d", $year+1900, $mon+1, $mday, $hour, $min, $sec;

my $prefix = "$db-$date";
my $schema   = "$prefix-schema.sql";
my $basedata = "$prefix-basedata.sql";
my $userdata = "$prefix-userdata.sql";

system("mysqldump -u$user -p$pass  -d $db > $schema") == 0
    or die "Could not dump the schema\n";
system("mysqldump -u$user -p$pass -tc $db --tables end method state > $basedata") == 0
    or die "Could not dump the basedata tables\n";
system("mysqldump -u$user -p$pass -tc $db --tables dataset history job model user > $userdata") == 0
    or die "Could not dump the userdata tables\n";



