#!/usr/bin/perl -w

use strict;
use Getopt::Long;

=head1 NAME

    load_spktest.pl -- build a test database from a dump of the production database

=head1 SYNOPSIS

    load_spktest.pl --schema name1 --basedata name2 --userdata name3 --database name4

=head1 ABSTRACT
    
    From a symbolic dump made previously from the production database, this program
    creates a test database. 

=head1 DESCRIPTION

    On localhost, in the database spktest, this program does the following:

    1. Drops all tables
    2. Recreates the tables, using sql statements in the schema input file.
    3. Inserts non-transactional data into tables end, method and state, using sql
       statements in the basedata input file
    4. Inserts transactional data into the other tables, using sql statements in
       the userdata input files

=head2 DEFAULT FILE NAME

    The following default filenames are used:

    schema:   schema.sql
    basedata: basedata.sql
    userdata: userdata.sql

=head2 OPTIONAL FILE NAMES

    If present on the command line, the following options override the default
    file names.  The user can provide no optional arguments, or one, two or
    three of the following, in any order:

    --schema name1
    --basedata name2
    --userdata name3

=head2 OPTIONAL DATABASE
    
    By default, the program builds the database spktest.  The --database option
    allows another database to be built.

=head2 OPTIONAL USER AND PASSWORD

    The program has a default user and password for building spktest.  The
    --user and --password options make it possible to use a different
    user/password pair.  In particular, this may be necessary if the --database
    option is used.

=head2 DEPENDENCIES

    The program depends on a MySQL utility, mysql, to do the hard work.

=cut

my $usage = 
    "usage: $0 --schema name1 --basedata name2 --userdata name3 --database name4\n"
    . "\t--user name5 --password name6\n"
    . "where the optional arguments override the folowing defaults:\n"
    . "\tschema.sql\n"
    . "\tbasedata.sql\n"
    . "\tuserdata.sql\n";

my $tmp_name = "/tmp/junk$$";
my $mysqldump = "/usr/bin/mysqldump";
my $dbuser = "tester";
my $dbpass = "tester";
my $dbname = "spktest";

my %file = ();
%file = (
	   schema   => 'schema.sql',
	   basedata => 'basedata.sql',
	   userdata => 'userdata.sql',
	  );
my %opt = ();
GetOptions (\%opt, 'schema=s', 'basedata=s', 'userdata=s', 'database=s', 'user=s', 'password=s');

for my $f (keys %file) {
    if (defined $opt{$f}) {
	print "For '$f', using '$opt{$f}', which you supplied as an argument\n";
	$file{$f} = $opt{$f};
    } 
    else {
	print "For '$f', using './$file{$f}', which is the default\n";
    }
    -f $file{$f} or die "Oops! File '$file{$f}' does not exist.\n$usage";
}
$dbname = $opt{'database'} if (defined $opt{'database'});
$dbuser = $opt{'user'}     if (defined $opt{'user'});
$dbpass = $opt{'password'} if (defined $opt{'password'});

print "Building database '$dbname', with user '$dbuser'\n";

open FD, ">$tmp_name"
    or die "Can't open $tmp_name\n";
print FD "use $dbname;\n";
print FD "drop table dataset;\n";
print FD "drop table end;\n";
print FD "drop table history;\n";
print FD "drop table job;\n";
print FD "drop table method;\n";
print FD "drop table model;\n";
print FD "drop table state;\n";
print FD "drop table user;\n";
close FD;

system "cat $tmp_name                 | mysql --force -p$dbpass -u$dbuser > /dev/null 2>&1";
system "echo 'use $dbname;' > $tmp_name";
system "cat $tmp_name $file{schema}   | mysql --force -p$dbpass -u$dbuser";
system "cat $tmp_name $file{basedata} | mysql --force -p$dbpass -u$dbuser";
system "cat $tmp_name $file{userdata} | mysql --force -p$dbpass -u$dbuser";

system "rm $tmp_name";



