#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use DBI;

=head1 NAME

    load_spktest.pl -- build a test database from a dump of the production database

=head1 SYNOPSIS

    load_spktest.pl [--schema name1] [--basedata name2] [--userdata name3] \
                    [ --modscript name4 \
                    [ --database name5] \
                    [--host name6] \
                    [ --user name7[ [--password --name8]

=head1 ABSTRACT 

    From a symbolic dump made previously from the production database,
    this program creates a test database.

=head1 DESCRIPTION

    On localhost, in the database spktest, this program does the following:

    1. Drops all tables
    2. Recreates the tables, using sql statements in the schema input file.
    3. Inserts non-transactional data into tables end, method and state, using sql
       statements in the basedata input file
    4. Inserts transactional data into the other tables, using sql statements in
       the userdata input files

=head2 DEFAULT FILE NAME

    The following default files are used:

    schema:   schema.sql
    basedata: basedata.sql
    userdata: userdata.sql

    In each of the three cases, either a file of that name must exist
    in the current directory, or the pathname of an existing file must
    be given in the optional corresponding command-line argument.

    In addition, the following file is used 

  modscript:  modscript.sql

    but only if it exists in the current directory or if the pathname of
    an existing file is given in the optional "modscript" command-line
    argument.  If neither the default modscript exists, nor a modscript
    is specified on the command-line, no modscript will be used.

=head2 OPTIONAL FILE NAMES

    If present on the command line, the following options override the
    default file names.  The user can provide no optional arguments,
    or one, two or three of the following, in any order:

    --schema name1
    --basedata name2
    --userdata name3

=head2 OPTIONAL DATABASE 

    By default, the program builds the database spktest.  The
    --database option allows another database to be built.

=head2 OPTIONAL HOST NAME

    By default, the program assumes that the database is on localhost.
    The --host option allows another host to be designated.

=head2 OPTIONAL USER AND PASSWORD

    The program has a default user and password for building spktest.
    The --user and --password options make it possible to use a
    different user/password pair.  In particular, this may be
    necessary if the --database option is used.

=head2 DEPENDENCIES

    The program depends on a MySQL utility, mysql, to do the hard
    work.

=cut

my $usage = 
    "usage: $0 [--schema name1] [--basedata name2] [--userdata name3]\n"
    . "\t[--modscript name4]\n"
    . "\t[--host name5 --database name6]\n"
    . "\t[--user name7 --password name8]\n"
    . "where the optional arguments override the folowing defaults:\n"
    . "\tschema.sql\n"
    . "\tbasedata.sql\n"
    . "\tuserdata.sql\n"
    . "\tmodscript.sql\n"
    . "\tlocalhost\n"
    . "\tspktest\n"
    . "\ttester\n"
    . "\ttester\n";

my $tmp_name = "/tmp/junk$$";
my $mysqldump = "/usr/bin/mysqldump";
my $dbuser = "tester";
my $dbpass = "tester";
my $dbname = "spktest";
my $dbhost = "localhost";
my $dbd    = "mysql";

my %file = ();
%file = (
	 schema    => 'schema.sql',
	 basedata  => 'basedata.sql',
	 userdata  => 'userdata.sql',
	 modscript => 'modscript.sql',
	  );
my %opt = ();
GetOptions (\%opt, 'schema=s', 'basedata=s', 'userdata=s', 'modscript=s', 
	    'host=s', 'database=s', 'user=s', 'password=s');

for my $f (keys %file) {
    if (defined $opt{$f}) {
	print "For '$f', using '$opt{$f}', which you supplied as an argument\n";
	$file{$f} = $opt{$f};
    } 
    elsif ($f =~ /modscript/ && ! -f $file{$f}) {
	next;
    }
    else {
	print "For '$f', using './$file{$f}', which is the default\n";
    }
    -f $file{$f} or die "Oops! File '$file{$f}' does not exist.\n$usage";
}
$dbname = $opt{'database'} if (defined $opt{'database'});
$dbhost = $opt{'host'}     if (defined $opt{'host'});
$dbuser = $opt{'user'}     if (defined $opt{'user'});
$dbpass = $opt{'password'} if (defined $opt{'password'});

my $dbh = DBI->connect("dbi:$dbd:$dbname:$dbhost", $dbuser, $dbpass)
    or die("Could not connect to database $dbname\n");

my $sql = "show tables;";
my $sth = $dbh->prepare($sql)
    or die("prepare($sql) failes\n");
$sth->execute()
    or die("could not execute ($sql)\n");

print "Building database '$dbname', on host $dbhost, with user '$dbuser'\n";


#  create a drop statement for each table in the database

my @row;
open FD, ">$tmp_name"
    or die "Can't open $tmp_name\n";
print FD "use $dbname;\n";
while (@row = $sth->fetchrow_array) {
    print FD "drop table $row[0];\n";
}
close FD;
$sth->finish();
$dbh->disconnect;

system "cat $tmp_name                 | mysql --force -h$dbhost -p$dbpass -u$dbuser > /dev/null 2>&1";
system "echo 'use $dbname;' > $tmp_name";
system "cat $tmp_name $file{schema}   | mysql --force -h$dbhost -p$dbpass -u$dbuser";
system "cat $tmp_name $file{basedata} | mysql --force -h$dbhost -p$dbpass -u$dbuser";
system "cat $tmp_name $file{userdata} | mysql --force -h$dbhost -p$dbpass -u$dbuser";
system "cat $tmp_name $file{modscript}| mysql --force -h$dbhost -p$dbpass -u$dbuser"
    if (defined $file{modscript} && -f $file{modscript});
system "rm $tmp_name";



