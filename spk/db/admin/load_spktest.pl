#!/usr/bin/perl -w

use strict;
use Getopt::Long;

my $usage = 
    "usage: $0 --schema name1 --basedata name2 --userdata name3\n"
    . "where the optional arguments override the folowing defaults:\n"
    . "\tschema.sql\n"
    . "\tbasedata.sql\n"
    . "\tuserdata.sql\n";

my $tmp_name = "/tmp/junk$$";
my $mysqldump = "/usr/bin/mysqldump";
my $user = "tester";
my $pass = "tester";
my $dbname = "spktest";

my %file = ();
%file = (
	   schema   => 'schema.sql',
	   basedata => 'basedata.sql',
	   userdata => 'userdata.sql',
	  );
my %opt = ();
GetOptions (\%opt, 'schema=s', 'basedata=s', 'userdata=s');

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

open FD, $tmp_name
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
system "cat $tmp_name $file{schema}   | mysql --force -p$dbpass -u$dbuser";
system "cat $tmp_name $file{basedata} | mysql --force -p$dbpass -u$dbuser";
system "cat $tmp_name $file{userdata} | mysql --force -p$dbpass -u$dbuser";

system "rm $tmp_name";



