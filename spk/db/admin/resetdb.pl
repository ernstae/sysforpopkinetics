#!/usr/bin/perl -w

use strict;

use Spkdb ('connect', 'disconnect', 'new_job', 'new_user');

@ARGV == 1
    or die("usage: resetdb.pl data-only-sql-file\n");

my $data = shift;

my $dbname = "spktest";
my $dbuser = "tester";
my $dbpass = "tester";

my $tmp_name = "/tmp/junk$$";
my $admin = ".";
my $drop   = "$admin/drop.sql";
my $schema = "$admin/schema.sql";

system "echo 'use $dbname;' > $tmp_name";
system "cat $tmp_name $drop   | mysql --force -p$dbpass -u$dbuser > /dev/null 2>&1";
system "cat $tmp_name $schema | mysql --force -p$dbpass -u$dbuser";
system "cat $tmp_name $data   | mysql --force -p$dbpass -u$dbuser";

system "rm $tmp_name";

exit 0;
