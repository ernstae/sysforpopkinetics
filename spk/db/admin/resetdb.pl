#!/usr/bin/perl -w

use strict;

use Spkdb ('connect', 'disconnect', 'new_job', 'new_user');

@ARGV == 1
    or die("usage: resetdb.pl sql-file\n");

my $schema = shift;

my $dbname = "spktest";
my $dbuser = "tester";
my $dbpass = "tester";

my $tmp_name = "/tmp/junk$$";
my $admin = ".";
my $drop   = "$admin/drop.sql";
system "echo 'use $dbname;' > $tmp_name";
system "cat $tmp_name $drop   | mysql --force -p$dbpass -u$dbuser > /dev/null 2>&1";
system "cat $tmp_name $schema | mysql --force -p$dbpass -u$dbuser";
system "rm $tmp_name";

exit 0;
