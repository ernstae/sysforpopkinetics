#!/usr/bin/perl -w

use strict;

use Spkdb ('connect', 'disconnect', 'new_job', 'new_user');

my $dbname = "spktest";
my $dbuser = "tester";
my $dbpass = "tester";

my $tmp_name = "/tmp/junk$$";
my $admin = "../../../spk/db/admin";
my $schema = "$admin/schema_2.sql";
my $drop   = "$admin/drop.sql";
system "echo 'use $dbname;' > $tmp_name";
system "cat $tmp_name $drop   | mysql --force -h dbserver -p$dbpass -u$dbuser";
system "cat $tmp_name $schema | mysql --force -h dbserver -p$dbpass -u$dbuser";
system "rm $tmp_name";

exit 0;
