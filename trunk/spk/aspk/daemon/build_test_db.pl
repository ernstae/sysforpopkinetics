#!/usr/bin/perl -w

use strict;

use Spkdb ('connect', 'disconnect', 'new_job', 'new_user');

my $rv;

my $tmp_name = "/tmp/junk$$";
my $admin = "../../../spk/db/admin";
my $schema = "$admin/schema.sql";
my $drop   = "$admin/drop.sql";
system "echo 'use spktest;' > $tmp_name";
system "cat $tmp_name $drop   | mysql --force -ptester -utester > /dev/null 2>&1";
system "cat $tmp_name $schema | mysql --force -ptester -utester";

my $dbh = &connect("spktest", "localhost", "tester", "tester");

my $username = "mjordan";
my $password = "codered";
my $first_name = "mikey";
my $surname = "Jordan";


my $user_id = &new_user($dbh,
			"username",   $username,
			"password",   "very-secret",
			"first_name", $first_name,
			"surname",    $surname);
			 
&new_job($dbh, $user_id, "Job 1", 11, "1.01", 11, "1.1", "source");
&new_job($dbh, $user_id, "Job 2", 22, "2.02", 22, "2.2", "source");
&new_job($dbh, $user_id, "Job 3", 33, "3.03", 33, "3.3", "source");
&new_job($dbh, $user_id, "Job 4", 44, "4.04", 44, "4.4", "source");
&new_job($dbh, $user_id, "Job 5", 55, "5.05", 55, "5.5", "source");

&disconnect($dbh);

exit 0;
