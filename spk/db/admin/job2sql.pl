#!/usr/bin/perl -w

use strict;
use bytes;

use Spkdb('connect', 'disconnect', 'de_q2c', 'en_q2r', 'end_job',
	  'get_cmp_jobs', 'get_dataset', 'email_for_job');


my job_id = shift;

my $database = "spkdb";
my $host     = "dbserver";
my $dbuser   = "selector";
my $dbpasswd = "selector";

my $dbh = &connect($database, $host, $dbuser, $dbpasswd)
	or die("can't connect to database=$database, host=$host");



