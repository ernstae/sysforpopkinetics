#!/usr/bin/perl -w

use strict;

use Test::Simple tests => 42;  # number of ok() tests

use Spkdb (
    'connect', 'disconnect', 'new_job', 'job_status', 
    'de_q2c', 'en_q2c',
    'en_q2r', 'de_q2r', 'end_job', 'job_report',
    'new_dataset', 'get_dataset', 'update_dataset', 'user_datasets',
    'new_model', 'get_model', 'update_model', 'user_models',
    'new_user', 'update_user', 'get_user' 
	   );

my $rv;



my $tmp_name = "/tmp/junk$$";
my $admin = "../../admin";
my $schema = "$admin/schema.sql";
my $drop   = "$admin/drop.sql";
system "echo 'use spktest;' > $tmp_name";
system "cat $tmp_name $drop   | mysql --force -ptester -utester > /dev/null 2>&1";
system "cat $tmp_name $schema | mysql --force -ptester -utester";

my $dbh = &connect("spktest", "localhost", "tester", "tester");

ok($dbh, "connect");

my $username = "mjordan";
my $password = "codered";
my $first_name = "mikey";
my $surname = "Jordan";

my $user_id = &new_user($dbh,
			"password",   $password,
			"first_name", $first_name,
			"surname",    $surname);
ok(! $user_id, "new_user, no username specified");

$user_id = &new_user($dbh,
			"username",   $username,
			"first_name", $first_name,
			"surname",    $surname);
ok(! $user_id, "new_user, no password specified");

$user_id = &new_user($dbh,
			"username",   $username,
			"password",   $password,
			"first_name", $first_name,
			"surname",    $surname);
ok($user_id, "new_user");

$rv = &new_user($dbh,
			"username",   $username,
			"password",   $password,
			"first_name", $first_name,
			"surname",    $surname);
ok(! $rv, "new_user, duplicate entry");

$first_name = "George";
$rv = &update_user($dbh,
			  "user_id", $user_id,
			  "first_name", $first_name );
ok($rv, "update_user");
$rv = &update_user($dbh,
			  "user_id", $user_id,
			  "username", $username );
ok(!$rv, "update_user, invalid attempt to change username");

$rv = &update_user($dbh, "username", $username);
ok(!$rv, "update_user, where user user_id is not specified");

my $row = &get_user($dbh, $username);
ok($row && $row->{"first_name"} =~ /^$first_name$/, "get_user");
			 
my $job_id = &new_job($dbh, $user_id, "Job 1", 22, "1.01", 33, "2.2", "source");
ok($job_id, "new_job");

$row = &job_status($dbh, $job_id);
ok($row && $row->{"state_code"} =~ /^q2c$/, "job_status");

sleep(1);			 
&new_job($dbh, $user_id, "Job 2", 33, "1.01", 55, "2.22", "source");
sleep(1);
my $newest_job_id = &new_job($dbh, $user_id, "Job 3", "88", "4.01", 22, "1.11", "source");

my $row_array = &Spkdb::user_jobs($dbh,  $user_id, 1);
$row = $row_array->[0];
ok($row && $row->{"job_id"} == $newest_job_id, "user_jobs, maxnum = 1");

$row_array = &Spkdb::user_jobs($dbh,  $user_id, 3);
my $flag = 1;
foreach $row (@$row_array) {
    if ($row->{"job_id"} != $newest_job_id--) {
	$flag = 0;
	last;
    }
}
ok($flag, "user_jobs for maxnum = 3");

$row = &de_q2c($dbh);
ok($row && $row->{"job_id"} == $job_id
             && $row->{"xml_source"},       "de_q2c, first job");
sleep(1);
$row = &de_q2c($dbh);
ok($row && $row->{"job_id"} == $job_id + 1
             && $row->{"xml_source"},       "de_q2c, second job");

ok(&en_q2r($dbh, $job_id + 1, "1st compiled"), "en_2qr");

sleep(1);
&en_q2r($dbh, $job_id, "2nd compiled");
sleep(1);
$row = &de_q2r($dbh);
ok($row && $row->{"job_id"} == $job_id + 1, "de_q2r");

sleep(1);
ok(&end_job($dbh, $job_id + 1, "srun", "end report"), "end_job");

ok(&end_job($dbh, $job_id, "abcd", "end report") == 0,
   "end_job, with invalid end code");

my $report = &job_report($dbh, $job_id + 1);
ok($report && $report =~ /^end report$/, "end_report");

ok(! &job_report($dbh, $job_id) && $Spkdb::err == $Spkdb::NOT_ENDED,
   "end_report for a job not at end");

ok(! &get_dataset($dbh, $user_id, "Dataset-T") && $Spkdb::err == 0,
   "get_dataset when none exists");

ok(&new_dataset($dbh, $user_id, "Dataset-T", "Good dataset", "dataset T text"),
   "new_dataset");
ok(! &new_dataset($dbh, $user_id, "Dataset-T", 'Good dataset', 'dataset T text'),
   "new_dataset duplicate add");

$row = &get_dataset($dbh, $user_id, "Dataset-T");
ok($row, "get_dataset");

ok(&update_dataset($dbh,
		 "dataset_id", $row->{'dataset_id'}, 
		 "abstract", "new_abstract"), "update_dataset");
ok(!&update_dataset($dbh,
		 "dataset_id", $row->{'dataset_id'}, 
		 "name", "new_name"), "update_dataset, attempt to change name");
ok(!&update_dataset($dbh,
		 "abstract", "new_abstract"), "update_dataset, no dataset_id");

my $dataset_id = &new_dataset($dbh,
			  $user_id, "Dataset-A", "Better dataset", "dataset A text");

ok($dataset_id, "another new_dataset");

$row_array = &Spkdb::user_datasets($dbh,  $user_id);
$flag = 1;
foreach $row (@$row_array) {
    if ($row->{"dataset_id"} != $dataset_id--) {
	$flag = 0;
	last;
    }
}
ok($flag, "user_datasets");

ok($row_array->[1]->{'name'} =~ /^Dataset-T$/, "user_datasets, sorting");

ok(! &get_model($dbh, $user_id, "Model-T") && $Spkdb::err == 0,
   "get_model when none exists");

ok(&new_model($dbh, $user_id, "Model-T", "Good model", "model T text"),
   "new_model");
ok(! &new_model($dbh, $user_id, "Model-T", 'Good model', 'model T text'),
   "new_model duplicate add");

$row = &get_model($dbh, $user_id, "Model-T");
ok($row, "get_model");

ok(&update_model($dbh,
		 "model_id", $row->{'model_id'}, 
		 "abstract", "new_abstract"), "update_model");
ok(!&update_model($dbh,
		 "model_id", $row->{'model_id'}, 
		 "name", "new_name"), "update_model, attempt to change name");
ok(!&update_model($dbh,
		 "abstract", "new_abstract"), "update_model, no model_id");

my $model_id = &new_model($dbh,
			  $user_id, "Model-A", "Better model", "model A text");

ok($model_id, "another new_model");

$row_array = &Spkdb::user_models($dbh,  $user_id);
$flag = 1;
foreach $row (@$row_array) {
    if ($row->{"model_id"} != $model_id--) {
	$flag = 0;
	last;
    }
}
ok($flag, "user_models");

ok($row_array->[1]->{'name'} =~ /^Model-T$/, "user_models, sorting");


ok(&disconnect($dbh), "disconnect");			 

exit 0;
