#!/usr/bin/perl -w

use strict;

use Test::Simple tests => 66;  # number of ok() tests

use Spkdb (
    'connect', 'disconnect', 'new_job', 'job_status', 
    'de_q2c', 'set_state_code', 'set_parallel', 'de_q2ac', 'de_q2ar', 'get_q2c_job', 'get_q2r_job', 'get_job_ids',
    'en_q2r', 'de_q2r', 'get_job', 'end_job', 'job_report', 'job_checkpoint', 'job_history',
    'new_dataset', 'get_dataset', 'update_dataset', 'user_datasets',
    'new_model', 'get_model', 'update_model', 'user_models',
    'new_user', 'update_user', 'get_user', 'set_mail_notice', 'get_mail_notice', 'email_for_job'
	   );
my $rv;

my $tmp_name = "/tmp/junk$$";
my $admin = "../../admin";
my $schema = "$admin/schema.sql";
my $data = "$admin/data.sql";
my $drop   = "$admin/drop.sql";
system "load_spktest.pl --schema ../schema.sql --basedata ../basedata.sql --userdata ../userdata.sql";

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
			"password",   "very-secret",
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
			  "first_name", $first_name,
		          "password", $password);
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

$row = &Spkdb::get_job($dbh, 2);
ok($row && $row->{"job_id"} == 2, "get_job");

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

my $email = &email_for_job($dbh, 1);
ok(!defined $email, "email address was never defined");

my $email_addr = "mj\@u.washington.edu";
&update_user($dbh, "user_id", 1, "email", $email_addr);

$email = &email_for_job($dbh, 1);
ok($email =~ $email_addr, "email_for_job");

my $mail_notice = &Spkdb::get_mail_notice($dbh, 1);
ok($mail_notice == 0, "mail notice is not requested");

&Spkdb::set_mail_notice($dbh, 1, 1);
$mail_notice = &Spkdb::get_mail_notice($dbh, 1);
ok($mail_notice == 1, "mail notice is requested");

$row = &de_q2c($dbh);
ok($row && $row->{"job_id"} == $job_id
             && $row->{"xml_source"},       "de_q2c, first job");
sleep(1);
$row = &de_q2c($dbh);
ok($row && $row->{"job_id"} == $job_id + 1
             && $row->{"xml_source"},       "de_q2c, second job");
$row = &get_q2c_job($dbh, $job_id);
ok($row && $row->{"xml_source"} eq "source",       "get_q2c_job");
my $jobId;
&set_state_code($dbh, $job_id, "q2ac");
sleep(1);
&set_state_code($dbh, $job_id + 1, "q2ac");
$jobId = &de_q2ac($dbh);
ok($jobId && $jobId == $job_id,             "de_q2ac, first job");
sleep(1);
$jobId = &de_q2ac($dbh);
ok($jobId && $jobId == $job_id + 1,         "de_q2ac, second job");

&set_state_code($dbh, $job_id, "q2ar");
sleep(1);
&set_state_code($dbh, $job_id + 1, "q2ar");
$jobId = &de_q2ar($dbh);
ok($jobId && $jobId == $job_id,             "de_q2ar, first job,$jobId,  ,$job_id");
sleep(1);
$jobId = &de_q2ar($dbh);
ok($jobId && $jobId == $job_id + 1,         "de_q2ar, second job,$jobId,  ,$job_id");

&set_state_code($dbh, $job_id, "q2c");
&set_state_code($dbh, $job_id + 1, "q2c");

$row_array = &Spkdb::get_cmp_jobs($dbh);
my $j_id = $job_id;
$flag = 1;
foreach $row (@$row_array) {
    if ($row->{"job_id"} != $j_id++) {
	$flag = 0;
    }
}
ok($flag, "get_cmp_jobs");

ok(&en_q2r($dbh, $job_id + 1, "1st compiled"), "en_2qr");

sleep(1);
&en_q2r($dbh, $job_id, "2nd compiled");
sleep(1);
$row = &de_q2r($dbh);
ok($row && $row->{"job_id"} == $job_id + 1, "de_q2r");
$row = &de_q2r($dbh);
ok($row && $row->{"job_id"} == $job_id, "de_q2r");
&set_parallel($dbh, $job_id + 1, 1);
$row = &get_q2r_job($dbh, $job_id + 1);
ok($row && $row->{"cpp_source"} eq "1st compiled", "get_q2r_job, source");
ok($row && $row->{"parallel"} == 1, "get_q2r_job, parallel");
$row_array = &Spkdb::get_run_jobs($dbh);
$j_id = $job_id;
$flag = 1;
foreach $row (@$row_array) {
    if ($row->{"job_id"} != $j_id++) {
	$flag = 0;
    }
}
ok($flag, "get_run_jobs");

sleep(1);
ok(&end_job($dbh, $job_id + 1, "srun", "end report", "end checkpoint"), "end_job");

ok(&end_job($dbh, $job_id, "abcd", "end report", "end checkpoint") == 0,
   "end_job, with invalid end code");

$row = &get_job($dbh, $job_id);
#print "delta = ", $row->{'event_time'} - $row->{'start_time'}, "\n";
ok($row->{'event_time'} - $row->{'start_time'} >= 4, "job time difference");

my $report = &job_report($dbh, $job_id + 1);
ok($report && $report =~ /^end report$/, "job_report");

ok(! &job_report($dbh, $job_id) && $Spkdb::err == $Spkdb::NOT_ENDED,
   "job_report for a job not at end");

my $checkpoint = &job_checkpoint($dbh, $job_id + 1);
ok($checkpoint && $checkpoint =~ /^end checkpoint$/, "job_checkpoint");

ok(! &job_checkpoint($dbh, $job_id) && $Spkdb::err == $Spkdb::NOT_ENDED,
   "job_checkpoint for a job not at end");

ok(! &get_dataset($dbh, 55) && $Spkdb::err == 0,
   "get_dataset when none exists");

ok(&new_dataset($dbh, $user_id, "Dataset-T", "Good dataset", "dataset T text"),
   "new_dataset");
ok(! &new_dataset($dbh, $user_id, "Dataset-T", 'Good dataset', 'dataset T text'),
   "new_dataset duplicate add");
ok(&new_dataset($dbh, $user_id, "Dataset-R", "Fine dataset", "dataset R text"),
   "newer_dataset");

$row_array = &Spkdb::user_datasets($dbh, $user_id);
my $d_id = 2;
$flag = 1;
foreach $row (@$row_array) {
    if ($row->{"dataset_id"} != $d_id--) {
	$flag = 0;
    }
}
ok($flag, "user_datasets");

$row = &get_dataset($dbh, 2);
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

ok($flag, "user_datasets");

ok($row_array->[1]->{'name'} =~ /^Dataset-T$/, "user_datasets, sorting");


ok(! &get_model($dbh, 55) && $Spkdb::err == 0,
   "get_model when none exists");

ok(&new_model($dbh, $user_id, "Model-T", "Good model", "model T text"),
   "new_model");
ok(! &new_model($dbh, $user_id, "Model-T", 'Good model', 'model T text'),
   "new_model duplicate add");
ok(&new_model($dbh, $user_id, "Model-R", "Fine model", "model R text"),
   "newer_model");

$row_array = &Spkdb::user_models($dbh, $user_id);
$d_id = 2;
$flag = 1;
foreach $row (@$row_array) {
    if ($row->{"model_id"} != $d_id--) {
	$flag = 0;
    }
}
ok($flag, "user_models");

$row = &get_model($dbh, 2);
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

ok($flag, "user_models");

ok($row_array->[1]->{'name'} =~ /^Model-T$/, "user_models, sorting");

&set_state_code($dbh, $job_id, "acmp");
&set_state_code($dbh, $job_id + 1, "acmp");
my @jobs = &get_job_ids($dbh, "acmp");
ok(@jobs && $jobs[0] == 1 && $jobs[1] == 2, "get_job_ids");

$row_array = &job_history($dbh, 2);
ok (@$row_array == 12, "job_history");

ok(!defined &disconnect($dbh), "disconnect");			 

exit 0;
