#!/usr/bin/perl -w

########################################################################
#
# This file is part of the System for Population Kinetics (SPK), which
# was developed with support from NIH grants RR-12609 and P41-
# EB001975. Please cite these grants in any publication for which this
# software is used and send a notification to the address given above.
#
# SPK is Copyright (C) 1998-2003, by the University of Washington,
# Resource Facility for Population Kinetics, and is made available as
# free open source software under the terms of the University of
# Washington Free-Fork License as a public service.  A copy of the
# License can be found in the COPYING file in the root directory of this
# distribution or can be obtained from
#     Resource Facility for Population Kinetics
#     Department of Bioengineering Box 352255
#     University of Washington
#     Seattle, WA 98195-2255
########################################################################

package Spkdb;

use 5.008;
use strict;
use warnings;
use Sys::Hostname;

use Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = (
    'connect', 'disconnect', 'new_job', 'get_job', 'job_status', 'user_jobs', 'set_state_code', 'set_parallel',
    'de_q2c', 'de_q2ac', 'get_q2c_job', 'get_job_ids', 'get_cmp_jobs', 'get_run_jobs', 'en_q2r', 
    'de_q2r', 'de_q2ar', 'get_q2r_job', 'end_job', 'job_report', 'job_checkpoint', 'job_history',
    'new_dataset', 'get_dataset', 'update_dataset', 'user_datasets',
    'new_model', 'get_model', 'update_model', 'user_models',
    'new_user', 'update_user', 'get_user', 'set_mail_notice', 'get_mail_notice', 'email_for_job'
    );

our $VERSION = '0.01';

=head1 NAME

Spkdb - Perl Binding for the Spk Database API

=head1 SYNOPSIS

  use Spkdb;
  
  See individual function descriptions.

=head2 EXPORT

None by default.  All subroutines names may be imported.

=head1 ABSTRACT

Module Spkdb provides an Application Programming Interface (API)
to the Spk Database.  It accesses the database via the 
Perl modules DBI and DBD::Mysql.  This module is simply a collection
of Perl subroutines which correspond to the functions defined
in the Spk Database API Specification.

=head1 DESCRIPTION

A short description of the usage of each of the subroutines 
follows.

=cut

use DBI;

our $errstr = " ";
our $err = 0;

our $USER_EXISTS = 1;
our $SQL_ERROR = 2;
our $INSERT_FAILED = 3;
our $COLUMN_REQUIRED = 4;
our $KEY_REQUIRED = 5;
our $UPDATE_FAILED = 6;
our $USERNAME_REQUIRED = 7;
our $GET_FAILED = 8;
our $PREPARE_FAILED = 9;
our $EXECUTE_FAILED = 10;
our $INVALID_END = 11;
our $CANNOT_CONNECT = 12;
our $NOT_ENDED = 13;
our $MODEL_EXISTS = 14;
our $TOO_MANY = 15;
our $INVALID_CHANGE = 16;
our $DATASET_EXISTS = 17;

=head2 connect -- open a database connection

Establish a connection to the database, returning 
a handle which is a required argument to all other
subroutines in this package:

$dbh = &Spkdb::connect($dbname, $hostname, $username, $password);

$dbname is a string containing the name of an existing database. 

$hostname is a string containing the hostname where the database resides.

$username is a string containing the name of a valid user of Spkdb.

$password is a string containing the password of the given user.

Returns

  success: a valid database handle
  failure: undef

=cut

sub connect() {
    
    my $dbname   = shift;
    my $hostname = shift;
    my $username = shift;
    my $password = shift;
    $err = 0;
    $errstr = "";

    my $dbd      = 'mysql';

    my $dbh = DBI->connect("dbi:$dbd:$dbname:$hostname", $username, $password);
    unless ($dbh) {
	$errstr = "Cannot connect to database $dbname";
	$err = $CANNOT_CONNECT;
	return undef;
    }
    return $dbh;
}

=head2 disconnect -- close a database connection

Close a connection to the database, releasing resources:

    &Spkdb::disconnect($dbh);

$dbh is the handle of an open database connection.

Returns undef.

=cut 


sub disconnect() {
    my $dbh = shift;
    $err = 0;
    $errstr = "";

    $dbh->disconnect();
    return undef;
}

=head2 new_job -- submit a new job

Submit a new job to be compiled and run.

    $job_id = &Spkdb::new_job($dbh,
                              $user_id,
			      $abstract,
			      $dataset_id,
			      $dataset_version,
			      $model_id,
			      $model_version,
			      $xml_source,
			      );
$dbh is the handle to an open database connection.

$user_id is the user_id number of an existing user.

$abstract is a string containing a brief description of the model.

$dataset_id is the dataset_id number of an existing dataset

$dataset_version is the version string of the dataset

$model_id is the model_id number of an existing model.

$model_version is the version string of the model.

$xml_source is the source string.

Returns:

  success: automatically generated value of the job_id primary key
  failure: 0
        $errstr contains an error messages string
        $err == $Spkdb::INSERT_FAILED

=cut

sub new_job() {
    my $dbh             = shift;
    my $user_id         = shift;
    my $abstract        = shift;
    my $dataset_id      = shift;
    my $dataset_version = shift;
    my $model_id        = shift;
    my $model_version   = shift;
    my $r_source        = \$_[0];
    $err = 0;
    $errstr = "";

    my $state_code = "q2c";

    my $start_time = time();
    my $event_time = $start_time;

    my $sql = "insert into job (state_code, user_id, abstract, dataset_id, "
	            .          "dataset_version, model_id, model_version, "
	            .          "xml_source, start_time, event_time)"
		    . " values ('$state_code', $user_id, '$abstract', "
                    .          "$dataset_id, '$dataset_version', "
		    .          "$model_id, '$model_version', '$$r_source', "
		    .          "$start_time, $event_time);";
    unless ($dbh->do($sql)) {
	$err = $INSERT_FAILED;
	$errstr = "can't insert new job";
	return 0;
    }
    my $job_id = $dbh->{'mysql_insertid'};

    &add_to_history($dbh, $job_id, $state_code);

    return $job_id;
}

=head2 job_history -- get event history for this job

Returns a reference to an array of rows from the history table.  Each
row is a reference to a hash table, with the field name as key and field
value as value.  Within the array, rows are sorted in order of occurence.
Each row contains all fields of the history table.

    $array_row = $Spkdb::job_history($dbh, $job_id);

Returns

  success:
    reference to an array of references to hash tables, each 
    representing a row in the history table

  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub job_history() {
    
    my $dbh = shift;
    my $job_id = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select * from history where job_id=$job_id order by history_id;";

    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute statement: $sql; error returned "
	    . $sth->errstr;
	return undef;
    }
    my $array_row_ref = $sth->fetchall_arrayref({});
    $sth->finish;
    return $array_row_ref;
}

=head2 get_job -- retrieve a job for a user

Retrieve the job corresponding to a job_id

    $row = &Spkdb::get_job($dbh, $job_id);

$dbh is the handle to an open database connection

$job_id is the integer which uniquely identifies the job

Returns

  success: reference to a hash table of column/value pairs
           0 if no datatset corresponds to the given job_id
  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub get_job() {
    my $dbh = shift;
    my $job_id = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select * from job where job_id = $job_id;";
    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute state: $sql; error returned "
	    . $sth->errstr;
        return undef;
    }
    my $count = $sth->rows;

    if ($count == 0) {
	return 0;
    }
    unless ($count == 1) {
	$err = $TOO_MANY;
	$errstr = "duplicate job record";
	return undef;
    }
    my $jrow = $sth->fetchrow_hashref();
    $sth->finish;
    return $jrow;
}

=head2 job_status -- return the current state of a job

Return the state_code, event_time, and end_code for a job.  Note that
the event_time is the time of the last job state transistion, such
as the transition from "queued to compile" to "compiling".

    $row = &Spkdb::job_status($dbh, $job_id);

$dbh is the handle to an open database connection.


$job_id is the primary key of the job in question.


Returns

  success: reference to a hash table containing the following
    columns from the selected row of the job table:
        state_code
        event_time
        end_code
    In the hash table, the above field names are the keys and 
    the column values the values.
  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function fail
                == $Spkdb::EXECUTE_FAILED if execute function fails

=cut    

sub job_status() {

    my $dbh = shift;
    my $job_id = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select state_code, event_time, end_code "
	      . "from job where job_id='$job_id';";
    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute state: $sql; error returned "
	    . $sth->errstr;
        return undef;
    }
    my $jrow = $sth->fetchrow_hashref();
    $sth->finish;
    return $jrow;
}

=head2 user_jobs -- get status for the most recent jobs of a user

Returns a reference to an array of rows.  Each row is a reference
to a hash table, with the field name as key and field value as value.
Within the array, jobs are sorted in reverse order of job_id, hence
the most recently submitted job appears first.  The maximum number 
of jobs to return is the third argument.

The fields returned are job_id, abstract, state_code, start_time,
event_time and end_code.

    $array_row = $Spkdb::user_jobs($dbh, $user_id, $maxnum);

Returns

  success:
    reference to an array of references to hash tables, each 
    representing a subset of a row in the job table

  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub user_jobs() {
    
    my $dbh = shift;
    my $user_id = shift;
    my $maxnum = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select job_id, abstract, state_code, start_time, event_time, end_code "
	. "from job where user_id='$user_id' "
	. "order by job_id desc limit $maxnum;";
    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute state: $sql; error returned "
	    . $sth->errstr;
	return undef;
    }
    my $array_row_ref = $sth->fetchall_arrayref({});
    $sth->finish;
    return $array_row_ref;
}

=head2 set_state_code -- set job state_code
 
Set the state_code of a specified job. 

    $r = &Spkdb::set_state_code($dbh, $job_id, $state_code);

$dbh is the handle to an open database connection.

$job_id is the key to the job table

$state_code is the state_code to set.

Returns

  success: 1 if state_code of the job is set, 0 otherwise
  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::UPDATE_FAILED

=cut

sub set_state_code() {
    my $dbh = shift;
    my $job_id = shift;
    my $state_code = shift;
    my $event_time = time();
    $err = 0;
    $errstr = "";

    $dbh->begin_work;

    my $sql = "update job set state_code='$state_code',event_time='$event_time' "
              . "where job_id='$job_id';";

    unless ($dbh->do($sql)) {
	$err = $UPDATE_FAILED;
	$errstr = "could not execute $sql; error returned ";
	$dbh->rollback;
	return undef;
    }
    $dbh->commit;
    &add_to_history($dbh, $job_id, $state_code);
    return 1;
}

=head2 de_q2c -- remove highest priority job from compile queue

Remove the highest priority job from the compiler queue, so that
it can be compiled.

    $row = &Spkdb::de_q2c($dbh);

$dbh is the handle to an open database connection.

Returns

  success: 
    reference to a hash for the row of highest priority, 
    containing the following fields:
        job_id
        dataset_id
        dataset_version
        xml_source
    false if compiler queue is empty   

  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub de_q2c() {
    my $dbh = shift;
    $err = 0;
    $errstr = "";
    
    $dbh->begin_work;

    my $sql = "select job_id, dataset_id, dataset_version, xml_source from job "
	    .       "where state_code='q2c' "
            .       "order by event_time "
            .       "limit 1 "
            .       "for update; ";

    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	$dbh->rollback;
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute $sql; error returned "
	    . $sth->errstr;
	$dbh->rollback;
        return undef;
    }
    my $row = $sth->fetchrow_hashref();
    $sth->finish;
    unless ($row) {
	$dbh->rollback;
	return 0;
    }
    my $job_id = $row->{"job_id"};
    my $state_code = "cmp";
    my $event_time = time();
    
    $sql = "update job set state_code = '$state_code', event_time = '$event_time' "
           . "where job_id=$job_id;";
    unless ($dbh->do($sql)) {
	$err = $UPDATE_FAILED;
	$errstr = "could not execute $sql; error returned ";
	$dbh->rollback;
	return undef;
    }
    $dbh->commit;
    &add_to_history($dbh, $job_id, $state_code);
    return $row;
}

=head2 get_q2c_job -- get a queued-to-compile job from the database

Remove the highest priority job from the compiler queue, so that
it can be compiled.

    $row = &Spkdb::get_q2c_job($dbh, $job_id);

$dbh is the handle to an open database connection.
$job_id is the key to a row in the job table.

Returns

  success: 
    reference to a hash for the row of highest priority, 
    containing the following fields:
        dataset_id
        dataset_version
        xml_source
    false if compiler queue is empty   

  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub get_q2c_job() {
    my $dbh = shift;
    my $job_id = shift;
    $err = 0;
    $errstr = "";
    
    $dbh->begin_work;

    my $sql = "select dataset_id, dataset_version, xml_source from job "
	      . "where job_id='$job_id' for update;";

    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	$dbh->rollback;
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute $sql; error returned "
	    . $sth->errstr;
	$dbh->rollback;
        return undef;
    }
    my $row = $sth->fetchrow_hashref();
    $sth->finish;
    unless ($row) {
	$dbh->rollback;
	return 0;
    }
    my $state_code = "cmp";
    my $event_time = time();
    
    $sql = "update job set state_code = '$state_code', event_time = '$event_time' "
           . "where job_id=$job_id;";
    unless ($dbh->do($sql)) {
	$err = $UPDATE_FAILED;
	$errstr = "could not execute $sql; error returned ";
	$dbh->rollback;
	return undef;
    }
    $dbh->commit;
    &add_to_history($dbh, $job_id, $state_code);
    return $row;
}
=head2 de_q2ac -- remove highest priority job from aborting compile queue

Remove the highest priority job from the aborting compiler queue, so that
it can be aborted by compiler daemon.

    $job_id = &Spkdb::de_q2ac($dbh);

$dbh is the handle to an open database connection.

Returns

  success: 
    job_id of highest priority aborting compile, 
    false if aborting compiler queue is empty   

  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub de_q2ac() {
    my $dbh = shift;
    $err = 0;
    $errstr = "";
    
    $dbh->begin_work;

    my $sql = "select job_id from job "
	    .       "where state_code='q2ac' "
            .       "order by event_time "
            .       "limit 1 "
            .       "for update; ";

    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	$dbh->rollback;
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute $sql; error returned "
	    . $sth->errstr;
	$dbh->rollback;
        return undef;
    }
    my $row = $sth->fetchrow_hashref();
    $sth->finish;
    unless ($row) {
	$dbh->rollback;
	return 0;
    }
    my $job_id = $row->{"job_id"};
    my $state_code = "acmp";
    my $event_time = time();
    
    $sql = "update job set state_code = '$state_code', event_time = '$event_time' where job_id=$job_id;";
    unless ($dbh->do($sql)) {
	$err = $UPDATE_FAILED;
	$errstr = "could not execute $sql; error returned ";
	$dbh->rollback;
	return undef;
    }
    $dbh->commit;
    &add_to_history($dbh, $job_id, $state_code);
    return $job_id;
}

=head2 get_job_ids -- get job_ids of all jobs with a given state_code

Get job_ids of all jobs as an array with a given state_code.

    @array_job_ids = &Spkdb::get_job_ids($dbh, $state_code);

$dbh is the handle to an open database connection.

$state_code is the given state_code of the jobs.

Returns

  success: 
    an array of job_ids of all the jobs with the given state_code,
    0 if there is no job with the given state_code   

  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub get_job_ids() {
    my $dbh = shift;
    my $state_code = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select job_id from job where state_code='$state_code';";
    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	$dbh->rollback;
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute $sql; error returned "
	    . $sth->errstr;
	$dbh->rollback;
        return undef;
    }
    my $job;
    my @jobs;
    while(($job) = $sth->fetchrow_array) {
        push(@jobs, $job);
    }
    $sth->finish;
    my $count = @jobs;
    if ($count == 0) {
	return 0;
    }
    return @jobs;
}

=head2 get_cmp_jobs -- get all jobs currently being compiled

Returns a reference to an array of rows.  Each row is a reference
to a hash table, with the field name as key and field value as value.
Within the array, jobs are sorted in order of job_id, hence the first
job submitted appears first. Each row contains
        job_id
        dataset_id
        dataset_version
        xml_source


    $array_row = $Spkdb::get_cmp_jobs($dbh);

Returns

  success:
    reference to an array of references to hash tables, each 
    representing a subset of a row in the job table; 
    false if there are no jobs with state_code 'cmp'

  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub get_cmp_jobs() {
    
    my $dbh = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select job_id, dataset_id, dataset_version, xml_source "
	. "from job where state_code='cmp' "
	. "order by job_id;";
    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute state: $sql; error returned "
	    . $sth->errstr;
	return undef;
    }
    my $array_row_ref = $sth->fetchall_arrayref({});
    $sth->finish;
    return $array_row_ref;
}

=head2 en_q2r -- add a compiled job to the run queue

Add a compiled job to the queue of jobs that are ready to run.

    $r = &Spkdb::en_q2r($dbh, $job_id, $cpp_source);

$dbh is the handle to an open database connection.

$job_id is the key to the job table

$cpp_source is a string of c++ source code.

Returns

  success: true
  failure: false
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::UPDATE_FAILED

=cut

sub en_q2r() {
    my $dbh = shift;
    my $job_id = shift;
    my $cpp_source = shift;
    $err = 0;
    $errstr = "";

    my $state_code = 'q2r';
    my $event_time = time();

    my $sql = "update job "
	      .  "set state_code='$state_code', "
	      .  "event_time=$event_time, "
              .  "cpp_source=? "
              .  "where job_id=$job_id;";

    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$errstr = "en_q2r failed prepare";
	$err = $PREPARE_FAILED;
	return 0;
    }
    unless ($sth->execute($cpp_source)) {
	$errstr = "en_q2r failed to update the job table";
	$err = $UPDATE_FAILED;
	return 0;
    }
    $sth->finish;
    &add_to_history($dbh, $job_id, $state_code);
    return 1;
}

=head2 de_q2r -- remove the highest priority job from the run queue

Remove the highest priority job from the ready to run queue, so
that it can be run.

    $row = &Spkdb::de_q2r($dbh);

$dbh is the handle to an open database connection.

Returns

  success: 

    reference to a hash for the row of highest priority, 
    containing the following fields:
        job_id
        cpp_source
    false if ready to run queue is empty   

  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub de_q2r() {
    my $dbh = shift;
    $err = 0;
    $errstr = "";
    
    $dbh->begin_work;

    my $sql = "select job_id, cpp_source, checkpoint from job "
	    .       "where state_code='q2r' "
            .       "order by event_time "
            .       "limit 1 "
            .       "for update;";

    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	$dbh->rollback;
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute $sql; error returned "
	    . $sth->errstr;
	$dbh->rollback;
        return undef;
    }
    my $row = $sth->fetchrow_hashref();
    $sth->finish;
    unless ($row) {
	$dbh->rollback;
	return 0;
    }
    my $job_id = $row->{"job_id"};
    my $event_time = time();
    
    my $state_code = "run";
    $sql = "update job set state_code = '$state_code', event_time = '$event_time' "
           . "where job_id=$job_id;";
    unless ($dbh->do($sql)) {
	$err = $UPDATE_FAILED;
	$errstr = "could not execute $sql; error returned ";
	$dbh->rollback;
	return;
    }
    $dbh->commit;
    &add_to_history($dbh, $job_id, $state_code);
    return $row;
}

=head2 get_q2r_job -- get a queued-to-run job from the database
Remove the highest priority job from the ready to run queue, so
that it can be run.

    $row = &Spkdb::get_q2r_job($dbh, $job_id);

$dbh is the handle to an open database connection.
$job_id is the key to a row in the job table.

Returns

  success: 

    reference to a hash for the row of highest priority, 
    containing the following fields:
        cpp_source
        checkpoint
    false if ready to run queue is empty   

  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub get_q2r_job() {
    my $dbh = shift;
    my $job_id = shift;
    $err = 0;
    $errstr = "";
    
    $dbh->begin_work;

    my $sql = "select cpp_source, checkpoint, parallel from job "
	      . "where job_id='$job_id' for update;";

    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	$dbh->rollback;
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute $sql; error returned "
	    . $sth->errstr;
	$dbh->rollback;
        return undef;
    }
    my $row = $sth->fetchrow_hashref();
    $sth->finish;
    unless ($row) {
	$dbh->rollback;
	return 0;
    }
    my $event_time = time();
    my $state_code = "run";
    $sql = "update job set state_code = '$state_code', event_time = '$event_time' "
           . "where job_id=$job_id;";
    unless ($dbh->do($sql)) {
	$err = $UPDATE_FAILED;
	$errstr = "could not execute $sql; error returned ";
	$dbh->rollback;
	return;
    }
    $dbh->commit;
    &add_to_history($dbh, $job_id, $state_code);
    return $row;
}

=head2 de_q2ar -- remove the highest priority job from the aborting run queue

Remove the highest priority job from the aborting run queue, so
that it can be aborted by run daemon.

    $job_id = &Spkdb::de_q2ar($dbh);

$dbh is the handle to an open database connection.

Returns

  success: 

    job_id of highest priority aborting run job 
    false if aborting run queue is empty   

  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub de_q2ar() {
    my $dbh = shift;
    $err = 0;
    $errstr = "";
    
    $dbh->begin_work;

    my $sql = "select job_id from job "
	    .       "where state_code='q2ar' "
            .       "order by event_time "
            .       "limit 1 "
            .       "for update; ";

    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	$dbh->rollback;
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute $sql; error returned "
	    . $sth->errstr;
	$dbh->rollback;
        return undef;
    }
    my $row = $sth->fetchrow_hashref();
    $sth->finish;
    unless ($row) {
	$dbh->rollback;
	return 0;
    }
    my $job_id = $row->{"job_id"};
    my $state_code = "arun";
    my $event_time = time();
    
    $sql = "update job set state_code = '$state_code', event_time = '$event_time' where job_id=$job_id;";
    unless ($dbh->do($sql)) {
	$err = $UPDATE_FAILED;
	$errstr = "could not execute $sql; error returned ";
	$dbh->rollback;
	return undef;
    }
    $dbh->commit;
    &add_to_history($dbh, $job_id, $state_code);
    return $job_id;
}

=head2 get_run_jobs -- get all jobs with state_code 'run'

Returns a reference to an array of rows.  Each row is a reference
to a hash table, with the field name as key and field value as value.
Within the array, jobs are sorted in order of job_id, hence the first
job submitted appears first. Each row contains 
    job_id
    cpp_source

    $array_row = $Spkdb::get_run_jobs($dbh);

Returns

  success:
    reference to an array of references to hash tables, each 
    representing a subset of a row in the job table; 
    false if there are no jobs with state_code 'run'

  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub get_run_jobs() {
    
    my $dbh = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select job_id, cpp_source, checkpoint "
	. "from job where state_code='run' "
	. "order by job_id;";
    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute state: $sql; error returned "
	    . $sth->errstr;
	return undef;
    }
    my $array_row_ref = $sth->fetchall_arrayref({});
    $sth->finish;
    return $array_row_ref;
}

=head2 end_job -- end a job, whether successful or not

End the job, whether successfully or not.  Note, this does
not actually terminate the processing of a job.  Instead,
it sets the state of the job to 'end', and stores the
final report.

    $r = &Spkdb::end_job($dbh, $job_id, $end, $report, $checkpoint);

$dbh is the handle to an open database connection.

$job_id is a unique job table identifier.

$end_code is a valid end_code, defined in the end table.

$report is a string containing the report.

$checkpoint is a string containing the checkpoint file

Returns

  success: true
  failure: false
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::INVALID_END
                == $SPKDB::UPDATE_FAILED

=cut

sub end_job() {
    my $dbh = shift;
    my $job_id = shift;
    my $end_code = shift;
    my $report = shift;
    my $checkpoint = shift;
    my $event_time = time();
    $err = 0;
    $errstr = "";

    my $sql = "select end_code from end where end_code='$end_code';";
    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "failed to prepare '$sql'";
	return 0;
    }
    unless ($sth->execute()) {
	$err = $EXECUTE_FAILED;
        $errstr = "failed to execute '$sql' $!";
	return 0;
    }
    unless ($sth->fetchrow_array) {
	$errstr = "$end_code is not a valid end_code";
        $err = $INVALID_END;
	return 0;
    }
    $sth->finish;
    my $state_code = "end";
    $sql = "update job "
	      .  "set state_code='$state_code', "
              .      "end_code='$end_code', "
	      .      "event_time=$event_time, "
              .      "cpp_source=null, "
              .      "report=?, "
	      .      "checkpoint=? "
              .  "where job_id=$job_id;";

    $sth = $dbh->prepare($sql);
    unless ($sth) {
	$errstr = "end_job failed prepare";
	$err = $PREPARE_FAILED;
	return 0;
    }
    unless ($sth->execute($report, $checkpoint)) {
	$errstr = "end_job failed to update the job table";
	$err = $UPDATE_FAILED;
	return 0;
    }
    $sth->finish;
    &add_to_history($dbh, $job_id, $state_code);
    return 1;
} 

=head2 job_report -- retrieve final report for a job

Retrieve the report string for a job in the 'end' state.

    $report = &Spkdb::job_report($dbh, $job_id);

$dbh is the handle to an open database connection.

$job_id is the unique numeric identifier of a job;

Returns

    success: a string containing the report
    failure: undef
        $Spkdb::errstr contains an error message string
        $Spkdb::err == $Spkdb::NOT_ENDED if job not in 'end' state
                    == $Spkdb::PREPARE_FAILED if prepare function failed
                    == $Spkdb::EXECUTE_FAILED if execute function failed
                    == $Spkdb::GET_FAILED if retieval failed

=cut

sub job_report() {
    my $dbh = shift;
    my $job_id = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select state_code, report from job where job_id=$job_id;";
    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute state: $sql; error returned "
	    . $sth->errstr;
        return undef;
    }
    my $rrow = $sth->fetchrow_arrayref;
    unless ($rrow) {
	$errstr = "retrieval of job report failed";
	$err = $GET_FAILED;
	return undef;
    }
    unless ($rrow->[0] =~ /^end$/) {
	$errstr = "no report (job not at end)";
	$err = $NOT_ENDED;
	return undef;
    }
    $sth->finish;
    return $rrow->[1];
}

=head2 job_checkpoint -- retrieve final checkpoint for a job

Retrieve the checkpoint string for a job in the 'end' state.

    $checkpoint = &Spkdb::job_checkpoint($dbh, $job_id);

$dbh is the handle to an open database connection.

$job_id is the unique numeric identifier of a job;

Returns

    success: a string containing the checkpoint
    failure: undef
        $Spkdb::errstr contains an error message string
        $Spkdb::err == $Spkdb::NOT_ENDED if job not in 'end' state
                    == $Spkdb::PREPARE_FAILED if prepare function failed
                    == $Spkdb::EXECUTE_FAILED if execute function failed
                    == $Spkdb::GET_FAILED if retieval failed

=cut

sub job_checkpoint() {
    my $dbh = shift;
    my $job_id = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select state_code, checkpoint from job where job_id=$job_id;";
    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute state: $sql; error returned "
	    . $sth->errstr;
        return undef;
    }
    my $rrow = $sth->fetchrow_arrayref;
    unless ($rrow) {
	$errstr = "retrieval of job checkpoint failed";
	$err = $GET_FAILED;
	return undef;
    }
    unless ($rrow->[0] =~ /^end$/) {
	$errstr = "no checkpoint (job not at end)";
	$err = $NOT_ENDED;
	return undef;
    }
    $sth->finish;
    return $rrow->[1];
}

=head2 new_dataset -- add a new dataset

Add a new dataset to a users collection of datasets stored 
in the database.

    $dataset_id = &Spkdb::new_dataset($dbh, $user_id, $name, $abstract, $archive);

$dbh is the handle to an open database connection

$user_id is the unique identifier of a user

$name is the name of the dataset

$abstract is a short description of the dataset

$archive is a version archive, rcs format

Returns

  success: the automatically generated value of the dataset_id
  failure: 0
        $errstr contains an error message string
        $err == $Spkdb::DATASET_EXISTS     if user_id + name is not unique
             == $Spkdb::INSERT_FAILED    if the adding the new record failed

=cut

sub new_dataset() {
    my $dbh = shift;
    my $user_id = shift;
    my $name = shift;
    my $abstract = shift;
    $err = 0;
    $errstr = "";

    my $r_archive = \$_[0];

    # Don't allow a user to be added a name for the second time
    # Note: this will not catch the case where some other connection
    #       adds the user between the time we make this test and the
    #       time we actually do the insert.  In that case, however,
    #       our insert will fail, thus preserving the integrity of
    #       the table, due to the UNIQUE key on the username column.

    my $sql = "select name from dataset "
	. "where user_id=$user_id and name='$name';";
    unless ($dbh->do($sql) == 0) {
	$err = $DATASET_EXISTS;
	$errstr = "this user has another dataset with the same name";
	return 0;
    }
    $sql = "insert into dataset (user_id, name, abstract, archive) "
	. "values ($user_id, '$name', '$abstract', '$$r_archive')" ;
    unless ($dbh->do($sql)) {
	$err = $INSERT_FAILED;
	$errstr = "can't insert new job";
	$dbh->rollback;
	return 0;
    }
    return $dbh->{'mysql_insertid'};
}

=head2 get_dataset -- retrieve a dataset for a user

Retrieve the dataset corresponding to a name

    $row = &Spkdb::get_dataset($dbh, $dataset_id);

$dbh is the handle to an open database connection

$dataset_id is the integer which uniquely identifies the dataset

Returns

  success: reference to a hash table of column/value pairs
           0 if no datatset corresponds to the given dataset_id
  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub get_dataset() {
    my $dbh = shift;
    my $dataset_id = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select * from dataset where dataset_id = $dataset_id;";
    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute state: $sql; error returned "
	    . $sth->errstr;
        return undef;
    }
    my $count = $sth->rows;

    if ($count == 0) {
	return 0;
    }
    unless ($count == 1) {
	$err = $TOO_MANY;
	$errstr = "duplicate dataset record";
	return undef;
    }
    return $sth->fetchrow_hashref();
}

=head2 update_dataset -- update a dataset

Update the abstract and/or the archive of a dataset.

    $r = &Spkdb::update_dataset($dbh, "dataset_id", $dataset_id, ...);

$dbh is a handle to an open database connection.

"dataset_id", $dataset_id is a name value pair which uniquely identifies
a dataset.

Other name value pairs specify the fields to be change.  These
may be:

"abstract", $abstract
"archive",  $archive

These pairs may be in any order, but there must be at least one pair.

Returns

  success: 1
  failure: 0
    $Spkdb::errstr contains an error message string
    $Spkdb::err = $Spkdb::KEY_REQUIRED   if "dataset_id" not supplied
                = $Spkdb::INVALID_CHANGE if you have asked to change a
                            field that must never be changed
                = $Spkdb::UPDATE_FAILED  if failure for some other reason

=cut

sub update_dataset() {
    my $dbh = shift;
    my %args = @_;
    $err = 0;
    $errstr = "";

    # Make sure that args contain user_id, which is the primary key

    unless (defined $args{dataset_id}) {
	$err = $KEY_REQUIRED;
	$errstr = "argument list must contain dataset_id";
	return 0;
    }
    # Copy user_id, then remove from list because it won't change

    my $dataset_id = $args{dataset_id};
    my $sql = "update dataset set ";

    # Build the sql statement.  Make sure that password is coded.

    delete $args{dataset_id};
    for my $column (keys %args) {
	if ($column =~ /^archive$/ || $column =~ /^abstract$/) {
	    my $value = "'$args{$column}'";
	    $sql .= "$column=$value, ";
	} 
	else {
	    $err = $INVALID_CHANGE;
	    $errstr = "$column cannot be changed";
	    return 0;
	}
    }
    chop $sql; chop $sql;
    $sql .= " where dataset_id=$dataset_id";

    my $nrows = $dbh->do($sql);
    unless ($nrows && $nrows == 1) {
	$err = $UPDATE_FAILED;
	$errstr = "update of user record $dataset_id failed";
	return 0;
    }    
    return 1;
}

=head2 user_datasets -- get descriptions of all the datasets of a user

Returns a reference to an array of rows.  Each row is a reference
to a hash table, with the field name as key and field value as value.
Within the array, datasets are sorted in order of name.  The fields
returned are dataset_id, name, and abstract.

    $array_row = $Spkdb::user_datasets($dbh, $user_id);

Returns

  success:
    reference to an array of references to hash tables, each 
    representing a row in the dataset table

  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub user_datasets() {
    
    my $dbh = shift;
    my $user_id = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select dataset_id, name, abstract from dataset "
	. "where user_id='$user_id order by name;' ";

    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute statement: $sql; error returned "
	    . $sth->errstr;
	return undef;
    }
    my $array_row_ref = $sth->fetchall_arrayref({});
    $sth->finish;
    return $array_row_ref;
}


=head2 new_model -- add a new model

Add a new model to a users collection of models, stored 
in the database.

    $model_id = &Spkdb::new_model($dbh, $user_id, $name, $abstract, $archive);

$dbh is the handle to an open database connection

$user_id is the unique identifier of a user

$name is the name of the model

$abstract is a short description of the model

$archive is a version archive, rcs format

Returns

  success: the automatically generated value of the model_id
  failure: 0
        $errstr contains an error message string
        $err == $Spkdb::MODEL_EXISTS     if user_id + name is not unique
             == $Spkdb::INSERT_FAILED    if the adding the new record failed

=cut

sub new_model() {
    my $dbh = shift;
    my $user_id = shift;
    my $name = shift;
    my $abstract = shift;
    $err = 0;
    $errstr = "";

    my $r_archive = \$_[0];

    # Don't allow a user to be added a name for the second time
    # Note: this will not catch the case where some other connection
    #       adds the user between the time we make this test and the
    #       time we actually do the insert.  In that case, however,
    #       our insert will fail, thus preserving the integrity of
    #       the table, due to the UNIQUE key on the username column.

    my $sql = "select name from model "
	. "where user_id=$user_id and name='$name';";
    unless ($dbh->do($sql) == 0) {
	$err = $MODEL_EXISTS;
	$errstr = "this user has another model with the same name";
	return 0;
    }
    $sql = "insert into model (user_id, name, abstract, archive) "
	. "values ($user_id, '$name', '$abstract', '$$r_archive')" ;
    unless ($dbh->do($sql)) {
	$err = $INSERT_FAILED;
	$errstr = "can't insert new job";
	$dbh->rollback;
	return 0;
    }
    return $dbh->{'mysql_insertid'};
}

=head2 get_model -- retrieve a model for a user

Retrieve the model corresponding to a name

    $row = &Spkdb::get_model($dbh, $user_id, $name);

$dbh is the handle to an open database connection

$model_id is the integer which uniquely identifies the model

Returns

  success: reference to a hash table of column/value pairs
           0 if no model corresponds to the model_id
  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub get_model() {
    my $dbh = shift;
    my $model_id = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select * from model where model_id=$model_id;";
    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute state: $sql; error returned "
	    . $sth->errstr;
        return undef;
    }
    my $count = $sth->rows;

    if ($count == 0) {
	return 0;
    }
    unless ($count == 1) {
	$err = $TOO_MANY;
	$errstr = "duplicate model record";
	return undef;
    }
    my $mrow = $sth->fetchrow_hashref();
    $sth->finish;
    return $mrow;
}

=head2 update_model -- update a model

Update the abstract and/or the archive of a model.

    $r = &Spkdb::update_model($dbh, "model_id", $model_id, ...);

$dbh is a handle to an open database connection.

"model_id", $model_id is a name value pair which uniquely identifies
a model.

Other name value pairs specify the fields to be change.  These
may be:

"abstract", $abstract
"archive",  $archive

These pairs may be in any order, but there must be at least one pair.

Returns

  success: 1
  failure: 0
    $Spkdb::errstr contains an error message string
    $Spkdb::err = $Spkdb::KEY_REQUIRED   if "model_id" not supplied
                = $Spkdb::INVALID_CHANGE if you have asked to change a
                            field that must never be changed
                = $Spkdb::UPDATE_FAILED  if failure for some other reason

=cut

sub update_model() {
    my $dbh = shift;
    my %args = @_;
    $err = 0;
    $errstr = "";

    # Make sure that args contain user_id, which is the primary key

    unless (defined $args{model_id}) {
	$err = $KEY_REQUIRED;
	$errstr = "argument list must contain model_id";
	return 0;
    }
    # Copy user_id, then remove from list because it won't change

    my $model_id = $args{model_id};
    my $sql = "update model set ";

    # Build the sql statement.  Make sure that password is coded.

    delete $args{model_id};
    for my $column (keys %args) {
	if ($column =~ /^archive$/ || $column =~ /^abstract$/) {
	    my $value = "'$args{$column}'";
	    $sql .= "$column=$value, ";
	} 
	else {
	    $err = $INVALID_CHANGE;
	    $errstr = "$column cannot be changed";
	    return 0;
	}
    }
    chop $sql; chop $sql;
    $sql .= " where model_id=$model_id";

    my $nrows = $dbh->do($sql);
    unless ($nrows && $nrows == 1) {
	$err = $UPDATE_FAILED;
	$errstr = "update of user record $model_id failed";
	return 0;
    }    
    return 1;
}

=head2 user_models -- get descriptions of all the models of a user

Returns a reference to an array of rows.  Each row is a reference
to a hash table, with the field name as key and field value as value.
Within the array, models are sorted in order of name.  The fields
returned are model_id, name, and abstract.

    $array_row = $Spkdb::user_models($dbh, $user_id);

Returns

  success:
    reference to an array of references to hash tables, each 
    representing a row in the model table

  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub user_models() {
    
    my $dbh = shift;
    my $user_id = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select model_id, name, abstract from model "
	. "where user_id='$user_id order by name;' ";

    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute statement: $sql; error returned "
	    . $sth->errstr;
	return undef;
    }
    my $array_row_ref = $sth->fetchall_arrayref({});
    $sth->finish;
    return $array_row_ref;
}


=head2 new_user -- add a new user

Add a new Spk user to the database.

    $user_id = &Spkdb::new_user($dbh,
                                "username", $username,
                                "password", $password,
                                ...);

$dbh is the handle to an open database connection

$username is a unique string that will be used to identify
an Spk user.

$password is a string which will be used to authenticate
this Spk user.

Other name/value pairs corresponding to column names defined
for the user table.

Returns

  success: the automatically generated value of the user_id
  failure: 0
        $errstr contains an error message string
        $err == $Spkdb::COLUMN_REQUIRED if username or password not supplied
             == $Spkdb::USER_EXISTS     if the username is not unique
             == $Spkdb::INSERT_FAILED   if the adding the new record failed

=cut

sub new_user() {
    my $dbh = shift;
    my %args = @_; 
    $err = 0;
    $errstr = "";

#   Array of all required (NOT NULL) columns, with the exception of user_id,
#   which is automatic

    my @required = ("username", "password");

#   Insure that all required fields have been specified in args

    for my $column (@required) {
	unless (defined $args{$column}) {
	    $err = $COLUMN_REQUIRED;
	    $errstr = "a required column ($column) is missing";
	    return 0;
	}
    }

    # Don't allow a user to be added a second time
    # Note: this will not catch the case where some other connection
    #       adds the user between the time we make this test and the
    #       time we actually do the insert.  In that case, however,
    #       our insert will fail, thus preserving the integrity of
    #       the table, due to the UNIQUE key on the username column.

    my $sql = "select username from user where username = '$args{username}'";
    my $nrows = $dbh->do($sql);

    # Note that the returned value is false only if the query failed.
    # If the query is successful, the number of rows satisfying the
    # query is returned.  This number can be zero.

    unless ($nrows) {
	$err = $SQL_ERROR;
	$errstr = "SQL error in '$sql'";
	return 0;
    }
    if ($nrows != 0) {
	$err = $USER_EXISTS;
	$errstr = "can't add user $args{username} because record "
	    . "already exists";
	return 0;
    }

#   Surround values with quotes and apply MD5 to password

    for my $column (keys %args) {
	$args{$column} = "'$args{$column}'";
	if ($column =~ /^password$/) {
	    $args{$column} = "MD5($args{$column})";
	}
    }

#   Do the insertion

    $sql = "insert into user (" . join (", ", keys %args) . ") "
	. "values (" . join(", ", values %args) . ")";

    unless ($dbh->do($sql)) {
	$err = $INSERT_FAILED;
	$errstr = "can't insert new user record";
	return 0;
    }
    my $user_id = $dbh->{'mysql_insertid'};

    return $user_id;
}

=head2 update_user -- update a user record

Update the row in user identified by $user_id:

    $rv = &Spkdb::update_user($dbh, "user_id", $user_id, ...);

$dbh is a handle to an open database connection.

$user_id is value of the numerical primary key for an existing row.

Other name/value pairs specify columns to be changed.

Returns

  success: 1
  failure: 0
    $Spkdb::errstr contains an error message string
    $Spkdb::err = $Spkdb::KEY_REQUIRED  if "user_id" not suppled
                = $Spkdb::UPDATE_FAILED if failure for some other reason

=cut

sub update_user() {
    my $dbh = shift;
    my %args = @_;
    $err = 0;
    $errstr = "";

    # Make sure that args contain user_id, which is the primary key

    unless (defined $args{user_id}) {
	$err = $KEY_REQUIRED;
	$errstr = "argument list must contain user_id";
	return 0;
    }
    # Copy user_id, then remove from list because it won't change

    my $user_id = $args{user_id};
    my $sql = "update user set ";

    # Build the sql statement.  Make sure that password is coded.

    delete $args{user_id};
    for my $column (keys %args) {
	my $value = "'$args{$column}'";
	if ($column =~ /^username$/) {
	    $err = $INVALID_CHANGE;
	    $errstr = "can't change $column";
	    return 0;
	}
	if ($column =~ /^password$/) {
	    $value="MD5($value)";
	}
	$sql .= "$column=$value, ";
    }
    chop $sql; chop $sql;
    $sql .= " where user_id=$user_id";

    my $nrows = $dbh->do($sql);
    unless ($nrows && $nrows == 1) {
	$err = $UPDATE_FAILED;
	$errstr = "update of user record $user_id failed";
	return 0;
    }    
    return 1;
}

=head2 get_user -- retrieve a user record by username

Retrieve the row corresponding to a username.

    $row_hash_ref = &Spkdb::get_user($dbh, $username);

$dbh is the handle to an open database connection

$username is the value of username for an existing row.

Returns

  success: reference to a hash table of column/value pairs
  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub get_user() {
    my $dbh = shift;
    my $username = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select * from user where username='$username';";
    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute state: $sql; error returned "
	    . $sth->errstr;
	return undef;
    }
    unless ($sth->rows == 1) {
	return undef;
    }
    return $sth->fetchrow_hashref();
}

=head2 set_parallel -- set a job to run in parallel process mode 
Given a job_id, set the parallel process mode option for the job.
    
    $r = &Spkdb::set_parallel($dbh, $job_id, $parallel);

$dbh is the handle to an open database connection

$job_id is the key to a row in the job table

$parallel is true if parallel process mode is specified, false if otherwise

Returns

  success: true if parallel process mode option is set, false if otherwise
  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::UPDATE_FAILED

=cut

sub set_parallel() {
    my $dbh = shift;
    my $job_id = shift;
    my $parallel = shift;
    $err = 0;
    $errstr = "";

    my $sql = "update job set parallel=$parallel where job_id='$job_id';";

    my $nrows = $dbh->do($sql);
    unless ($nrows)
    {
	$err = $UPDATE_FAILED;
	$errstr = "could not execute $sql; error returned ";
        return undef;
    }
    unless ($nrows == 1)
    {
        return 0;
    }
    return 1;
}


=head2 set_mail_notice -- set end-job email notice request option
Given a job_id, set end-job email notice request option for the job.
    
    $r = &Spkdb::set_mail_notice($dbh, $job_id, $email);

$dbh is the handle to an open database connection

$job_id is the key to a row in the job table

$email is true if end-job email notice is requested, false if otherwise

Returns

  success: true if end-job email notice request option is set, false if otherwise
  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::UPDATE_FAILED

=cut

sub set_mail_notice() {
    my $dbh = shift;
    my $job_id = shift;
    my $email = shift;
    $err = 0;
    $errstr = "";

    my $sql = "update job set mail=$email where job_id='$job_id';";

    my $nrows = $dbh->do($sql);
    unless ($nrows)
    {
	$err = $UPDATE_FAILED;
	$errstr = "could not execute $sql; error returned ";
        return undef;
    }
    unless ($nrows == 1)
    {
        return 0;
    }
    return 1;
}

=head2 get_mail_notice -- get end-job email notice request option
Given a job_id, get end-job email notice request option for the job.
    
    $r = &Spkdb::get_mail_notice($dbh, $job_id);

$dbh is the handle to an open database connection

$job_id is the key to a row in the job table

Returns

  success: true if end-job email notice request option is set, false if otherwise
  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::UPDATE_FAILED

=cut

sub get_mail_notice() {
    my $dbh = shift;
    my $job_id = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select mail from job where job_id='$job_id';";
    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute state: $sql; error returned "
	    . $sth->errstr;
        return undef;
    }
    unless ($sth->rows == 1) {
	return undef;
    }
    my $row = $sth->fetchrow_hashref();
    $sth->finish;
    return $row->{"mail"};
}

=head2 email_for_job -- get email address for a job

Given a job_id, retrieve the email address of the user.

    $email = &Spkdb::email_for_job($dbh, $job_id);

$dbh is the handle to an open database connection

$job_id is the key to a row in the job table

Returns

  success: string containing an email address
  failure: undef
    $Spkdb::errstr contains an error message string
    $Spkdb::err == $Spkdb::PREPARE_FAILED if prepare function failed
                == $Spkdb::EXECUTE_FAILED if execute function failed

=cut

sub email_for_job() {
    my $dbh = shift;
    my $job_id = shift;
    $err = 0;
    $errstr = "";

    my $sql = "select email from job,user where job_id=$job_id and job.user_id=user.user_id;";

    my $sth = $dbh->prepare($sql);
    unless ($sth) {
	$err = $PREPARE_FAILED;
	$errstr = "could not prepare statement: $sql";
	return undef;
    }
    unless ($sth->execute())
    {
	$err = $EXECUTE_FAILED;
	$errstr = "could not execute state: $sql; error returned "
	    . $sth->errstr;
	return undef;
    }
    unless ($sth->rows == 1) {
	return undef;
    }
    my $row = $sth->fetchrow_hashref();
    $sth->finish;
    return $row->{"email"};
}



# local subroutines

sub add_to_history() {
    my $dbh = shift;
    my $job_id = shift;
    my $state_code = shift;
    my $event_time = time();
    my $host = hostname;

    my $sql = "insert into history (job_id, state_code, event_time, host) "
	         . "values($job_id, '$state_code', $event_time, '$host');";

    $dbh->do($sql);
}


=head1 SEE ALSO

For examples of the use of the subroutines in this module,
see the test driver, driver.pl, in the t directory of this
release.

=head1 AUTHOR

Alan Westhagen

=head1 COPYRIGHT AND LICENSE

This module is part of the System for Population Kinetics (SPK), which
was developed with support from NIH grants RR-12609 and P41-
EB001975. Please cite these grants in any publication for which this
software is used and send a notification to the address given above.

SPK is Copyright (C) 1998-2003, by the University of Washington,
Resource Facility for Population Kinetics, and is made available as
free open source software under the terms of the University of
Washington Free-Fork License as a public service.  A copy of the
License can be found in the COPYING file in the root directory of this
distribution. or can be obtained from

     Resource Facility for Population Kinetics
     Department of Bioengineering Box 352255
     University of Washington
     Seattle, WA 98195-2255

=cut
