#!/usr/bin/perl -w

use strict;
use Spkdb ('connect', 'disconnect', 'set_parallel');

=head1 NAME

    set_job_parallel.pl -- set ntasks in spktest database to make the job 
    run in parallel processing mode

=head1 SYNOPSIS

    set_job_parallel.pl job_id ntasks

    =head1 DESCRIPTION

    Given a job_id numbers and an integer, this program sets ntasks for the job in the job table of the spktest database to make the job run in parallel processing mode.

=head1 DEPENDENCIES

    It uses the SPK Perl API module:

    Spkdb.pm

=cut

my $job_id = shift;
my $ntasks = shift;
my $dbh = &connect("spktest", "localhost", "tester", "tester");
&set_parallel($dbh, $job_id, $ntasks);
&disconnect($dbh);
