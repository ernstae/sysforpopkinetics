#!/usr/bin/perl -w

use strict;
use Spkdb ('connect', 'disconnect', 'set_parallel');

=head1 NAME

    set_parallel.pl -- set flags in spktest database to make the jobs 
    run in parallel processing mode

=head1 SYNOPSIS

    set_parallel.pl job_id ...

    =head1 DESCRIPTION

    Given a list of job_id numbers, this program sets flags in the job table 
    of the spktest database to make the jobs run in parallel processing mode.

=head1 DEPENDENCIES

    It uses the SPK Perl API module:

    Spkdb.pm

=cut

my $dbh = &connect("spktest", "localhost", "tester", "tester");
for my $job_id (@ARGV) {
    &set_parallel($dbh, $job_id, 1);
}
&disconnect($dbh);
