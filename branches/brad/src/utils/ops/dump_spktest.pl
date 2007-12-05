#!/usr/bin/perl -w

use strict;

=head1 NAME
    
    dump_spktest.pl -- take a symbolic dump of the test database

=head1 ABSTRACT

    This program takes a symbolic dump of the test database, so that
    it can be easily recreated for additional testing.  The output
    can be used as input to load_spktest.pl

=head1 SYNOPSIS

    dump_spktest.pl

=head1 DESCRIPTION

    This program is designed to be run from the database server. It is not necessary
    for the user to have root privileges. It produces its output in the current directory.

=head2 OUTPUT FILE CONTENTS

    The output of this program consists of three files of sql statements:
    - schema:   'create table' statements
    - basedata: 'insert' statements for inserting all data into the non-transactional
                table; namely, class, end, method, and state
    - userdata: 'insert' statements for inserting all data into the rest of the tables

=head2 OUTPUT FILE NAMES

    schema.sql
    basedata.sql
    userdata.sql

=head2 ARGUMENTS

    No optional arguments are supported.

=head2 DEPENDENCIES

    This program calls dump_spkdb.pl, which in turn depends on a MySQL utility,
    mysqldump, to do the hard work.

=head1 SEE ALSO

    The documentation for load_spktest.pl 

=cut


my $usage = "usage: dump_spktest.pl";
my $dump_spkdb = "/usr/local/bin/dump_spkdb.pl";

@ARGV == 0 or die("$usage\n");

system("$dump_spkdb --noprefix --spktest") == 0
    or die("Could not execute $dump_spkdb\n");

exit 0;
