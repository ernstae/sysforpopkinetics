#!/usr/bin/perl -w

use strict;

my $mysqldump = "/usr/bin/mysqldump";
my $user = "daemon";
my $pass = "daemon";
my $db = "spkdb";

my ($sec, $min, $hour, $mday, $mon, $year) = localtime;
my $date = sprintf "%04d-%02d-%02d-%02d%02d-%02d", $year+1900, $mon+1, $mday, $hour, $min, $sec;

my $prefix = "$db-$date";
my $schema   = "$prefix-schema.sql";
my $basedata = "$prefix-basedata.sql";
my $userdata = "$prefix-userdata.sql";

open STDOUT, ">$schema";
my @args = ($mysqldump, "-u$user", "-p$pass", "-d", "$db");
system(@args) == 0
    or die "Could not dump the schema\n";
close STDOUT;

open STDOUT, ">$basedata";
@args = ($mysqldump, "-u$user", "-p$pass", "-tc", "$db");
push @args, ("--tables", "end", "method", "state");
system(@args) == 0
    or die "Could not dump the basedata tables\n";
close STDOUT;

open STDOUT, ">$userdata";
@args = ($mysqldump, "-u$user", "-p$pass", "-tc", "$db");
push @args, ("--tables", "dataset", "history", "job", "model", "user");
system(@args) == 0
    or die "Could not dump the userdata tables\n";
close STDOUT;



