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

if (scalar(@ARGV) != 2) {
    print "\nusage $0 database user\n";
    exit 0;
}

my $db = shift;
my $user = shift;

my $dumpfl = "/tmp/dump.sql";
my $schemafl = $db . "_schema.sql";


system "mysqldump $db -u $user -p > $dumpfl";

open(DUMP, $dumpfl);
open(SCHEMA, ">$schemafl");

print SCHEMA "use $db;\n\n";

while (<DUMP>) {
    /INSERT INTO history/ && next;
    /INSERT INTO job/ && next;
    /INSERT INTO model/ && next;
    /INSERT INTO user/ && next;
    print SCHEMA ;
}
