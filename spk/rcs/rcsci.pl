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


my $new = shift;
my $working_dir = shift;
my $log = shift;
my $author = shift;
my $filename = shift;

($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=localtime(time);
$year = $year + 1900;
$mon = $mon + 1;
if($mon < 10){$mon = "0$mon";}
if($mday < 10){$mday = "0$mday";}
if($hour < 10){$hour = "0$hour";}
if($min < 10){$min = "0$min";}
if($sec < 10){$sec = "0$sec";}
my $date = "$year/$mon/$mday $hour:$min:$sec";

chdir $working_dir;
if($new eq "1")
{
    my @args = ("ci", "-t-", "-m$log", "-w$author", "-d$date", $filename);
    system(@args);
}
if($new eq "0")
{
    my @args = ("ci", "-u", "-m$log", "-w$author", "-d$date", $filename);
    system(@args);
}

