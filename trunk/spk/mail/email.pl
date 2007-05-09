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

my $from = shift;
my $to = shift;
my $bcc = shift;
my $subject = shift;
my $message = shift;

use MIME::Lite;
my $msg = MIME::Lite->new(
           From     => $from,
           To       => $to,
           Bcc      => $bcc,
           Subject  => $subject,
	   Data     => $message
           );
MIME::Lite->send('smtp','localhost',Debug=>0);
$msg->send; # send via default
