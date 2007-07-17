#!/usr/bin/perl

############################################################
# MRTG script for retrieving 5 minute CPU load from
# local or remote host via ssh
#
# by: Andrew Ernst (ernst@u.washington.edu)
#
############################################################

$numArgs = $#ARGV;

if ( length($ARGV[0]) <= 0 ) {
  $hostname = "";
}
else {
  $hostname = $ARGV[0];
}

# get load average from server
if ( $hostname ne "" ) {
  $load = `ssh $hostname uptime`;
}
else {
  $load = `uptime`;
  $hostname = `hostname -s`;
  chop($hostname);
}

chop($load);
$load =~ /.average:.+, (.+),.+/;
$la = $1 * 100;

print "$la\n";
print "0\n";
print "$load\n";   #uptime
print "$hostname\n"  #hostname
