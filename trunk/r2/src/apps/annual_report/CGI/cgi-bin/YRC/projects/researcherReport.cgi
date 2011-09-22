#!/usr/bin/perl -w

use strict;
use lib "/usr/local/lib/site_perl";

use CGI;

use YRC::DB;
use YRC::PROJECT::PROJECT;
use YRC::PROJECT::RESEARCHER;

use YRC::WWW::SESSION;
use YRC::WWW::LOGIN;
use YRC::WWW::TEMPLATES;
use YRC::WWW::HISTORY;

my($query) = new CGI;

my($sql, $title, $org);
my(@arr);
my($DBH, $sth, $PROJECT, $RESEARCHER, $USER);
$DBH = YRC::DB->getDBH();

# FORCE IT TO BE SECURE
#YRC::WWW::SESSION->enforceSecure($query);

# OUR ENTIRE AUTHENTICATION CODE
unless($USER = YRC::WWW::SESSION->getUserObject(YRC::WWW::SESSION->getSessionID($query))) {
   YRC::WWW::LOGIN->showLogin(QUERY=>$query, REQUESTER=>$ENV{REQUEST_URI});
   exit 0;
}

# WHO DO WE WANT TO HAVE ACCESS TO THIS PAGE?
$USER->checkGroup( NUMBER=>1 );

print "Content-type: text/plain\n\n";

$sql = "SELECT researcherID FROM tblResearchers ORDER BY researcherID";
$sth = $DBH->prepare($sql);
$sth->execute;

# PRINT HEADER LEGEND LINE
print "ID\tLAST NAME\tFIRST NAME\tSTATE\tCOUNTRY\tPROJECT(S)\n";

# RUN THROUGH ALL OF THE PROJECTS
while(@arr = $sth->fetchrow_array) {
   $RESEARCHER = YRC::PROJECT::RESEARCHER->new();
   $RESEARCHER->setDBH($DBH);
   $RESEARCHER->set((ID=>$arr[0]));

   # ID
   print $arr[0] . "\t";

   # LAST NAME
   if($RESEARCHER->query('LASTNAME')) { print deQuote($RESEARCHER->query('LASTNAME')); }
   print "\t";

   # FIRST NAME
   if($RESEARCHER->query('FIRSTNAME')) { print deQuote($RESEARCHER->query('FIRSTNAME')); }
   print "\t";

   # STATE
   if($RESEARCHER->query('STATE')) { print deQuote($RESEARCHER->query('STATE')); }
   print "\t";

   # COUNTRY
   if($RESEARCHER->query('COUNTRY')) { print deQuote($RESEARCHER->query('COUNTRY')); }
   print "\t";

   # PROJECTS
   if($RESEARCHER->queryProjects()) { print join(",", $RESEARCHER->queryProjects()); }
   print "\t";


   print "\n";

}

# STRIPS QUOTE OFF OF A STRING, IF QUOTED
# RETURNS THE STRING SANS QUOTES
sub deQuote {
   my($str) = @_;
       
   # SNIP OUT THE QUOTES

   while(substr($str, 0, 1) eq "\"") {  substr($str, 0, 1, ""); }
   while(substr($str, -1, 1) eq "\"") { substr($str, -1, 1, ""); }
   
   return $str;
}



exit 0;
