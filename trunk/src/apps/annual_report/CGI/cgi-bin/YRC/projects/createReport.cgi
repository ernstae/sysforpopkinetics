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
my($DBH, $sth, $PROJECT, $USER);
my($PI, $RESB, $RESC, $RESD);
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

my(@fields) = split(",", $query->param('reportFields'));
my($includeR) = 0;
foreach (@fields) { if(/PI_/ || /RES_/) { $includeR = 1; last; } }

$sql  = "SELECT projectID FROM tblProjects ORDER BY projectID";
$sth = $DBH->prepare($sql);
$sth->execute;

# PRINT HEADER LEGEND LINE
foreach (@fields) { print "$_\t"; } print "\n";


# RUN THROUGH ALL OF THE PROJECTS
while(@arr = $sth->fetchrow_array) {
   $PROJECT = YRC::PROJECT::PROJECT->new();
   $PROJECT->setDBH($DBH);
   $PROJECT->set((ID=>$arr[0]));

   $PI = 0;
   $RESB = 0;
   $RESC = 0;
   $RESD = 0;


   # RUN THROUGH THE SELECTED FIELDS FOR THE REPORT
   foreach (@fields) {
      if(/PI_([\w]+)/) {

         if(!$PI) {
            $PI = YRC::PROJECT::RESEARCHER->new();
            $PI->setDBH($DBH);
            $PI->load($PROJECT->query('PI'));
         }

         # USE PI INFORMATION
         if($PI->query($1)) { print deQuote($PI->query($1)); }
         print "\t";
         next;
      }
      if(/RESB_([\w]+)/) {

         if(!$RESB) {
            $RESB = YRC::PROJECT::RESEARCHER->new();
            $RESB->setDBH($DBH);
            $RESB->load($PROJECT->query('RESEARCHERB'));
         }

         # USE PI INFORMATION
         if($RESB->query($1)) { print deQuote($RESB->query($1)); }
         print "\t";
         next;
      }
      if(/RESC_([\w]+)/) {

         if(!$RESC) {
            $RESC = YRC::PROJECT::RESEARCHER->new();
            $RESC->setDBH($DBH);
            $RESC->load($PROJECT->query('RESEARCHERC'));
         }

         # USE PI INFORMATION
         if($RESC->query($1)) { print deQuote($RESC->query($1)); }
         print "\t";
         next;
      }
      if(/RESD_([\w]+)/) {

         if(!$RESD) {
            $RESD = YRC::PROJECT::RESEARCHER->new();
            $RESD->setDBH($DBH);
            $RESD->load($PROJECT->query('RESEARCHERD'));
         }

         # USE PI INFORMATION
         if($RESD->query($1)) { print deQuote($RESD->query($1)); }
         print "\t";
         next;
      }

      # SPECIAL CASE FOR DISPLAYING YRC GROUPS
      if($_ eq 'GROUPS') {
         if($PROJECT->queryGroups()) { print deQuote(join(',', $PROJECT->queryGroups())); }
         print "\t";
         next;
      }

      if($PROJECT->query($_)) { print deQuote($PROJECT->query($_)); }
      print "\t";

   }
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
