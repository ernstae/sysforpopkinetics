#!/usr/bin/perl -w

use strict;
use CGI qw(:standard escape);

use lib "/usr/local/lib/site_perl";
use YRC::DB;
use YRC::WWW::SESSION;
use YRC::WWW::LOGIN;
use YRC::WWW::TEMPLATES;
use YRC::WWW::HISTORY;
use YRC::WWW::ERROR;
use YRC::PROJECT::PROJECT;
use YRC::PROJECT::RESEARCHER;

my($query) = new CGI;
my($USER);
my($TITLE) = 'Delete Project';

# FORCE IT TO BE SECURE
#YRC::WWW::SESSION->enforceSecure($query);

# OUR ENTIRE AUTHENTICATION CODE
unless($USER = YRC::WWW::SESSION->getUserObject(YRC::WWW::SESSION->getSessionID($query))) {
   YRC::WWW::LOGIN->showLogin(QUERY=>$query, REQUESTER=>$ENV{REQUEST_URI});
   exit 0;
}

# PRINT OUR HEADER
print YRC::WWW::TEMPLATES->getHeader($TITLE, 1, $USER);

# GET OUR VARIABLES
my($action) = $query->param('action');
my($projectID) = $query->param('ID');
my($dbh) = YRC::DB->getDBH();
my($errorMsg);
my($successMsg);

my($HISTORY) = YRC::WWW::HISTORY->new( (SESSIONID=>$USER->query('SESSIONID')) ); 
$HISTORY->setHistory( ( TITLE=>$TITLE, LINK=>$query->url(-full=>1,-query=>1), TRUNC=>$query->param('trunc') ) );
print $HISTORY->getHistoryNav();

# WHO DO WE WANT TO HAVE ACCESS TO THIS PAGE?
$USER->checkGroup( NUMBER=>1 );

if($action eq 'delete') {
   # THEY WISH TO DELETE

   my($PROJECT) = YRC::PROJECT::PROJECT->new();
   $PROJECT->setDBH($dbh);
   $PROJECT->load($projectID) || $PROJECT->set('NOTFOUND', 1);

   if(!$projectID) {
      $errorMsg = "No project ID was specified...";
   } elsif($PROJECT->query('NOTFOUND')) {
      $errorMsg = "That project ID doesn't exist.  Did you delete it recently?";
   } else {
      # WE'VE BEEN PASSED A VALID PROJECT ID, LET'S DELETE IT HERE
      my($sql) = "DELETE FROM tblProjects WHERE projectID = $projectID";
      $dbh->do($sql);
      $sql = "DELETE FROM tblDissemination WHERE projectID = $projectID";
      $dbh->do($sql);
      $sql = "DELETE FROM tblCollaboration WHERE projectID = $projectID";
      $dbh->do($sql);
      $sql = "DELETE FROM tblTechnology WHERE projectID = $projectID";
      $dbh->do($sql);
      $sql = "DELETE FROM tblTraining WHERE projectID = $projectID";
      $dbh->do($sql);

      $successMsg = "<P>Project ID $projectID successfully deleted.\n";

      # TEST PI FOR PROJECT ASSOCIATION
      my($RE) = YRC::PROJECT::RESEARCHER->new();
      $RE->setDBH($dbh);
      if( $RE->load($PROJECT->query('PI')) && !$RE->queryProjects() ) {
         $sql = "DELETE FROM tblResearchers WHERE researcherID = " . $PROJECT->query('PI');
         $dbh->do($sql);
         $successMsg .= "<P>" . $RE->query('FIRSTNAME') . " " . $RE->query('LASTNAME'). " (";
         $successMsg .= $RE->query('ID') . ") was deleted (no projects associated).";
      }

      # TEST RESEARCHERB FOR PROJECT ASSOCIATION
      $RE = YRC::PROJECT::RESEARCHER->new();
      $RE->setDBH($dbh);
      if( $RE->load($PROJECT->query('RESEARCHERB')) && !$RE->queryProjects() ) {
         $sql = "DELETE FROM tblResearchers WHERE researcherID = " . $PROJECT->query('RESEARCHERB');
         $dbh->do($sql);
         $successMsg .= "<P>" . $RE->query('FIRSTNAME') . " " . $RE->query('LASTNAME'). " (";
         $successMsg .= $RE->query('ID') . ") was deleted (no projects associated).";
      }

      # TEST RESEARCHERB FOR PROJECT ASSOCIATION
      $RE = YRC::PROJECT::RESEARCHER->new();
      $RE->setDBH($dbh);
      if( $RE->load($PROJECT->query('RESEARCHERC')) && !$RE->queryProjects() ) {
         $sql = "DELETE FROM tblResearchers WHERE researcherID = " . $PROJECT->query('RESEARCHERC');
         $dbh->do($sql);
         $successMsg .= "<P>" . $RE->query('FIRSTNAME') . " " . $RE->query('LASTNAME'). " (";
         $successMsg .= $RE->query('ID') . ") was deleted (no projects associated).";
      }

      # TEST RESEARCHERB FOR PROJECT ASSOCIATION
      $RE = YRC::PROJECT::RESEARCHER->new();
      $RE->setDBH($dbh);
      if( $RE->load($PROJECT->query('RESEARCHERD')) && !$RE->queryProjects() ) {
         $sql = "DELETE FROM tblResearchers WHERE researcherID = " . $PROJECT->query('RESEARCHERD');
         $dbh->do($sql);
         $successMsg .= "<P>" . $RE->query('FIRSTNAME') . " " . $RE->query('LASTNAME'). " (";
         $successMsg .= $RE->query('ID') . ") was deleted (no projects associated).";
      }
   }
}



# DO WE HAVE AN ERROR?
if($errorMsg) { YRC::WWW::ERROR->showError($errorMsg); }
elsif($successMsg) { YRC::WWW::ERROR->showSuccess($successMsg); }

# PRINT OUR FOOTER
print YRC::WWW::TEMPLATES->getFooter();

exit 0;
