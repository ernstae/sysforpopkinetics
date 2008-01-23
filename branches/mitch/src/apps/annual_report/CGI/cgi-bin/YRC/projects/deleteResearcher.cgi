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
my($TITLE) = 'Delete Researcher';

# FORCE IT TO BE SECURE
#YRC::WWW::SESSION->enforceSecure($query);

# OUR ENTIRE AUTHENTICATION CODE
unless($USER = YRC::WWW::SESSION->getUserObject(YRC::WWW::SESSION->getSessionID($query))) {
   YRC::WWW::LOGIN->showLogin(QUERY=>$query, REQUESTER=>$ENV{REQUEST_URI});
   exit 0;
}

# GET OUR VARIABLES
my($action) = $query->param('action');
my($researcherID) = $query->param('ID');
my($subID) = $query->param('sID');
my($dbh) = YRC::DB->getDBH();
my($errorMsg);
my($successMsg);

# PRINT OUR HEADER
print YRC::WWW::TEMPLATES->getHeader($TITLE, 1, $USER);

my($HISTORY) = YRC::WWW::HISTORY->new( (SESSIONID=>$USER->query('SESSIONID')) ); 
if($action ne 'delete') {
   $HISTORY->setHistory( ( TITLE=>$TITLE, LINK=>$query->url(-full=>1,-query=>1), TRUNC=>$query->param('trunc') ) );
}
print $HISTORY->getHistoryNav();

# WHO DO WE WANT TO HAVE ACCESS TO THIS PAGE?
$USER->checkGroup( NUMBER=>1 );

if($action eq 'delete') {
   # THEY WISH TO DELETE

   my($RESEARCHER) = YRC::PROJECT::RESEARCHER->new();
   $RESEARCHER->setDBH($dbh);
   $RESEARCHER->load($researcherID) || $RESEARCHER->set('NOTFOUND', 1);

   my($SUB) = YRC::PROJECT::RESEARCHER->new();
   $SUB->setDBH($dbh);
   $SUB->load($subID) || $SUB->set('NOTFOUND', 1);

   if(!$researcherID) {
      $errorMsg = "You did not specify a researcher to delete.";
   } elsif($RESEARCHER->query('NOTFOUND')) {
      $errorMsg = "That researcher doesn't exist.  How can I delete it?";
   } elsif($subID && $SUB->query('NOTFOUND')) {
      $errorMsg = "Your substitute researcher does not exist!";
   } elsif($RESEARCHER->queryProjects() && !$subID) {
      $errorMsg = "<P ALIGN=\"left\">You can not delete a researcher who is still associated with a project, unless you select a substitute.";
      $errorMsg .= "<P ALIGN=\"left\">To disassociate a researcher from a project, edit the project.  ";
      $errorMsg .= "<A HREF=\"viewResearcher.cgi?ID=$researcherID\">Click here to view associated projects.</A>\n";

   } else {
      # DATA INTEGRITY CHECKED OUT, LET'S DO THE DEED
      if(!$subID) { $subID = "0"; }

      # DELETE THE RESEARCHER
      my($sql) = "DELETE FROM tblResearchers WHERE researcherID = $researcherID";
      $dbh->do($sql);
      $successMsg = "<P>Researcher successfully deleted...\n";

      # SUBSTITUTE ALL INSTANCES OF THIS RESEARCHER WITH subID
      if($subID) {
         $sql = "UPDATE tblProjects SET projectPI = $subID WHERE projectPI = $researcherID";
         $dbh->do($sql);
         $sql = "UPDATE tblProjects SET projectResearcherB = $subID WHERE projectResearcherB = $researcherID";
         $dbh->do($sql);
         $sql = "UPDATE tblProjects SET projectResearcherC = $subID WHERE projectResearcherC = $researcherID";
         $dbh->do($sql);
         $sql = "UPDATE tblProjects SET projectResearcherD = $subID WHERE projectResearcherD = $researcherID";
         $dbh->do($sql);
         $successMsg .= "<P>Researcher successfully substituted with " . $SUB->query('FIRSTNAME') . " " . $SUB->query('LASTNAME') . ".\n";
      }

   }
}


# DO WE HAVE AN ERROR?
if($errorMsg) { YRC::WWW::ERROR->showError($errorMsg); }
elsif($successMsg) { YRC::WWW::ERROR->showSuccess($successMsg); }

print <<END;

<SCRIPT LANGUAGE="JavaScript">
 function confirmDelete() {
    if(confirm('Are you SURE you want to do this?')) {
       document.form1.submit();
       return 1;
    }
    return 0;
  }
</SCRIPT>


<CENTER>

<DIV CLASS="researcher_header" STYLE="width: 600;">
<CENTER>DELETE RESEARCHER:</CENTER>
</DIV>
<DIV CLASS="researcher" STYLE="width: 600;">

<FORM ACTION="deleteResearcher.cgi" METHOD="GET" NAME="form1">
 <INPUT TYPE="HIDDEN" NAME="trunc" VALUE="1">
 <INPUT TYPE="HIDDEN" NAME="action" VALUE="delete">

 <TABLE WIDTH="100%" CELLPADDING="no" CELLSPACING="0">
  <TR>
   <TD>Researcher to be <B>REMOVED</B>:</TD>
   <TD>(Optional) Substitute with:</TD>
  </TR>
END

print "  <TR>\n";
print "   <TD>\n";
print "    <SELECT NAME=\"ID\">\n";
print "     " . showResearcherList($researcherID) . "\n";
print "    </SELECT>\n";
print "   </TD>\n";

print "   <TD>\n";
print "    <SELECT NAME=\"sID\">\n";
print "     " . showResearcherList() . "\n";
print "    </SELECT>\n";
print "   </TD>\n";
print " </TR>\n";

print <<END;
 </TABLE>

<A onClick="confirmDelete()"><IMG SRC="/v2/images/researcher-delete.gif" WIDTH="200" HEIGHT="33" BORDER="0"></A>
END

if($researcherID) {
   print <<END;
&nbsp;&nbsp;<A onClick="javascript:document.location.href='viewResearcher.cgi?ID=$researcherID&trunc=1'">
<IMG SRC="/v2/images/researcher-cancel.gif" WIDTH="200" HEIGHT="33" BORDER="0"></A>
END
}

print <<END;
</FORM>

</DIV>
</CENTER>

END


# PRINT OUR FOOTER
print YRC::WWW::TEMPLATES->getFooter();

sub numerically { $a <=> $b }

sub showResearcherList {
   my($default) = @_;
   my($output, $sql, $sth, @arr);

   $output = "<OPTION VALUE=\"0\">None\n";

   $sql  = "SELECT researcherID, researcherFirstName, researcherLastName, researcherDegree FROM ";
   $sql .= "tblResearchers ORDER BY researcherLastName, researcherFirstName, researcherDegree";

   $sth = $dbh->prepare($sql);
   $sth->execute; 

   while(@arr = $sth->fetchrow_array()) {   
      $output .= "<OPTION VALUE=\"" . $arr[0] . "\"";
      if($default && $arr[0] == $default) { $output .= " SELECTED"; }
      $output .= ">" . $arr[2] . ", " . $arr[1] . " - " . $arr[3] . "\n";
   }
   return $output;
}
   

exit 0;
