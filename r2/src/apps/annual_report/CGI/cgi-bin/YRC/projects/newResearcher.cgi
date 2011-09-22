#!/usr/bin/perl -w

use strict;
use CGI qw(:standard escape);

use lib "/usr/local/lib/site_perl";
use YRC::DB;

use YRC::WWW::SESSION;
use YRC::WWW::LOGIN;
use YRC::WWW::TEMPLATES;
use YRC::WWW::ERROR;
use YRC::WWW::HISTORY;

use YRC::PROJECT::RESEARCHER;

my($query) = new CGI;
my($dbh) = YRC::DB->getDBH();
my($errorMsg);
my($USER);
my($TITLE) = 'Create New Researcher';

# OUR ENTIRE AUTHENTICATION CODE
unless($USER = YRC::WWW::SESSION->getUserObject(YRC::WWW::SESSION->getSessionID($query))) {
   YRC::WWW::LOGIN->showLogin(QUERY=>$query, REQUESTER=>$ENV{REQUEST_URI});
   exit 0;
}


# WHAT ARE WE DOING?
my($action) = $query->param('action');
my(%data) = ();
if($action eq 'add') {
   # WE ARE DDING DATA
   $data{FIRSTNAME}	=	$query->param('FIRSTNAME');
   $data{LASTNAME}	=	$query->param('LASTNAME');
   $data{EMAIL}		=	$query->param('EMAIL');
   $data{DEPT}		=	$query->param('DEPT');
   $data{ORGANIZATION}	=	$query->param('ORGANIZATION');
   $data{DEGREE}	=	$query->param('DEGREE');
   $data{STATE}		=	$query->param('STATE');
   $data{ZIP}		=	$query->param('ZIP');
   $data{COUNTRY}	=	$query->param('COUNTRY');

   if(!$data{FIRSTNAME} || !$data{LASTNAME}) {
      $errorMsg = "You must enter a first and last name.";
   } elsif(!$data{EMAIL}) {
      $errorMsg = "You must enter an email address.";
   } else {
      # DATA PASSED OUR DATA INTEGRITY CHECKS

      # VERIFY THAT NO OTHER RESEARCHER W/ THIS NAME & EMAIL EXISTS
      my($sql) = "SELECT researcherID FROM tblResearchers WHERE ";
      $sql .= "LCASE(researcherFirstName) = LCASE('" . $data{FIRSTNAME} . "') AND ";
      $sql .= "LCASE(researcherLastName) = LCASE('" . $data{LASTNAME} . "') AND ";
      $sql .= "LCASE(researcherEmail) = LCASE('" . $data{EMAIL} . "')";

      my($sth) = $dbh->prepare($sql);
      $sth->execute;
      if($sth->rows) {
         my(@arr) = $sth->fetchrow_array;
         # ANOTHER RESEARCHER W/ THIS DATA ALREADY EXISTS UNDER A DIFFERENT ID
         $errorMsg = "Another researcher with that name and email address already exists.  \n";
         $errorMsg .= "<P><A HREF=\"viewResearcher.cgi?ID=" . $arr[0] . "\">Click here</A> to view other researcher.\n";
      } else {
         # SAVE RESEARCHER
         my($RESEARCHER) = YRC::PROJECT::RESEARCHER->new();
         $RESEARCHER->setDBH($dbh);
         $RESEARCHER->set(%data);

         unless($RESEARCHER->save()) {
            $errorMsg = "There was an error saving researcher data (" . $RESEARCHER->error() . ").\n";
         } else {
            # REDIRECT TO VIEW PAGE
            print $query->redirect('viewResearcher.cgi?ID=' . $RESEARCHER->query('ID'));
         }
      }
   }
}


# PRINT OUR HEADER
print YRC::WWW::TEMPLATES->getHeader($TITLE, 1, $USER);

my($HISTORY) = YRC::WWW::HISTORY->new( (SESSIONID=>$USER->query('SESSIONID')) ); 
$HISTORY->setHistory( ( TITLE=>$TITLE, LINK=>$query->url(-full=>1,-query=>1), TRUNC=>$query->param('trunc') ) );
print $HISTORY->getHistoryNav();

# WHO DO WE WANT TO HAVE ACCESS TO THIS PAGE?
# $USER->checkGroup( NUMBER=>1 );

# DO WE HAVE AN ERROR?
if($errorMsg) { YRC::WWW::ERROR->showError($errorMsg); }

print <<END;
<CENTER>

<DIV CLASS="researcher_header" STYLE="width: 450;">
<CENTER>CREATE NEW RESEARCHER:</CENTER>
</DIV>
<DIV CLASS="researcher" STYLE="width: 450;">

<FORM ACTION="newResearcher.cgi" METHOD="GET">
 <INPUT TYPE="HIDDEN" NAME="action" VALUE="add">
 <INPUT TYPE="HIDDEN" NAME="trunc" VALUE="1">

<TABLE WIDTH="100%" CELLPADDING="no" CELLSPACING="0">
END

print " <TR>\n";
print "  <TD VALIGN=\"top\">First Name:</TD>\n";
print "  <TD VALIGN=\"top\">";
print "   <INPUT TYPE=\"TEXT\" NAME=\"FIRSTNAME\" SIZE=\"20\" MAXLENGTH=\"30\" VALUE=\"" . $data{FIRSTNAME} . "\">";
print "  </TD>\n";
print " </TR>\n";

print " <TR>\n";
print "  <TD VALIGN=\"top\">Last Name:</TD>\n";
print "  <TD VALIGN=\"top\">";
print "   <INPUT TYPE=\"TEXT\" NAME=\"LASTNAME\" SIZE=\"20\" MAXLENGTH=\"30\" VALUE=\"" . $data{LASTNAME} . "\">";
print "  </TD>\n";
print " </TR>\n";

print " <TR>\n";
print "  <TD VALIGN=\"top\">Degree:</TD>\n";
print "  <TD VALIGN=\"top\">";
print "   <INPUT TYPE=\"TEXT\" NAME=\"DEGREE\" SIZE=\"5\" MAXLENGTH=\"10\" VALUE=\"" . $data{DEGREE} . "\">";
print "  </TD>\n";
print " </TR>\n";

print " <TR>\n";
print "  <TD VALIGN=\"top\">Email:</TD>\n";
print "  <TD VALIGN=\"top\">";
print "   <INPUT TYPE=\"TEXT\" NAME=\"EMAIL\" SIZE=\"30\" MAXLENGTH=\"50\" VALUE=\"" . $data{EMAIL} . "\">";
print "  </TD>\n";
print " </TR>\n";

print " <TR>\n";
print "  <TD VALIGN=\"top\">Department:</TD>\n";
print "  <TD VALIGN=\"top\">";
print "   <INPUT TYPE=\"TEXT\" NAME=\"DEPT\" SIZE=\"30\" MAXLENGTH=\"50\" VALUE=\"" . $data{DEPT} . "\">";
print "  </TD>\n";
print " </TR>\n";

print " <TR>\n";
print "  <TD VALIGN=\"top\">Organization:</TD>\n";
print "  <TD VALIGN=\"top\">";
print "   <INPUT TYPE=\"TEXT\" NAME=\"ORGANIZATION\" SIZE=\"30\" MAXLENGTH=\"60\" VALUE=\"" . $data{ORGANIZATION} . "\">";
print "  </TD>\n";
print " </TR>\n";

print " <TR>\n";
print "  <TD VALIGN=\"top\">State:</TD>\n";
print "  <TD VALIGN=\"top\">";
print "   <INPUT TYPE=\"TEXT\" NAME=\"STATE\" SIZE=\"3\" MAXLENGTH=\"2\" VALUE=\"" . $data{STATE} . "\">";
print "  </TD>\n";
print " </TR>\n";

print " <TR>\n";
print "  <TD VALIGN=\"top\">Zip Code:</TD>\n";
print "  <TD VALIGN=\"top\">";
print "   <INPUT TYPE=\"TEXT\" NAME=\"ZIP\" SIZE=\"8\" MAXLENGTH=\"11\" VALUE=\"" . $data{ZIP} . "\">";
print "  </TD>\n";
print " </TR>\n";

print " <TR>\n";
print "  <TD VALIGN=\"top\">Country:</TD>\n";
print "  <TD VALIGN=\"top\">";
print "   <INPUT TYPE=\"TEXT\" NAME=\"COUNTRY\" SIZE=\"20\" MAXLENGTH=\"40\" VALUE=\"" . $data{COUNTRY} . "\">";
print "  </TD>\n";
print " </TR>\n";


print <<END;
</TABLE>

<P>
  <INPUT TYPE="IMAGE" BORDER="0" SRC="/v2/images/researcher-save.gif" WIDTH="200" HEIGHT="33">&nbsp;&nbsp;
 </FORM>
</P>
</FORM>
</DIV>


</CENTER>
END

# PRINT OUR FOOTER
print YRC::WWW::TEMPLATES->getFooter();

sub numerically { $a <=> $b }

# STRIPS QUOTE OFF OF A STRING, IF QUOTED
# RETURNS THE STRING SANS QUOTES
sub deQuote {
   my($str) = @_;

   # SNIP OUT THE QUOTES
   if(substr($str, 0, 1) eq "\"") { substr($str, 0, 1, ""); }
   if(substr($str, -1, 1) eq "\"") {substr($str, -1, 1, ""); }
  
   return $str;
}

exit 0;
