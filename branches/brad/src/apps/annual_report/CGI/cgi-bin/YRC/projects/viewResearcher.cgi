#!/usr/bin/perl -w

use strict;
use CGI qw(:standard escape);

use lib "/usr/local/lib/site_perl";
use YRC::DB;
use YRC::WWW::SESSION;
use YRC::WWW::LOGIN;
use YRC::WWW::TEMPLATES;
use YRC::WWW::HISTORY;
use YRC::PROJECT::PROJECT;
use YRC::PROJECT::RESEARCHER;

my($query) = new CGI;
my($USER);
my($TITLE) = 'View Researcher Details';

# FORCE IT TO BE SECURE
#YRC::WWW::SESSION->enforceSecure($query);

# OUR ENTIRE AUTHENTICATION CODE
unless($USER = YRC::WWW::SESSION->getUserObject(YRC::WWW::SESSION->getSessionID($query))) {
   YRC::WWW::LOGIN->showLogin(QUERY=>$query, REQUESTER=>$ENV{REQUEST_URI});
   exit 0;
}

# PRINT OUR HEADER
print YRC::WWW::TEMPLATES->getHeader($TITLE, 1, $USER);

my($HISTORY) = YRC::WWW::HISTORY->new( (SESSIONID=>$USER->query('SESSIONID')) ); 
$HISTORY->setHistory( ( TITLE=>$TITLE, LINK=>$query->url(-full=>1,-query=>1), TRUNC=>$query->param('trunc') ) );
print $HISTORY->getHistoryNav();

my($PROJECT);
my($researcherID) = $query->param('ID');
my($RESEARCHER) = YRC::PROJECT::RESEARCHER->new();
my($dbh) = YRC::DB->getDBH();
$RESEARCHER->setDBH($dbh);
$RESEARCHER->load($researcherID);

unless(!$RESEARCHER->query('FIRSTNAME') && !$RESEARCHER->query('LASTNAME') &&!$RESEARCHER->query('EMAIL')) {
   print <<END;

<CENTER>

<DIV CLASS="researcher_header" STYLE="width: 450;">
<CENTER>RESEARCHER DETAILS:</CENTER>
</DIV>
<DIV CLASS="researcher" STYLE="width: 450;">

<TABLE WIDTH="100%" CELLPADDING="no" CELLSPACING="0">
END

print " <TR>\n";
print "  <TD VALIGN=\"top\">Name:</TD>\n";
print "  <TD VALIGN=\"top\">" . $RESEARCHER->query('FIRSTNAME') . " " . $RESEARCHER->query('LASTNAME') . "</TD>\n";
print " </TR>\n";

print " <TR BGCOLOR=\"#FFFA9B\">\n";
print "  <TD VALIGN=\"top\">Degree:</TD>\n";
print "  <TD VALIGN=\"top\">" . $RESEARCHER->query('DEGREE') . "&nbsp;</TD>\n";
print " </TR>\n";

print " <TR>\n";
print "  <TD VALIGN=\"top\">Email:</TD>\n";
print "  <TD VALIGN=\"top\"><A HREF=\"mailto:" . $RESEARCHER->query('EMAIL') . "\">" . $RESEARCHER->query('EMAIL') . "</A></TD>\n";
print " </TR>\n";

print " <TR BGCOLOR=\"#FFFA9B\">\n";
print "  <TD VALIGN=\"top\">Department:</TD>\n";
print "  <TD VALIGN=\"top\">" . $RESEARCHER->query('DEPT') . "</TD>\n";
print " </TR>\n";

print " <TR>\n";
print "  <TD VALIGN=\"top\">Organization:</TD>\n";
print "  <TD VALIGN=\"top\">" . $RESEARCHER->query('ORGANIZATION') . "</TD>\n";
print " </TR>\n";

print " <TR BGCOLOR=\"#FFFA9B\">\n";
print "  <TD>State:</TD>\n";
print "  <TD>" . $RESEARCHER->query('STATE') . "</TD>\n";
print " </TR>\n";

print " <TR>\n";
print "  <TD>Zip Code:</TD>\n";
print "  <TD>" . $RESEARCHER->query('ZIP') . "</TD>\n";
print " </TR>\n";

print " <TR BGCOLOR=\"#FFFA9B\">\n";
print "  <TD>Country:</TD>\n";
print "  <TD>" . $RESEARCHER->query('COUNTRY') . "</TD>\n";
print " </TR>\n";

print <<END;
</TABLE>
END

# IF THEY'RE A MEMBER OF THE ADMIN GROUP
if($USER->queryAccessGroupMembership( 1 )) {
   print <<END;
<P>
 <TABLE BORDER="0" CELLPADDING="NO" CELLSPACING="0">
  <TR>
   <TD>
    <FORM ACTION="editResearcher.cgi" METHOD="GET">
     <INPUT TYPE="HIDDEN" NAME="ID" VALUE="$researcherID">
     <INPUT TYPE="IMAGE" BORDER="0" SRC="/v2/images/researcher-edit.gif" WIDTH="200" HEIGHT="33">
    </FORM>
   </TD>
   <TD>
    <FORM ACTION="deleteResearcher.cgi" METHOD="GET">
     <INPUT TYPE="HIDDEN" NAME="ID" VALUE="$researcherID">
     <INPUT TYPE="IMAGE" BORDER="0" SRC="/v2/images/researcher-delete.gif" WIDTH="200" HEIGHT="33">
    </FORM>
   </TD>
  </TR>
 </TABLE>
</P>
END
}

print <<END;
</DIV>


<P>
<DIV CLASS="project_header" STYLE="width: 450;">
<CENTER>ASSOCIATED PROJECTS:</CENTER>
</DIV>
<DIV CLASS="project" STYLE="width: 450;">
END

print <<END;
<TABLE WIDTH="100%" CELLPADDING="no" CELLSPACING="0">
 <TR>
  <TD WIDTH="10%"><B><U>ID</U></B></TD>
  <TD WIDTH="10%"><B><U>TYPE</U></B></TD>
  <TD WIDTH="80%" ALIGN="center"><B><U>TITLE</U></B></TD>
 </TR>
END
my($counter) = 0;
foreach (sort numerically ($RESEARCHER->queryProjects())) {
   $PROJECT = YRC::PROJECT::PROJECT->new();
   $PROJECT->setDBH($dbh);
   $PROJECT->load($_);

   if(!$PROJECT->query('ID')) { next; }

   print " <TR";
   if((++$counter) % 2) { print " BGCOLOR=\"#eee2ff\""; }
   print ">\n";
   print "  <TD VALIGN=\"top\"><A HREF=\"viewProject.cgi?ID=$_\">" . $_ . "</A>&nbsp;</TD>\n";
   print "  <TD VALIGN=\"top\">" . $PROJECT->query('TYPE') . "&nbsp;</TD>\n";
   print "  <TD VALIGN=\"top\">" . $PROJECT->query('TITLE') . "&nbsp;</TD>\n";
   print " </TR>\n";
}

print <<END;
</TABLE>

</DIV>


</CENTER>
END
} else {
   print <<END;
<CENTER>
<DIV CLASS="researcher_header" STYLE="width: 400;">
<CENTER>RESEARCHER DETAILS:</CENTER>
</DIV>
<DIV CLASS="researcher" STYLE="width: 400;">
<CENTER>No researcher with that ID ($researcherID) exists in the database.<BR>Perhaps they were deleted recently?</CENTER>
</DIV>
</CENTER>
END
}

# PRINT OUR FOOTER
print YRC::WWW::TEMPLATES->getFooter();

sub numerically { $a <=> $b }

exit 0;
