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
my($TITLE) = 'Project Details';

# FORCE IT TO BE SECURE
#YRC::WWW::SESSION->enforceSecure($query);

# OUR ENTIRE AUTHENTICATION CODE
unless($USER = YRC::WWW::SESSION->getUserObject(YRC::WWW::SESSION->getSessionID($query))) {
   YRC::WWW::LOGIN->showLogin(QUERY=>$query, REQUESTER=>$ENV{REQUEST_URI});
   exit 0;
}

# PRINT OUR HEADER
print YRC::WWW::TEMPLATES->getHeader($TITLE,1, $USER);

my($HISTORY) = YRC::WWW::HISTORY->new( (SESSIONID=>$USER->query('SESSIONID')) ); 
$HISTORY->setHistory( ( TITLE=>$TITLE, LINK=>$query->url(-full=>1,-query=>1), TRUNC=>$query->param('trunc') ) );
print $HISTORY->getHistoryNav();

# INITIALIZE OUR VARIABLES
my($PROJECT, $PI, $RESEARCHER);
my($rowCounter) = 0;
my($projectID) = $query->param('ID');
my($dbh) = YRC::DB->getDBH();

$PROJECT = YRC::PROJECT::PROJECT->new();
$PROJECT->setDBH($dbh);
$PROJECT->load($projectID) || $PROJECT->set('NOTFOUND', 1);

unless($PROJECT->query('NOTFOUND')) {
   print <<END;

<CENTER>

<DIV CLASS="project_header" STYLE="width: 600;">
<CENTER>PROJECT DETAILS:</CENTER>
</DIV>
<DIV CLASS="project" STYLE="width: 600;">

<TABLE BORDER="0" CELLPADDING="no" CELLSPACING="0" WIDTH="100%">
END

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">ID:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->query('ID') . "</TD>\n";
print " </TR>\n";

if($PROJECT->query('TITLE')) {
   print getRowTag();
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Title:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->query('TITLE') . "</TD>\n";
   print " </TR>\n";
}

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Type:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->getTypeString($PROJECT->query('TYPE')) . "&nbsp;</TD>\n";
print " </TR>\n";

if($PROJECT->getTypeString($PROJECT->query('TYPE')) =~ /Collaboration/) {
   print getRowTag();
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Coll. Groups:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->getGroupString($PROJECT->queryGroups('C')) . "&nbsp;</TD>\n";
   print " </TR>\n";
}

if($PROJECT->getTypeString($PROJECT->query('TYPE')) =~ /Technology Development/) {
   print getRowTag();
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Tech. Dev. Groups:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->getGroupString($PROJECT->queryGroups('Tech')) . "&nbsp;</TD>\n";
   print " </TR>\n";
}

# DISPLAY TRAINING INFORMATION IF AVAILABLE
if($PROJECT->getTypeString($PROJECT->query('TYPE')) =~ /Training/) {
   print getRowTag();
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Training Groups:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->getGroupString($PROJECT->queryGroups('T')) . "</TD>\n";
   print " </TR>\n";
   print getRowTag();
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Training Desc.:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->toHTML($PROJECT->queryTraining('DESCRIPTION')) . "&nbsp;</TD>\n";
   print " </TR>\n";
   if($PROJECT->queryTraining('HOURS')) {
      print getRowTag();
      print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Training Hours:</TD>\n";
      print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->queryTraining('HOURS') . "&nbsp;</TD>\n";
      print " </TR>\n";
   }
   if($PROJECT->queryTraining('DAYS')) {
      print getRowTag();
      print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Training Days:</TD>\n";
      print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->queryTraining('DAYS') . "&nbsp;</TD>\n";
      print " </TR>\n";
   }
}

$PI = YRC::PROJECT::RESEARCHER->new();
$PI->setDBH($dbh);
$PI->load($PROJECT->query('PI'));
print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">PI:</TD>\n";
if($PI->query('ID')) {
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
   print "   <A HREF=\"viewResearcher.cgi?ID=" . $PI->query('ID') . "\">";
   print $PI->query('FIRSTNAME')  . " " . $PI->query('LASTNAME') . ", " . $PI->query('DEGREE');
   print "</A></TD>\n";
} else {
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">None on file.</TD>\n";
}
print " </TR>\n";

$RESEARCHER = YRC::PROJECT::RESEARCHER->new();
$RESEARCHER->setDBH($dbh);
$RESEARCHER->load($PROJECT->query('RESEARCHERB'));
print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Researcher B:</TD>\n";
if($RESEARCHER->query('ID')) {
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
   print "   <A HREF=\"viewResearcher.cgi?ID=" . $RESEARCHER->query('ID') . "\">";
   print $RESEARCHER->query('FIRSTNAME')  . " " . $RESEARCHER->query('LASTNAME') . ", " . $RESEARCHER->query('DEGREE');
   print "</A></TD>\n";
} else {
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">None on file.</TD>\n";
}
print " </TR>\n";

$RESEARCHER = YRC::PROJECT::RESEARCHER->new();
$RESEARCHER->setDBH($dbh);
$RESEARCHER->load($PROJECT->query('RESEARCHERC'));
print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Researcher C:</TD>\n";
if($RESEARCHER->query('ID')) {
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
   print "   <A HREF=\"viewResearcher.cgi?ID=" . $RESEARCHER->query('ID') . "\">";
   print $RESEARCHER->query('FIRSTNAME')  . " " . $RESEARCHER->query('LASTNAME') . ", " . $RESEARCHER->query('DEGREE');
   print "</A></TD>\n";
} else {
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">None on file.</TD>\n";
}
print " </TR>\n";

$RESEARCHER = YRC::PROJECT::RESEARCHER->new();
$RESEARCHER->setDBH($dbh);
$RESEARCHER->load($PROJECT->query('RESEARCHERD'));
print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Researcher D:</TD>\n";
if($RESEARCHER->query('ID')) {
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
   print "   <A HREF=\"viewResearcher.cgi?ID=" . $RESEARCHER->query('ID') . "\">";
   print $RESEARCHER->query('FIRSTNAME')  . " " . $RESEARCHER->query('LASTNAME') . ", " . $RESEARCHER->query('DEGREE');
   print "</A></TD>\n";
} else {
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">None on file.</TD>\n";
}
print " </TR>\n";

if($PROJECT->query('PI')) {
   print getRowTag();
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Organization:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PI->query('ORGANIZATION') . "&nbsp;</TD>\n";
   print " </TR>\n";
}

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Funding Source(s):</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->query('FUNDINGTYPES') . "&nbsp;</TD>\n";
print " </TR>\n";

if($PROJECT->query('FUNDINGTYPES') =~ /FEDERAL/) {
print getRowTag();
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Federal Funding:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->query('FUNDINGFEDERAL') . "&nbsp;</TD>\n";
   print " </TR>\n";
}

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">% Effort (BTA):</TD>\n";
if($PROJECT->query('BTA')) {
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->query('BTA') . "%&nbsp;</TD>\n";
} else {
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">NOT ENTERED</TD>\n";
}
print " </TR>\n";


if($PROJECT->query('AXISI')) {
   print getRowTag();
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">AXIS I:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->query('AXISI') . "&nbsp;</TD>\n";
   print " </TR>\n";
}

if($PROJECT->query('AXISII')) {
   print getRowTag();
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">AXIS II:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->query('AXISII') . "&nbsp;</TD>\n";
   print " </TR>\n";
}


if($PROJECT->query('ABSTRACT')) {
   print getRowTag();
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Abstract:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->toHTML($PROJECT->query('ABSTRACT')) . "&nbsp;</TD>\n";
   print " </TR>\n";
}

if($PROJECT->query('PROGRESS')) {
   print getRowTag();
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Progress/Results:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->toHTML($PROJECT->query('PROGRESS')) . "&nbsp;</TD>\n";
   print " </TR>\n";
}

if($PROJECT->query('KEYWORDS')) {
   print getRowTag();
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Keywords:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->toHTML($PROJECT->query('KEYWORDS')) . "&nbsp;</TD>\n";
   print " </TR>\n";
}

if($PROJECT->query('PUBLICATIONS')) {
   print getRowTag();
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Publications:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->toHTML($PROJECT->query('PUBLICATIONS')) . "&nbsp;</TD>\n";
   print " </TR>\n";
}

if($PROJECT->query('COMMENTS')) {
   print getRowTag();
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Comments:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->toHTML($PROJECT->query('COMMENTS')) . "&nbsp;</TD>\n";
   print " </TR>\n";
}

if($PROJECT->query('SUBMITDATE')) {
   print getRowTag();   
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Submit Date:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->query('SUBMITDATE') . "&nbsp;</TD>\n";
   print " </TR>\n";
}

print <<END;
</TABLE>

<P>

<SCRIPT LANGUAGE="JavaScript">
 function confirmDelete() {
    if(confirm("Are you sure you want to delete this project?")) {
       if(confirm("Are you ABSOLUTELY sure you want to delete this project?")) {
          document.form1.submit();
          return 1;
       }
    }
    return 0;
 }
</SCRIPT>

<TABLE BORDER="0" CELLPADDING="NO" CELLSPACING="0">
 <TR>
END

# LIMIT TO MEMBERS OF THIS PROJECT'S GROUP(S)
if($USER->queryProjectAccess( $PROJECT )) {
   print <<END;
  <TD>
   <FORM ACTION="editProject.cgi" METHOD="GET">
    <INPUT TYPE="HIDDEN" NAME="ID" VALUE="$projectID">
    <INPUT TYPE="IMAGE" SRC="/v2/images/project-edit.gif" WIDTH="200" HEIGHT="33">
   </FORM>
  </TD>
END
}

# LIMIT THIS TO ADMINS
if($USER->queryAccessGroupMembership( 1 )) {
   print <<END;
  <TD>
   <FORM ACTION="deleteProject.cgi" METHOD="GET" NAME="form1">
    <INPUT TYPE="HIDDEN" NAME="ID" VALUE="$projectID">
    <INPUT TYPE="HIDDEN" NAME="action" VALUE="delete">
    &nbsp;&nbsp;<A onClick="confirmDelete()"><IMG SRC="/v2/images/project-delete.gif" WIDTH="200" HEIGHT="33" BORDER="0"></A>
   </FORM>
  </TD>
END
}

print <<END;
 </TR>
</TABLE>

</DIV>
END

if($PROJECT->getTypeString($PROJECT->query('TYPE')) =~ /Dissemination/) {
   $rowCounter = 0;
   print <<END;
<P><DIV CLASS="dissemination_header" STYLE="width: 600;">
<CENTER>DISSEMINATION DETAILS:</CENTER>
</DIV>
<DIV CLASS="dissemination" STYLE="width: 600;">

<TABLE BORDER="0" CELLPADDING="no" CELLSPACING="0" WIDTH="100%">
END

   print getRowTag('D');
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Plasmid Type:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->getPlasmidString($PROJECT->queryDissemination('PLASMIDTYPE')) . "</TD>\n";
   print " </TR>\n";

   print getRowTag('D');
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Name:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->queryDissemination('NAME') . "</TD>\n";
   print " </TR>\n";

   print getRowTag('D');
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Phone:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->queryDissemination('PHONE') . "</TD>\n";
   print " </TR>\n";

   print getRowTag('D');
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Email:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->queryDissemination('EMAIL') . "</TD>\n";
   print " </TR>\n";

   print getRowTag('D');
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Address:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->toHTML($PROJECT->queryDissemination('ADDRESS')) . "</TD>\n";
   print " </TR>\n";

   if($PROJECT->queryDissemination('FEDEX')) {
      print getRowTag('D');
      print "  <TD VALIGN=\"top\" WIDTH=\"25%\">FEDEX #:</TD>\n";
      print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->queryDissemination('FEDEX') . "</TD>\n";
      print " </TR>\n";
   }

   print getRowTag('D');
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Description:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->toHTML($PROJECT->queryDissemination('DESCRIPTION')) . "</TD>\n";
   print " </TR>\n";

   if($PROJECT->queryDissemination('COMMENTS')) {
      print getRowTag('D');
      print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Comments:</TD>\n";
      print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->toHTML($PROJECT->queryDissemination('COMMENTS')) . "</TD>\n";
      print " </TR>\n";
   }

   print getRowTag('D');
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Commercial Use:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->queryDissemination('COMMERCIAL') . "</TD>\n";
   print " </TR>\n";

   print getRowTag('D');
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Plasmid Shipped:</TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . $PROJECT->queryDissemination('SHIPPED') . "</TD>\n";
   print " </TR>\n";

   print <<END;
</TABLE>
</DIV>
END
}

print <<END;
</CENTER>
END

} else {
   print <<END;
<CENTER>
<DIV CLASS="project_header" STYLE="width: 400;">
<CENTER>PROJECT DETAILS:</CENTER>
</DIV>
<DIV CLASS="project" STYLE="width: 400;">
<CENTER>No project with that ID ($projectID) exists in the database.<BR>Perhaps it was deleted recently?</CENTER>
</DIV>
</CENTER>
END
}




# PRINT OUR FOOTER
print YRC::WWW::TEMPLATES->getFooter();

sub getRowTag {
   my($type) = $_[0];
   if(!$type) { $type = ''; }
   my($output);

   $output = "<TR";
   if($rowCounter % 2) {
      if($type eq 'D') { $output .= " BGCOLOR=\"FFBC9B\""; }
      else { $output .= " BGCOLOR=\"EEE2FF\""; }
   }
   $output .= ">";

   $rowCounter++;
   return $output;
}

sub numerically { $a <=> $b }

exit 0;
