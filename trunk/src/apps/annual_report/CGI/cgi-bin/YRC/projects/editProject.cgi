#!/usr/bin/perl -w

use strict;
use CGI qw(:standard escape);

use lib "/usr/local/lib/site_perl";
use YRC::DB;
use YRC::WWW::SESSION;
use YRC::WWW::LOGIN;
use YRC::WWW::FORM;
use YRC::WWW::ERROR;
use YRC::WWW::TEMPLATES;
use YRC::WWW::HISTORY;
use YRC::PROJECT::PROJECT;
use YRC::PROJECT::RESEARCHER;

my($query) = new CGI;
my($USER);
my($TITLE) = 'Edit Project Data';

# FORCE IT TO BE SECURE
YRC::WWW::SESSION->enforceSecure($query);

# OUR ENTIRE AUTHENTICATION CODE
unless($USER = YRC::WWW::SESSION->getUserObject(YRC::WWW::SESSION->getSessionID($query))) {
   YRC::WWW::LOGIN->showLogin(QUERY=>$query, REQUESTER=>$ENV{REQUEST_URI});
   exit 0;
}

# INITIALIZE OUR VARIABLES
my($projectID) = $query->param('ID');
my($errorMsg);
my($PROJECT, $PI, $RESEARCHER);
my($rowCounter) = 0;
my($dbh) = YRC::DB->getDBH();

$PROJECT = YRC::PROJECT::PROJECT->new();
$PROJECT->setDBH($dbh);
$PROJECT->load($projectID) || $PROJECT->set('NOTFOUND', 1);

if( $query->param('action') ) {
if( $query->param('action') eq 'edit' && !( $PROJECT->query( 'NOTFOUND' ) ) ) {
   # RETRIEVE OUR CGI PASSED PARAMETERS
   my(%project, %training, %collaboration, %dissemination, %technology);

   # PROJECT DATA
   $project{TITLE} = $query->param('TITLE');
   $project{PI} = $query->param('PI');
   $project{RESEARCHERB} = $query->param('RESEARCHERB');
   $project{RESEARCHERC} = $query->param('RESEARCHERC');
   $project{RESEARCHERD} = $query->param('RESEARCHERD');
   $project{FUNDINGTYPES} = join(",", ($query->param('FUNDINGTYPES')));
   $project{FUNDINGFEDERAL} = join(",", ($query->param('FUNDINGFEDERAL')));
   $project{BTA} = $query->param('BTA');
   $project{AXISI} = $query->param('AXISI');
   $project{AXISII} = $query->param('AXISII');
   if(!$project{BTA}) { $project{BTA} = 'NULL'; }
   $project{ABSTRACT} = $query->param('ABSTRACT');
   $project{PROGRESS} = $query->param('PROGRESS');
   $project{KEYWORDS} = $query->param('KEYWORDS');
   $project{PUBLICATIONS} = $query->param('PUBLICATIONS');
   $project{COMMENTS} = $query->param('COMMENTS');

   # SET THE PROJECT TYPE(S)
   my(@tmparr) = ();
   if($query->param('CGROUPS')) { push(@tmparr, ('C')); }
   if($query->param('techGROUPS')) { push(@tmparr, ('Tech')); }
   if($query->param('TGROUPS')) { push(@tmparr, ('T')); }
   if($query->param('DPLASMIDTYPE')) { push(@tmparr, ('D')); }
   $project{TYPE} = join(",", @tmparr);

   # COLLABORATION DATA
   $collaboration{GROUPS} = join(",", ($query->param('CGROUPS')));

   # TECHNOLOGY DATA
   $technology{GROUPS} = join(",", ($query->param('techGROUPS')));

   # TRAINING DATA
   $training{GROUPS} = join(",", ($query->param('TGROUPS')));
   $training{HOURS} = $query->param('THOURS');
   $training{DAYS} = $query->param('TDAYS');
   $training{DESCRIPTION} = $query->param('TDESC');

   # DISSEMINATION DATA
   @tmparr = $query->param('DPLASMIDTYPE');
   if(@tmparr > 1) { $dissemination{PLASMIDTYPE} = 'MT'; }
   else { $dissemination{PLASMIDTYPE} = $tmparr[0]; }

   $dissemination{NAME} = $query->param('DNAME');
   $dissemination{PHONE} = $query->param('DPHONE');
   $dissemination{EMAIL} = $query->param('DEMAIL');
   $dissemination{ADDRESS} = $query->param('DADDRESS');
   $dissemination{DESCRIPTION} = $query->param('DDESC');
   $dissemination{FEDEX} = $query->param('DFEDEX');
   $dissemination{COMMENTS} = $query->param('DCOMMENTS');
   $dissemination{COMMERCIAL} = $query->param('DCOMMERCIAL');
   $dissemination{SHIPPED} = $query->param('DSHIPPED');


   # VALIDATE OUR DATA BEFORE SAVING
   if(!$project{TYPE}) {
      $errorMsg = "You must select a YRC group for this project, or a plasmid type for dissemination.";
   } elsif($project{BTA} ne 'NULL' && !($project{BTA} =~ /^[\d]*[\.]{0,1}[\d]*$/)) {
      $errorMsg = "Your project BTA must be a number or a decimal.";
   } else {
      # DATA PASSES INSPECTION, LET'S SAVE IT
      $PROJECT->set(%project);
      $PROJECT->setTraining(%training);
      $PROJECT->setCollaboration(%collaboration);
      $PROJECT->setTechnology(%technology);
      $PROJECT->setDissemination(%dissemination);

      if(!($PROJECT->save())) {
         $errorMsg = "Project NOT saved.  Error returned:  '" . $PROJECT->error() . "'";
      } else {
         # PROJECT SUCCESSFULLY CHANGED, REDIRECT TO PROJECT DETAILS PAGE
         print $query->redirect("viewProject.cgi?ID=$projectID");
         exit 0;
      }
   }
}
}

# PRINT OUR HEADER
print YRC::WWW::TEMPLATES->getHeader($TITLE,1, $USER);

my($HISTORY) = YRC::WWW::HISTORY->new( (SESSIONID=>$USER->query('SESSIONID')) ); 
$HISTORY->setHistory( ( TITLE=>$TITLE, LINK=>$query->url(-full=>1,-query=>1), TRUNC=>$query->param('trunc') ) );
print $HISTORY->getHistoryNav();

# WHO DO WE WANT TO HAVE ACCESS TO THIS PAGE?
$USER->restrictToProjectGroup($PROJECT);

# DO WE HAVE AN ERROR?
if($errorMsg) { YRC::WWW::ERROR->showError($errorMsg); }

unless($PROJECT->query('NOTFOUND')) {
   print <<END;

<CENTER>

<DIV CLASS="project_header" STYLE="width: 950;">
<CENTER>EDIT PROJECT:</CENTER>
</DIV>
<DIV CLASS="project" STYLE="width: 950;">

<!--
	CODE FOR OUR POP UP WINDOW FOR HANDLING AXIS CHANGES
-->
<SCRIPT LANGUAGE="JavaScript">

function openAXISWindow(type) {
 var AXISI_WIN, AXISII_WIN;
 var doc = "chooseAXIS.cgi?ID=$projectID&type=" + type;

 if(type == "I") {
    AXISI_WIN = window.open(doc, "AXISI_WIN",
                                  "width=750,height=550,status=no,resizable=yes,scrollbars");
 } else if(type == "II") {
    AXISI_WIN = window.open(doc, "AXISII_WIN",
                                  "width=750,height=550,status=no,resizable=yes,scrollbars");
 }
}

</SCRIPT>


<FORM ACTION="editProject.cgi" METHOD="POST" NAME="form1">
 <INPUT TYPE="HIDDEN" NAME="action" VALUE="edit">
 <INPUT TYPE="HIDDEN" NAME="ID" VALUE="$projectID">
 <INPUT TYPE="HIDDEN" NAME="trunc" VALUE="1">

<TABLE BORDER="0" CELLPADDING="yes" CELLSPACING="10" WIDTH="100%">
 <TR>
  <TD WIDTH="50%" VALIGN="top">
   <TABLE BORDER="0" CELLPADDING="no" CELLSPACING="0" WIDTH="100%">
    <TR><TD COLSPAN="2" ALIGN="CENTER">
     <DIV CLASS="project_header" STYLE="width: 100%;"><CENTER>GENERAL PROJECT DATA</CENTER></DIV>
    </TD></TR>
END



print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\"><B>ID:</B></TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\"><B>" . $PROJECT->query('ID') . "</B></TD>\n";
print " </TR>\n";

if($PROJECT->query('SUBMITDATE')) {
   print getRowTag();
   print "  <TD VALIGN=\"top\" WIDTH=\"25%\"><B>Submit Date:</B></TD>\n";
   print "  <TD VALIGN=\"top\" WIDTH=\"75%\"><B>" . $PROJECT->query('SUBMITDATE') . "</B></TD>\n";
   print " </TR>\n";
}

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Title:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
print "<INPUT TYPE=\"TEXT\" NAME=\"TITLE\" SIZE=\"45\" MAXLENGTH=\"255\" VALUE=\"" . deQuote($PROJECT->query('TITLE')) . "\">\n";
print "  </TD>\n";
print " </TR>\n";

$PI = YRC::PROJECT::RESEARCHER->new();
$PI->setDBH($dbh);
$PI->load($PROJECT->query('PI'));
print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">PI:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
print "<SELECT NAME=\"PI\">" . showResearcherList($PI->query('ID')) . "</SELECT>\n";
print "  </TD>\n";
print " </TR>\n";

$RESEARCHER = YRC::PROJECT::RESEARCHER->new();
$RESEARCHER->setDBH($dbh);
$RESEARCHER->load($PROJECT->query('RESEARCHERB'));
print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Researcher B:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
print "<SELECT NAME=\"RESEARCHERB\">" . showResearcherList($RESEARCHER->query('ID')) . "</SELECT>\n";
print "  </TD>\n";  
print " </TR>\n";

$RESEARCHER = YRC::PROJECT::RESEARCHER->new();
$RESEARCHER->setDBH($dbh);
$RESEARCHER->load($PROJECT->query('RESEARCHERC'));
print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Researcher C:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
print "<SELECT NAME=\"RESEARCHERC\">" . showResearcherList($RESEARCHER->query('ID')) . "</SELECT>\n";
print "  </TD>\n";
print " </TR>\n";

$RESEARCHER = YRC::PROJECT::RESEARCHER->new();
$RESEARCHER->setDBH($dbh);
$RESEARCHER->load($PROJECT->query('RESEARCHERD'));
print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Researcher D:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
print "<SELECT NAME=\"RESEARCHERD\">" . showResearcherList($RESEARCHER->query('ID')) . "</SELECT>\n";
print "  </TD>\n";
print " </TR>\n";

# SET UP NESTED ARRAY OF FUNDING TYPES OPTIONS
my(@options) = (
		['FUNDINGTYPES', 'FEDERAL', 'Federal'],
		['FUNDINGTYPES', 'FOUNDATION', 'Foundation'],
		['FUNDINGTYPES', 'INDUSTRY', 'Industry'],
		['FUNDINGTYPES', 'PROFASSOC', 'Professional Association'],
		['FUNDINGTYPES', 'LOCGOV', 'Local Government'],
		['FUNDINGTYPES', 'OTHER', 'Other']
              );
# SET UP DEFAULTS
my(@tmparr) = split(',', $PROJECT->query('FUNDINGTYPES'));
my(@defaults) = ();
foreach (@tmparr) {
   push(@defaults, ({FUNDINGTYPES=>$_}));
}

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Funding Source(s):</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . YRC::WWW::FORM->checkboxList(\@options,\@defaults) . "</TD>\n";
print " </TR>\n";


# SET UP NESTED ARRAY OF FEDERAL FUNDING TYPES OPTIONS
@options = (
                ['FUNDINGFEDERAL', 'NASA', 'NASA'],
                ['FUNDINGFEDERAL', 'NIH', 'NIH'],
                ['FUNDINGFEDERAL', 'NSF', 'NSF'],
                ['FUNDINGFEDERAL', 'DOD', 'DOD'],
                ['FUNDINGFEDERAL', 'DOE', 'DOE'],
                ['FUNDINGFEDERAL', 'NIST', 'NIST'],
                ['FUNDINGFEDERAL', 'DVA', 'DVA'],
                ['FUNDINGFEDERAL', 'OTHER', 'Other']
              );
# SET UP DEFAULTS
@tmparr = split(',', $PROJECT->query('FUNDINGFEDERAL'));
@defaults = (); 
foreach (@tmparr) {
   push(@defaults, ({FUNDINGFEDERAL=>$_}));
}


print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Federal Funding:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . YRC::WWW::FORM->checkboxList(\@options,\@defaults) . "</TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">% Effort (BTA):</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
print "<INPUT TYPE=\"TEXT\" NAME=\"BTA\" SIZE=\"5\" MAXLENGTH=\"8\" VALUE=\"" . deQuote($PROJECT->query('BTA')) . "\">\n";
print "  </TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\"><NOBR>AXIS I:&nbsp;&nbsp;";
print "<FONT STYLE=\"font-size:10pt;\"><A HREF=\"javascript:openAXISWindow('I')\">(Modify)</A></FONT>\n";
print "</NOBR></TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
print "<INPUT TYPE=\"TEXT\" NAME=\"AXISI\" SIZE=\"20\" MAXLENGTH=\"40\" VALUE=\"" . deQuote($PROJECT->query('AXISI')) . "\">\n";
print "  </TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\"><NOBR>AXIS II:&nbsp;&nbsp;";
print "<FONT STYLE=\"font-size:10pt;\"><A HREF=\"javascript:openAXISWindow('II')\">(Modify)</A></FONT>\n";
print "</NOBR></TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
print "<INPUT TYPE=\"TEXT\" NAME=\"AXISII\" SIZE=\"20\" MAXLENGTH=\"40\" VALUE=\"" . deQuote($PROJECT->query('AXISII')) . "\">\n";
print "  </TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Abstract:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\"><TEXTAREA NAME=\"ABSTRACT\" COLS=\"35\" ROWS=\"6\">";
print $PROJECT->query('ABSTRACT') . "</TEXTAREA></TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Progress:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\"><TEXTAREA NAME=\"PROGRESS\" COLS=\"35\" ROWS=\"6\">";
print $PROJECT->query('PROGRESS') . "</TEXTAREA></TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Keywords:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\"><TEXTAREA NAME=\"KEYWORDS\" COLS=\"35\" ROWS=\"3\">";
print $PROJECT->query('KEYWORDS') . "</TEXTAREA></TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Publications:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\"><TEXTAREA NAME=\"PUBLICATIONS\" COLS=\"35\" ROWS=\"3\">";
print $PROJECT->query('PUBLICATIONS') . "</TEXTAREA></TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Comments:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\"><TEXTAREA NAME=\"COMMENTS\" COLS=\"35\" ROWS=\"3\">";
print $PROJECT->query('COMMENTS') . "</TEXTAREA></TD>\n";
print " </TR>\n";

print <<END;
   </TABLE>
  </TD>
  <TD WIDTH="50%" VALIGN="top">
   <TABLE BORDER="0" CELLPADDING="no" CELLSPACING="0" WIDTH="100%">




    <TR><TD COLSPAN="2" ALIGN="CENTER">
     <DIV CLASS="project_header" STYLE="width: 100%;"><CENTER>COLLABORATION DATA</CENTER></DIV>
    </TD></TR>
END

# SET UP NESTED ARRAY OF GROUPS
@options = (
                ['CGROUPS', 'Yates', 'Yates (MS)'],
                ['CGROUPS', 'Aebersold', 'Aebersold (MS)'],
                ['CGROUPS', 'TwoHybrid', 'Two Hybrid'],
                ['CGROUPS', 'Microscopy', 'Microscopy'],
                ['CGROUPS', 'Informatics', 'Informatics'],
                ['CGROUPS', 'PSP', 'Protein Structure Prediction']
              );
# SET UP DEFAULTS
@tmparr = $PROJECT->queryGroups('C');
@defaults = (); 
foreach (@tmparr) {
   push(@defaults, ({CGROUPS=>$_}));
}

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Collaboration Groups:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . YRC::WWW::FORM->checkboxList(\@options,\@defaults) . "</TD>\n";
print " </TR>\n";


print <<END;
    <TR><TD COLSPAN="2" ALIGN="CENTER">
     <DIV CLASS="project_header" STYLE="width: 100%;"><CENTER>TECHNOLOGY DEVELOPMENT</CENTER></DIV>
    </TD></TR>
END

# SET UP NESTED ARRAY OF GROUPS
@options = (
                ['techGROUPS', 'Yates', 'Yates (MS)'],
                ['techGROUPS', 'Aebersold', 'Aebersold (MS)'],
                ['techGROUPS', 'TwoHybrid', 'Two Hybrid'],
                ['techGROUPS', 'Microscopy', 'Microscopy'],
                ['techGROUPS', 'Informatics', 'Informatics'],
                ['techGROUPS', 'PSP', 'Protein Structure Prediction']
              );
# SET UP DEFAULTS
@tmparr = $PROJECT->queryGroups('Tech');
@defaults = (); 
foreach (@tmparr) {
   push(@defaults, ({techGROUPS=>$_}));
}

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Technology Development Groups:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . YRC::WWW::FORM->checkboxList(\@options,\@defaults) . "</TD>\n";
print " </TR>\n";




print <<END;
    <TR><TD COLSPAN="2" ALIGN="CENTER">
     <P><DIV CLASS="project_header" STYLE="width: 100%;"><CENTER>TRAINING DATA</CENTER></DIV>
    </TD></TR>
END

# SET UP NESTED ARRAY OF GROUPS                       
@options = (
                ['TGROUPS', 'Yates', 'Yates (MS)'],
                ['TGROUPS', 'Aebersold', 'Aebersold (MS)'],
                ['TGROUPS', 'TwoHybrid', 'Two Hybrid'],
                ['TGROUPS', 'Microscopy', 'Microscopy'],
                ['TGROUPS', 'Informatics', 'Informatics'],
                ['TGROUPS', 'PSP', 'Protein Structure Prediction']
              );
# SET UP DEFAULTS
@tmparr = $PROJECT->queryGroups('T');
@defaults = ();
foreach (@tmparr) {
   push(@defaults, ({TGROUPS=>$_}));
}  
   
print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Training Groups:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . YRC::WWW::FORM->checkboxList(\@options,\@defaults) . "</TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Hours:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\"><INPUT TYPE=\"TEXT\" NAME=\"THOURS\" SIZE=\"4\" MAXLENGTH=\"4\" ";
print "VALUE=\"" . $PROJECT->queryTraining('HOURS') . "\"></TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Days:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\"><INPUT TYPE=\"TEXT\" NAME=\"TDAYS\" SIZE=\"4\" MAXLENGTH=\"4\" ";
print "VALUE=\"" . $PROJECT->queryTraining('DAYS') . "\"></TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Description:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\"><TEXTAREA NAME=\"TDESC\" COLS=\"35\" ROWS=\"3\">";
print $PROJECT->queryTraining('DESCRIPTION') . "</TEXTAREA></TD>\n";
print " </TR>\n";

print <<END;
    <TR><TD COLSPAN="2" ALIGN="CENTER">
     <P><DIV CLASS="project_header" STYLE="width: 100%;"><CENTER>DISSEMINATION DATA</CENTER></DIV>
    </TD></TR>
END

# SET UP NESTED ARRAY OF GROUPS
@options = (
                ['DPLASMIDTYPE', 'M', 'Microscopy'],
                ['DPLASMIDTYPE', 'T', 'Two Hybrid']
              );
# SET UP DEFAULTS
@defaults = ();
if($PROJECT->queryDissemination('PLASMIDTYPE') eq 'M' || $PROJECT->queryDissemination('PLASMIDTYPE') eq 'MT') {
   push(@defaults, ({'DPLASMIDTYPE'=>'M'}));
}
if($PROJECT->queryDissemination('PLASMIDTYPE') eq 'T' || $PROJECT->queryDissemination('PLASMIDTYPE') eq 'MT') {
   push(@defaults, ({'DPLASMIDTYPE'=>'T'}));
}

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Plasmid Type:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">" . YRC::WWW::FORM->checkboxList(\@options,\@defaults) . "</TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Name:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
print "   <INPUT TYPE=\"TEXT\" NAME=\"DNAME\" SIZE=\"40\" MAXLENGTH=\"60\" VALUE=\"";
print $PROJECT->queryDissemination('NAME') . "\">";
print "</TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Phone:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
print "   <INPUT TYPE=\"TEXT\" NAME=\"DPHONE\" SIZE=\"20\" MAXLENGTH=\"20\" VALUE=\"";
print $PROJECT->queryDissemination('PHONE') . "\">";
print "</TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Email:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
print "   <INPUT TYPE=\"TEXT\" NAME=\"DEMAIL\" SIZE=\"45\" MAXLENGTH=\"50\" VALUE=\"";
print $PROJECT->queryDissemination('EMAIL') . "\">";
print "</TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Address:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\"><TEXTAREA NAME=\"DADDRESS\" COLS=\"35\" ROWS=\"3\">";
print $PROJECT->queryDissemination('ADDRESS') . "</TEXTAREA></TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Description:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\"><TEXTAREA NAME=\"DDESC\" COLS=\"35\" ROWS=\"3\">";
print $PROJECT->queryDissemination('DESCRIPTION') . "</TEXTAREA></TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Comments:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\"><TEXTAREA NAME=\"DCOMMENTS\" COLS=\"35\" ROWS=\"3\">";
print $PROJECT->queryDissemination('COMMENTS') . "</TEXTAREA></TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Commercial?</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
print YRC::WWW::FORM->truefalseList((NAME=>'DCOMMERCIAL', DEFAULT=>$PROJECT->queryDissemination('COMMERCIAL')));
print "  </TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">Shipped?</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
print YRC::WWW::FORM->truefalseList((NAME=>'DSHIPPED', DEFAULT=>$PROJECT->queryDissemination('SHIPPED')));
print "  </TD>\n";
print " </TR>\n";

print getRowTag();
print "  <TD VALIGN=\"top\" WIDTH=\"25%\">FEDEX #:</TD>\n";
print "  <TD VALIGN=\"top\" WIDTH=\"75%\">";
print "   <INPUT TYPE=\"TEXT\" NAME=\"DFEDEX\" SIZE=\"20\" MAXLENGTH=\"30\" VALUE=\"";
print $PROJECT->queryDissemination('FEDEX') . "\">";
print "</TD>\n";
print " </TR>\n";


print <<END;
   </TABLE>
  </TD>
 </TR>
</TABLE>

<P><INPUT TYPE="IMAGE" WIDTH="200" HEIGHT="33" SRC="/v2/images/project-save.gif" BORDER="0">&nbsp;&nbsp;
<A onClick="javascript:document.location.href='viewProject.cgi?ID=$projectID&trunc=1'">
<IMG SRC="/v2/images/project-cancel.gif" WIDTH="200" HEIGHT="33" BORDER="0"></A>
</FORM>

</DIV>
END

print <<END;
</CENTER>
END
} else {
   print <<END;
<CENTER>
<DIV CLASS="project_header" STYLE="width: 400;">
<CENTER>EDIT PROJECT:</CENTER>
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
   my($type) = @_;
   my($output);

   if(!$type) { $type = ''; }

   $output = "<TR";
   if($rowCounter % 2) {
      if($type eq 'D') { $output .= " BGCOLOR=\"FFBC9B\""; }
      else { $output .= " BGCOLOR=\"EEE2FF\""; }
   }
   $output .= ">";

   $rowCounter++;
   return $output;
}

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
      if($arr[0] == $default) { $output .= " SELECTED"; }
      $output .= ">" . $arr[2] . ", " . $arr[1] . " - " . $arr[3] . "\n";
   }
   return $output;
}

sub numerically { $a <=> $b }

# STRIPS QUOTE OFF OF A STRING, IF QUOTED
# RETURNS THE STRING SANS QUOTES
sub deQuote {
   my($str) = @_;
   if(!$str) { return ''; }

   # SNIP OUT THE QUOTES
   if(substr($str, 0, 1) eq "\"") { substr($str, 0, 1, ""); }
   if(substr($str, -1, 1) eq "\"") {substr($str, -1, 1, ""); }
  
   return $str;
}

exit 0;
