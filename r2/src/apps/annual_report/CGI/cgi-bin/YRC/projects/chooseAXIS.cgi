#!/usr/bin/perl -w

use strict;
use CGI qw(:standard escape);

use lib "/usr/local/lib/site_perl";
use YRC::DB;
use YRC::WWW::SESSION;
use YRC::WWW::LOGIN;
use YRC::WWW::TEMPLATES;
use YRC::PROJECT::PROJECT;
use YRC::PROJECT::AXIS;

my($query) = new CGI;
my($USER);

my(@DISP_ARR) = ( );	## HOLDS OUR DATA IN THE COLUMNS FOR DISPLAYING
my($COLS) = 3;		## HOW MANY COLUMNS TO SHOW

# FORCE IT TO BE SECURE
#YRC::WWW::SESSION->enforceSecure($query);

# OUR ENTIRE AUTHENTICATION CODE
unless($USER = YRC::WWW::SESSION->getUserObject(YRC::WWW::SESSION->getSessionID($query))) {
   YRC::WWW::LOGIN->showLogin(QUERY=>$query, REQUESTER=>$ENV{REQUEST_URI});
   exit 0;
}

# PRINT OUR HEADER
print YRC::WWW::TEMPLATES->getSimpleHeader();

my($PROJECT);
my($projectID) = $query->param('ID');
my($axisType) = $query->param('type');

## SET OUR DATABASE HANDLE
my($dbh) = YRC::DB->getDBH();

## SET UP OUR PROJECT OF INTEREST
$PROJECT = YRC::PROJECT::PROJECT->new();
$PROJECT->setDBH($dbh);
$PROJECT->load($projectID) || $PROJECT->set('NOTFOUND', 1); 

print <<END;

<SCRIPT LANGUAGE="JavaScript">

/**
 *	JavaScript code written by Michael Riffle <mriffle\@u.washington.edu>
 *	June 22, 2002
 */

// Sort two objects, based on their value property
function checkboxSort(a, b) {
   var lastAChar, lastBChar;

   lastAChar = a.value.charAt(a.value.length - 1);
   lastBChar = b.value.charAt(b.value.length - 1);

   if(parseInt(a.value) == parseInt(b.value)) {
      if(lastAChar >= lastBChar) { return 1; }
      return -1;
   }

   return parseInt(a.value) - parseInt(b.value);
}

function changeAXIS() {
   var LIST = document.form1.AXIS;
   var i;
   var output;

   // Need to set up the array, because LIST isn't a true array and doesn't support sorting
   var myArray = new Array(LIST.length);
   for(i=0; i<LIST.length; i++) {
      myArray[i] = LIST[i];
   }

   // Sort this array based on the value property of each checkbox object
   myArray.sort(checkboxSort);

   // Generate the output string we're sending back to the main form
   output = "";
   for(i=0; i<myArray.length; i++) {
      if(myArray[i].checked) { 
         if(output.length != 0) { output += " "; }
         output += myArray[i].value;
      }
   }

   // Send the output string to the main form, and close this window
   opener.document.form1.AXIS$axisType.value = output;
   alert("AXIS $axisType information updated.  This change will be only be saved if you save this project.");
   window.close();

}

</SCRIPT>


<CENTER>
<DIV CLASS="project_header" STYLE="width: 650;">
<CENTER>CHOOSE AXIS $axisType:</CENTER>
</DIV>

<DIV CLASS="project" STYLE="width: 650;">
END

## BUILD @DISP_ARR HOLDING DATA FOR EACH COLUMN
my(@LIST) = YRC::PROJECT::AXIS->queryAXISList($axisType);
my($TMPVAL) = sprintf("%.0f", (@LIST / $COLS));
my($counter, $i, $k);
for($i=0; $i<$TMPVAL; $i++) {

   for($k=0; $k<$COLS; $k++) {
      push(@{$DISP_ARR[$k]}, ($LIST[$i + $TMPVAL*$k]));
   }

}

## SET UP A QUICK HASH WITH OUR CURRENT AXIS VALUES SO THAT WE CAN CHECK
## APPROPRIATE BOXES ON THE FORM BELOW IN ADVANCE
my(%DEFS) = ( );
foreach (split(" ", $PROJECT->query("AXIS$axisType"))) {
   $DEFS{$_} = 1;
}


## SHOW THE FORM WITH CHECKBOX OPTIONS
$counter = 0;
$i = 0;
my($ROW);

print "<FORM NAME=\"form1\">\n";
print "<TABLE BORDER=\"0\" CELLPADDING=\"NO\" CELLSPACING=\"5\">\n";

while($DISP_ARR[0]->[$counter]) {

   print "<TR>\n";

   for($i=0; $i<$COLS; $i++) {
      if(!$DISP_ARR[$i]->[$counter][0]) {
         print "<TD>&nbsp;</TD>\n";
      } else {
         print "<TD>";
         print "<INPUT TYPE=\"checkbox\" NAME=\"AXIS\" VALUE=\"" . $DISP_ARR[$i]->[$counter][0] . "\"";
         if($DEFS{$DISP_ARR[$i]->[$counter][0]}) { print " checked"; }
         print ">\n";
         print "<FONT STYLE=\"font-size:10pt;\">";
         print $DISP_ARR[$i]->[$counter][0] . " : " . $DISP_ARR[$i]->[$counter][1] . "</FONT></TD>\n";
      }
   }

   print "</TR>\n";

   $counter++;
}

print "</TABLE>\n";

print "<P ALIGN=\"CENTER\"><INPUT TYPE=\"button\" onClick=\"changeAXIS()\" VALUE=\"KEEP CHANGES\">\n";
print "<INPUT TYPE=\"button\" onClick=\"window.close()\" VALUE=\"CANCEL\">\n";

print "</FORM>\n";

print <<END;
</DIV>
</CENTER>
END

# PRINT OUR FOOTER
print YRC::WWW::TEMPLATES->getFooter();

exit 0;
