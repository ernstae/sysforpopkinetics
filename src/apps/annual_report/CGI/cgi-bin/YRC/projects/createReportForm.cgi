#!/usr/bin/perl -w

use strict;
use CGI qw(:standard);

use lib "/usr/local/lib/site_perl";
use YRC::WWW::SESSION;
use YRC::WWW::LOGIN;
use YRC::WWW::TEMPLATES;
use YRC::WWW::HISTORY;

my($query) = new CGI;
my($TITLE) = 'Create Custom Report';
my($USER);

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
$HISTORY->setHistory( ( TITLE=>$TITLE, LINK=>$query->url(-full=>1,-query=>1) ), TRUNC=>$query->param('trunc') );
print $HISTORY->getHistoryNav();

# WHO DO WE WANT TO HAVE ACCESS TO THIS PAGE?
$USER->checkGroup( NUMBER=>1 );

print <<END;

<CENTER>

<DIV CLASS="main_header" STYLE="width: 500;">
<CENTER>CREATE CUSTOM REPORT:</CENTER>
</DIV>
<DIV CLASS="main" STYLE="width: 500;">

<P><FORM ACTION="createReport.cgi" method="GET" NAME="form1">
<INPUT TYPE="HIDDEN" NAME="reportFields" VALUE = "">
<CENTER>

<TABLE WIDTH="80%" BORDER="0" CELLPADDING="no" CELLSPACING="0">
 <TR>

  <TD WIDTH="40%" VALIGN="TOP" ALIGN="CENTER"><FONT STYLE="font-size:10pt;">

   <NOBR><U>POSSIBLE FIELDS</U>:</NOBR><BR>
   <SELECT NAME="POSSIBLES" SIZE="14" ID="POSSIBLES">
    <OPTION VALUE="ID">Project ID</OPTION>
    <OPTION VALUE="TITLE">Title</OPTION>
    <OPTION VALUE="GROUPS">YRC Groups</OPTION>
    <OPTION VALUE="TYPE">Project Type</OPTION>
    <OPTION VALUE="FUNDINGTYPES">Funding Sources</OPTION>
    <OPTION VALUE="FUNDINGFEDERAL">Federal Funding</OPTION>
    <OPTION VALUE="BTA">BTA</OPTION>
    <OPTION VALUE="SUBMITDATE">Submit Date</OPTION>
    <OPTION VALUE="PI_FIRSTNAME">PI First Name</OPTION>
    <OPTION VALUE="PI_LASTNAME">PI Last Name</OPTION>
    <OPTION VALUE="PI_DEGREE">PI Degree</OPTION>
    <OPTION VALUE="PI_ORGANIZATION">PI Organization</OPTION>
    <OPTION VALUE="PI_STATE">PI State</OPTION>
    <OPTION VALUE="PI_COUNTRY">PI Country</OPTION>
    <OPTION VALUE="RESB_FIRSTNAME">Res. B First Name</OPTION>
    <OPTION VALUE="RESB_LASTNAME">Res. B Last Name</OPTION>
    <OPTION VALUE="RESB_DEGREE">Res. B Degree</OPTION>
    <OPTION VALUE="RESB_ORGANIZATION">Res. B Organization</OPTION>
    <OPTION VALUE="RESB_STATE">Res. B State</OPTION>
    <OPTION VALUE="RESB_COUNTRY">Res. B Country</OPTION>
    <OPTION VALUE="RESC_FIRSTNAME">Res. C First Name</OPTION>
    <OPTION VALUE="RESC_LASTNAME">Res. C Last Name</OPTION>
    <OPTION VALUE="RESC_DEGREE">Res. C Degree</OPTION>
    <OPTION VALUE="RESC_ORGANIZATION">Res. C Organization</OPTION>
    <OPTION VALUE="RESC_STATE">Res. C State</OPTION>
    <OPTION VALUE="RESC_COUNTRY">Res. C Country</OPTION>
    <OPTION VALUE="RESD_FIRSTNAME">Res. D First Name</OPTION>
    <OPTION VALUE="RESD_LASTNAME">Res. D Last Name</OPTION>
    <OPTION VALUE="RESD_DEGREE">Res. D Degree</OPTION>
    <OPTION VALUE="RESD_ORGANIZATION">Res. D Organization</OPTION>
    <OPTION VALUE="RESD_STATE">Res. D State</OPTION>
    <OPTION VALUE="RESD_COUNTRY">Res. D Country</OPTION>
   </SELECT>
  </TD>

  <TD WIDTH="40%" VALIGN="CENTER" ALIGN="CENTER"><FONT STYLE="font-size:10pt;">

   <SCRIPT LANGUAGE="JavaScript">

    // Our global vars
    var isNav, isIE;

    // What browser do we have?
    if(parseInt(navigator.appVersion) >= 4) {
       if(navigator.appName == "Netscape") {
          isNav = true;
       } else {
          isIE = true;
       }
    }

    var emptyOption = new Option("Currently None.", "");

    function moveField(direction) {

       var myList, targList, index;

       if(direction == "right") {
          myList = document.form1.POSSIBLES;
          targList = document.form1.FIELDS;
       } else {
          myList = document.form1.FIELDS;
          targList = document.form1.POSSIBLES;
       }

       var index = myList.selectedIndex;

       if(index == -1) {
          alert ("You must select a field.");
          return 1;
       }

       var myText = myList.options[index].text;
       var myValue = myList.options[index].value;
       var newOption;

       if(targList.options[0].value == "") {
          if(isNav) {
             targList.options[0] = myList.options[index];
          } else {
             targList.options[0].text = myText;
             targList.options[0].value = myValue;
             myList.options.remove(index);
          }
       }
       else {
          if(isNav) {
             targList.options[targList.options.length] = myList.options[index];
          } else {
             newOption = document.createElement("OPTION");
             newOption.text = myText;
             newOption.value = myValue;
             targList.options.add(newOption);
             myList.options.remove(index);
          }
       }

       // Did we empty out the list?  Show that now.
       if(myList.options.length == 0) {
          if(isNav) {
             myList.options[0] = emptyOption;
          } else {
             newOption = document.createElement("OPTION");
             newOption.text = "Currently None.";
             newOption.value = "";
             myList.options.add(newOption);
          }
       }

       return 1;
    }


    function shiftField(direction) {
       var myList = document.form1.FIELDS;
       var index = myList.selectedIndex;
       var tmpOb, tmpOb2;

       if(myList.options[0].value == "") {
          alert("Nothing to move up or down.");
          return 1;
       }

       if(index == -1) {
          alert("You must select something to move up or down.");
          return 1;
       }

       if(direction == "up") {
          if(index == 0) {
             alert("That is already at the top.");
             return 1;
          }
          if(isNav) {
             tmpOb = new Option(myList.options[index].text, myList.options[index].value);
             tmpOb2 = new Option(myList.options[index - 1].text, myList.options[index - 1].value);
             myList.options[index] = tmpOb2;
             myList.options[index - 1] = tmpOb;
             myList.options[index - 1].selected = true;
          } else {
             tmpOb = document.createElement("OPTION");
             tmpOb.text = myList.options[index].text;
             tmpOb.value = myList.options[index].value;
             myList.options.add(tmpOb, (index - 1));
             myList.options[index - 1].selected = true;
             myList.options.remove(index + 1);
          }

          return 1;
       }
       if(direction == "down") {
          if(index == (myList.options.length - 1)) {
             alert("That is already at the bottom.");
             return 1;
          }
          if(isNav) {
             tmpOb = new Option(myList.options[index].text, myList.options[index].value);
             tmpOb2 = new Option(myList.options[index + 1].text, myList.options[index + 1].value);
             myList.options[index] = tmpOb2;
             myList.options[index + 1] = tmpOb;
             tmpOb.selected = true;
          } else {
             tmpOb = document.createElement("OPTION");
             tmpOb.text = myList.options[index].text;
             tmpOb.value = myList.options[index].value;
             myList.options.remove(index);
             myList.options.add(tmpOb, (index + 1));
             myList.options[index + 1].selected = true;
          }

          return 1;
       }
   }

   function runReport() {
       var myList = document.form1.FIELDS;
       var list = "";
       var i = 0;
             
       if(myList.options[0].value == "") {
          alert("No fields added to the report!");
          return 1;
       }

       for(i=0; i<(myList.options.length); i++) {
          if(i != 0) { list = list + ","; }
          list = list + myList.options[i].value;;
       }

       document.form1.reportFields.value = list;
       document.form1.submit();
   }


   </SCRIPT>

   <INPUT TYPE="BUTTON" VALUE="ADD --&gt;" onClick="moveField('right')"><BR>
   <INPUT TYPE="BUTTON" VALUE="&lt;-- DELETE" onClick="moveField('left')">
  </TD>

  <TD WIDTH="40%" VALIGN="TOP" ALIGN="CENTER"><FONT STYLE="font-size:10pt;">

   <NOBR><U>FIELDS TO BE IN REPORT</U></NOBR>:<BR>
   <SELECT NAME="FIELDS" SIZE="14" ID="FIELDS">
    <OPTION VALUE="">Currently None</OPTION>
   </SELECT><BR>

   <NOBR>
   <INPUT TYPE="BUTTON" VALUE="UP /\\" onClick="shiftField('up')">
   <INPUT TYPE="BUTTON" VALUE="DOWN \\/" onClick="shiftField('down')">
   </NOBR>

  </TD>

 </TR>

</TABLE>

<P><INPUT TYPE="BUTTON" VALUE="RUN REPORT" onClick="runReport()">

</CENTER>
</FORM>
</DIV>


</CENTER>
END



# PRINT OUR FOOTER
print YRC::WWW::TEMPLATES->getFooter();

exit 0;
