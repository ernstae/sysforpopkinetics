#!/usr/bin/perl -w

use strict;
use CGI::Carp "fatalsToBrowser";
use CGI qw(:standard);

use lib "/usr/local/lib/site_perl";
use YRC::WWW::SESSION;
use YRC::WWW::LOGIN;
use YRC::WWW::TEMPLATES;
use YRC::WWW::HISTORY;

my($query) = new CGI;
my($TITLE) = 'Search Projects';
my($USER);

# FORCE IT TO BE SECURE
# YRC::WWW::SESSION->enforceSecure($query);

# OUR ENTIRE AUTHENTICATION CODE
unless($USER = YRC::WWW::SESSION->getUserObject(YRC::WWW::SESSION->getSessionID($query))) {
   YRC::WWW::LOGIN->showLogin(QUERY=>$query, REQUESTER=>$ENV{REQUEST_URI});
   exit 0;
}

# FORCE THEM TO ENTER A NEW PASSWORD IF THEY HAVEN'T
unless($USER->query('LASTCHANGED')) { print $query->redirect('/cgi-bin/YRC/admin/changePassword.cgi'); }

# PRINT OUR HEADER
print YRC::WWW::TEMPLATES->getHeader($TITLE, 1, $USER);

my($HISTORY) = YRC::WWW::HISTORY->new( (SESSIONID=>$USER->query('SESSIONID')) );
$HISTORY->setHistory( TITLE=>$TITLE, LINK=>$query->url(-full=>1,-query=>1) , TRUNC=>$query->param('trunc') );
print $HISTORY->getHistoryNav();

print <<END;

<CENTER>

<DIV CLASS="main_header">
<CENTER>SEARCH PROJECTS:</CENTER>
</DIV>
<DIV CLASS="main">

<P ALIGN="left">Please fill out the form below to search the current projects in the database.  You may use words 
found in the keywords, description, PI name or abstract.  Use asterisk (*) as a wildcard.

<P><FORM ACTION="searchProjects.cgi" method="GET">

<CENTER>

<INPUT TYPE="text" NAME="query" SIZE="50">

<P>Limit your search to:<BR>

 <P><U>Project Type:</U><BR>
 <INPUT TYPE="checkbox" NAME="C" VALUE="C">Collaboration
 <INPUT TYPE="checkbox" NAME="T" VALUE="T">Training
 <INPUT TYPE="checkbox" NAME="D" VALUE="D">Dissemination
 <NOBR><INPUT TYPE="checkbox" NAME="Tech" VALUE="Tech">Technology Development</NOBR>

 <P><U>Groups:</U><BR>
 <NOBR><INPUT TYPE="checkbox" NAME="group" VALUE="IntMetab">IntMetab</NOBR>
 <NOBR><INPUT TYPE="checkbox" NAME="group" VALUE="LipidMetab">LipidMetab</NOBR>
 <NOBR><INPUT TYPE="checkbox" NAME="group" VALUE="PKPD">PKPD</NOBR>
 <NOBR><INPUT TYPE="checkbox" NAME="group" VALUE="EnvTox">EnvTox</NOBR>
 <NOBR><INPUT TYPE="checkbox" NAME="group" VALUE="CBNet">CBNet</NOBR>
 <NOBR><INPUT TYPE="checkbox" NAME="group" VALUE="PMImageD">PMImageD</NOBR>
 <NOBR><INPUT TYPE="checkbox" NAME="group" VALUE="SysMod">SysMod</NOBR>
 <NOBR><INPUT TYPE="checkbox" NAME="group" VALUE="StatMod">StatMod</NOBR>
 <NOBR><INPUT TYPE="checkbox" NAME="group" VALUE="SoftDev">SoftDev</NOBR>


<P><INPUT TYPE="SUBMIT" VALUE="Search Projects">

</CENTER>
</FORM>
</DIV>


</CENTER>
END



# PRINT OUR FOOTER
print YRC::WWW::TEMPLATES->getFooter();

exit 0;
