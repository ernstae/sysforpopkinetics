#!/usr/bin/perl -w

use strict;
use CGI qw(:standard);
use Digest::MD5 qw(md5_hex);

use lib "/usr/local/lib/site_perl";
use YRC::WWW::SESSION;
use YRC::WWW::LOGIN;
use YRC::WWW::TEMPLATES;
use YRC::WWW::HISTORY;
use YRC::WWW::ERROR;

my($query) = new CGI;
my($action) = $query->param('action');
my($TITLE) = 'Change Password';
my($USER);
my($errorMsg, $successMsg);

# FORCE IT TO BE SECURE
#YRC::WWW::SESSION->enforceSecure($query);

# OUR ENTIRE AUTHENTICATION CODE
unless($USER = YRC::WWW::SESSION->getUserObject(YRC::WWW::SESSION->getSessionID($query))) {
   YRC::WWW::LOGIN->showLogin(QUERY=>$query, REQUESTER=>$ENV{REQUEST_URI});
   exit 0;
}

# WHAT ARE WE DOING?
if($action && $action eq 'change') {
   my($pw) = $query->param('newPassword');
   my($pw2) = $query->param('newPassword2');

   if(!$pw || !$pw2) { $errorMsg = "You must enter a new password and confirm it."; }
   elsif($pw ne $pw2) { $errorMsg = "Your password and confirmation do not match.  Please re-enter them."; }
   elsif(md5_hex($pw) eq $USER->query('PASSWORD')) { $errorMsg = "You can not change your password to your current password."; }
   else {
      # IT CHECKS OUT, LET'S SAVE IT
      $pw = md5_hex($pw);
      $USER->setPassword($pw);
      $successMsg = "Password updated successfully.";
   }
}



# PRINT OUR HEADER
print YRC::WWW::TEMPLATES->getHeader($TITLE, 1, $USER);

my($HISTORY) = YRC::WWW::HISTORY->new( (SESSIONID=>$USER->query('SESSIONID')) );
$HISTORY->setHistory( ( TITLE=>$TITLE, LINK=>$query->url(-full=>1,-query=>1) ), TRUNC=>$query->param('trunc') );
print $HISTORY->getHistoryNav();

# DO WE HAVE AN ERROR?
if($errorMsg) { YRC::WWW::ERROR->showError($errorMsg); }
elsif($successMsg) { YRC::WWW::ERROR->showSuccess($successMsg); }

print <<END;

<CENTER>

<DIV CLASS="admin_header">
<CENTER>CHANGE PASSWORD:</CENTER>
</DIV>
<DIV CLASS="admin">

<P ALIGN="left">Please fill out the form below to change your password.

<P><FORM ACTION="changePassword.cgi" method="GET">
<INPUT TYPE="hidden" NAME="action" VALUE="change">
<INPUT TYPE="hidden" NAME="trunc" VALUE="1">

<CENTER>

<TABLE BORDER="0">

 <TR>
  <TD><FONT SIZE="2" FACE="verdana, helvetica, arial">New password:</FONT></TD>
  <TD><FONT SIZE="2" FACE="verdana, helvetica, arial"><INPUT TYPE="PASSWORD" NAME="newPassword" SIZE="20" MAXLENGTH="50"></FONT></TD>
 </TR>

 <TR>
  <TD><FONT SIZE="2" FACE="verdana, helvetica, arial">Please confirm:</FONT></TD>
  <TD><FONT SIZE="2" FACE="verdana, helvetica, arial"><INPUT TYPE="PASSWORD" NAME="newPassword2" SIZE="20" MAXLENGTH="50"></FONT></TD>
 </TR>

</TABLE>

<P><INPUT TYPE="SUBMIT" VALUE="Change Password">

</CENTER>
</FORM>
</DIV>


</CENTER>
END



# PRINT OUR FOOTER
print YRC::WWW::TEMPLATES->getFooter();

exit 0;
