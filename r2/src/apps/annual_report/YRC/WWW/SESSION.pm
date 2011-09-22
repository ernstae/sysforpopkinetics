################################################################################
################################################################################
##
## SESSION.pm
## Purpose: To provide a set of functions for obtaining information from the
## sessions table, or information based on session IDs
##
## Author:      Michael Riffle <mriffle@u.washington.edu>
## Date:        March 22, 2002
##
################################################################################
################################################################################

package YRC::WWW::SESSION;
use strict;

use lib "/usr/local/lib/site_perl";
use YRC::WWW::COOKIE;
use YRC::WWW::USER;
use YRC::DB;

# TAKES A SESSION ID AND RETURNS AN ACTUAL, POPULATED USER OBJECT
# RETURNS 0 IF THERE IS NO SESSION, NO USER OR IF THERE IS AN ERROR
sub getUserObject {
   my($self,$sessionID,$dbh) = @_;
   my($userID, $USER);

   if(!$sessionID) { return 0; }
   if(!$dbh) { $dbh = YRC::DB->getDBH(); }

   ($userID = $self->getUserID($dbh, $sessionID)) || return 0;

   $USER = YRC::WWW::USER->new((DBH=>$dbh));
   $USER->load($userID);
   $USER->set( (SESSIONID=>$sessionID) );
   return $USER;
}

# TAKES A SESSION ID AND RETURNS A CORRESPONDING USER ID, IF VALID
sub getUserID {
   my($module,$dbh, $sessionID) = @_;
   my($sql, $sth, @arr);

   if(!$sessionID) { return 0; }

   if(!$dbh) { $dbh = YRC::DB->getDBH(); }
   
   $sql = "SELECT yrcID FROM tblSessions WHERE sessionID = '$sessionID'";
   $sth = $dbh->prepare($sql);
   $sth->execute;
   if(!$sth->rows) { return 0; }
   @arr = $sth->fetchrow_array;

   # UPDATE THE TIMESTAMP IN tblSessions
   $sql = "UPDATE tblSessions SET sessionLastAccess = " . time() . " WHERE sessionID = '" . $sessionID . "'";
   $dbh->do($sql);

   return $arr[0];   
}

# TAKES A USER ID AND SESSION ID (COOKIE VALUE) AND SETS THE CREATES A SESSION ROW ENTRY
sub setSession {
   my($self, %args) = @_;
   my($sql, $sth, @arr);

   if(!$args{ID}) { return 0; }
   if(!$args{COOKIE}) { return 0; }

   if(!$args{DBH}) { $args{DBH} = YRC::DB->getDBH(); }

   $sql =  "DELETE FROM tblSessions WHERE sessionID = '" . $args{COOKIE} . "' OR ";
   $sql .= "yrcID = " . $args{ID};
   $args{DBH}->do($sql);

   $sql  = "INSERT INTO tblSessions (sessionID, yrcID, sessionLastAccess) VALUES (";
   $sql .= "'" . $args{COOKIE} . "', " . $args{ID} . ", " . time() . ")";
   $args{DBH}->do($sql);

   $sql =  "UPDATE tblYRCLogins SET loginLastLogin = NOW() WHERE yrcID = " . $args{ID};
   $args{DBH}->do($sql);

   return 1;
}

sub getSessionID {
   my($self, $query) = @_;

   return YRC::WWW::COOKIE->queryCookie($query);
}


sub enforceSecure {
   my($self, $query) = @_;

   if( !$ENV{HTTPS} ) {
      # THE PAGE ISN'T SECURE, REDIRECT TO HTTPS VERSION OF HTTP URL
      my($URL) = $query->url(-path_info=>1,-query=>1);
      unless($URL =~ 'https://') { $URL =~ s/http\:\/\//https:\/\//; }
      print $query->redirect($URL);
      exit 0;
   }
}

1;
