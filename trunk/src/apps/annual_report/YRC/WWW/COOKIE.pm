################################################################################
################################################################################
##
## COOKIE.pm
## Purpose: To provide functions for setting/reading cookies for the YRC site
##
## Author:      Michael Riffle <mriffle@u.washington.edu>
## Date:        April 1, 2002
##
################################################################################
################################################################################

package YRC::WWW::COOKIE;
use strict;

# SET A COOKIE OF THE GIVEN NAME W/ THE GIVEN VALUE
# DEFAULTS TO 'YEASTRC_LOGIN' AND A GENERATED SESSION ID
sub setCookieHeader {
   my($module, $query, $name, $cvalue) = @_;
   my($cookie, $remoteaddrcook);

   if(!$query) { return 0; }
   if(!$name) { $name = 'YEASTRC_LOGIN'; }
   if(!$cvalue) { $cvalue = time() . "-$$"; }

   # Do they have a cookie?  If not, let's give them one.
   if(!($cookie = $query->cookie($name))) {
       $remoteaddrcook = $query->cookie(
				-name	=>	$name,
				-domain	=>	'',
				-value	=>	$cvalue,
				-path	=>	'/cookies'
				);

       print $query->header(-cookie=>$remoteaddrcook);

       $cookie = $cvalue;

   } else {
      print "Content-type: text/html\n\n";
   }

   return $cookie;

}

# SIMPLY RETURN THE VALUE FOR A COOKIE W/ THE SUPPLIED NAME
# DEFAULTS TO 'YEASTRC_LOGIN'
sub queryCookie {
   my($module, $query, $name) = @_;

   if(!$query) { return 0; }
   if(!$name) { $name = 'YEASTRC_LOGIN'; }

   ( my($cookie) = $query->cookie($name) ) || return 1;
   return $cookie;
}

1;
