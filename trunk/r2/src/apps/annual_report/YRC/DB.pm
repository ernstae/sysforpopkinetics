################################################################################
################################################################################
##
## DB.pm
## Purpose: Simply a centralized location for our DB connection information
## and a quick way to acquire a database handle for that connection info.
##
## Author:	Michael Riffle <mriffle@u.washington.edu>
## Date:	March 22, 2002
##
################################################################################
################################################################################

package YRC::DB;
use strict;
use lib "/usr/local/lib/site_perl";
use DBI;

my(%HANDLES) = ( );

# RETURNS A DATABASE HANDLE WITH THE CORRECT CONNECTION INFORMATION
sub getDBH {
   my($msg);
   my($trash,$database) = @_;

#   if(!$database) { $database = 'PROJECTS'; }
   if(!$database) { $database = 'YRC'; }

   if($HANDLES{$database}) { return $HANDLES{$database}; }

#   my($driver,$username,$password,$host) = ('mysql','username', 'password', 'localhost');
   my($driver,$username,$password,$host) = ('mysql','admin', '4SPKdb%', 'localhost');
#   my($driver) = ('mysql');
#   my($host) = ('localhost');
#   my($username) = $query->param('username');
#   my($password) = $query->param('password');



   my($dsn) = "DBI:$driver:database=$database:host=$host";
   my($DBH) = DBI->connect($dsn, $username, $password, {'RaiseError' => 0, 'PrintError' => 0});
#   my($DBH) = DBI->connect($dsn, $username, $password);

   $HANDLES{$database} = $DBH;
   return $HANDLES{$database};
}

1;
