################################################################################
################################################################################
##
## USER.pm
## Purpose: To hold information regarding a validated user on the YRC web site.
## Also provides a series of methods for accessing and changing this information.
##
## Author:	Michael Riffle <mriffle@u.washington.edu>
## Date:	March 22, 2002
##
################################################################################
################################################################################


package YRC::WWW::USER;
use strict;

use lib "/usr/local/lib/site_perl";
use YRC::DB;
use YRC::WWW::SESSION;
use YRC::WWW::TEMPLATES;
use YRC::WWW::ERROR;

sub new {
   my ($class, %args) = @_;
   my ($cookie);

   my ($self) = bless {}, ref($class)||$class;

   $self->_init(%args);
   return $self;
}

# INITIALIZE THE USER OBJECT
sub _init {
   my ($self, %args) = @_;

   $self->{TABLE} = "tblYRCPeople";

   # SET UP THE INITIAL PARAMETERS PASSED IN WITH new()
   foreach $_ (keys(%args)) {
      $self->{$_} = $args{$_};
   }

   return 1;
}

# SETS UP REFERENCE TO THE DATABASE HANDLE
sub setDBH {
   my ($self, $dbh) = @_;
   $self->{DBH} = $dbh;
}

# SETS A VARIABLE, OR SET OF VARIABLES TO SPECIFIED VALUES
# TAKES A HASH OF KEYS=>VALUES AS THE PARAMETER
sub set {
   my ($self, %args) = @_;
   my ($key, $nkey);

   foreach $key (%args) {
      $nkey = $key;
      $nkey =~ tr/[a-z]/[A-Z]/;
      $self->{"_$nkey"} = $args{$key};
   }
   return 1;
}


# RETURNS THE VALUE FOR THE GIVEN ATTRIBUTE
# RETURNS 0 ON ERROR
sub query {
   my ($self, $attr) = @_;

   $attr =~ tr/[a-z]/[A-Z]/;
   if(!$self->{"_$attr"}) {
      # NO ATTRIBUTE SET, LOAD IN DATA FROM DB
      if(!$self->{_ID}) {
         # CAN'T LOAD IN DATA W/O AN ID
         $self->{ERROR} = "No attribute set, and no ID set (can not query DB).";
         return 0;
      }
      # LOAD THE DATA
      $self->load();
   }

   return $self->{"_$attr"};
}      

# loads data from database for the given ID into object variables
# with the same name (capitalized) of the database table columns
sub load {
   my ($self, $ID, $USERNAME) = @_;
   my ($sql, $sth, $hashref, $key, $nkey, @arr);

   if( !$ID && !( $ID = $self->{_ID} ) && !$USERNAME ) { return 0; }

   if(!$self->{DBH}) {
      $self->{ERROR} = "No database handle set.  Call setDBH first.";
      return 0;
   }

   # IF WE WERE PASSED A USERNAME INSTEAD OF AN ID, GET THE ID
   if($USERNAME) {
      $sql = "SELECT yrcID, loginPassword FROM tblYRCLogins WHERE loginUsername = '$USERNAME'";
      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;
      if(!$sth->rows) { return 0; }
      @arr = $sth->fetchrow_array;
      $ID = $arr[0];
      $self->{_PASSWORD} = $arr[1];
      $self->{_USERNAME} = $USERNAME;
   }

   # IF WE WERE PASSED AN ID, MAKE SURE WE HAVE THE USERNAME
   if($ID) {
      $sql = "SELECT loginUsername, loginPassword, loginLastChanged, loginLastLogin FROM tblYRCLogins WHERE yrcID = $ID";
      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;
      if(!$sth->rows) { return 0; }
      @arr = $sth->fetchrow_array;
      $self->{_PASSWORD} = $arr[1];
      $self->{_USERNAME} = $arr[0];
      $self->{_LASTCHANGED} = $arr[2];
      $self->{_LASTLOGIN} = $arr[3];
   }



   $sql = "SELECT * FROM " . $self->{TABLE} . " WHERE yrcID = " . $ID;
   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;
   if(!$sth->rows) { return 0; }

   $hashref = $sth->fetchrow_hashref();
   foreach $key (keys( %{$hashref} )) {
      $nkey = $key;
      $nkey =~ tr/[a-z]/[A-Z]/;
      $nkey =~ s/YRC//;
      $self->{"_$nkey"} = $hashref->{$key};
   }
   return 1;
}

# save this user into the database as a new entry.
# returns 1 if successful, 0 if not.
sub save {
   my($self) = @_;
   my($sql, $sth, @arr);

   if(!$self->{DBH}) {
      $self->{ERROR} = "DBH not set...";
      return 0;
   }

   $self->{_ID} = $sth->{mysql_insertid};
   return $self->{_ID};
}


# just return the last error
sub error {
   my($self) = @_;
   return $self->{ERROR};
}

# RETURN AN ARRAY OF ACCESS GROUP IDS TO WHICH THIS USER BELONGS
sub queryAccessGroups {
   my($self) = @_;
   my($sql, $sth, @arr, @groups);

   @groups = ( );

   if(!$self->{_ID}) {
      $self->{ERROR} = "ID not set...";
      return 0;
   }

   if(!$self->{DBH}) {
      $self->{ERROR} = "DBH not set...";
      return 0;
   }

   $sql = "SELECT groupID FROM tblAccessGroupMembers WHERE yrcID = " . $self->{_ID};
   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;

   if(!$sth->rows) { return 0; }

   while (@arr = $sth->fetchrow_array()) {
      push( @groups, ($arr[0]) );
   }

   return @groups;
}

# RETURN 1 IF A MEMBER OF SUPPLIED GROUP ID, 0 IF NOT
sub queryAccessGroupMembership {
   my($self, $group) = @_;

   if(!$group) { return 0; }

   if(!$self->{_ID}) {
      $self->{ERROR} = "ID not set...";
      return 0;
   }

   if(!$self->{DBH}) {
      $self->{ERROR} = "DBH not set...";
      return 0;
   }

   foreach ($self->queryAccessGroups()) {

      # AUTOMATICALLY RETURN 1 IF THEY BELONG TO THE ADMIN GROUP
      if($_ == 1) { return 1; }

      if($_ == $group) { return 1; }
   }

   return 0;
}

sub checkGroup {
   my($self, %group) = @_;
   my($group);

   # ALWAYS RETURN 1 IF THEY ARE A MEMBER OF THE ADMINSTRATORS GROUP
   if($self->queryAccessGroupMembership( 1 )) { return 1; }
   
   if($group{NAME}) { $group = $self->queryGroupNumber($group{NAME}); }
   elsif($group{NUMBER}) { $group = $group{NUMBER}; }
   else { $group = 0; }

   if(!$self->queryAccessGroupMembership( $group )) {
      my($msg) = <<END;
<P>Access to this page is restricted.

<P>If you feel that you are receiving this message in error, please contact
<A HREF="mailto:mriffle\@u.washington.edu">the webmaster</A>.
END
   
      YRC::WWW::ERROR->showError($msg);
      print YRC::WWW::TEMPLATES->getFooter();
      exit 0;
   }

}

# RETURNS 1 IF THIS USER HAS ACCESS TO A GROUP FOR WHICH THE GIVEN PROJECT IS AFFILIATED
# RETURNS 0 IF NOT
sub queryProjectAccess {
   my($self, $PROJECT) = @_;

   # ALWAYS RETURN 1 IF THEY ARE A MEMBER OF THE ADMINSTRATORS GROUP
   if($self->queryAccessGroupMembership( 1 )) { return 1; }

   foreach ($PROJECT->queryGroups()) {
      if($self->queryAccessGroupMembership( $self->queryGroupNumber($_) ) ) { return 1; }
   }

   return 0;

}

sub restrictToProjectGroup {
   my($self, $PROJECT) = @_;

   # ALWAYS RETURN 1 IF THEY ARE A MEMBER OF THE ADMINSTRATORS GROUP
   if($self->queryAccessGroupMembership( 1 )) { return 1; }

   foreach ($PROJECT->queryGroups()) {
      if($self->queryAccessGroupMembership( $self->queryGroupNumber($_) ) ) { return 1; }
   }

   my($msg) = <<END;
<P>Access to this page is restricted to individuals involved with this project.
   
<P>If you feel that you are receiving this message in error, please contact
<A HREF="mailto:mriffle\@u.washington.edu">the webmaster</A>.
END
   
   YRC::WWW::ERROR->showError($msg);
   print YRC::WWW::TEMPLATES->getFooter();
   exit 0;
}

# RETURNS THE GROUP NUMBER FOR A GIVEN GROUP NAME
sub queryGroupNumber {
   my($self, $groupname) = @_;
   my($sth, $sql, @arr);

   if(!$self->{DBH}) { return 0; }

   $sql = "SELECT groupID FROM tblAccessGroups WHERE groupName = '" . $groupname . "'";
   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;

   if($sth->rows) {
      @arr = $sth->fetchrow_array;
      return $arr[0];
   }

   return 0;
}

# RETURNS THE GROUP NAME FOR A GIVEN GROUP NUMBER
sub queryGroupName {
   my($self, $groupnumber) = @_;
   my($sth, $sql, @arr);

   if(!$self->{DBH}) { return 0; }

   $sql = "SELECT groupName FROM tblAccessGroups WHERE groupID = $groupnumber";
   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;

   if($sth->rows) {
      @arr = $sth->fetchrow_array;
      return $arr[0];
   }

   return 0;
}

# SETS AND SAVES A PASSWORD  TAKES AN ENCRYPTED PASSWORD USING MD5_HEX
sub setPassword {
   my($self, $password) = @_;
   my($sql);

   if(!$self->{DBH}) { $self->{DBH} = YRC::DB->getDBH(); }

   $sql = "UPDATE tblYRCLogins SET loginPassword = '$password', loginLastChanged = NOW() WHERE yrcID = " . $self->{_ID};
   $self->{DBH}->do($sql);


   return 1;
}

1;
