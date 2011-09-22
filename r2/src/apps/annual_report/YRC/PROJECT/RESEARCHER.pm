################################################
#
# Michael Riffle <mriffle@u.washington.edu>
# 3/19/2002
#
################################################

package YRC::PROJECT::RESEARCHER;
use strict;

sub new {
   my ($class, %args) = @_;
   my ($self) = bless {}, ref($class)||$class;
   $self->_init(%args);
   return $self;
}

# initialize the RESEARCHER object
sub _init {
   my ($self, %args) = @_;

   $self->{TABLE} = "tblResearchers";

   return 1;
}

# sets up a reference to a database handle for
# use by this object
sub setDBH {
   my ($self, $dbh) = @_;
   $self->{DBH} = $dbh;
}

# sets a variable in this object to a value
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


# returns the value for a given attribute
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
   my ($self, $ID) = @_;
   my ($sql, $sth, $hashref, $key, $nkey);

   if( !$ID && !( $ID = $self->{_ID} ) ) { return 0; }

   if(!$self->{DBH}) {
      $self->{ERROR} = "No database handle set.  Call setDBH first.";
      return 0;
   }

   $sql = "SELECT * FROM " . $self->{TABLE} . " WHERE researcherID = " . $ID;
   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;
   if(!$sth->rows) { return 0; }

   $hashref = $sth->fetchrow_hashref();
   foreach $key (keys(%{$hashref})) {
      $nkey = $key;
      $nkey =~ tr/[a-z]/[A-Z]/;
      $nkey =~ s/RESEARCHER//;
      $self->{"_$nkey"} = $hashref->{$key};
   }
   return 1;
}

# save this researcher into the database as a new entry.
# returns 1 if successful, 0 if not.
sub save {
   my($self) = @_;
   my($sql, $sth, @arr);

   if(!$self->{DBH}) {
      $self->{ERROR} = "DBH not set...";
      return 0;
   }

   if(!$self->{_FIRSTNAME} || !$self->{_LASTNAME}) {
      $self->{ERROR} = "No name set...";
      return 0;
   }   

   $self->{_FIRSTNAME} = $self->{DBH}->quote($self->{_FIRSTNAME});
   $self->{_LASTNAME} = $self->{DBH}->quote($self->{_LASTNAME});
   $self->{_EMAIL} = $self->{DBH}->quote($self->{_EMAIL});
   $self->{_PHONE} = $self->{DBH}->quote($self->{_PHONE});
   $self->{_DEGREE} = $self->{DBH}->quote($self->{_DEGREE});
   $self->{_DEPT} = $self->{DBH}->quote($self->{_DEPT});
   $self->{_ORGANIZATION} = $self->{DBH}->quote($self->{_ORGANIZATION});
   $self->{_STATE} = $self->{DBH}->quote($self->{_STATE});
   $self->{_ZIP} = $self->{DBH}->quote($self->{_ZIP});
   $self->{_COUNTRY} = $self->{DBH}->quote($self->{_COUNTRY});

   # CHECK TO SEE IF THIS ID ALREADY EXISTS IN THE DATABASE.  IF SO, PERFORM AN UPDATE
   if($self->{_ID}) {
      $sql = "SELECT researcherID FROM tblResearchers WHERE researcherID = " . $self->{_ID};
      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;
      if($sth->rows) {
         # THIS ID IS ALREADY IN THE DATABASE, PERFORM UPDATE
         $sql  = "UPDATE tblResearchers SET ";
         $sql .= "researcherFirstName = " . $self->{_FIRSTNAME} . ", ";
         $sql .= "researcherLastName = " . $self->{_LASTNAME} . ", ";
         $sql .= "researcherEmail = " . $self->{_EMAIL} . ", ";
         $sql .= "researcherDegree = " . $self->{_DEGREE} . ", ";
         $sql .= "researcherDept = " . $self->{_DEPT} . ", ";
         $sql .= "researcherOrganization = " . $self->{_ORGANIZATION} . ", ";
         $sql .= "researcherState = " . $self->{_STATE} . ", ";
         $sql .= "researcherZip = " . $self->{_ZIP} . ", ";
         $sql .= "researcherCountry = " . $self->{_COUNTRY} . " WHERE ";
         $sql .= "researcherID = " . $self->{_ID};

         $sth = $self->{DBH}->prepare($sql);
         $sth->execute;

         return 1;
      }
      # WE HAVE AN ID, BUT IT'S NOT IN THE DATABASE?  QUITE STRANGE...  DO NOTHING
   }


   # CHECK TO SEE IF A RESEARCHER W/ THIS FIRST/LAST/EMAIL ALREADY EXISTS
   # IF SO, SET ID TO THAT RESEARCHER, SET ERROR MESSAGE AND RETURN 0
   $sql  = "SELECT researcherID FROM tblResearchers WHERE ";
   $sql .= "LOWER(researcherFirstName) = " . lc($self->{_FIRSTNAME}) . " AND ";
   $sql .= "LOWER(researcherLastName) = " . lc($self->{_LASTNAME}) . " AND ";
   $sql .= "LOWER(researcherEmail) = " . lc($self->{_EMAIL});
   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;
   if($sth->rows) {
      # THIS INVESTIGATOR IS ALREADY IN THE DATABASE!!
      $self->{ERROR} = "This researcher already exists...";
      @arr = $sth->fetchrow_array;
      $self->{_ID} = $arr[0];
      return 0;
   }

   # BUILD OUR SQL STATEMENT
   $sql =  "INSERT INTO tblResearchers (researcherFirstName, researcherLastName, researcherEmail, researcherPhone, ";
   $sql .= "researcherDegree, researcherDept, researcherOrganization, researcherState, researcherZip, ";
   $sql .= "researcherCountry) VALUES (";
   $sql .= $self->{_FIRSTNAME} . ",";
   $sql .= $self->{_LASTNAME} . ",";
   $sql .= $self->{_EMAIL} . ",";
   $sql .= $self->{_PHONE} . ",";
   $sql .= $self->{_DEGREE} . ",";
   $sql .= $self->{_DEPT} . ",";
   $sql .= $self->{_ORGANIZATION} . ",";
   $sql .= $self->{_STATE} . ",";
   $sql .= $self->{_ZIP} . ",";
   $sql .= $self->{_COUNTRY};
   $sql .= ")";

   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;

   $self->{_ID} = $sth->{mysql_insertid};
   return $self->{_ID};
}

# RETURN AN ARRAY OF PROJECT IDS FOR WHICH THIS RESEARCHER IS ASSOCIATED
sub queryProjects {
   my($self, $ID) = @_;
   my($sql, $sth, @arr, %result);

   if(!$ID && !($ID = $self->{_ID})) { return 0; }
   if(!$self->{DBH}) { return 0; }

   $sql =  "SELECT projectID FROM tblProjects WHERE projectPI = $ID OR projectResearcherB = $ID OR ";
   $sql .= "projectResearcherC = $ID OR projectResearcherD = $ID OR projectResearcherE = $ID";

   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;

   if(!$sth->rows) { return 0; }

   while(@arr = $sth->fetchrow_array()) {
      $result{$arr[0]} = 1;
   }

   return keys(%result);
}



# just return the last error
sub error {
   my($self) = @_;
   return $self->{ERROR};
}

1;
