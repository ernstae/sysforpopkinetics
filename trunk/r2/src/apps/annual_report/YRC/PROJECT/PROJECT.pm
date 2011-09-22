################################################
#
# Michael Riffle <mriffle@u.washington.edu>
# 3/19/2002
#
################################################

package YRC::PROJECT::PROJECT;
use strict;

use lib "/usr/local/lib/site_perl";
use YRC::PROJECT::RESEARCHER;
use YRC::DB;

sub new {
   my ($class, %args) = @_;
   my ($self) = bless {}, ref($class)||$class;
   $self->_init(%args);
   return $self;
}

# initialize the PROJECT object
sub _init {
   my ($self, %args) = @_;

   $self->{TABLE} = "tblProjects";

   $self->{TRAINING} = { };
   $self->{DISSEMINATION} = { };
   $self->{COLLABORATION} = { };
   $self->{TECHNOLOGY} = { };

   return 1;
}

# sets up a reference to a database handle for
# use by this object
sub setDBH {
   my ($self, $dbh) = @_;

   if(!$dbh) {
      my($DBH) = YRC::DB->getDBH();
      $self->{DBH} = $DBH;
   } else {
      $self->{DBH} = $dbh;
   }
}

# sets a variable in this object to a value
sub set {
   my ($self, %args) = @_;
   my ($key, $nkey);

   foreach $key (keys(%args)) {
      $nkey = $key;
      $nkey =~ tr/[a-z]/[A-Z]/;
      $self->{"_$nkey"} = $args{$key};
   }
   return 1;
}

# sets a variable in this object to a value
sub setDissemination {
   my ($self, %args) = @_;
   my ($key, $nkey);

   foreach $key (keys(%args)) {
      $nkey = $key;
      $nkey =~ tr/[a-z]/[A-Z]/;
      $self->{DISSEMINATION}{"_$nkey"} = $args{$key};
   }
   return 1;
}
# sets a variable in this object to a value
sub setTraining {
   my ($self, %args) = @_;
   my ($key, $nkey);

   foreach $key (keys(%args)) {
      $nkey = $key;
      $nkey =~ tr/[a-z]/[A-Z]/;
      $self->{TRAINING}{"_$nkey"} = $args{$key};
   }
   return 1;
}
# sets a variable in this object to a value
sub setCollaboration {
   my ($self, %args) = @_;
   my ($key, $nkey);

   foreach $key (keys(%args)) {
      $nkey = $key;
      $nkey =~ tr/[a-z]/[A-Z]/;
      $self->{COLLABORATION}{"_$nkey"} = $args{$key};
   }
   return 1;
}
# sets a variable in this object to a value
sub setTechnology {
   my ($self, %args) = @_;
   my ($key, $nkey);

   foreach $key (keys(%args)) {
      $nkey = $key;
      $nkey =~ tr/[a-z]/[A-Z]/;
      $self->{TECHNOLOGY}{"_$nkey"} = $args{$key};
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

   return ($self->{"_$attr"} || '');
}      

# returns the value for a given attribute
sub queryTraining {
   my ($self, $attr) = @_;

   $attr =~ tr/[a-z]/[A-Z]/;
   if(!$self->{TRAINING}{"_$attr"}) {
      # NO ATTRIBUTE SET, LOAD IN DATA FROM DB
      if(!$self->{_ID}) {
         # CAN'T LOAD IN DATA W/O AN ID
         $self->{ERROR} = "No attribute set, and no ID set (can not query DB).";
         return 0;
      }
      # LOAD THE DATA
      $self->loadTraining();
   }

   return ($self->{TRAINING}{"_$attr"} || '');
}

# returns the value for a given attribute
sub queryDissemination {
   my ($self, $attr) = @_;

   $attr =~ tr/[a-z]/[A-Z]/;
   if(!$self->{DISSEMINATION}{"_$attr"}) {
      # NO ATTRIBUTE SET, LOAD IN DATA FROM DB
      if(!$self->{_ID}) {
         # CAN'T LOAD IN DATA W/O AN ID
         $self->{ERROR} = "No attribute set, and no ID set (can not query DB).";
         return 0;
      }
      # LOAD THE DATA
      $self->loadDissemination();
   }

   return ($self->{DISSEMINATION}{"_$attr"} || '');
}

# returns the value for a given attribute
sub queryCollaboration {
   my ($self, $attr) = @_;

   $attr =~ tr/[a-z]/[A-Z]/;
   if(!$self->{COLLABORATION}{"_$attr"}) {
      # NO ATTRIBUTE SET, LOAD IN DATA FROM DB
      if(!$self->{_ID}) {
         # CAN'T LOAD IN DATA W/O AN ID
         $self->{ERROR} = "No attribute set, and no ID set (can not query DB).";
         return 0;
      }
      # LOAD THE DATA
      $self->loadCollaboration();
   }

   return ($self->{COLLABORATION}{"_$attr"} || '');
}      
# returns the value for a given attribute
sub queryTechnology {
   my ($self, $attr) = @_;

   $attr =~ tr/[a-z]/[A-Z]/;
   if(!$self->{TECHNOLOGY}{"_$attr"}) {
      # NO ATTRIBUTE SET, LOAD IN DATA FROM DB
      if(!$self->{_ID}) {
         # CAN'T LOAD IN DATA W/O AN ID
         $self->{ERROR} = "No attribute set, and no ID set (can not query DB).";
         return 0;
      }
      # LOAD THE DATA
      $self->loadTechnology();
   }

   return $self->{TECHNOLOGY}{"_$attr"};
}      

# loads data from database for the given ID into object variables
# with the same name (capitalized) of the database table columns
sub load {
   my ($self, $ID) = @_;
   my ($sql, $sth, $hashref, $key, $nkey);

   if( !$ID && !( $ID = $self->{_ID} ) ) { return 0; }

   if(!$self->{DBH}) { $self->setDBH(); }

   $sql = "SELECT * FROM " . $self->{TABLE} . " WHERE projectID = " . $ID;
   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;
   if(!$sth->rows) { return 0; }

   $hashref = $sth->fetchrow_hashref();
   foreach $key (keys(%{$hashref})) {
      $nkey = $key;
      $nkey =~ tr/[a-z]/[A-Z]/;
      $nkey =~ s/PROJECT//;
      $self->{"_$nkey"} = $hashref->{$key};
   }
   return 1;
}

# loads data from database for the given ID into object variables
# with the same name (capitalized) of the database table columns
sub loadTraining {
   my ($self, $ID) = @_;
   my ($sql, $sth, $hashref, $key, $nkey);

   if( !$ID && !( $ID = $self->{_ID} ) ) { return 0; }

   if(!$self->{DBH}) {
      $self->{ERROR} = "No database handle set.  Call setDBH first.";
      return 0;
   }

   $sql = "SELECT * FROM tblTraining WHERE projectID = " . $ID;
   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;
   if(!$sth->rows) { return 0; }

   $hashref = $sth->fetchrow_hashref();
   foreach $key (keys(%{$hashref})) {
      $nkey = $key;
      $nkey =~ tr/[a-z]/[A-Z]/;
      $nkey =~ s/TRAINING//;
      $self->{TRAINING}{"_$nkey"} = $hashref->{$key};
   }
   return 1;
}

# loads data from database for the given ID into object variables
# with the same name (capitalized) of the database table columns
sub loadDissemination {
   my ($self, $ID) = @_;
   my ($sql, $sth, $hashref, $key, $nkey);

   if( !$ID && !( $ID = $self->{_ID} ) ) { return 0; }

   if(!$self->{DBH}) {
      $self->{ERROR} = "No database handle set.  Call setDBH first.";
      return 0;
   }

   $sql = "SELECT * FROM tblDissemination WHERE projectID = " . $ID;
   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;
   if(!$sth->rows) { return 0; }

   $hashref = $sth->fetchrow_hashref();
   foreach $key (keys(%{$hashref})) {
      $nkey = $key;
      $nkey =~ tr/[a-z]/[A-Z]/;
      $nkey =~ s/SHIP//;
      $self->{DISSEMINATION}{"_$nkey"} = $hashref->{$key};
   }
   return 1;
}

# loads data from database for the given ID into object variables
# with the same name (capitalized) of the database table columns
sub loadCollaboration {
   my ($self, $ID) = @_;
   my ($sql, $sth, $hashref, $key, $nkey);

   if( !$ID && !( $ID = $self->{_ID} ) ) { return 0; }

   if(!$self->{DBH}) {
      $self->{ERROR} = "No database handle set.  Call setDBH first.";
      return 0;
   }

   $sql = "SELECT * FROM tblCollaboration WHERE projectID = " . $ID;
   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;
   if(!$sth->rows) { return 0; }

   $hashref = $sth->fetchrow_hashref();
   foreach $key (keys(%{$hashref})) {
      $nkey = $key;
      $nkey =~ tr/[a-z]/[A-Z]/;
      $nkey =~ s/SHIP//;
      $self->{COLLABORATION}{"_$nkey"} = $hashref->{$key};
   }
   return 1;
}
# loads data from database for the given ID into object variables
# with the same name (capitalized) of the database table columns
sub loadTechnology {
   my ($self, $ID) = @_;
   my ($sql, $sth, $hashref, $key, $nkey);

   if( !$ID && !( $ID = $self->{_ID} ) ) { return 0; }

   if(!$self->{DBH}) {
      $self->{ERROR} = "No database handle set.  Call setDBH first.";
      return 0;
   }

   $sql = "SELECT * FROM tblTechnology WHERE projectID = " . $ID;
   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;
   if(!$sth->rows) { return 0; }

   $hashref = $sth->fetchrow_hashref();
   foreach $key (keys(%{$hashref})) {
      $nkey = $key;
      $nkey =~ tr/[a-z]/[A-Z]/;
      $nkey =~ s/SHIP//;
      $self->{TECHNOLOGY}{"_$nkey"} = $hashref->{$key};
   }
   return 1;
}

# save this researcher into the database as a new entry.
# returns 1 if successful, 0 if not.
sub save {
   my($self) = @_;
   my($sql, $sth);
   my($action);

   ##########################################################################
   #########################  SAVE MAIN PROJECT DATA  #######################
   ##########################################################################

   $self->{_TITLE} = $self->{DBH}->quote($self->{_TITLE});
   $self->{_TYPE} = $self->{DBH}->quote($self->{_TYPE});

   if($self->{_SUBMITDATE}) { $self->{_SUBMITDATE} = $self->{DBH}->quote($self->{_SUBMITDATE}); }
   else { $self->{_SUBMITDATE} = "NOW()"; }

   $self->{_FUNDINGTYPES} = $self->{DBH}->quote($self->{_FUNDINGTYPES});
   $self->{_FUNDINGFEDERAL} = $self->{DBH}->quote($self->{_FUNDINGFEDERAL});
   $self->{_ABSTRACT} = $self->{DBH}->quote($self->{_ABSTRACT});
   $self->{_PROGRESS} = $self->{DBH}->quote($self->{_PROGRESS});
   $self->{_KEYWORDS} = $self->{DBH}->quote($self->{_KEYWORDS});
   $self->{_PUBLICATIONS} = $self->{DBH}->quote($self->{_PUBLICATIONS});
   $self->{_COMMENTS} = $self->{DBH}->quote($self->{_COMMENTS});
   $self->{_AXISI} = $self->{DBH}->quote($self->{_AXISI});
   $self->{_AXISII} = $self->{DBH}->quote($self->{_AXISII});

   if($self->{_NEW}) { $self->{_NEW} = $self->{DBH}->quote($self->{_NEW}); }
   else { $self->{_NEW} = "'T'"; }

   # DETERMINE IF WE'RE INSERTING OR UPDATING
   if($self->{_ID}) {
      $sql = "SELECT projectID FROM tblProjects WHERE projectID = " . $self->{_ID};
      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;
      if($sth->rows) {
         # WE HAVE AN ID SET AND THIS ID IS IN THE DATABASE:  PERFORM UPDATE
         $action = 'update';
      } else {
         # WE HAVE AN ID SET BUT IT'S NOT IN THE DATABASE?  WTF:  PERFORM INSERT
         $self->{_ID} = 0;
         $action = 'insert';
      }
   } else {
         # WE HAVE NO ID SET:  PERFORM INSERT
         $action = 'insert';
   }

   # BUILD OUR SQL STATEMENT
   if($action eq 'insert') {
      $sql =  "INSERT INTO tblProjects (projectType, projectSubmitDate, projectTitle, projectPI, ";
      $sql .= "projectResearcherB, projectResearcherC, projectResearcherD, projectFundingTypes, ";
      $sql .= "projectFundingFederal, projectAbstract, projectProgress, projectKeywords, projectPublications, ";
      $sql .= "projectComments, projectNew, projectAXISI, projectAXISII, projectBTA) VALUES (";
      $sql .= $self->{_TYPE} . ",";
      $sql .= $self->{_SUBMITDATE} . ",";
      $sql .= $self->{_TITLE} . ",";
      $sql .= $self->{_PI} . ",";
      $sql .= $self->{_RESEARCHERB} . ",";
      $sql .= $self->{_RESEARCHERC} . ",";
      $sql .= $self->{_RESEARCHERD} . ",";
      $sql .= $self->{_FUNDINGTYPES} . ",";
      $sql .= $self->{_FUNDINGFEDERAL} . ",";
      $sql .= $self->{_ABSTRACT} . ",";
      $sql .= $self->{_PROGRESS} . ",";
      $sql .= $self->{_KEYWORDS} . ",";
      $sql .= $self->{_PUBLICATIONS} . ",";
      $sql .= $self->{_COMMENTS} . ",";
      $sql .= $self->{_NEW} . ",";
      $sql .= $self->{_AXISI} . ",";
      $sql .= $self->{_AXISII} . ",";
      $sql .= $self->{_BTA};
      $sql .= ")";
   } else {
      $sql =  "UPDATE tblProjects SET ";
      $sql .= "projectType = " . $self->{_TYPE} . ", ";
      $sql .= "projectSubmitDate = " . $self->{_SUBMITDATE} . ", ";
      $sql .= "projectTitle = " . $self->{_TITLE} . ", ";
      $sql .= "projectPI = " . $self->{_PI} . ", ";
      $sql .= "projectResearcherB = " . $self->{_RESEARCHERB} . ", ";
      $sql .= "projectResearcherC = " . $self->{_RESEARCHERC} . ", ";
      $sql .= "projectResearcherD = " . $self->{_RESEARCHERD} . ", ";
      $sql .= "projectFundingTypes = " . $self->{_FUNDINGTYPES} . ", ";
      $sql .= "projectFundingFederal = " . $self->{_FUNDINGFEDERAL} . ", ";
      $sql .= "projectAbstract = " . $self->{_ABSTRACT} . ", ";
      $sql .= "projectProgress = " . $self->{_PROGRESS} . ", ";
      $sql .= "projectKeywords = " . $self->{_KEYWORDS} . ", ";
      $sql .= "projectPublications = " . $self->{_PUBLICATIONS} . ", ";
      $sql .= "projectComments = " . $self->{_COMMENTS} . ", ";
      $sql .= "projectBTA = " . $self->{_BTA} . ", ";
      $sql .= "projectNew = " . $self->{_NEW} . ", ";
      $sql .= "projectAXISI = " . $self->{_AXISI} . ", ";
      $sql .= "projectAXISII = " . $self->{_AXISII} . " ";
      $sql .= "WHERE projectID = " . $self->{_ID};
   }

#   print "Content-type: text/html\n\n";
#   print $sql;
#   exit 0;

   $sth = $self->{DBH}->prepare($sql);
   $sth->execute;

   if($action eq 'insert') { $self->{_ID} = $sth->{mysql_insertid}; }
   if(!$self->{_ID}) {
      $self->{ERROR} = "No INSERT ID returned after insert...";
      return 0;
   }

   ##########################################################################
   ####################  NOW SAVE TRAINING INFORMATION  #####################
   ##########################################################################

   if($self->{TRAINING}{_GROUPS}) {

      # DO WE UPDATE OR INSERT?
      $sql = "SELECT projectID FROM tblTraining WHERE projectID = " . $self->{_ID};
      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;
      if($sth->rows) {
         # THIS ID IS IN THE DATABASE:  PERFORM UPDATE
         $action = 'update';
      } else {
         # THIS ID IS NOT IN THE DATABASE:  PERFORM INSERT
         $action = 'insert';
      }

      $self->{TRAINING}{_DESCRIPTION} = $self->{DBH}->quote($self->{TRAINING}{_DESCRIPTION});
      $self->{TRAINING}{_GROUPS} = $self->{DBH}->quote($self->{TRAINING}{_GROUPS});
      if(!$self->{TRAINING}{_HOURS}) { $self->{TRAINING}{_HOURS} = 0; }
      if(!$self->{TRAINING}{_DAYS}) { $self->{TRAINING}{_DAYS} = 0; }

      if($action eq 'insert') {
         $sql  = "INSERT INTO tblTraining (projectID, trainingHours, trainingDays, trainingDescription, trainingGroups) VALUES (";
         $sql .= $self->{_ID} . "," . $self->{TRAINING}{_HOURS} . "," . $self->{TRAINING}{_DAYS} . ",";
         $sql .= $self->{TRAINING}{_DESCRIPTION} . "," . $self->{TRAINING}{_GROUPS} . ")";
      } else {
         $sql  = "UPDATE tblTraining SET ";
         $sql .= "trainingHours = " . $self->{TRAINING}{_HOURS} . ", ";
         $sql .= "trainingDays = " . $self->{TRAINING}{_DAYS} . ", ";
         $sql .= "trainingDescription = " . $self->{TRAINING}{_DESCRIPTION} . ", ";
         $sql .= "trainingGroups = " . $self->{TRAINING}{_GROUPS} . " ";
         $sql .= "WHERE projectID = " . $self->{_ID};
      }
      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;
   } else {
      if($self->queryGroups('T')) {
         $sql = "DELETE FROM tblTraining WHERE projectID = " . $self->{_ID};
         $self->{DBH}->do($sql);
      }
   }

   ##########################################################################
   ##################  NOW SAVE COLLABORATION INFORMATION  ##################
   ##########################################################################

   if($self->{COLLABORATION}{_GROUPS}) {

      # DO WE UPDATE OR INSERT?
      $sql = "SELECT projectID FROM tblCollaboration WHERE projectID = " . $self->{_ID};
      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;
      if($sth->rows) {
         # THIS ID IS IN THE DATABASE:  PERFORM UPDATE
         $action = 'update';
      } else {
         # THIS ID IS NOT IN THE DATABASE:  PERFORM INSERT
         $action = 'insert';
      }


      $self->{COLLABORATION}{_GROUPS} = $self->{DBH}->quote($self->{COLLABORATION}{_GROUPS});

      if($action eq 'insert') {
         $sql  = "INSERT INTO tblCollaboration (projectID, collGroups) VALUES (";
         $sql .= $self->{_ID} . "," . $self->{COLLABORATION}{_GROUPS} . ")";
      } else {
         $sql = "UPDATE tblCollaboration SET ";
         $sql .= "collGroups = " . $self->{COLLABORATION}{_GROUPS} . " ";
         $sql .= "WHERE projectID = " . $self->{_ID};
      }
      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;
   } else {
      if($self->queryGroups('C')) {
         $sql = "DELETE FROM tblCollaboration WHERE projectID = " . $self->{_ID};
         $self->{DBH}->do($sql);
      }
   }


   ##########################################################################
   ##################  NOW SAVE TECHNOLOGY INFORMATION  #####################
   ##########################################################################

   if($self->{TECHNOLOGY}{_GROUPS}) {

      # DO WE UPDATE OR INSERT?
      $sql = "SELECT projectID FROM tblTechnology WHERE projectID = " . $self->{_ID};
      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;
      if($sth->rows) {
         # THIS ID IS IN THE DATABASE:  PERFORM UPDATE
         $action = 'update';
      } else {
         # THIS ID IS NOT IN THE DATABASE:  PERFORM INSERT
         $action = 'insert';
      }


      $self->{TECHNOLOGY}{_GROUPS} = $self->{DBH}->quote($self->{TECHNOLOGY}{_GROUPS});

      if($action eq 'insert') {
         $sql  = "INSERT INTO tblTechnology (projectID, techGroups) VALUES (";
         $sql .= $self->{_ID} . "," . $self->{TECHNOLOGY}{_GROUPS} . ")";
      } else {
         $sql = "UPDATE tblTechnology SET ";
         $sql .= "techGroups = " . $self->{TECHNOLOGY}{_GROUPS} . " ";
         $sql .= "WHERE projectID = " . $self->{_ID};
      }
      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;
   } else {
      if($self->queryGroups('Tech')) {
         $sql = "DELETE FROM tblTechnology WHERE projectID = " . $self->{_ID};
         $self->{DBH}->do($sql);
      }
   }


   ##########################################################################
   ##################  NOW SAVE DISSEMINATION INFORMATION  ##################
   ##########################################################################

   if($self->{DISSEMINATION}{_PLASMIDTYPE}) {

      # DO WE UPDATE OR INSERT?
      $sql = "SELECT projectID FROM tblDissemination WHERE projectID = " . $self->{_ID};
      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;
      if($sth->rows) {
         # THIS ID IS IN THE DATABASE:  PERFORM UPDATE
         $action = 'update';
      } else {
         # THIS ID IS NOT IN THE DATABASE:  PERFORM INSERT
         $action = 'insert';
      }



      $self->{DISSEMINATION}{_PLASMIDTYPE}	=	$self->{DBH}->quote($self->{DISSEMINATION}{_PLASMIDTYPE});
      $self->{DISSEMINATION}{_NAME}		=	$self->{DBH}->quote($self->{DISSEMINATION}{_NAME});
      $self->{DISSEMINATION}{_PHONE}		=	$self->{DBH}->quote($self->{DISSEMINATION}{_PHONE});
      $self->{DISSEMINATION}{_EMAIL}		=	$self->{DBH}->quote($self->{DISSEMINATION}{_EMAIL});
      $self->{DISSEMINATION}{_ADDRESS}		=	$self->{DBH}->quote($self->{DISSEMINATION}{_ADDRESS});
      $self->{DISSEMINATION}{_DESCRIPTION}	=	$self->{DBH}->quote($self->{DISSEMINATION}{_DESCRIPTION});
      $self->{DISSEMINATION}{_FEDEX}		=	$self->{DBH}->quote($self->{DISSEMINATION}{_FEDEX});
      $self->{DISSEMINATION}{_COMMENTS}		=	$self->{DBH}->quote($self->{DISSEMINATION}{_COMMENTS});
      $self->{DISSEMINATION}{_COMMERCIAL}	=	$self->{DBH}->quote($self->{DISSEMINATION}{_COMMERCIAL});
      $self->{DISSEMINATION}{_SHIPPED}		=	$self->{DBH}->quote($self->{DISSEMINATION}{_SHIPPED});

      if($action eq 'insert') {
         $sql =  "INSERT INTO tblDissemination (projectID, shipPlasmidType, shipName, shipPhone, shipEmail, ";
         $sql .= "shipAddress, shipDescription, shipFEDEX, shipComments, shipCommercial, shipShipped) VALUES (";
         $sql .= $self->{_ID} . "," . $self->{DISSEMINATION}{_PLASMIDTYPE} . "," . $self->{DISSEMINATION}{_NAME} . ",";
         $sql .= $self->{DISSEMINATION}{_PHONE} . "," . $self->{DISSEMINATION}{_EMAIL} . ",";
         $sql .= $self->{DISSEMINATION}{_ADDRESS} . "," . $self->{DISSEMINATION}{_DESCRIPTION} . ",";
         $sql .= $self->{DISSEMINATION}{_FEDEX} . "," . $self->{DISSEMINATION}{_COMMENTS} . ",";
         $sql .= $self->{DISSEMINATION}{_COMMERCIAL} . "," . $self->{DISSEMINATION}{_SHIPPED} . ")";
      } else {
         $sql =  "UPDATE tblDissemination SET ";
         $sql .= "shipPlasmidType = " . $self->{DISSEMINATION}{_PLASMIDTYPE} . ", ";
         $sql .= "shipName = " . $self->{DISSEMINATION}{_NAME} . ", ";
         $sql .= "shipPhone = " . $self->{DISSEMINATION}{_PHONE} . ", ";
         $sql .= "shipEmail = " . $self->{DISSEMINATION}{_EMAIL} . ", ";
         $sql .= "shipAddress = " . $self->{DISSEMINATION}{_ADDRESS} . ", ";
         $sql .= "shipDescription = " . $self->{DISSEMINATION}{_DESCRIPTION} . ", ";
         $sql .= "shipFEDEX = " . $self->{DISSEMINATION}{_FEDEX} . ", ";
         $sql .= "shipComments = " . $self->{DISSEMINATION}{_COMMENTS} . ", ";
         $sql .= "shipCommercial = " . $self->{DISSEMINATION}{_COMMERCIAL} . ", ";
         $sql .= "shipShipped = " . $self->{DISSEMINATION}{_SHIPPED} . " ";
         $sql .= "WHERE projectID = " . $self->{_ID};
      }

      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;
   } else {
      if($self->queryGroups('D')) {
         $sql = "DELETE FROM tblDissemination WHERE projectID = " . $self->{_ID};
         $self->{DBH}->do($sql);
      }
   }


   return $self->{_ID};
}


# just return the last error
sub error {
   my($self) = @_;
   $self->{ERROR} || "No error.";
}

# RETURN AN OBJECT CORRESPONDING TO THE RESEARCHER REQUESTED
# TAKES EITHER 'PI', 'B', 'C', 'D', 'E'
# DEFAULTS TO 'PI'
sub queryResearcher {
   my($self, $who) = @_;
   my($RESEARCHER);

   # DO WE HAVE A PROJECT ID SET?  IF NOT, THIS IS IMPOSSIBLE
   if(!$self->{_ID}) {
      $self->{ERROR} = "Call to queryResearcher with no project ID set in object.";
      return 0;
   }

   # WHAT NO DATABASE HANDLE?
   if(!$self->{DBH}) {
      $self->{ERROR} = "Call to queryResearcher() with no database handle set.";
      return 0;
   }

   # WHAT, NO WHO?  DEFAULT TO PI
   if(!$who) { $who = 'PI'; }

   # MAKE SURE WE HAVE VALID PARAMETERS
   if($who ne 'PI' && !($who =~ /[BCDE]/)) {
      $self->{ERROR} = "Parameter must be 'PI', 'B', 'C', 'D' or 'E'.  Parameter was set to '$who'.";
      return 0;
   }

   # CONVERT PARAMETER TO USABLE LOCAL PARAMETERS
   unless($who eq 'PI') {
      $who = 'RESEARCHER' . $who;
   }

   if(!($who = $self->query($who))) { return 0; }

   $RESEARCHER = YRC::PROJECT::RESEARCHER->new();
   $RESEARCHER->setDBH($self->{DBH});
   if(!$RESEARCHER->load($who)) {
      $self->{ERROR} = "Unable to load researcher ($who) through load().";
      return 0;
   }

   return $RESEARCHER;
}

# THIS WILL RETURN 1 IF THIS PROJECT IS ASSOCIATED W/ THE SUPPLIED GROUP (the 5 groups)
# OR 0 IF NOT
sub queryGroup {
  my($self, $group) = @_;
  my(@groups) = ();

   if(!$self->{DBH}) {
      $self->{ERROR} = "Call to queryGroups with no database handle set.";
      return 0;
   }

   if(!$self->{_ID}) {
      $self->{ERROR} = "Call to queryGroups with no ID set.";
      return 0;
   }

   @groups = $self->queryGroups();

   foreach (@groups) {
      if($group eq $_) { return 1; }
   }

   return 0;
}


# THIS WILL RETURN ALL YRC GROUPS THAT THIS PROJECT IS INVOLVED WITH
# SEARCHS tblCollaboration, tblTechnology, tblTraining and tblDissemination
sub queryGroups {
   my($self, $group) = @_;
   my(%GROUPS, @GROUPS);
   my($sql, $sth, @arr);

   if(!$self->{DBH}) {
      $self->{ERROR} = "Call to queryGroups with no database handle set.";
      return 0;
   }

   if(!$self->{_ID}) {
      $self->{ERROR} = "Call to queryGroups with no ID set.";
      return 0;
   }

   if(!$group || $group eq 'T') {
      $sql = "SELECT trainingGroups FROM tblTraining WHERE projectID = " . $self->{_ID};
      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;

      if($sth->rows) {
         @arr = $sth->fetchrow_array;

         @GROUPS = split(',', $arr[0]);
         foreach $_ (@GROUPS) {
            $GROUPS{$_} = 1;
         }
      }
   }

   if(!$group || $group eq 'C') {
      $sql = "SELECT collGroups FROM tblCollaboration WHERE projectID = " . $self->{_ID};
      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;

      if($sth->rows) {
         @arr = $sth->fetchrow_array;
         @GROUPS = split(',', $arr[0]);
         foreach $_ (@GROUPS) {
            $GROUPS{$_} = 1;
         }
      }
   }

   if(!$group || $group eq 'Tech') {
      $sql = "SELECT techGroups FROM tblTechnology WHERE projectID = " . $self->{_ID};
      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;

      if($sth->rows) {
         @arr = $sth->fetchrow_array;
         @GROUPS = split(',', $arr[0]);
         foreach $_ (@GROUPS) {
            $GROUPS{$_} = 1;
         }
      }
   }

   if(!$group || $group eq 'D') {
      $sql = "SELECT shipPlasmidType FROM tblDissemination WHERE projectID = " . $self->{_ID};
      $sth = $self->{DBH}->prepare($sql);
      $sth->execute;

      if($sth->rows) {
         @arr = $sth->fetchrow_array;
         if($arr[0] eq 'T') { $GROUPS{TwoHybrid} = 1; }
         elsif($arr[0] eq 'M') { $GROUPS{Microscopy} = 1; }
         elsif($arr[0] eq 'MT') {
            $GROUPS{TwoHybrid} = 1;
            $GROUPS{Microscopy} = 1;
         }
      }
   }

   return keys(%GROUPS);

}

# GET THE FULL STRING FOR GROUP ABBREVIATIONS
sub getGroupString {
   my($self, @groups) = @_;

   my($groups) = join(",", @groups);

   $groups =~ s/,/, /g;
   $groups =~ s/IntMetab/Intermediary Metabolism/;
   $groups =~ s/LipidMetab/Lipid Metabolism/;
   $groups =~ s/PKPD/P.Dynamics and P.Kinetics/;
   $groups =~ s/EnvTox/Environmental Toxicology/;
   $groups =~ s/CBNet/Celular B Networks/;
   $groups =~ s/PMImageD/Parametric Modelling of Image Data/;
   $groups =~ s/SysMod/Systematic Models/;
   $groups =~ s/StatMod/Statistical Models/;
   $groups =~ s/SoftDev/Software Devevelopment/;

   return $groups;
}

# CONVERT PROJECT TYPE TO A MORE DETAILED STRING DESCRIPTION
sub getTypeString {
   my($self, $type) = @_;

   $type =~ s/,/, /g;
   $type =~ s/Tech/technology development/;
   $type =~ s/C/Collaboration/;
   $type =~ s/T/Training/;
   $type =~ s/D/Dissemination/;
   $type =~ s/technology development/Technology Development/;

   return $type;
}

# CONVERT PLASMID TYPE TO A MORE DETAILED STRING DESCRIPTION
sub getPlasmidString {
   my($self, $type) = @_;

   if($type eq 'M') { return "Microscopy"; }
   if($type eq 'T') { return "Two Hybrid"; }
   if($type eq 'MT') { return "Microscopy and Two Hybrid"; }

   return "Unknown";
}

# STRIP \r and \n out of text and replace with <BR>
sub toHTML {
   my($self, $output) = @_;

   # STRIP OUT A LEADING AND TRAILING QUOTATION MARK
   if(substr($output, 0, 1) eq "\"") { substr($output, 0, 1, ""); }
   if(substr($output, -1, 1) eq "\"") { substr($output, -1, 1, ""); }

#   $output =~ s/\\n/\n/g;
   $output =~ s/\\n//g;
   $output =~ s/\\r/\r/g;
   $output =~ s/\\t/\t/g;

   chomp($output);

#   $output =~ s/\n/<BR>/g;
   $output =~ s/\r/<BR>/g;

   return $output;
}

1;
