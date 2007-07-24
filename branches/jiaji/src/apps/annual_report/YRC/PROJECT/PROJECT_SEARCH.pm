################################################
#
# Michael Riffle <mriffle@u.washington.edu>
# 3/19/2002
#
################################################

package YRC::PROJECT::PROJECT_SEARCH;
use strict;

use lib "/usr/local/lib/site_perl";
use YRC::PROJECT::PROJECT;
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

#   $self->{GROUPS} = qw(Yates Aebersold TwoHybrid Microscopy PSP Informatics);

   return 1;
}

# sets up a reference to a database handle for
# use by this object
sub setDBH {
   my ($self, $dbh) = @_;
   ($dbh) ? ($self->{DBH} = $dbh) : ($self->{DBH} = YRC::DB->getDBH());
}

# TAKES A HASH KEY/VALUE SET AS A PARAMETER  THE POSSIBLE KEYS ARE:
# GROUP     -> REFERENCE TO AN ARRAY OF GROUP NAMES
# TYPE      -> REFERENCE TO AN ARRAY OF PROJECT TYPES
# ASOBJECTS -> IF PRESENT, AND SET TO NON-ZERO, WILL RETURN AN ARRAY OF PROJECT OBJECTS
#              OTHERWISE, RETURNS AN ARRAY OF PROJECT IDS (INTS)
sub queryProjects {
   my($self, %MARGS) = @_;
   my(@returnArr) = ( );
   my($GROUP, $counter);

   # NO GROUP OR TYPE SUPPLIED
   if(!$MARGS{TYPE} && !$MARGS{GROUP}) { return 0; }

   # DO THE SEARCH

   my($sql) = "";
   my($sth, @arr);

   my(@TYPES) = (
			['C', 'tblCollaboration', 'collGroups'],
			['T', 'tblTraining', 'trainingGroups'],
			['Tech', 'tblTechnology', 'techGroups'],
   );

   foreach (@TYPES) {
      if(!$MARGS{TYPE} || $self->searchArray($MARGS{TYPE}, $_->[0])) {
         $sql  = "SELECT projectID FROM " . $_->[1];

         $counter = 0;
         foreach $GROUP (@{$MARGS{GROUP}}) {
            if($counter) { $sql .= " OR"; }
            else { $sql .= " WHERE"; }
            $sql .= " FIND_IN_SET('" . $GROUP . "', " . $_->[2] . ")";
            $counter++;
         }

         print $sql;

         if(!$self->{DBH}) { $self->setDBH(); }
         $sth = $self->{DBH}->prepare($sql);
         $sth->execute();

         while(@arr = $sth->fetchrow_array()) {

            # MAKE SURE IT'S A VALID PROJECT ID
            if(!$self->validateProject($arr[0])) { next; }

            if($MARGS{ASOBJECTS}) {
               my($PROJECT) = YRC::PROJECT::PROJECT->new();
               $PROJECT->load($arr[0]);
               push(@returnArr, ($PROJECT));
            } else {
               push(@returnArr, ($arr[0]));
            }
         }
      }
   }

   # RETURN A SORTED LIST OF PROJECT IDS
   if(!$MARGS{ASOBJECTS}) { return sort { $a <=> $b } @returnArr; }

   # RETURN A SORTED LIST OF PROJECT OBJECTS, BASED ON PROJECT ID
   return sort { $a->query('ID') <=> $b->query('ID') } @returnArr;
}


sub validateProject {
   my($self, $ID) = @_;
   my($sql, $sth);

   if(!$ID) { return 0; }

   $sql = "SELECT projectID FROM tblProjects WHERE projectID = $ID";
   if(!$self->{DBH}) { $self->setDBH(); }
   $sth = $self->{DBH}->prepare($sql);
   $sth->execute();

   return $sth->rows();
}

sub searchArray {
   my($self, $arr, $item) = @_;

   foreach (@{$arr}) {
      if($_ eq $item) { return 1; }
   }
   return 0;
}


1;
