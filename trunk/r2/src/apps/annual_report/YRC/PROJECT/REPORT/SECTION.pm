package YRC::PROJECT::REPORT::SECTION;
use strict;

use lib "/usr/local/lib/site_perl";

use YRC::DB;
use YRC::PROJECT::PROJECT;
use YRC::PROJECT::RESEARCHER;
use YRC::PROJECT::REPORT::BTA;

use RTF::Writer;

## IMPORT OUR SETTINGS FOR THIS REPORT
## SETS UP SOME GLOBAL VARIABLES HOLDING OUR SETTINGS
use YRC::PROJECT::REPORT::DEFS qw(:DEFAULT);

sub makeSection {
   my($self, $rtf, $sectionType) = @_;
   my($LAB, $projectIDRef, $counter, $group);
   my(%done) = ();

   ## SET UP DBH AND DB VARIABLES
   my($sql, $sth, $dbh);   
   $dbh = YRC::DB->getDBH();

   ## SET UP OBJECTS
   my($PROJECT, $RESEARCHER);

   ## SET UP THE COLUMN SIZES FOR OUR TABLES
   my($decl) = RTF::Writer::TableRowDecl->new('widths' => [2880,5760]);
   my($row1, $row2, $row3, $row4);

   $sql = "SELECT projectID FROM tblProjects WHERE FIND_IN_SET('$sectionType',projectType) AND(projectSubmitDate >= '$STARTDATE' AND projectSubmitDate <= '$ENDDATE') ORDER BY projectSubmitDate";
   $sth = $dbh->prepare($sql);
   $sth->execute;
   my($PROJECTS) = $sth->fetchall_arrayref;

   ## HASH TO KEEP TRACK OF WHICH PROJECTS WE'VE ALREADY LISTED
   my(%FOUND_PROJECTS) = ( );

   # CYCLE THROUGH EACH LAB, IN ORDER
   foreach $LAB (@LABS) {

      %done = ();
      foreach $projectIDRef (@{$PROJECTS}) {

         $counter = 1;
         %done = ( );

         $PROJECT = YRC::PROJECT::PROJECT->new();
         $PROJECT->setDBH($dbh);
         $PROJECT->load($projectIDRef->[0]);

         # FILTER WHICH PROJECTS ARE SHOWN HERE
         if($FOUND_PROJECTS{$projectIDRef->[0]}) { next; }	# SKIP THIS IF WE'VE ALREADY DONE IT IN THIS SECTION
         if(!$PROJECT->queryGroup($LAB)) { next; }		# SKIP THIS PROJECT IF IT'S THE WRONG LAB
         $FOUND_PROJECTS{$projectIDRef->[0]} = 1;		# MARK THIS AS BEING DONE IN THIS SECTION

         # BTA HEADER
         $rtf->table($decl, [ "BTA UNIT:", YRC::PROJECT::REPORT::BTA->getBTAString($sectionType) ]);

         # TITLE
         $rtf->table($decl, [ "TITLE:", deQuote($PROJECT->query('TITLE')) ]);

         # KEYWORDS
         $rtf->table($decl, [ "KEYWORDS:", deQuote(strip($PROJECT->query('KEYWORDS'))) ]);

         # AXIS I
         if($PROJECT->query('AXISI')) {
            $rtf->table($decl, [ "AXIS I:", $PROJECT->query('AXISI') ]);
         } else {
            $rtf->table($decl, [ "AXIS I:", "INSERT AXIS I HERE" ]);
         }

         # AXIS II
         if($PROJECT->query('AXISII')) {
            $rtf->table($decl, [ "AXIS II:", $PROJECT->query('AXISII') ]);
         } else {
            $rtf->table($decl, [ "AXIS II:", "INSERT AXIS II HERE" ]);
         }

         # PUT IN YRC GROUP MEMBERS
         foreach $group (@LABS) {
            if($PROJECT->queryGroup($group)) {
               foreach (@{$MEMBERS{$group}}) {
                  $row1 = [ "INVEST$counter:", toUpper($_->[0]) ];
                  $row2 = [ "DEGREE$counter:", toUpper($_->[1]) ];
                  $row3 = [ "DEPT$counter:", toUpper($_->[2]) ];
                  if($_->[3] eq $HOST_NAME) {
                     $row4 = [ "NONHOST$counter:", "" ];
                  } else {
                     $row4 = [ "NONHOST$counter:", toUpper($_->[3] . "\n" . $_->[4] . ", " . $_->[5]) ];
                  }
                  $rtf->table($decl, $row1, $row2, $row3, $row4);
                  $done{$_->[0]} = 1;
                  $counter++;
               }
            }
         }


         # PUT IN COLLABORATORS
         my(@RESEARCHERS) = ('PI', 'RESEARCHERB', 'RESEARCHERC', 'RESEARCHERD');
         foreach (@RESEARCHERS) {
            if($PROJECT->query($_)) {
               $RESEARCHER = YRC::PROJECT::RESEARCHER->new();
               $RESEARCHER->setDBH($dbh);
               $RESEARCHER->load($PROJECT->query($_));

               if(!$done{$RESEARCHER->query('LASTNAME') . ", " . $RESEARCHER->query('FIRSTNAME')}) {
                  # ONLY DISPLAY THEM IF THEY HAVEN'T ALREADY BEEN DISPLAYED FOR THIS PROJECT
                  $row1 = [ "INVEST$counter:", deQuote(toUpper($RESEARCHER->query('LASTNAME')) . ", " . toUpper($RESEARCHER->query('FIRSTNAME'))) ];
                  $row2 = [ "DEGREE$counter:", deQuote(toUpper($RESEARCHER->query('DEGREE'))) ];
                  $row3 = [ "DEPT$counter:", deQuote(toUpper($RESEARCHER->query('DEPT'))) ];
                  if($RESEARCHER->query('ORGANIZATION') eq $HOST_NAME) {
                     $row4 = [ "NONHOST$counter:", "" ];
                  } else {
                     if($RESEARCHER->query('STATE') && $RESEARCHER->query('STATE') ne 'No') {
                        $row4 = [ "NONHOST$counter:", deQuote(toUpper($RESEARCHER->query('ORGANIZATION'))) . "\n" . deQuote(toUpper($RESEARCHER->query('STATE'))) ];
                     } else {
                        $row4 = [ "NONHOST$counter:", deQuote(toUpper($RESEARCHER->query('ORGANIZATION'))) . "\n" . deQuote(toUpper($RESEARCHER->query('COUNTRY'))) ];
                     }
                  }

                  $rtf->table($decl, $row1, $row2, $row3, $row4);
                  $counter++;
               }
            }
         }

         if($PROJECT->query('BTA')) {
            $row1 = [ "% BTA \$:", $PROJECT->query('BTA') . "%" ];
         } else {
            $row1 = [ "% BTA \$:", "INSERT BTA NUMBER" ];
         }

         $row2 = [ "% AIDS \$:", "0%" ];
         $rtf->table($decl, $row1, $row2);

         $rtf->print("\n");
         $rtf->print("ABSTRACT\n" . deQuote(strip($PROJECT->query('ABSTRACT'))));

         if($PROJECT->query('PROGRESS')) {
            $rtf->print("\n\n");
            $rtf->print( deQuote( strip( $PROJECT->query( 'PROGRESS' ) ) ) );
         }

         if($sectionType eq 'T') {
            if($PROJECT->queryTraining('DESCRIPTION')) {
               if($PROJECT->query('ABSTRACT') || $PROJECT->query('PROGRESS')) {
                  $rtf->print("\n\n");
               }
               $rtf->print( deQuote( strip( $PROJECT->queryTraining( 'DESCRIPTION' ) ) ) );
            } 
         }

         $rtf->print(\'\page');
      }
   }

   return $rtf;

}


# STRIPS QUOTE OFF OF A STRING, IF QUOTED
# RETURNS THE STRING SANS QUOTES
sub deQuote {
   my($str) = @_;

   # SNIP OUT THE QUOTES

   while(substr($str, 0, 1) eq "\"") {  substr($str, 0, 1, ""); }
   while(substr($str, -1, 1) eq "\"") { substr($str, -1, 1, ""); }

   return $str;
}

sub strip {  
   my($str) = @_;
   $str =~ s/\\n/\n/g;
   $str =~ s/\\r//g;
   $str =~ s/\\t/\t/g;

   return $str;
}

sub toUpper {   
   my($str) = @_;
   $str =~ tr/[a-z]/[A-Z]/;
   return $str;  
}


1;
