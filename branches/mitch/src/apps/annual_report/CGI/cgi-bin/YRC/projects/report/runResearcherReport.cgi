#!/usr/bin/perl -w

my($time1) = time;

use strict;
use lib "/usr/local/lib/site_perl";

use RTF::Writer;
use YRC::DB;
use YRC::PROJECT::PROJECT;
use YRC::PROJECT::RESEARCHER;

my($CUTOFF) = '2003-06-11';
my(%FOUND_RESEARCHERS) = ( );
my($HOST_NAME) = 'University of Washington';

my($TYPE);
my(@TYPES) = (
		{
			'CODES'	=>	['Tech'],
			'DESC'	=>	'(T) Technological Research & Development',
		},
		{
			'CODES'	=>	['C'],
			'DESC'	=>	'(C) Collaborative Research & Services',
		},
		{
			'CODES'	=>	['T','D',],
			'DESC'	=>	'(D) Dissemination & Training',
		},
);


my(@COLUMNS) = (
		['LAST NAME', 'LASTNAME'],
		['FIRST NAME', 'FIRSTNAME'],
		['NON-HOST INSTITUTION', 'ORGANIZATION'],
		['TYPE', 'FUNDINGTYPES'],
		['SOURCE', 'FUNDINGFEDERAL'],
);

my(@ROWS);
my(@PROJECTS);

my($output) = '';
my($rtf) = RTF::Writer->new_to_string(\$output);

$rtf->prolog( 
	'title'		=>	"SOURCES OF INVESTIGATOR SUPPORT",
	'fonts'		=>	[ "Arial" ],
	'author'	=>	"Perl Script by Michael Riffle additions by M. Macaulay"
);


$rtf->number_pages;

my($sql, $sth, $dbh, @arr, $hashref);
my($PROJECT, $RESEARCHER);
$dbh = YRC::DB->getDBH();

my($decl);
my($ROW);

my($ID);
my($group);

# THE COLUMN DEFINITIONS FOR OUR TABLES
$decl = RTF::Writer::TableRowDecl->new('widths' => [1600, 1600, 3300, 1600, 1300]);


my($foundType, $tmpType);
my(%fundingTypes, %fundingSources);

# PRODUCE A MASTER ORDERED LIST OF RESEARCHER OBJECTS
my(@myData);
my(@MASTER_LIST) = ( );
$sql =  "SELECT researcherID, researcherLastName FROM tblResearchers ORDER BY researcherLastName";
$sth = $dbh->prepare($sql);
$sth->execute;
while(@myData = $sth->fetchrow_array()) {
   $RESEARCHER = YRC::PROJECT::RESEARCHER->new();
   $RESEARCHER->setDBH($dbh);
   $RESEARCHER->load($myData[0]);

   push(@MASTER_LIST, ($RESEARCHER));
}

# CYCLE THROUGH EACH TYPE
foreach $TYPE (@TYPES) {

   # PRINT THE TABLE HEADER
   $rtf->table($decl, [ [\'\fs20\b', "LAST NAME"],
                        [\'\fs20\b', "FIRST NAME"],
                        [\'\fs20\b', "NON-HOST INSTITUTION"],
                        [\'\fs20\b', "TYPE"],
                        [\'\fs20\b', "SOURCE"]
                      ]);

   # RE-INITIALIZE OUR ROWS FOR THIS TABLE
   @ROWS = ( );

   # CYCLE THROUGH OUR LIST OF RESEARCHERS
   foreach $RESEARCHER (@MASTER_LIST) {

      # VIEW PROJECTS FOR THIS RESEARCHER
      $foundType = 0;
      %fundingTypes = ( );
      %fundingSources = ( );

      foreach $ID ($RESEARCHER->queryProjects()) {
         $PROJECT = YRC::PROJECT::PROJECT->new();
         $PROJECT->setDBH($dbh);
         $PROJECT->load($ID);

         # ADD FUNDING TYPES TO THE HASH
         if($PROJECT->query('FUNDINGTYPES')) {
            foreach (split(",", $PROJECT->query('FUNDINGTYPES'))) {
               $fundingTypes{$_} = 1;
            }
         }

         # ADD FUNDING SOURCES TO THE HASH
         if($PROJECT->query('FUNDINGFEDERAL')) {
            foreach (split(",", $PROJECT->query('FUNDINGFEDERAL'))) {
               $fundingSources{$_} = 1;
            }
         }

         # DETERMINE IF THIS PROJECT IS OF THE CORRECT TYPE
         if($PROJECT->query('TYPE')) {
            foreach (split(",", $PROJECT->query('TYPE'))) {
               foreach $tmpType (@{$TYPE->{CODES}}) {
                  if($_ eq $tmpType) { $foundType = 1; }
               }
            }
         }
      }


      # CHECK TO SEE IF THIS RESEARCHER SHOULD BE LISTED IN THIS SECTION
      # IF NOT, MOVE ON TO THE NEXT RESEARCHER
      if(!$foundType) { next; }

      $ROW = [ ];

      push(@{$ROW}, ( [\'\fs20', $RESEARCHER->query('LASTNAME')] ));
      push(@{$ROW}, ( [\'\fs20', $RESEARCHER->query('FIRSTNAME')] ));

      if(deQuote($RESEARCHER->query('ORGANIZATION')) eq $HOST_NAME) {
         push(@{$ROW}, (""));
      } else {
         push(@{$ROW}, ( [\'\fs20', deQuote($RESEARCHER->query('ORGANIZATION'))] ));
      }
      push(@{$ROW}, ( [\'\fs20', join(", ", ( sort keys( %fundingTypes ) ) )] ) );
      push(@{$ROW}, ( [\'\fs20', join(", ", (sort keys(%fundingSources ) ) )] ) );

      # ADD THIS ROW INTO OUR LIST OF ROWS
      push(@ROWS, ($ROW));

   }

   # PRINT OUR TABLE
   $rtf->table($decl, @ROWS);

   # PRINT A NEW PAGE
   $rtf->print(\'\page');

}

$rtf->close;

$output = "Content-type: text/rtf\n\n$output";

print $output;


# STRIPS QUOTE OFF OF A STRING, IF QUOTED
# RETURNS THE STRING SANS QUOTES
sub deQuote {
   my($str) = @_;
       
   # SNIP OUT THE QUOTES

   while(substr($str, 0, 1) eq "\"") {  substr($str, 0, 1, ""); }
   while(substr($str, -1, 1) eq "\"") { substr($str, -1, 1, ""); }
   
   return $str;
}


exit 0;
