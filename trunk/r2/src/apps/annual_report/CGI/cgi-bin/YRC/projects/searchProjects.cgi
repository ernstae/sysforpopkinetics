#!/usr/bin/perl -w

use strict;
use CGI qw(:standard escape);

use lib "/usr/local/lib/site_perl";
use YRC::DB;
use YRC::WWW::SESSION;
use YRC::WWW::LOGIN;
use YRC::WWW::HISTORY;
use YRC::WWW::ERROR;
use YRC::WWW::TEMPLATES;
use YRC::PROJECT::PROJECT;

my($query) = new CGI;
my($USER);
my($TITLE) = 'Search Projects Results';

# FORCE IT TO BE SECURE
#YRC::WWW::SESSION->enforceSecure($query);

# OUR ENTIRE AUTHENTICATION CODE
unless($USER = YRC::WWW::SESSION->getUserObject(YRC::WWW::SESSION->getSessionID($query))) {
   YRC::WWW::LOGIN->showLogin(QUERY=>$query, REQUESTER=>$ENV{REQUEST_URI});
   exit 0;
}

# PRINT OUR HEADER
print YRC::WWW::TEMPLATES->getHeader($TITLE,1,$USER);

my($HISTORY) = YRC::WWW::HISTORY->new( (SESSIONID=>$USER->query('SESSIONID')) ); 
$HISTORY->setHistory( ( TITLE=>$TITLE, LINK=>$query->url(-full=>1,-query=>1), TRUNC=>$query->param('trunc') ) );
print $HISTORY->getHistoryNav();

# DO OUR SEARCH HERE
my($search_term) = $query->param('query');
if(!$search_term) { $search_term = '*'; }
$search_term =~ s/\*/\%/g;
$search_term = lc($search_term);

my($C) = $query->param('C');
my($D) = $query->param('D');
my($T) = $query->param('T');
my($Tech) = $query->param('Tech');
my(@groups) = $query->param('group');
my($orderBy) = $query->param('orderBy');
if(!$orderBy) { $orderBy = 'projectID'; }
my($orderDir) = $query->param('orderDir');
if(!$orderDir) { $orderDir = 'ASC'; }

my(@terms) = split(' ', $search_term);

my(@fields) = qw(P.projectTitle P.projectAbstract P.projectKeywords P.projectComments R.researcherFirstname R.researcherLastname);

my($sql) =  "SELECT P.projectID, R.researcherFirstname, R.researcherLastname FROM tblProjects AS P ";
$sql .= "LEFT OUTER JOIN tblResearchers AS R ON P.projectPI = R.researcherID WHERE (";
my($i) = 0;
my($j) = 0;
my($column);
my($PROJECT, $sth, $dbh, @arr);

foreach $column (@fields) {
   if($i) { $sql .= " OR "; }

   $j = 0;
   foreach (@terms) {
      if($j) { $sql .= " OR "; }

      if(isQuoted($_)) {
         # IS IT QUOTED?  SEARCH FOR THE INDIVIDUAL WORD
         $sql .= "LOWER($column) REGEXP LOWER(\"(^|[^a-zA-Z0-9])" . deQuote($_) . "([^a-zA-Z0-9]|\$)\")";

      } else {
         # NOT QUOTED... FIND ALL OCCURANCES
         $sql .=  "LOWER($column) LIKE LOWER('%" . $_ . "%')";

      }

      $j++;
   }   
   $i++;
}
$sql .= ")";

# LIMIT THE SEARCH BY SELECTED PROJECT TYPE HERE
if($C || $T || $D || $Tech) {
   $sql .= " AND (";
   if($C) { $sql .= "FIND_IN_SET('C',projectType)"; }
   if($T) {
      if($C) { $sql .= " OR "; }
      $sql .= "FIND_IN_SET('T',projectType)";
   }
   if($D) {
      if($C || $T) { $sql .= " OR "; }
      $sql .= "FIND_IN_SET('D',projectType)";
   }
   if($Tech) {
      if($C || $T|| $D) { $sql .= " OR "; }
      $sql .= "FIND_IN_SET('Tech',projectType)";
   }
   $sql .= ")";
}
$sql .= "ORDER BY $orderBy $orderDir";

#print $sql;
#exit 0;

$dbh = YRC::DB->getDBH();
$sth = $dbh->prepare($sql);
$sth->execute;

my($matchCount) = $sth->rows;

print <<END;

<CENTER>

<DIV CLASS="main_header" STYLE="width: 700;">
<CENTER>SEARCH RESULTS:<BR>
<FONT STYLE="font-size: 10pt;font-weight: 500;">
END
print "Searching for: <B>\"" . $query->param('query') . "\"</B>\n";
#print "Number of matches: " . $sth->rows;
if(@groups) { print "<BR>Filtering results for: " . join(", ", @groups); }
print <<END;
</FONT>
</CENTER>
</DIV>
<DIV CLASS="main" STYLE="width: 700;">
END


#print "SQL: $sql <BR><BR>\n";

my($url_query) = "query=" . escape($search_term);
$url_query .= "&C=$C&T=$T&D=$D";
foreach(@groups) {
   $url_query .= "&group=$_";
}


# TOGGLE OUR SORTING DIRECTION FOR URL SUBMISSION
if($orderDir eq 'ASC') { $orderDir = 'DESC'; }
else { $orderDir = 'ASC'; }

print <<END;
<TABLE WIDTH="100%" CELLPADDING="no" CELLSPACING="0">
 <TR>
  <TD WIDTH="5%"><FONT STYLE="font-size:10pt;"><B><A HREF="searchProjects.cgi?$url_query&orderBy=P.projectID&orderDir=$orderDir">ID</A></B></FONT></TD>
  <TD WIDTH="5%"><FONT STYLE="font-size:10pt;"><B><A HREF="searchProjects.cgi?$url_query&orderBy=P.projectType&orderDir=$orderDir">Type</A></B></FONT></TD>
  <TD WIDTH="10%"><FONT STYLE="font-size:10pt;"><B><U>Groups</U></B></FONT></TD>
  <TD WIDTH="23%"><FONT STYLE="font-size:10pt;"><B><A HREF="searchProjects.cgi?$url_query&orderBy=R.researcherLastName&orderDir=$orderDir">PI</A></B></FONT></TD>
  <TD WIDTH="57%"><FONT STYLE="font-size:10pt;"><B><A HREF="searchProjects.cgi?$url_query&orderBy=P.projectTitle&orderDir=$orderDir">Title</A></B></FONT></TD>
 </TR>
END
my($counter) = 0;
my($found) = 0;
while(@arr = $sth->fetchrow_array) {
   $PROJECT = YRC::PROJECT::PROJECT->new();
   $PROJECT->setDBH($dbh);
   $PROJECT->load($arr[0]);

   # THEY LIMITED THEIR SEARCH TO SPECIFIC GROUPS
   # FILTER IT HERE
   $found = 0;
   if(@groups) {
      foreach (@groups) {
         if($PROJECT->queryGroup($_)) {
            # THIS PROJECT IS A MEMBER OF THE SUPPLIED GROUP(s)
            $found = 1;
            last;
         }
      }
      # THIS PROJECT ISN'T A MEMBER OF THE SUPPLIED GROUP(S), DON'T DISPLAY IT
      if(!$found) { $matchCount--; next; }
   }

   print " <TR";
   if((++$counter) % 2) { print " BGCOLOR=\"#f5f5f5\""; }
   print ">\n";
   print "  <TD VALIGN=\"top\"><FONT STYLE=\"font-size:10pt;\"><A HREF=\"viewProject.cgi?ID=" . $PROJECT->query('ID') . "\">" . $PROJECT->query('ID') . "</A>&nbsp;</FONT></TD>\n";
   print "  <TD VALIGN=\"top\"><FONT STYLE=\"font-size:10pt;\">" . $PROJECT->query('TYPE') . "&nbsp;</FONT></TD>\n";
   print "  <TD VALIGN=\"top\"><FONT STYLE=\"font-size:10pt;\">" . join(",<BR>", sort($PROJECT->queryGroups())) . "</FONT></TD>\n";
   if($PROJECT->query('PI')) {
      print "  <TD VALIGN=\"top\"><FONT STYLE=\"font-size:10pt;\"><A HREF=\"viewResearcher.cgi?ID=" . $PROJECT->query('PI') . "\">" . $arr[2] . ", " . $arr[1] . "</A>&nbsp;</FONT></TD>\n";
   } else {
      print "  <TD VALIGN=\"top\"><FONT STYLE=\"font-size:10pt;\"><B>No PI Entered</B></FONT></TD>\n";
   }
   print "  <TD VALIGN=\"top\"><FONT STYLE=\"font-size:10pt;\">" . $PROJECT->query('TITLE') . "&nbsp;</FONT></TD>\n";
   print " </TR>\n";

}

print " <TR";
if((++$counter) % 2) { print " BGCOLOR=\"#f5f5f5\""; }
print ">\n";
print "<TD COLSPAN=\"5\" ALIGN=\"CENTER\"><FONT STYLE=\"font-size:10;\"><P><B>Total Matches: $matchCount</B></FONT></TD>\n";
print "</TR>\n";

print "</TABLE>\n";


print <<END;


</DIV>


</CENTER>
END



# PRINT OUR FOOTER
print YRC::WWW::TEMPLATES->getFooter();

# RETURNS 1 IF A STRING IS QUOTED e.g.: "STRING"
# RETURNS 0 IF IT'S NOT SURROUNDED BY QUOTES
sub isQuoted {
   my($str) = @_;

   if( (substr($str, 0, 1) eq "\"") && (substr($str, -1, 1) eq "\"") ) { return 1; }
   return 0;
}

# STRIPS QUOTE OFF OF A STRING, IF QUOTED
# RETURNS THE STRING SANS QUOTES
sub deQuote {
   my($str) = @_;

   if(!isQuoted($str)) { return $str; }

   # SNIP OUT THE QUOTES
   substr($str, 0, 1, "");
   substr($str, -1, 1, "");

   return $str;
}

exit 0;
