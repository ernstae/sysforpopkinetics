################################################################################
################################################################################
##
## TEMPLATES.pm
## Purpose: To provide functions for accessing HTML templates for the site, such
## as headers and footers.
##
## Author:      Michael Riffle <mriffle@u.washington.edu>
## Date:        April 4, 2002
##
################################################################################
################################################################################

package YRC::WWW::TEMPLATES;
use strict;

my($TMPL_DIR) = "/var/www/html/v2/tmpl/";
my($TMPL_URL) = "/v2/";
my($IMG_URL) = $TMPL_URL . 'images/';
my($CGI_URL) = '/cgi-bin/YRC/';

my(%SUBNAV_INDEX) = (
   projects	=>	'PROJECT',
   admin	=>	'ADMIN',
);

my(%SUBNAV) = (
   PROJECT=>[
	{
		IMAGE	=>	'tab-bottom-projects-search.gif',
		URL	=>	$CGI_URL . 'projects/searchProjectsForm.cgi',
		ALT	=>	'Search Project Data',
	},
	{
		IMAGE	=>	'tab-bottom-projects-new.gif',
		URL	=>	$CGI_URL . 'projects/newProject.cgi',
		ALT	=>	'Enter a New Project',
	},
	{
		IMAGE	=>	'tab-bottom-projects-new-researcher.gif',
		URL	=>	$CGI_URL . 'projects/newResearcher.cgi',
		ALT	=>	'Enter a New Researcher',
	},
	{
		IMAGE	=>	'tab-bottom-data-run_report.gif',
		URL	=>	$CGI_URL . 'projects/report/runReport.cgi',
		ALT	=>	'Run NIH Report',
		GROUPS	=>	[1,],
	},
	{
		IMAGE	=>	'tab-bottom-projects-custom_tdr.gif',
		URL	=>	$CGI_URL . 'projects/createReportForm.cgi',
		ALT	=>	'Create Custom Tab-Delimited Report',
		GROUPS	=>	[1,],
	},
    ],
    ADMIN=>[
	{
		IMAGE	=>	'tab-bottom-account-new_password.gif',
		URL	=>	$CGI_URL . 'admin/changePassword.cgi',
		ALT	=>	'Change Your Password',
	},
    ],
);

sub getSimpleHeader {
   my $returnStr;
   $returnStr = "Content-type: text/html\n\n";
   open (FILE, "<".$TMPL_DIR."simpleheader.html") || die("Unable to open ".$TMPL_DIR."header.html.\n");
   foreach (<FILE>) {
      $returnStr .= $_;
   }
   close (FILE);
   return $returnStr;
}

sub getHeader {
   my($self, $title, $showHeader, $USER) = @_;
   my($output) = "";
   my($username, $fullname, $groups);
   my($bottombar);

   if($showHeader) { $output .= "Content-type: text/html\n\n"; }
   $bottombar = getBottomBar($USER);

   open (FILE, "<".$TMPL_DIR."header.html") || die("Unable to open ".$TMPL_DIR."header.html.\n");
   foreach (<FILE>) {
      $_ =~ s/#TITLE#/$title/;
#      $_ =~ s/#TOPBAR#/getTopBar()/;
      $_ =~ s/#BOTTOMBAR#/$bottombar/;

      if($USER) {
         $username = $USER->query('USERNAME');
         $fullname = $USER->query('FIRSTNAME') . " " . $USER->query('LASTNAME');

         $groups = join(",", map { $USER->queryGroupName($_) } $USER->queryAccessGroups());

         $_ =~ s/#USERNAME#/$username/;
         $_ =~ s/#FULLNAME#/$fullname/;
         $_ =~ s/#GROUPS#/$groups/;
      }
      $output .= $_;
   }
   close (FILE);
   return $output;
}

sub getFooter {
   my($self, %args) = @_;
   my($output) = "";

   open (FILE, "<".$TMPL_DIR."footer.html") || die("Unable to open ".$TMPL_DIR."footer.html.\n");
   foreach (<FILE>) {
      $output .= $_;
   }
   close (FILE);
   return $output;
}


sub getBottomBar {
   my($USER) = @_;
   my($URI) = $ENV{REQUEST_URI};
   my($subdir, $returnstr);
   my($groupID, $access);

   $returnstr = '';

   if($URI =~ /^\/cgi-bin\/YRC\/([A-Za-z0-9]+)\/[.]*/) {
      $subdir = $1;
   } else { return "No dir found..."; }

   if(!$SUBNAV_INDEX{$subdir}) { return ''; }

   foreach (@{$SUBNAV{$SUBNAV_INDEX{$subdir}}}) {
      $access = 1;

      # RESTRICT DISPLAY OF THIS TAB TO THE DESIGNATED GROUPS
      if($_->{GROUPS} && !$USER->queryAccessGroupMembership(1)) {
         $access = 0;
         foreach $groupID (@{$_->{GROUPS}}) { if($USER->queryAccessGroupMembership($groupID)) { $access = 1; last; } }
      }

      if($access) {
         $returnstr .= "<A HREF=\"" . $_->{URL} . "\">";
         $returnstr .= "<IMG SRC=\"" . $IMG_URL . $_->{IMAGE} . "\" ALT=\"" . $_->{ALT} . "\" WIDTH=\"100\" HEIGHT=\"15\" BORDER=\"0\"></A>";
      }
   }

   return $returnstr;

}

1;
