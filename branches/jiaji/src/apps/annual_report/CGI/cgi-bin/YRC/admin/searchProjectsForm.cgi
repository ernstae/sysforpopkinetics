#!/usr/bin/perl

use CGI;

$query = new CGI;

print $query->redirect('/cgi-bin/YRC/projects/searchProjectsForm.cgi');

exit 0;
