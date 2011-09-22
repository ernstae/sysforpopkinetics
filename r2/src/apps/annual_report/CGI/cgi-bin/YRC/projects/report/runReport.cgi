#!/usr/bin/perl -w

use strict;
use lib "/usr/local/lib/site_perl";

use RTF::Writer;
use YRC::PROJECT::REPORT::SECTION;

my($output) = '';
my($rtf) = RTF::Writer->new_to_string(\$output);

$rtf->prolog( 
	'title'		=>	"NIH RFPK REPORT",
	'fonts'		=>	[ "Times" ],
	'author'	=>	"Perl Script by Michael Rifflei, Mods by M. Macaulay"
);


$rtf->number_pages;

# PRODUCE OUR SECTIONS
YRC::PROJECT::REPORT::SECTION->makeSection($rtf, 'Tech');		# TECHNOLOGY SECTION
YRC::PROJECT::REPORT::SECTION->makeSection($rtf, 'C');			# COLLABORATION SECTION
YRC::PROJECT::REPORT::SECTION->makeSection($rtf, 'T');			# TRAINING AND DISSEMINATION SECTION

$rtf->close;

$output = "Content-type: text/rtf\n\n$output";

print $output;

exit 0;
