#! /usr/bin/perl

use strict;
use warnings;
use convert;
use Cwd;

my $dir = $ARGV[0];

opendir( D, $dir ) or die "no such a directory - $dir -.";
our $home = cwd;
chdir( $dir );
my @allfiles = readdir D;
closedir D;
my $searchstring = "<relTol>0.002</relTol>";
my $replacestring= "<relTol>0.01</relTol>";

foreach my $file (@allfiles) {
   if( $file =~ /\.in\b/ ) {
        my $tempfilename = $file;
        $tempfilename =~ s/\.in\b/\.tmp/;

        print( "--- ", $file, "---", "\n" );
	if( not open( IN, "<$file" ) ) {
 	   print( STDERR "failed to open $file\n" );
        }
        if( not open( TEMP, "> $tempfilename" ) ) {
           print( STDERR "failed to create a temporary file\n" );
        }
        while( <IN> ) {
           print ".";
           s/$searchstring/$replacestring/;
           printf( TEMP  $_) ;
        }
   	close( IN );	
        close( TEMP );
        unlink $file;
        rename( $tempfilename, $file );
   }
}
print "\n";

chdir $home;