#! /usr/bin/perl

use strict;
use warnings;

my $filePath = "./";

#opendir( D, "../test_inputs_linux/" );
opendir( D, $filePath );

my @allcontrols = readdir D;
closedir D;

foreach my $file (@allcontrols) {
      if( $file =~ /\.in\b/ ) {
         my @command = ( "perl",
                         "./runSpkAndNonmem.pl",
                         "$filePath$file",
                        "./genericDriver"
                       );
         system( @command );
         print( STDOUT "Done! ($file).\n" );
      }
}
