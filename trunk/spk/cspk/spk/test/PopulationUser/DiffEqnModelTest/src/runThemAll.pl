#! /usr/bin/perl

use strict;
use warnings;


opendir( D, "." );

my @allcontrols = readdir D;
closedir D;


foreach my $file (@allcontrols) {
      if( $file =~ /\.in\b/ ) {
         my @command = ( "perl",
                         "./runSpkAndNonmem.pl",
                         "$file",
                        "./DiffEqnModelTest"
                       );
         system( @command );
         print( STDOUT "Done! ($file).\n" );
      }
}
