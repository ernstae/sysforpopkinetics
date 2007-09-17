#! /usr/bin/perl

use strict;
use warnings;


opendir( D, "../inputs" );

my @allcontrols = readdir D;
closedir D;


foreach my $file (@allcontrols) {
      if( $file =~ /\.in\b/ ) {
         my @command = ( "perl",
                         "./runSpkAndNonmem.pl",
                         "../inputs/$file",
                         "./DiffEqnModelTest"
                       );
         system( @command );
         print( STDOUT "Done! ($file).\n" );
      }
}
