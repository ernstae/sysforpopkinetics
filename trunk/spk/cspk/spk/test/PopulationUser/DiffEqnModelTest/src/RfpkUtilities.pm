#*********************************************************************************
#
# File: RfpkUtilities.pm
#
#
# This Perl module contains various utility subroutines.
#
# Author: Mitch Watrous
#
#
# Exported Symbols
# ----------------
#
# printArray
#
# printArrayOfArrayRefs
#
# isRelTolEqual
#
#*********************************************************************************

#------------------------------------------------------------------------
# Preliminaries.
#------------------------------------------------------------------------

use strict;

package RfpkUtilities;

# Specify the symbols that will be exported.
require Exporter;
our @ISA        = qw(Exporter);
our @EXPORT     = qw( printArray
                      printArrayOfArrayRefs
                      isRelTolEqual );


#*****************************************************************
#
# Subroutine: printArray
#
#
# Prints the values in an array.
#
# Arguments
# ---------
#
# $_[0]
#
# Ref to an array of values.
#
#*****************************************************************

sub printArray
{
  for ( @_ )
  {
    print "$_ \n";
  }
}


#*****************************************************************
#
# Subroutine: printArrayOfArrayRefs
#
#
# Prints the values in an array of array references.
#
# Arguments
# ---------
#
# $_[0]
#
# Ref to an array of array references.
#
#*****************************************************************

sub printArrayOfArrayRefs
{
  for ( @_ )
  {
    for ( @$_ )
    {
      print "$_  ";
    }
    print "\n";
  }
}
  

#*****************************************************************
#
# Subroutine: isRelTolEqual
#
#
# Checks to see if two numbers are equal up to relative tolerance,
# where the scale for the test is the larger of the two.
#
#
# Arguments
# ---------
#
# $_[0]
#
# First number for the test.
#
#
# $_[1]
#
# Second number for the test.
#
#
# $_[2]
#
# Tolerance for the test.
#
#
# Return Values
# -------------
#
# The return value is true if the numbers are equal up to the
# tolerance.  Otherwise the return value is false. 
#
#*****************************************************************

sub isRelTolEqual
{
  # Get the arguments.
  my $x   = shift;
  my $y   = shift;
  my $tol = shift;

  # Set the scale.
  my $scale;
  if ( abs( $x ) > abs( $y ) )
  {
    $scale = abs( $x );
  }
  else
  {
    $scale = abs( $y );
  }

  # Compare the values.
  if ( abs( $x - $y ) <= $tol * $scale)
  {
    return 1;   # True.
  }
  else 
  {
    return 0;   # False.
  }
}


#*********************************************************************************
#
# Module return value.
#
#*********************************************************************************

1;
