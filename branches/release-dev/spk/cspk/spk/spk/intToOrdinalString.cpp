/*
%************************************************************************
%                                                                       *
%  From:   Resource Facility for Population Kinetics                    *
%          Department of Bioengineering Box 352255                      *
%          University of Washington                                     *
%          Seattle, WA 98195-2255                                       *
%                                                                       *
%  Copyright (C) 2002, University of Washington,                        *
%  Resource Facility for Population Kinetics. All Rights Reserved.      *
%                                                                       *
%  This software was developed with support from NIH grant RR-12609.    *
%  Please cite this grant in any publication for which this software    *
%  is used and send a notification to the address given above.          *
%                                                                       *
%  Check for updates and notices at:                                    *
%  http://www.rfpk.washington.edu                                       *
%                                                                       *
%************************************************************************

*/
/*************************************************************************
 *
 * File: intToOrdinalString.cpp
 *
 *
 * Converts an integer to an ordinal number string.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: intToOrdinalString
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin intToOrdinalString$$

$spell
  1st
  2nd
  3rd
  enum
  int
$$

$section Conversion of Integers to Ordinal Number Strings$$

$index intToOrdinalString$$
$cindex Conversion of Integers \to Ordinal \Number \Strings$$

$table
$bold Enumerator:$$ $cend
$syntax/enum FirstIntForOrdinals { ZERO_IS_FIRST_INT, ONE_IS_FIRST_INT }/$$ $rend
$bold Prototype:$$ $cend
$syntax/string intToOrdinalString( int /i/, enum FirstIntForOrdinals /firstInt/ )
/$$
$tend

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Returns a string that contains an ordinal number, i.e., 
$pre

    "1st", "2nd", "3rd", ... , 

$$
that corresponds to the integer $italic i$$.  Either $math%0%$$ or 
$math%1%$$ is considered to be the first integer depending on the 
value for $italic firstInt$$.
$pre

$$
If $italic i$$ is less than the first integer, then the string
will just contain the value for $italic i$$ along with a warning, eg.
$pre

    "-2 (error: nonpositive ordinal)"  .

$$

$head Arguments$$
$syntax/
/i/
/$$
This is the integer to convert to the ordinal string.

$syntax/

/firstInt/
/$$
This argument specifies whether $math%0%$$ or $math%1%$$ is 
considered to be the first integer.

$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include "intToOrdinalString.h"

// Standard library header files.
#include <cmath>
#include <sstream>
#include <string>

using namespace std;


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

string intToOrdinalString( int i, enum FirstIntForOrdinals firstInt )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  std::ostringstream ordinalStream;


  //------------------------------------------------------------
  // Get the integer's position relative to the first integer.
  //------------------------------------------------------------

  int iRelative;

  if ( firstInt == ZERO_IS_FIRST_INT )
  {
    iRelative = i + 1;
  }
  else
  {
    iRelative = i;
  }


  //------------------------------------------------------------
  // Handle integers that are less than the first integer.
  //------------------------------------------------------------

  // If the input integer is less than the first integer, then return
  // a string that contains it and a warning message.
  if ( iRelative < 1 )
  {
    ordinalStream << i << " (warning: nonpositive ordinal)";
    return ordinalStream.str();
  }


  //------------------------------------------------------------
  // Set the ordinal string.
  //------------------------------------------------------------

  // Set the number part.
  ordinalStream << iRelative;

  int lastDigit     = static_cast<int>( fmod( static_cast<double>( iRelative ),  10.0 ) );
  int lastTwoDigits = static_cast<int>( fmod( static_cast<double>( iRelative ), 100.0 ) );

  // Add the ending.
  if ( lastTwoDigits > 10 && lastTwoDigits < 14 )
  {
    // If the last two digits are 11, 12, or 13, then add "th".
    ordinalStream << "th";
  }
  else
  {
    // Add the appropriate ending based on the last digit.
    switch( lastDigit )
    {
    case 1:
      ordinalStream << "st";
      break;
    case 2:
      ordinalStream << "nd";
      break;
    case 3:
      ordinalStream << "rd";
      break;
    default:
      ordinalStream << "th";
    }
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  return ordinalStream.str();

}
