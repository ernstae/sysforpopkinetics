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
 * File: isUnnormnumber.cpp
 *
 *
 * Checks to see if a value is unnormalized, i.e., less than (or
 * greater) than the largest (or smallest) normalized value.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: isUnnormNumber
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin isUnnormNumber$$

$spell
$$

$section Checking for Values that are Unnormalized$$

$index isUnnormNumber$$
$cindex \Checking \for \Values \that \are Unnormalized$$

$table
$bold Prototype:$$ $cend
$syntax/template<class ValueType>
bool isUnnormNumber( const ValueType& /value/ )
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

Returns true if $italic value$$ is unnormalized, i.e., less than (or
greater) than the largest (or smallest) normalized value.

$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include "isUnnormNumber.h"

// Standard library header files.
#include <limits>

// CppAD header files.
#include <CppAD/CppAD.h>


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

template<class ValueType>
bool isUnnormNumber( const ValueType& value )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Set the limits for the range of values that are normalized.
  //------------------------------------------------------------

  ValueType minAllowed;
  ValueType maxAllowed;

  if ( numeric_limits<ValueType>::is_bounded )
  {
    maxAllowed = numeric_limits<ValueType>::max();
    minAllowed = - maxAllowed;
  }
  else
  {
    ValueType one  = ValueType( 1 );
    ValueType zero = ValueType( 0 );

    maxAllowed = one / zero;
    minAllowed = - maxAllowed;
  }


  //------------------------------------------------------------
  // Check to see if the value is unnormalized.
  //------------------------------------------------------------

  if ( value > minAllowed && value < maxAllowed )
  {
    // This value is not unnormalized.
    return false;
  }
  else
  {
    // This value is less than (or greater) than the largest (or smallest)
    // normalized value, so it is unnormalized.
    return true;
  }

}


/*------------------------------------------------------------------------
 * Template Function Instantiations.
 *------------------------------------------------------------------------*/

template bool isUnnormNumber<double>( const double& value );

template bool isUnnormNumber< CppAD::AD<double> >( const CppAD::AD<double> & value );
