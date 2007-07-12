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
 * File: isNotANumber.cpp
 *
 *
 * Checks to see if a value is not a number (NaN).
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: isNotANumber
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin isNotANumber$$

$spell
$$

$section Checking for Values that are not a Number (NaN)$$

$index isNotANumber$$
$cindex \Checking \for \Values \that \are not \a Number (NaN)$$

$table
$bold Prototype:$$ $cend
$syntax/template<class ValueType>
bool isNotANumber( const ValueType& /value/ )
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

Returns true if $italic value$$ is not a number (NaN).  Otherwise, it returns false.

$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include "isNotANumber.h"

// CppAD header files.
#include <CppAD/CppAD.h>

// GiNaC computer algebra library header files.
#include <ginac/ginac.h>


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

template<class ValueType>
bool isNotANumber( const ValueType& value )
{
  // Check to see if the value is not a number (NaN).
  if ( value == value )
  {
    // This value is equal to itself, so it is not a NaN.
    return false;
  }
  else
  {
    // This value is not equal to itself, so it is a NaN.
    return true;
  }

}

template<>
bool isNotANumber( const GiNaC::ex& value )
{
  // There are no NaN's in GiNaC, so always return a value of false.
  return false;
}


/*------------------------------------------------------------------------
 * Template Function Instantiations.
 *------------------------------------------------------------------------*/

template bool isNotANumber<double>( const double& value );

template bool isNotANumber< CppAD::AD<double> >( const CppAD::AD<double> & value );

template bool isNotANumber< CppAD::AD< CppAD::AD<double> > >( const CppAD::AD< CppAD::AD<double> > & value );
template bool isNotANumber< CppAD::AD< CppAD::AD< CppAD::AD<double> > > >( const CppAD::AD< CppAD::AD< CppAD::AD<double> > > & value );
template bool isNotANumber<GiNaC::ex>( const GiNaC::ex& value );


