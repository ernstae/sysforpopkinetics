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
 * File: hasUnnormNumber.cpp
 *
 *
 * Checks to see if a vector contains a value that is unnormalized,
 * i.e., less than (or greater) than the largest (or smallest)
 * normalized value.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: hasUnnormNumber
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin hasUnnormNumber$$

$spell
$$

$section Checking for Values in a Vector that are Unnormalized$$

$index hasUnnormNumber$$
$cindex \Checking \for \Values \in \a Vector \that \are Unnormalized$$

$table
$bold Prototype:$$ $cend
$syntax/template<class VectorType>
bool hasUnnormNumber( const VectorType& /vector/ )
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

Returns true if $italic vector$$ contains a value that is
unnormalized, i.e., less than (or greater) than the largest (or
smallest) normalized value.

$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include "hasUnnormNumber.h"
#include "isUnnormNumber.h"
#include "SpkValarray.h"

// CppAD header files.
#include <CppAD/CppAD.h>

// GiNaC computer algebra library header files.
#include <ginac/ginac.h>

using SPK_VA::valarray;


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

template<class VectorType>
bool hasUnnormNumber( const VectorType& vector )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Get the number of elements in the vector.
  //------------------------------------------------------------

  int nElem = vector.size();


  //------------------------------------------------------------
  // Look for a value that is unnormalized.
  //------------------------------------------------------------

  int i;

  // Return true if an unnormalized value is found.
  for ( i = 0; i < nElem; i++ )
  {
    if ( isUnnormNumber( vector[i] ) )
    {
      return true;
    }
  }

  // Return false because an unnormalized value was not found.
  return false;
}


/*------------------------------------------------------------------------
 * Template Function Instantiations.
 *------------------------------------------------------------------------*/

template bool hasUnnormNumber< valarray<double> >( const valarray<double>& vector );

template bool hasUnnormNumber< valarray< CppAD::AD<double> > >( const valarray< CppAD::AD<double> >& vector );

template bool hasUnnormNumber< valarray<GiNaC::ex> >( const valarray<GiNaC::ex>& vector );

