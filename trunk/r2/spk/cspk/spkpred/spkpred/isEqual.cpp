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
 *//**
 * @file: isEqual.cpp
 *
 *
 * Implements isEqual() function.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include <spk/SpkValarray.h>

// CppAD header files.
#include <CppAD/CppAD.h>

using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: isEqual
 *
 *//**
 * Returns true if the arrays x and y have the same number of elements
 * and if every element in x is equal to the corresponding element in y.
 */
/*************************************************************************/

template<class Scalar>
bool isEqual(
  const SPK_VA::valarray<Scalar>&  x,
  const SPK_VA::valarray<Scalar>&  y )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int n = x.size();

  // If the dimensions don't agree, then they're not equal.
  if ( n != y.size() )
  {
    return false;
  }


  //------------------------------------------------------------
  // Compare the elements in the two arrays.
  //------------------------------------------------------------

  int i;

  // If any element is different, then return false.
  for ( int i = 0; i < n; i++ )
  {
    if ( x[i] != y[i] )
    {
      return false;
    }
  }

  // If all of the elements are the same, then return true.
  return true;
}

template<>
bool isEqual(
  const SPK_VA::valarray< CppAD::AD<double> >&  x,
  const SPK_VA::valarray< CppAD::AD<double> >&  y )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int n = x.size();

  // If the dimensions don't agree, then they're not equal.
  if ( n != y.size() )
  {
    return false;
  }


  //------------------------------------------------------------
  // Compare the elements in the two arrays.
  //------------------------------------------------------------

  int i;

  // If any element is different, then return false.
  for ( int i = 0; i < n; i++ )
  {
    // Note that the CppAD != operator only checks the numeric values
    // for AD objects and not their operation sequence, and it,
    // therefore, cannot detect the case where one variable is made an
    // independent variable with the same value as the first variable.
    //
    // Use the CppAD function that determines if two AD objects are
    // equal, and if they are variables, determines if they correspond
    // to the same operation sequence.
    if ( !CppAD::EqualOpSeq( x[i], y[i] ) )
    {
      return false;
    }
  }

  // If all of the elements are the same, then return true.
  return true;
}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

// Declare double versions of these functions.
template bool isEqual<double>(
  const SPK_VA::valarray<double>&  x,
  const SPK_VA::valarray<double>&  y );

// Declare CppAD::AD<double> versions of these functions.
template bool isEqual< CppAD::AD<double> >(
  const SPK_VA::valarray< CppAD::AD<double> >&  x,
  const SPK_VA::valarray< CppAD::AD<double> >&  y );

