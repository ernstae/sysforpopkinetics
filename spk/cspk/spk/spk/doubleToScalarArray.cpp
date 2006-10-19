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
 * @file: doubleToScalarArray.cpp
 *
 *
 * Implements doubleToScalarArray() function.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include <spk/doubleToScalarArray.h>
#include <spk/SpkValarray.h>

// CppAD header files.
#include <CppAD/CppAD.h>

using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: doubleToScalarArray
 *
 *//**
 * Converts a double type array to Scalar type.
 */
/*************************************************************************/

template<class Scalar>
void doubleToScalarArray( 
  const valarray<double>& doubleArrayIn,
  valarray<Scalar>& scalarArrayOut )
{
  int n = doubleArrayIn.size();
  scalarArrayOut.resize( n );
  int k;
  for ( k = 0; k < n; k++ )
  {
    scalarArrayOut[k] = doubleArrayIn[k];
  }
}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

template void doubleToScalarArray<double>( 
  const valarray<double>& doubleArrayIn,
  valarray<double>& scalarArrayOut );

template void doubleToScalarArray< CppAD::AD<double> >( 
  const valarray<double>& doubleArrayIn,
  valarray< CppAD::AD<double> >& scalarArrayOut );

