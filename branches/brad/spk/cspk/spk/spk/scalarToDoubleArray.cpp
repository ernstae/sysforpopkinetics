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
 * @file: scalarToDoubleArray.cpp
 *
 *
 * Implements scalarToDoubleArray() function.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include <spk/scalarToDouble.h>
#include <spk/scalarToDoubleArray.h>
#include <spk/SpkValarray.h>

// CppAD header files.
#include <CppAD/CppAD.h>

using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: scalarToDoubleArray
 *
 *//**
 * Converts a Scalar type array to double type.
 */
/*************************************************************************/

template<class Scalar>
void scalarToDoubleArray( 
  const valarray<Scalar>& scalarArrayIn,
  valarray<double>& doubleArrayOut )
{
  int n = scalarArrayIn.size();
  doubleArrayOut.resize( n );
  int k;
  for ( k = 0; k < n; k++ )
  {
    scalarToDouble( scalarArrayIn[k], doubleArrayOut[k] );
  }
}

/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

template void scalarToDoubleArray<double>( 
  const valarray<double>& scalarArrayIn,
  valarray<double>& doubleArrayOut );

template void scalarToDoubleArray< CppAD::AD<double> >( 
  const valarray< CppAD::AD<double> >& scalarArrayIn,
  valarray<double>& doubleArrayOut );

