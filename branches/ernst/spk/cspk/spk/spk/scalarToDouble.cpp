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
 * @file: scalarToDouble.cpp
 *
 *
 * Implements scalarToDouble() function.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include <spk/scalarToDouble.h>

// CppAD header files.
#include <CppAD/CppAD.h>


/*************************************************************************
 *
 * Function: scalarToDouble
 *
 *//**
 * Converts a Scalar type variable to double type.
 */
/*************************************************************************/

template<class Scalar>
void scalarToDouble( const Scalar& scalarIn, double& doubleOut )
{
  doubleOut = scalarIn;
}
 
template<>
void scalarToDouble< CppAD::AD<double> >( const CppAD::AD<double>& scalarIn, double& doubleOut )
{
  doubleOut = CppAD::Value( CppAD::Var2Par( scalarIn ) );
}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

template void scalarToDouble( const double& scalarIn, double& doubleOut );

template void scalarToDouble( const CppAD::AD<double>& scalarIn, double& doubleOut );

