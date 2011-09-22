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
 * @file: doubleToScalar.cpp
 *
 *
 * Implements doubleToScalar() function.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include <spk/doubleToScalar.h>
#include <spk/SpkValarray.h>

// CppAD header files.
#include <CppAD/CppAD.h>

// GiNaC computer algebra library header files.
#include <ginac/ginac.h>

using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: doubleToScalar
 *
 *//**
 * Converts a double type variable to Scalar type.
 */
/*************************************************************************/

template<class Scalar>
void doubleToScalar( const double& doubleIn, Scalar& scalarOut )
{
  scalarOut = doubleIn;
}

template<>
void doubleToScalar< CppAD::AD< CppAD::AD<double> > >( const double& doubleIn, CppAD::AD< CppAD::AD<double> >& scalarOut )
{
  scalarOut = CppAD::AD<double>( CppAD::AD<double>( doubleIn ) );
}

template<>
void doubleToScalar< CppAD::AD< CppAD::AD< CppAD::AD<double> > > >( const double& doubleIn, CppAD::AD< CppAD::AD< CppAD::AD<double> > >& scalarOut )
{
  scalarOut = CppAD::AD<double>( CppAD::AD<double>( CppAD::AD<double>( doubleIn ) ) );
}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

template void doubleToScalar( const double& doubleIn, double& scalarOut );

template void doubleToScalar( const double& doubleIn, CppAD::AD<double>& scalarOut );

template void doubleToScalar( const double& doubleIn, CppAD::AD< CppAD::AD<double> >& scalarOut );

template void doubleToScalar( const double& doubleIn, CppAD::AD< CppAD::AD< CppAD::AD<double> > >& scalarOut );

template void doubleToScalar( const double& doubleIn, GiNaC::ex& scalarOut );

