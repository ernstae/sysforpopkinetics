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
/************************************************************************
 *//**
 * @file: Cov.cpp
 *
 *
 * Implements Cov class.
 *//*
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "Cov.h"
#include "isEqual.h"

// SPK library header files.
#include <spk/SpkValarray.h>

// CppAD header files.
#include <CppAD/CppAD.h>

// Standard library header files.
#include <cassert>

using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: Cov
 *
 *//**
 * Constructs a covariance matrix with nRowIn rows and columns
 * and with nParIn parameters in its minimal representation.
 *//*
 *************************************************************************/

template<class Scalar>
Cov<Scalar>::Cov( int nRowIn, int nParIn )
  :
  nRow             ( nRowIn ),
  nPar             ( nParIn ),
  parFixed         ( false, nParIn ),
  parCurr          ( nParIn ),
  covCurr          ( nRowIn * nRowIn ),
  cov_parCurr      ( nRowIn * nRowIn * nParIn ),
  invCurr          ( nRowIn * nRowIn ),
  inv_parCurr      ( nRowIn * nRowIn * nParIn ),
  isCovCurrOk      ( false ),
  isCov_parCurrOk  ( false ),
  isInvCurrOk      ( false ),
  isInv_parCurrOk  ( false )
{
}

//[Revisit - eliminate the need for two constructors in Cov, FullCov, DiagCov by
//  providing default (all false) in PopPredModel and IndPredModel constructors - Dave]
template<class Scalar>
Cov<Scalar>::Cov( int nRowIn, int nParIn, const SPK_VA::valarray<bool>& minRepFixedIn )
  :
  nRow             ( nRowIn ),
  nPar             ( nParIn ),
  parFixed         ( nParIn),
  parCurr          ( nParIn ),
  covCurr          ( nRowIn * nRowIn ),
  cov_parCurr      ( nRowIn * nRowIn * nParIn ),
  invCurr          ( nRowIn * nRowIn ),
  inv_parCurr      ( nRowIn * nRowIn * nParIn ),
  isCovCurrOk      ( false ),
  isCov_parCurrOk  ( false ),
  isInvCurrOk      ( false ),
  isInv_parCurrOk  ( false )
{

  // For FullCov (including banded) ONLY:
  //minRepParFixed enters as an upper triangle in row major order...
  //parFixed needs to be a lower triangle in row major order.
  //(Note:  minRep and parCurr are similarly opposing - confusing)
  
  if (nPar > nRow) {
  //Store minRepFixed in a full matrix
  valarray<double> fullCovFixed( nRow * nRow );
  int k = 0;
  for( int i = 0; i < nRow; i++ )
  {
    for( int j = 0; j <= i; j++ )
    {
      fullCovFixed[ i * nRow + j] = minRepFixedIn[ k++ ];
    }
  }
  //parse full matrix to parFixed
  k = 0;
  for( int i = 0; i < nRow; i++ )
  {
    for( int j = i; j < nRow; j++ )
    {
      parFixed[ k++ ] = fullCovFixed[ j * nRow + i ];
    }
  }
  }
  else
    parFixed = minRepFixedIn;

}

/*************************************************************************
 *
 * Function: setPar
 *
 *//**
 * Sets the current values for the minimal representation parameters
 * equal to parIn.
 *//*
 *************************************************************************/

template<class Scalar>
void Cov<Scalar>::setPar( const SPK_VA::valarray<Scalar>& parIn )
{
  assert( parIn.size() == nPar );

  // Only reset the parameter value if it has changed.
  if ( !isEqual( parIn, parCurr ) )
  { 
    // If the parameter has changed, then any cached
    // values are no longer valid.
    invalidateCache();

    // Set the new value.
    parCurr = parIn;
  }
}


/*************************************************************************
 *
 * Function: setCov
 *
 *//**
 * Sets the current values for the minimal representation parameters
 * equal to the minimal representation parameters for covIn.
 *//*
 *************************************************************************/

template<class Scalar>
void Cov<Scalar>::setCov( const SPK_VA::valarray<Scalar>& covIn )
{
  assert( covIn.size() == nRow * nRow );

  // Get the parameters that correspond to this covariance matrix.
  valarray<Scalar> parCovIn( nPar );
  calcPar( covIn, parCovIn );

  // Set the new value for the parameter if it has changed.
  setPar( parCovIn );
}


/*************************************************************************
 *
 * Function: invalidateCache
 *
 *//**
 * Invalidates all of the values stored in the cache.
 *//*
 *************************************************************************/

template<class Scalar>
void Cov<Scalar>::invalidateCache() const
{
  isCovCurrOk     = false;
  isCov_parCurrOk = false;
  isInvCurrOk     = false;
  isInv_parCurrOk = false;
}


/*************************************************************************
 *
 * Function: getUsedCachedCov
 *
 *************************************************************************/

template<class Scalar>
bool Cov<Scalar>::getUsedCachedCov() const
{
  return usedCachedCov;
}


/*************************************************************************
 *
 * Function: getUsedCachedCov_par
 *
 *************************************************************************/

template<class Scalar>
bool Cov<Scalar>::getUsedCachedCov_par() const
{
  return usedCachedCov_par;
}


/*************************************************************************
 *
 * Function: getUsedCachedInv
 *
 *************************************************************************/

template<class Scalar>
bool Cov<Scalar>::getUsedCachedInv() const
{
  return usedCachedInv;
}


/*************************************************************************
 *
 * Function: getUsedCachedInv_par
 *
 *************************************************************************/

template<class Scalar>
bool Cov<Scalar>::getUsedCachedInv_par() const
{
  return usedCachedInv_par;
}


/*************************************************************************
 *
 * Function: getNRow
 *
 *************************************************************************/

template<class Scalar>
int Cov<Scalar>::getNRow() const
{
  return nRow;
}


/*************************************************************************
 *
 * Function: getNPar
 *
 *************************************************************************/

template<class Scalar>
int Cov<Scalar>::getNPar() const
{
  return nPar;
}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

// Declare double versions of these functions.
template Cov<double>::Cov( int nRowIn, int nParIn );
template Cov<double>::Cov( int nRowIn, int nParIn, const SPK_VA::valarray<bool>& minRepFixedIn );

template void Cov<double>::setPar( const SPK_VA::valarray<double>& parIn );
template void Cov<double>::setCov( const SPK_VA::valarray<double>& covIn );

template void Cov<double>::invalidateCache() const;

template bool Cov<double>::getUsedCachedCov()     const;
template bool Cov<double>::getUsedCachedCov_par() const;
template bool Cov<double>::getUsedCachedInv()     const;
template bool Cov<double>::getUsedCachedInv_par() const;

template int Cov<double>::getNRow() const;
template int Cov<double>::getNPar() const;

// Declare CppAD::AD<double> versions of these functions.t
template Cov< CppAD::AD<double> >::Cov( int nRowIn, int nParIn );
template Cov< CppAD::AD<double> >::Cov( int nRowIn, int nParIn, const SPK_VA::valarray<bool>& minRepFixedIn );

template void Cov< CppAD::AD<double> >::setPar( const SPK_VA::valarray< CppAD::AD<double> >& parIn );
template void Cov< CppAD::AD<double> >::setCov( const SPK_VA::valarray< CppAD::AD<double> >& covIn );

template void Cov< CppAD::AD<double> >::invalidateCache() const;

template bool Cov< CppAD::AD<double> >::getUsedCachedCov()     const;
template bool Cov< CppAD::AD<double> >::getUsedCachedCov_par() const;
template bool Cov< CppAD::AD<double> >::getUsedCachedInv()     const;
template bool Cov< CppAD::AD<double> >::getUsedCachedInv_par() const;

template int Cov< CppAD::AD<double> >::getNRow() const;
template int Cov< CppAD::AD<double> >::getNPar() const;

