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

Cov::Cov( int nRowIn, int nParIn )
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
Cov::Cov( int nRowIn, int nParIn, const SPK_VA::valarray<bool>& minRepFixedIn )
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

void Cov::setPar( const SPK_VA::valarray<double>& parIn )
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

void Cov::setCov( const SPK_VA::valarray<double>& covIn )
{
  assert( covIn.size() == nRow * nRow );

  // Get the parameters that correspond to this covariance matrix.
  valarray<double> parTemp( nPar );
  calcPar( covIn, parTemp );

  // Set the new value for the parameter if it has changed.
  setPar( parTemp );
}


/*************************************************************************
 *
 * Function: invalidateCache
 *
 *//**
 * Invalidates all of the values stored in the cache.
 *//*
 *************************************************************************/

void Cov::invalidateCache() const
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

bool Cov::getUsedCachedCov() const
{
  return usedCachedCov;
}


/*************************************************************************
 *
 * Function: getUsedCachedCov_par
 *
 *************************************************************************/

bool Cov::getUsedCachedCov_par() const
{
  return usedCachedCov_par;
}


/*************************************************************************
 *
 * Function: getUsedCachedInv
 *
 *************************************************************************/

bool Cov::getUsedCachedInv() const
{
  return usedCachedInv;
}


/*************************************************************************
 *
 * Function: getUsedCachedInv_par
 *
 *************************************************************************/

bool Cov::getUsedCachedInv_par() const
{
  return usedCachedInv_par;
}


/*************************************************************************
 *
 * Function: getNRow
 *
 *************************************************************************/

int Cov::getNRow() const
{
  return nRow;
}


/*************************************************************************
 *
 * Function: getNPar
 *
 *************************************************************************/

int Cov::getNPar() const
{
  return nPar;
}

