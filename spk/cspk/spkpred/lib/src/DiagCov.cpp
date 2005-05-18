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
 * @file: DiagCov.cpp
 *
 *
 * Implements DiagCov class.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "DiagCov.h"

// SPK library header files.
#include <spk/SpkValarray.h>

// Standard library header files.
#include <cassert>
#include <cmath>

using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: DiagCov
 *
 *//**
 * Constructs a diagonal covariance matrix with nRowIn rows and columns.
 *//*
 *************************************************************************/

DiagCov::DiagCov( int nRowIn )
  :
  Cov( nRowIn, nRowIn )
{
}


/*************************************************************************
 *
 * Function: cov
 *
 *//**
 * Evaluates the covariance matrix at the current parameter value.
 *
 * In particular, this function sets covOut equal to
 * \f[
 *     \mbox{cov}(\mbox{par}) .
 * \f]
 *//*
 *************************************************************************/

void DiagCov::cov( SPK_VA::valarray<double>& covOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  covOut.resize( nRow * nRow );


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  if ( isCovCurrOk )
  {
    covOut = covCurr;
    usedCachedCov = true;

    return;
  }
  else
  {
    usedCachedCov = false;
  }


  //------------------------------------------------------------
  // Evaluate the covariance matrix.
  //------------------------------------------------------------

  // Create a matrix that has only zeroes.
  covCurr = 0.0;

  // Set the diagonal elements,
  //
  //    cov      ( par )  =  exp[ 2 par  ]  .
  //       (i, i)                      i   
  //
  int i;
  for ( i = 0; i < nPar; i++ )
  {
    covCurr[i + i * nRow] = exp( 2.0 * parCurr[i] );
  }    


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isCovCurrOk = true;
  covOut = covCurr;
}


/*************************************************************************
 *
 * Function: cov_par
 *
 *//**
 * Evaluates the derivative of the covariance matrix at the current
 * parameter value.
 *
 * In particular, this function sets cov_parOut equal to
 * \f[
 *     \partial_{\mbox{par}} \; \mbox{cov}(\mbox{par}) =
 *       \partial_{\mbox{par}} \; \mbox{rvec} \left[ \mbox{cov}(\mbox{par}) \right] .
 * \f]
 *//*
 *************************************************************************/

void DiagCov::cov_par( SPK_VA::valarray<double>& cov_parOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  cov_parOut.resize( nRow * nRow * nPar );


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  if ( isCov_parCurrOk )
  {
    cov_parOut = cov_parCurr;
    usedCachedCov_par = true;

    return;
  }
  else
  {
    usedCachedCov_par = false;
  }


  //------------------------------------------------------------
  // Evaluate the derivative of the covariance matrix.
  //------------------------------------------------------------

  int nCov_parRow( nRow * nRow );

  // Create a matrix that has only zeroes.
  cov_parCurr = 0.0;

  // Set the nonzero elements of the derivative, i.e. the partial
  // derivatives of the diagonal elements of the covariance,
  //
  //     (i)     
  //    d     cov      ( par )  =  2 exp[ 2 par  ]  .
  //     par     (i, i)                        i
  //
  // Note that an rvec operation is performed on the elements of
  // the covariance before the partial derivatives are computed,
  //                                       -            -
  //                                      |              |
  //     d     cov( par )  =  d     rvec  |  cov( par )  |  .
  //      par                  par        |              |
  //                                       -            -
  int i;
  int row;
  for ( i = 0; i < nPar; i++ )
  {
    // Set the row in the rvec version of the covariance.
    row = i * nRow + i;

    cov_parCurr[row + i * nCov_parRow] = 2.0 * exp( 2.0 * parCurr[i] );
  }    


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isCov_parCurrOk = true;
  cov_parOut = cov_parCurr;
}


/*************************************************************************
 *
 * Function: inv
 *
 *//**
 * Evaluates the inverse of the covariance matrix at the current
 * parameter value.
 *
 * In particular, this function sets invOut equal to
 * \f[
 *     \mbox{cov}^{-1}(\mbox{par}) .
 * \f]
 *//*
 *************************************************************************/

void DiagCov::inv( SPK_VA::valarray<double>& invOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  invOut.resize( nRow * nRow );


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  if ( isInvCurrOk )
  {
    invOut = invCurr;
    usedCachedInv = true;

    return;
  }
  else
  {
    usedCachedInv = false;
  }


  //------------------------------------------------------------
  // Evaluate the inverse of the covariance matrix.
  //------------------------------------------------------------

  // Create a matrix that has only zeroes.
  invCurr = 0.0;

  // Set the diagonal elements,
  //
  //    inv      ( par )  =  exp[ - 2 par  ]  .
  //       (i, i)                        i
  //
  int i;
  for ( i = 0; i < nPar; i++ )
  {
    invCurr[i + i * nRow] = exp( -2.0 * parCurr[i] );
  }    


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isInvCurrOk = true;
  invOut = invCurr;
}


/*************************************************************************
 *
 * Function: inv_par
 *
 *//**
 * Evaluates the derivative of the inverse of the covariance matrix
 * at the current parameter value.
 *
 * In particular, this function sets inv_parOut equal to
 * \f[
 *     \partial_{\mbox{par}} \; \mbox{cov}^{-1}(\mbox{par}) =
 *       \partial_{\mbox{par}} \; \mbox{rvec} \left[ \mbox{cov}^{-1}(\mbox{par}) \right] .
 * \f]
 *//*
 *************************************************************************/

void DiagCov::inv_par( SPK_VA::valarray<double>& inv_parOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  inv_parOut.resize( nRow * nRow * nPar );


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  if ( isInv_parCurrOk )
  {
    inv_parOut = inv_parCurr;
    usedCachedInv_par = true;

    return;
  }
  else
  {
    usedCachedInv_par = false;
  }


  //------------------------------------------------------------
  // Evaluate the derivative of the inverse of the covariance matrix.
  //------------------------------------------------------------

  int nInv_parRow( nRow * nRow );

  // Create a matrix that has only zeroes.
  inv_parCurr = 0.0;

  // Set the nonzero elements of the derivative, i.e. the partial
  // derivatives of the diagonal elements of the inverse of the 
  // the covariance,
  //
  //     (i)     
  //    d     inv      ( par )  =  -2 exp[ -2 par  ]  .
  //     par     (i, i)                          i
  //
  // Note that an rvec operation is performed on the elements of
  // the covariance before the partial derivatives are computed,
  //                                       -            -
  //                                      |              |
  //     d     inv( par )  =  d     rvec  |  inv( par )  |  .
  //      par                  par        |              |
  //                                       -            -
  int i;
  int row;
  for ( i = 0; i < nPar; i++ )
  {
    // Set the row in the rvec version of the inverse of the covariance.
    row = i * nRow + i;

    inv_parCurr[row + i * nInv_parRow] = -2.0 * exp( -2.0 * parCurr[i] );
  }    


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isInv_parCurrOk = true;
  inv_parOut = inv_parCurr;
}


/*************************************************************************
 *
 * Function: getParLimits
 *
 *//**
 * Gets the lower and upper limits for the covariance matrix parameters
 * at the current parameter value.  These limits are for use during the
 * optimization of objective functions that depend on these parameters.
 *
 * This function assumes that the current values for the covariance
 * parameters are approximately equal to the final or true values.
 *//*
 *************************************************************************/

void DiagCov::getParLimits(
  SPK_VA::valarray<double>&  parLow,
  SPK_VA::valarray<double>&  parUp ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  parLow.resize( nPar );
  parUp .resize( nPar );


  //------------------------------------------------------------
  // Evaluate the current value for the covariance matrix. 
  //------------------------------------------------------------

  // If the covariance has not been evaluated at the current
  // value for the parameters, then evaluate it.
  if ( !isCovCurrOk )
  {
    valarray<double> tempCov( nRow * nRow );
    cov( tempCov );
  }


  //------------------------------------------------------------
  // Set the limits for the parameters.
  //------------------------------------------------------------

  // Set the limits for optimization to constrain the diagonal
  // elements of the covariance matrix as follows,
  //
  //      1      (curr)                              (curr)
  //     ---  cov        <=   cov        <=  100  cov        .
  //     100     (i,i)           (i,i)               (i,i) 
  //
  // These limits for the covariance diagonal elements imply
  // these limits for its parameters,
  //
  //              -                -                            -                -
  //             |                  |                          |                  |
  //      1      |   1      (curr)  |                   1      |          (curr)  |
  //     --- log |  ---  cov        |  <=   par    <=  --- log |  100  cov        |  .
  //      2      |  100     (i,i)   |          i        2      |          (i,i)   |
  //             |                  |                          |                  |
  //              -                -                            -                -
  //
  int i;
  for ( i = 0; i < nPar; i++ )
  {
    parLow[i] = 0.5 * log( covCurr[i + i * nRow] / 100.0 );
    parUp[i]  = 0.5 * log( covCurr[i + i * nRow] * 100.0 );
  }    

}


/*************************************************************************
 *
 * Function: calcPar
 *
 *//**
 * Sets parOut equal to the covariance matrix parameters that
 * correspond to the covariance matrix covIn.
 *//*
 *************************************************************************/

void DiagCov::calcPar( 
  const SPK_VA::valarray<double>& covIn,
  SPK_VA::valarray<double>&       parOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  int nCovInRow = static_cast<int>( sqrt( static_cast<double>( covIn.size() ) ) );
  assert( covIn.size() == nCovInRow * nCovInRow );

  // Set the number of parameters for this covariance matrix.
  int nCovInPar = nCovInRow;

  parOut.resize( nCovInPar );


  //------------------------------------------------------------
  // Set the covariance matrix parameters.
  //------------------------------------------------------------

  // Set the parameter elements,
  //
  //              1
  //    par   =  ---  log[ cov      ( par ) ]  .
  //       i      2           (i, i)             
  //
  int i;
  for ( i = 0; i < nCovInPar; i++ )
  {
    parOut[i] = 0.5 * log( covIn[i + i * nCovInRow] );
  }    

}


/*************************************************************************
 *
 * Function: calcCovMinRep
 *
 *//**
 * Sets covMinRepOut equal to the minimal representation for the
 * covariance matrix covIn.
 *
 * The minimal representation is the set of diagonal elements.
 *//*
 *************************************************************************/

void DiagCov::calcCovMinRep( 
  const SPK_VA::valarray<double>& covIn,
  SPK_VA::valarray<double>&       covMinRepOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  int nCovInRow = static_cast<int>( sqrt( static_cast<double>( covIn.size() ) ) );
  assert( covIn.size() == nCovInRow * nCovInRow );

  // Set the number of parameters for this covariance matrix.
  int nCovInPar = nCovInRow;

  covMinRepOut.resize( nCovInPar );


  //------------------------------------------------------------
  // Set the minimal representation for the covariance matrix.
  //------------------------------------------------------------

  // Extract the diagonal elements from the covariance matrix.
  int i;
  for ( i = 0; i < nCovInPar; i++ )
  {
    covMinRepOut[i] = covIn[i + i * nCovInRow];
  }    

}


/*************************************************************************
 *
 * Function: calcCovMinRep_par
 *
 *//**
 * Sets covMinRep_parOut equal to the derivative of the minimal
 * representation for the covariance matrix with derivative cov_parIn.
 *
 * The minimal representation is the set of diagonal elements.
 *//*
 *************************************************************************/

void DiagCov::calcCovMinRep_par( 
  const SPK_VA::valarray<double>& cov_parIn,
  int                             nCov_parInCol,
  SPK_VA::valarray<double>&       covMinRep_parOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Set the number of parameters for the covariance matrix.
  int nCovInPar = nCov_parInCol;

  // Set the number of rows in the covariance matrix.
  int nCovInRow = nCovInPar;
  assert( cov_parIn.size() == nCovInRow * nCovInRow * nCovInPar );

  // Set the number of rows in the derivative of the covariance matrix
  // and its minimal representation.
  int nCov_parInRow        = nCovInRow * nCovInRow;
  int nCovMinRep_parOutRow = nCovInPar;

  covMinRep_parOut.resize( nCovInPar * nCovInPar );


  //------------------------------------------------------------
  // Set the derivative of the covariance matrix minimal representation.
  //------------------------------------------------------------

  int i;
  int k;

  // Extract the derivatives of the elements that are on the diagonal.
  for ( k = 0; k < nCovInPar; k++ )
  {
    for ( i = 0; i < nCovInRow; i++ )
    {
      covMinRep_parOut[( i                 ) + k * nCovMinRep_parOutRow] = 
        cov_parIn     [( i + i * nCovInRow ) + k * nCov_parInRow];
    }    
  }    

}


/*************************************************************************
 *
 * Function: expandCovMinRep
 *
 *//**
 * Sets covOut equal to the covariance matrix that corresponds
 * to the minimal representation for the covariance matrix that
 * is contained in covMinRepIn.
 *
 * The minimal representation is the set of diagonal elements.
 *//*
 *************************************************************************/

void DiagCov::expandCovMinRep( 
  const SPK_VA::valarray<double>& covMinRepIn,
  SPK_VA::valarray<double>&       covOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Set the number of parameters for this covariance matrix.
  int nCovInPar = covMinRepIn.size();

  // Set the number of rows in the covariance matrix.
  int nCovInRow = nCovInPar;

  covOut.resize( nCovInRow * nCovInRow );


  //------------------------------------------------------------
  // Expand the minimal representation for the covariance matrix.
  //------------------------------------------------------------

  // Set all of the covariance matrix elements equal to zero.
  covOut = 0.0;

  // Set the diagonal elements from the covariance matrix.
  int i;
  for ( i = 0; i < nCovInPar; i++ )
  {
    covOut[i + i * nCovInRow] = covMinRepIn[i];
  }    

}

