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
 * File: FullCov.cpp
 *
 *
 * This class supports full covariance matrices.  It is a concrete 
 * subclass of the abstract covariance base class.
 *
 * This class utilizes the following parameterization for the covariance
 * matrix in order to insure that it is positive definite and symmetric:
 *
 *                                     T
 *    cov( par )  =  L( par )  L( par )  ,
 *
 * where par contains the current value for the parameters and
 * L( par ) is the following square, lower triangular matrix,
 *
 *                  -                                                            -
 *                 |  exp[ par  ]                                           0     |
 *                 |          0                                                   |
 *                 |                                                              |
 *                 |     par       exp[ par  ]                                    |
 *                 |        1              2                                      |
 *                 |                                                              |
 *    L( par )  =  |     par          par       exp[ par  ]                       |  ,
 *                 |        3            4              5                         |
 *                 |                                                              |
 *                 |       .            .    .            .                       |
 *                 |       .            .      .            .                     |
 *                 |       .            .        .            .                   |
 *                 |                                                              |
 *                 |                               par          exp[ par       ]  |
 *                 |                                  nPar-2            nPar-1    |
 *                  -                                                            -
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "FullCov.h"

// SPK library header files.
#include <spk/AkronBtimesC.h>
#include <spk/inverse.h>
#include <spk/cholesky.h>
#include <spk/SpkValarray.h>

// Standard library header files.
#include <cassert>
#include <cmath>

using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: FullCov
 *
 *************************************************************************/

FullCov::FullCov( int nRowIn )
  :
  Cov( nRowIn, nRowIn * ( nRowIn + 1 ) / 2 )
{
}


/*************************************************************************
 *
 * Function: cov
 *
 *
 * Evaluates the covariance matrix at the current parameter value.
 *
 *************************************************************************/

void FullCov::cov( valarray<double>& covOut ) const
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

  int i;
  int j;
  int k;

  // Create a matrix that has only zeroes.
  covCurr = 0.0;

  // Set the diagonal elements,
  //
  //                         ---- 
  //                         \                   2
  //     cov     ( par )  =  /      par              +  exp[ 2 par           ]  ,
  //        (i,i)            ----      (sum(i)+k)                 (sum(i)+i)
  //                         k < i
  //
  // where
  //
  //     sum( m )  =  0 + 1 + 2 + ... + m  .
  //
  int covIndex;
  int sumI = 0;
  for ( i = 0; i < nRow; i++ )
  {
    sumI += i;

    covIndex = i + i * nRow;

    for ( k = 0; k < i; k++ )
    {
      covCurr[covIndex] += parCurr[sumI + k] * parCurr[sumI + k];
    }    

    covCurr[covIndex] += exp( 2.0 * parCurr[sumI + i] );
  }    

  // Set the elements below the diagonal (j < i),
  //
  //                         ---- 
  //                         \   
  //     cov     ( par )  =  /      par            par            +  par            exp[ par           ]  ,
  //        (i,j)            ----      (sum(i)+k)     (sum(j)+k)        (sum(i)+j)          (sum(j)+j)
  //                         k < j
  //
  // where
  //
  //     sum( m )  =  0 + 1 + 2 + ... + m  .
  //
  sumI = 0;
  int sumJ;
  for ( i = 1; i < nRow; i++ )
  {
    sumI += i;
    sumJ = 0;

    for ( j = 0; j < i; j++ )
    {
      sumJ += j;

      covIndex = i + j * nRow;
    
      for ( k = 0; k < j; k++ )
      {
        covCurr[covIndex] += parCurr[sumI + k] * parCurr[sumJ + k];
      }    
    
      covCurr[covIndex] += parCurr[sumI + j] * exp( parCurr[sumJ + j] );
    }
  }    

  // Set the elements above the diagonal (j > i),
  //
  //     cov     ( par )  =  cov     ( par )  .
  //        (i,j)               (j,i)
  //
  for ( i = 0; i < nRow - 1; i++ )
  {
    for ( j = i + 1; j < nRow; j++ )
    {
      covCurr[i + j * nRow] = covCurr[j + i * nRow];
    }
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
 *
 * Evaluates the derivative of the covariance matrix at the current
 * parameter value.
 *
 *************************************************************************/

void FullCov::cov_par( valarray<double>& cov_parOut ) const
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

  int i;
  int j;
  int k;
  int row;
  int rowAbove;
  int rowBelow;

  // Create a matrix that has only zeroes.
  cov_parCurr = 0.0;

  // Set the partial derivatives of the diagonal elements for
  // the case k < i,
  //
  //      (sum(i)+k)
  //     d             cov     ( par )  =  2  par            ,
  //      par             (i,i)                  (sum(i)+k)
  //
  // and the case k = i,
  //
  //      (sum(i)+i)
  //     d             cov     ( par )  =  2  exp[ 2 par           ]  ,
  //      par             (i,i)                         (sum(i)+i)
  //
  // where
  //
  //     sum( m )  =  0 + 1 + 2 + ... + m  .
  //
  // Note that an rvec operation is performed on the elements of
  // the covariance before the partial derivatives are computed,
  //                                       -            -
  //                                      |              |
  //     d     cov( par )  =  d     rvec  |  cov( par )  |  .
  //      par                  par        |              |
  //                                       -            -
  int sumI = 0;
  for ( i = 0; i < nRow; i++ )
  {
    // Set the row in the rvec version of the covariance.
    row = i * nRow + i;

    sumI += i;

    for ( k = 0; k < i; k++ )
    {
      // Add the derivative of this term with respect to this parameter.
      cov_parCurr[row + ( sumI + k ) * nCov_parRow] += 2.0 * parCurr[sumI + k];
    }    

    // Add the derivative of this term with respect to this parameter.
    cov_parCurr[row + ( sumI + i ) * nCov_parRow] += 2.0 * exp( 2.0 * parCurr[sumI + i] );
  }    

  // Set the partial derivatives of the elements below the 
  // diagonal (j < i) for the case k < j,
  //
  //      (sum(i)+k)
  //     d             cov     ( par )  =  par           .
  //      par             (i,j)               (sum(j)+k)
  //
  //      (sum(j)+k)
  //     d             cov     ( par )  =  par           .
  //      par             (i,j)               (sum(i)+k)
  //
  // and the case k = j,
  //
  //      (sum(i)+j)                       
  //     d             cov     ( par )  =  exp[ par           ]  ,
  //      par             (i,j)                    (sum(j)+j)
  //
  //      (sum(j)+j)                       
  //     d             cov     ( par )  =  par            exp[ par           ]  ,
  //      par             (i,j)               (sum(i)+j)          (sum(j)+j)
  //
  // where
  //
  //     sum( m )  =  0 + 1 + 2 + ... + m  .
  //
  // Also set the elements above the diagonal (j > i),
  //
  //      q                         q    
  //     d     cov     ( par )  =  d     cov     ( par )  .
  //      par     (i,j)             par     (j,i)
  //
  // Note that an rvec operation is performed on the elements of
  // the covariance before the partial derivatives are computed,
  //                                       -            -
  //                                      |              |
  //     d     cov( par )  =  d     rvec  |  cov( par )  |  .
  //      par                  par        |              |
  //                                       -            -
  sumI = 0;
  int sumJ;
  for ( i = 1; i < nRow; i++ )
  {
    sumI += i;
    sumJ = 0;

    for ( j = 0; j < i; j++ )
    {
      sumJ += j;

      // Set the rows in the rvec version of the covariance
      // for this element below the diagonal and its equivalent
      // element above the diagonal.
      rowBelow = j * nRow + i;
      rowAbove = i * nRow + j;

      for ( k = 0; k < j; k++ )
      {
        // Add the derivative of this term with respect to this parameter to the derivative element below and above the diagonal.
        cov_parCurr[rowBelow + ( sumI + k ) * nCov_parRow] += parCurr[sumJ + k];
        cov_parCurr[rowAbove + ( sumI + k ) * nCov_parRow] += parCurr[sumJ + k];

        // Add the derivative of this term with respect to this parameter to the derivative element below and above the diagonal.
        cov_parCurr[rowBelow + ( sumJ + k ) * nCov_parRow] += parCurr[sumI + k];
        cov_parCurr[rowAbove + ( sumJ + k ) * nCov_parRow] += parCurr[sumI + k];
      }    
    
      // Add the derivative of this term with respect to this parameter to the derivative element below and above the diagonal.
      cov_parCurr[rowBelow + ( sumI + j ) * nCov_parRow] += exp( parCurr[sumJ + j] );
      cov_parCurr[rowAbove + ( sumI + j ) * nCov_parRow] += exp( parCurr[sumJ + j] );

      // Add the derivative of this term with respect to this parameter to the derivative element below and above the diagonal.
      cov_parCurr[rowBelow + ( sumJ + j ) * nCov_parRow] += parCurr[sumI + j] * exp( parCurr[sumJ + j] );
      cov_parCurr[rowAbove + ( sumJ + j ) * nCov_parRow] += parCurr[sumI + j] * exp( parCurr[sumJ + j] );
    }
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
 *
 * Evaluates the inverse of the covariance matrix at the current
 * parameter value,
 *
 *                                  -1
 *     inv( par )  =  [ cov( par ) ]    .
 * 
 *
 *************************************************************************/

void FullCov::inv( valarray<double>& invOut ) const
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

  // If the current value for the covariance is not valid, then
  // calculate it.
  if ( isCovCurrOk )
  {
    // In order to avoid the work required to create a temporary
    // matrix to use for the argument to the cov() function, 
    // temporarily set the inverse equal to the covariance.
    cov( invCurr );
  }

  // Calculate the inverse.
  invCurr = inverse( covCurr, nRow );


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
 *
 * Evaluates the derivative of the inverse of the covariance matrix
 * at the current parameter value,
 *                                 -                   -
 *                                |                -1   |
 *     d     inv( par )  =  d     |  [ cov( par ) ]     |  .
 *      par                  par  |                     |
 *                                 -                   -
 *
 *************************************************************************/

void FullCov::inv_par( valarray<double>& inv_parOut ) const
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

  // If the current value for the inverse of the covariance is not
  // valid, then calculate it.
  if ( isInvCurrOk )
  {
    // Create a temporary matrix to use for the argument to inv().
    valarray<double> invTemp( nRow * nRow );

    // Set the current value for the inverse.
    inv( invTemp );
  }

  // If the current value for the derivative of the covariance is
  // not valid, then calculate it.
  if ( isCov_parCurrOk )
  {
    // In order to avoid the work required to create a temporary
    // matrix to use for the argument to the cov_par() function, 
    // temporarily set the derivative of the inverse equal to the
    // derivative of the covariance.
    cov_par( inv_parCurr );
  }

  // Calculate the value for the derivative of the inverse of the
  // covariance using Lemma 10 of B. M. Bell, "Approximating the
  // marginal likelihood estimate for models with random parameters",
  // Applied Mathematics and Computation, 119 (2001), pp. 57-73,
  // which states that
  //
  //              -1                   -1                  -1
  //     d     cov  ( par )  =  - [ cov  ( par )  kron  cov  ( par ) ]  d    cov ( par )  .
  //      par                                                            par
  //
  // where kron is the Kronecker product operator.
  inv_parCurr = AkronBtimesC(
    invCurr,     nRow,
    invCurr,     nRow,
    cov_parCurr, nPar );
  inv_parCurr *= -1.0;


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
 *
 * Gets the lower and upper limits for the covariance matrix parameters
 * at the current parameter value.  These limits are for use during the
 * optimization of objective functions that depend on these parameters.
 *
 * This function assumes that the current values for the covariance
 * parameters are approximately equal to the final or true values.
 *
 *************************************************************************/

void FullCov::getParLimits(
  valarray<double>&  parLow,
  valarray<double>&  parUp ) const
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

  int i;
  int k;

  // Set the limits for optimization to constrain the diagonal
  // elements of the covariance matrix as follows,
  //
  //      1      (curr)                              (curr)
  //     ---  cov        <=   cov        <=  100  cov        .
  //     100     (i,i)           (i,i)               (i,i) 
  //
  // These limits for the covariance diagonal elements imply
  // these limits for the parameters that are on the diagonal
  // of the Cholesky factor:
  //
  //              -                -                                     -                -
  //             |                  |                                   |                  |
  //      1      |   1      (curr)  |                            1      |          (curr)  |
  //     --- log |  ---  cov        |  <=   par             <=  --- log |  100  cov        |  ,
  //      2      |  100     (i,i)   |          (sum(i)+i)        2      |          (i,i)   |
  //             |                  |                                   |                  |
  //              -                -                                     -                -
  //
  // and these limits for the parameters that are not on the 
  // diagonal of the Cholesky factor for the covariance (k < i):
  //
  //            ------------------                                  ------------------
  //           /          (curr)                                   /          (curr)  
  //     -    /   100  cov          <=   par             <=  +    /   100  cov          ,
  //        \/            (i,i)             (sum(i)+k)          \/            (i,i)   
  //
  // where
  //
  //     sum( m )  =  0 + 1 + 2 + ... + m  .
  //
  double covDiag;
  int sumI = 0;
  for ( i = 0; i < nRow; i++ )
  {
    sumI += i;

    covDiag = covCurr[i + i * nRow];

    // Set the limits for the parameters not on the diagonal.
    for ( k = 0; k < i; k++ )
    {
      parLow[sumI + k] = - sqrt( covDiag * 100.0 );
      parUp [sumI + k] = + sqrt( covDiag * 100.0 );
    }    

    // Set the limits for the parameter that is on the diagonal.
    parLow[sumI + i] = 0.5 * log( covDiag / 100.0 );
    parUp [sumI + i] = 0.5 * log( covDiag * 100.0 );
  }    

}


/*************************************************************************
 *
 * Function: calcPar
 *
 *
 * Sets parOut equal to the covariance matrix parameters that
 * correspond to the covariance matrix covIn.
 *
 *************************************************************************/

void FullCov::calcPar( 
  const valarray<double>& covIn,
  valarray<double>&       parOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Get the number of rows in the covariance matrix.
  int nCovInRow = static_cast<int>( sqrt( static_cast<double>( covIn.size() ) ) );

  // Set the number of parameters for this covariance matrix.
  int nCovInPar = nCovInRow * ( nCovInRow + 1 ) / 2;

  parOut.resize( nCovInPar );


  //------------------------------------------------------------
  // Set the covariance matrix parameters.
  //------------------------------------------------------------

  int i;
  int k;

  // Calculate the Cholesky factor for the covariance matrix.
  valarray<double> chol( nCovInRow * nCovInRow );
  chol = cholesky( covIn, nCovInRow );

  // Set the covariance matrix parameters using the following
  // parameterization for the Cholesky factor,
  //
  //                  -                                                                    -
  //                 |  exp[ par  ]                                                   0     |
  //                 |          0                                                           |
  //                 |                                                                      |
  //                 |     par       exp[ par  ]                                            |
  //                 |        1              2                                              |
  //                 |                                                                      |
  //    L( par )  =  |     par          par       exp[ par  ]                               |  .
  //                 |        3            4              5                                 |
  //                 |                                                                      |
  //                 |       .            .    .            .                               |
  //                 |       .            .      .            .                             |
  //                 |       .            .        .            .                           |
  //                 |                                                                      |
  //                 |                               par             exp[ par            ]  |
  //                 |                                  nCovInPar-2          nCovInPar-1    |
  //                  -                                                                    -
  //
  int sumI = 0;
  for ( i = 0; i < nCovInRow; i++ )
  {
    sumI += i;

    // Set the parameters from this row excluding the diagonal.
    for ( k = 0; k < i; k++ )
    {
      parOut[sumI + k] = chol[i + k * nCovInRow];
    }    

    // Set the parameter that is on the diagonal.
    parOut[sumI + i] = log( chol[i + i * nCovInRow] );
  }    

}


/*************************************************************************
 *
 * Function: calcCovMinRep
 *
 *
 * Sets covMinRepOut equal to the minimal representation for the
 * covariance matrix.
 *
 * The minimal representation is the elements from the lower triangle
 * of the covariance matrix stored in row major order.
 *
 *************************************************************************/

void FullCov::calcCovMinRep( 
  const valarray<double>& covIn,
  valarray<double>&       covMinRepOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Get the number of rows in the covariance matrix.
  int nCovInRow = static_cast<int>( sqrt( static_cast<double>( covIn.size() ) ) );

  // Set the number of parameters for this covariance matrix.
  int nCovInPar = nCovInRow * ( nCovInRow + 1 ) / 2;

  covMinRepOut.resize( nCovInPar );


  //------------------------------------------------------------
  // Set the minimal representation for the covariance matrix.
  //------------------------------------------------------------

  int i;
  int j;

  // Extract the elements from the lower triangle of the covariance
  // matrix and put them in the array in row major order.
  int sumI = 0;
  for ( i = 0; i < nCovInRow; i++ )
  {
    sumI += i;

    // Get the elements from this row excluding the diagonal.
    for ( j = 0; j < i; j++ )
    {
      covMinRepOut[sumI + j] = covIn[i + j * nCovInRow];
    }    

    // Get the element that is on the diagonal.
    covMinRepOut[sumI + i] = covIn[i + i * nCovInRow];
  }    

}


/*************************************************************************
 *
 * Function: expandCovMinRep
 *
 *
 * Sets covOut equal to the full covariance matrix that corresponds
 * to the minimal representation for the covariance matrix that is
 * contained in covMinRepIn.
 *
 * The minimal representation is the elements from the lower triangle
 * of the covariance matrix stored in row major order.
 *
 *************************************************************************/

void FullCov::expandCovMinRep( 
  const valarray<double>& covMinRepIn,
  valarray<double>&       covOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Get the number of parameters for this covariance matrix.
  int nCovInPar = covMinRepIn.size();

  // Set the number of rows in the covariance matrix.
  int nCovInRow = ( -1 + static_cast<int>( sqrt( 1 + 8 * nCovInPar ) ) ) / 2;

  covOut.resize( nCovInRow * nCovInRow );


  //------------------------------------------------------------
  // Expand the minimal representation for the covariance matrix.
  //------------------------------------------------------------

  int i;
  int j;

  // Set the elements of the covariance matrix.
  int sumI = 0;
  for ( i = 0; i < nCovInRow; i++ )
  {
    sumI += i;

    // Set the elements from this row excluding the diagonal
    // and set the corresponding elements above the diagonal.
    for ( j = 0; j < i; j++ )
    {
      covOut[i + j * nCovInRow] = covMinRepIn[sumI + j];
      covOut[j + i * nCovInRow] = covMinRepIn[sumI + j];
    }    

    // Set the element that is on the diagonal.
    covOut[i + i * nCovInRow] = covMinRepIn[sumI + i];
  }    

}

