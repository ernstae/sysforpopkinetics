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
 * @file: FullCovBase.cpp
 *
 *
 * Implements FullCovBase class.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "FullCovBase.h"

// SPK library header files.
#include <spk/AkronBtimesC.h>
#include <spk/doubleToScalar.h>
#include <spk/doubleToScalarArray.h>
#include <spk/inverse.h>
#include <spk/cholesky.h>
#include <spk/scalarToDouble.h>
#include <spk/scalarToDoubleArray.h>
#include <spk/SpkValarray.h>

// CppAD header files.
#include <CppAD/CppAD.h>

// Standard library header files.
#include <cassert>
#include <cmath>

using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: FullCovBase
 *
 *//**
 * Constructs a full covariance matrix with nRowIn rows and columns.
 *//*
 *************************************************************************/

template<class Scalar>
FullCovBase<Scalar>::FullCovBase( int nRowIn )
  :
  Cov<Scalar>( nRowIn, nRowIn * ( nRowIn + 1 ) / 2 )
{
}

template<class Scalar>
FullCovBase<Scalar>::FullCovBase( int nRowIn, const valarray<bool>& minRepFixedIn )
  :
  Cov<Scalar>( nRowIn, nRowIn * ( nRowIn + 1 ) / 2, minRepFixedIn )
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

template<class Scalar>
void FullCovBase<Scalar>::cov( SPK_VA::valarray<Scalar>& covOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  covOut.resize( Cov<Scalar>::nRow * Cov<Scalar>::nRow );


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  if ( Cov<Scalar>::isCovCurrOk )
  {
    covOut = Cov<Scalar>::covCurr;
    Cov<Scalar>::usedCachedCov = true;

    return;
  }
  else
  {
    Cov<Scalar>::usedCachedCov = false;
  }


  //------------------------------------------------------------
  // Evaluate the covariance matrix.
  //------------------------------------------------------------

  int i;
  int j;
  int k;

  // Create a matrix that has only zeroes.
  Cov<Scalar>::covCurr = 0.0;

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
  for ( i = 0; i < Cov<Scalar>::nRow; i++ )
  {
    sumI += i;

    covIndex = i + i * Cov<Scalar>::nRow;

    for ( k = 0; k < i; k++ )
    {
      Cov<Scalar>::covCurr[covIndex] += Cov<Scalar>::parCurr[sumI + k] * Cov<Scalar>::parCurr[sumI + k];
    }

    Cov<Scalar>::covCurr[covIndex] += exp( 2.0 * Cov<Scalar>::parCurr[sumI + i] );
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
  for ( i = 1; i < Cov<Scalar>::nRow; i++ )
  {
    sumI += i;
    sumJ = 0;

    for ( j = 0; j < i; j++ )
    {
      sumJ += j;

      covIndex = i + j * Cov<Scalar>::nRow;
    
      for ( k = 0; k < j; k++ )
      {
        Cov<Scalar>::covCurr[covIndex] += Cov<Scalar>::parCurr[sumI + k] * Cov<Scalar>::parCurr[sumJ + k];
      }
    
      Cov<Scalar>::covCurr[covIndex] += Cov<Scalar>::parCurr[sumI + j] * exp( Cov<Scalar>::parCurr[sumJ + j] );
    }
  }

  // Set the elements above the diagonal (j > i),
  //
  //     cov     ( par )  =  cov     ( par )  .
  //        (i,j)               (j,i)
  //
  for ( i = 0; i < Cov<Scalar>::nRow - 1; i++ )
  {
    for ( j = i + 1; j < Cov<Scalar>::nRow; j++ )
    {
      Cov<Scalar>::covCurr[i + j * Cov<Scalar>::nRow] = Cov<Scalar>::covCurr[j + i * Cov<Scalar>::nRow];
    }
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  Cov<Scalar>::isCovCurrOk = true;
  covOut = Cov<Scalar>::covCurr;
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

template<class Scalar>
void FullCovBase<Scalar>::cov_par( SPK_VA::valarray<double>& cov_parOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  cov_parOut.resize( Cov<Scalar>::nRow * Cov<Scalar>::nRow * Cov<Scalar>::nPar );


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  if ( Cov<Scalar>::isCov_parCurrOk )
  {
    cov_parOut = Cov<Scalar>::cov_parCurr;
    Cov<Scalar>::usedCachedCov_par = true;

    return;
  }
  else
  {
    Cov<Scalar>::usedCachedCov_par = false;
  }


  //------------------------------------------------------------
  // Evaluate the derivative of the covariance matrix.
  //------------------------------------------------------------

  // Create a double version of the current parameter.
  valarray<double> parCurrDouble( Cov<Scalar>::nPar );
  scalarToDoubleArray( Cov<Scalar>::parCurr, parCurrDouble );

  int nCov_parRow( Cov<Scalar>::nRow * Cov<Scalar>::nRow );

  int i;
  int j;
  int k;
  int row;
  int rowAbove;
  int rowBelow;

  // Create a matrix that has only zeroes.
  Cov<Scalar>::cov_parCurr = 0.0;

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
  for ( i = 0; i < Cov<Scalar>::nRow; i++ )
  {
    // Set the row in the rvec version of the covariance.
    row = i * Cov<Scalar>::nRow + i;

    sumI += i;

    for ( k = 0; k < i; k++ )
    {
      // Add the derivative of this term with respect to this parameter.
      Cov<Scalar>::cov_parCurr[row + ( sumI + k ) * nCov_parRow] += 2.0 * parCurrDouble[sumI + k];
    }

    // Add the derivative of this term with respect to this parameter.
    Cov<Scalar>::cov_parCurr[row + ( sumI + i ) * nCov_parRow] += 2.0 * exp( 2.0 * parCurrDouble[sumI + i] );
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
  for ( i = 1; i < Cov<Scalar>::nRow; i++ )
  {
    sumI += i;
    sumJ = 0;

    for ( j = 0; j < i; j++ )
    {
      sumJ += j;

      // Set the rows in the rvec version of the covariance
      // for this element below the diagonal and its equivalent
      // element above the diagonal.
      rowBelow = j * Cov<Scalar>::nRow + i;
      rowAbove = i * Cov<Scalar>::nRow + j;

      for ( k = 0; k < j; k++ )
      {
        // Add the derivative of this term with respect to this parameter to the derivative element below and above the diagonal.
        Cov<Scalar>::cov_parCurr[rowBelow + ( sumI + k ) * nCov_parRow] += parCurrDouble[sumJ + k];
        Cov<Scalar>::cov_parCurr[rowAbove + ( sumI + k ) * nCov_parRow] += parCurrDouble[sumJ + k];

        // Add the derivative of this term with respect to this parameter to the derivative element below and above the diagonal.
        Cov<Scalar>::cov_parCurr[rowBelow + ( sumJ + k ) * nCov_parRow] += parCurrDouble[sumI + k];
        Cov<Scalar>::cov_parCurr[rowAbove + ( sumJ + k ) * nCov_parRow] += parCurrDouble[sumI + k];
      }
    
      // Add the derivative of this term with respect to this parameter to the derivative element below and above the diagonal.
      Cov<Scalar>::cov_parCurr[rowBelow + ( sumI + j ) * nCov_parRow] += exp( parCurrDouble[sumJ + j] );
      Cov<Scalar>::cov_parCurr[rowAbove + ( sumI + j ) * nCov_parRow] += exp( parCurrDouble[sumJ + j] );

      // Add the derivative of this term with respect to this parameter to the derivative element below and above the diagonal.
      Cov<Scalar>::cov_parCurr[rowBelow + ( sumJ + j ) * nCov_parRow] += parCurrDouble[sumI + j] * exp( parCurrDouble[sumJ + j] );
      Cov<Scalar>::cov_parCurr[rowAbove + ( sumJ + j ) * nCov_parRow] += parCurrDouble[sumI + j] * exp( parCurrDouble[sumJ + j] );
    }
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  Cov<Scalar>::isCov_parCurrOk = true;
  cov_parOut = Cov<Scalar>::cov_parCurr;
}


/*************************************************************************
 *
 * Function: inv
 *
 *//**
 * Evaluates the inverse of the covariance matrix at the current
 * parameter value
 *
 * In particular, this function sets invOut equal to
 * \f[
 *     \mbox{cov}^{-1}(\mbox{par}) .
 * \f]
 *//*
 *************************************************************************/

template<class Scalar>
void FullCovBase<Scalar>::inv( SPK_VA::valarray<Scalar>& invOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  invOut.resize( Cov<Scalar>::nRow * Cov<Scalar>::nRow );


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  if ( Cov<Scalar>::isInvCurrOk )
  {
    invOut = Cov<Scalar>::invCurr;
    Cov<Scalar>::usedCachedInv = true;

    return;
  }
  else
  {
    Cov<Scalar>::usedCachedInv = false;
  }


  //------------------------------------------------------------
  // Evaluate the inverse of the covariance matrix.
  //------------------------------------------------------------

  int i;

  // If the current value for the covariance is not valid, then
  // calculate it.
  if ( !Cov<Scalar>::isCovCurrOk )
  {
    // In order to avoid the work required to create a temporary
    // matrix to use for the argument to the cov() function, 
    // temporarily set the inverse equal to the covariance.
    cov( Cov<Scalar>::invCurr );
  }

  // Create an identity matrix.
  valarray<Scalar> identity( Cov<Scalar>::nRow * Cov<Scalar>::nRow );
  identity = Scalar( 0 );
  for ( i = 0; i < Cov<Scalar>::nRow; i++ )
  {
    identity[i + i * Cov<Scalar>::nRow] = 1;
  }

  // Calculate the inverse by solving the equation
  //
  //     A  *  X  =  1  ,
  //
  // where
  //
  //     A  = cov
  //
  // and
  //             -1
  //     X  = cov
  //
  int signdet;
  Scalar logdet;
  signdet = CppAD::LuSolve(
    Cov<Scalar>::nRow,
    Cov<Scalar>::nRow,
    Cov<Scalar>::covCurr,
    identity,
    Cov<Scalar>::invCurr,
    logdet );


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  Cov<Scalar>::isInvCurrOk = true;
  invOut = Cov<Scalar>::invCurr;
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

template<class Scalar>
void FullCovBase<Scalar>::inv_par( SPK_VA::valarray<double>& inv_parOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  inv_parOut.resize( Cov<Scalar>::nRow * Cov<Scalar>::nRow * Cov<Scalar>::nPar );


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  if ( Cov<Scalar>::isInv_parCurrOk )
  {
    inv_parOut = Cov<Scalar>::inv_parCurr;
    Cov<Scalar>::usedCachedInv_par = true;

    return;
  }
  else
  {
    Cov<Scalar>::usedCachedInv_par = false;
  }


  //------------------------------------------------------------
  // Evaluate the derivative of the inverse of the covariance matrix.
  //------------------------------------------------------------

  // If the current value for the inverse of the covariance is not
  // valid, then calculate it.
  if ( !Cov<Scalar>::isInvCurrOk )
  {
    // Create a temporary matrix to use for the argument to inv().
    valarray<Scalar> invTemp( Cov<Scalar>::nRow * Cov<Scalar>::nRow );

    // Set the current value for the inverse.
    inv( invTemp );
  }

  // Get a double version of the inverse.
  valarray<double> invCurrDouble( Cov<Scalar>::nRow * Cov<Scalar>::nRow );
  scalarToDoubleArray( Cov<Scalar>::invCurr, invCurrDouble );

  // If the current value for the derivative of the covariance is
  // not valid, then calculate it.
  if ( !Cov<Scalar>::isCov_parCurrOk )
  {
    // In order to avoid the work required to create a temporary
    // matrix to use for the argument to the cov_par() function, 
    // temporarily set the derivative of the inverse equal to the
    // derivative of the covariance.
    cov_par( Cov<Scalar>::inv_parCurr );
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
  Cov<Scalar>::inv_parCurr = AkronBtimesC(
    invCurrDouble,     Cov<Scalar>::nRow,
    invCurrDouble,     Cov<Scalar>::nRow,
    Cov<Scalar>::cov_parCurr, Cov<Scalar>::nPar );
  Cov<Scalar>::inv_parCurr *= -1.0;


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  Cov<Scalar>::isInv_parCurrOk = true;
  inv_parOut = Cov<Scalar>::inv_parCurr;
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

template<class Scalar>
void FullCovBase<Scalar>::getParLimits(
  SPK_VA::valarray<double>&  parLow,
  SPK_VA::valarray<double>&  parUp ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  parLow.resize( Cov<Scalar>::nPar );
  parUp .resize( Cov<Scalar>::nPar );


  //------------------------------------------------------------
  // Evaluate the current value for the covariance matrix. 
  //------------------------------------------------------------

  // If the covariance has not been evaluated at the current
  // value for the parameters, then evaluate it.
  if ( !Cov<Scalar>::isCovCurrOk )
  {
    valarray<Scalar> tempCov( Cov<Scalar>::nRow * Cov<Scalar>::nRow );
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
  //
  // For fixed covariance elements, the upper and lower bounds are set to the par value.
  //
  double covDiag;
  double parCurrValue;
  int sumI = 0;
  for ( i = 0; i < Cov<Scalar>::nRow; i++ )
  {
    sumI += i;

    scalarToDouble( Cov<Scalar>::covCurr[i + i * Cov<Scalar>::nRow], covDiag );

    // CHECK if Cov<Scalar>::parCurr has been filled in

    // Set the limits for the parameters not on the diagonal.
    for ( k = 0; k < i; k++ )
    {
      if( Cov<Scalar>::parFixed[sumI + k] )
      {
        scalarToDouble( Cov<Scalar>::parCurr[sumI + k], parCurrValue );

        parLow[sumI + k] = parCurrValue;
        parUp [sumI + k] = parCurrValue;
      }
      else
      {
        parLow[sumI + k] = - sqrt( covDiag * 100.0 );
        parUp [sumI + k] = + sqrt( covDiag * 100.0 );
      }
    }

    // Set the limits for the parameter that is on the diagonal.
    if( Cov<Scalar>::parFixed[sumI + i] )
    {
      scalarToDouble( Cov<Scalar>::parCurr[sumI + i], parCurrValue );

      parLow[sumI + i] = parCurrValue;
      parUp [sumI + i] = parCurrValue;
    }
    else
    {
      parLow[sumI + i] = 0.5 * log( covDiag / 100.0 );
      parUp [sumI + i] = 0.5 * log( covDiag * 100.0 );
    }

  }
}


/*************************************************************************
 *
 * Function: calcPar
 *
 *//**
 * Sets parOut equal to the covariance matrix parameters that
 * correspond to the covariance matrix covIn.
 *
 * Note: the calculation of the value for parOut is not performed
 * using Scalar type variables, and so its calculation cannot be
 * taped.  The reason for this is that the calculation of the Cholesky
 * factor of the covariance matrix is done using a function that only
 * works for double values.
 *//*
 *************************************************************************/

template<class Scalar>
void FullCovBase<Scalar>::calcPar( 
  const SPK_VA::valarray<Scalar>& covIn,
  SPK_VA::valarray<Scalar>&       parOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  int nCovInRow = static_cast<int>( sqrt( static_cast<double>( covIn.size() ) ) );
  assert( covIn.size() == nCovInRow * nCovInRow );

  // Get a double version of the covariance matrix.
  SPK_VA::valarray<double> covInDouble( nCovInRow * nCovInRow );
  scalarToDoubleArray( covIn, covInDouble );

  // Set the number of parameters for this covariance matrix.
  int nCovInPar = nCovInRow * ( nCovInRow + 1 ) / 2;

  // Get a double version of the output parameters.
  SPK_VA::valarray<double> parOutDouble( nCovInPar );

  parOut.resize( nCovInPar );


  //------------------------------------------------------------
  // Set the covariance matrix parameters.
  //------------------------------------------------------------

  int i;
  int k;

  // Calculate the Cholesky factor for the covariance matrix.
  SPK_VA::valarray<double> chol( nCovInRow * nCovInRow );
  chol = cholesky( covInDouble, nCovInRow );

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
      parOutDouble[sumI + k] = chol[i + k * nCovInRow];
    }

    // Set the parameter that is on the diagonal.
    parOutDouble[sumI + i] = log( chol[i + i * nCovInRow] );
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Get a Scalar version of the parameters.
  doubleToScalarArray( parOutDouble, parOut );

}


/*************************************************************************
 *
 * Function: calcCovMinRep
 *
 *//**
 * Sets covMinRepOut equal to the minimal representation for the
 * covariance matrix covIn.
 *
 * The minimal representation is the elements from the lower triangle
 * of the covariance matrix stored in column major order.
 *//*
 *************************************************************************/

template<class Scalar>
void FullCovBase<Scalar>::calcCovMinRep( 
  const SPK_VA::valarray<Scalar>& covIn,
  SPK_VA::valarray<Scalar>&       covMinRepOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  int nCovInRow = static_cast<int>( sqrt( static_cast<double>( covIn.size() ) ) );
  assert( covIn.size() == nCovInRow * nCovInRow );

  // Set the number of parameters for this covariance matrix.
  int nCovInPar = nCovInRow * ( nCovInRow + 1 ) / 2;

  covMinRepOut.resize( nCovInPar );


  //------------------------------------------------------------
  // Set the minimal representation for the covariance matrix.
  //------------------------------------------------------------

  int i;
  int j;

  // Extract the elements from the lower triangle of the covariance
  // matrix and put them in the array in column major order.
  int sumI = 0;
  for ( j = 0; j < nCovInRow; j++ )
  {
    // Get the elements from this column including the diagonal.
    for ( i = j; i < nCovInRow; i++ )
    {
      covMinRepOut[sumI++] = covIn[i + j * nCovInRow];
    }
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
 * The minimal representation is the elements from the lower triangle
 * of the covariance matrix stored in column major order.
 *//*
 *************************************************************************/

template<class Scalar>
void FullCovBase<Scalar>::calcCovMinRep_par( 
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
  int nCovInRow = static_cast<int>( 
    sqrt( static_cast<double>( cov_parIn.size() / nCovInPar ) ) );
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
  int j;
  int k;

  // Extract the derivatives of the elements from the lower triangle
  // of the covariance matrix in column major order.
  int sumI;
  for ( k = 0; k < nCovInPar; k++ )
  {
    sumI = 0;
    for ( j = 0; j < nCovInRow; j++ )
    {
      // Get the elements from this column including the diagonal.
      for ( i = j; i < nCovInRow; i++ )
      {
        covMinRep_parOut[( sumI++ )            + k * nCovMinRep_parOutRow] = 
          cov_parIn     [( i + j * nCovInRow ) + k * nCov_parInRow];
      }
    }
  }

}


/*************************************************************************
 *
 * Function: calcCovMinRepMask
 *
 *//**
 * Sets covMinRepMaskOut equal to the minimal representation ordered
 * mask that corresponds to the covariance parameter ordered mask
 * parMaskIn.
 *
 * The minimal representation is the elements from the lower triangle
 * of the covariance matrix stored in column major order.
 *//*
 *************************************************************************/

template<class Scalar>
void FullCovBase<Scalar>::calcCovMinRepMask( 
  const SPK_VA::valarray<bool>& parMaskIn,
  SPK_VA::valarray<bool>&       covMinRepMaskOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Set the number of parameters for the covariance matrix.
  int nMaskInPar = parMaskIn.size();

  // Set the number of rows in the covariance matrix that corresponds
  // to the parameter ordered mask.
  int nCovInRow = ( -1 + static_cast<int>( sqrt( 
    static_cast<double>( 1 + 8 * nMaskInPar ) ) ) ) / 2;
  assert( nMaskInPar == nCovInRow * ( nCovInRow + 1 ) / 2 );

  covMinRepMaskOut.resize( nMaskInPar );


  //------------------------------------------------------------
  // Set the mask for the covariance matrix minimal representation.
  //------------------------------------------------------------

  int i;
  int j;
  int k;

  // Set the elements of the minimal representation ordered mask.
  //
  // Note that the elements of the lower triangle of the covariance
  // matrix that make up the minimal representation,
  //
  //                -                                             -
  //               |     cov                                       |
  //               |        (0, 0)                                 |
  //               |                                               |
  //               |     cov          cov                          |
  //               |        (1, 0)       (1, 1)                    |
  //               |                                               |
  //    MinRep  =  |     cov          cov          cov             |  ,
  //               |        (2, 0)       (2, 1)       (2, 2)       |
  //               |                                               |
  //               |       .            .    .             .       |
  //               |       .            .      .             .     |
  //               |       .            .        .             .   |
  //               |                                               |
  //                -                                             -
  //
  // are stored in column major order, but the elements of the
  // covariance parameters that make up the Cholesky factor,
  //
  //                  -                                                                      -
  //                 |  exp[ par  ]                                                     0     |
  //                 |          0                                                             |
  //                 |                                                                        |
  //                 |     par       exp[ par  ]                                              |
  //                 |        1              2                                                |
  //                 |                                                                        |
  //    L( par )  =  |     par          par       exp[ par  ]                                 |  ,
  //                 |        3            4              5                                   |
  //                 |                                                                        |
  //                 |       .            .    .             .                                |
  //                 |       .            .      .             .                              |
  //                 |       .            .        .             .                            |
  //                 |                                                                        |
  //                 |                               par              exp[ par             ]  |
  //                 |                                  nMaskInPar-2          nMaskInPar-1    |
  //                  -                                                                      -
  //
  // are stored in row major order.
  int sumI = 0;
  int minRepIndex = 0;
  for ( i = 0; i < nCovInRow; i++ )
  {
    sumI += i;

    for ( j = 0; j <= i; j++ )
    {
      // Add the elements in the minimal representation columns that
      // come before this column.
      minRepIndex = 0;
      for ( k = 0; k < j; k++ )
      {
        minRepIndex += nCovInRow - k;
      }

      // Add in this elements position in this column of the minimal
      // representation.
      minRepIndex += i - j;

      // Set the element of the minimal representation ordered mask
      // that corresponds to
      //
      //     cov        .
      //        (i, j)
      //
      // In particular, set
      //
      //    MinRepMask               =  parMask           ,
      //              (minRepIndex)            (sum(i)+j)
      //
      // where
      //
      //     sum( m )  =  0 + 1 + 2 + ... + m  .
      //
      covMinRepMaskOut[minRepIndex] = parMaskIn[sumI + j];
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
 * The minimal representation is the elements from the lower triangle
 * of the covariance matrix stored in column major order.
 *//*
 *************************************************************************/

template<class Scalar>
void FullCovBase<Scalar>::expandCovMinRep( 
  const SPK_VA::valarray<double>& covMinRepIn,
  SPK_VA::valarray<Scalar>&       covOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Set the number of parameters for this covariance matrix.
  int nCovInPar = covMinRepIn.size();

  // Set the number of rows in the covariance matrix.
  int nCovInRow = ( -1 + static_cast<int>( sqrt( 1 + 8 * nCovInPar ) ) ) / 2;
  assert( nCovInPar == nCovInRow * ( nCovInRow + 1 ) / 2 );

  covOut.resize( nCovInRow * nCovInRow );


  //------------------------------------------------------------
  // Expand the minimal representation for the covariance matrix.
  //------------------------------------------------------------

  int i;
  int j;

  // Set the elements of the covariance matrix.
  int sumI = 0;
  for ( j = 0; j < nCovInRow; j++ )
  {
    // Set the element that is on the diagonal.
    covOut[j + j * nCovInRow] = covMinRepIn[sumI++];

    // Set the elements from this column below the diagonal
    // and set the corresponding elements above the diagonal.
    for ( i = j + 1; i < nCovInRow; i++ )
    {
      covOut[i + j * nCovInRow] = covMinRepIn[sumI];
      covOut[j + i * nCovInRow] = covMinRepIn[sumI++];
    }
  }

}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

// Declare double versions of these functions.
template FullCovBase<double>::FullCovBase( int nRowIn );
template FullCovBase<double>::FullCovBase( int nRowIn,  const SPK_VA::valarray<bool>& minRepFixedIn );

template void FullCovBase<double>::cov    ( SPK_VA::valarray<double>& covOut     ) const;
template void FullCovBase<double>::cov_par( SPK_VA::valarray<double>& cov_parOut ) const;

template void FullCovBase<double>::inv    ( SPK_VA::valarray<double>& invOut     ) const;
template void FullCovBase<double>::inv_par( SPK_VA::valarray<double>& inv_parOut ) const;

template void FullCovBase<double>::getParLimits(
    SPK_VA::valarray<double>&  parLow,
    SPK_VA::valarray<double>&  parUp ) const;

template void FullCovBase<double>::calcPar( 
    const SPK_VA::valarray<double>& covIn,
    SPK_VA::valarray<double>&       parOut ) const;

template void FullCovBase<double>::calcCovMinRep( 
    const SPK_VA::valarray<double>& covIn,
    SPK_VA::valarray<double>&       covMinRepOut ) const;

template void FullCovBase<double>::calcCovMinRep_par( 
    const SPK_VA::valarray<double>& cov_parIn,
    int                             nCov_parInCol,
    SPK_VA::valarray<double>&       covMinRep_parOut ) const;

template void FullCovBase<double>::calcCovMinRepMask( 
    const SPK_VA::valarray<bool>& parMaskIn,
    SPK_VA::valarray<bool>&       covMinRepMaskOut ) const;

template void FullCovBase<double>::expandCovMinRep( 
    const SPK_VA::valarray<double>& covMinRepIn,
    SPK_VA::valarray<double>&       covOut ) const;


// Declare CppAD::AD<double> versions of these functions.
template FullCovBase< CppAD::AD<double> >::FullCovBase( int nRowIn );
template FullCovBase< CppAD::AD<double> >::FullCovBase( int nRowIn,  const SPK_VA::valarray<bool>& minRepFixedIn );

template void FullCovBase< CppAD::AD<double> >::cov    ( SPK_VA::valarray< CppAD::AD<double> >& covOut     ) const;
template void FullCovBase< CppAD::AD<double> >::cov_par( SPK_VA::valarray<double>& cov_parOut ) const;

template void FullCovBase< CppAD::AD<double> >::inv    ( SPK_VA::valarray< CppAD::AD<double> >& invOut     ) const;
template void FullCovBase< CppAD::AD<double> >::inv_par( SPK_VA::valarray<double>& inv_parOut ) const;

template void FullCovBase< CppAD::AD<double> >::getParLimits(
    SPK_VA::valarray<double>&  parLow,
    SPK_VA::valarray<double>&  parUp ) const;

template void FullCovBase< CppAD::AD<double> >::calcPar( 
    const SPK_VA::valarray< CppAD::AD<double> >& covIn,
    SPK_VA::valarray< CppAD::AD<double> >&       parOut ) const;

template void FullCovBase< CppAD::AD<double> >::calcCovMinRep( 
    const SPK_VA::valarray< CppAD::AD<double> >& covIn,
    SPK_VA::valarray< CppAD::AD<double> >&       covMinRepOut ) const;

template void FullCovBase< CppAD::AD<double> >::calcCovMinRep_par( 
    const SPK_VA::valarray<double>& cov_parIn,
    int                             nCov_parInCol,
    SPK_VA::valarray<double>&       covMinRep_parOut ) const;

template void FullCovBase< CppAD::AD<double> >::calcCovMinRepMask( 
    const SPK_VA::valarray<bool>& parMaskIn,
    SPK_VA::valarray<bool>&       covMinRepMaskOut ) const;

template void FullCovBase< CppAD::AD<double> >::expandCovMinRep( 
    const SPK_VA::valarray<double>& covMinRepIn,
    SPK_VA::valarray< CppAD::AD<double> >&       covOut ) const;

