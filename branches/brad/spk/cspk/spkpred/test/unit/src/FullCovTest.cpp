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
 * File: FullCovTest.cpp
 *
 *
 * Unit test for the class FullCov.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK Pred test suite header files.
#include "FullCovTest.h"
#include "compareToKnown.h"

// SPK Pred library header files.
#include "../../../spkpred/FullCov.h"

// SPK library header files.
#include <spk/AkronItimesC.h>
#include <spk/identity.h>
#include <spk/IkronBtimesC.h>
#include <spk/inverse.h>
#include <spk/multiply.h>
#include <spk/SpkValarray.h>
#include <spk/transpose.h>
#include <spk/transposeDerivative.h>

// CppUnit framework header files.
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

// Standard library header files.
#include <iomanip>
#include <cassert>
#include <string>
#include <cmath>
#include <iostream>

using namespace CppUnit;
using std::string;
using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: setUp
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void FullCovTest::setUp()
{
    // initializations
}


/*************************************************************************
 *
 * Function: tearDown
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void FullCovTest::tearDown()
{
    // clean up
}


/*************************************************************************
 *
 * Function: suite
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

Test* FullCovTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "FullCovTest" );

  suiteOfTests->addTest(new TestCaller<FullCovTest>(
    "oneByOneCovTest", 
    &FullCovTest::oneByOneCovTest ));

  suiteOfTests->addTest(new TestCaller<FullCovTest>(
    "twoByTwoCovTest", 
    &FullCovTest::twoByTwoCovTest ));

  suiteOfTests->addTest(new TestCaller<FullCovTest>(
    "threeByThreeCovTest", 
    &FullCovTest::threeByThreeCovTest ));

  suiteOfTests->addTest(new TestCaller<FullCovTest>(
    "isCachingProperlyTest", 
    &FullCovTest::isCachingProperlyTest ));

 suiteOfTests->addTest(new TestCaller<FullCovTest>(
    "FixedTwoByTwoCovTest", 
    &FullCovTest::fixedTwoByTwoCovTest ));

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: oneByOneCovTest
 *
 *
 * The goal of this test is to check that the covariance class works
 * for the case of a one-by-one covariance matrix.
 *
 *************************************************************************/

void FullCovTest::oneByOneCovTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i;


  //------------------------------------------------------------
  // Prepare the covariance matrix.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  const int nRow = 1;

  // Construct the covariance matrix.
  FullCov omega( nRow );

  // Get the number of parameters.
  int nPar = omega.getNPar();
  assert( nPar == 1 );

  // Initialize the current value for the parameters.
  valarray<double> par( nPar );
  par[0] = 0.1;

  // Set the current value for the parameters.
  omega.setPar( par );

  // Initialize the current value for the parameter mask.
  valarray<bool> parMask( nPar );
  parMask[0] = true;


  //------------------------------------------------------------
  // Calculate various quantities for the test.
  //------------------------------------------------------------

  valarray<double> omegaCov    ( nRow * nRow );
  valarray<double> omegaCov_par( nRow * nRow * nPar );
  valarray<double> omegaInv    ( nRow * nRow );
  valarray<double> omegaInv_par( nRow * nRow * nPar );

  valarray<double> parLow( nPar );
  valarray<double> parUp ( nPar );

  valarray<double> omegaAtParLow    ( nRow * nRow );
  valarray<double> omegaDiagAtParLow( nRow );
  valarray<double> omegaAtParUp     ( nRow * nRow );
  valarray<double> omegaDiagAtParUp ( nRow );

  valarray<double> omegaMinRep    ( nPar );
  valarray<double> omegaMinRep_par( nPar * nPar );
  valarray<bool>   omegaMinRepMask( nPar );

  valarray<double> omegaCovTimesInv( nRow * nRow );

  // Calculate the covariance matrix, its derivative, its inverse,
  // and the derivative of its inverse.
  omega.cov    ( omegaCov );
  omega.cov_par( omegaCov_par );
  omega.inv    ( omegaInv );
  omega.inv_par( omegaInv_par );

  // Get the limits for the covariance matrix parameters.
  omega.getParLimits( parLow, parUp );

  // Evaluate the covariance matrix at the upper and lower limits
  // for the parameters.
  omega.setPar( parLow );
  omega.cov( omegaAtParLow );
  omega.setPar( parUp );
  omega.cov( omegaAtParUp );

  // Get the covariance matrix diagonals at the limits.
  omegaDiagAtParLow[0] = omegaAtParLow[0];
  omegaDiagAtParUp [0] = omegaAtParUp [0];

  // Calculate the minimal representation for the covariance matrix
  // and its derivative.
  omega.calcCovMinRep    ( omegaCov,           omegaMinRep );
  omega.calcCovMinRep_par( omegaCov_par, nPar, omegaMinRep_par );

  // Calculate the mask for the minimal representation for the
  // covariance matrix.
  omega.calcCovMinRepMask( parMask, omegaMinRepMask );

  // Multiply the covariance matrix and its inverse.
  omegaCovTimesInv = multiply( omegaCov, nRow, omegaInv, nRow );


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  valarray<double> omegaCovKnown    ( nRow * nRow );
  valarray<double> omegaCov_parKnown( nRow * nRow * nPar );
  valarray<double> omegaInvKnown    ( nRow * nRow );
  valarray<double> omegaInv_parKnown( nRow * nRow * nPar );

  valarray<double> omegaDiagAtParLowKnown( nRow );
  valarray<double> omegaDiagAtParUpKnown ( nRow );

  valarray<double> omegaMinRepKnown    ( nPar );
  valarray<double> omegaMinRep_parKnown( nPar * nPar );
  valarray<bool>   omegaMinRepMaskKnown( nPar );

  valarray<double> omegaCovTimesInvKnown( nRow * nRow );

  // The diagonal elements should be 
  //
  //    cov      ( par )  =  exp[ 2 par  ]  .
  //       (i, i)                      i   
  //
  omegaCovKnown[0]     = exp( 2.0 * par[0] );
  omegaCov_parKnown[0] = 2.0 * exp( 2.0 * par[0] );
  omegaInvKnown[0]     = exp( -2.0 * par[0] );
  omegaInv_parKnown[0] = -2.0 * exp( -2.0 * par[0] );

  // The diagonal elements of the covariance matrix should be
  // constrained as follows,
  //
  //      1      (curr)                              (curr)
  //     ---  cov        <=   cov        <=  100  cov        .
  //     100     (i,i)           (i,i)               (i,i) 
  //
  omegaDiagAtParLowKnown[0] = omegaCovKnown[0] / 100.0;
  omegaDiagAtParUpKnown[0]  = omegaCovKnown[0] * 100.0;

  // Set the known value for the minimal representation for the
  // covariance matrix and its derivative.
  omegaMinRepKnown[0]     = omegaCovKnown[0];
  omegaMinRep_parKnown[0] = omegaCov_parKnown[0];

  // Set the known value for the mask for the minimal representation
  // for the covariance matrix.
  omegaMinRepMaskKnown = parMask;

  // The covariance matrix multiplied by its inverse should be
  // equal to the identity matrix.
  identity( nRow, omegaCovTimesInvKnown );


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  compareToKnown( 
    omegaCov,
    omegaCovKnown,
    "omegaCov",
    tol );

  compareToKnown( 
    omegaCov_par,
    omegaCov_parKnown,
    "omegaCov_par",
    tol );

  compareToKnown( 
    omegaInv,
    omegaInvKnown,
    "omegaInv",
    tol );

  compareToKnown( 
    omegaInv_par,
    omegaInv_parKnown,
    "omegaInv_par",
    tol );

  compareToKnown( 
    omegaDiagAtParLow,
    omegaDiagAtParLowKnown,
    "omega diagonals at parLow",
    tol );

  compareToKnown( 
    omegaDiagAtParUp,
    omegaDiagAtParUpKnown,
    "omega diagonals at parUp",
    tol );

  compareToKnown( 
    omegaMinRep,
    omegaMinRepKnown,
    "omegaMinRep",
    tol );

  compareToKnown( 
    omegaMinRep_par,
    omegaMinRep_parKnown,
    "omegaMinRep_par",
    tol );

  compareToKnown( 
    omegaMinRepMask,
    omegaMinRepMaskKnown,
    "omegaMinRepMask" );

  compareToKnown( 
    omegaCovTimesInv,
    omegaCovTimesInvKnown,
    "omegaCov times omegaInv",
    tol );

}


/*************************************************************************
 *
 * Function: twoByTwoCovTest
 *
 *
 * The goal of this test is to check that the covariance class works
 * for the case of a two-by-two covariance matrix.
 *
 *************************************************************************/

void FullCovTest::twoByTwoCovTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i;
  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the covariance matrix.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  const int nRow = 2;

  // Set the number of rows in the derivative of the covariance matrix.
  int nCov_parRow = nRow * nRow;

  // Construct the covariance matrix.
  FullCov omega( nRow );

  // Get the number of parameters.
  int nPar = omega.getNPar();
  assert( nPar == 3 );

  // Initialize the current value for the parameters.
  valarray<double> par( nPar );
  par[0] = 0.1;
  par[1] = 0.005;
  par[2] = -3.0;

  // Set the current value for the parameters.
  omega.setPar( par );

  //------------------------------------------------------------
  // Calculate various quantities for the test.
  //------------------------------------------------------------

  valarray<double> omegaCov    ( nRow * nRow );
  valarray<double> omegaCov_par( nRow * nRow * nPar );
  valarray<double> omegaInv    ( nRow * nRow );
  valarray<double> omegaInv_par( nRow * nRow * nPar );

  valarray<double> parLow( nPar );
  valarray<double> parUp ( nPar );

  valarray<double> omegaAtParLow    ( nRow * nRow );
  valarray<double> omegaDiagAtParLow( nRow );
  valarray<double> omegaAtParUp     ( nRow * nRow );
  valarray<double> omegaDiagAtParUp ( nRow );

  valarray<double> omegaMinRep    ( nPar );
  valarray<double> omegaMinRep_par( nPar * nPar );

  valarray<double> omegaCovTimesInv( nRow * nRow );

  // Calculate the covariance matrix, its derivative, its inverse,
  // and the derivative of its inverse.
  omega.cov    ( omegaCov );
  omega.cov_par( omegaCov_par );
  omega.inv    ( omegaInv );
  omega.inv_par( omegaInv_par );

  // Get the limits for the covariance matrix parameters.
  omega.getParLimits( parLow, parUp );

  // Evaluate the covariance matrix at the upper and lower limits
  // for the parameters.
  omega.setPar( parLow );
  omega.cov( omegaAtParLow );
  omega.setPar( parUp );
  omega.cov( omegaAtParUp );

  // Get the covariance matrix diagonals at the limits.
  for ( i = 0; i < nRow; i++ )
  {
    omegaDiagAtParLow[i] = omegaAtParLow[i + i * nRow];
    omegaDiagAtParUp [i] = omegaAtParUp [i + i * nRow];
  }    

  // Calculate the minimal representation for the covariance matrix
  // and its derivative.
  omega.calcCovMinRep    ( omegaCov,           omegaMinRep );
  omega.calcCovMinRep_par( omegaCov_par, nPar, omegaMinRep_par );

  // Multiply the covariance matrix and its inverse.
  omegaCovTimesInv = multiply( omegaCov, nRow, omegaInv, nRow );


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  valarray<double> omegaCovKnown    ( nRow * nRow );
  valarray<double> omegaCov_parKnown( nRow * nRow * nPar );
  valarray<double> omegaInvKnown    ( nRow * nRow );
  valarray<double> omegaInv_parKnown( nRow * nRow * nPar );

  valarray<double> omegaCholKnown        ( nRow * nRow );
  valarray<double> omegaCholTranKnown    ( nRow * nRow );
  valarray<double> omegaChol_parKnown    ( nRow * nRow * nPar );
  valarray<double> omegaCholTran_parKnown( nRow * nRow * nPar );

  valarray<double> omegaDiagAtParLowKnown( nRow );
  valarray<double> omegaDiagAtParUpKnown ( nRow );

  valarray<double> omegaMinRepKnown    ( nPar );
  valarray<double> omegaMinRep_parKnown( nPar * nPar );

  valarray<double> omegaCovTimesInvKnown( nRow * nRow );

  // Create matrices that have only zeroes.
  omegaCholKnown         = 0.0;
  omegaCholTranKnown     = 0.0;
  omegaChol_parKnown     = 0.0;
  omegaCholTran_parKnown = 0.0;

  // Calculate the Cholesky factor for the covariance,
  //
  //                  -                                                            -
  //                 |  exp[ par  ]                                           0     |
  //                 |          0                                                   |
  //                 |                                                              |
  //                 |     par       exp[ par  ]                                    |
  //                 |        1              2                                      |
  //                 |                                                              |
  //    L( par )  =  |     par          par       exp[ par  ]                       |  ,
  //                 |        3            4              5                         |
  //                 |                                                              |
  //                 |       .            .    .            .                       |
  //                 |       .            .      .            .                     |
  //                 |       .            .        .            .                   |
  //                 |                                                              |
  //                 |                               par          exp[ par       ]  |
  //                 |                                  nPar-2            nPar-1    |
  //                  -                                                            -
  //
  int parIndex = 0;
  for ( i = 0; i < nRow; i++ )
  {
    for ( j = 0; j < i; j++ )
    {
      omegaCholKnown[i + j * nRow] = par[parIndex];
      parIndex++;
    }

    omegaCholKnown[i + i * nRow] = exp( par[parIndex] );
    parIndex++;
  }

  // The covariance should be 
  //
  //                                     T
  //    cov( par )  =  L( par )  L( par )  .
  //
  omegaCholTranKnown = transpose( omegaCholKnown, nRow );
  omegaCovKnown = multiply(
    omegaCholKnown,
    nRow,
    omegaCholTranKnown,
    nRow );

  // Calculate the derivative of the Cholesky factor.
  int row;
  parIndex = 0;
  for ( i = 0; i < nRow; i++ )
  {
    for ( j = 0; j < i; j++ )
    {
      // Note that an rvec operation is performed on the elements
      // of the Cholesky factor, and its transpose, before the
      // derivatives are calculated.
      omegaChol_parKnown    [(i * nRow + j) + parIndex * nCov_parRow] = 1.0;
      omegaCholTran_parKnown[(j * nRow + i) + parIndex * nCov_parRow] = 1.0;

      parIndex++;
    }

    // Note that an rvec operation is performed on the elements
    // of the Cholesky factor, and its transpose, before the
    // derivatives are calculated.
    omegaChol_parKnown    [(i * nRow + i) + parIndex * nCov_parRow] = exp( par[parIndex] );
    omegaCholTran_parKnown[(i * nRow + i) + parIndex * nCov_parRow] = exp( par[parIndex] );

    parIndex++;
  }

  // By Corollary 4 of B. M. Bell, "Approximating the marginal
  // likelihood estimate for models with random parameters", 
  // Applied Mathematics and Computation, 119 (2001), pp. 57-73,
  // the derivative of the covariance should be 
  //
  //                                                                T
  //    cov_par( par )  =  [  L( par )  kron  I  ]  d    [  L( par )   ] 
  //                                                 par
  //
  //                    +  [  I  kron  L( par )  ]  d    [  L( par )  ] 
  //                                                 par
  //
  // where the matrix I is an nRow by nRow identity matrix.
  valarray<double> identityMatrix( nRow * nRow );
  identity( nRow, identityMatrix );
  omegaCov_parKnown = 
    AkronItimesC(
      omegaCholKnown,
      nRow,
      identityMatrix,
      nRow,
      omegaCholTran_parKnown,
      nPar )
    +
    IkronBtimesC(
      identityMatrix,
      nRow,
      omegaCholKnown,
      nRow,
      omegaChol_parKnown,
      nPar );

  // Calculate the inverse of the covariance.
  double factor = exp( - 2.0 * ( par[0] + par[2] ) );
  omegaInvKnown[0] = factor * ( par[1] * par[1] + exp( 2.0 * par[2] ) );
  omegaInvKnown[1] = - factor * par[1] * exp( par[0] );
  omegaInvKnown[2] = omegaInvKnown[1];
  omegaInvKnown[3] = factor * exp( 2.0 * par[0] );

  // Calculate the derivative of the inverse of the covariance.
  double factor_x0 = - 2.0 * exp( - 2.0 * ( par[0] + par[2] ) );
  double factor_x1 = 0.0;
  double factor_x2 = - 2.0 * exp( - 2.0 * ( par[0] + par[2] ) );
  omegaInv_parKnown[0 + 0 * nCov_parRow] = factor_x0 * omegaInvKnown[0] / factor;
  omegaInv_parKnown[1 + 0 * nCov_parRow] = factor_x0 * omegaInvKnown[1] / factor - factor * par[1] * exp( par[0] );
  omegaInv_parKnown[2 + 0 * nCov_parRow] = omegaInv_parKnown[1 + 0 * nCov_parRow];
  omegaInv_parKnown[3 + 0 * nCov_parRow] = 0.0;
  omegaInv_parKnown[0 + 1 * nCov_parRow] = factor_x1 * omegaInvKnown[0] / factor + 2.0 * factor * par[1];
  omegaInv_parKnown[1 + 1 * nCov_parRow] = factor_x1 * omegaInvKnown[1] / factor - factor * exp( par[0] );
  omegaInv_parKnown[2 + 1 * nCov_parRow] = omegaInv_parKnown[1 + 1 * nCov_parRow];
  omegaInv_parKnown[3 + 1 * nCov_parRow] = 0.0;
  omegaInv_parKnown[0 + 2 * nCov_parRow] = factor_x2 * omegaInvKnown[0] / factor + factor * 2.0 * exp( 2.0 * par[2] );
  omegaInv_parKnown[1 + 2 * nCov_parRow] = factor_x2 * omegaInvKnown[1] / factor;
  omegaInv_parKnown[2 + 2 * nCov_parRow] = omegaInv_parKnown[1 + 2 * nCov_parRow];
  omegaInv_parKnown[3 + 2 * nCov_parRow] = - 2.0 * exp( - 2.0 * par[2] );

  // The diagonal elements of the covariance matrix at the lower
  // limits of all of its parameters should be:
  //
  //                  (low)          1                   (curr)
  //     cov     ( par      )  =  [ ---  +  i  100 ]  cov        .
  //        (i,i)                   100                  (i,i) 
  //
  // The diagonal elements of the covariance matrix at the upper
  // limits of all of its parameters should be:
  //
  //                  (up)                              (curr)
  //     cov     ( par     )   =  [ ( i + 1 ) 100 ]  cov        .
  //        (i,i)                                       (i,i) 
  //
  for ( i = 0; i < nRow; i++ )
  {
    omegaDiagAtParLowKnown[i] = ( 1.0 / 100.0 + i * 100.0 ) * 
      omegaCovKnown[i + i * nRow];
    omegaDiagAtParUpKnown [i] = ( i + 1 ) * 100.0 *
      omegaCovKnown[i + i * nRow];
  }    

  // Set the known value for the minimal representation for the
  // covariance matrix and its derivative.
  int nCovMinRep_parRow = nPar;
  int sumI = 0;
  for ( i = 0; i < nRow; i++ )
  {
    sumI += i;

    // Set the elements from this row including the diagonal.
    for ( j = 0; j <= i; j++ )
    {
      omegaMinRepKnown[sumI + j] = omegaCovKnown[i + j * nRow];
    
      // Set the derivatives for this element.
      for ( k = 0; k < nPar; k++ )
      {
        omegaMinRep_parKnown[( sumI + j     ) + k * nCovMinRep_parRow] = 
          omegaCov_parKnown [( i * nRow + j ) + k * nCov_parRow];
      }
    }
  }

  // The covariance matrix multiplied by its inverse should be
  // equal to the identity matrix.
  identity( nRow, omegaCovTimesInvKnown );


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  compareToKnown( 
    omegaCov,
    omegaCovKnown,
    "omegaCov",
    tol );

  compareToKnown( 
    omegaCov_par,
    omegaCov_parKnown,
    "omegaCov_par",
    tol );

  compareToKnown( 
    omegaInv,
    omegaInvKnown,
    "omegaInv",
    tol );

  // Increase the tolerance slightly for this comparison because of
  // roundoff errors in some partial derivatives that should be zero.
  tol = 3.0e-13;

  compareToKnown( 
    omegaInv_par,
    omegaInv_parKnown,
    "omegaInv_par",
    tol );

  tol = 1.0e-14;

  compareToKnown( 
    omegaDiagAtParLow,
    omegaDiagAtParLowKnown,
    "omega diagonals at parLow",
    tol );

  compareToKnown( 
    omegaDiagAtParUp,
    omegaDiagAtParUpKnown,
    "omega diagonals at parUp",
    tol );

  compareToKnown( 
    omegaMinRep,
    omegaMinRepKnown,
    "omegaMinRep",
    tol );

  compareToKnown( 
    omegaMinRep_par,
    omegaMinRep_parKnown,
    "omegaMinRep_par",
    tol );

  compareToKnown( 
    omegaCovTimesInv,
    omegaCovTimesInvKnown,
    "omegaCov times omegaInv",
    tol );
}


/*************************************************************************
 *
 * Function: threeByThreeCovTest
 *
 *
 * The goal of this test is to check that the covariance class works
 * for the case of a three-by-three covariance matrix.
 *
 *************************************************************************/

void FullCovTest::threeByThreeCovTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i;
  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the covariance matrix.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  const int nRow = 3;

  // Set the number of rows in the derivative of the covariance matrix.
  int nCov_parRow = nRow * nRow;

  // Construct the covariance matrix.
  FullCov omega( nRow );

  // Get the number of parameters.
  int nPar = omega.getNPar();
  assert( nPar == 6 );

  // Create a known covariance matrix with a miminal
  // representation that is easy to check,
  //
  //                      -                 -
  //                     |  1.0   0.2   0.3  |
  //      omegaKnown  =  |  0.2   4.0   0.5  |  .
  //                     |  0.3   0.5   6.0  |
  //                      -                 -
  //
  valarray<double> omegaCovKnown( nRow * nRow );
  omegaCovKnown[0 + 0 * nRow] = 1.0;
  omegaCovKnown[1 + 0 * nRow] = 0.2;
  omegaCovKnown[2 + 0 * nRow] = 0.3;
  omegaCovKnown[0 + 1 * nRow] = 0.2;
  omegaCovKnown[1 + 1 * nRow] = 4.0;
  omegaCovKnown[2 + 1 * nRow] = 0.5;
  omegaCovKnown[0 + 2 * nRow] = 0.3;
  omegaCovKnown[1 + 2 * nRow] = 0.5;
  omegaCovKnown[2 + 2 * nRow] = 6.0;

  // Initialize the current value for the parameters.
  valarray<double> par( nPar );
  omega.calcPar( omegaCovKnown, par );

  // Set the current value for the parameters.
  omega.setPar( par );

  // Initialize the current value for the parameter mask.
  valarray<bool> parMask( nPar );
  parMask[0] = true;
  parMask[1] = false;
  parMask[2] = true;
  parMask[3] = false;
  parMask[4] = false;
  parMask[5] = true;


  //------------------------------------------------------------
  // Calculate various quantities for the test.
  //------------------------------------------------------------

  valarray<double> omegaCov    ( nRow * nRow );
  valarray<double> omegaCov_par( nRow * nRow * nPar );
  valarray<double> omegaInv    ( nRow * nRow );
  valarray<double> omegaInv_par( nRow * nRow * nPar );

  valarray<double> parLow( nPar );
  valarray<double> parUp ( nPar );

  valarray<double> omegaAtParLow    ( nRow * nRow );
  valarray<double> omegaDiagAtParLow( nRow );
  valarray<double> omegaAtParUp     ( nRow * nRow );
  valarray<double> omegaDiagAtParUp ( nRow );

  valarray<double> omegaMinRep    ( nPar );
  valarray<double> omegaMinRep_par( nPar * nPar );
  valarray<bool>   omegaMinRepMask( nPar );
  valarray<double> omegaExpMinRep ( nRow * nRow );

  valarray<double> omegaCovTimesInv( nRow * nRow );

  // Calculate the covariance matrix, its derivative, its inverse,
  // and the derivative of its inverse.
  omega.cov    ( omegaCov );
  omega.cov_par( omegaCov_par );
  omega.inv    ( omegaInv );
  omega.inv_par( omegaInv_par );

  // Get the limits for the covariance matrix parameters.
  omega.getParLimits( parLow, parUp );

  // Evaluate the covariance matrix at the upper and lower limits
  // for the parameters.
  omega.setPar( parLow );
  omega.cov( omegaAtParLow );
  omega.setPar( parUp );
  omega.cov( omegaAtParUp );

  // Get the covariance matrix diagonals at the limits.
  for ( i = 0; i < nRow; i++ )
  {
    omegaDiagAtParLow[i] = omegaAtParLow[i + i * nRow];
    omegaDiagAtParUp [i] = omegaAtParUp [i + i * nRow];
  }    

  // Calculate the minimal representation for the covariance matrix
  // and its derivative.
  omega.calcCovMinRep    ( omegaCov,           omegaMinRep );
  omega.calcCovMinRep_par( omegaCov_par, nPar, omegaMinRep_par );

  // Calculate the mask for the minimal representation for the
  // covariance matrix.
  omega.calcCovMinRepMask( parMask, omegaMinRepMask );

  // Expand the minimal representation for the covariance matrix.
  omega.expandCovMinRep( omegaMinRep, omegaExpMinRep );

  // Multiply the covariance matrix and its inverse.
  omegaCovTimesInv = multiply( omegaCov, nRow, omegaInv, nRow );


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  valarray<double> omegaCov_parKnown( nRow * nRow * nPar );
  valarray<double> omegaInvKnown    ( nRow * nRow );
  valarray<double> omegaInv_parKnown( nRow * nRow * nPar );

  valarray<double> omegaCholKnown        ( nRow * nRow );
  valarray<double> omegaCholTranKnown    ( nRow * nRow );
  valarray<double> omegaChol_parKnown    ( nRow * nRow * nPar );
  valarray<double> omegaCholTran_parKnown( nRow * nRow * nPar );

  valarray<double> omegaDiagAtParLowKnown( nRow );
  valarray<double> omegaDiagAtParUpKnown ( nRow );

  valarray<double> omegaMinRepKnown    ( nPar );
  valarray<double> omegaMinRep_parKnown( nPar * nPar );
  valarray<bool>   omegaMinRepMaskKnown( nPar );
  valarray<double> omegaExpMinRepKnown ( nRow * nRow );

  valarray<double> omegaCovTimesInvKnown( nRow * nRow );

  // Create matrices that have only zeroes.
  omegaCholKnown         = 0.0;
  omegaCholTranKnown     = 0.0;
  omegaChol_parKnown     = 0.0;
  omegaCholTran_parKnown = 0.0;

  // Calculate the Cholesky factor for the covariance,
  //
  //                  -                                                            -
  //                 |  exp[ par  ]                                           0     |
  //                 |          0                                                   |
  //                 |                                                              |
  //                 |     par       exp[ par  ]                                    |
  //                 |        1              2                                      |
  //                 |                                                              |
  //    L( par )  =  |     par          par       exp[ par  ]                       |  ,
  //                 |        3            4              5                         |
  //                 |                                                              |
  //                 |       .            .    .            .                       |
  //                 |       .            .      .            .                     |
  //                 |       .            .        .            .                   |
  //                 |                                                              |
  //                 |                               par          exp[ par       ]  |
  //                 |                                  nPar-2            nPar-1    |
  //                  -                                                            -
  //
  int parIndex = 0;
  for ( i = 0; i < nRow; i++ )
  {
    for ( j = 0; j < i; j++ )
    {
      omegaCholKnown[i + j * nRow] = par[parIndex];
      parIndex++;
    }

    omegaCholKnown[i + i * nRow] = exp( par[parIndex] );
    parIndex++;
  }

  // The covariance should be 
  //
  //                                     T
  //    cov( par )  =  L( par )  L( par )  .
  //
  omegaCholTranKnown = transpose( omegaCholKnown, nRow );
  omegaCovKnown = multiply(
    omegaCholKnown,
    nRow,
    omegaCholTranKnown,
    nRow );

  // Calculate the derivative of the Cholesky factor.
  int row;
  parIndex = 0;
  for ( i = 0; i < nRow; i++ )
  {
    for ( j = 0; j < i; j++ )
    {
      // Note that an rvec operation is performed on the elements
      // of the Cholesky factor, and its transpose, before the
      // derivatives are calculated.
      omegaChol_parKnown    [(i * nRow + j) + parIndex * nCov_parRow] = 1.0;
      omegaCholTran_parKnown[(j * nRow + i) + parIndex * nCov_parRow] = 1.0;

      parIndex++;
    }

    // Note that an rvec operation is performed on the elements
    // of the Cholesky factor, and its transpose, before the
    // derivatives are calculated.
    omegaChol_parKnown    [(i * nRow + i) + parIndex * nCov_parRow] = exp( par[parIndex] );
    omegaCholTran_parKnown[(i * nRow + i) + parIndex * nCov_parRow] = exp( par[parIndex] );

    parIndex++;
  }

  // By Corollary 4 of B. M. Bell, "Approximating the marginal
  // likelihood estimate for models with random parameters", 
  // Applied Mathematics and Computation, 119 (2001), pp. 57-73,
  // the derivative of the covariance should be 
  //
  //                                                                T
  //    cov_par( par )  =  [  L( par )  kron  I  ]  d    [  L( par )   ] 
  //                                                 par
  //
  //                    +  [  I  kron  L( par )  ]  d    [  L( par )  ] 
  //                                                 par
  //
  // where the matrix I is an nRow by nRow identity matrix.
  valarray<double> identityMatrix( nRow * nRow );
  identity( nRow, identityMatrix );
  omegaCov_parKnown = 
    AkronItimesC(
      omegaCholKnown,
      nRow,
      identityMatrix,
      nRow,
      omegaCholTran_parKnown,
      nPar )
    +
    IkronBtimesC(
      identityMatrix,
      nRow,
      omegaCholKnown,
      nRow,
      omegaChol_parKnown,
      nPar );

  // Calculate the inverse of the covariance.
  omegaInvKnown = inverse( omegaCovKnown, nRow );

  // The diagonal elements of the covariance matrix at the lower
  // limits of all of its parameters should be:
  //
  //                  (low)          1                   (curr)
  //     cov     ( par      )  =  [ ---  +  i  100 ]  cov        .
  //        (i,i)                   100                  (i,i) 
  //
  // The diagonal elements of the covariance matrix at the upper
  // limits of all of its parameters should be:
  //
  //                  (up)                              (curr)
  //     cov     ( par     )   =  [ ( i + 1 ) 100 ]  cov        .
  //        (i,i)                                       (i,i) 
  //
  for ( i = 0; i < nRow; i++ )
  {
    omegaDiagAtParLowKnown[i] = ( 1.0 / 100.0 + i * 100.0 ) * 
      omegaCovKnown[i + i * nRow];
    omegaDiagAtParUpKnown [i] = ( i + 1 ) * 100.0 *
      omegaCovKnown[i + i * nRow];
  }    

  // Set the known value for the minimal representation 
  // for the covariance matrix.
  omegaMinRepKnown[0] = 1.0;
  omegaMinRepKnown[1] = 0.2;
  omegaMinRepKnown[2] = 0.3;
  omegaMinRepKnown[3] = 4.0;
  omegaMinRepKnown[4] = 0.5;
  omegaMinRepKnown[5] = 6.0;

  // Set the known value for the derivative of the minimal
  // representation for the covariance matrix.
  int nCovMinRep_parRow = nPar;
  int sumI = 0;
  for ( k = 0; k < nPar; k++ )
  {
    sumI = 0;
    for ( j = 0; j < nRow; j++ )
    {
      // Get the elements from this column including the diagonal.
      for ( i = j; i < nRow; i++ )
      {
        omegaMinRep_parKnown[( sumI++ )       + k * nCovMinRep_parRow] = 
          omegaCov_parKnown [( i + j * nRow ) + k * nCov_parRow];
      }    
    }    
  }    

  // Set the known value for the mask for the minimal representation
  // for the covariance matrix.
  //
  // Note that the third and fourth elements of the masks are switched
  // because the elements of the minimal representation are stored in
  // column major order, but the elements of the covariance parameters
  // that make up the Cholesky factor are stored in row major order.
  omegaMinRepMaskKnown[0] = parMask[0];
  omegaMinRepMaskKnown[1] = parMask[1];
  omegaMinRepMaskKnown[2] = parMask[3];    // This element is switched.
  omegaMinRepMaskKnown[3] = parMask[2];    // This element is switched.
  omegaMinRepMaskKnown[4] = parMask[4];
  omegaMinRepMaskKnown[5] = parMask[5];

  // The known value for the expanded minimal representation 
  // should just be the original covariance matrix.
  omegaExpMinRepKnown = omegaCovKnown;

  // The covariance matrix multiplied by its inverse should be
  // equal to the identity matrix.
  identity( nRow, omegaCovTimesInvKnown );


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  compareToKnown( 
    omegaCov,
    omegaCovKnown,
    "omegaCov",
    tol );

  compareToKnown( 
    omegaCov_par,
    omegaCov_parKnown,
    "omegaCov_par",
    tol );

  compareToKnown( 
    omegaInv,
    omegaInvKnown,
    "omegaInv",
    tol );

  // This comparison is not performed for this test because it
  // is too difficult to calculate the known value analytically.
  /*
  compareToKnown( 
    omegaInv_par,
    omegaInv_parKnown,
    "omegaInv_par",
    tol );
  */

  compareToKnown( 
    omegaDiagAtParLow,
    omegaDiagAtParLowKnown,
    "omega diagonals at parLow",
    tol );

  compareToKnown( 
    omegaDiagAtParUp,
    omegaDiagAtParUpKnown,
    "omega diagonals at parUp",
    tol );

  compareToKnown( 
    omegaMinRep,
    omegaMinRepKnown,
    "omegaMinRep",
    tol );

  compareToKnown( 
    omegaMinRep_par,
    omegaMinRep_parKnown,
    "omegaMinRep_par",
    tol );

  compareToKnown( 
    omegaMinRepMask,
    omegaMinRepMaskKnown,
    "omegaMinRepMask" );

  compareToKnown( 
    omegaExpMinRep,
    omegaExpMinRepKnown,
    "omegaExpMinRep",
    tol );

  compareToKnown( 
    omegaCovTimesInv,
    omegaCovTimesInvKnown,
    "omegaCov times omegaInv",
    tol );
}


/*************************************************************************
 *
 * Function: isCachingProperlyTest
 *
 *
 * The goal of this test is to check that the covariance class
 * is caching properly.
 *
 *************************************************************************/

void FullCovTest::isCachingProperlyTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Prepare the covariance matrix.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  const int nRow = 1;

  // Construct the covariance matrix.
  FullCov omega( nRow );

  // Get the number of parameters.
  int nPar = omega.getNPar();
  assert( nPar == 1 );

  // Initialize the current value for the parameters.
  valarray<double> par( nPar );
  par[0] = 0.1;


  //------------------------------------------------------------
  // Prepare to see if values are being cached.
  //------------------------------------------------------------

  valarray<double> omegaCov    ( nRow * nRow );
  valarray<double> omegaCov_par( nRow * nRow * nPar );
  valarray<double> omegaInv    ( nRow * nRow );
  valarray<double> omegaInv_par( nRow * nRow * nPar );

  valarray<double> parLow( nPar );
  valarray<double> parUp ( nPar );

  // Get the limits for the covariance matrix parameters.
  omega.getParLimits( parLow, parUp );

  // Evaluate the covariance matrix, its derivative, its inverse,
  // and the derivative of its inverse at the current parameter
  // value.
  omega.setPar( par );
  omega.cov    ( omegaCov );
  omega.cov_par( omegaCov_par );
  omega.inv    ( omegaInv );
  omega.inv_par( omegaInv_par );


  //------------------------------------------------------------
  // See if cached values are used when the parameter changes.
  //------------------------------------------------------------

  // Evaluate the quantities at a different parameter value.
  // The cached values should not be used in this case.
  omega.setPar( parLow );

  omega.cov( omegaCov );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaCov was used when it was not valid.",
    omega.getUsedCachedCov() == false );

  omega.cov_par( omegaCov_par );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaCov_par was used when it was not valid.",
    omega.getUsedCachedCov_par() == false );

  omega.inv( omegaInv );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaInv was used when it was not valid.",
    omega.getUsedCachedInv() == false );

  omega.inv_par( omegaInv_par );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaInv_par was used when it was not valid.",
    omega.getUsedCachedInv_par() == false );


  //------------------------------------------------------------
  // See if cached values are used when the parameter does not change.
  //------------------------------------------------------------

  // Evaluate the quantities at the same parameter value.
  // The cached values should be used in this case.
  omega.setPar( parLow );

  omega.cov( omegaCov );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaCov was not used when it was valid.",
    omega.getUsedCachedCov() == true );

  omega.cov_par( omegaCov_par );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaCov_par was not used when it was valid.",
    omega.getUsedCachedCov_par() == true );

  omega.inv( omegaInv );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaInv was not used when it was valid.",
    omega.getUsedCachedInv() == true );

  omega.inv_par( omegaInv_par );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaInv_par was not used when it was valid.",
    omega.getUsedCachedInv_par() == true );
}

 

/*************************************************************************
 *
 * Function: FixedTwoByTwoCovTest
 *
 *
 * The goal of this test is to check that the covariance class works
 * for the case of a FIXed two-by-two covariance matrix.
 *
 *************************************************************************/

void FullCovTest::fixedTwoByTwoCovTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i;
  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the covariance matrix.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  const int nRow = 2;
  const int nPar = 3;

  // Set all ek=lements as fixed
  valarray<bool> parFixed( nPar );
  parFixed = true;

  // Set the number of rows in the derivative of the covariance matrix.
  //...int nCov_parRow = nRow * nRow;

  // Construct the covariance matrix.
  FullCov omega( nRow, parFixed );

  // check the number of parameters.
  assert( nPar == omega.getNPar() );

  // Initialize the current value for the parameters.
  valarray<double> par( nPar );
  par[0] = 0.1;
  par[1] = 0.005;
  par[2] = -3.0;

  // Set the current value for the parameters.
  omega.setPar( par );

  //------------------------------------------------------------
  // Calculate various quantities for the test.
  //------------------------------------------------------------

  valarray<double> omegaCov    ( nRow * nRow );
  
  valarray<double> parLow( nPar );
  valarray<double> parUp ( nPar );

  valarray<double> omegaAtParLow    ( nRow * nRow );
  valarray<double> omegaDiagAtParLow( nRow );
  valarray<double> omegaAtParUp     ( nRow * nRow );
  valarray<double> omegaDiagAtParUp ( nRow );

  valarray<double> omegaMinRep    ( nPar );
  
  // Calculate the covariance matrix, its derivative, its inverse,
  // and the derivative of its inverse.
  omega.cov    ( omegaCov );
 
  // Get the limits for the covariance matrix parameters.
  omega.getParLimits( parLow, parUp );

  // Evaluate the covariance matrix at the upper and lower limits
  // for the parameters.
  omega.setPar( parLow );
  omega.cov( omegaAtParLow );
  omega.setPar( parUp );
  omega.cov( omegaAtParUp );

  // Get the covariance matrix diagonals at the limits.
  for ( i = 0; i < nRow; i++ )
  {
    omegaDiagAtParLow[i] = omegaAtParLow[i + i * nRow];
    omegaDiagAtParUp [i] = omegaAtParUp [i + i * nRow];
  }    

  // Calculate the minimal representation for the covariance matrix
  // and its derivative.
  omega.calcCovMinRep    ( omegaCov, omegaMinRep );
  
  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  valarray<double> omegaCovKnown    ( nRow * nRow );
  
  valarray<double> omegaCholKnown        ( nRow * nRow );
  valarray<double> omegaCholTranKnown    ( nRow * nRow );
 
  valarray<double> omegaDiagAtParLowKnown( nRow );
  valarray<double> omegaDiagAtParUpKnown ( nRow );

  valarray<double> omegaMinRepKnown    ( nPar );
 

  // Create matrices that have only zeroes.
  omegaCholKnown         = 0.0;
  omegaCholTranKnown     = 0.0;
  

  // Calculate the Cholesky factor for the covariance,
  int parIndex = 0;
  for ( i = 0; i < nRow; i++ )
  {
    for ( j = 0; j < i; j++ )
    {
      omegaCholKnown[i + j * nRow] = par[parIndex];
      parIndex++;
    }

    omegaCholKnown[i + i * nRow] = exp( par[parIndex] );
    parIndex++;
  }

  // The covariance should be 
  //
  //                                     T
  //    cov( par )  =  L( par )  L( par )  .
  //
  omegaCholTranKnown = transpose( omegaCholKnown, nRow );
  omegaCovKnown = multiply(
    omegaCholKnown,
    nRow,
    omegaCholTranKnown,
    nRow );

  // Calculate the derivative of the Cholesky factor.
  int row;
  parIndex = 0;
  for ( i = 0; i < nRow; i++ )
  {
    for ( j = 0; j < i; j++ )
    {
      // Note that an rvec operation is performed on the elements
      // of the Cholesky factor, and its transpose, before the
      // derivatives are calculated.
    }

    // Note that an rvec operation is performed on the elements
    // of the Cholesky factor, and its transpose, before the
    // derivatives are calculated.
  }

  
  // The diagonal elements of the covariance matrix at the lower
  // limits of all of its parameters should be:
  //
  //                  (low)          1                   (curr)
  //     cov     ( par      )  =  [ ---  +  i  100 ]  cov        .
  //        (i,i)                   100                  (i,i) 
  //
  // The diagonal elements of the covariance matrix at the upper
  // limits of all of its parameters should be:
  //
  //                  (up)                              (curr)
  //     cov     ( par     )   =  [ ( i + 1 ) 100 ]  cov        .
  //        (i,i)                                       (i,i) 
  //
  for ( i = 0; i < nRow; i++ )
  {
    omegaDiagAtParLowKnown[i] = omegaCovKnown[i + i * nRow];
    omegaDiagAtParUpKnown [i] = omegaCovKnown[i + i * nRow];
  }    

  // Set the known value for the minimal representation for the
  // covariance matrix and its derivative.
  int nCovMinRep_parRow = nPar;
  int sumI = 0;
  for ( i = 0; i < nRow; i++ )
  {
    sumI += i;

    // Set the elements from this row including the diagonal.
    for ( j = 0; j <= i; j++ )
    {
      omegaMinRepKnown[sumI + j] = omegaCovKnown[i + j * nRow];
    }
  }


  //Revisit 2006-04-27 (Dave).  The following two lines prevent a compiler segmentation fault
  // when compiling in *release* mode.  I don't even use omegaMinRepKnown2... but any
  // line of code which touches omegaMinRepKnown prevents this compile error.
  // gcc version 3.2.3  is being used.  I am hoping this issue dissapears when we update the compiler version.
  valarray<double> omegaMinRepKnown2    ( nPar );
  omegaMinRepKnown2 = omegaMinRepKnown;


  // The covariance matrix multiplied by its inverse should be
  // equal to the identity matrix.
  //...identity( nRow, omegaCovTimesInvKnown );


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  compareToKnown( 
    omegaCov,
    omegaCovKnown,
    "omegaCov",
    tol );

 
  compareToKnown( 
    omegaDiagAtParLow,
    omegaDiagAtParLowKnown,
    "omega diagonals at parLow",
    tol );

  compareToKnown( 
    omegaDiagAtParUp,
    omegaDiagAtParUpKnown,
    "omega diagonals at parUp",
    tol );

  compareToKnown( 
    omegaMinRep,
    omegaMinRepKnown,
    "omegaMinRep",
    tol );

}
