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
 * File: DiagCovTest.cpp
 *
 *
 * Unit test for the class DiagCov.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK Pred test suite header files.
#include "DiagCovTest.h"
#include "compareToKnown.h"

// SPK Pred library header files.
#include <spkpred/DiagCov.h>

// SPK library header files.
#include <spk/identity.h>
#include <spk/multiply.h>
#include <spk/SpkValarray.h>

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

void DiagCovTest::setUp()
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

void DiagCovTest::tearDown()
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

Test* DiagCovTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "DiagCovTest" );

  suiteOfTests->addTest(new TestCaller<DiagCovTest>(
    "oneByOneCovTest", 
    &DiagCovTest::oneByOneCovTest ));

  suiteOfTests->addTest(new TestCaller<DiagCovTest>(
    "twoByTwoCovTest", 
    &DiagCovTest::twoByTwoCovTest ));

  suiteOfTests->addTest(new TestCaller<DiagCovTest>(
    "isCachingProperlyTest", 
    &DiagCovTest::isCachingProperlyTest ));

  suiteOfTests->addTest(new TestCaller<DiagCovTest>(
    "FixedTwoByTwoCovTest", 
    &DiagCovTest::fixedTwoByTwoCovTest ));

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

void DiagCovTest::oneByOneCovTest()
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
  DiagCov omega( nRow );

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

  valarray<double> omegaAtParLow( nRow * nRow );
  valarray<double> omegaAtParUp ( nRow * nRow );

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

  // Calculate the minimal representation for the covariance matrix
  // its derivative.
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

  valarray<double> omegaAtParLowKnown( nRow * nRow );
  valarray<double> omegaAtParUpKnown ( nRow * nRow );

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
  omegaAtParLowKnown[0] = omegaCovKnown[0] / 100.0;
  omegaAtParUpKnown[0]  = omegaCovKnown[0] * 100.0;

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
    omegaAtParLow,
    omegaAtParLowKnown,
    "omega at parLow",
    tol );

  compareToKnown( 
    omegaAtParUp,
    omegaAtParUpKnown,
    "omega at parUp",
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

void DiagCovTest::twoByTwoCovTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i;
  int k;


  //------------------------------------------------------------
  // Prepare the covariance matrix.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  const int nRow = 2;

  // Set the number of rows in the derivative of the covariance matrix.
  int nCov_parRow = nRow * nRow;

  // Construct the covariance matrix.
  DiagCov omega( nRow );

  // Get the number of parameters.
  int nPar = omega.getNPar();
  assert( nPar == 2 );

  // Initialize the current value for the parameters.
  valarray<double> par( nPar );
  par[0] = 0.1;
  par[1] = -3.0;

  // Set the current value for the parameters.
  omega.setPar( par );

  // Initialize the current value for the parameter mask.
  valarray<bool> parMask( nPar );
  parMask[0] = true;
  parMask[1] = false;


  //------------------------------------------------------------
  // Calculate various quantities for the test.
  //------------------------------------------------------------

  valarray<double> omegaCov    ( nRow * nRow );
  valarray<double> omegaCov_par( nRow * nRow * nPar );
  valarray<double> omegaInv    ( nRow * nRow );
  valarray<double> omegaInv_par( nRow * nRow * nPar );

  valarray<double> parLow( nPar );
  valarray<double> parUp ( nPar );

  valarray<double> omegaAtParLow( nRow * nRow );
  valarray<double> omegaAtParUp ( nRow * nRow );

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

  valarray<double> omegaAtParLowKnown( nRow * nRow );
  valarray<double> omegaAtParUpKnown ( nRow * nRow );

  valarray<double> omegaMinRepKnown    ( nPar );
  valarray<double> omegaMinRep_parKnown( nPar * nPar );
  valarray<bool>   omegaMinRepMaskKnown( nPar );

  valarray<double> omegaCovTimesInvKnown( nRow * nRow );

  // Create a matrices that have only zeroes.
  omegaCovKnown     = 0.0;
  omegaCov_parKnown = 0.0;
  omegaInvKnown     = 0.0;
  omegaInv_parKnown = 0.0;

  // The diagonal elements should be 
  //
  //    cov      ( par )  =  exp[ 2 par  ]  .
  //       (i, i)                      i   
  //
  for ( i = 0; i < nPar; i++ )
  {
    omegaCovKnown[i + i * nRow] = exp( 2.0 * par[i] );
    omegaInvKnown[i + i * nRow] = exp( -2.0 * par[i] );
  }

  // The partial derivatives of the diagonal elements of
  // the covariance should be
  //
  //     (i)     
  //    d     cov      ( par )  =  2 exp[ 2 par  ]  .
  //     par     (i, i)                        i
  //
  int row;
  for ( i = 0; i < nPar; i++ )
  {
    // Set the row in the rvec version of the covariance.
    row = i * nRow + i;

    omegaCov_parKnown[row + i * nCov_parRow] = 2.0 * exp( 2.0 * par[i] );
    omegaInv_parKnown[row + i * nCov_parRow] = -2.0 * exp( -2.0 * par[i] );
  }    

  // The diagonal elements of the covariance matrix should be
  // constrained as follows,
  //
  //      1      (curr)                              (curr)
  //     ---  cov        <=   cov        <=  100  cov        .
  //     100     (i,i)           (i,i)               (i,i) 
  //
  for ( i = 0; i < nPar; i++ )
  {
    omegaAtParLowKnown[i + i * nRow] = omegaCovKnown[i + i * nRow] / 100.0;
    omegaAtParUpKnown[ i + i * nRow] = omegaCovKnown[i + i * nRow] * 100.0;
  }

  // Set the known value for the minimal representation for the
  // covariance matrix and its derivative.
  for ( i = 0; i < nPar; i++ )
  {
    omegaMinRepKnown[i] = omegaCovKnown[i + i * nRow];

    for ( k = 0; k < nPar; k++ )
    {
      omegaMinRep_parKnown[( i            ) + k * nPar] = 
        omegaCov_parKnown [( i * nRow + i ) + k * nCov_parRow];
    }
  }

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
    omegaAtParLow,
    omegaAtParLowKnown,
    "omega at parLow",
    tol );

  compareToKnown( 
    omegaAtParUp,
    omegaAtParUpKnown,
    "omega at parUp",
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
 * Function: isCachingProperlyTest
 *
 *
 * The goal of this test is to check that the covariance class
 * is caching properly.
 *
 *************************************************************************/

void DiagCovTest::isCachingProperlyTest()
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
  DiagCov omega( nRow );

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

  omega.cov    ( omegaCov );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaCov was used when it was not valid.",
    omega.getUsedCachedCov() == false );

  omega.cov_par( omegaCov_par );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaCov_par was used when it was not valid.",
    omega.getUsedCachedCov_par() == false );

  omega.inv    ( omegaInv );
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

  omega.cov    ( omegaCov );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaCov was not used when it was valid.",
    omega.getUsedCachedCov() == true );

  omega.cov_par( omegaCov_par );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaCov_par was not used when it was valid.",
    omega.getUsedCachedCov_par() == true );

  omega.inv    ( omegaInv );
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
 * Function: fixedTwoByTwoCovTest
 *
 *
 * The goal of this test is to check that an element of a diagonal covariance 
 * can be fixed - for the case of a two-by-two covariance matrix.
 *
 *************************************************************************/

void DiagCovTest::fixedTwoByTwoCovTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i;
  int k;


  //------------------------------------------------------------
  // Prepare the covariance matrix.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  const int nRow = 2;

  // Set only the 2,2 element as fixed
  valarray<bool> parFixed( nRow );
  parFixed[0] = false;
  parFixed[1] = true;

  // Set the number of rows in the derivative of the covariance matrix.
  //...int nCov_parRow = nRow * nRow;

  // Construct the covariance matrix.
  DiagCov omega( nRow, parFixed );

  // Get the number of parameters.
  int nPar = omega.getNPar();
  assert( nPar == 2 );

  // Initialize the current value for the parameters.
  valarray<double> par( nPar );
  par[0] = 0.1;
  par[1] = -3.0;

  // Set the current value for the parameters.
  omega.setPar( par );

  //------------------------------------------------------------
  // Calculate various quantities for the test.
  //------------------------------------------------------------

  valarray<double> omegaCov    ( nRow * nRow );
 
  valarray<double> parLow( nPar );
  valarray<double> parUp ( nPar );

  valarray<double> omegaAtParLow( nRow * nRow );
  valarray<double> omegaAtParUp ( nRow * nRow );

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

  // Calculate the minimal representation for the covariance matrix
  // and its derivative.
  omega.calcCovMinRep    ( omegaCov,           omegaMinRep );
 
  // Multiply the covariance matrix and its inverse.
  //...omegaCovTimesInv = multiply( omegaCov, nRow, omegaInv, nRow );


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  valarray<double> omegaCovKnown    ( nRow * nRow );
 
  valarray<double> omegaAtParLowKnown( nRow * nRow );
  valarray<double> omegaAtParUpKnown ( nRow * nRow );

  valarray<double> omegaMinRepKnown    ( nPar );
 
  // Create a matrices that have only zeroes.
  omegaCovKnown     = 0.0;
 
  // The diagonal elements should be 
  //
  //    cov      ( par )  =  exp[ 2 par  ]  .
  //       (i, i)                      i   
  //
  for ( i = 0; i < nPar; i++ )
  {
    omegaCovKnown[i + i * nRow] = exp( 2.0 * par[i] );
    //... omegaInvKnown[i + i * nRow] = exp( -2.0 * par[i] );
  }

  // The partial derivatives of the diagonal elements of
  // the covariance should be
  //
  //     (i)     
  //    d     cov      ( par )  =  2 exp[ 2 par  ]  .
  //     par     (i, i)                        i
  //
  int row;
  for ( i = 0; i < nPar; i++ )
  {
    // Set the row in the rvec version of the covariance.
    row = i * nRow + i;

    //...omegaCov_parKnown[row + i * nCov_parRow] = 2.0 * exp( 2.0 * par[i] );
    //...omegaInv_parKnown[row + i * nCov_parRow] = -2.0 * exp( -2.0 * par[i] );
  }    

  // The diagonal elements of the covariance matrix should be
  // constrained as follows,
  //
  //      1      (curr)                              (curr)
  //     ---  cov        <=   cov        <=  100  cov        .
  //     100     (i,i)           (i,i)               (i,i) 
  //

  //this should purposely break it
  //parFixed[0] = true;
  //parFixed[1] = false;
  //
  for ( i = 0; i < nPar; i++ )
  {
    if( parFixed[i] )
    {
      omegaAtParLowKnown[i + i * nRow] = omegaCovKnown[i + i * nRow];
      omegaAtParUpKnown[ i + i * nRow] = omegaCovKnown[i + i * nRow];
    }
    else
    {
      omegaAtParLowKnown[i + i * nRow] = omegaCovKnown[i + i * nRow] / 100.0;
      omegaAtParUpKnown[ i + i * nRow] = omegaCovKnown[i + i * nRow] * 100.0;
    }
  }

  // Set the known value for the minimal representation for the
  // covariance matrix and its derivative.
  for ( i = 0; i < nPar; i++ )
  {
    omegaMinRepKnown[i] = omegaCovKnown[i + i * nRow];

    for ( k = 0; k < nPar; k++ )
    {
      //... omegaMinRep_parKnown[( i            ) + k * nPar] = 
      //...omegaCov_parKnown [( i * nRow + i ) + k * nCov_parRow];
    }
  }

  // The covariance matrix multiplied by its inverse should be
  // equal to the identity matrix.
  //...  identity( nRow, omegaCovTimesInvKnown );


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
    omegaAtParLow,
    omegaAtParLowKnown,
    "omega at parLow",
    tol );

  compareToKnown( 
    omegaAtParUp,
    omegaAtParUpKnown,
    "omega at parUp",
    tol );

  compareToKnown( 
    omegaMinRep,
    omegaMinRepKnown,
    "omegaMinRep",
    tol );

}

