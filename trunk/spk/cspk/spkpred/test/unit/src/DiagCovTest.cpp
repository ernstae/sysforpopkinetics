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
  const int nCovRow = 1;

  // Construct the covariance matrix.
  DiagCov omega( nCovRow );

  // Get the number of parameters.
  int nPar = omega.getNPar();
  assert( nPar == 1 );

  // Initialize the current value for the parameters.
  valarray<double> par( nPar );
  par[0] = 0.1;

  // Set the current value for the parameters.
  omega.setPar( par );


  //------------------------------------------------------------
  // Calculate various quantities for the test.
  //------------------------------------------------------------

  valarray<double> omegaCov    ( nCovRow * nCovRow );
  valarray<double> omegaCov_par( nCovRow * nCovRow * nPar );
  valarray<double> omegaInv    ( nCovRow * nCovRow );
  valarray<double> omegaInv_par( nCovRow * nCovRow * nPar );

  valarray<double> parLow( nPar );
  valarray<double> parUp ( nPar );

  valarray<double> omegaAtParLow( nCovRow * nCovRow );
  valarray<double> omegaAtParUp ( nCovRow * nCovRow );

  valarray<double> omegaCovTimesInv( nCovRow * nCovRow );

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

  // Multiply the covariance matrix and its inverse.
  omegaCovTimesInv = multiply( omegaCov, nCovRow, omegaInv, nCovRow );


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  valarray<double> omegaCovKnown    ( nCovRow * nCovRow );
  valarray<double> omegaCov_parKnown( nCovRow * nCovRow * nPar );
  valarray<double> omegaInvKnown    ( nCovRow * nCovRow );
  valarray<double> omegaInv_parKnown( nCovRow * nCovRow * nPar );

  valarray<double> omegaAtParLowKnown( nCovRow * nCovRow );
  valarray<double> omegaAtParUpKnown ( nCovRow * nCovRow );

  valarray<double> omegaCovTimesInvKnown( nCovRow * nCovRow );

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

  // The covariance matrix multiplied by its inverse should be
  // equal to the identity matrix.
  identity( nCovRow, omegaCovTimesInvKnown );


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


  //------------------------------------------------------------
  // Prepare the covariance matrix.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  const int nCovRow = 2;

  // Set the number of rows in the derivative of the covariance matrix.
  int nCov_parRow = nCovRow * nCovRow;

  // Construct the covariance matrix.
  DiagCov omega( nCovRow );

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

  valarray<double> omegaCov    ( nCovRow * nCovRow );
  valarray<double> omegaCov_par( nCovRow * nCovRow * nPar );
  valarray<double> omegaInv    ( nCovRow * nCovRow );
  valarray<double> omegaInv_par( nCovRow * nCovRow * nPar );

  valarray<double> parLow( nPar );
  valarray<double> parUp ( nPar );

  valarray<double> omegaAtParLow( nCovRow * nCovRow );
  valarray<double> omegaAtParUp ( nCovRow * nCovRow );

  valarray<double> omegaCovTimesInv( nCovRow * nCovRow );

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

  // Multiply the covariance matrix and its inverse.
  omegaCovTimesInv = multiply( omegaCov, nCovRow, omegaInv, nCovRow );


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  valarray<double> omegaCovKnown    ( nCovRow * nCovRow );
  valarray<double> omegaCov_parKnown( nCovRow * nCovRow * nPar );
  valarray<double> omegaInvKnown    ( nCovRow * nCovRow );
  valarray<double> omegaInv_parKnown( nCovRow * nCovRow * nPar );

  valarray<double> omegaAtParLowKnown( nCovRow * nCovRow );
  valarray<double> omegaAtParUpKnown ( nCovRow * nCovRow );

  valarray<double> omegaCovTimesInvKnown( nCovRow * nCovRow );

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
    omegaCovKnown[i + i * nCovRow] = exp( 2.0 * par[i] );
    omegaInvKnown[i + i * nCovRow] = exp( -2.0 * par[i] );
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
    row = i * nCovRow + i;

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
    omegaAtParLowKnown[i + i * nCovRow] = omegaCovKnown[i + i * nCovRow] / 100.0;
    omegaAtParUpKnown[ i + i * nCovRow] = omegaCovKnown[i + i * nCovRow] * 100.0;
  }

  // The covariance matrix multiplied by its inverse should be
  // equal to the identity matrix.
  identity( nCovRow, omegaCovTimesInvKnown );


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
  const int nCovRow = 1;

  // Construct the covariance matrix.
  DiagCov omega( nCovRow );

  // Get the number of parameters.
  int nPar = omega.getNPar();
  assert( nPar == 1 );

  // Initialize the current value for the parameters.
  valarray<double> par( nPar );
  par[0] = 0.1;


  //------------------------------------------------------------
  // Calculate various quantities for the test.
  //------------------------------------------------------------

  valarray<double> omegaCov    ( nCovRow * nCovRow );
  valarray<double> omegaCov_par( nCovRow * nCovRow * nPar );
  valarray<double> omegaInv    ( nCovRow * nCovRow );
  valarray<double> omegaInv_par( nCovRow * nCovRow * nPar );

  valarray<double> parLow( nPar );
  valarray<double> parUp ( nPar );

  // Get the limits for the covariance matrix parameters.
  omega.getParLimits( parLow, parUp );


  //------------------------------------------------------------
  // See if values are being cached.
  //------------------------------------------------------------

  // Evaluate the covariance matrix, its derivative, its inverse,
  // and the derivative of its inverse at the current parameter
  // value.
  omega.setPar( par );
  omega.cov    ( omegaCov );
  omega.cov_par( omegaCov_par );
  omega.inv    ( omegaInv );
  omega.inv_par( omegaInv_par );

  // Evaluate the quantities at a different parameter value.
  // The cached values should not be used in this case.
  omega.setPar( parLow );
  omega.cov    ( omegaCov );
  omega.cov_par( omegaCov_par );
  omega.inv    ( omegaInv );
  omega.inv_par( omegaInv_par );

  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaCov was used when it was not valid.",
    omega.getUsedCachedCov() == false );

  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaCov_par was used when it was not valid.",
    omega.getUsedCachedCov_par() == false );

  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaInv was used when it was not valid.",
    omega.getUsedCachedInv() == false );

  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaInv_par was used when it was not valid.",
    omega.getUsedCachedInv_par() == false );

  // Evaluate the quantities at the same parameter value.
  // The cached values should be used in this case.
  omega.setPar( parLow );
  omega.cov    ( omegaCov );
  omega.cov_par( omegaCov_par );
  omega.inv    ( omegaInv );
  omega.inv_par( omegaInv_par );

  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaCov was not used when it was valid.",
    omega.getUsedCachedCov() == true );

  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaCov_par was not used when it was valid.",
    omega.getUsedCachedCov_par() == true );

  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaInv was not used when it was valid.",
    omega.getUsedCachedInv() == true );

  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaInv_par was not used when it was valid.",
    omega.getUsedCachedInv_par() == true );

}

 
