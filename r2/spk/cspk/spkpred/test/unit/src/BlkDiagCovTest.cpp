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
 * File: BlkDiagCovTest.cpp
 *
 *
 * Unit test for the class BlkDiagCov.
 *
 * Author: David Salinger / Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK Pred test suite header files.
#include "BlkDiagCovTest.h"
#include "compareToKnown.h"

// SPK Pred library header files.
#include <spkpred/DiagCov.h>
#include <spkpred/FullCov.h>
#include <spkpred/BlkDiagCov.h>

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
using SPK_VA::slice;


/*************************************************************************
 *
 * Function: setUp
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void BlkDiagCovTest::setUp()
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

void BlkDiagCovTest::tearDown()
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

Test* BlkDiagCovTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "BlkDiagCovTest" );

  suiteOfTests->addTest(new TestCaller<BlkDiagCovTest>(
    "isCachingProperlyTest", 
    &BlkDiagCovTest::isCachingProperlyTest ));

  suiteOfTests->addTest(new TestCaller<BlkDiagCovTest>(
    "blockThreeByThreeCovTest", 
    &BlkDiagCovTest::blockThreeByThreeCovTest ));

 suiteOfTests->addTest(new TestCaller<BlkDiagCovTest>(
    "blockFourByFourCovTest", 
    &BlkDiagCovTest::blockFourByFourCovTest ));

 suiteOfTests->addTest(new TestCaller<BlkDiagCovTest>(
    "blockFourByFourCov_oneByOneDiagSameAsPrev_Test", 
    &BlkDiagCovTest::blockFourByFourCov_oneByOneDiagSameAsPrev_Test ));

 suiteOfTests->addTest(new TestCaller<BlkDiagCovTest>(
    "blockFourByFourCov_twoByTwoFullSameAsPrev_Test", 
    &BlkDiagCovTest::blockFourByFourCov_twoByTwoFullSameAsPrev_Test ));

  return suiteOfTests;
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

void BlkDiagCovTest::isCachingProperlyTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Prepare the Block Diagonal covariance matrix.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  const int nRow = 3;

  // Set the number of rows in the derivative of the covariance matrix.
  int nCov_parRow = nRow * nRow;

  // Construct the covariance matrices.
  valarray<bool>  minRepFixedIn( 3 );
  minRepFixedIn = false;
  int nBlocks = 2;
  valarray<covStruct> blockStruct( nBlocks );
  blockStruct[0] = DIAGONAL;
  blockStruct[1] = DIAGONAL; 
  valarray<int> blockDims( nBlocks );
  blockDims[0] = 1;
  blockDims[1] = 2;
  valarray<bool>  blockSameAsPrev( nBlocks );
  blockSameAsPrev = false;
  
  BlkDiagCov blkOmega( nRow, minRepFixedIn, blockStruct, 
                      blockDims, blockSameAsPrev );

  // Get the number of parameters.
  int nPar = blkOmega.getNPar();
  assert( nPar == 3 );


  // Initialize the current value for the parameters.
  valarray<double> par( nPar );
  par[0] = 4.0;
  par[1] = 0.1;
  par[2] = -3.0;



  //------------------------------------------------------------
  // Prepare to see if values are being cached.
  //------------------------------------------------------------
  valarray<double> blkOmegaCov    ( nRow * nRow );
  valarray<double> blkOmegaCov_par( nRow * nRow * nPar );
  valarray<double> blkOmegaInv    ( nRow * nRow );
  valarray<double> blkOmegaInv_par( nRow * nRow * nPar );

  valarray<double> blkOparLow( nPar );
  valarray<double> blkOparUp ( nPar );

  // Get the limits for the covariance matrix parameters.
  blkOmega.getParLimits( blkOparLow, blkOparUp );


  // Evaluate the covariance matrix, its derivative, its inverse,
  // and the derivative of its inverse at the current parameter
  // value.
  blkOmega.setPar( par );
  blkOmega.cov    ( blkOmegaCov );
  blkOmega.cov_par( blkOmegaCov_par );
  blkOmega.inv    ( blkOmegaInv );
  blkOmega.inv_par( blkOmegaInv_par );

 
  //------------------------------------------------------------
  // See if cached values are used when the parameter changes.
  //------------------------------------------------------------

  // Evaluate the quantities at a different parameter value.
  // The cached values should not be used in this case.
  blkOmega.setPar( blkOparLow );

  blkOmega.cov    ( blkOmegaCov );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for blkOmegaCov was used when it was not valid.",
    blkOmega.getUsedCachedCov() == false );

  blkOmega.cov_par( blkOmegaCov_par );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for blkOmegaCov_par was used when it was not valid.",
    blkOmega.getUsedCachedCov_par() == false );

  blkOmega.inv    ( blkOmegaInv );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for blkOmegaInv was used when it was not valid.",
    blkOmega.getUsedCachedInv() == false );

  blkOmega.inv_par( blkOmegaInv_par );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for blkOmegaInv_par was used when it was not valid.",
    blkOmega.getUsedCachedInv_par() == false );


  //------------------------------------------------------------
  // See if cached values are used when the parameter does not change.
  //------------------------------------------------------------

  // Evaluate the quantities at the same parameter value.
  // The cached values should be used in this case.
  blkOmega.setPar( blkOparLow );

  blkOmega.cov    ( blkOmegaCov );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for blkOmegaCov was not used when it was valid.",
    blkOmega.getUsedCachedCov() == true );

  blkOmega.cov_par( blkOmegaCov_par );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for blkOmegaCov_par was not used when it was valid.",
    blkOmega.getUsedCachedCov_par() == true );

  blkOmega.inv    ( blkOmegaInv );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for blkOmegaInv was not used when it was valid.",
    blkOmega.getUsedCachedInv() == true );

  blkOmega.inv_par( blkOmegaInv_par );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for blkOmegaInv_par was not used when it was valid.",
    blkOmega.getUsedCachedInv_par() == true );

}

 

/*************************************************************************
 *
 * Function: blockThreeByThreeCovTest
 *
 *
 * The goal of this test is to check that the covariance class works
 * for the case of a 3x3 block diagonal cov matrix composed of 
 * a 1x1 and a 2x2 diagonal block.  The block structured 3x3 will be 
 * compared to a single diagonal 3x3.
 *
 *************************************************************************/

void BlkDiagCovTest::blockThreeByThreeCovTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i;
  int k;



  //------------------------------------------------------------
  // Calculate the known values (for DIAGONAL case)
  //------------------------------------------------------------


  // Set the number of rows in the covariance matrix.
  const int nRow = 3;

  // Set the number of rows in the derivative of the covariance matrix.
  int nCov_parRow = nRow * nRow;

  // Construct the covariance matrices.
  DiagCov omega( nRow );

  // Get the number of parameters.
  int nPar = omega.getNPar();
  assert( nPar == 3 );

  // Initialize the current value for the parameters.
  valarray<double> par( nPar );
  par[0] = 4.0;
  par[1] = 0.1;
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

  valarray<double> omegaAtParLow( nRow * nRow );
  valarray<double> omegaAtParUp ( nRow * nRow );

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
 
  // Calculate the minimal representation for the covariance matrix
  // and its derivative.
  omega.calcCovMinRep    ( omegaCov,           omegaMinRep );
  omega.calcCovMinRep_par( omegaCov_par, nPar, omegaMinRep_par );

  // Multiply the covariance matrix and its inverse.
  omegaCovTimesInv = multiply( omegaCov, nRow, omegaInv, nRow );

  //------------------------------------------------------------
  // Prepare the Block Diagonal covariance matrix.
  //------------------------------------------------------------

 // Set the number of rows in the covariance matrix.
  //const int nRow = 3;

  // Set the number of rows in the derivative of the covariance matrix.
  //int nCov_parRow = nRow * nRow;

  // Construct the covariance matrices.
  valarray<bool>  minRepFixedIn( 3 );
  minRepFixedIn = false;
  int nBlocks = 2;
  valarray<covStruct> blockStruct( nBlocks );
  blockStruct[0] = DIAGONAL;
  blockStruct[1] = DIAGONAL; 
  valarray<int> blockDims( nBlocks );
  blockDims[0] = 2;
  blockDims[1] = 1;
  valarray<bool>  blockSameAsPrev( nBlocks );
  blockSameAsPrev = false;
  

  // Construct the covariance matrices.
  BlkDiagCov blkOmega( nRow, minRepFixedIn, blockStruct, 
                      blockDims, blockSameAsPrev );

  // Get the number of parameters.
  nPar = blkOmega.getNPar();
  assert( nPar == 3 );


  // Initialize the current value for the parameters.
  par.resize( nPar );
  par[0] = 4.0;
  par[1] = 0.1;
  par[2] = -3.0;

  // Set the current value for the parameters.
  blkOmega.setPar( par );

  //------------------------------------------------------------
  // Calculate various quantities for the test.
  //------------------------------------------------------------

  valarray<double> blkOmegaCov    ( nRow * nRow );
  valarray<double> blkOmegaCov_par( nRow * nRow * nPar );
  valarray<double> blkOmegaInv    ( nRow * nRow );
  valarray<double> blkOmegaInv_par( nRow * nRow * nPar );

  valarray<double> blkOparLow( nPar );
  valarray<double> blkOparUp ( nPar );

  valarray<double> blkOmegaAtParLow( nRow * nRow );
  valarray<double> blkOmegaAtParUp ( nRow * nRow );

  valarray<double> blkOmegaMinRep    ( nPar );
  valarray<double> blkOmegaMinRep_par( nPar * nPar );

  valarray<double> blkOmegaCovTimesInv( nRow * nRow );

  // Calculate the covariance matrix, its derivative, its inverse,
  // and the derivative of its inverse.
  blkOmega.cov    ( blkOmegaCov );
  blkOmega.cov_par( blkOmegaCov_par );
  blkOmega.inv    ( blkOmegaInv );
  blkOmega.inv_par( blkOmegaInv_par );
  //cout << " omegaCov_par" << omegaCov_par <<  endl;
  //cout << " blkOmegaCov_par" << blkOmegaCov_par <<  endl;
  //cout << " omegaInv_par" << omegaInv_par <<  endl;
  //cout << " blkOmegaInv_par" << blkOmegaInv_par <<  endl;

  // Get the limits for the covariance matrix parameters.
  blkOmega.getParLimits( blkOparLow, blkOparUp );//     (i)     
  
  // Evaluate the covariance matrix at the upper and lower limits
  // for the parameters.
  blkOmega.setPar( blkOparLow );
  blkOmega.cov( blkOmegaAtParLow );
  blkOmega.setPar( blkOparUp );
  blkOmega.cov( blkOmegaAtParUp );
 
  // Calculate the minimal representation for the covariance matrix
  // and its derivative.
  blkOmega.calcCovMinRep    ( blkOmegaCov,           blkOmegaMinRep );
  blkOmega.calcCovMinRep_par( blkOmegaCov_par, nPar, blkOmegaMinRep_par );
  blkOmegaCovTimesInv = multiply( blkOmegaCov, nRow, blkOmegaInv, nRow );
  //cout << " omegaMinRep_par" << omegaMinRep_par <<  endl;
  //cout << " blkOmegaMinRep_par" << blkOmegaMinRep_par <<  endl;


  // The covariance matrix multiplied by its inverse should be
  // equal to the identity matrix.
  //  identity( nRow, omegaCovTimesInvKnown );


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  compareToKnown( 
    omegaCov,
    blkOmegaCov,
    "blkOmegaCov",
    tol );

  compareToKnown( 
    omegaCov_par,
    blkOmegaCov_par,
    "blkOmegaCov_par",
    tol );

  compareToKnown( 
    omegaInv,
    blkOmegaInv,
    "blkOmegaInv",
    tol );

  compareToKnown( 
    omegaInv_par,
    blkOmegaInv_par,
    "blkOmegaInv_par",
    tol );

  compareToKnown( 
    omegaAtParLow,
    blkOmegaAtParLow,
    "blkOmega at parLow",
    tol );

  compareToKnown( 
    omegaAtParUp,
    blkOmegaAtParUp,
    "blkOmega at parUp",
    tol );

  compareToKnown( 
    omegaMinRep,
    blkOmegaMinRep,
    "blkOmegaMinRep",
    tol );

  compareToKnown( 
    omegaMinRep_par,
    blkOmegaMinRep_par,
    "blkOmegaMinRep_par",
    tol );

  compareToKnown( 
    omegaCovTimesInv,
    blkOmegaCovTimesInv,
    "blkOmegaCov times blkOmegaInv",
    tol );

}


/*************************************************************************
 *
 * Function: blockFourByFourCovTest
 *
 *
 * The goal of this test is to check that the covariance class works
 * for the case of a 4x4 block diagonal cov matrix composed of two
 * 1x1 diagonal blocks and a 2x2 full block.  The block structured 4x4  
 * will be compared to a single full 4x4 (some culling is needed since
 * a full 4x4 has nPar=10, whereas this block structured cov has nPar=5).
 *
 *************************************************************************/

void BlkDiagCovTest::blockFourByFourCovTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i;
  int k;



  //------------------------------------------------------------
  // Calculate the known values (for FULL case)
  //------------------------------------------------------------


  // Set the number of rows in the covariance matrix.
  const int nRow = 4;

  // Set the number of rows in the derivative of the covariance matrix.
  int nCov_parRow = nRow * nRow;

  // Construct the covariance matrices.
  FullCov omega( nRow );

  // Get the number of parameters.
  int nPar = omega.getNPar();
  assert( nPar == 10 );

  // Initialize the current value for the parameters.
  double c_par[10] = {4,0,-3,0,0,5,0,0,1,3};
  valarray<double> par( c_par, nPar );
  
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

  // Calculate the minimal representation for the covariance matrix
  // and its derivative.
  omega.calcCovMinRep    ( omegaCov,           omegaMinRep );
  omega.calcCovMinRep_par( omegaCov_par, nPar, omegaMinRep_par );

  // Multiply the covariance matrix and its inverse.
  omegaCovTimesInv = multiply( omegaCov, nRow, omegaInv, nRow );

  // **************************************************************
  // NOTE:  omegaCov_par, omegaInv_par omegaMinRep_par ,omegaMinRep,
  // parLow, parUp
  // were computed based on 10 paramters (5 of which should be == 0).
  // The BlockDiag formulation has only 5 parameters...  Parse so
  // that the two are comparable (keep elements corresponding to derivitives
  // in paramters 1,3,6,9,10).

  int newNPar = 5;
  valarray<double> omegaCovXX_par( nRow * nRow * newNPar );
  valarray<double> omegaInvXX_par( nRow * nRow * newNPar );
  valarray<double> omegaMinRepXX_par( newNPar * newNPar );
  valarray<double> omegaMinRepXX( newNPar );
  valarray<double> parLowXX( newNPar );
  valarray<double> parUpXX( newNPar );  

  int nRow2 =  nRow * nRow;
  int start = 0;
  int startXX = 0;
  int start2XX = 0;
  for (int k =1; k <= 10; k++)
  {
    if (k==1 || k==3 || k==6 || k==9 || k==10) 
    {
      omegaCovXX_par[ slice( startXX, nRow2, 1 ) ] = omegaCov_par[ slice( start, nRow2, 1 ) ];
      omegaInvXX_par[ slice( startXX, nRow2, 1 ) ] = omegaInv_par[ slice( start, nRow2, 1 ) ];
      startXX += nRow2;

      parLowXX[ start2XX ]  = parLow[ k-1 ]; 
      parUpXX[ start2XX++ ] = parUp[ k-1 ]; 
    }
    start += nRow2;
  }


  startXX = 0;
  for (int j =1; j <= 10; j++)
  {
    if (j==1 || j==5 || j==8 || j==9 || j==10) 
    {
      omegaMinRepXX[ startXX++ ] = omegaMinRep[ j-1 ];
    }
  }

 
  startXX = 0;
  for (int k =1; k <= 10; k++)
  {
    for (int j =1; j <= 10; j++)
    {
      if (k==1 || k==3 || k==6 || k==9 || k==10)
      {
             if (j==1 || j==5 || j==8 || j==9 || j==10) 
        {
         omegaMinRepXX_par[ startXX++ ] = omegaMinRep_par[ (k-1)*10 +j-1 ];
        } 
      }
    }
  }
  // **************************************************************


  //------------------------------------------------------------
  // Prepare the Block Diagonal covariance matrix.
  //------------------------------------------------------------

 // Set the number of rows in the covariance matrix.
  //const int nRow = 4; ...set above

  // Set the number of rows in the derivative of the covariance matrix.
  //int nCov_parRow = nRow * nRow;  ...set above

  // Construct the covariance matrices.
  valarray<bool>  minRepFixedIn( 5 );
  minRepFixedIn = false;
  int nBlocks = 3;
  valarray<covStruct> blockStruct( nBlocks );
  blockStruct[0] = DIAGONAL;
  blockStruct[1] = DIAGONAL;
  blockStruct[2] = FULL;
  valarray<int> blockDims( nBlocks );
  blockDims[0] = 1;
  blockDims[1] = 1;
  blockDims[2] = 2;
  valarray<bool>  blockSameAsPrev( nBlocks );
  blockSameAsPrev = false;
  

  // Construct the covariance matrices.
  BlkDiagCov blkOmega( nRow, minRepFixedIn, blockStruct, 
                      blockDims, blockSameAsPrev );

  // Get the number of parameters.
  nPar = blkOmega.getNPar();
  assert( nPar == 5 );


  // Initialize the current value for the parameters.
  double c_parBlk[5] = {4,-3,5,1,3};
  valarray<double> BlkOpar( c_parBlk, nPar );
  //par[0] = 4.0;
  //par[1] = 0.1;
  //par[2] = -3.0;

  // Set the current value for the parameters.
  blkOmega.setPar( BlkOpar );

  // Initialize the current value for the parameter mask.
  valarray<bool> parMask( nPar );
  parMask[0] = true;
  parMask[1] = false;
  parMask[2] = true;
  parMask[3] = false;
  parMask[4] = false;

  //------------------------------------------------------------
  // Calculate various quantities for the test.
  //------------------------------------------------------------

  valarray<double> blkOmegaCov    ( nRow * nRow );
  valarray<double> blkOmegaCov_par( nRow * nRow * nPar );
  valarray<double> blkOmegaInv    ( nRow * nRow );
  valarray<double> blkOmegaInv_par( nRow * nRow * nPar );

  valarray<double> blkOparLow( nPar );
  valarray<double> blkOparUp ( nPar );

  valarray<double> blkOmegaMinRep    ( nPar );
  valarray<double> blkOmegaMinRep_par( nPar * nPar );
  valarray<bool>   blkOmegaMinRepMask( nPar );

  valarray<double> blkOmegaCovTimesInv( nRow * nRow );

  //Revisit - Dave: remove print statements 2006-01-24

  // Calculate the covariance matrix, its derivative, its inverse,
  // and the derivative of its inverse.
  blkOmega.cov    ( blkOmegaCov );
  //cout << " omegaCov" << omegaCov <<  endl;
  //cout << " blkOmegaCov" << blkOmegaCov <<  endl;
  blkOmega.cov_par( blkOmegaCov_par );
  //cout << " omegaCovXX_par" << omegaCovXX_par <<  endl;
  //cout << " blkOmegaCov_par" << blkOmegaCov_par <<  endl;
  blkOmega.inv    ( blkOmegaInv );
  //cout << " omegaInv" << omegaInv <<  endl;
  //cout << " blkOmegaInv" << blkOmegaInv <<  endl;
  blkOmega.inv_par( blkOmegaInv_par );
  //cout << " omegaInvXX_par" << omegaInvXX_par <<  endl;
  //cout << " blkOmegaInv_par" << blkOmegaInv_par <<  endl;

  // Get the limits for the covariance matrix parameters.
  blkOmega.getParLimits( blkOparLow, blkOparUp ); 
  //cout << "   parLowXX" <<   parLowXX <<  endl;
  //cout << "   parUpXX" <<   parUpXX <<  endl;
  //cout << "   blkOparLow" <<   blkOparLow <<  endl;
  //cout << "   blkOparUp" <<   blkOparUp <<  endl;

  // Calculate the minimal representation for the covariance matrix
  // and its derivative.
  blkOmega.calcCovMinRep    ( blkOmegaCov,           blkOmegaMinRep );
  //cout << " omegaMinRep" << omegaMinRep <<  endl;
  //cout << " omegaMinRepXX" << omegaMinRepXX <<  endl;
  //cout << " blkOmegaMinRep" << blkOmegaMinRep <<  endl;
  blkOmega.calcCovMinRep_par( blkOmegaCov_par, nPar, blkOmegaMinRep_par );
  //cout << " omegaMinRep_par" << omegaMinRep_par <<  endl;
  //cout << " omegaMinRepXX_par" << omegaMinRepXX_par <<  endl;
  //cout << " blkOmegaMinRep_par" << blkOmegaMinRep_par <<  endl;

  // Calculate the mask for the minimal representation for the
  // covariance matrix.
  blkOmega.calcCovMinRepMask( parMask, blkOmegaMinRepMask );
  //calculate known mask (unfortunately, no re-ordering should take place)
  valarray<bool>   blkOmegaMinRepMaskKnown( nPar );
  blkOmegaMinRepMaskKnown = parMask;
  //cout << " blkOmegaMinRepMask" << blkOmegaMinRepMask <<  endl;
  //cout << " blkOmegaMinRepMaskKnown" << blkOmegaMinRepMaskKnown <<  endl;
  

  // Multiply the covariance matrix and its inverse.
  blkOmegaCovTimesInv = multiply( blkOmegaCov, nRow, blkOmegaInv, nRow );



  // The covariance matrix multiplied by its inverse should be
  // equal to the identity matrix.
  //  identity( nRow, omegaCovTimesInvKnown );


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  compareToKnown( 
    omegaCov,
    blkOmegaCov,
    "blkOmegaCov",
    tol );

  compareToKnown( 
    omegaCovXX_par,
    blkOmegaCov_par,
    "blkOmegaCov_par",
    tol );

  compareToKnown( 
    omegaInv,
    blkOmegaInv,
    "blkOmegaInv",
    tol );

  compareToKnown( 
    omegaInvXX_par,
    blkOmegaInv_par,
    "blkOmegaInv_par",
    tol );


  compareToKnown( 
    parLowXX,
    blkOparLow,
    "blkOparLow",
    tol );

 compareToKnown( 
    parUpXX,
    blkOparUp,
    "blkOparUp",
    tol );
 

  compareToKnown( 
    omegaMinRepXX,
    blkOmegaMinRep,
    "blkOmegaMinRep",
    tol );

  compareToKnown( 
    omegaMinRepXX_par,
    blkOmegaMinRep_par,
    "blkOmegaMinRep_par",
    tol );

  compareToKnown( 
    blkOmegaMinRepMask,
    blkOmegaMinRepMaskKnown,
    "omegaMinRepMask" );

  compareToKnown( 
    omegaCovTimesInv,
    blkOmegaCovTimesInv,
    "blkOmegaCov times blkOmegaInv",
    tol );

}


/*************************************************************************
 *
 * Function: blockFourByFourCov_oneByOneDiagSameAsPrev_Test
 *
 *
 * The goal of this test is to check that the covariance class works
 * for the case of a 4x4 block diagonal cov matrix composed of two
 * 1x1 diagonal blocks and a 2x2 full block.  
 *
 * The second 1x1 block is the same as the first one.
 *
 * The block structured 4x4 will be compared to a single full 4x4
 * (some culling is needed since a full 4x4 has nPar=10, whereas this
 * block structured cov has nPar=4).
 *
 *************************************************************************/

void BlkDiagCovTest::blockFourByFourCov_oneByOneDiagSameAsPrev_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i;
  int k;



  //------------------------------------------------------------
  // Calculate the known values (for FULL case)
  //------------------------------------------------------------


  // Set the number of rows in the covariance matrix.
  const int nRow = 4;

  // Set the number of rows in the derivative of the covariance matrix.
  int nCov_parRow = nRow * nRow;

  // Construct the covariance matrices.
  FullCov omega( nRow );

  // Get the number of parameters.
  int nPar = omega.getNPar();
  assert( nPar == 10 );

  // Initialize the current value for the parameters.
  double c_par[10] = {4,0,4,0,0,5,0,0,1,3};
  valarray<double> par( c_par, nPar );
  
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

  // Calculate the minimal representation for the covariance matrix
  // and its derivative.
  omega.calcCovMinRep    ( omegaCov,           omegaMinRep );
  omega.calcCovMinRep_par( omegaCov_par, nPar, omegaMinRep_par );

  // Multiply the covariance matrix and its inverse.
  omegaCovTimesInv = multiply( omegaCov, nRow, omegaInv, nRow );

  // **************************************************************
  // NOTE:  omegaCov_par, omegaInv_par omegaMinRep_par ,omegaMinRep,
  // parLow, parUp
  // were computed based on 10 paramters (5 of which should be == 0).
  // The BlockDiag formulation has only 4 parameters...  Parse so
  // that the two are comparable (keep elements corresponding to derivitives
  // in paramters 1,6,9,10).

  int newNPar = 4;
  valarray<double> omegaCovXX_par( nRow * nRow * newNPar );
  valarray<double> omegaInvXX_par( nRow * nRow * newNPar );
  valarray<double> omegaMinRepXX_par( newNPar * newNPar );
  valarray<double> omegaMinRepXX( newNPar );
  valarray<double> parLowXX( newNPar );
  valarray<double> parUpXX( newNPar );  

  int nRow2 =  nRow * nRow;
  int start = 0;
  int startXX = 0;
  int start2XX = 0;
  for (int k =1; k <= 10; k++)
  {
    if (k==1 || k==6 || k==9 || k==10) 
    {
      omegaCovXX_par[ slice( startXX, nRow2, 1 ) ] = omegaCov_par[ slice( start, nRow2, 1 ) ];
      omegaInvXX_par[ slice( startXX, nRow2, 1 ) ] = omegaInv_par[ slice( start, nRow2, 1 ) ];
      startXX += nRow2;

      parLowXX[ start2XX ]  = parLow[ k-1 ]; 
      parUpXX[ start2XX++ ] = parUp[ k-1 ]; 
    }
    start += nRow2;
  }

  // Set the elements that are equal because their block is the same
  // as the previous.
  omegaCovXX_par[ nRow + 1 ] = omegaCov_par[ 0 ];
  omegaInvXX_par[ nRow + 1 ] = omegaInv_par[ 0 ];

  startXX = 0;
  for (int j =1; j <= 10; j++)
  {
    if (j==1 || j==8 || j==9 || j==10) 
    {
      omegaMinRepXX[ startXX++ ] = omegaMinRep[ j-1 ];
    }
  }

 
  startXX = 0;
  for (int k =1; k <= 10; k++)
  {
    for (int j =1; j <= 10; j++)
    {
      if (k==1 || k==6 || k==9 || k==10)
      {
             if (j==1 || j==8 || j==9 || j==10) 
        {
         omegaMinRepXX_par[ startXX++ ] = omegaMinRep_par[ (k-1)*10 +j-1 ];
        } 
      }
    }
  }
  // **************************************************************


  //------------------------------------------------------------
  // Prepare the Block Diagonal covariance matrix.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  //const int nRow = 4; ...set above

  // Set the number of rows in the derivative of the covariance matrix.
  //int nCov_parRow = nRow * nRow;  ...set above

  // Construct the covariance matrices.
  valarray<bool>  minRepFixedIn( 5 );
  minRepFixedIn = false;
  int nBlocks = 3;
  valarray<covStruct> blockStruct( nBlocks );
  blockStruct[0] = DIAGONAL;
  blockStruct[1] = DIAGONAL;
  blockStruct[2] = FULL;
  valarray<int> blockDims( nBlocks );
  blockDims[0] = 1;
  blockDims[1] = 1;
  blockDims[2] = 2;
  valarray<bool>  blockSameAsPrev( nBlocks );
  blockSameAsPrev[0] = false;
  blockSameAsPrev[1] = true;
  blockSameAsPrev[2] = false;
  

  // Construct the covariance matrices.
  BlkDiagCov blkOmega( nRow, minRepFixedIn, blockStruct, 
                      blockDims, blockSameAsPrev );

  // Get the number of parameters.
  nPar = blkOmega.getNPar();
  assert( nPar == 4 );


  // Initialize the current value for the parameters.
  double c_parBlk[4] = {4,5,1,3};
  valarray<double> BlkOpar( c_parBlk, nPar );

  // Set the current value for the parameters.
  blkOmega.setPar( BlkOpar );

  // Initialize the current value for the parameter mask.
  valarray<bool> parMask( nPar );
  parMask[0] = true;
  parMask[1] = true;
  parMask[2] = false;
  parMask[3] = false;

  //------------------------------------------------------------
  // Calculate various quantities for the test.
  //------------------------------------------------------------

  valarray<double> blkOmegaCov    ( nRow * nRow );
  valarray<double> blkOmegaCov_par( nRow * nRow * nPar );
  valarray<double> blkOmegaInv    ( nRow * nRow );
  valarray<double> blkOmegaInv_par( nRow * nRow * nPar );

  valarray<double> blkOparLow( nPar );
  valarray<double> blkOparUp ( nPar );

  valarray<double> blkOmegaMinRep    ( nPar );
  valarray<double> blkOmegaMinRep_par( nPar * nPar );
  valarray<bool>   blkOmegaMinRepMask( nPar );

  valarray<double> blkOmegaCovTimesInv( nRow * nRow );

  //Revisit - Dave: remove print statements 2006-01-24

  // Calculate the covariance matrix, its derivative, its inverse,
  // and the derivative of its inverse.
  blkOmega.cov    ( blkOmegaCov );
  //cout << " omegaCov" << omegaCov <<  endl;
  //cout << " blkOmegaCov" << blkOmegaCov <<  endl;
  blkOmega.cov_par( blkOmegaCov_par );
  //cout << " omegaCovXX_par" << omegaCovXX_par <<  endl;
  //cout << " blkOmegaCov_par" << blkOmegaCov_par <<  endl;
  blkOmega.inv    ( blkOmegaInv );
  //cout << " omegaInv" << omegaInv <<  endl;
  //cout << " blkOmegaInv" << blkOmegaInv <<  endl;
  blkOmega.inv_par( blkOmegaInv_par );
  //cout << " omegaInvXX_par" << omegaInvXX_par <<  endl;
  //cout << " blkOmegaInv_par" << blkOmegaInv_par <<  endl;

  // Get the limits for the covariance matrix parameters.
  blkOmega.getParLimits( blkOparLow, blkOparUp ); 
  //cout << "   parLowXX" <<   parLowXX <<  endl;
  //cout << "   parUpXX" <<   parUpXX <<  endl;
  //cout << "   blkOparLow" <<   blkOparLow <<  endl;
  //cout << "   blkOparUp" <<   blkOparUp <<  endl;

  // Calculate the minimal representation for the covariance matrix
  // and its derivative.
  blkOmega.calcCovMinRep    ( blkOmegaCov,           blkOmegaMinRep );
  //cout << " omegaMinRep" << omegaMinRep <<  endl;
  //cout << " omegaMinRepXX" << omegaMinRepXX <<  endl;
  //cout << " blkOmegaMinRep" << blkOmegaMinRep <<  endl;
  blkOmega.calcCovMinRep_par( blkOmegaCov_par, nPar, blkOmegaMinRep_par );
  //cout << " omegaMinRep_par" << omegaMinRep_par <<  endl;
  //cout << " omegaMinRepXX_par" << omegaMinRepXX_par <<  endl;
  //cout << " blkOmegaMinRep_par" << blkOmegaMinRep_par <<  endl;

  // Calculate the mask for the minimal representation for the
  // covariance matrix.
  blkOmega.calcCovMinRepMask( parMask, blkOmegaMinRepMask );
  //calculate known mask (unfortunately, no re-ordering should take place)
  valarray<bool>   blkOmegaMinRepMaskKnown( nPar );
  blkOmegaMinRepMaskKnown = parMask;
  //cout << " blkOmegaMinRepMask" << blkOmegaMinRepMask <<  endl;
  //cout << " blkOmegaMinRepMaskKnown" << blkOmegaMinRepMaskKnown <<  endl;
  

  // Multiply the covariance matrix and its inverse.
  blkOmegaCovTimesInv = multiply( blkOmegaCov, nRow, blkOmegaInv, nRow );



  // The covariance matrix multiplied by its inverse should be
  // equal to the identity matrix.
  //  identity( nRow, omegaCovTimesInvKnown );


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  compareToKnown( 
    omegaCov,
    blkOmegaCov,
    "blkOmegaCov",
    tol );

  compareToKnown( 
    omegaCovXX_par,
    blkOmegaCov_par,
    "blkOmegaCov_par",
    tol );

  compareToKnown( 
    omegaInv,
    blkOmegaInv,
    "blkOmegaInv",
    tol );

  compareToKnown( 
    omegaInvXX_par,
    blkOmegaInv_par,
    "blkOmegaInv_par",
    tol );


  compareToKnown( 
    parLowXX,
    blkOparLow,
    "blkOparLow",
    tol );

 compareToKnown( 
    parUpXX,
    blkOparUp,
    "blkOparUp",
    tol );
 

  compareToKnown( 
    omegaMinRepXX,
    blkOmegaMinRep,
    "blkOmegaMinRep",
    tol );

  compareToKnown( 
    omegaMinRepXX_par,
    blkOmegaMinRep_par,
    "blkOmegaMinRep_par",
    tol );

  compareToKnown( 
    blkOmegaMinRepMask,
    blkOmegaMinRepMaskKnown,
    "omegaMinRepMask" );

  compareToKnown( 
    omegaCovTimesInv,
    blkOmegaCovTimesInv,
    "blkOmegaCov times blkOmegaInv",
    tol );

}


/*************************************************************************
 *
 * Function: blockFourByFourCov_twoByTwoFullSameAsPrev_Test
 *
 *
 * The goal of this test is to check that the covariance class works
 * for the case of a 4x4 block diagonal cov matrix composed of two
 * 2x2 full blocks.  
 *
 * The second 2x2 block is the same as the first one.
 *
 * The block structured 4x4 will be compared to a single full 4x4
 * (some culling is needed since a full 4x4 has nPar=10, whereas this
 * block structured cov has nPar=3).
 *
 *************************************************************************/

void BlkDiagCovTest::blockFourByFourCov_twoByTwoFullSameAsPrev_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i;
  int k;



  //------------------------------------------------------------
  // Calculate the known values (for FULL case)
  //------------------------------------------------------------


  // Set the number of rows in the covariance matrix.
  const int nRow = 4;

  // Set the number of rows in the derivative of the covariance matrix.
  int nCov_parRow = nRow * nRow;

  // Construct the covariance matrices.
  FullCov omega( nRow );

  // Get the number of parameters.
  int nPar = omega.getNPar();
  assert( nPar == 10 );

  // Initialize the current value for the parameters.
  double c_par[10] = {5,1,3,0,0,5,0,0,1,3};
  valarray<double> par( c_par, nPar );
  
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

  // Calculate the minimal representation for the covariance matrix
  // and its derivative.
  omega.calcCovMinRep    ( omegaCov,           omegaMinRep );
  omega.calcCovMinRep_par( omegaCov_par, nPar, omegaMinRep_par );

  // Multiply the covariance matrix and its inverse.
  omegaCovTimesInv = multiply( omegaCov, nRow, omegaInv, nRow );

  // **************************************************************
  // NOTE:  omegaCov_par, omegaInv_par omegaMinRep_par ,omegaMinRep,
  // parLow, parUp
  // were computed based on 10 paramters (4 of which should be == 0).
  // The BlockDiag formulation has only 3 parameters...  Parse so
  // that the two are comparable (keep elements corresponding to derivitives
  // in paramters 1,2,3).

  int newNPar = 3;
  valarray<double> omegaCovXX_par( nRow * nRow * newNPar );
  valarray<double> omegaInvXX_par( nRow * nRow * newNPar );
  valarray<double> omegaMinRepXX_par( newNPar * newNPar );
  valarray<double> omegaMinRepXX( newNPar );
  valarray<double> parLowXX( newNPar );
  valarray<double> parUpXX( newNPar );  

  int nRow2 =  nRow * nRow;
  int start = 0;
  int startXX = 0;
  int start2XX = 0;
  for (int k =1; k <= 10; k++)
  {
    if (k==1 || k==2 || k==3) 
    {
      omegaCovXX_par[ slice( startXX, nRow2, 1 ) ] = omegaCov_par[ slice( start, nRow2, 1 ) ];
      omegaInvXX_par[ slice( startXX, nRow2, 1 ) ] = omegaInv_par[ slice( start, nRow2, 1 ) ];

      // Set the elements that are equal because their block is the same
      // as the previous.
      omegaCovXX_par[ slice( startXX + 2*nRow + 2, 6, 1 ) ] = omegaCov_par[ slice( start, 6, 1 ) ];
      omegaInvXX_par[ slice( startXX + 2*nRow + 2, 6, 1 ) ] = omegaInv_par[ slice( start, 6, 1 ) ];

      startXX += nRow2;

      parLowXX[ start2XX ]  = parLow[ k-1 ]; 
      parUpXX[ start2XX++ ] = parUp[ k-1 ]; 
    }
    start += nRow2;
  }

  startXX = 0;
  for (int j =1; j <= 10; j++)
  {
    if (j==1 || j==2 || j==5) 
    {
      omegaMinRepXX[ startXX++ ] = omegaMinRep[ j-1 ];
    }
  }

  startXX = 0;
  for (int k =1; k <= 10; k++)
  {
    for (int j =1; j <= 10; j++)
    {
      if (k==1 || k==2 || k==3)
      {
        if (j==1 || j==2 || j==5) 
        {
         omegaMinRepXX_par[ startXX++ ] = omegaMinRep_par[ (k-1)*10 +j-1 ];
        } 
      }
    }
  }
  // **************************************************************


  //------------------------------------------------------------
  // Prepare the Block Diagonal covariance matrix.
  //------------------------------------------------------------

 // Set the number of rows in the covariance matrix.
  //const int nRow = 4; ...set above

  // Set the number of rows in the derivative of the covariance matrix.
  //int nCov_parRow = nRow * nRow;  ...set above

  // Construct the covariance matrices.
  valarray<bool>  minRepFixedIn( 6 );
  minRepFixedIn = false;
  int nBlocks = 2;
  valarray<covStruct> blockStruct( nBlocks );
  blockStruct[0] = FULL;
  blockStruct[1] = FULL;
  valarray<int> blockDims( nBlocks );
  blockDims[0] = 2;
  blockDims[1] = 2;
  valarray<bool>  blockSameAsPrev( nBlocks );
  blockSameAsPrev[0] = false;
  blockSameAsPrev[1] = true;
  

  // Construct the covariance matrices.
  BlkDiagCov blkOmega( nRow, minRepFixedIn, blockStruct, 
                       blockDims, blockSameAsPrev );

  // Get the number of parameters.
  nPar = blkOmega.getNPar();
  assert( nPar == 3 );


  // Initialize the current value for the parameters.
  double c_parBlk[3] = {5,1,3};
  valarray<double> BlkOpar( c_parBlk, nPar );

  // Set the current value for the parameters.
  blkOmega.setPar( BlkOpar );

  // Initialize the current value for the parameter mask.
  valarray<bool> parMask( nPar );
  parMask[0] = true;
  parMask[1] = false;
  parMask[2] = false;

  //------------------------------------------------------------
  // Calculate various quantities for the test.
  //------------------------------------------------------------

  valarray<double> blkOmegaCov    ( nRow * nRow );
  valarray<double> blkOmegaCov_par( nRow * nRow * nPar );
  valarray<double> blkOmegaInv    ( nRow * nRow );
  valarray<double> blkOmegaInv_par( nRow * nRow * nPar );

  valarray<double> blkOparLow( nPar );
  valarray<double> blkOparUp ( nPar );

  valarray<double> blkOmegaMinRep    ( nPar );
  valarray<double> blkOmegaMinRep_par( nPar * nPar );
  valarray<bool>   blkOmegaMinRepMask( nPar );

  valarray<double> blkOmegaCovTimesInv( nRow * nRow );

  //Revisit - Dave: remove print statements 2006-01-24

  // Calculate the covariance matrix, its derivative, its inverse,
  // and the derivative of its inverse.
  blkOmega.cov    ( blkOmegaCov );
  //cout << " omegaCov" << omegaCov <<  endl;
  //cout << " blkOmegaCov" << blkOmegaCov <<  endl;
  blkOmega.cov_par( blkOmegaCov_par );
  //cout << " omegaCovXX_par" << omegaCovXX_par <<  endl;
  //cout << " blkOmegaCov_par" << blkOmegaCov_par <<  endl;
  blkOmega.inv    ( blkOmegaInv );
  //cout << " omegaInv" << omegaInv <<  endl;
  //cout << " blkOmegaInv" << blkOmegaInv <<  endl;
  blkOmega.inv_par( blkOmegaInv_par );
  //cout << " omegaInvXX_par" << omegaInvXX_par <<  endl;
  //cout << " blkOmegaInv_par" << blkOmegaInv_par <<  endl;

  // Get the limits for the covariance matrix parameters.
  blkOmega.getParLimits( blkOparLow, blkOparUp ); 
  //cout << "   parLowXX" <<   parLowXX <<  endl;
  //cout << "   parUpXX" <<   parUpXX <<  endl;
  //cout << "   blkOparLow" <<   blkOparLow <<  endl;
  //cout << "   blkOparUp" <<   blkOparUp <<  endl;

  // Calculate the minimal representation for the covariance matrix
  // and its derivative.
  blkOmega.calcCovMinRep    ( blkOmegaCov,           blkOmegaMinRep );
  //cout << " omegaMinRep" << omegaMinRep <<  endl;
  //cout << " omegaMinRepXX" << omegaMinRepXX <<  endl;
  //cout << " blkOmegaMinRep" << blkOmegaMinRep <<  endl;
  blkOmega.calcCovMinRep_par( blkOmegaCov_par, nPar, blkOmegaMinRep_par );
  //cout << " omegaMinRep_par" << omegaMinRep_par <<  endl;
  //cout << " omegaMinRepXX_par" << omegaMinRepXX_par <<  endl;
  //cout << " blkOmegaMinRep_par" << blkOmegaMinRep_par <<  endl;

  // Calculate the mask for the minimal representation for the
  // covariance matrix.
  blkOmega.calcCovMinRepMask( parMask, blkOmegaMinRepMask );
  //calculate known mask (unfortunately, no re-ordering should take place)
  valarray<bool>   blkOmegaMinRepMaskKnown( nPar );
  blkOmegaMinRepMaskKnown = parMask;
  //cout << " blkOmegaMinRepMask" << blkOmegaMinRepMask <<  endl;
  //cout << " blkOmegaMinRepMaskKnown" << blkOmegaMinRepMaskKnown <<  endl;
  

  // Multiply the covariance matrix and its inverse.
  blkOmegaCovTimesInv = multiply( blkOmegaCov, nRow, blkOmegaInv, nRow );



  // The covariance matrix multiplied by its inverse should be
  // equal to the identity matrix.
  //  identity( nRow, omegaCovTimesInvKnown );


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  compareToKnown( 
    omegaCov,
    blkOmegaCov,
    "blkOmegaCov",
    tol );

  compareToKnown( 
    omegaCovXX_par,
    blkOmegaCov_par,
    "blkOmegaCov_par",
    tol );

  compareToKnown( 
    omegaInv,
    blkOmegaInv,
    "blkOmegaInv",
    tol );

  compareToKnown( 
    omegaInvXX_par,
    blkOmegaInv_par,
    "blkOmegaInv_par",
    tol );


  compareToKnown( 
    parLowXX,
    blkOparLow,
    "blkOparLow",
    tol );

 compareToKnown( 
    parUpXX,
    blkOparUp,
    "blkOparUp",
    tol );
 

  compareToKnown( 
    omegaMinRepXX,
    blkOmegaMinRep,
    "blkOmegaMinRep",
    tol );

  compareToKnown( 
    omegaMinRepXX_par,
    blkOmegaMinRep_par,
    "blkOmegaMinRep_par",
    tol );

  compareToKnown( 
    blkOmegaMinRepMask,
    blkOmegaMinRepMaskKnown,
    "omegaMinRepMask" );

  compareToKnown( 
    omegaCovTimesInv,
    blkOmegaCovTimesInv,
    "blkOmegaCov times blkOmegaInv",
    tol );

}


