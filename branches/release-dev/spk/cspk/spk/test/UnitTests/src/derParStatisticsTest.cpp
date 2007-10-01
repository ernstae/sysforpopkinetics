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
 * File: derParStatisticsTest.cpp
 *
 *
 * Unit test for the function derParStatistics.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK test suite header files.
#include "derParStatisticsTest.h"

// SPK library header files.
#include "../../../spk/derParStatistics.h"
#include "../../../spk/multiply.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/statistics.h"

// CppUnit framework header files.
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

// Standard library header files.
#include <cassert>
#include <cmath>

using namespace CppUnit;
using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: setUp
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void derParStatisticsTest::setUp()
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

void derParStatisticsTest::tearDown()
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

Test* derParStatisticsTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "derParStatisticsTest" );

  suiteOfTests->addTest(new TestCaller<derParStatisticsTest>(
    "twoByTwoCovTest", 
    &derParStatisticsTest::twoByTwoCovTest ));

  suiteOfTests->addTest(new TestCaller<derParStatisticsTest>(
    "testWrapper", 
    &derParStatisticsTest::testWrapper ));

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: twoByTwoCovTest
 *
 *
 * The goal of this test is to check that the derived statistics
 * function works for the case of a two-by-two covariance matrix.
 *
 *************************************************************************/

void derParStatisticsTest::twoByTwoCovTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Prepare quantities related to the original parameter.
  //------------------------------------------------------------

  // Set the number of elements in the original parameter x.
  const int nX = 2;

  // Set the number of degrees of freedom.
  int nDegFreedom = 2;

  // Set the original parameter.
  valarray<double> x( nX );
  x[0] = 13.0;
  x[1] = -0.2;

  // Set the covariance of x,
  //
  //                  -        -
  //                 |  1    2  |
  //     cov[ x ] =  |          |   .
  //                 |  2    5  |
  //                  -        -
  //
  valarray<double> xCov( nX * nX );
  xCov[0 + 0 * nX] = 1.0;
  xCov[1 + 0 * nX] = 2.0;
  xCov[0 + 1 * nX] = 2.0;
  xCov[1 + 1 * nX] = 5.0;

  // Get the correction factor that depends on the number of degrees
  // of freedom and that is used to calculate the confidence intervals
  //                        
  //       (begin)
  //     CI       [ x ]     =  x     -  t( nDegFreedom )  SE[ x ]    ,
  //                   (i)      (i)                              (i)
  //
  //       (end)
  //     CI       [ x ]     =  x     +  t( nDegFreedom )  SE[ x ]     .
  //                   (i)      (i)                              (i)
  //
  valarray<double> xSETemp( nX );
  valarray<double> xCITemp( 2 * nX );
  valarray<double>* pNull = 0;
  statistics(
    x,
    xCov,
    nDegFreedom,
    &xSETemp,
    pNull,
    pNull,
    &xCITemp );
  double t = ( xCITemp[2] - xCITemp[0] ) / ( 2.0 * xSETemp[0] );


  //------------------------------------------------------------
  // Prepare quantities related to the derived parameter.
  //------------------------------------------------------------

  // Set the number of elements in the derived parameter z.
  const int nZ = 2;
  assert( nZ == nX );

  // Set the derived parameter,
  //
  //                 -               -
  //                |    x   +  2 x   |
  //                |     0        1  |
  //     z( x )  =  |                 |   .
  //                |  3 x   +  4 x   |
  //                |     0        1  |
  //                 -               -
  //
  valarray<double> z( nZ );
  z[0] =       x[0] + 2.0 * x[1];
  z[1] = 3.0 * x[0] + 4.0 * x[1];

  // Set the derivative of the derived parameter with respect to the
  // original parameter,
  //
  //                     -        -
  //                    |  1    2  |
  //     d   z( x )  =  |          |   .
  //      x             |  3    4  |
  //                     -        -
  //
  valarray<double> z_x( nZ * nZ );
  z_x[0 + 0 * nZ] = 1.0;
  z_x[1 + 0 * nZ] = 3.0;
  z_x[0 + 1 * nZ] = 2.0;
  z_x[1 + 1 * nZ] = 4.0;

  valarray<double> zCovOut( nZ * nZ );
  valarray<double> zSEOut ( nZ );
  valarray<double> zCorOut( nZ * nZ );
  valarray<double> zCVOut ( nZ );
  valarray<double> zCIOut ( 2 * nZ );


  //------------------------------------------------------------
  // Calculate the statistics for the derived parameter.
  //------------------------------------------------------------

  // Calculate the statistics.
  derParStatistics(
    xCov,
    z,
    z_x,
    nDegFreedom,
    &zCovOut,
    &zSEOut,
    &zCorOut,
    &zCVOut,
    &zCIOut );


  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  valarray<double> zCovKnown( nZ * nZ );
  valarray<double> zSEKnown ( nZ );
  valarray<double> zCorKnown( nZ * nZ );
  valarray<double> zCVKnown ( nZ );
  valarray<double> zCIKnown ( 2 * nZ );

  // Set the known covariance of z(x),
  //
  //                     -          -
  //                    |  29    63  |
  //     cov[ z(x) ] =  |            |   .
  //                    |  63   137  |
  //                     -          -
  //
  zCovKnown[0 + 0 * nZ] = 29.0;
  zCovKnown[1 + 0 * nZ] = 63.0;
  zCovKnown[0 + 1 * nZ] = 63.0;
  zCovKnown[1 + 1 * nZ] = 137.0;

  // Set the known standard errors for z(x),
  //
  //                        -                  -  1/2
  //     SE[ z(x) ]     =  |  cov[ z(x) ]       |      .
  //               (i)     |             (i,i)  |
  //                        -                  -
  //
  zSEKnown[0] = sqrt( zCovKnown[0 + 0 * nZ] );
  zSEKnown[1] = sqrt( zCovKnown[1 + 1 * nZ] );

  // Set the known correlation of z(x),
  //
  //                                           cov[ z(x) ]       
  //                                                      (i,j)  
  //     corr[ z(x) ]       =  -------------------------------------------------  .
  //                 (i,j)       -                                    -  1/2
  //                            |  cov[ z(x) ]       cov[ z(x) ]       |
  //                            |             (i,i)             (j,j)  |
  //                             -                                    -
  //
  zCorKnown[0 + 0 * nZ] = 1.0;
  zCorKnown[1 + 0 * nZ] = zCovKnown[1 + 0 * nZ] / 
                            sqrt( zCovKnown[1 + 1 * nZ] * 
				  zCovKnown[0 + 0 * nZ] );
  zCorKnown[0 + 1 * nZ] = zCovKnown[0 + 1 * nZ] / 
                            sqrt( zCovKnown[0 + 0 * nZ] * 
				  zCovKnown[1 + 1 * nZ] );
  zCorKnown[1 + 1 * nZ] = 1.0;

  // Set the known coefficients of variation for z(x),
  //
  //                             SE[ z(x) ]
  //                                       (i)
  //     CV[ z(x) ]     =  100  ---------------  .
  //               (i)             z(x)
  //                                  (i)
  //
  zCVKnown[0] = zSEKnown[0] / z[0] * 100.0;
  zCVKnown[1] = zSEKnown[1] / z[1] * 100.0;

  // Set the known confidence intervals for z(x),
  //                        
  //       (begin)
  //     CI       [ z(x) ]     =  z(x)     -  t( nDegFreedom )  SE[ z(x) ]    ,
  //                      (i)         (i)                                 (i)
  //
  //       (end)
  //     CI       [ z(x) ]     =  z(x)     +  t( nDegFreedom )  SE[ z(x) ]     .
  //                      (i)         (i)                                 (i)
  //
  zCIKnown[0] = z[0] - t * zSEKnown[0];
  zCIKnown[1] = z[1] - t * zSEKnown[1];
  zCIKnown[2] = z[0] + t * zSEKnown[0];
  zCIKnown[3] = z[1] + t * zSEKnown[1];


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  int k;

  double tol = 1.0e-13;

  // Check the covariance of the derived parameter.
  for ( k = 0; k < nZ * nZ; k++ )
  {
    CPPUNIT_ASSERT_DOUBLES_EQUAL( zCovOut[k], zCovKnown[k], tol );
  }

  // Check the standard errors of the derived parameter.
  for ( k = 0; k < nZ; k++ )
  {
    CPPUNIT_ASSERT_DOUBLES_EQUAL( zSEOut[k], zSEKnown[k], tol );
  }

  // Check the correlation of the derived parameter.
  for ( k = 0; k < nZ * nZ; k++ )
  {
    CPPUNIT_ASSERT_DOUBLES_EQUAL( zCorOut[k], zCorKnown[k], tol );
  }

  // Check the coefficients of variation of the derived parameter.
  for ( k = 0; k < nZ; k++ )
  {
    CPPUNIT_ASSERT_DOUBLES_EQUAL( zCVOut[k], zCVKnown[k], tol );
  }

  // Check the confidence intervals of the derived parameter.
  for ( k = 0; k < 2 * nZ; k++ )
  {
    CPPUNIT_ASSERT_DOUBLES_EQUAL( zCIOut[k], zCIKnown[k], tol );
  }
}

void derParStatisticsTest::testWrapper()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Prepare quantities related to the original parameter.
  //------------------------------------------------------------

  // Set the number of supressed elements in the parameter vector x.
  const int nFixed = 1;

  // Set the number of elements in the original parameter x.
  const int nX = 2 + nFixed;

  // Set the number of degrees of freedom.
  int nDegFreedom = 2 + nFixed;

  // Set the original parameter.
  valarray<double> x( 0.0, nX );
  x[0] = 13.0;
  x[2] = -0.2;

  // Set the mask indicating which x(i) are valid.
  valarray<bool> xMask( nX );
  xMask[0] = true;
  xMask[1] = false;
  xMask[2] = true;

  // Set the covariance of x,
  //
  //                  -        -
  //                 |  1  0  2  |
  //     cov[ x ] =  |  0  0  0  |   .
  //                 |  2  0  5  |
  //                  -        -
  //
  valarray<double> xCov( 0.0, nX * nX );
  xCov[0 + 0 * nX] = 1.0;
  xCov[2 + 0 * nX] = 2.0;
  xCov[0 + 2 * nX] = 2.0;
  xCov[2 + 2 * nX] = 5.0;

  // Get the correction factor that depends on the number of degrees
  // of freedom and that is used to calculate the confidence intervals
  //                        
  //       (begin)
  //     CI       [ x ]     =  x     -  t( nDegFreedom )  SE[ x ]    ,
  //                   (i)      (i)                              (i)
  //
  //       (end)
  //     CI       [ x ]     =  x     +  t( nDegFreedom )  SE[ x ]     .
  //                   (i)      (i)                              (i)
  //
  valarray<double> xSETemp( nX );
  valarray<double> xCITemp( 2 * nX );
  valarray<double>* pNull = 0;
  statistics(xMask,
	     x,
	     xCov,
	     nDegFreedom,
	     &xSETemp,
	     pNull,
	     pNull,
	     &xCITemp );

  double ts[] = { 12.706, 4.303, 3.182, 2.776, 2.571, 2.447, 
		 2.365, 2.306, 2.262, 2.228, 2.201, 2.179, 
		 2.160, 2.145, 2.131, 2.120, 2.110, 2.101, 
		 2.093, 2.086, 2.080, 2.074, 2.069, 2.064, 
		 2.060, 2.056, 2.052, 2.048, 2.045, 2.042 };
  
  double t;
  
  if( nDegFreedom <= 30 )
    t = ts[ nDegFreedom - 1 ];
  if( nDegFreedom > 30 && nDegFreedom <= 40 )
    t = 2.042 - ( nDegFreedom - 30 ) * 0.021 / 10.0;
  if( nDegFreedom > 40 && nDegFreedom <= 60 )
    t = 2.021 - ( nDegFreedom - 40 ) * 0.021 / 20.0;
  if( nDegFreedom > 60 && nDegFreedom <= 120 )
    t = 2.000 - ( nDegFreedom - 60 ) * 0.020 / 60.0;
  if( nDegFreedom > 120 )
    t = 1.960;
  //  double t = ( xCITemp[2] - xCITemp[0] ) / ( 2.0 * xSETemp[0] );

  //------------------------------------------------------------
  // Prepare quantities related to the derived parameter.
  //------------------------------------------------------------

  // Set the number of elements in the derived parameter z.
  const int nZ = nX;
  assert( nZ == nX );

  // Set the derived parameter,
  //
  //                 -               -
  //                |    x   +  2 x   |
  //                |     0        1  |
  //                |
  //     z( x )  =  |        0        |
  //                |
  //                |  3 x   +  4 x   |
  //                |     0        1  |
  //                 -               -
  //
  valarray<double> z( 0.0, nZ );
  z[0] =       x[0] + 2.0 * x[1];
  z[2] = 3.0 * x[0] + 4.0 * x[1];

  // Set the mask indicating which z(i) are valid.
  valarray<bool> zMask( xMask );

  // Set the derivative of the derived parameter with respect to the
  // original parameter,
  //
  //                     -        -
  //                    |  1  0  2  |
  //     d   z( x )  =  |  0  0  0  |   .
  //      x             |  3  0  4  |
  //                     -        -
  //
  valarray<double> z_x( 0.0, nZ * nZ );
  z_x[0 + 0 * nZ] = 1.0;
  z_x[2 + 0 * nZ] = 3.0;
  z_x[0 + 2 * nZ] = 2.0;
  z_x[2 + 2 * nZ] = 4.0;
  
  valarray<double> zCovOut   ( nZ * nZ );
  valarray<double> zInvCovOut( nZ * nZ );
  valarray<double> zSEOut    ( nZ );
  valarray<double> zCorOut   ( nZ * nZ );
  valarray<double> zCVOut    ( nZ );
  valarray<double> zCIOut    ( 2 * nZ );


  //------------------------------------------------------------
  // Calculate the statistics for the derived parameter.
  //------------------------------------------------------------

  // Calculate the statistics.
  derParStatistics( xMask,
		    xCov,
		    zMask,
		    z,
		    z_x,
		    nDegFreedom,
		    &zCovOut,
                    &zInvCovOut,
		    &zSEOut,
		    &zCorOut,
		    &zCVOut,
		    &zCIOut );

  //------------------------------------------------------------
  // Calculate the known values.
  //------------------------------------------------------------

  valarray<double> zCovKnown   ( NAN, nZ * nZ );
  valarray<double> zInvCovKnown( NAN, nZ * nZ );
  valarray<double> zSEKnown    ( NAN, nZ );
  valarray<double> zCorKnown   ( NAN, nZ * nZ );
  valarray<double> zCVKnown    ( NAN, nZ );
  valarray<double> zCIKnown    ( NAN, 2 * nZ );

  // Set the known covariance of z(x),
  //
  //                     -          -
  //                    |  29   0   63  |
  //     cov[ z(x) ] =  |   0   0    0  |   .
  //                    |  63   0  137  |
  //                     -          -
  //
  zCovKnown[0 + 0 * nZ] = 29.0;
  zCovKnown[2 + 0 * nZ] = 63.0;
  zCovKnown[0 + 2 * nZ] = 63.0;
  zCovKnown[2 + 2 * nZ] = 137.0;

  // Inverse of zCov, ignoring zeros.
  //
  //                      | 1/29  0   1/63  |
  //     cov[ z(x) ]^-1 = |  0    0    0    |
  //                      | 1/63  0   1/137 |
  //
  zInvCovKnown[0 + 0 * nZ] = 1.0/29.0;
  zInvCovKnown[2 + 0 * nZ] = 1.0/63.0;
  zInvCovKnown[0 + 2 * nZ] = 1.0/63.0;
  zInvCovKnown[2 + 2 * nZ] = 1.0/137.0;
  

  // Set the known standard errors for z(x),
  //
  //                        -                  -  1/2
  //     SE[ z(x) ]     =  |  cov[ z(x) ]       |      .
  //               (i)     |             (i,i)  |
  //                        -                  -
  //
  zSEKnown[0] = sqrt( zCovKnown[0 + 0 * nZ] );
  zSEKnown[2] = sqrt( zCovKnown[2 + 2 * nZ] );

  // Set the known correlation of z(x),
  //
  //                                           cov[ z(x) ]       
  //                                                      (i,j)  
  //     corr[ z(x) ]       =  -------------------------------------------------  .
  //                 (i,j)       -                                    -  1/2
  //                            |  cov[ z(x) ]       cov[ z(x) ]       |
  //                            |             (i,i)             (j,j)  |
  //                             -                                    -
  //
  zCorKnown[0 + 0 * nZ] = 1.0;
  zCorKnown[2 + 0 * nZ] = zCovKnown[2 + 0 * nZ] / 
    sqrt( zCovKnown[2 + 2 * nZ] * 
	  zCovKnown[0 + 0 * nZ] );
  zCorKnown[0 + 2 * nZ] = zCovKnown[0 + 2 * nZ] / 
    sqrt( zCovKnown[0 + 0 * nZ] * 
	  zCovKnown[2 + 2 * nZ] );
  zCorKnown[2 + 2 * nZ] = 1.0;

  // Set the known coefficients of variation for z(x),
  //
  //                             SE[ z(x) ]
  //                                       (i)
  //     CV[ z(x) ]     =  100  ---------------  .
  //               (i)             z(x)
  //                                  (i)
  //
  zCVKnown[0] = zSEKnown[0] / z[0] * 100.0;
  zCVKnown[2] = zSEKnown[2] / z[2] * 100.0;

  // Set the known confidence intervals for z(x),
  //                        
  //       (begin)
  //     CI       [ z(x) ]     =  z(x)     -  t( nDegFreedom )  SE[ z(x) ]    ,
  //                      (i)         (i)                                 (i)
  //
  //       (end)
  //     CI       [ z(x) ]     =  z(x)     +  t( nDegFreedom )  SE[ z(x) ]     .
  //                      (i)         (i)                                 (i)
  //
  zCIKnown[0 + 0 * nX] = z[0] - t * zSEKnown[0];
  zCIKnown[2 + 0 * nX] = z[2] - t * zSEKnown[2];
  zCIKnown[0 + 1 * nX] = z[0] + t * zSEKnown[0];
  zCIKnown[2 + 1 * nX] = z[2] + t * zSEKnown[2];


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  int k;

  double tol = 1.0e-13;

  //
  // Note: To verify a NaN is a NaN, test a == a?
  // If it is a NaN, it will fail because NaN is not equal to anything.
  //

  // Check the covariance of the derived parameter.
  for ( k = 0; k < nZ * nZ; k++ )
    {
      if( !( (zCovOut[k] != zCovOut[k]) & (zCovKnown[k] != zCovKnown[k]) )           // NaN == NaN ?
	  &
	  !( (zCovKnown[k]-tol <= zCovOut[k]) & (zCovOut[k] <= zCovKnown[k]+tol) ) ) // min <= x <= max ?
	{
	  fprintf( stderr, "expected %f but it was %f\n", zCovKnown[k], zCovOut[k] );
	  CPPUNIT_ASSERT(false);
	}
	
    }

  // Check the standard errors of the derived parameter.
  for ( k = 0; k < nZ; k++ )
    {
      if( !( (zSEOut[k] != zSEOut[k]) & (zSEKnown[k] != zSEKnown[k]) )               // NaN == NaN ?
	  & 
	  !( (zSEKnown[k]-tol <= zSEOut[k]) && (zSEOut[k] <= zSEKnown[k]+tol) ) )    // min <= x <= max ?
	{
	  fprintf( stderr, "expected %f but it was %f\n", zSEKnown[k], zSEOut[k] );
	  CPPUNIT_ASSERT(false);
	}
    }

  // Check the correlation of the derived parameter.
  for ( k = 0; k < nZ * nZ; k++ )
    {
      if( !( (zCorOut[k] != zCorOut[k]) & (zCorKnown[k] != zCorKnown[k]) )           // NaN == NaN ?
	  & 
	  !( (zCorKnown[k]-tol <= zCorOut[k]) && (zCorOut[k] <= zCorKnown[k]+tol) ) )// min <= x <= max ?
	{
	  fprintf( stderr, "expected %f but it was %f\n", zCorKnown[k], zCorOut[k] );
	  CPPUNIT_ASSERT(false);
	}
    }

  // Check the coefficients of variation of the derived parameter.
  for ( k = 0; k < nZ; k++ )
    {
      if( !( (zCVOut[k] != zCVOut[k]) & (zCVKnown[k] != zCVKnown[k]) )               // NaN == NaN ?
	  & 
	  !( (zCVKnown[k]-tol <= zCVOut[k]) && (zCVOut[k] <= zCVKnown[k]+tol) ) )    // min <= x <= max ?
	{
	  fprintf( stderr, "expected %f but it was %f\n", zCVKnown[k], zCVOut[k] );
	  CPPUNIT_ASSERT(false);
	}
    }

  // Check the confidence intervals of the derived parameter.
  for ( k = 0; k < 2 * nZ; k++ )
    {
      if( !( (zCIOut[k] != zCIOut[k]) & (zCIKnown[k] != zCIKnown[k]) )               // NaN == NaN ?
	  & 
	  !( (zCIKnown[k]-tol <= zCIOut[k]) && (zCIOut[k] <= zCIKnown[k]+tol) ) )    // min <= x <= max ?
	{
	  fprintf( stderr, "expected %f but it was %f\n", zCIKnown[k], zCIOut[k] );
	  CPPUNIT_ASSERT(false);
	}
    }

}
