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
 * File: randNormalTest.cpp
 *
 *
 * Test cases for randNormal()
 *
 * Author: Viet Nyuyen
 * Updated by: sachiko honda
 *
 *************************************************************************/
#include <iostream>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/randNormal.h"
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/SpkValarray.h"
#include "randNormalTest.h"

using SPK_VA::valarray;
using namespace CppUnit;

void randNormalTest::setUp()
{

}
void randNormalTest::tearDown()
{
    // clean up
}

Test* randNormalTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("randNormalTest");

  suiteOfTests->addTest(new TestCaller<randNormalTest>(
    "randNormal",  &randNormalTest::test));
  
  return suiteOfTests;
}

//=================================================================
// randNormal() shall return a sequance of random numbers
// drawn from normal distribution with mean 0.0 and variance ~1.0
// weighted by the covariance.
//=================================================================
void randNormalTest::test()
{
  using namespace std;

  const double mean = 0.0;
  const int n = 1000;

  // randNormal() should return the sequence of random numbers
  // straight out without weighting (or weighted by unity)
  // as long as we feed the identity matrix as variance.
  valarray<double> v(0.0, n*n);
  for( int j=0; j<n; j++ )
    {
      for( int i=0; i<n; i++ )
	{
	  if( i==j )
	    v[i+j*n] = 1.0;
	}
    }

  srand( 5 );
  valarray<double> r = randNormal( v, n );

  const double range = r.max() - r.min();
  CPPUNIT_ASSERT_MESSAGE( "The range between the largest and the smallest in the sequence is zero!", range > 0.0 );

  // normalize r for testing
  r /= range;
  const double max = +1.0;
  const double min = -1.0;

  const int boxes = 10;
  const double d  = 0.2;

  // A distribution which describes many situations where 
  // observations are distributed symmetrically around the mean . 
  // 68% of all values under the curve lie within one standard deviation
  // of the mean and 95% lie within two standard deviations.
  //
  // Let X0 be the mean of data,
  // X0~S  : contain roughly 68% of the data
  // X0~2S : contain 95%
  // X0~3S : contain almost all
  //
  // S = ( 1/(n-1) * sum(Xi-X0)^2 )^(1/2) over 1<=i<=n.
  //
  double sum = 0.0;
  for( int i=0; i<n; i++ )
    sum += ((r[i]-mean)*(r[i]-mean));
  const double S = sqrt( 1.0/(n-1) * sum );

  /*
  cout << "mean      = " << mean << endl;
  cout << "min       = " << min << endl; 
  cout << "max       = " << max << endl;
  cout << "range     = " << range << endl;
  cout << "boxes     = " << boxes << endl;
  cout << "increment = " << d << endl;
  cout << "S         = " << S << endl;
  */

  double lower;
  double upper;
  const double oneSLower = mean - S;
  const double oneSUpper = mean + S;
  int oneS = 0;
  const double twoSLower = mean - 2.0 * S;
  const double twoSUpper = mean + 2.0 * S;
  int twoS = 0;

  valarray<int> hist( 0, boxes+1 );
  for (int i=0; i<n; i++ )
  {
     if( oneSLower <= r[i] && r[i] <= oneSUpper )
       oneS++;
     if( twoSLower <= r[i] && r[i] <= twoSUpper )
       twoS++;

     lower = min - (d*0.5);
     upper = min + (d*0.5);
     
     for( int j=0; j<=boxes; j++ )
     {
        if( lower <= r[i] && r[i] < upper )
        {
	  //	   cout << lower << " <= " << r[i] << " < " << upper << endl;
           hist[ j ]++;
	   break;
        }
	lower = upper;
	upper += d;
     }
  }

  double one = (double)oneS / (double)n * 100.0;
  double two = (double)twoS / (double)n * 100.0;
  double tol = 0.5;

  /*
  lower = min - (d*0.5);
  upper = min + (d*0.5);
  for( int i=0; i<=boxes; i++, lower=upper, upper+=d )
    {
      printf( "[%03d](%+.4f - %+.4f) %7d\n", i, lower, upper, hist[i] );
    }                                                           
  
  printf( "One standard deviation from mean: %f <= %f <= %f (%d).\n", 
	  oneSLower, mean, oneSUpper, oneS );
  printf( "%f percent lies within one standard deviation.\n", one );
  printf( "Two standard deviation from mean: %f <= %f <= %f (%d).\n", 
	  twoSLower, mean, twoSUpper, twoS );
  printf( "%f percent lies within two standard deviation.\n", two );
  */

  CPPUNIT_ASSERT_MESSAGE( "68% of the drawn values should lie within one standard deviation!",
			  ( 68.0 - tol <= one ) && ( one <= 68.0 + tol ) );
  CPPUNIT_ASSERT_MESSAGE( "95% of the drawn values should lie within two standard deviations!",
			  ( 95.0 - tol <= two ) && ( two <= 95.0 + tol ) );
  
  return;
}
