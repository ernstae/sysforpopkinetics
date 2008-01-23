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
 * File: calcMeanTest.cpp
 *
 *
 * Test cases for calcMean()
 *
 * Author: Viet Nyuyen
 * Updated by: sachiko honda
 *
 *************************************************************************/
#include <iostream>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/calcMean.h"
#include "../../../spk/SpkValarray.h"
#include "calcMeanTest.h"

using SPK_VA::valarray;
using namespace CppUnit;

void calcMeanTest::setUp()
{
}
void calcMeanTest::tearDown()
{
    // clean up
}

Test* calcMeanTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("calcMeanTest");

  // duplicate the following example for each test case, replacing the case name.
  suiteOfTests->addTest(new TestCaller<calcMeanTest>("calcMean",  &calcMeanTest::test));
  
  return suiteOfTests;
}
void calcMeanTest::test()
{
  // -----------------------------------------------------------------------------
  // Testing simulate() results in also testing calcMean - this is further testing
  // -----------------------------------------------------------------------------
  
  int i;
  int m = 1, n = 1;
  valarray<double> mean( m * n );
  double temp;
  
  // Create a 1x1 matrix
  valarray<double> one(m * n);
  for (i = 0; i < m*n; i++)
    {
      one[i] = i;
    }
  
  mean = calcMean(one, m);
  
  CPPUNIT_ASSERT_EQUAL( m, static_cast<int>( mean.size() ) ); // dimensions of mean
  CPPUNIT_ASSERT_EQUAL( 0.0, mean[0] );	  // value of mean = 0
  
  
  // Create a 5x5 matrix
  m = 5;
  n = 5;
  mean.resize( m );
  valarray<double> five( m * n );
  for (i = 0; i < m*n; i++)
    {
      five[i] = i;
    }
  
  mean = calcMean(five, m);
  
  CPPUNIT_ASSERT_EQUAL( m, static_cast<int>( mean.size() ) );  // dimensions of mean
  
  for (i = 0; i < n; i++)
    {
      CPPUNIT_ASSERT_EQUAL( 2.0 * n + i, mean[i]);			
    }
  
  // Create a 5x2 matrix
  m = 5;
  n = 2;
  valarray<double> fiveTwo(m * n);
  for (i = 0; i < m*n; i++)
    {
      fiveTwo[i] = i;
    }
  
  mean = calcMean(fiveTwo, m);
  
  CPPUNIT_ASSERT_EQUAL( m, static_cast<int>( mean.size() ) );// dimensions of mean
  
  for (i = 0; i < m; i++)
    {
      temp = (5.0 + 2*i)/2.0;
      CPPUNIT_ASSERT_DOUBLES_EQUAL( temp, mean[i], 0.00001 );
    }
  
  return;
}
