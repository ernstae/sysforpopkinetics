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
#include <spk/randNormal.h>
#include <spk/DoubleMatrix.h>
#include <spk/SpkValarray.h>
#include "randNormalTest.h"

#include "nag.h"
#include "nagg05.h"			// for random number generation

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
void randNormalTest::test()
{
  // --------------------------------------------------------------------------
  // Testing simulate() results in also testing randNormal 
  // - this is further testing
  // --------------------------------------------------------------------------
  
  using namespace std;
  
  Integer seed = 1;
  
  const int n = 2;

  // Required for NAG routines
  g05cbc(1);					
  
  valarray<double> randNormOut( n );
  valarray<double> V( n * n );// has to be square

  V[0] = 2.0;
  V[1] = 1.0;
  V[2] = 1.0;
  V[3] = 3.0;

  randNormOut = randNormal( V, n );

  CPPUNIT_ASSERT_DOUBLES_EQUAL( -0.367696, randNormOut[0], 0.01);
  // value should be -0.367596
  
  CPPUNIT_ASSERT_DOUBLES_EQUAL( -1.335914, randNormOut[1], 0.01);
  // value should be -1.335914
  
  return;
}
