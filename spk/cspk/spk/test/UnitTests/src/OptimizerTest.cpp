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
 * File: OptimizerTest.cpp
 *
 *
 * Unit test for Optimizer class.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/
#include <iostream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include <spk/Optimizer.h>
#include "OptimizerTest.h"

using namespace CppUnit;

void OptimizerTest::setUp()
{
    // initializations
}
void OptimizerTest::tearDown()
{
    // clean up
}

Test* OptimizerTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("OptimizerTest");

  // duplicate the following example for each test case, replacing the case name.
  suiteOfTests->addTest(new TestCaller<OptimizerTest>("case1", &OptimizerTest::case1));
  
  return suiteOfTests;
}

void OptimizerTest::case1()
{
  using namespace std;
  
  // constructor
  Optimizer opt1( .001, 10, 1 );
  // getEpsilon
  CPPUNIT_ASSERT_EQUAL( opt1.getEpsilon(), .001 );
  // getNMaxIter
  CPPUNIT_ASSERT_EQUAL( opt1.getNMaxIter(), 10 );
  // getLevel
  CPPUNIT_ASSERT_EQUAL( opt1.getLevel(), 1 );
  // getNIterCompleted
  CPPUNIT_ASSERT_EQUAL( opt1.getNIterCompleted(), 0 );
  // getIsTooManyIter
  CPPUNIT_ASSERT( !opt1.getIsTooManyIter() );
  // getIsWarmStart
  CPPUNIT_ASSERT( !opt1.getIsWarmStart() );
  // setEpsilon
  opt1.setEpsilon( .01 );
  CPPUNIT_ASSERT_EQUAL( opt1.getEpsilon(), .01 );
  // setNMaxIter
  opt1.setNMaxIter( 100 );
  CPPUNIT_ASSERT_EQUAL( opt1.getNMaxIter(), 100 );
  // setLevel
  opt1.setLevel( 2 );
  CPPUNIT_ASSERT_EQUAL( opt1.getLevel(), 2 );
  // setNIterCompleted
  opt1.setNIterCompleted( 20 );
  CPPUNIT_ASSERT_EQUAL( opt1.getNIterCompleted(), 20 );
  // setIsTooManyIter
  opt1.setIsTooManyIter( true );
  CPPUNIT_ASSERT( opt1.getIsTooManyIter() );
  
  // copy constructor
  Optimizer opt2(opt1);
  // getEpsilon
  CPPUNIT_ASSERT_EQUAL( opt2.getEpsilon(), .01 );
  // getNMaxIter
  CPPUNIT_ASSERT_EQUAL( opt2.getNMaxIter(), 100 );
  // getLevel
  CPPUNIT_ASSERT_EQUAL( opt2.getLevel(), 2 );
  // getNIterCompleted
  CPPUNIT_ASSERT_EQUAL( opt2.getNIterCompleted(), 20 );
  // getIsTooManyIter
  CPPUNIT_ASSERT( opt2.getIsTooManyIter() );
  // getIsTooManyIter
  CPPUNIT_ASSERT( opt1.getIsTooManyIter() );
  // getIsWarmStart
  CPPUNIT_ASSERT_EQUAL( opt1.getIsWarmStart(), false );
  
  // assignment operator
  Optimizer opt3( .01, 1, 0 );
  opt3 = opt2;
  // getEpsilon
  CPPUNIT_ASSERT_EQUAL( opt3.getEpsilon(), .01 );
  // getNMaxIter
  CPPUNIT_ASSERT_EQUAL( opt3.getNMaxIter(), 100 );
  // getLevel
  CPPUNIT_ASSERT_EQUAL( opt3.getLevel(), 2 );
  // getNIterCompleted
  CPPUNIT_ASSERT_EQUAL( opt3.getNIterCompleted(), 20 );
  // getIsTooManyIter
  CPPUNIT_ASSERT( opt3.getIsTooManyIter() );
  // getIsWarmStart
  CPPUNIT_ASSERT( !opt3.getIsWarmStart() );
  
  // setupWarmStart
  opt1.setupWarmStart( 2 );
  StateInfo s3;
  s3 = opt1.getStateInfo();
  CPPUNIT_ASSERT_EQUAL( s3.n, 2 );
  CPPUNIT_ASSERT_MESSAGE( "The memory for warm start is not allocated", s3.x != 0 );
  CPPUNIT_ASSERT_MESSAGE( "The memory for warm start is not allocated", s3.g != 0 );	
  CPPUNIT_ASSERT_MESSAGE( "The memory for warm start is not allocated", s3.h != 0 );	
  
  // setIsWarmStart
  opt1.setIsWarmStart( true );
  // getIsWarmStart
  CPPUNIT_ASSERT( opt1.getIsWarmStart() );
  
  // setStateInfo
  int     n   = 2;
  int     b   = 9;
  double  r   = 5.2;
  double  f   = 7.5;
  double  x[] = { 1.0, 2.0 };
  double  g[] = { 1.0, 2.0 };
  double  h[] = { 1.0, 2.0, 3.0, 4.0 };
  StateInfo s1;
  s1.n = n;
  s1.b = b;
  s1.r = r;
  s1.f = f;
  s1.x = x;
  s1.g = g;
  s1.h = h;
  opt1.setStateInfo( s1 );
  
  // getStateInfo
  StateInfo s2;
  s2 = opt1.getStateInfo();
  CPPUNIT_ASSERT_EQUAL( s2.n, 2 );
  CPPUNIT_ASSERT_EQUAL( s2.b, 9 );
  CPPUNIT_ASSERT_EQUAL( s2.r, 5.2 );
  CPPUNIT_ASSERT_EQUAL( s2.f, 7.5 );
  CPPUNIT_ASSERT_EQUAL( s2.x[ 0 ], 1.0 );
  CPPUNIT_ASSERT_EQUAL( s2.x[ 1 ], 2.0 );
  CPPUNIT_ASSERT_EQUAL( s2.g[ 0 ], 1.0 );
  CPPUNIT_ASSERT_EQUAL( s2.g[ 1 ], 2.0 );
  CPPUNIT_ASSERT_EQUAL( s2.h[ 0 ], 1.0 );
  CPPUNIT_ASSERT_EQUAL( s2.h[ 1 ], 2.0 );
  CPPUNIT_ASSERT_EQUAL( s2.h[ 2 ], 3.0 );
  CPPUNIT_ASSERT_EQUAL( s2.h[ 3 ], 4.0 );
  
  // deleteStateInfo
  opt1.deleteStateInfo();
  s2 = opt1.getStateInfo();
  CPPUNIT_ASSERT_EQUAL ( s2.n, 0 );
  CPPUNIT_ASSERT_MESSAGE( "The memory should be returned", s2.x == 0 );	
  CPPUNIT_ASSERT_MESSAGE( "The memory should be returned", s2.g == 0 );	
  CPPUNIT_ASSERT_MESSAGE( "The memory should be returned", s2.h == 0 );	
}
