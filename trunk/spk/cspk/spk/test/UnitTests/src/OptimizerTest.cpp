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
  
  int    nSet     = 2;
  size_t bSet     = 9;
  double rSet     = 5.2;
  double fSet     = 7.5;
  double xSet[]   = { 1.0, 2.0 };
  double gSet[]   = { 1.0, 2.0 };
  double hSet[]   = { 1.0, 2.0, 3.0, 4.0 };
  int    mSet     = 3;
  double lowSet[] = { -10.0, -20.0, -30.0 };
  double upSet[]  = { +10.0, +20.0, +30.0 };
  int    posSet[] = { 1, 3 };

  // constructor
  Optimizer opt1( .001, 10, 1 );
  // setStateInfo
  opt1.setStateInfo( nSet, bSet, rSet, fSet, xSet, gSet, hSet,
                     mSet, lowSet, upSet, posSet );

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
  // getIsWarmStartPossible
  CPPUNIT_ASSERT( !opt1.getIsWarmStartPossible() );
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
  // getIsWarmStartPossible
  CPPUNIT_ASSERT_EQUAL( opt1.getIsWarmStartPossible(), false );
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
  // getIsWarmStartPossible
  CPPUNIT_ASSERT( !opt3.getIsWarmStartPossible() );
  // getIsWarmStart
  CPPUNIT_ASSERT( !opt3.getIsWarmStart() );
  
  // setIsWarmStartPossible
  opt1.setIsWarmStartPossible( true );
  // getIsWarmStartPossible
  CPPUNIT_ASSERT( opt1.getIsWarmStartPossible() );
  
  // setIsWarmStart
  opt1.setIsWarmStart( true );
  // getIsWarmStart
  CPPUNIT_ASSERT( opt1.getIsWarmStart() );

  int     nGet = nSet;
  size_t  bGet;
  double  rGet;
  double  fGet;
  double* xGet = new double[ nGet ];
  double* gGet = new double[ nGet ];
  double* hGet = new double[ nGet * nGet ];

  // getStateInfo
  opt1.getStateInfo( nGet, bGet, rGet, fGet, xGet, gGet, hGet );

  CPPUNIT_ASSERT_MESSAGE( "The memory for warm start is not allocated", xGet != 0 );
  CPPUNIT_ASSERT_MESSAGE( "The memory for warm start is not allocated", gGet != 0 );	
  CPPUNIT_ASSERT_MESSAGE( "The memory for warm start is not allocated", hGet != 0 );	
  
  CPPUNIT_ASSERT_EQUAL( nGet, 2 );
  CPPUNIT_ASSERT_EQUAL( static_cast<int>( bGet ), 9 );
  CPPUNIT_ASSERT_EQUAL( rGet, 5.2 );
  CPPUNIT_ASSERT_EQUAL( fGet, 7.5 );
  CPPUNIT_ASSERT_EQUAL( xGet[ 0 ], 1.0 );
  CPPUNIT_ASSERT_EQUAL( xGet[ 1 ], 2.0 );
  CPPUNIT_ASSERT_EQUAL( gGet[ 0 ], 1.0 );
  CPPUNIT_ASSERT_EQUAL( gGet[ 1 ], 2.0 );
  CPPUNIT_ASSERT_EQUAL( hGet[ 0 ], 1.0 );
  CPPUNIT_ASSERT_EQUAL( hGet[ 1 ], 2.0 );
  CPPUNIT_ASSERT_EQUAL( hGet[ 2 ], 3.0 );
  CPPUNIT_ASSERT_EQUAL( hGet[ 3 ], 4.0 );
}
