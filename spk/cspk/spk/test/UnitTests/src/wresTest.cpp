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
 * File: wresTest.cpp
 *
 *
 * Unit test for wres.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#include <iostream>
#include <cassert>
#include <valarray>
#include <spk/wres.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "wresTest.h"

using namespace CppUnit;
using namespace std;

void wresTest::setUp()
{
    // initializations
}
void wresTest::tearDown()
{
    // clean up
}

Test* wresTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "wresTest" );

  // Test cases for this unit test.
  suiteOfTests->addTest( new TestCaller<wresTest>( "emptyCase",  &wresTest::emptyCase ) );
  suiteOfTests->addTest( new TestCaller<wresTest>( "oneCase",    &wresTest::oneCase ) );
  suiteOfTests->addTest( new TestCaller<wresTest>( "threeCase",  &wresTest::threeCase ) );

  return suiteOfTests;
}


void wresTest::emptyCase()
{
  int n = 0;
  valarray<double> y(n);
  valarray<double> yHat(n);
  valarray<double> R(n*n);

  valarray<double> r(n);
  valarray<double> wr(n);
  wres( y, yHat, R, r, wr );
  CPPUNIT_ASSERT( true );
}
void wresTest::oneCase()
{
  int n = 1;

  //
  // y = [ 1.0 ]
  //
  valarray<double> y( 1.0, n );

  //
  // y^ = [ 1.5 ]
  //
  valarray<double> yHat( 1.5, n );

  //
  //     /       \   /       \   /       \
  // R = |  4.0  | = |  2.0  | * |  2.0  |
  //     \       /   \       /   \       /
  //
  valarray<double> R( 4.0, n * n );

  valarray<double> r(n);
  valarray<double> wr(n);
  wres( y, yHat, R, r, wr );
  
  //
  // r = [ -0.5 ]
  //
  CPPUNIT_ASSERT_DOUBLES_EQUAL( -0.5, r[0], 0.0 );

  //
  // wres = [ -1.0 ]
  //
  CPPUNIT_ASSERT_DOUBLES_EQUAL( -1.0, wr[0], 0.0 );
}
void wresTest::threeCase()
{
  int n = 3;

  //
  // y = [ 1.1, 2.2, 3.3 ]
  //
  double yIn[] = { 1.1, 2.2, 3.3 };
  valarray<double> y( yIn, n );

  //
  // y^ = [ 1.0, 2.0, 3.0 ]
  //
  double yHatIn[] = { 1.0, 2.0, 3.0 };
  valarray<double> yHat( yHatIn, n );

  //
  //
  //     /                   \    /                \   /                 \
  //     |  1.0   0.0   0.0  |   |  1.0  0.0  0.0  |   |  1.0  0.0  0.0  |
  // R = |  0.0   4.0   0.0  | = |  0.0  2.0  0.0  | * |  0.0  2.0  0.0  |
  //     |  0.0   0.0   9.0  |   |  0.0  0.0  3.0  |   |  0.0  0.0  3.0  |
  //     \                   /    \                /   \                 /
  //
  //
  valarray<double> R( 0.0, n * n );
  //R[ slice( 0, n, n+1 ) ] = 1.0;
  R[0] = 1.0;
  R[4] = 4.0;
  R[8] = 9.0;

  valarray<double> r(n);
  valarray<double> wr(n);
  wres( y, yHat, R, r, wr );

  //
  // r = [ 0.1, 0.2, 0.3 ]
  //
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.1, r[0], fabs(0.1-r[0])/0.1 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.2, r[1], fabs(0.1-r[1])/0.1 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.3, r[2], fabs(0.1-r[2])/0.1 );

  //
  // wr = [ 0.1, 0.2, 0.3 ]
  // 
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.1, wr[0], fabs(0.1-wr[0])/0.1 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.4, wr[1], fabs(0.1-wr[1])/0.1 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 0.9, wr[2], fabs(0.1-wr[2])/0.1 );
}
