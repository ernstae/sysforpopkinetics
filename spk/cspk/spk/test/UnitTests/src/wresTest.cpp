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
 * Modified later by: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

#include <iostream>
#include <cassert>
#include <valarray>
#include "../../../spk/isDblEpsEqual.h"
#include "../../../spk/wres.h"
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "wresTest.h"

using namespace CppUnit;
using namespace std;


/*************************************************************************
 *
 * CppUnit framework functions.
 *
 *************************************************************************/

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


/*************************************************************************
 *
 * Function: emptyCase
 *
 *************************************************************************/

void wresTest::emptyCase()
{
  int n = 0;
  valarray<double> y(n);
  valarray<double> f(n);
  valarray<double> cov(n*n);

  valarray<double> r(n);
  valarray<double> wr(n);
  wres( y, f, cov, &r, &wr );
  CPPUNIT_ASSERT( true );
}


/*************************************************************************
 *
 * Function: oneCase
 *
 *************************************************************************/

void wresTest::oneCase()
{
  int n = 1;

  //
  // y  =  [ 1.0 ]
  //
  valarray<double> y( 1.0, n );

  //
  // f  =  [ 1.5 ]
  //
  valarray<double> f( 1.5, n );

  //
  //         /       \     /       \   /       \
  // cov  =  |  4.0  |  =  |  2.0  | * |  2.0  |
  //         \       /     \       /   \       /
  //
  valarray<double> cov( 4.0, n * n );

  valarray<double> r(n);
  valarray<double> wr(n);
  wres( y, f, cov, &r, &wr );
  
  //
  // r  =  y - f
  //
  //    =  [ -0.5 ]
  //
  CPPUNIT_ASSERT( isDblEpsEqual( -0.5, r[0], fabs( r[0] ) ) );

  //
  //              -1/2
  // wr   =   cov      *  r
  //
  //      =  [ -0.25 ]
  //
  CPPUNIT_ASSERT( isDblEpsEqual( -0.25, wr[0], fabs( wr[0] ) ) );
}


/*************************************************************************
 *
 * Function: threeCase
 *
 *************************************************************************/

void wresTest::threeCase()
{
  int n = 3;

  //
  // y  =  [ 1.1, 2.2, 3.3 ]
  //
  double yIn[] = { 1.1, 2.2, 3.3 };
  valarray<double> y( yIn, n );

  //
  // f  =  [ 1.0, 2.0, 3.0 ]
  //
  double fIn[] = { 1.0, 2.0, 3.0 };
  valarray<double> f( fIn, n );

  //
  //
  //         /                   \
  //         |  1.0   0.2   0.3  |
  // cov  =  |  0.2   4.0   0.5  |  .
  //         |  0.3   0.5   6.0  |
  //         \                   /
  //
  valarray<double> cov( n * n );
  cov[0 + 0 * n] = 1.0;
  cov[1 + 0 * n] = 0.2;
  cov[2 + 0 * n] = 0.3;
  cov[0 + 1 * n] = 0.2;
  cov[1 + 1 * n] = 4.0;
  cov[2 + 1 * n] = 0.5;
  cov[0 + 2 * n] = 0.3;
  cov[1 + 2 * n] = 0.5;
  cov[2 + 2 * n] = 6.0;

  valarray<double> r(n);
  valarray<double> wr(n);
  wres( y, f, cov, &r, &wr );

  //
  // r  =  y - f
  //
  //    =  [ 0.1, 0.2, 0.3 ]
  //
  CPPUNIT_ASSERT( isDblEpsEqual( 0.1, r[0], fabs( r[0] ) ) );
  CPPUNIT_ASSERT( isDblEpsEqual( 0.2, r[1], fabs( r[1] ) ) );
  CPPUNIT_ASSERT( isDblEpsEqual( 0.3, r[2], fabs( r[2] ) ) );

  //
  //            -1/2
  // wr  =   cov      *  r  ,
  //
  //     = [ 0.0848385640149240, 0.0910497809638078,  0.1155754330865829 ]
  // 
  // These values were calculated using the following Octave code.
  //
  //     format long
  //     
  //     y  =  [ 1.1;
  //             2.2;
  //             3.3 ]
  //     
  //     f  =  [ 1.0;
  //             2.0;
  //             3.0 ]
  //     
  //     cov = [ 1.0,   0.2,   0.3;
  //             0.2,   4.0,   0.5;
  //             0.3,   0.5,   6.0  ]
  //     
  //     wres = sqrtm( inverse( cov ) ) * ( y - f )
  //
  CPPUNIT_ASSERT( isDblEpsEqual( 0.0848385640149240, wr[0], fabs( wr[0] ) ) );
  CPPUNIT_ASSERT( isDblEpsEqual( 0.0910497809638078, wr[1], fabs( wr[1] ) ) );
  CPPUNIT_ASSERT( isDblEpsEqual( 0.1155754330865829, wr[2], fabs( wr[2] ) ) );
}
