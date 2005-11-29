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
 * File: inxToMaxTest.cpp
 *
 *
 * Unit test for inxToMax.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#include <iostream>
#include <cmath>
#include <cfloat>
#include "../../../spk/inxToMax.h"
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "inxToMaxTest.h"

using namespace CppUnit;

void inxToMaxTest::setUp()
{
    // initializations
}
void inxToMaxTest::tearDown()
{
    // clean up
}

Test* inxToMaxTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "inxToMaxTest" );

  // Test cases for this unit test.
  suiteOfTests->addTest( new TestCaller<inxToMaxTest>( "increasingCase", &inxToMaxTest::increasingCase ) );
  suiteOfTests->addTest( new TestCaller<inxToMaxTest>( "decreasingCase", &inxToMaxTest::decreasingCase ) );
  suiteOfTests->addTest( new TestCaller<inxToMaxTest>( "negAndPosCase", &inxToMaxTest::negAndPosCase ) );
  suiteOfTests->addTest( new TestCaller<inxToMaxTest>( "someSameIncrCase", &inxToMaxTest::someSameIncrCase ) );
  suiteOfTests->addTest( new TestCaller<inxToMaxTest>( "someSameDecrCase", &inxToMaxTest::someSameDecrCase ) );
  suiteOfTests->addTest( new TestCaller<inxToMaxTest>( "allSmallCase", &inxToMaxTest::allSmallCase ) );

  return suiteOfTests;
}

static void exampleInSpec();
static DoubleMatrix mySet(int d1, int d2, int d3, int d4, int d5, int d6);
static DoubleMatrix mySet(double d1, double d2, double d3, double d4, double d5, double d6);

void inxToMaxTest::increasingCase()
{
    using namespace std;
    DoubleMatrix dmatA(3,2);
    dmatA = mySet(1,2,3,4,5,6);
    CPPUNIT_ASSERT_EQUAL( inxToMax(dmatA), 5 );
}
void inxToMaxTest::decreasingCase()
{
    using namespace std;
    DoubleMatrix dmatA(3,2);
    dmatA = mySet(6,5,4,3,2,1);
    CPPUNIT_ASSERT_EQUAL( inxToMax(dmatA), 0 );
}
void inxToMaxTest::negAndPosCase()
{
    using namespace std;
    DoubleMatrix dmatA(3,2);
    dmatA = mySet(-2,-1,0,1,2,3);
    CPPUNIT_ASSERT_EQUAL( inxToMax(dmatA), 5 );
}
void inxToMaxTest::someSameIncrCase()
{
    using namespace std;
    DoubleMatrix dmatA(3,2);
    dmatA = mySet(1,1,1,2,2,2);
    CPPUNIT_ASSERT_EQUAL( inxToMax(dmatA), 3 );
}
void inxToMaxTest::someSameDecrCase()
{
    using namespace std;
    DoubleMatrix dmatA(3,2);
    dmatA = mySet(2,2,2,1,1,1);
    CPPUNIT_ASSERT_EQUAL( inxToMax(dmatA), 0 );
}
void inxToMaxTest::allSmallCase()
{
    using namespace std;
    DoubleMatrix dmatA(3,2);
    //    dmatA = mySet(1.0e-10, 1.0e-11, 1.0e-12, 1.0e-13, 1.0e-14, 1.0e-15);
    CPPUNIT_ASSERT_EQUAL( inxToMax(dmatA), 0 );
}
static void exampleInSpec(){

    using namespace std;

    DoubleMatrix dmatA(3,3);
    double *pdA = dmatA.data();

    for( int i=0; i<3*3; i++ )
        pdA[i] = i%4;

}
static DoubleMatrix mySet(int d1, int d2, int d3, int d4, int d5, int d6){
    DoubleMatrix dmatA(3,2);
    double *pdA = dmatA.data();

    pdA[0] = d1;
    pdA[1] = d2;
    pdA[2] = d3;
    pdA[3] = d4;
    pdA[4] = d5;
    pdA[5] = d6;
    return dmatA;
}
static DoubleMatrix mySet(double d1, double d2, double d3, double d4, double d5, double d6){
    DoubleMatrix dmatA(3,2);
    double *pdA = dmatA.data();

    pdA[0] = d1;
    pdA[1] = d2;
    pdA[2] = d3;
    pdA[3] = d4;
    pdA[4] = d5;
    pdA[5] = d6;
    return dmatA;
}

