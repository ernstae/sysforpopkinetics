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
/*
 * File: getRowTest.cpp
 *
 * test script for getRow()
 *
 * Note: Illegal indices (out of range) have been also tested.
 *
 * Author: Sachiko Honda
 */
#include <iostream>
#include "../../../spk/getRow.h"
#include "../../../spk/DoubleMatrix.h"
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "getRowTest.h"

using namespace CppUnit;

void getRowTest::setUp()
{
    // initializations
}
void getRowTest::tearDown()
{
    // clean up
}

Test* getRowTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "getRowTest" );

  // Test cases for this unit test.
  suiteOfTests->addTest( new TestCaller<getRowTest>( "specCase", &getRowTest::specCase ) );

  return suiteOfTests;
}

static void exampleInSpec();
static bool check( DoubleMatrix dmatA,
                  double d1=-1,
                  double d2=-1,
                  double d3=-1,
                  double d4=-1,
                  double d5=-1,
                  double d6=-1,
                  double d7=-1,
                  double d8=-1,
                  double d9=-1 );
                   
void getRowTest::specCase()
{
    using namespace std;

    //exampleInSpec();

    int m = 3, n = 3;
    DoubleMatrix dmatA(m,n);
    DoubleMatrix drowSub;
    double *pdA = dmatA.data();

    // [ 0  3  6 ]
    // [ 1  4  7 ]
    // [ 2  5  8 ]
    for( int i=0; i<m*n; i++ )
        pdA[i] = i;


    CPPUNIT_ASSERT( check( getRow(dmatA, 2), 2,5,8 ) );

    CPPUNIT_ASSERT( check( getRow(dmatA, 1), 1,4,7 ) );

    CPPUNIT_ASSERT(check( getRow(dmatA, 0), 0,3,6 ) );

}
static bool check(
    DoubleMatrix drowA,
    double d1, double d2, double d3,
    double d4, double d5, double d6,
    double d7, double d8, double d9)
{
    int m = drowA.nr(), n = drowA.nc();
    double d[9];
    double *pdA = drowA.data();
    bool isOkay = true;
    int i;

    d[0] = d1;
    d[1] = d2;
    d[2] = d3;
    d[3] = d4;
    d[4] = d5;
    d[5] = d6;
    d[6] = d7;
    d[7] = d8;
    d[8] = d9;
    
    for( i=0; i<n && isOkay; i++ ){
        if( d[i] != pdA[i] )
            isOkay = false;
    }
    return isOkay;
}

static void exampleInSpec(){
    using namespace std;

    int m = 2, n = 3;
    DoubleMatrix dmatA(m,n);
    DoubleMatrix dmatSub;
    double *pdA = dmatA.data();
    int ith;

    for( int i=0; i<m*n; i++ )
        pdA[i] = i;


    ith = 0;
    dmatSub = getRow(dmatA, ith);
}
