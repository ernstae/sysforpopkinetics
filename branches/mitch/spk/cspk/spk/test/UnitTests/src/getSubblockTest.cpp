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
 * File: getSubblockTest.cpp
 *
 *
 * Unit test for getSubblock.
 *
 * Author: Sachiko Honda
 *
 * Note: This routine has been also tested for illegal indices.
 *
 *************************************************************************/
#include <iostream>
#include "../../../spk/getSubblock.h"
#include "../../../spk/DoubleMatrix.h"
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "getSubblockTest.h"

using namespace CppUnit;

void getSubblockTest::setUp()
{
    // initializations
}
void getSubblockTest::tearDown()
{
    // clean up
}

Test* getSubblockTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "getSubblockTest" );

  // Test cases for this unit test.
  suiteOfTests->addTest( new TestCaller<getSubblockTest>( "specCase", &getSubblockTest::specCase ) );

  return suiteOfTests;
}

static void exampleInSpec();
static bool check(DoubleMatrix dmatA,
                  int rows,
                  int cols,
                  int d1=-1, int d2=-1, int d3=-1, 
                  int d4=-1, int d5=-1, int d6=-1,
                  int d7=-1, int d8=-1, int d9=-1);


void getSubblockTest::specCase()
{
    using namespace std;
    DoubleMatrix dmatA(3,3);
    DoubleMatrix dmatSub;

    double *pdA = dmatA.data();
    int inx = -1;

    
    //exampleInSpec();
    // set the matrix to:
    // 
    // [0, 3, 6]
    // [1, 4, 7]
    // [2, 5, 8]
    //
    for( int i=0; i<3*3; i++ )
        pdA[i] = i;

    CPPUNIT_ASSERT(check( getSubblock(dmatA, 1, 1, 2, 2), 2, 2, 4, 5, 7, 8 ) );

    CPPUNIT_ASSERT(check( getSubblock(dmatA, 2, 2, 1, 1), 1, 1, 8 ) );

    CPPUNIT_ASSERT(check( getSubblock(dmatA, 1, 2, 1, 1), 1, 1, 7));
}
static void exampleInSpec(){
        using namespace std;

        DoubleMatrix dmatA(3,3);
        DoubleMatrix dmatSub;
        double *pdA = dmatA.data();

        // set the matrix to:
        // 
        // [0, 3, 6]
        // [1, 4, 7]
        // [2, 5, 8]
        //
        for( int i=0; i<3*3; i++ )
            pdA[i] = i;

        dmatSub = getSubblock(dmatA, 0, 0, 2, 2);
}

static bool check(DoubleMatrix dmatB,
                  int rows,
                  int cols,
                  int d1, int d2, int d3, 
                  int d4, int d5, int d6,
                  int d7, int d8, int d9)
{
    using namespace std;
    bool isOkay = true;
    int m = dmatB.nr();
    int n = dmatB.nc();
    double *pdB = dmatB.data();

    if( m != rows || n != n ){
        isOkay = false;
        return isOkay;
    }

    int d[9];
    d[0] = d1;
    d[1] = d2;
    d[2] = d3;
    d[3] = d4;
    d[4] = d5;
    d[5] = d6;
    d[6] = d7;
    d[7] = d8;
    d[8] = d9;
    
    for( int i=0; i<rows*cols && isOkay; i++ ){
        if( d[i] != pdB[i] ){
            isOkay = false;
        }
    }
    return isOkay;
}
