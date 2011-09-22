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
 * File: replaceSubblockTest.cpp
 *
 *
 * Unit test for replaceSubblock.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: replaceSubblockTest
 *
 *
 * Performs the unit test for replaceSubblock.
 *
 *
 *************************************************************************/
#include <iostream>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/replaceSubblock.h"
#include "replaceSubblockTest.h"
#include "../../../spk/SpkValarray.h"

using namespace CppUnit;
using SPK_VA::valarray;

static void exampleInSpec();
static bool check(DoubleMatrix dmatA,
                  int rows,
                  int cols,
                  int d1=-1, int d2=-1, int d3=-1, 
                  int d4=-1, int d5=-1, int d6=-1,
                  int d7=-1, int d8=-1, int d9=-1);

void replaceSubblockTest::setUp()
{

}
void replaceSubblockTest::tearDown()
{
    // clean up
}

Test* replaceSubblockTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "replaceSubblockTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<replaceSubblockTest>
		                 ("testReplaceSubblockDoubleMatrix", 
                                  &replaceSubblockTest::testReplaceSubblockDoubleMatrix));

    suiteOfTests->addTest(new TestCaller<replaceSubblockTest>
		                 ("testReplaceSubblockValarray", 
                                  &replaceSubblockTest::testReplaceSubblockValarray));

    return suiteOfTests;
}
void replaceSubblockTest::testReplaceSubblockDoubleMatrix()
{
    using namespace std;
    DoubleMatrix dmatA(3,3);
    DoubleMatrix dmatB(2,2);

    double *pdA = dmatA.data();
    double *pdB = dmatB.data();
    int inx = -1;
    int i;
    // set the matrix dmatA to:
    // 
    // [0, 3, 6]
    // [1, 4, 7]
    // [2, 5, 8]
    //
    for(  i=0; i<3*3; i++ )
        pdA[i] = i;

    // set the matrix dmatB to:
    // 
    // [4, 2]
    // [3, 1]
    //
    for( i=0; i<2*2; i++ )
        pdB[i] = 4 - i;

	replaceSubblock( dmatA, dmatB, 0, 0 );
    CPPUNIT_ASSERT_MESSAGE( "Failed to replace A(0,0)", check( dmatA, 3, 3, 4, 3, 2, 2, 1, 5, 6, 7, 8 ) );

	for( i=0; i<3*3; i++ )
        pdA[i] = i;
	replaceSubblock( dmatA, dmatB, 1, 1 );
    CPPUNIT_ASSERT_MESSAGE( "Failed to replace A(1,1)", check( dmatA, 3, 3, 0, 1, 2, 3, 4, 3, 6, 2, 1 ) );

	for( i=0; i<3*3; i++ )
        pdA[i] = i;
	replaceSubblock( dmatA, dmatB, 0, 1 );
    CPPUNIT_ASSERT_MESSAGE( "Failed to replace A(0,1)", check( dmatA, 3, 3, 0, 1, 2, 4, 3, 5, 2, 1, 8 ) );

	for( i=0; i<3*3; i++ )
        pdA[i] = i;
	replaceSubblock( dmatA, dmatB, 1, 0 );
    CPPUNIT_ASSERT_MESSAGE( "Failed to replace A(1,0)", check ( dmatA, 3, 3, 0, 4, 3, 3, 2, 1, 6, 7, 8 ) );
}
void replaceSubblockTest::testReplaceSubblockValarray()
{
    using namespace std;
    const int nARows = 3;
    const int nACols = 3;
    
    const int nBRows = 2;
    const int nBCols = 2;

    valarray<double> a( nARows * nACols );
    valarray<double> b( nBRows * nBCols);

    int inx = -1;
    int i;

    // matrix A is:
    // 
    // [0, 3, 6]
    // [1, 4, 7]
    // [2, 5, 8]
    //
	//
	// corresponding valarray is:
	//
	// { 0, 1, 2, 3, 4, 5, 6, 7, 8 }
	//
    for( i=0; i<nARows*nACols; i++ )
        a[i] = i;

    // set the matrix B to:
    // 
    // [4, 2]
    // [3, 1]
    //
	//
	// corresponding valarray is:
	//
	// { 4, 3, 2, 1 }
	//
    for( i=0; i<nBRows*nBCols; i++ )
        b[i] = 4 - i;

	//
	// want to replace a subblock of A starting from the left upper corner.
	//
	int strCol = 0;
	int strRow = 0;

	// 
	// replace { aij | strCol * nACols + strRow }
	//
	// that is, these elements of a with double quotations in { "0", "1", 2, "3", "4", 5, 6, 7, 8 }.
	//
	valarray<double> aa = a;
	replaceSubblock( aa, nACols, b, nBCols, strRow, strCol );
	CPPUNIT_ASSERT_EQUAL( b[0], aa[0] );
	CPPUNIT_ASSERT_EQUAL( b[1], aa[1] );
	CPPUNIT_ASSERT_EQUAL( a[2], aa[2] );
	CPPUNIT_ASSERT_EQUAL( b[2], aa[3] );
	CPPUNIT_ASSERT_EQUAL( b[3], aa[4] );
	CPPUNIT_ASSERT_EQUAL( a[5], aa[5] );
	CPPUNIT_ASSERT_EQUAL( a[6], aa[6] );
	CPPUNIT_ASSERT_EQUAL( a[7], aa[7] );
	CPPUNIT_ASSERT_EQUAL( a[8], aa[8] );

	aa = a;
	strCol = 1;
	strRow = 1;

	replaceSubblock( aa, nACols, b, nBCols, strRow, strCol );
	CPPUNIT_ASSERT_EQUAL( a[0], aa[0] );
	CPPUNIT_ASSERT_EQUAL( a[1], aa[1] );
	CPPUNIT_ASSERT_EQUAL( a[2], aa[2] );
	CPPUNIT_ASSERT_EQUAL( a[3], aa[3] );
	CPPUNIT_ASSERT_EQUAL( b[0], aa[4] );
	CPPUNIT_ASSERT_EQUAL( b[1], aa[5] );
	CPPUNIT_ASSERT_EQUAL( a[6], aa[6] );
	CPPUNIT_ASSERT_EQUAL( b[2], aa[7] );
	CPPUNIT_ASSERT_EQUAL( b[3], aa[8] );

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
            cout << "d[" << i << "] received was " << d[i] << endl;
            cout << "mat[" << i << "] was " << pdB[i] << endl;
            dmatB.print();
            isOkay = false;
        }
    }
    return isOkay;
}
