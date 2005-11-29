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
 * File: blockDiagonalDerivativeTest.cpp
 *
 *
 * Test case for blockDiagonalDerivative
 *
 * Author: Jiaji Du
 *
 *************************************************************************/
#pragma warning ( disable : 4786 )

#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/blockDiagonalDerivative.h"
#include "blockDiagonalDerivativeTest.h"

using namespace CppUnit;

void blockDiagonalDerivativeTest::setUp()
{
    // initializations
}
void blockDiagonalDerivativeTest::tearDown()
{
    // clean up
}

Test* blockDiagonalDerivativeTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("blockDiagonalDerivativeTest");

  // duplicate the following example for each test case, replacing the case name.
  suiteOfTests->addTest(new TestCaller<blockDiagonalDerivativeTest>(
		       "testBlockDiagonalDerivative", &blockDiagonalDerivativeTest::testBlockDiagonalDerivative));

    return suiteOfTests;
}

void blockDiagonalDerivativeTest::testBlockDiagonalDerivative()
{
    using namespace std;

    DoubleMatrix dmatA(4,2);
    DoubleMatrix dmatB(4,2);
	DoubleMatrix dmatC(1,2);

    double *pdA = dmatA.data();
	double *pdB = dmatB.data();
    double *pdC = dmatC.data();

    // set the matrix A to:
    // 
    // [0, 4]
    // [1, 5]
	// [2, 6]
	// [3, 7]
    //
    int i;
    for( i=0; i<4*2; i++ )
        pdA[i] = i;

    // set the matrix B to:
    // 
    // [10, 14]
    // [11, 15]
	// [12  16]
	// [13, 17]
    //
    for( i=0; i<4*2; i++ )
        pdB[i] = 10 + i;

    // set the matrix C to:
    // 
    // [100]
    //
    for( i=0; i<1*2; i++ )
        pdC[i] = 20 + i;

    vector<DoubleMatrix> dmatBlock( 3 );
	dmatBlock[ 0 ]  = dmatA;
    dmatBlock[ 1 ]  = dmatB;
	dmatBlock[ 2 ]  = dmatC;

    DoubleMatrix dmatD = blockDiagonalDerivative( dmatBlock );

	// dmatD should be
/*
		[ 0 4 ]
		[ 1 5 ]
		[ 0 0 ]
		[ 0 0 ]
		[ 0 0 ]
		[ 2 6 ]
		[ 3 7 ]
		[ 0 0 ]
		[ 0 0 ]
		[ 0 0 ]
		[ 0 0 ]
		[ 0 0 ]
		[ 10 14 ]
		[ 11 15 ]
		[ 0 0 ]
		[ 0 0 ]
		[ 0 0 ]
		[ 12 16 ]
		[ 13 17 ]
		[ 0 0 ]
		[ 0 0 ]
		[ 0 0 ]
		[ 0 0 ]
		[ 0 0 ]
		[ 20 21 ]
*/
    double* pdmatD = dmatD.data();
	CPPUNIT_ASSERT( pdmatD[ 0 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 1 ] == 1 );
	CPPUNIT_ASSERT( pdmatD[ 2 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 3 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 4 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 5 ] == 2 );
	CPPUNIT_ASSERT( pdmatD[ 6 ] == 3 );
	CPPUNIT_ASSERT( pdmatD[ 7 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 8 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 9 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 10 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 11 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 12 ] == 10 );
	CPPUNIT_ASSERT( pdmatD[ 13 ] == 11 );
	CPPUNIT_ASSERT( pdmatD[ 14 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 15 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 16 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 17 ] == 12 );
	CPPUNIT_ASSERT( pdmatD[ 18 ] == 13 );
	CPPUNIT_ASSERT( pdmatD[ 19 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 20 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 21 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 22 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 23 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 24 ] == 20 );
	CPPUNIT_ASSERT( pdmatD[ 25 ] == 4 );
	CPPUNIT_ASSERT( pdmatD[ 26 ] == 5 );
	CPPUNIT_ASSERT( pdmatD[ 27 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 28 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 29 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 30 ] == 6 );
	CPPUNIT_ASSERT( pdmatD[ 31 ] == 7 );
	CPPUNIT_ASSERT( pdmatD[ 32 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 33 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 34 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 35 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 36 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 37 ] == 14 );
	CPPUNIT_ASSERT( pdmatD[ 38 ] == 15 );
	CPPUNIT_ASSERT( pdmatD[ 39 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 40 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 41 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 42 ] == 16 );
	CPPUNIT_ASSERT( pdmatD[ 43 ] == 17 );
	CPPUNIT_ASSERT( pdmatD[ 44 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 45 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 46 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 47 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 48 ] == 0 );
	CPPUNIT_ASSERT( pdmatD[ 49 ] == 21 );
}

