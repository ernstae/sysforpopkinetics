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
 * File: transposeDerivative.cpp
 *
 *
 * Test cases for transposeDerivative
 *
 * Author: Jiaji Du
 * Extended: Sachiko Honda, 10/01/02
 *
 *************************************************************************/

#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/transposeDerivative.h"
#include "../../../spk/SpkValarray.h"
#include "transposeDerivativeTest.h"

using namespace CppUnit;
using SPK_VA::valarray;

void transposeDerivativeTest::setUp()
{
    // initializations
}
void transposeDerivativeTest::tearDown()
{
    // clean up
}

Test* transposeDerivativeTest::suite()
{
	TestSuite *suiteOfTests = new TestSuite( "transposeDerivativeTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<transposeDerivativeTest>(
                          "testDerivative", 
                          &transposeDerivativeTest::testDerivative));
    suiteOfTests->addTest(new TestCaller<transposeDerivativeTest>(
                          "testValarrayVersion", 
                          &transposeDerivativeTest::testValarrayVersion));

    return suiteOfTests;
}

void transposeDerivativeTest::testDerivative()
{
    using namespace std;

    const double x[] = {1.0, 2.0};

    //
    // Set A to:
    //   [ 0     x[1] ]
    //   [ x[0]  1    ]
    //
    DoubleMatrix A(2,2);
    A.data()[0] = 0;
    A.data()[1] = x[0];
    A.data()[2] = x[1];
    A.data()[3] = 1.0;

    //
    // Therefore A_x is:
    //
    //    [ 0  0 ]
    //    [ 0  1 ]
    //    [ 1  0 ]
    //    [ 0  0 ]
    //
    DoubleMatrix A_x(4,2);
    A_x.fill(0);
    A_x.data()[2] = 1.0;
    A_x.data()[5] = 1.0;

    //
    // C = (A')_x should be:
    //
    //   [ 0  0 ]
    //   [ 1  0 ]
    //   [ 0  1 ]
    //   [ 0  0 ]
    //
    DoubleMatrix expectedC(4,2);
    expectedC.data()[0] = 0;
    expectedC.data()[1] = 1;
    expectedC.data()[2] = 0;
    expectedC.data()[3] = 0;
    expectedC.data()[5] = 0;
    expectedC.data()[4] = 0;
    expectedC.data()[6] = 1;
    expectedC.data()[7] = 0;

    DoubleMatrix C = transposeDerivative( A, A_x );

    for( int i=0; i<8; i++ )
    {
        CPPUNIT_ASSERT_EQUAL(expectedC.data()[i], C.data()[i]);
    }

}
void transposeDerivativeTest::testValarrayVersion()
{
    using namespace std;

    valarray<double> x(2);
    x[0] = 1.0;
    x[1] = 2.0;

    //
    // Set A to:
    //   [ 0     x[1] ]
    //   [ x[0]  1    ]
    //
    valarray<double> A(2 * 2);
    A[0] = 0;
    A[1] = x[0];
    A[2] = x[1];
    A[3] = 1.0;

    //
    // Therefore A_x is:
    //
    //    [ 0  0 ]
    //    [ 0  1 ]
    //    [ 1  0 ]
    //    [ 0  0 ]
    //
    valarray<double> A_x(4 * 2);
    A_x = 0.0;
    A_x[2] = 1.0;
    A_x[5] = 1.0;

    //
    // C = (A')_x should be:
    //
    //   [ 0  0 ]
    //   [ 1  0 ]
    //   [ 0  1 ]
    //   [ 0  0 ]
    //
    valarray<double> expectedC(4 * 2);
    expectedC[0] = 0;
    expectedC[1] = 1;
    expectedC[2] = 0;
    expectedC[3] = 0;
    expectedC[5] = 0;
    expectedC[4] = 0;
    expectedC[6] = 1;
    expectedC[7] = 0;

    valarray<double> C = transposeDerivative( A_x, /* nRowsA*/ 2, /* nColsA */ 2, /* nX */ 2 );

    for( int i=0; i<8; i++ )
    {
        CPPUNIT_ASSERT_EQUAL(expectedC[i], C[i]);
    }

}
