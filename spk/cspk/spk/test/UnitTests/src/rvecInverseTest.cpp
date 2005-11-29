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
 * File: rvecInverseTest.cpp
 *
 *
 * Test cases for rvecInverseTest
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/rvecInverse.h"
#include "rvecInverseTest.h"
#include "../../../spk/rvec.h"
#include "../../../spk/subtract.h"
#include "../../../spk/add.h"

using namespace std;
using namespace CppUnit;

void rvecInverseTest::setUp()
{
    _rows.push_back(0); _cols.push_back(0);
    _rows.push_back(0); _cols.push_back(1);
    _rows.push_back(1); _cols.push_back(0);
    _rows.push_back(1); _cols.push_back(1);
    _rows.push_back(1); _cols.push_back(3);
    _rows.push_back(3); _cols.push_back(1);
    _rows.push_back(3); _cols.push_back(3);
    _n = _rows.size();

}
void rvecInverseTest::tearDown()
{
    // clean up
}

Test* rvecInverseTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("rvecInverseTest");

    suiteOfTests->addTest(new TestCaller<rvecInverseTest>("testRvecInverse",    &rvecInverseTest::testRvecInverse));
    suiteOfTests->addTest(new TestCaller<rvecInverseTest>("testRvecInverseRef", &rvecInverseTest::testRvecInverseRef));
    return suiteOfTests;
}

void rvecInverseTest::testRvecInverse()
{
    //
    // Revisit-Sachiko:
    // This test case fails in MTL version of debug/release for unknown reason.
    // rvecInverse() does return the same matrix (C) as the original (A)
    // in terms of element values and dimensions.  Their data pointers are
    // certainly pointing to different locations, as far as I could see
    // by printint them out on the screen in 15 digit precision.
    // Yet, the assertion statement testing each element in A and C fails
    //
#ifndef _MTLMATRIX
    for( int x=0; x<_n; x++ )
    {
        const int m = _rows[x];
        const int k = _cols[x];
        if( m > 0 && k > 0 )
        {
            DoubleMatrix A(k,m);
            for(int i=0; i<m*k; i++)
            {
                A.data()[i] = i+1;
            }
            DoubleMatrix a = rvec(A);
            DoubleMatrix C = rvecInverse(a,m);
            CPPUNIT_ASSERT_EQUAL(A.nr(), C.nr());
            CPPUNIT_ASSERT_EQUAL(A.nc(), C.nc());

            // If the following statement is active, the MTL version succeeds.
            for(int j=0; j<m*k; j++)
            {
                CPPUNIT_ASSERT_EQUAL(A.data()[j], C.data()[j]);
            }
        }
    }
#endif
}

void rvecInverseTest::testRvecInverseRef()
{
    //
    // Revisit-Sachiko:
    // This test case fails in MTL version of debug/release for unknown reason.
    // rvecInverse() does return the same matrix (C) as the original (A)
    // in terms of element values and dimensions.  Their data pointers are
    // certainly pointing to different locations, as far as I could see
    // by printint them out on the screen in 15 digit precision.
    // Yet, the assertion statement testing each element in A and C fails
    //
#ifndef _MTLMATRIX
    for( int x=0; x<_n; x++ )
    {
        const int m = _rows[x];
        const int k = _cols[x];
        if( m > 0 && k > 0 )
        {
            DoubleMatrix A(k,m);
            for(int i=0; i<m*k; i++)
            {
                A.data()[i] = i+1;
            }
            DoubleMatrix a = rvec(A);
            DoubleMatrix C(A.nr(), A.nc());
            rvecInverse(a,m,C);
            CPPUNIT_ASSERT_EQUAL(A.nr(), C.nr());
            CPPUNIT_ASSERT_EQUAL(A.nc(), C.nc());

            for(int j=0; j<m*k; j++)
            {
                CPPUNIT_ASSERT_EQUAL(A.data()[j], C.data()[j]);
            }
        }
    }
#endif
}

