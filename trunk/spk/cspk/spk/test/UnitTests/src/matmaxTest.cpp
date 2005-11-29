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
 * File: matmaxTest.cpp
 *
 *
 * Test cases for matmax()
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
#include "../../../spk/matmax.h"
#include "matmaxTest.h"

#include <cfloat>

using namespace std;
using namespace CppUnit;

static int rows[] = {0,0,0,1,1,1,2,2,2};
static int cols[] = {0,1,2,0,1,2,0,1,2};

void matmaxTest::setUp()
{
    // initializations
}
void matmaxTest::tearDown()
{
    // clean up
}

Test* matmaxTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("matmaxTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<matmaxTest>("scalarVersion",   &matmaxTest::scalarVersion));
    suiteOfTests->addTest(new TestCaller<matmaxTest>("matricesVersion", &matmaxTest::matricesVersion));
    return suiteOfTests;
}

void matmaxTest::scalarVersion()
{
    double expected;
    int middle;

    int i;
    for( i=0; i<8; i++ )
    {
        DoubleMatrix original(rows[i], cols[i]);
        original.fill(DBL_MIN);
        expected = DBL_MIN;
        if( !original.isEmpty() )
        {

            double actual = matmax(original);
            CPPUNIT_ASSERT_EQUAL(expected, matmax(original));
        }
    }
    for( i=0; i<8; i++ )
    {
        DoubleMatrix original(rows[i], cols[i]);
        if( !original.isEmpty() )
        {
            original.fill(DBL_MIN);
            middle = original.nr()*original.nc() / 2;
            original.data()[middle] = DBL_MAX;
            expected = original.data()[middle];

            double actual = matmax(original);
            CPPUNIT_ASSERT_EQUAL(expected, matmax(original));
        }
    }
}
void matmaxTest::matricesVersion()
{
    for( int i=0; i<8; i++ )
    {
        DoubleMatrix A(rows[i], cols[i]);
        DoubleMatrix B(A.nr(), A.nc());
        A.fill(DBL_MIN);
        B.fill(DBL_MAX);
        DoubleMatrix C = matmax(A,B);
        for( int j=0; j<A.nc(); j++ )
        {
            for(int i=0; i<A.nr(); i++)
            {
                CPPUNIT_ASSERT_EQUAL(B.data()[i], C.data()[i]);
            }
        }
    }
}
