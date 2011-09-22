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
 * File: matminTest.cpp
 *
 *
 * Test cases for matmin()
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
#include "../../../spk/matmin.h"
#include "matminTest.h"

#include <cfloat>

using namespace std;
using namespace CppUnit;

static int rows[] = {0,0,0,1,1,1,2,2,2};
static int cols[] = {0,1,2,0,1,2,0,1,2};

void matminTest::setUp()
{
    // initializations
}
void matminTest::tearDown()
{
    // clean up
}

Test* matminTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "matminTest" );

  // duplicate the following example for each test case, replacing the case name.
  suiteOfTests->addTest(new TestCaller<matminTest>("scalarVersion",   &matminTest::scalarVersion));
  suiteOfTests->addTest(new TestCaller<matminTest>("matricesVersion", &matminTest::matricesVersion));
  return suiteOfTests;
}


void matminTest::scalarVersion()
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

            double actual = matmin(original);
            CPPUNIT_ASSERT_EQUAL(expected, matmin(original));
        }
    }
    for( i=0; i<8; i++ )
    {
        DoubleMatrix original(rows[i], cols[i]);
        if( !original.isEmpty() )
        {
            original.fill(DBL_MAX);
            middle = original.nr()*original.nc() / 2;
            original.data()[middle] = DBL_MIN;
            expected = original.data()[middle];

            double actual = matmin(original);
            CPPUNIT_ASSERT_EQUAL(expected, matmin(original));
        }
    }
}
void matminTest::matricesVersion()
{
    for( int i=0; i<8; i++ )
    {
        DoubleMatrix A(rows[i], cols[i]);
        DoubleMatrix B(A.nr(), A.nc());
        A.fill(DBL_MIN);
        B.fill(DBL_MAX);
        DoubleMatrix C = matmin(A,B);
        for( int j=0; j<A.nc(); j++ )
        {
            for(int i=0; i<A.nr(); i++)
            {
                CPPUNIT_ASSERT_EQUAL(A.data()[i], C.data()[i]);
            }
        }
    }
}
