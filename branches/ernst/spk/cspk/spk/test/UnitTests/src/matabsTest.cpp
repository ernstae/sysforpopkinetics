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
 * File: matabsTest.cpp
 *
 *
 * Test cases for matabs()
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
#include "../../../spk/matabs.h"
#include "matabsTest.h"

#include <cmath>

using namespace std;
using namespace CppUnit;

void matabsTest::setUp()
{
    // initializations
}
void matabsTest::tearDown()
{
    // clean up
}

Test* matabsTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("matabsTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<matabsTest>("positiveOnly", &matabsTest::positiveOnly));
    suiteOfTests->addTest(new TestCaller<matabsTest>("negativeOnly", &matabsTest::negativeOnly));
    suiteOfTests->addTest(new TestCaller<matabsTest>("mix",          &matabsTest::mix));

    return suiteOfTests;
}

void matabsTest::positiveOnly()
{
    const double PVAL = +3.1;
    const double NVAL = -3.1;
    int cols[] = {0, 0, 0, 1, 1, 1, 2, 2, 2};
    int rows[] = {0, 1, 2, 0, 1, 2, 0, 1, 2};

    for( int i=0; i<8; i++ )
    {
        DoubleMatrix original(rows[i], cols[i]);
        original.fill(PVAL);

        DoubleMatrix absolute = matabs(original);
        for(int col=0; col < original.nc(); col++)
        {
            for(int row=0; row < original.nr(); row++)
            {
                CPPUNIT_ASSERT_EQUAL(fabs(original.data()[col*original.nr()+row]), absolute.data()[col*original.nr()+row]);
            }
        }
    }
}
void matabsTest::negativeOnly()
{
    const double PVAL = +3.1;
    const double NVAL = -3.1;
    int cols[] = {0, 0, 0, 1, 1, 1, 2, 2, 2};
    int rows[] = {0, 1, 2, 0, 1, 2, 0, 1, 2};

    for( int i=0; i<8; i++ )
    {
        DoubleMatrix original(rows[i], cols[i]);
        original.fill(NVAL);

        DoubleMatrix absolute = matabs(original);
        for(int col=0; col < original.nc(); col++)
        {
            for(int row=0; row < original.nr(); row++)
            {
                CPPUNIT_ASSERT_EQUAL(fabs(original.data()[col*original.nr()+row]), absolute.data()[col*original.nr()+row]);
            }
        }
    }
}
void matabsTest::mix()
{
    const double PVAL = +3.1;
    const double NVAL = -3.1;
    int cols[] = {0, 0, 0, 1, 1, 1, 2, 2, 2};
    int rows[] = {0, 1, 2, 0, 1, 2, 0, 1, 2};
    int col, row;

    for( int i=0; i<8; i++ )
    {
        DoubleMatrix original(rows[i], cols[i]);

        for(col=0; col<original.nc(); col++)
        {
            for(row=0; row<original.nr(); row++)
            {
                original.data()[col*original.nr()+row] =(col*row % 2 == 0? PVAL : NVAL);
            }
        }

        DoubleMatrix absolute = matabs(original);

        for(col=0; col < original.nc(); col++)
        {
            for(row=0; row < original.nr(); row++)
            {
                CPPUNIT_ASSERT_EQUAL(fabs(original.data()[col*original.nr()+row]), absolute.data()[col*original.nr()+row]);
            }
        }
    }
}
