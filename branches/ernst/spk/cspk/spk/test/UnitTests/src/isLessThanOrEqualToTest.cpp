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
#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/isLessThanOrEqualTo.h"
#include "isLessThanOrEqualToTest.h"
#include "../../../spk/DoubleMatrix.h"

using namespace CppUnit;

void isLessThanOrEqualToTest::setUp()
{
	n = 8;
	nRows = new int[n];
		nRows[0] = 0;
		nRows[1] = 0;
		nRows[2] = 1;
		nRows[3] = 1;
		nRows[4] = 1;
		nRows[5] = 3;
		nRows[6] = 2;
		nRows[7] = 1;
	nCols = new int[n];
		nCols[0] = 0;
		nCols[1] = 1;
		nCols[2] = 0;
		nCols[3] = 1;
		nCols[4] = 3;
		nCols[5] = 1;
		nCols[6] = 3;
		nCols[7] = 2;

    pos   = 1.0;
    zero  = 0.0;
    neg   =-1.0;
}
void isLessThanOrEqualToTest::tearDown()
{
	delete [] nRows;
	delete [] nCols;
}

Test* isLessThanOrEqualToTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "isLessThanOrEqualToTest" );

    // Tests against a scalar
    suiteOfTests->addTest(new TestCaller<isLessThanOrEqualToTest>(
                  "allPosAgainstPos",   &isLessThanOrEqualToTest::allPosAgainstPos));
    suiteOfTests->addTest(new TestCaller<isLessThanOrEqualToTest>(
		  "allPosAgainstZero",  &isLessThanOrEqualToTest::allPosAgainstZero));
    suiteOfTests->addTest(new TestCaller<isLessThanOrEqualToTest>(
                  "allPosAgainstZero",  &isLessThanOrEqualToTest::allPosAgainstNeg));
    suiteOfTests->addTest(new TestCaller<isLessThanOrEqualToTest>(
                  "allPosAgainstPos",   &isLessThanOrEqualToTest::allNegAgainstPos));
    suiteOfTests->addTest(new TestCaller<isLessThanOrEqualToTest>(
                  "allPosAgainstZero",  &isLessThanOrEqualToTest::allNegAgainstZero));
    suiteOfTests->addTest(new TestCaller<isLessThanOrEqualToTest>(
                  "allPosAgainstZero",  &isLessThanOrEqualToTest::allNegAgainstNeg));
    suiteOfTests->addTest(new TestCaller<isLessThanOrEqualToTest>(
                  "allZeroAgainstZero", &isLessThanOrEqualToTest::allZeroAgainstZero));

    // Tests against another matrix
    suiteOfTests->addTest(new TestCaller<isLessThanOrEqualToTest>(
                  "allPosAgainstAllPos",   &isLessThanOrEqualToTest::allPosAgainstPos));
    suiteOfTests->addTest(new TestCaller<isLessThanOrEqualToTest>(
                  "allPosAgainstAllZero",  &isLessThanOrEqualToTest::allPosAgainstZero));
    suiteOfTests->addTest(new TestCaller<isLessThanOrEqualToTest>(
                  "allPosAgainstAllZero",  &isLessThanOrEqualToTest::allPosAgainstNeg));
    suiteOfTests->addTest(new TestCaller<isLessThanOrEqualToTest>(
                  "allPosAgainstAllPos",   &isLessThanOrEqualToTest::allNegAgainstPos));
    suiteOfTests->addTest(new TestCaller<isLessThanOrEqualToTest>(
                  "allPosAgainstAllZero",  &isLessThanOrEqualToTest::allNegAgainstZero));
    suiteOfTests->addTest(new TestCaller<isLessThanOrEqualToTest>(
                  "allPosAgainstAllZero",  &isLessThanOrEqualToTest::allNegAgainstNeg));
    suiteOfTests->addTest(new TestCaller<isLessThanOrEqualToTest>(
                  "allZeroAgainstAllZero", &isLessThanOrEqualToTest::allZeroAgainstZero));
    return suiteOfTests;
}

void isLessThanOrEqualToTest::allPosAgainstPos()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        a.fill(pos);
        DoubleMatrix c = (a <= pos);
        for( int j=0; j<nRows[i]*nCols[i]; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
void isLessThanOrEqualToTest::allPosAgainstZero()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        a.fill(pos);
        DoubleMatrix c = a <= zero;
        for( int j=0; j<nRows[i]*nCols[i]; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void isLessThanOrEqualToTest::allPosAgainstNeg()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        a.fill(pos);
        DoubleMatrix c = a <= neg;
        for( int j=0; j<nRows[i]*nCols[i]; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void isLessThanOrEqualToTest::allNegAgainstPos()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        a.fill(neg);
        DoubleMatrix c = a <= pos;
        for( int j=0; j<nRows[i]*nCols[i]; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
void isLessThanOrEqualToTest::allNegAgainstZero()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        a.fill(neg);
        DoubleMatrix c = a <= zero;
        for( int j=0; j<nRows[i]*nCols[i]; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
void isLessThanOrEqualToTest::allNegAgainstNeg()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        a.fill(neg);
        DoubleMatrix c = a <= neg;
        for( int j=0; j<nRows[i]*nCols[i]; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
void isLessThanOrEqualToTest::allZeroAgainstZero()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        a.fill(zero);
        DoubleMatrix c = a <= zero;
        for( int j=0; j<nRows[i]*nCols[i]; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
//--------------------------------------------------------------------------------------
void isLessThanOrEqualToTest::allPosAgainstAllPos()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);

        a.fill(pos);
        b.fill(pos);

        DoubleMatrix c = a <= b;
        for( int j=0; j<nRows[i]*nCols[i]; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
void isLessThanOrEqualToTest::allPosAgainstAllZero()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);

        a.fill(pos);
        b.fill(zero);
        DoubleMatrix c = a <= b;
        for( int j=0; j<nRows[i]*nCols[i]; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void isLessThanOrEqualToTest::allPosAgainstAllNeg()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);

        a.fill(pos);
        b.fill(neg);
        DoubleMatrix c = a <= b;
        for( int j=0; j<nRows[i]*nCols[i]; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void isLessThanOrEqualToTest::allNegAgainstAllPos()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(neg);
        b.fill(pos);
        DoubleMatrix c = a <= b;
        for( int j=0; j<nRows[i]*nCols[i]; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
void isLessThanOrEqualToTest::allNegAgainstAllZero()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(neg);
        b.fill(zero);
        DoubleMatrix c = a <= b;
        for( int j=0; j<nRows[i]*nCols[i]; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
void isLessThanOrEqualToTest::allNegAgainstAllNeg()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(neg);
        b.fill(neg);
        DoubleMatrix c = a <= b;
        for( int j=0; j<nRows[i]*nCols[i]; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
void isLessThanOrEqualToTest::allZeroAgainstAllZero()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(neg);
        b.fill(zero);
        DoubleMatrix c = a <= zero;
        for( int j=0; j<nRows[i]*nCols[i]; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
