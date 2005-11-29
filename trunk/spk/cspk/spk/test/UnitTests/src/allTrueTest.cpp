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
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/Test.h>

#include "../../../spk/allTrue.h"
#include "../../../spk/DoubleMatrix.h"
#include "allTrueTest.h"

using namespace CppUnit;

void allTrueTest::setUp()
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

    T=1.0;
}
void allTrueTest::tearDown()
{
	delete [] nRows;
	delete [] nCols;
}

Test* allTrueTest::suite()
{
    TestSuite *suiteOfTests = new TestSuite( "allTrueTest" );
    suiteOfTests->addTest(new TestCaller<allTrueTest>("dimAcceptanceTest", 
						      &allTrueTest::dimAcceptanceTest));
    suiteOfTests->addTest(new TestCaller<allTrueTest>("trueInMiddle", 
						      &allTrueTest::trueInMiddle));
    suiteOfTests->addTest(new TestCaller<allTrueTest>("trueAtTop", 
						      &allTrueTest::trueAtTop));
    suiteOfTests->addTest(new TestCaller<allTrueTest>("trueAtEnd", 
						      &allTrueTest::trueAtEnd));
    suiteOfTests->addTest(new TestCaller<allTrueTest>("allTrues", 
						      &allTrueTest::allTrues));
    suiteOfTests->addTest(new TestCaller<allTrueTest>("allFalses", 
						      &allTrueTest::allFalses));

    return suiteOfTests;
}

void allTrueTest::dimAcceptanceTest()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        a.fill(T);
        CPPUNIT_ASSERT_MESSAGE( "allTrue(a)", allTrue(a) );
    }
}
void allTrueTest::trueInMiddle()
{
    int m=3;
    int n=4;

    DoubleMatrix a(m,n);
    a.fill(T+0.1);
    a.data()[m*n/2+1] = T;
    CPPUNIT_ASSERT_MESSAGE( "!allTrue(a)", !allTrue(a) );
}
void allTrueTest::trueAtTop()
{
    int m=3;
    int n=4;

    DoubleMatrix a(m,n);
    a.fill(T+0.1);
    a.data()[1] = T;
    CPPUNIT_ASSERT_MESSAGE( "!allTrue(a)", !allTrue(a) );
}
void allTrueTest::trueAtEnd()
{
    int m=3;
    int n=4;

    DoubleMatrix a(m,n);
    a.fill(T+0.1);
    a.data()[m*n-1] = T;
    CPPUNIT_ASSERT_MESSAGE( "!allTrue(a)", !allTrue(a) );
}
void allTrueTest::allTrues()
{
    int m=3;
    int n=4;

    DoubleMatrix a(m,n);
    a.fill(T);
    CPPUNIT_ASSERT_MESSAGE( "allTrue(a)", allTrue(a) );
}
void allTrueTest::allFalses()
{
    int m=3;
    int n=4;

    DoubleMatrix a(m,n);
    a.fill(T+0.1);
    CPPUNIT_ASSERT_MESSAGE( "!allTrue(a)", !allTrue(a) );
}
