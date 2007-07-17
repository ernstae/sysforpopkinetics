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
#include "../../../spk/elementwiseAnd.h"
#include "../../../spk/DoubleMatrix.h"
#include "elementwiseAndTest.h"

using namespace CppUnit;

void elementwiseAndTest::setUp()
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

    T   = 1.0;
    F   = 0.0;
    neg =-1.0;
    pos = 2.0; 
}
void elementwiseAndTest::tearDown()
{
	delete [] nRows;
	delete [] nCols;
}

Test* elementwiseAndTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "elementwiseAndTest" );
    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allTrueAgainstAllTrue", 
							     &elementwiseAndTest::allTrueAgainstAllTrue));
    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allTrueAgainstAllFalse",  
							     &elementwiseAndTest::allTrueAgainstAllFalse));
    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allTrueAgainstAllNeg",  
							     &elementwiseAndTest::allTrueAgainstAllNeg));
    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allTrueAgainstAllPos",  
							     &elementwiseAndTest::allTrueAgainstAllPos));

    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allFalseAgainstAllTrue",  
							     &elementwiseAndTest::allFalseAgainstAllTrue));
    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allFalseAgainstAllFalse",  
							     &elementwiseAndTest::allFalseAgainstAllFalse));
    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allFalseAgainstAllNeg",  
							     &elementwiseAndTest::allFalseAgainstAllNeg));
    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allFalseAgainstAllPos",  
							     &elementwiseAndTest::allFalseAgainstAllPos));

    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allNegAgainstAllTrue",  
							     &elementwiseAndTest::allNegAgainstAllTrue));
    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allNegAgainstAllFalse",  
							     &elementwiseAndTest::allNegAgainstAllFalse));
    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allNegAgainstAllNeg",  
							     &elementwiseAndTest::allNegAgainstAllNeg));
    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allNegAgainstAllPos",  
							     &elementwiseAndTest::allNegAgainstAllPos));

    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allPosAgainstAllTrue",  
							     &elementwiseAndTest::allPosAgainstAllTrue));
    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allPosAgainstAllFalse",  
							     &elementwiseAndTest::allPosAgainstAllFalse));
    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allPosAgainstAllNeg",  
							     &elementwiseAndTest::allPosAgainstAllNeg));
    suiteOfTests->addTest(new TestCaller<elementwiseAndTest>("allPosAgainstAllPos",  
							     &elementwiseAndTest::allPosAgainstAllPos));

    return suiteOfTests;
}

//----------------------------------------------------------------------
void elementwiseAndTest::allTrueAgainstAllTrue()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(T);
        b.fill(T);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_DOUBLES_EQUAL(c.data()[j], 1.0, c.data()[j]-1.0);
        }
    }
}
void elementwiseAndTest::allTrueAgainstAllFalse()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(T);
        b.fill(F);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseAndTest::allTrueAgainstAllNeg()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(T);
        b.fill(neg);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseAndTest::allTrueAgainstAllPos()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(T);
        b.fill(pos);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
//----------------------------------------------------------------------
void elementwiseAndTest::allFalseAgainstAllTrue()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(F);
        b.fill(T);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseAndTest::allFalseAgainstAllFalse()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(F);
        b.fill(F);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseAndTest::allFalseAgainstAllNeg()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(F);
        b.fill(neg);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseAndTest::allFalseAgainstAllPos()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(F);
        b.fill(pos);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
//----------------------------------------------------------------------
void elementwiseAndTest::allNegAgainstAllTrue()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(neg);
        b.fill(T);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseAndTest::allNegAgainstAllFalse()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(neg);
        b.fill(F);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseAndTest::allNegAgainstAllNeg()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(neg);
        b.fill(neg);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseAndTest::allNegAgainstAllPos()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(neg);
        b.fill(pos);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
//----------------------------------------------------------------------
void elementwiseAndTest::allPosAgainstAllTrue()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(pos);
        b.fill(T);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseAndTest::allPosAgainstAllFalse()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(pos);
        b.fill(F);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseAndTest::allPosAgainstAllNeg()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(pos);
        b.fill(neg);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseAndTest::allPosAgainstAllPos()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(pos);
        b.fill(pos);
        DoubleMatrix c = bband(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.00);
        }
    }
}
