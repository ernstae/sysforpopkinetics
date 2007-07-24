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
#include "../../../spk/elementwiseOr.h"
#include "elementwiseOrTest.h"
#include "../../../spk/DoubleMatrix.h"

using namespace CppUnit;
void elementwiseOrTest::setUp()
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
void elementwiseOrTest::tearDown()
{
	delete [] nRows;
	delete [] nCols;
}

Test* elementwiseOrTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "elementwiseOrTest" );
    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allTrueAgainstAllTrue", 
							    &elementwiseOrTest::allTrueAgainstAllTrue));
    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allTrueAgainstAllFalse",  
							    &elementwiseOrTest::allTrueAgainstAllFalse));
    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allTrueAgainstAllNeg",  
							    &elementwiseOrTest::allTrueAgainstAllNeg));
    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allTrueAgainstAllPos",  
							    &elementwiseOrTest::allTrueAgainstAllPos));

    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allFalseAgainstAllTrue",  
							    &elementwiseOrTest::allFalseAgainstAllTrue));
    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allFalseAgainstAllFalse",  
							    &elementwiseOrTest::allFalseAgainstAllFalse));
    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allFalseAgainstAllNeg",  
							    &elementwiseOrTest::allFalseAgainstAllNeg));
    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allFalseAgainstAllPos",  
							    &elementwiseOrTest::allFalseAgainstAllPos));

    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allNegAgainstAllTrue",  
							    &elementwiseOrTest::allNegAgainstAllTrue));
    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allNegAgainstAllFalse",  
							    &elementwiseOrTest::allNegAgainstAllFalse));
    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allNegAgainstAllNeg",  
							    &elementwiseOrTest::allNegAgainstAllNeg));
    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allNegAgainstAllPos",  
							    &elementwiseOrTest::allNegAgainstAllPos));

    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allPosAgainstAllTrue",  
							    &elementwiseOrTest::allPosAgainstAllTrue));
    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allPosAgainstAllFalse",   
							    &elementwiseOrTest::allPosAgainstAllFalse));
    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allPosAgainstAllNeg",   
							    &elementwiseOrTest::allPosAgainstAllNeg));
    suiteOfTests->addTest(new TestCaller<elementwiseOrTest>("allPosAgainstAllPos",   
							    &elementwiseOrTest::allPosAgainstAllPos));

    return suiteOfTests;
}

//----------------------------------------------------------------------
void elementwiseOrTest::allTrueAgainstAllTrue()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(T);
        b.fill(T);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
void elementwiseOrTest::allTrueAgainstAllFalse()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(T);
        b.fill(F);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
void elementwiseOrTest::allTrueAgainstAllNeg()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(T);
        b.fill(neg);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
void elementwiseOrTest::allTrueAgainstAllPos()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(T);
        b.fill(pos);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
//----------------------------------------------------------------------
void elementwiseOrTest::allFalseAgainstAllTrue()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(F);
        b.fill(T);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
void elementwiseOrTest::allFalseAgainstAllFalse()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(F);
        b.fill(F);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseOrTest::allFalseAgainstAllNeg()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(F);
        b.fill(neg);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseOrTest::allFalseAgainstAllPos()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(F);
        b.fill(pos);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
//----------------------------------------------------------------------
void elementwiseOrTest::allNegAgainstAllTrue()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(neg);
        b.fill(T);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
void elementwiseOrTest::allNegAgainstAllFalse()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(neg);
        b.fill(F);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseOrTest::allNegAgainstAllNeg()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(neg);
        b.fill(neg);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseOrTest::allNegAgainstAllPos()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(neg);
        b.fill(pos);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
//----------------------------------------------------------------------
void elementwiseOrTest::allPosAgainstAllTrue()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(pos);
        b.fill(T);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 1.0);
        }
    }
}
void elementwiseOrTest::allPosAgainstAllFalse()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(pos);
        b.fill(F);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseOrTest::allPosAgainstAllNeg()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(pos);
        b.fill(neg);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
void elementwiseOrTest::allPosAgainstAllPos()
{
    for(int i=0; i<n; i++)
    {
        DoubleMatrix a(nRows[i], nCols[i]);
        DoubleMatrix b(nRows[i], nCols[i]);
        a.fill(pos);
        b.fill(pos);
        DoubleMatrix c = bbor(a,b);
        for(int j=0; j<nRows[i]*nCols[i]; j++)
        {
            CPPUNIT_ASSERT_EQUAL(c.data()[j], 0.0);
        }
    }
}
