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
 * File: subtractTest.cpp
 *
 *
 * Test cases for subtract()
 *
 * Author: sachiko honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>

#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/subtract.h"
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"
#include "subtractTest.h"

#include <cstdlib>
#include <cmath>
#include <cfloat>
#include <ctime>

using namespace CppUnit;

void subtractTest::setUp()
{
    _rows.push_back(0);  _cols.push_back(0);
    _rows.push_back(1);  _cols.push_back(1);
    _rows.push_back(3);  _cols.push_back(3);
    _n = _rows.size();
}
void subtractTest::tearDown()
{
    // clean up
}

Test* subtractTest::suite()
{
    TestSuite *suiteOfTests = new TestSuite( "subtractTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<subtractTest>("<testSubtractVal", &subtractTest::testSubtractVal));
    suiteOfTests->addTest(new TestCaller<subtractTest>("<testSubtractRef", &subtractTest::testSubtractRef));

    return suiteOfTests;
}

void subtractTest::testSubtractRef()
{
	srand( time(0) );

    for( int k=0; k<_n; k++ )
    {
        int iRows = _rows[k];
		int iCols = _cols[k];
		DoubleMatrix A(iRows, iCols);
		DoubleMatrix B(iRows, iCols);
		DoubleMatrix BB;
		DoubleMatrix C;
		DoubleMatrix S(iRows, iCols);

		for(int i=0; i<iRows*iCols; i++)
        {
			A.data()[i] = rand() / 1000.0;
			B.data()[i] = rand() / 1000.0;
			S.data()[i] = fabs(A.data()[i]-B.data()[i])/fabs(A.data()[i])*DBL_EPS_EQUAL_MULT;
                //(fabs(A.data()[i]) >= fabs(B.data()[i]) ? fabs(A.data()[i]) :fabs(B.data()[i]) );
		}
		// Computes C = A - B
		C = subtract(A, B);

		// Computes B = A - C
		BB = subtract(A, C);

		for(int j=0; j<iRows*iCols; j++)
        {
            CPPUNIT_ASSERT_DOUBLES_EQUAL(B.data()[j], BB.data()[j], S.data()[j]);
        }
	}
}
void subtractTest::testSubtractVal()
{
	srand( time(0) );

    for( int k=0; k<_n; k++ )
    {
        int iRows = _rows[k];
		int iCols = _cols[k];
		DoubleMatrix A(iRows, iCols);
		DoubleMatrix B(iRows, iCols);
		DoubleMatrix BB;
		DoubleMatrix C;
		DoubleMatrix S(iRows, iCols);

		for(int i=0; i<iRows*iCols; i++)
        {
			A.data()[i] = rand() / 1000.0;
			B.data()[i] = rand() / 1000.0;
			S.data()[i] = fabs(A.data()[i]-B.data()[i])/fabs(A.data()[i])*DBL_EPS_EQUAL_MULT;
		}

		// Computes C = A - B
		subtract(A, B, C);

		// Computes B = A - C
		subtract(A, C, BB);
		
		for(int j=0; j<iRows*iCols; j++)
        {
            CPPUNIT_ASSERT_DOUBLES_EQUAL(B.data()[j], BB.data()[j], S.data()[j]);
        }
	}
}
