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
 * File: mulByScalarTest.cpp
 *
 *
 * Test cases for mulByScalar
 *
 * Author: <AUTHOR_NAME>
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>

#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/mulByScalar.h"
#include "mulByScalarTest.h"

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>

#include <ctime>
#include <cmath>
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"

using namespace CppUnit;

void mulByScalarTest::setUp()
{
    _rows.push_back(0); _cols.push_back(0);
    _rows.push_back(0); _cols.push_back(1);
    _rows.push_back(1); _cols.push_back(0);
    _rows.push_back(1); _cols.push_back(1);
    _rows.push_back(3); _cols.push_back(1);
    _rows.push_back(1); _cols.push_back(3);

    _n=_rows.size();

    double *pdA;
    DoubleMatrix *A;

	// seed the random number generator
	srand( time(0) );
	_scalar = 0.0;
	do{
		_scalar = rand() / 1000.0;
	}while(_scalar == 0.0);
    
    for( int i=0; i<_n; i++ )
    {
	    A  = new DoubleMatrix( _rows[i], _cols[i] );
        pdA = A->data();
	    for( int j=0; j<_rows[i]*_cols[i]; j++ ){
		    pdA[j] = rand() / 10000.0;
	    }

        _As.push_back(A);
    }
}
void mulByScalarTest::tearDown()
{
    for( int i=0; i<_n; i++ )
    {
        delete _As[i];
    }
}

Test* mulByScalarTest::suite()
{
	TestSuite *suiteOfTests = new TestSuite( "mulByScalarTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<mulByScalarTest>("testMatScalarVal", &mulByScalarTest::testMatScalarVal));
    suiteOfTests->addTest(new TestCaller<mulByScalarTest>("testMatMatVal",    &mulByScalarTest::testMatMatVal));
    suiteOfTests->addTest(new TestCaller<mulByScalarTest>("testMatScalarRef", &mulByScalarTest::testMatScalarRef));

    return suiteOfTests;
}

void mulByScalarTest::testMatScalarVal()
{
    // Testing multiplication by a scalar
    for( int i=0; i<_n; i++ )
    {
		// Compute B = A * scalar;
		DoubleMatrix dmatB = mulByScalar(*_As[i], _scalar);
		double* pdB  = dmatB.data();
        double* pdA  = _As[i]->data();


		// Compute A = dmatAA = B / scalar
		for(int j=0; j<_rows[i]*_cols[i]; j++ )
        {
			CPPUNIT_ASSERT_DOUBLES_EQUAL( pdA[j], pdB[j] / _scalar, 
                fabs(pdA[j] - pdB[j]*_scalar)/fabs(pdA[j])*DBL_EPS_EQUAL_MULT
                );
		}
	}

}
void mulByScalarTest::testMatMatVal()
{
    // Testing elementwise multiplication by another matrix
    for( int i=0; i<_n; i++ )
    {
        DoubleMatrix Scalars(_As[i]->nr(), _As[i]->nc());
		DoubleMatrix dmatB(_As[i]->nr(), _As[i]->nc());

        double* pdScalars = Scalars.data();
        double* pdA  = _As[i]->data();

        Scalars.fill(_scalar);

		// Compute B = A * scalar;
        dmatB = mulByScalar(*_As[i], Scalars);
		double* pdB  = dmatB.data();

		// Compute A = dmatAA = B / scalar
		for(int j=0; j<_rows[i]*_cols[i]; j++ )
        {
			CPPUNIT_ASSERT_DOUBLES_EQUAL( pdA[j], pdB[j] / pdScalars[j], 
                fabs(pdA[j] - pdB[j]*pdScalars[j])/fabs(pdA[j])*DBL_EPS_EQUAL_MULT
                );
		}
	}

}

void mulByScalarTest::testMatScalarRef()
{
    // Testing multiplication by a scalar
    for( int i=0; i<_n; i++ )
    {
		// Compute B = A * scalar;
		DoubleMatrix dmatB(_As[i]->nr(), _As[i]->nc());
        mulByScalar(*_As[i], _scalar, dmatB);
		double* pdB  = dmatB.data();
        double* pdA  = _As[i]->data();


		// Compute A = dmatAA = B / scalar
		for(int j=0; j<_rows[i]*_cols[i]; j++ )
        {
			CPPUNIT_ASSERT_DOUBLES_EQUAL( pdA[j], pdB[j] / _scalar, 
                fabs(pdA[j] - pdB[j]*_scalar)/fabs(pdA[j])*DBL_EPS_EQUAL_MULT
                );
		}
	}

}
