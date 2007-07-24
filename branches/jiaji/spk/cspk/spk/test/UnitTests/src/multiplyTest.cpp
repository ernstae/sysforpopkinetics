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
 * File: multiplyTest.cpp
 *
 *
 * Test cases for multiply(...)
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/


#include <iostream>
#include <fstream>
#include <string>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>

#include "multiplyTest.h"
#include "../../../spk/multiply.h"
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/SpkValarray.h"

using SPK_VA::valarray;
using namespace CppUnit;

void multiplyTest::setUp()
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
}
void multiplyTest::tearDown()
{
	delete [] nRows;
	delete [] nCols;
}

Test* multiplyTest::suite()
{
    TestSuite *suiteOfTests = new TestSuite( "multiplyTest" );
    suiteOfTests->addTest(new TestCaller<multiplyTest>("testMultiply",         &multiplyTest::testMultiply));
    suiteOfTests->addTest(new TestCaller<multiplyTest>("testRefMultiply",      &multiplyTest::testRefMultiply));
    suiteOfTests->addTest(new TestCaller<multiplyTest>("testValarrayMultiply", &multiplyTest::testValarrayMultiply));
    
    return suiteOfTests;
}
void multiplyTest::testMultiply()
{
	int m, nn, k, p, i,j,s;
	for(s=0; s<n; s++)
	{
		// A(m by p) * B(p by nn) = C(m by nn)
		m = nRows[s];
		p = nCols[s];
		nn = nRows[s];
		DoubleMatrix A(m, p);
		A.fill(2);
		DoubleMatrix B(p, nn);
		B.fill(1);
		DoubleMatrix CC(m,nn);
		CC.fill(0);

		DoubleMatrix C(m, nn);
		C = multiply(A, B);
		if( !C.isEmpty() )
		{
			for(i=0; i<m; i++)
			{
				for(j=0; j<nn; j++)
				{
					for(k=0; k<p; k++)
					{
						CC.data()[i+j*m] += A.data()[i+k*m]*B.data()[k+j*p];
					}
				}
			}
			for(i=0; i<m; i++)
			{
				for(j=0; j<nn; j++)
				{
					for(k=0; k<p; k++)
					{
						CPPUNIT_ASSERT_DOUBLES_EQUAL(CC.data()[i+j*m], C.data()[i+j*m], 0.0 );
					}
				}
			}

		}
	}
}

/*
 * Testing the 3 argument version.
 *
 * Revisit by Sachiko
 * The routine asserts aliacing in the arguments.  This should be replaced by perhaps exception
 * throwing so that a test case can be written for checking it.
 */
void multiplyTest::testRefMultiply()
{
	int m, nn, k, p, i,j,s;
	for(s=0; s<n; s++)
	{
		// A(m by p) * B(p by nn) = C(m by nn)
		m = nRows[s];
		p = nCols[s];
		nn = nRows[s];
		DoubleMatrix A(m, p);
		A.fill(2);
		DoubleMatrix B(p, nn);
		B.fill(1);
		DoubleMatrix CC(m,nn);
		CC.fill(0);

		DoubleMatrix C(m, nn);
		multiply(A, B, C);
		if( !C.isEmpty() )
		{
			for(i=0; i<m; i++)
			{
				for(j=0; j<nn; j++)
				{
					for(k=0; k<p; k++)
					{
						CC.data()[i+j*m] += A.data()[i+k*m]*B.data()[k+j*p];
					}
				}
			}
			for(i=0; i<m; i++)
			{
				for(j=0; j<nn; j++)
				{
					for(k=0; k<p; k++)
					{
						CPPUNIT_ASSERT_DOUBLES_EQUAL(CC.data()[i+j*m], C.data()[i+j*m], 0.0 );
					}
				}
			}

		}
	}
}
void multiplyTest::testValarrayMultiply()
{
  using namespace std;

	int m, nn, k, p, i,j,s;
	for(s=0; s<n; s++)
	{
		// A(m by p) * B(p by nn) = C(m by nn)
		m = nRows[s];
		p = nCols[s];
		nn = nRows[s];

        valarray<double> A(m * p);
        A = 2.0;
        valarray<double> B(p * nn);
        B = 1.0;
        valarray<double> C = multiply(A,p, B,nn);
        valarray<double> CC( m * nn );
        CC = 0.0;

		if( C.size() > 0 )
		{
			for(i=0; i<m; i++)
			{
				for(j=0; j<nn; j++)
				{
					for(k=0; k<p; k++)
					{
						CC[i+j*m] += A[i+k*m]*B[k+j*p];
					}
				}
			}
			for(i=0; i<m; i++)
			{
				for(j=0; j<nn; j++)
				{
					for(k=0; k<p; k++)
					{
						CPPUNIT_ASSERT_DOUBLES_EQUAL(CC[i+j*m], C[i+j*m], 0.0 );
					}
				}
			}

		}
	}
}
