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

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>
#include <cppunit/TextTestResult.h>
#include <cppunit/ui/text/TestRunner.h>

#include "MatrixTest.h"
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/System.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/multiply.h"

using SPK_VA::valarray;
using namespace CppUnit;

static DoubleMatrix StaticMat_A(__FILE__);
static DoubleMatrix StaticIligalMat;

static DoubleMatrix* detour(int n)
{
	// this doesn't allocate data memory for each matrix
	DoubleMatrix *bunch = new DoubleMatrix[n];
	for( int i=0; i<n; i++ )
	{
		// this allocates memory and increment the counter
		bunch[i].resize(i,i);
	}
	DoubleMatrix a(0,0);
	DoubleMatrix b(1,0);
	DoubleMatrix c(1,1);
	DoubleMatrix d(a);
	DoubleMatrix e(c);
	return bunch;
}
static bool delfile(const std::string& file)
{
	try{
		System::del(File(".", file));
	}
	catch(...)
	{
		return false;
	}
	return true;

}
void MatrixTest::setUp()
{
    _n = 8;
    _nRows.reserve(_n);
    _nCols.reserve(_n);
	_nRows.push_back(0); _nCols.push_back(0);
	_nRows.push_back(0); _nCols.push_back(1);
	_nRows.push_back(1); _nCols.push_back(0);
	_nRows.push_back(1); _nCols.push_back(1);
	_nRows.push_back(1); _nCols.push_back(3);
	_nRows.push_back(3); _nCols.push_back(1);
	_nRows.push_back(2); _nCols.push_back(3);
	_nRows.push_back(1); _nCols.push_back(2);

}
void MatrixTest::tearDown()
{
    _nRows.clear();
    _nCols.clear();
}

CppUnit::Test* MatrixTest::suite()
{
  CppUnit::TestSuite *suiteOfTests = new CppUnit::TestSuite( "MatrixTest" );
  suiteOfTests->addTest( new CppUnit::TestCaller<MatrixTest>("testDefaultConstructor",
							    &MatrixTest::testDefaultConstructor ) );
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testStaticConstructor", 
							    &MatrixTest::testStaticConstructor) );    
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testCopyConstructor", 
							    &MatrixTest::testCopyConstructor));
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testSizeConstructor", 
							    &MatrixTest::testSizeConstructor));
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testScalarConstructor", 
							    &MatrixTest::testScalarConstructor));
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testResize", 
							    &MatrixTest::testResize));
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testDestructor",
							    &MatrixTest::testDestructor));
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testData", 
							    &MatrixTest::testData));
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testIsEmpty", 
							    &MatrixTest::testIsEmpty));
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testssign", 
							    &MatrixTest::testAssign));
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testNrnc",
							    &MatrixTest::testNrnc));
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testFill",
							    &MatrixTest::testFill));
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testInsertExtract", 
							    &MatrixTest::testInsertExtract));
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testMultiply", 
							    &MatrixTest::testMultiply));
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testValarrayConstructor",
							    &MatrixTest::testValarrayConstructor));   
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testValarrayAssign",
							    &MatrixTest::testValarrayAssign));
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testFromValarray", 
							    &MatrixTest::testFromValarray));
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testToValArraySelfDestruction", 
							    &MatrixTest::testToValArraySelfDestruction));
  suiteOfTests->addTest(new CppUnit::TestCaller<MatrixTest>("testToValArraySelfMaintain", 
							    &MatrixTest::testToValArraySelfMaintain));

    return suiteOfTests;
}

void MatrixTest::testDefaultConstructor()
{
	/*
	 * The deafult constructor should create a 0 by 0 matrix, whose data pointer points to the static const empty matrix.
	 */
    DoubleMatrix a;
	    
    CPPUNIT_ASSERT_EQUAL( 0, a.nr() );
	CPPUNIT_ASSERT_EQUAL( 0, a.nc() );
    CPPUNIT_ASSERT_EQUAL( 1, a.getRefCount() );
 
    CPPUNIT_ASSERT_MESSAGE("a.data() should return null.", a.data()==0);

    DoubleMatrix b;
    CPPUNIT_ASSERT_EQUAL( 0, b.nr() );
    CPPUNIT_ASSERT_EQUAL( 0, b.nc() );
    CPPUNIT_ASSERT_EQUAL( 1, b.getRefCount() );

    CPPUNIT_ASSERT_MESSAGE("b.data() should return null.", b.data()==0);
}
void MatrixTest::testStaticConstructor()
{
    /*
     * The static constructor should create a 0 by 0 matrix, whose data pointer points to the static const empty matrix.
     */
    CPPUNIT_ASSERT_EQUAL( 0, StaticMat_A.nr() );
    CPPUNIT_ASSERT_EQUAL( 0, StaticMat_A.nc() );
    CPPUNIT_ASSERT_EQUAL( 1, StaticMat_A.getRefCount() );

    CPPUNIT_ASSERT_MESSAGE( "StaticMat_A.data() should return null.", StaticMat_A.data()==0 );

}
void MatrixTest::testCopyConstructor()
{
    double val = 9;
    int i, j;

	for( i=0; i<_n; i++ )
	{
        int m = _nRows[i];
        int n = _nCols[i];
		DoubleMatrix a(m,n);

        // test dimensions
		CPPUNIT_ASSERT_EQUAL( a.nr(), m );
		CPPUNIT_ASSERT_EQUAL( a.nc(), n );
		CPPUNIT_ASSERT_EQUAL( 1, a.getRefCount() );

        for( j=0; j<m*n; j++ )
        {
            a.data()[j] = val;
        }

        for( j=0; j<m*n; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(val, a.data()[j]);
        }

        DoubleMatrix b( a );
		if( m*n )
		{
		    CPPUNIT_ASSERT_EQUAL( 2, b.getRefCount() );
		    CPPUNIT_ASSERT_EQUAL( 2, a.getRefCount() );
        }

        for( j=0; j<m*n; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(a.data()[j], b.data()[j]);
        }

        for( j=0; j<m*n; j++ )
        {
            b.data()[j] = a.data()[j] + 1;
        }

        for( j=0; j<m*n; j++ )
        {
            CPPUNIT_ASSERT_EQUAL(a.data()[j]+1, b.data()[j]);
        }
		CPPUNIT_ASSERT_EQUAL( 1, b.getRefCount() );
		CPPUNIT_ASSERT_EQUAL( 1, a.getRefCount() );

	}
}
void MatrixTest::testSizeConstructor()
{
	double val = 9;
    int i,j;

	for( i=0; i<_n; i++ )
	{
        int m = _nRows[i];
        int n = _nCols[i];
		DoubleMatrix a(m, n);

		CPPUNIT_ASSERT_EQUAL( m, a.nr() );
		CPPUNIT_ASSERT_EQUAL( n, a.nc() );
		CPPUNIT_ASSERT_EQUAL( 1, a.getRefCount() );
		
        for( j=0; j<m*n; j++ )
        {
            a.data()[j] = val;
        }

        for( j=0; j<m*n; j++ )
        {
            CPPUNIT_ASSERT_DOUBLES_EQUAL(val, a.data()[j], 0);
        }
    }
}
void MatrixTest::testScalarConstructor()
{
    const double val = 9.0;

	DoubleMatrix a(val);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1, a.nr(), 0 );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1, a.nc(), 0 );
	CPPUNIT_ASSERT_EQUAL( 1, a.getRefCount() );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( val, a.data()[0], 0 );

	DoubleMatrix b(a);
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1, b.nr(), 0 );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( 1, b.nc(), 0 );
	CPPUNIT_ASSERT_DOUBLES_EQUAL( val, b.data()[0], 0 );
}
void MatrixTest::testDestructor()
{
	DoubleMatrix *p = detour(3);
	delete [] p;

    // create an empty matrix and resize it later
    DoubleMatrix a;
    a.resize(1,1);

    // Test whether it properly handles a static matrix
    StaticMat_A.resize(1,1);
    StaticMat_A.resize(0,0);
}
void MatrixTest::testResize()
{
	const double val = 9.0;
    int i, j;

	for( i=0; i<_n; i++ )
	{
        int m = _nRows[i];
        int n = _nCols[i];

        DoubleMatrix a(m, n);
		CPPUNIT_ASSERT_DOUBLES_EQUAL(m, a.nr(), 0);
		CPPUNIT_ASSERT_DOUBLES_EQUAL(n, a.nc(), 0);

        for( j=0; j<m*n; j++ )
        {
            a.data()[j] = val;
        }
        

        // Check contents
        for( j=0; j<m*n; j++ )
        {
            CPPUNIT_ASSERT_DOUBLES_EQUAL(val, a.data()[j], 0);
        }

        // Grow
        a.resize(m+10, n+10);   // this action gives no guarantee in data reservation
        CPPUNIT_ASSERT_DOUBLES_EQUAL(m+10, a.nr(), 0);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(n+10, a.nc(), 0);

        // Shrink
        a.resize(m,n);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(m, a.nr(), 0);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(n, a.nc(), 0);


        // Grow
        a.resize(m+2, n+2);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(m+2, a.nr(), 0);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(n+2, a.nc(), 0);

        StaticMat_A.resize(m,n);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(m, StaticMat_A.nr(), 0);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(n, StaticMat_A.nc(), 0);

	}
}
void MatrixTest::testData()
{
    const double val = 9.0;
    int i, j;

	for(i=0; i<_n; i++)
	{
        int m = _nRows[i];
        int n = _nCols[i];

		// This should do a deep copy.
		DoubleMatrix a(m,n);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(m, a.nr(), 0);
        CPPUNIT_ASSERT_DOUBLES_EQUAL(n, a.nc(), 0);

		if( m*n==0 )
		{
			CPPUNIT_ASSERT_MESSAGE("An empty matrix should return null as a data pointer.", a.data()==0);
		}
		else
        {
            CPPUNIT_ASSERT_MESSAGE("A NON empty matrix should return NON null as a data pointer.", a.data()!=0);
            for( j=0; j<m*n; j++ )
                a.data()[j] = val;
        }
        for( j=0; j<m*n; j++ )
        {
            CPPUNIT_ASSERT_DOUBLES_EQUAL(val, a.data()[j], 0);
        }
	}
}
void MatrixTest::testNrnc()
{
	double val = 9.0;

	for(int i=0; i<_n; i++)
	{
        int m = _nRows[i];
        int n = _nCols[i];
		DoubleMatrix a(m,n);
		CPPUNIT_ASSERT_DOUBLES_EQUAL(m, a.nr(), 0);
		CPPUNIT_ASSERT_DOUBLES_EQUAL(n, a.nc(), 0);

		DoubleMatrix b;
        b.resize(m,n);
		CPPUNIT_ASSERT_DOUBLES_EQUAL(m, b.nr(), 0);
		CPPUNIT_ASSERT_DOUBLES_EQUAL(n, b.nc(), 0);
	}
}
void MatrixTest::testFill()
{
	double val = 9.0;

	for(int i=0; i<_n; i++)
	{
        int m = _nRows[i];
        int n = _nCols[i];

		DoubleMatrix a(m,n);
		a.fill(val);
		for(int j=0; j<m*n; j++)
		{
			CPPUNIT_ASSERT_DOUBLES_EQUAL( val, a.data()[j], 0 );
		}
	}
}
void MatrixTest::testIsEmpty()
{
	double val = 9.0;

	for(int i=0; i<_n; i++)
	{
        int m = _nRows[i];
        int n = _nCols[i];
		DoubleMatrix a(m,n);
		a.fill(val);
		if( m*n==0 )
			CPPUNIT_ASSERT_MESSAGE("a.isEmpty()", a.isEmpty());
		else
			CPPUNIT_ASSERT_MESSAGE("!a.isEmpty()", !a.isEmpty());
	}
}
void MatrixTest::testAssign()
{
	double val = 9.0;
    int i, j;

	for(i=0; i<_n; i++)
	{
        int m = _nRows[i];
        int n = _nCols[i];
		DoubleMatrix a = DoubleMatrix(m,n);
		CPPUNIT_ASSERT_DOUBLES_EQUAL(m, a.nr(),0);
		CPPUNIT_ASSERT_DOUBLES_EQUAL(n, a.nc(),0);

        DoubleMatrix b = a;
		CPPUNIT_ASSERT_DOUBLES_EQUAL(m, b.nr(),0);
		CPPUNIT_ASSERT_DOUBLES_EQUAL(n, b.nc(),0);
        if( m*n > 0 )
            CPPUNIT_ASSERT_MESSAGE("a and b should have unique memory location for data!", a.data() != b.data());
        for( j=0; j<m*n; j++ )
        {
            a.data()[j] = val;
            b.data()[j] = val*2;

            CPPUNIT_ASSERT_DOUBLES_EQUAL(val, a.data()[j], 0);
            CPPUNIT_ASSERT_MESSAGE("a.data()[j] != b.data()[j]", a.data()[j] != b.data()[j]);
        }
	}
}
void MatrixTest::testMultiply()
{
	int m, nn, k, p, i,j,s;
	for(s=0; s<_n; s++)
	{
		// A(m by p) * B(p by nn) = C(m by nn)
		m = _nRows[s];
		p = _nCols[s];
		nn = _nRows[s];
		DoubleMatrix A(m, p);
		A.fill(2);
		DoubleMatrix B(p, nn);
		B.fill(1);
		DoubleMatrix CC(m,nn);
		CC.fill(0);

		DoubleMatrix C(m, nn);
		multiply(A,B,C);
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
						CPPUNIT_ASSERT_DOUBLES_EQUAL(CC.data()[i+j*m], C.data()[i+j*m], 0 );
					}
				}
			}

		}
	}
}

void MatrixTest::testInsertExtract()
{
	const double val=0.0;
    int i, m, n;
    std::string filename("inserted");

    for( i=0; i<_n; i++ )
    {
        m = _nRows[i];
        n = _nCols[i];
        DoubleMatrix d1(m,n);
		d1.fill(0);
		DoubleMatrix d2;

        std::ofstream inserted(filename.c_str());
		CPPUNIT_ASSERT_MESSAGE("inserted.good()", inserted.good());
        inserted << d1;
        inserted.close();

        std::ifstream extracted(filename.c_str());
		CPPUNIT_ASSERT_MESSAGE("extracted.good()", extracted.good());
        extracted >> d2;

		for( int k=0; k<m*n; k++ )
		{
            CPPUNIT_ASSERT_DOUBLES_EQUAL(d1.data()[k], d2.data()[k], 0);
		}
        extracted.close();
    }    
	delfile(filename);
}
void MatrixTest::testValarrayConstructor()
{
	// Test finite size array
	int size = 6;
    valarray<double> array( size );
	for( int j = 0; j < size; j++ )
		array[ j ] = j;

	DoubleMatrix dmat( array, 2 );
	double* pdmat = dmat.data();
	for( int i = 0; i < size; i++ )
	    CPPUNIT_ASSERT_DOUBLES_EQUAL( pdmat[ i ], array[ i ], 0 );
	CPPUNIT_ASSERT_EQUAL( dmat.nc(), 2 );
	CPPUNIT_ASSERT_EQUAL( dmat.nr(), 3 );
	CPPUNIT_ASSERT_EQUAL( dmat.getRefCount(), 1 );

	// Test zero size array
	array.resize( 0 );
	DoubleMatrix zero( array, 0 );
	assert( zero.data() == 0 );
	CPPUNIT_ASSERT_EQUAL( zero.nc(), 0 );
	CPPUNIT_ASSERT_EQUAL( zero.nr(), 0 );
	CPPUNIT_ASSERT_EQUAL( zero.getRefCount(), 1 );
}
void MatrixTest::testValarrayAssign()
{
	// Test finite array size
	int size = 6;
    valarray<double> array( size );
	for( int j = 0; j < size; j++ )
		array[ j ] = j;

	DoubleMatrix dmat;
	dmat = array;
	double* pdmat = dmat.data();
	for( int i = 0; i < size; i++ )
	    CPPUNIT_ASSERT_DOUBLES_EQUAL( pdmat[ i ], array[ i ], 0 );
	CPPUNIT_ASSERT_EQUAL( dmat.nc(), 1 );
	CPPUNIT_ASSERT_EQUAL( dmat.nr(), 6 );
	CPPUNIT_ASSERT_EQUAL( dmat.getRefCount(), 1 );

	// Test zero size array
	array.resize( 0 );
	DoubleMatrix zero;
	zero = array;
	assert( zero.data() == 0 );
	CPPUNIT_ASSERT_EQUAL( zero.nc(), 0 );
	CPPUNIT_ASSERT_EQUAL( zero.nr(), 0 );
	CPPUNIT_ASSERT_EQUAL( zero.getRefCount(), 1 );
}
void MatrixTest::testFromValarray()
{
	// Test finite array size
	int size = 6;
	valarray<double> array( size );
	for( int j = 0; j < size; j++ )
		array[ j ] = j;

	DoubleMatrix dmat;
	double* pdmat = dmat.fromValarray( array, 2 ).data();
	for( int i = 0; i < size; i++ )
	    CPPUNIT_ASSERT_DOUBLES_EQUAL( pdmat[ i ], array[ i ], 0 );
	CPPUNIT_ASSERT_EQUAL( dmat.nc(), 2 );
	CPPUNIT_ASSERT_EQUAL( dmat.nr(), 3 );
	CPPUNIT_ASSERT_EQUAL( dmat.getRefCount(), 1 );
	
	// Test zero size array
	array.resize( 0 );
	DoubleMatrix zero;
    double* pzero = zero.fromValarray( array, 0 ).data();
	assert( zero.data() == 0 );
	CPPUNIT_ASSERT_EQUAL( zero.nc(), 0 );
	CPPUNIT_ASSERT_EQUAL( zero.nr(), 0 );
	CPPUNIT_ASSERT_EQUAL( zero.getRefCount(), 1 );
}
void MatrixTest::testToValArraySelfDestruction()
{
    // Test finite array size
	int size = 6;
	DoubleMatrix dmat( 2, 3);
	double* pdmat = dmat.data();
	for( int j = 0; j < size; j++ )
		pdmat[ j ] = j;

	valarray<double> array;
	dmat.toValarray( array );

	for( int i = 0; i < size; i++ )
	    CPPUNIT_ASSERT_DOUBLES_EQUAL( array[ i ], i, 0 );
	CPPUNIT_ASSERT_EQUAL( static_cast<int>(array.size()), size );

	// Test zero size matrix
	DoubleMatrix zero;
        zero.toValarray( array );
	CPPUNIT_ASSERT_EQUAL( static_cast<int>(array.size()), 0 );

	// Test calling matrix becoming zero
	CPPUNIT_ASSERT_MESSAGE( "The original matrix should be destroyed", dmat.data() == 0 );
	CPPUNIT_ASSERT_EQUAL( dmat.nc(), 0 );
	CPPUNIT_ASSERT_EQUAL( dmat.nr(), 0 );
	CPPUNIT_ASSERT_EQUAL( dmat.getRefCount(), 1 );
}

void MatrixTest::testToValArraySelfMaintain()
{
    int i, j;

	for(i=0; i<_n; i++)
	{
        int m = _nRows[i];
        int n = _nCols[i];
		DoubleMatrix X(m,n);
        
        for( j=0; j<m*n; j++ )
          X.data()[j] = j;

        valarray<double> x = X.toValarray();
        CPPUNIT_ASSERT_DOUBLES_EQUAL( m*n, x.size(), 0.0 );
        CPPUNIT_ASSERT_DOUBLES_EQUAL( m, X.nr(), 0.0 );
        CPPUNIT_ASSERT_DOUBLES_EQUAL( n, X.nc(), 0.0 );

        for( j=0; j<m*n; j++ )
        {
          CPPUNIT_ASSERT_DOUBLES_EQUAL( X.data()[j], x[j], 0.0 );
        }
	}
}

