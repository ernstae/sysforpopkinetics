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
 * File: rvecTest.cpp
 *
 *
 * Test cases for rvec()
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
#include "../../../spk/rvec.h"
#include "../../../spk/isDblEpsEqual.h"
#include "../../../spk/isDmatEpsEqual.h"
#include "../../../spk/transpose.h"
#include "rvecTest.h"

#include <cstdlib>
#include <ctime>
#include <cassert>
#include <float.h>

using namespace CppUnit;

static DoubleMatrix Rvec( const DoubleMatrix &A );
static DoubleMatrix another_rvec(const DoubleMatrix& A);
static DoubleMatrix cvec(const DoubleMatrix & A);

void rvecTest::setUp()
{
    // initializations
}
void rvecTest::tearDown()
{
    // clean up
}

Test* rvecTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("rvecTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<rvecTest>("testDoubleMatrixVersion", &rvecTest::testDoubleMatrixVersion));
    suiteOfTests->addTest(new TestCaller<rvecTest>("testValarrayVersion",     &rvecTest::testValarrayVersion));

    return suiteOfTests;
}

void rvecTest::testDoubleMatrixVersion()
{
	using namespace std;

    int i,
		iRows,
		iCols;
    double *pA, *pB, *pNaiveB, *pS;

    for( iRows=2, iCols=10; iRows<=10 && iCols>0; iRows++, iCols--)
	{
        DoubleMatrix dmatA(iRows,iCols);
		DoubleMatrix dmatB( iRows*iCols, 1 );
		DoubleMatrix naiveB(iRows*iCols, 1 );
		DoubleMatrix dmatS( iRows*iCols, 1 );
        pA = dmatA.data();

        for( i=0; i<iRows*iCols; i++ ){
            pA[i] = i+1;
        }
        dmatB  = rvec( dmatA );
        naiveB = another_rvec( dmatA );
		pB     = dmatB.data();
		pNaiveB= naiveB.data();
		pS     = dmatS.data();
		
		for( i=0; i<iRows*iCols; i++ )
        {
			pS[i] = ( fabs(pB[i]) >= fabs(pNaiveB[i]) ? fabs(pB[i]) : fabs(pNaiveB[i]));

            CPPUNIT_ASSERT_EQUAL(dmatB.data()[i], naiveB.data()[i]);
		}
    }
    return;
}

void rvecTest::testValarrayVersion()
{
	using namespace std;

    int i;
    int iRows;
    int iCols;

    for( iRows=2, iCols=10; iRows<=10 && iCols>0; iRows++, iCols--)
	{
        valarray<double> A( iRows * iCols );
		valarray<double> B( iRows*iCols );
		valarray<double> naiveB( iRows * iCols );
		valarray<double> S( iRows * iCols );

        for( i=0; i<iRows*iCols; i++ ){
            A[i] = i+1;
        }
        B  = rvec( A, iCols );
        naiveB = another_rvec( DoubleMatrix(A,iCols) ).toValarray();
		
		for( i=0; i<iRows*iCols; i++ )
        {
			S[i] = ( fabs(B[i]) >= fabs(naiveB[i]) ? fabs(B[i]) : fabs(naiveB[i]));

            CPPUNIT_ASSERT_EQUAL(B[i], naiveB[i]);
		}
    }
    return;
}

static DoubleMatrix another_rvec(const DoubleMatrix& A)
{
    using namespace std;
    //
    // A  = [ 1  2 ]
    //      [ 3  4 ]
    //      [ 5  6 ]
    //
    int nr = A.nr();
    int nc = A.nc();

    DoubleMatrix tA = transpose(A);
    DoubleMatrix colvec(nr*nc,1);
    std::copy(tA.data(), tA.data()+nr*nc, colvec.data());
    return colvec;
}

static DoubleMatrix cvec(const DoubleMatrix & A)
{
    //
    // A  = [ 1  2 ]
    //      [ 3  4 ]
    //      [ 5  6 ]
    //
    int nr = A.nr();
    int nc = A.nc();

    if( nc == 1 )
    {
        return A;
    }
    // divide into upper half, middle, lower half row blocks
    else
    {
        bool odd = (nc%2 == 0? false : true);
        int  nHalfCols = nc/2;
        DoubleMatrix Marge(nr*nc,1);
        double* marge = Marge.data();

        unsigned int begin = 0;
        unsigned int end   = begin+nHalfCols*nr;
        // upper half
        DoubleMatrix upper(nr, nHalfCols);
        // copy upper portion data to the subblock matrix
        std::copy(A.data()+begin, A.data()+end, upper.data());
        // further divide
        upper = cvec(upper);
        std::copy(upper.data(), upper.data()+nHalfCols*nr, marge+begin);
        
        begin = end;
        // middle
        if(odd)
        {
            end = begin+nr;
            DoubleMatrix middle(nr,1);
            std::copy(A.data()+begin, A.data()+end, middle.data());
            middle = cvec(middle);
            std::copy(middle.data(), middle.data()+nr, marge+begin);
            begin = end;
        }
        end += nHalfCols*nr;

        // further divide
        DoubleMatrix lower(nr, nHalfCols);
        std::copy(A.data()+begin, A.data()+end, lower.data());
        // lower
        lower = cvec(lower);
        std::copy(lower.data(), lower.data()+nHalfCols*nr, marge+begin);

        return Marge;
    }
}
