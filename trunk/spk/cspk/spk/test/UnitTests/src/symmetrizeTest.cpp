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
 * File: symmetrizeTest.cpp
 *
 *
 * Test cases for symmetrize
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
#include "../../../spk/symmetrize.h"
#include "../../../spk/transpose.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/printInMatrix.h"

#include "symmetrizeTest.h"

using SPK_VA::valarray;

using namespace std;
using namespace CppUnit;

void symmetrizeTest::setUp()
{
    _rows.push_back(0); _cols.push_back(0);
    _rows.push_back(0); _cols.push_back(1);
    _rows.push_back(1); _cols.push_back(0);
    _rows.push_back(1); _cols.push_back(1);
    _rows.push_back(1); _cols.push_back(3);
    _rows.push_back(3); _cols.push_back(1);
    _rows.push_back(3); _cols.push_back(3);
    _n = _rows.size();

}
void symmetrizeTest::tearDown()
{
    // clean up
}

Test* symmetrizeTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("symmetrizeTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<symmetrizeTest>("testSymmetrize",  &symmetrizeTest::testSymmetrize));
    suiteOfTests->addTest(new TestCaller<symmetrizeTest>("testValarrayVer", &symmetrizeTest::testValarrayVer));

    return suiteOfTests;
}

void symmetrizeTest::testSymmetrize()
{   
    int i,j, x;
    for( x=0; x<_n; x++ )
    {
        int m = _rows[x];
        DoubleMatrix Sym(m,m);
        double* pS = Sym.data();

        // Initilaize Sym such that Sym is symmetric.
        for( j=0; j<m; j++ )
        {
            // fill lower half + diagonal
            for( i=j; i<m; i++ )
            {
                pS[i+j*m] = i+j+1;
            }        
        }
        // Test self-modification
        symmetrize(Sym, Sym);

        DoubleMatrix SymTrans = transpose(Sym);
        double* pSt = SymTrans.data();
        for( i=0; i<m*m; i++ )
        {
            CPPUNIT_ASSERT_EQUAL(pS[i],pSt[i]);
        }
        DoubleMatrix Res(m,m);
        double* pR = Res.data();

        // Test NON self-modification
        symmetrize(Sym,Res);

        DoubleMatrix ResTrans = transpose(Res);
        double* pRt = ResTrans.data();
        for( i=0; i<m*m; i++ )
        {
            CPPUNIT_ASSERT_EQUAL(pR[i],pRt[i]);
        }
    }
}
void symmetrizeTest::testValarrayVer()
{
    using SPK_VA::valarray;
    using namespace std;

    int n = 3;
    valarray<double> A(0.0, n * n);
    valarray<double> B(n * n);

    //
    //     /             \
    // A = |  1   0   0  |
    //     |  2   3   0  |
    //     |  3   4   5  |
    //     \             /
    //
    for( int j=0; j<n; j++ )
    {
        // fill lower half + diagonal with arbitrary values
        for( int i=j; i<n; i++ )
        {
            A[i+j*n] = i+j+1;
        }        
    }

    // Keep the original
    symmetrize(A, n, B);
    CPPUNIT_ASSERT_EQUAL( 1.0, B[0] );
    CPPUNIT_ASSERT_EQUAL( 2.0, B[1] );
    CPPUNIT_ASSERT_EQUAL( 3.0, B[2] );
    CPPUNIT_ASSERT_EQUAL( 2.0, B[3] );
    CPPUNIT_ASSERT_EQUAL( 3.0, B[4] );
    CPPUNIT_ASSERT_EQUAL( 4.0, B[5] );
    CPPUNIT_ASSERT_EQUAL( 3.0, B[6] );
    CPPUNIT_ASSERT_EQUAL( 4.0, B[7] );
    CPPUNIT_ASSERT_EQUAL( 5.0, B[8] );


/*
    cout << "A: " << endl;
    printInMatrix( A, n );
    cout << endl;

    cout << "B: " << endl;
    printInMatrix( B, n );
    cout << endl;

    // Self modify
    symmetrize(A, n, A);
    cout << "A: " << endl;
    printInMatrix( A, n );
    cout << endl;
*/
}
