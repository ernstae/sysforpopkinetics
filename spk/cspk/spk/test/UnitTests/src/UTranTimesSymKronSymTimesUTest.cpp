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
 * File: UTranTimesSymKronSymTimesUTest.cpp
 *
 *
 * Test cases for expectedHessian
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
#include "../../../spk/UTranTimesSymKronSymTimesU.h"
#include "../../../spk/symmetrize.h"
#include "../../../spk/rvec.h"
#include "../../../spk/AkronBtimesC.h"
#include <cmath>
#include "../../../spk/transpose.h"
#include "../../../spk/multiply.h"
#include "../../../spk/replaceJth.h"
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"
#include "../../../spk/SpkException.h"
#include "../../../spk/FpErrorChecker.h"

#include "UTranTimesSymKronSymTimesUTest.h"

using namespace std;
using namespace CppUnit;

void UTranTimesSymKronSymTimesUTest::setUp()
{
    _m.push_back(0); _k.push_back(0);
    _m.push_back(0); _k.push_back(1);
    _m.push_back(1); _k.push_back(0);
    _m.push_back(1); _k.push_back(1);
    _m.push_back(1); _k.push_back(3);
    _m.push_back(3); _k.push_back(1);
    _m.push_back(3); _k.push_back(3);
    _n = _m.size();

}
void UTranTimesSymKronSymTimesUTest::tearDown()
{
    // clean up
}

Test* UTranTimesSymKronSymTimesUTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "UTranTimesSymKronSymTimesUTest" );

  suiteOfTests->addTest(new TestCaller<UTranTimesSymKronSymTimesUTest>(
                       "testUTranTimesSymKronSymTimesU", 
		       &UTranTimesSymKronSymTimesUTest::testUTranTimesSymKronSymTimesU));
  return suiteOfTests;
}

void UTranTimesSymKronSymTimesUTest::testUTranTimesSymKronSymTimesU()
{
    double tol;

    for( int y=0; y<_n; y++ )
    {
        const int m = _m[y];
        const int k = _k[y];
        if( m < 1 )
            continue;
        int j, i, x;
        DoubleMatrix U(m*m,k);
        DoubleMatrix Sym(m,m);
        DoubleMatrix Ut;
        std::vector<DoubleMatrix> Uarray(k);
        
        // Initialize U such that each column of U is rvec of a symmetric matrix.
        for( x=0; x<k; x++ )
        {
            DoubleMatrix Uj(m,m);
            for( j=0; j<m; j++ )
            {
                // fill lower half + diagonal, not just diagonal elements
                for( i=j; i<m; i++ )
                {
                   Uj.data()[i+j*m] = i+j+x;
                }        
            }

            symmetrize(Uj, Uj);
            // Double-check if Uj is symmetric.
            DoubleMatrix Ujt = transpose(Uj);
            for( i=0; i<m*m; i++ )
            {
                CPPUNIT_ASSERT_EQUAL(Uj.data()[i], Ujt.data()[i]);
            }
            Uarray[x] = Uj;
            replaceJth(U,x,rvec(Uj));
        }

        // Initilaize Sym such that Sym is symmetric.
        for( j=0; j<m; j++ )
        {
            // fill lower half + diagonal, not just diagonal elements
            for( i=j; i<m; i++ )
            {
                Sym.data()[i+j*m] = i+j+1;
            }        
        }
        symmetrize(Sym, Sym);

        // Double-check if Sym is created symmetric.
        DoubleMatrix SymT = transpose(Sym);
        for( i=0; i<m*m; i++ )
        {
            CPPUNIT_ASSERT_EQUAL(Sym.data()[i], SymT.data()[i]);
        }
        transpose(U, Ut);

        DoubleMatrix A[k];
/*
cout << "Sym=" << Sym;
cout << "U=" << U;
*/
        DoubleMatrix UtVVU;
        try{
            UTranTimesSymKronSymTimesU(Sym, U, k, UtVVU, A);
        }
        catch( const SpkException& e )
        {
            cerr << e << endl;
            CPPUNIT_ASSERT_MESSAGE( "UTranTimesSymKronSymTimesU threw an exception.", false );
        }
        CPPUNIT_ASSERT_EQUAL(k, UtVVU.nr());
        CPPUNIT_ASSERT_EQUAL(k, UtVVU.nc());

        DoubleMatrix UtABC = multiply(Ut, AkronBtimesC(Sym,Sym,U));
        CPPUNIT_ASSERT_EQUAL(k, UtABC.nr());
        CPPUNIT_ASSERT_EQUAL(k, UtABC.nc());
        double actual;
        double expected;

        for( i=0; i<k*k; i++ )
        {
            expected = UtABC.data()[i];
            actual   = UtVVU.data()[i];
            if( expected == 0 )
                tol = 0;
            else
                tol = fabs(expected-actual)/fabs(expected)*DBL_EPS_EQUAL_MULT;
            CPPUNIT_ASSERT_DOUBLES_EQUAL(expected, actual, tol);
        }
    }
}
