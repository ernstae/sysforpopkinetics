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
 * File: backDivTest.cpp
 *
 *
 * Test cases for backDiv()
 *
 * Author: sachiko honda
 *
 *************************************************************************/
#include <iostream>
#include <iomanip>
#include <ctime>
#include <cmath>

#include <spk/backDiv.h>
#include <spk/subtract.h>
#include <spk/multiply.h>
#include <spk/DoubleMatrix.h>

#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>

#include "backDivTest.h"

using namespace std;
using namespace CppUnit;

void dump( 
          const int cnt, 
          const DoubleMatrix &dmatA,
          const DoubleMatrix &dmatB,
          const DoubleMatrix &dmatC
          );
static void exampleInSpec();

/*************************************************************************
 *
 * Local function: 
 *
 *
 * ...
 *
 *************************************************************************/
void dump( 
          const int cnt, 
          const DoubleMatrix &dmatA,
          const DoubleMatrix &dmatB,
          const DoubleMatrix &dmatx
          )
{
    using namespace std;

    DoubleMatrix dmatAx;

    cout << "A = " << endl;
    dmatA.print();
    cout << endl;

    cout << "x = " << endl;
    dmatx.print();
    cout << endl;

    cout << "B = " << endl;
    dmatB.print();
    cout << endl;

    dmatAx = multiply(dmatA, dmatx);
    cout << "Ax (=B) ? " << endl;
    (dmatAx).print();
    cout << endl;

    cout << "Ax - B? " << endl;
    (subtract((dmatAx), dmatB)).print();
    return;
}

void backDivTest::setUp()
{
    // Matrix must be square
    _rows.push_back(0);  _cols.push_back(0);
    _rows.push_back(1);  _cols.push_back(1);
    _rows.push_back(3);  _cols.push_back(3);
}
void backDivTest::tearDown()
{
    // clean up
}

Test* backDivTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "backDivTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<backDivTest>("test",          &backDivTest::test));
    suiteOfTests->addTest(new TestCaller<backDivTest>("exampleInSpec", &backDivTest::exampleInSpec));

    return suiteOfTests;
}

void backDivTest::test()
{
    using namespace std;

    const int MAX = _rows.size();
    int       i, j, m, n;

    srand(time(NULL));
    for( int cnt=0; cnt<MAX; cnt++ )
    {

        m = _rows[cnt];
        n = _cols[cnt];

        DoubleMatrix dmatA( n, n );
        DoubleMatrix dmatB( n, m );
        DoubleMatrix dmatx( n, m );
        DoubleMatrix dmatS( n, m );
        DoubleMatrix dmatAx(n, m );
  
        double *pdA = dmatA.data();
        double *pdB = dmatB.data();
        double *pdS;
        double *pdAx;

        for( j=0; j<n; j++ ){
            for( i=0; i<n; i++ ){
                if( i==j )
                    pdA[i+j*n] = rand() / 10000.0 + 2.0 ;
                else
                    pdA[i+j*n] = rand() / 10000.0 * (rand()%2 == 0? 1.0 : -1.0);
            }
        }

        for( i=0; i<m*n; i++ ){
            pdB[i] = rand() / 10000.0;
        }

        dmatx = backDiv(dmatA, dmatB);

        dmatAx = multiply(dmatA, dmatx);
        pdS    = dmatS.data();
        pdAx   = dmatAx.data();
        pdB    = dmatB.data();

        for( i=0; i<dmatS.nr()*dmatS.nc(); i++ ){
            pdS[i] = 1.0e4* (fabs(pdAx[i]) >= fabs(pdB[i]) ? fabs(pdAx[i]): fabs(pdB[i]) );
            if( pdS[i] == 0.0 )
                pdS[i] = DBL_EPSILON;
            CPPUNIT_ASSERT_DOUBLES_EQUAL(pdAx[i], pdB[i], pdS[i]);
        }
    }
    return;
}
void backDivTest::exampleInSpec()
{

    using namespace std;

    const int m = 1;
    const int n = 3;
    
    DoubleMatrix A(n,n);    // A must be positive definite
    DoubleMatrix B(n,m);    // B could be anything
    DoubleMatrix x(n,m);    // unknown parameter we want to solve

    double *pdA = A.data();
    double *pdB = B.data();

    int i;

    // Set A to a matrix:
    //  [ 1  4  2 ]
    //  [ 2  5  3 ]
    //  [ 3  1  4 ]
    for( i=0; i<n*n; i++ )
        pdA[i] = i % 5 + 1;

    // Set B to a vector:
    //  [ 1 ]
    //  [ 1 ]
    //  [ 1 ]
    for( i=0; i<m*n; i++ )
        pdB[i] = 1;

    x = backDiv(A,B);

    /*
    cout << "A \\ B = x = " << endl;
    x.print();
    cout << endl;
    cout << "A x (should be equal to B) = " << endl;
    (A*x).print();
    */
}
