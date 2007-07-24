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
 * File: getMulRowsTest.cpp
 *
 *
 * Unit test for getMulRows.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include "../../../spk/getMulRows.h"
#include "../../../spk/isDmatEpsEqual.h"
#include "../../../spk/DoubleMatrix.h"
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "getMulRowsTest.h"

using namespace CppUnit;

void getMulRowsTest::setUp()
{
    // initializations
}
void getMulRowsTest::tearDown()
{
    // clean up
}

Test* getMulRowsTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "getMulRowsTest" );

  // Test cases for this unit test.
  suiteOfTests->addTest( new TestCaller<getMulRowsTest>( "specCase", &getMulRowsTest::specCase ) );
  suiteOfTests->addTest( new TestCaller<getMulRowsTest>( "emptyCase", &getMulRowsTest::emptyCase ) );

  return suiteOfTests;
}

static DoubleMatrix exampleInSpec();

void getMulRowsTest::specCase()
{
    using namespace std;
    
    int i,j;

    DoubleMatrix dmatC = exampleInSpec();
    DoubleMatrix dmatAsubS(2,3);
    double *pdAsubS = dmatAsubS.data();
    for( i=0; i<2; i++ ){
        for( j=0; j<3; j++ )
            pdAsubS[i+j*2] = (i==0? 0:2)+(j*3);
    }

    for( i=0; i<dmatC.nr()*dmatC.nc(); i++ )
        CPPUNIT_ASSERT_EQUAL(dmatAsubS.data()[i], dmatC.data()[i]);
}
void getMulRowsTest::emptyCase()
{
    DoubleMatrix A(3,3);
    DoubleMatrix S(3,1);
    S.fill(false);
    CPPUNIT_ASSERT( getMulRows(A,S).isEmpty() );
}
static DoubleMatrix exampleInSpec(){
        using namespace std;
        int m = 3;
        int n = 3; 
        int i,j;
        DoubleMatrix A(m,n);
        DoubleMatrix S(m,1);
        DoubleMatrix C;
        double *pdA = A.data();
        double *pdS = S.data();

        // Set A to a matrix:
        //  [ 0  3  6 ]
        //  [ 1  4  7 ]
        //  [ 2  5  8 ]
        //
        // Set S to a vector:
        //  [ true  ] = [ 1 ]
        //  [ false ]   [ 0 ]
        //  [ true  ]   [ 1 ]
        //
        for( i=0; i<m; i++ ){
            for( j=0; j<n; j++ ){
                pdA[i+j*m] = i+j*m;
            }
            pdS[i] = (double)(!(i % 2));
        }

        C = getMulRows(A,S);

        /*

        // Result =
        //  [0, 3, 6]
        //  [2, 5, 8]
        */

        return C;
}

