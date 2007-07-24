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
 * File: getMulColsTest.cpp
 *
 *
 * Unit test for getMulCols.
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
#include "../../../spk/getMulCols.h"
#include "../../../spk/isDmatEpsEqual.h"
#include "getMulColsTest.h"

using namespace CppUnit;

void getMulColsTest::setUp()
{
    // initializations
}
void getMulColsTest::tearDown()
{
    // clean up
}

Test* getMulColsTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "getMulColsTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<getMulColsTest>("specCase",  &getMulColsTest::specCase));
    suiteOfTests->addTest(new TestCaller<getMulColsTest>("emptyCase", &getMulColsTest::emptyCase));

    return suiteOfTests;
}

static DoubleMatrix exampleInSpec();

void getMulColsTest::specCase()
{
    using namespace std;
    
    int i,j;

    
    DoubleMatrix expected(3,2);
    double *pdExpected = expected.data();
    for( j=0; j<2; j++ )
    {
        for( i=0; i<3; i++ )
        {
            pdExpected[i+j*3] = (j==0? 0:6)+i;
        }
    }

    DoubleMatrix actual = exampleInSpec();
    
    for( i=0; i<expected.nr()*expected.nc(); i++)
    {
        CPPUNIT_ASSERT_EQUAL(pdExpected[i], actual.data()[i] );
    }
}
void getMulColsTest::emptyCase()
{
    DoubleMatrix A(3,3);
    DoubleMatrix S(1,3);
    S.fill(false);
    DoubleMatrix actual = getMulCols(A,S);
    CPPUNIT_ASSERT_MESSAGE( "actual.isEmpty()", actual.isEmpty() );
}
static DoubleMatrix exampleInSpec()
{
    using namespace std;

    int m = 3;
    int n = 3; 
    int i,j;
    DoubleMatrix A(m,n);
    DoubleMatrix S(1,n);
    DoubleMatrix C;
    double *pdA = A.data();
    double *pdS = S.data();

    // Set A to a matrix:
    //  [ 0  3  6 ]
    //  [ 1  4  7 ]
    //  [ 2  5  8 ]
    //
    // Set S to a vector:
    //  [ true, false, true] = [ 1, 0, 1 ]
    //
    for( j=0; j<n; j++ ){
        for( i=0; i<m; i++ ){
            pdA[i+j*m] = i+j*m;
        }
        pdS[j] = (double)(!(j % 2));
    }

    C = getMulCols(A,S);

    /*
    cout << "A = " << endl;
    A.print();

    cout << "\nS = " << endl;
    S.print();

    // Result =
    //  [0, 6]
    //  [1, 7]
    //  [2, 8]
    cout << "\nResult = " << endl;
    C.print();
    */

    return C;
}

