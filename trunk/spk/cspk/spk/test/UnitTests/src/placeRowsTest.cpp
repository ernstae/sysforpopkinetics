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
 * File: placeRowsTest.cpp
 *
 *
 * Test cases for placeRowsTest
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
#include "../../../spk/placeRows.h"
#include "placeRowsTest.h"

using namespace CppUnit;

void placeRowsTest::setUp()
{
    // initializations
}
void placeRowsTest::tearDown()
{
    // clean up
}

Test* placeRowsTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("placeRowsTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<placeRowsTest>("noneCase",   &placeRowsTest::noneCase));
    suiteOfTests->addTest(new TestCaller<placeRowsTest>("allCase",    &placeRowsTest::allCase));
    suiteOfTests->addTest(new TestCaller<placeRowsTest>("mixedCase",  &placeRowsTest::mixedCase));
    suiteOfTests->addTest(new TestCaller<placeRowsTest>("emptryCase", &placeRowsTest::emptryCase));
    return suiteOfTests;
}

void placeRowsTest::noneCase()
{
    int m = 0;
    int n = 3;
    int k = 3;

    DoubleMatrix A(m,n);
    A.fill(1.0);

    DoubleMatrix B(m+k, n);
    B.fill(0.0);

    //
    // Set mask so that none of the elements in B gets altered.
    // 
    DoubleMatrix mask(m+k, 1);
    mask.fill(0);

    placeRows(A,B,mask);

    double* pB = B.data();
    for( int j=0; j<n; j++ )
    {
        for( int i=0; i<m+k; i++ )
        {
            CPPUNIT_ASSERT_EQUAL( pB[i+j*(m+k)], 0.0 );
        }
    }
}
void placeRowsTest::allCase()
{
    int m = 3;
    int n = 3;
    int k = 0;

    DoubleMatrix A(m,n);
    A.fill(1.0);

    DoubleMatrix B(m+k, n);
    B.fill(0.0);

    //
    // Set mask so that all elements in B gets altered.
    // 
    DoubleMatrix mask(m+k, 1);
    mask.fill(1);

    placeRows(A,B,mask);

    double* pB = B.data();
    for( int j=0; j<n; j++ )
    {
        for( int i=0; i<m+k; i++ )
        {
            CPPUNIT_ASSERT_EQUAL( pB[i+j*(m+k)], 1.0 );
        }
    }
   
}
void placeRowsTest::mixedCase()
{
    int m = 2;
    int n = 3;
    int k = 1;

    DoubleMatrix A(m,n);
    A.fill(1.0);

    DoubleMatrix B(m+k, n);
    B.fill(0.0);

    //
    // Set mask so that the first and third rows of B get altered.
    //
    DoubleMatrix mask(m+k, 1);
    mask.fill(0);
    double* pMask = mask.data();
    pMask[0] = 1;
    pMask[1] = 0;
    pMask[2] = 1;

    placeRows(A,B,mask);

    double* pB = B.data();
    for( int j=0; j<n; j++ )
    {
        for( int i=0; i<m+k; i++ )
        {
            if( i == 0 || i == 2 )
                CPPUNIT_ASSERT_EQUAL( pB[i+j*(m+k)], 1.0 );
            else
                CPPUNIT_ASSERT_EQUAL( pB[i+j*(m+k)], 0.0 );
        }
    }
}
void placeRowsTest::emptryCase()
{
    int m = 2;
    int n = 0;
    int k = 1;

    DoubleMatrix A(m,n);
    A.fill(1.0);

    DoubleMatrix B(m+k, n);
    B.fill(0.0);

    DoubleMatrix mask(m+k, 1);
    mask.fill(0);

    //
    // This operation should be successful regardless of emptiness.
    //
    placeRows(A,B,mask);

    double* pB = B.data();
    for( int j=0; j<n; j++ )
    {
        for( int i=0; i<m+k; i++ )
        {
            CPPUNIT_ASSERT_EQUAL( pB[i+j*(m+k)], 0.0 );
        }
    }
}
