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
 * File: countTruesTest.cpp
 *
 *
 * Test cases for countTrues
 *
 * Author: sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <algorithm>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/countTrues.h"
#include "countTruesTest.h"

#include "../../../spk/identity.h"

using namespace CppUnit;

void countTruesTest::setUp()
{
    // initializations
}
void countTruesTest::tearDown()
{
    // clean up
}

Test* countTruesTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "countTruesTest" );

  // duplicate the following example for each test case, replacing the case name.
  suiteOfTests->addTest(new TestCaller<countTruesTest>("squareCase", &countTruesTest::squareCase));
  suiteOfTests->addTest(new TestCaller<countTruesTest>("nonsquareCase", &countTruesTest::nonsquareCase));
  suiteOfTests->addTest(new TestCaller<countTruesTest>("emptyCase", &countTruesTest::emptyCase));

  return suiteOfTests;
}

void countTruesTest::squareCase()
{
    int m = 3;
  
    DoubleMatrix allFalse(m,m);
    allFalse.fill( (double)false );

    CPPUNIT_ASSERT_EQUAL( countTrues(allFalse), 0 );

    DoubleMatrix allTrue(m,m);
    allTrue.fill( (double)true );

    CPPUNIT_ASSERT_EQUAL( countTrues(allTrue), m*m );

    DoubleMatrix mix = identity(m);

    CPPUNIT_ASSERT_EQUAL( countTrues(mix), m );
    
}
void countTruesTest::nonsquareCase()
{
    int m = 3;
    int n = 2;
  
    DoubleMatrix allFalse(m,n);
    allFalse.fill( (double)false );

    CPPUNIT_ASSERT_EQUAL( countTrues(allFalse), 0 );

    DoubleMatrix allTrue(m,n);
    allTrue.fill( (double)true );

    CPPUNIT_ASSERT_EQUAL( countTrues(allTrue), m*n );

    DoubleMatrix mix(m,n);
    double* pMix = mix.data();
    mix.fill( (double)false );
    int j, cnt;
    for( j=0, cnt=0; j<n; j++ )
    {
        for( int i=0; i<m; i++ )
        {
            if( i == j )
            {
                pMix[i+j*m] = (double)true;
                ++cnt;
            }
        }
    }

    CPPUNIT_ASSERT_EQUAL( countTrues( mix ), cnt );
}
void countTruesTest::emptyCase()
{

    int zero    = 0;
    int nonzero = 3;

    DoubleMatrix m0x0( zero, zero );
    m0x0.fill( (double)true );

    CPPUNIT_ASSERT_EQUAL( countTrues(m0x0), 0 );

    DoubleMatrix m0x3( zero, nonzero );
    m0x3.fill( (double)true );

    CPPUNIT_ASSERT_EQUAL( countTrues(m0x3), 0 );

    DoubleMatrix m3x0( nonzero, zero );
    m3x0.fill( (double)true );

    CPPUNIT_ASSERT_EQUAL( countTrues(m3x0), 0 );
}
