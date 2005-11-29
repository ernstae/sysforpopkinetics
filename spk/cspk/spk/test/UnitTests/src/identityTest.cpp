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
 * File: identityTest.cpp
 *
 *
 * Unit test for identity()
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/identity.h"
#include "identityTest.h"
#include "../../../spk/SpkValarray.h"

using SPK_VA::valarray;
using SPK_VA::slice;
using namespace CppUnit;

void identityTest::setUp()
{
    // initializations
}
void identityTest::tearDown()
{
    // clean up
}

Test* identityTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("identityTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<identityTest>("testDoubleMatrix", &identityTest::testDoubleMatrix));
    suiteOfTests->addTest(new TestCaller<identityTest>("testValarray",     &identityTest::testValarray));
    return suiteOfTests;
}

void identityTest::testDoubleMatrix()
{
    //
    // Currently identity() terminates the program if the specified the value
    // (dimension) is zero or smaller.  
    // So for now, test with only legal cases.
    //
    for( int i=1; i<=3; i++ )
    {
        DoubleMatrix actual = identity( i );
        CPPUNIT_ASSERT_EQUAL(i, actual.nr());
        CPPUNIT_ASSERT_EQUAL(i, actual.nc());
        for( int col=0; col<i; col++ )
        {
            for(int row=0; row<i; row++)
            {
                if( row == col )
                    CPPUNIT_ASSERT_EQUAL(1.0, actual.data()[col*actual.nr()+row] );
                else
                    CPPUNIT_ASSERT_EQUAL(0.0, actual.data()[col*actual.nr()+row] );
            }
        }
    }

}
void identityTest::testValarray()
{
  const int n = 5;
  valarray<double> I(n * n); 
  identity( n, I );
  for( int j=0; j<n; j++ )
  {
    for( int i=0; i<n; i++ )
    {
      if( i == j )
        CPPUNIT_ASSERT_EQUAL( 1.0, I[ i + j * n ] );
      else
        CPPUNIT_ASSERT_EQUAL( 0.0, I[ i + j * n ] );
    }
  }
}
