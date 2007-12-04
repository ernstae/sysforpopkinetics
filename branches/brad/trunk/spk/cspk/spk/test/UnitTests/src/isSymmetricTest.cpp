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
 * File: isSymmetricTest.cpp
 *
 *
 * Test cases for isSymmetric
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#pragma warning( disable : 4786 )

#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/isSymmetric.h"
#include "isSymmetricTest.h"
#include "../../../spk/identity.h"

using namespace std;
using namespace CppUnit;

void isSymmetricTest::setUp()
{
    // initializations
}
void isSymmetricTest::tearDown()
{
    // clean up
}

Test* isSymmetricTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("isSymmetricTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<isSymmetricTest>("testTrue", &isSymmetricTest::testTrue));
    suiteOfTests->addTest(new TestCaller<isSymmetricTest>("testFalse", &isSymmetricTest::testFalse));
    suiteOfTests->addTest(new TestCaller<isSymmetricTest>("testIllegal", &isSymmetricTest::testIllegal));
    return suiteOfTests;
}

void isSymmetricTest::testTrue()
{
  const int n = 3;

  //
  // Test a matrix filled with all zeros.  This should be true.
  //
  //  A = [ 0.0  0.0  0.0 ]
  //      [ 0.0  0.0  0.0 ]
  //      [ 0.0  0.0  0.0 ]
  //
  DoubleMatrix A( n, n );
  A.fill(0.0);
  CPPUNIT_ASSERT_MESSAGE( "isSymmetric(A)", isSymmetric(A) );

  //
  // Test a no-doubt symmetric matrix.  This should be true.
  //
  //  B = [ 1.0  0.0  0.0 ]
  //      [ 0.0  1.0  0.0 ]
  //      [ 0.0  0.0  1.0 ]
  //
  DoubleMatrix B = identity(n);
  CPPUNIT_ASSERT_MESSAGE("isSymmetric(B)",  isSymmetric(B) );

  //
  // Test a supposedly symmetric matrix.  This should be true.
  //
  //  C = [ 1.0  2.0  0.6 ]
  //      [ 2.0  1.0  3.0 ]
  //      [ 0.6  3.0  1.0 ]
  //
  DoubleMatrix C( n, n );
  double * c = C.data();
  c[0] = 1.0;
  c[1] = 2.0;
  c[2] = 0.6;
  c[3] = 2.0;
  c[4] = 1.0;
  c[5] = 3.0;
  c[6] = 0.6;
  c[7] = 3.0;
  c[8] = 1.0;
  
  CPPUNIT_ASSERT_MESSAGE("isSymmetric(C)",  isSymmetric(C) );

}
void isSymmetricTest::testFalse()
{
  const int n = 3;
  //
  // Test a supposedly non-symmetric matrix.  This should be false.
  //
  //  C = [ 1.0  DBL_EPSILON  DBL_EPSILON ]
  //      [ 0.0  1.0          DBL_EPSILON ]
  //      [ 0.0  0.0          1.0         ]
  //
  // Definition of DBL_EPSILON:
  //     DBL_EPSILON is the smallest positive number, x, 
  //     such that x + 1.0 is not equal to 1.0.
  //
  DoubleMatrix C( n, n );
  double * c = C.data();
  c[0] = 1.0;
  c[1] = 0.0;
  c[2] = 0.0;
  c[3] = DBL_EPSILON;
  c[4] = 1.0;
  c[5] = 0.0;
  c[6] = DBL_EPSILON;
  c[7] = DBL_EPSILON;
  c[8] = 1.0;
  
  CPPUNIT_ASSERT_MESSAGE( "!isSymmetric(C)", !isSymmetric(C) );

}
void isSymmetricTest::testIllegal()
{
  
}
