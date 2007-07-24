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
 * File: isDblEpsEqualTest.cpp
 *
 *
 * Unit test for the function isDblEpsEqual.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <cfloat>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/isDblEpsEqual.h"
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"
#include "isDblEpsEqualTest.h"

using namespace CppUnit;

void isDblEpsEqualTest::setUp()
{
    // initializations
}
void isDblEpsEqualTest::tearDown()
{
    // clean up
}

Test* isDblEpsEqualTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "isDblEpsEqualTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<isDblEpsEqualTest>("case1", &isDblEpsEqualTest::case1));

    return suiteOfTests;
}

void isDblEpsEqualTest::case1()
{
  using namespace std;

  double a, b, c, step, scale;

  a = -DBL_MAX;
  step = 2.0*(DBL_MAX/100.0);

  for (int i = 0; i < 99; i++) {
    a += step;
    b = a;
    scale = fabs(a);

    // Addition of this value should not change the epsilon  
    // equality of two double precision numbers.
    c = 0.5 * scale * ( DBL_EPS_EQUAL_MULT * DBL_EPSILON ); 

    CPPUNIT_ASSERT_MESSAGE( "isDblEpsEqual should return true",  isDblEpsEqual(a, b, scale) );
    CPPUNIT_ASSERT_MESSAGE( "isDblEpsEqual should return true",  isDblEpsEqual(a, b + c, scale) );
    CPPUNIT_ASSERT_MESSAGE( "isDblEpsEqual should return false", !isDblEpsEqual(a, b + 3.0*c, scale) );
  }

  return;
}
