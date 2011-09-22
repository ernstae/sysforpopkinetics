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
 * File: allZeroTest.cpp
 *
 *
 * Test cases for allZero
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#pragma warning ( disable : 4786 )

#include <iostream>
#include <fstream>
#include <string>
#include "Test.h"
#include "TestCase.h"
#include "TestSuite.h"
#include "TestCaller.h"
#include "DoubleMatrix.h"
#include "allZero.h"
#include "allZeroTest.h"

void allZeroTest::setUp()
{
    // initializations
}
void allZeroTest::tearDown()
{
    // clean up
}

Test* allZeroTest::suite()
{
	TestSuite *suiteOfTests = new TestSuite;

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<allZeroTest>("allZeroCase", allZeroCase));
    suiteOfTests->addTest(new TestCaller<allZeroTest>("someZeroCase", someZeroCase));
    suiteOfTests->addTest(new TestCaller<allZeroTest>("allNonZeroCase", allNonZeroCase));

    return suiteOfTests;
}

void allZeroTest::allZeroCase()
{
  DoubleMatrix A(0,1);
  assertImplementation(allZero(A), "allZero(A)", __LINE__, __FILE__);

  DoubleMatrix D(2,2);
  D.fill(0.0);
  assertImplementation(allZero(D), "allZero(D)", __LINE__, __FILE__);
}

void allZeroTest::someZeroCase()
{
  DoubleMatrix A(1,2);
  double* a = A.data();

  a[0] = 1.0;
  a[1] = 0.0;
  assertImplementation(!allZero(A), "!allZero(A)", __LINE__, __FILE__);
}
void allZeroTest::allNonZeroCase()
{
  DoubleMatrix B(2,2);
  B.fill(0.0+DBL_EPSILON);
  assertImplementation(!allZero(B), "!allZero(B)", __LINE__, __FILE__);

  DoubleMatrix C(2,2);
  C.fill(0.0-DBL_EPSILON);
  assertImplementation(!allZero(C), "!allZero(C)", __LINE__, __FILE__);

}
