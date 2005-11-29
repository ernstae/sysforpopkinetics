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
 * File: PopConstValsTest.cpp
 *
 *
 * Unit test for PopConstVals.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#include <sstream>
#include "../../../spk/PopConstVals.h"
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/namespace_population_analysis.h"
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "PopConstValsTest.h"

using namespace CppUnit;

void PopConstValsTest::setUp()
{
    // initializations
}
void PopConstValsTest::tearDown()
{
    // clean up
}

Test* PopConstValsTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "PopConstValsTest" );

  // Test cases for this unit test.
  suiteOfTests->addTest( new TestCaller<PopConstValsTest>( "laplaceCase", &PopConstValsTest::laplaceCase ) );
  suiteOfTests->addTest( new TestCaller<PopConstValsTest>( "hessianCase", &PopConstValsTest::hessianCase ) );

  return suiteOfTests;
}

using namespace std;
using namespace population_analysis;

static void runTest(int objective);
void PopConstValsTest::laplaceCase()
{
    runTest(LAPLACE);
}
void PopConstValsTest::hessianCase()
{
    runTest(HESSIAN);
}
static void runTest(int objective)
{
    int size = 10;
    int mitr = 0;
    double epsilon = 0.0;
    int level = 1;
	Optimizer optimizer( epsilon, mitr, level );
    int nB   = 3;
    DoubleMatrix low(nB,1);
    DoubleMatrix up(nB,1);
    DoubleMatrix step(nB,1);
    bool isD = true;

    low.fill(1);
    up.fill(2);
    step.fill(0.5);

    PopConstVals ds1(size, optimizer, objective, low, up, step);
    PopConstVals ds2(size, optimizer, objective, up, low, step);

    stringstream stream1;
    stream1 << ds1;

    stream1 >> ds2;

    CPPUNIT_ASSERT( !(ds1 != ds2) );
    return;
}
