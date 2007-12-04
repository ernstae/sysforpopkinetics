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
 * File: IndInputDataPackageTest.cpp
 *
 *
 * Unit test for IndInputDataPackage.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <sstream>
#include "../../../spk/IndDataPackage.h"
#include "../../../spk/PopConstVals.h"
#include "../../../spk/PopVars.h"
#include "../../../spk/IndVars.h"
#include "../../../spk/namespace_population_analysis.h"
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "IndInputDataPackageTest.h"

using namespace CppUnit;

void IndInputDataPackageTest::setUp()
{
    // initializations
}
void IndInputDataPackageTest::tearDown()
{
    // clean up
}

Test* IndInputDataPackageTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "IndInputDataPackageTest" );

  // Test cases for this unit test.
  suiteOfTests->addTest( new TestCaller<IndInputDataPackageTest>( "laplaceCase", &IndInputDataPackageTest::laplaceCase ) );
  suiteOfTests->addTest( new TestCaller<IndInputDataPackageTest>( "hessianCase", &IndInputDataPackageTest::hessianCase ) );

  return suiteOfTests;
}

using namespace std;
using namespace population_analysis;

static void runTest(int objective);

void IndInputDataPackageTest::laplaceCase()
{
    runTest(LAPLACE);
}
void IndInputDataPackageTest::hessianCase()
{
    runTest(HESSIAN);
}
static void runTest(int objective)
{
    int inx  = 0;
    int size = 10;
    int mitr = 0;
    int curItr = 3; // arbitrary number for test

    double epsilon = 0.0;
    int level = 1;
    int nB   = 3;
    DoubleMatrix low(nB,1);
    DoubleMatrix up(nB,1);
    DoubleMatrix step(nB,1);
    int sizeofData = nB;
    int inxToData  = 0;

    low.fill(1);
    up.fill(2);
    step.fill(0.5);
	Optimizer optimizer( epsilon, mitr, level );

    PopConstVals popconst(size, optimizer, objective, low, up, step);

    DoubleMatrix indForAll(3,2);
    DoubleMatrix pop1(0,0);
    pop1.fill(1);
    PopVars popvars(pop1, true, true, true);

    int n = 3;
    DoubleMatrix bIn(n,1);
    DoubleMatrix y(n,1);
    DoubleMatrix dummy(n,1);
    bIn.fill(1);
    dummy.fill(0);

    IndVars indvars(inx, bIn, y);

    IndInputDataPackage pack1(curItr, popconst, popvars, indvars);
    IndInputDataPackage pack2;
    IndInputDataPackage pack3;
    IndInputDataPackage pack4;
    IndInputDataPackage pack5;

    stringstream stream;
    stream << pack1 << endl;

    try{
		stream >> pack2;
	}
	catch(...)
	{
		cerr << "Control should not reach here." << endl;
        CPPUNIT_ASSERT(false);
	}

    CPPUNIT_ASSERT( !pack1.empty() );
    CPPUNIT_ASSERT( !(pack1 != pack2) );
    CPPUNIT_ASSERT( pack4.empty() );


    // pack4 is empty
    stream.flush();
    stream << pack4 << endl;

    // so, this should throw an exception
    try{
        stream >> pack5;
    }
    catch(...)
    {
        // Good, it threw an exception!
        // BUT, the kind of exception thrown by the operation should be determined concrete.
        CPPUNIT_ASSERT(true);
    }

    CPPUNIT_ASSERT( pack5.empty() );
}
