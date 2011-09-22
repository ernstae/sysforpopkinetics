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
 * File: IndOutputDataPackageTest.cpp
 *
 *
 * Unit test for IndOutputDataPackage structure.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#include <sstream>
#include "../../../spk/IndDataPackage.h"
#include "../../../spk/IndResults.h"
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/Optimizer.h"
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "IndOutputDataPackageTest.h"

using namespace CppUnit;

void IndOutputDataPackageTest::setUp()
{
    // initializations
}
void IndOutputDataPackageTest::tearDown()
{
    // clean up
}

Test* IndOutputDataPackageTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "IndOutputDataPackageTest" );

  // Test cases for this unit test.
  suiteOfTests->addTest( new TestCaller<IndOutputDataPackageTest>( "equalCase", &IndOutputDataPackageTest::equalCase ) );

  return suiteOfTests;
}

using namespace std;

void IndOutputDataPackageTest::equalCase()
{
    int index = 3;
    int curItr = 4;
    int nB = 3;
    int nA = 2;

    DoubleMatrix bOut(nB,1), bTilde(nB,1), bTilde_alp(nB,nA);
    double dLambda=1, dLogdetLambda2diff=1;
	Optimizer optimizer;
    IndResults indResults(index, bOut, bTilde, bTilde_alp, dLambda, dLogdetLambda2diff);
    IndOutputDataPackage pack1(curItr, indResults);
    IndOutputDataPackage pack2;

    stringstream stream;
    stream << pack1 << endl;

    stream >> pack2;

    CPPUNIT_ASSERT( !( pack1 != pack2 ) );
}

