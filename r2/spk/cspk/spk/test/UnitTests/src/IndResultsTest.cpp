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
 * File: IndResults.cpp
 *
 *
 * Unit test for IndResults class.
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
#include "../../../spk/IndResults.h"
#include "../../../spk/DoubleMatrix.h"

#include "IndResultsTest.h"

using namespace std;
using namespace CppUnit;

void IndResultsTest::setUp()
{
    // initializations
}
void IndResultsTest::tearDown()
{
    // clean up
}

Test* IndResultsTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("IndResultsTest");

  // duplicate the following example for each test case, replacing the case name.
  suiteOfTests->addTest(new TestCaller<IndResultsTest>("allTests", &IndResultsTest::allTests));

  return suiteOfTests;
}

void IndResultsTest::allTests()
{
    int nAlp = 2;
    int n = 3;
    DoubleMatrix bHat(n,1);
    DoubleMatrix bTilde(n,1);
    DoubleMatrix bTilde_alp(n, nAlp);
    DoubleMatrix dummy(n,1);
    double dLambda=1, dLogdetLambda2diff=1;
    double ddum=-1;
    dummy.fill(-1);

    bHat.fill(1);
    bTilde.fill(1);
    bTilde_alp.fill(1);

    IndResults ob1(0, bHat, bTilde, bTilde_alp, dLambda, dLogdetLambda2diff);
    ofstream out("junk");
    CPPUNIT_ASSERT( out.good() );

    out << ob1 << endl;
    out.close();

    ifstream in("junk");
    CPPUNIT_ASSERT( in.good() );

    IndResults ob2(1, dummy, dummy, dummy, ddum, ddum);
    in >> ob2;
    in.close();
    remove( "junk" );

    // inserter-extrator test
    assert( !(ob1 != ob2) );
    
    IndResults ob3(3, dummy, dummy, dummy);
    ob3 = ob1;
    assert(!( ob3 != ob1 ));

    return;
}

