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
 * File: IndVarsTest.cpp
 *
 *
 * Unit test for IndVars class.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#pragma warning( disable : 4786 )

#include <sstream>
#include "../../../spk/IndVars.h"
#include "../../../spk/DoubleMatrix.h"
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "IndVarsTest.h"

using namespace CppUnit;

void IndVarsTest::setUp()
{
    // initializations
}
void IndVarsTest::tearDown()
{
    // clean up
}

Test* IndVarsTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "IndVarsTest" );

  // Test cases for this unit test.
  suiteOfTests->addTest( new TestCaller<IndVarsTest>( "extractorInsertorCase", &IndVarsTest::extractorInsertorCase ) );
  suiteOfTests->addTest( new TestCaller<IndVarsTest>( "assignmentCase", &IndVarsTest::assignmentCase ) );

  return suiteOfTests;
}

using namespace std;

void IndVarsTest::extractorInsertorCase()
{
    int inx = 3;
    int n = 3;
    DoubleMatrix bIn(n,1);
    DoubleMatrix y(n,1);
    DoubleMatrix dummy(n,1);
    bIn.fill(1);
    dummy.fill(0);

    IndVars ob1(inx, bIn, y);

    stringstream stream;
    stream << ob1;

    IndVars ob2(9, dummy, y);
    stream >> ob2;

    // extractor-inserter test
    CPPUNIT_ASSERT( !(ob1 != ob2) );
}
void IndVarsTest::assignmentCase()
{
    int inx = 3;
    int n = 3;
    DoubleMatrix bIn(n,1);
    DoubleMatrix y(n,1);
    DoubleMatrix dummy(n,1);
    bIn.fill(1);
    dummy.fill(0);

    IndVars ob1(inx, bIn, y);

    // assignment test
    IndVars ob3(7, dummy, y);
    ob3 = ob1;
    CPPUNIT_ASSERT( !( ob3 != ob1 ) );
}

