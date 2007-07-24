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
 * File: normTest.cpp
 *
 *
 * Unit test for norm.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#include <iostream>
#include <cassert>
#include "../../../spk/norm.h"
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "normTest.h"

using namespace CppUnit;

void normTest::setUp()
{
    // initializations
}
void normTest::tearDown()
{
    // clean up
}

Test* normTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "normTest" );

  // Test cases for this unit test.
  suiteOfTests->addTest( new TestCaller<normTest>( "posAndNegCase", &normTest::posAndNegCase ) );
  suiteOfTests->addTest( new TestCaller<normTest>( "noNegCase", &normTest::noNegCase ) );

  return suiteOfTests;
}

static void exampleInSpec();
static DoubleMatrix mySet(int d1, int d2, int d3, int d4, int d5, int d6);

void normTest::posAndNegCase()
{
    using namespace std;
    DoubleMatrix dmatA(3,2);

    //exampleInSpec();
    
    dmatA = mySet(1,1,0,-1,0,1);
    CPPUNIT_ASSERT_EQUAL( norm(dmatA), 2.0 );
}
void normTest::noNegCase()
{
    using namespace std;
    DoubleMatrix dmatA(3,2);

    dmatA = mySet(2,0,2,2,2,0);
    CPPUNIT_ASSERT_EQUAL( norm(dmatA), 4.0 );
}
static void exampleInSpec(){

    using namespace std;

        using namespace std;

        DoubleMatrix dmatA(3,3);
        double *pdA = dmatA.data();

        // set the matrix to:
        // 
        // [0, 3, 2]
        // [1, 0, 3]
        // [2, 1, 0]
        //
        for( int i=0; i<3*3; i++ )
            pdA[i] = i%4;
}
static DoubleMatrix mySet(int d1, int d2, int d3, int d4, int d5, int d6){
    DoubleMatrix dmatA(3,2);
    double *pdA = dmatA.data();

    pdA[0] = d1;
    pdA[1] = d2;
    pdA[2] = d3;
    pdA[3] = d4;
    pdA[4] = d5;
    pdA[5] = d6;
    return dmatA;
}
    
