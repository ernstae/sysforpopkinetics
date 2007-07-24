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
 * File: intToOrdinalStringTest.cpp
 *
 *
 * Unit test for the function intToOrdinalString.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

#include <iostream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/intToOrdinalString.h"
#include "intToOrdinalStringTest.h"

using namespace std;
using namespace CppUnit;


/*************************************************************************
 *
 * CppUnit framework functions.
 *
 *************************************************************************/

void intToOrdinalStringTest::setUp()
{
  // initializations
}
void intToOrdinalStringTest::tearDown()
{
  // clean up
}

Test* intToOrdinalStringTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "intToOrdinalStringTest" );

  suiteOfTests->addTest( new TestCaller<intToOrdinalStringTest>(
    "basicTest", &intToOrdinalStringTest::basicTest ) );

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: basicTest
 *
 *
 * This test checks all of the capabilities of the function.
 *
 *************************************************************************/

void intToOrdinalStringTest::basicTest()
{
  //------------------------------------------------------------
  // Check the values when the first integer is zero.
  //------------------------------------------------------------

  CPPUNIT_ASSERT( intToOrdinalString( -3, ZERO_IS_FIRST_INT ) ==
                  "-3 (warning: nonpositive ordinal)" );
  CPPUNIT_ASSERT( intToOrdinalString( -2, ZERO_IS_FIRST_INT ) ==
                  "-2 (warning: nonpositive ordinal)" );
  CPPUNIT_ASSERT( intToOrdinalString( -1, ZERO_IS_FIRST_INT ) ==
                  "-1 (warning: nonpositive ordinal)" );

  CPPUNIT_ASSERT( intToOrdinalString(  0, ZERO_IS_FIRST_INT ) == "1st" );
  CPPUNIT_ASSERT( intToOrdinalString(  1, ZERO_IS_FIRST_INT ) == "2nd" );
  CPPUNIT_ASSERT( intToOrdinalString(  2, ZERO_IS_FIRST_INT ) == "3rd" );
  CPPUNIT_ASSERT( intToOrdinalString(  3, ZERO_IS_FIRST_INT ) == "4th" );

  CPPUNIT_ASSERT( intToOrdinalString( 10, ZERO_IS_FIRST_INT ) == "11th" );
  CPPUNIT_ASSERT( intToOrdinalString( 11, ZERO_IS_FIRST_INT ) == "12th" );
  CPPUNIT_ASSERT( intToOrdinalString( 12, ZERO_IS_FIRST_INT ) == "13th" );
  CPPUNIT_ASSERT( intToOrdinalString( 13, ZERO_IS_FIRST_INT ) == "14th" );

  CPPUNIT_ASSERT( intToOrdinalString( 20, ZERO_IS_FIRST_INT ) == "21st" );
  CPPUNIT_ASSERT( intToOrdinalString( 21, ZERO_IS_FIRST_INT ) == "22nd" );
  CPPUNIT_ASSERT( intToOrdinalString( 22, ZERO_IS_FIRST_INT ) == "23rd" );
  CPPUNIT_ASSERT( intToOrdinalString( 23, ZERO_IS_FIRST_INT ) == "24th" );

  CPPUNIT_ASSERT( intToOrdinalString( 90, ZERO_IS_FIRST_INT ) == "91st" );
  CPPUNIT_ASSERT( intToOrdinalString( 91, ZERO_IS_FIRST_INT ) == "92nd" );
  CPPUNIT_ASSERT( intToOrdinalString( 92, ZERO_IS_FIRST_INT ) == "93rd" );
  CPPUNIT_ASSERT( intToOrdinalString( 93, ZERO_IS_FIRST_INT ) == "94th" );

  CPPUNIT_ASSERT( intToOrdinalString( 100, ZERO_IS_FIRST_INT ) == "101st" );
  CPPUNIT_ASSERT( intToOrdinalString( 101, ZERO_IS_FIRST_INT ) == "102nd" );
  CPPUNIT_ASSERT( intToOrdinalString( 102, ZERO_IS_FIRST_INT ) == "103rd" );
  CPPUNIT_ASSERT( intToOrdinalString( 103, ZERO_IS_FIRST_INT ) == "104th" );

  CPPUNIT_ASSERT( intToOrdinalString( 110, ZERO_IS_FIRST_INT ) == "111th" );
  CPPUNIT_ASSERT( intToOrdinalString( 111, ZERO_IS_FIRST_INT ) == "112th" );
  CPPUNIT_ASSERT( intToOrdinalString( 112, ZERO_IS_FIRST_INT ) == "113th" );
  CPPUNIT_ASSERT( intToOrdinalString( 113, ZERO_IS_FIRST_INT ) == "114th" );

  CPPUNIT_ASSERT( intToOrdinalString( 120, ZERO_IS_FIRST_INT ) == "121st" );
  CPPUNIT_ASSERT( intToOrdinalString( 121, ZERO_IS_FIRST_INT ) == "122nd" );
  CPPUNIT_ASSERT( intToOrdinalString( 122, ZERO_IS_FIRST_INT ) == "123rd" );
  CPPUNIT_ASSERT( intToOrdinalString( 123, ZERO_IS_FIRST_INT ) == "124th" );

  CPPUNIT_ASSERT( intToOrdinalString( 98432300, ZERO_IS_FIRST_INT ) == "98432301st" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432301, ZERO_IS_FIRST_INT ) == "98432302nd" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432302, ZERO_IS_FIRST_INT ) == "98432303rd" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432303, ZERO_IS_FIRST_INT ) == "98432304th" );

  CPPUNIT_ASSERT( intToOrdinalString( 98432310, ZERO_IS_FIRST_INT ) == "98432311th" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432311, ZERO_IS_FIRST_INT ) == "98432312th" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432312, ZERO_IS_FIRST_INT ) == "98432313th" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432313, ZERO_IS_FIRST_INT ) == "98432314th" );

  CPPUNIT_ASSERT( intToOrdinalString( 98432320, ZERO_IS_FIRST_INT ) == "98432321st" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432321, ZERO_IS_FIRST_INT ) == "98432322nd" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432322, ZERO_IS_FIRST_INT ) == "98432323rd" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432323, ZERO_IS_FIRST_INT ) == "98432324th" );


  //------------------------------------------------------------
  // Check the values when the first integer is one.
  //------------------------------------------------------------

  CPPUNIT_ASSERT( intToOrdinalString( -2, ONE_IS_FIRST_INT ) ==
                  "-2 (warning: nonpositive ordinal)" );
  CPPUNIT_ASSERT( intToOrdinalString( -1, ONE_IS_FIRST_INT ) ==
                  "-1 (warning: nonpositive ordinal)" );
  CPPUNIT_ASSERT( intToOrdinalString(  0, ONE_IS_FIRST_INT ) ==
                  "0 (warning: nonpositive ordinal)" );

  CPPUNIT_ASSERT( intToOrdinalString(  1, ONE_IS_FIRST_INT ) == "1st" );
  CPPUNIT_ASSERT( intToOrdinalString(  2, ONE_IS_FIRST_INT ) == "2nd" );
  CPPUNIT_ASSERT( intToOrdinalString(  3, ONE_IS_FIRST_INT ) == "3rd" );
  CPPUNIT_ASSERT( intToOrdinalString(  4, ONE_IS_FIRST_INT ) == "4th" );

  CPPUNIT_ASSERT( intToOrdinalString( 11, ONE_IS_FIRST_INT ) == "11th" );
  CPPUNIT_ASSERT( intToOrdinalString( 12, ONE_IS_FIRST_INT ) == "12th" );
  CPPUNIT_ASSERT( intToOrdinalString( 13, ONE_IS_FIRST_INT ) == "13th" );
  CPPUNIT_ASSERT( intToOrdinalString( 14, ONE_IS_FIRST_INT ) == "14th" );

  CPPUNIT_ASSERT( intToOrdinalString( 21, ONE_IS_FIRST_INT ) == "21st" );
  CPPUNIT_ASSERT( intToOrdinalString( 22, ONE_IS_FIRST_INT ) == "22nd" );
  CPPUNIT_ASSERT( intToOrdinalString( 23, ONE_IS_FIRST_INT ) == "23rd" );
  CPPUNIT_ASSERT( intToOrdinalString( 24, ONE_IS_FIRST_INT ) == "24th" );

  CPPUNIT_ASSERT( intToOrdinalString( 91, ONE_IS_FIRST_INT ) == "91st" );
  CPPUNIT_ASSERT( intToOrdinalString( 92, ONE_IS_FIRST_INT ) == "92nd" );
  CPPUNIT_ASSERT( intToOrdinalString( 93, ONE_IS_FIRST_INT ) == "93rd" );
  CPPUNIT_ASSERT( intToOrdinalString( 94, ONE_IS_FIRST_INT ) == "94th" );

  CPPUNIT_ASSERT( intToOrdinalString( 101, ONE_IS_FIRST_INT ) == "101st" );
  CPPUNIT_ASSERT( intToOrdinalString( 102, ONE_IS_FIRST_INT ) == "102nd" );
  CPPUNIT_ASSERT( intToOrdinalString( 103, ONE_IS_FIRST_INT ) == "103rd" );
  CPPUNIT_ASSERT( intToOrdinalString( 104, ONE_IS_FIRST_INT ) == "104th" );

  CPPUNIT_ASSERT( intToOrdinalString( 111, ONE_IS_FIRST_INT ) == "111th" );
  CPPUNIT_ASSERT( intToOrdinalString( 112, ONE_IS_FIRST_INT ) == "112th" );
  CPPUNIT_ASSERT( intToOrdinalString( 113, ONE_IS_FIRST_INT ) == "113th" );
  CPPUNIT_ASSERT( intToOrdinalString( 114, ONE_IS_FIRST_INT ) == "114th" );

  CPPUNIT_ASSERT( intToOrdinalString( 121, ONE_IS_FIRST_INT ) == "121st" );
  CPPUNIT_ASSERT( intToOrdinalString( 122, ONE_IS_FIRST_INT ) == "122nd" );
  CPPUNIT_ASSERT( intToOrdinalString( 123, ONE_IS_FIRST_INT ) == "123rd" );
  CPPUNIT_ASSERT( intToOrdinalString( 124, ONE_IS_FIRST_INT ) == "124th" );

  CPPUNIT_ASSERT( intToOrdinalString( 98432301, ONE_IS_FIRST_INT ) == "98432301st" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432302, ONE_IS_FIRST_INT ) == "98432302nd" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432303, ONE_IS_FIRST_INT ) == "98432303rd" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432304, ONE_IS_FIRST_INT ) == "98432304th" );

  CPPUNIT_ASSERT( intToOrdinalString( 98432311, ONE_IS_FIRST_INT ) == "98432311th" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432312, ONE_IS_FIRST_INT ) == "98432312th" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432313, ONE_IS_FIRST_INT ) == "98432313th" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432314, ONE_IS_FIRST_INT ) == "98432314th" );

  CPPUNIT_ASSERT( intToOrdinalString( 98432321, ONE_IS_FIRST_INT ) == "98432321st" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432322, ONE_IS_FIRST_INT ) == "98432322nd" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432323, ONE_IS_FIRST_INT ) == "98432323rd" );
  CPPUNIT_ASSERT( intToOrdinalString( 98432324, ONE_IS_FIRST_INT ) == "98432324th" );

}

