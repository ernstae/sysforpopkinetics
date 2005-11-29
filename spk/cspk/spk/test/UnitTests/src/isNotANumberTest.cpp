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
 * File: isNotANumberTest.cpp
 *
 *
 * Unit test for the function isNotANumber.
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
#include <CppAD/CppAD.h>
#include "../../../spk/isNotANumber.h"
#include "isNotANumberTest.h"

using namespace std;
using namespace CppUnit;


/*************************************************************************
 *
 * CppUnit framework functions.
 *
 *************************************************************************/

void isNotANumberTest::setUp()
{
  // initializations
}
void isNotANumberTest::tearDown()
{
  // clean up
}

Test* isNotANumberTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "isNotANumberTest" );

  suiteOfTests->addTest( new TestCaller<isNotANumberTest>(
    "basicTest", &isNotANumberTest::basicTest ) );

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: basicTest
 *
 *
 * Tests basic capabilities of the function.
 *
 *************************************************************************/

void isNotANumberTest::basicTest()
{
  //------------------------------------------------------------
  // Test double values
  //------------------------------------------------------------

  bool isNotANumberResult;

  double plusOne  = +1.0;
  double minusOne = -1.0;
  double value;

  // Check a value that is a number.
  value = sqrt( plusOne );
  isNotANumberResult = isNotANumber( value ); 
  CPPUNIT_ASSERT( !isNotANumberResult );

  // Check a value that is not a number (NaN).
  value = sqrt( minusOne );
  isNotANumberResult = isNotANumber( value ); 
  CPPUNIT_ASSERT( isNotANumberResult );


  //------------------------------------------------------------
  // Test CppAD<double> values.
  //------------------------------------------------------------

  CppAD::AD<double> plusOneAD  = +1.0;
  CppAD::AD<double> minusOneAD = -1.0;
  CppAD::AD<double> valueAD;

  // Check a value that is a number.
  valueAD = sqrt( plusOneAD );
  isNotANumberResult = isNotANumber( valueAD ); 
  CPPUNIT_ASSERT( !isNotANumberResult );

  // Check a value that is not a number (NaN).
  valueAD = sqrt( minusOneAD );
  isNotANumberResult = isNotANumber( valueAD ); 
  CPPUNIT_ASSERT( isNotANumberResult );

}

