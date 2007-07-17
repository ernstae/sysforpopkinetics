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
 * File: isUnnormNumberTest.cpp
 *
 *
 * Unit test for the function isUnnormNumber.
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
#include "../../../spk/isUnnormNumber.h"
#include "isUnnormNumberTest.h"

using namespace std;
using namespace CppUnit;


/*************************************************************************
 *
 * CppUnit framework functions.
 *
 *************************************************************************/

void isUnnormNumberTest::setUp()
{
  // initializations
}
void isUnnormNumberTest::tearDown()
{
  // clean up
}

Test* isUnnormNumberTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "isUnnormNumberTest" );

  suiteOfTests->addTest( new TestCaller<isUnnormNumberTest>(
    "basicTest", &isUnnormNumberTest::basicTest ) );

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

void isUnnormNumberTest::basicTest()
{
  //------------------------------------------------------------
  // Test double values
  //------------------------------------------------------------

  bool isUnnormNumberResult;

  double one  = 1.0;
  double zero = 0.0;
  double value;

  // Check a number that is normalized.
  value = one;
  isUnnormNumberResult = isUnnormNumber( value ); 
  CPPUNIT_ASSERT( !isUnnormNumberResult );

  // Check plus infinity.
  value = one / zero;
  isUnnormNumberResult = isUnnormNumber( value ); 
  CPPUNIT_ASSERT( isUnnormNumberResult );

  // Check minus infinity.
  value = - one / zero;
  isUnnormNumberResult = isUnnormNumber( value ); 
  CPPUNIT_ASSERT( isUnnormNumberResult );

  // Check a positive number that is unnormalized.
  value = numeric_limits<double>::max() * 10.0;
  isUnnormNumberResult = isUnnormNumber( value ); 
  CPPUNIT_ASSERT( isUnnormNumberResult );

  // Check a positive number that is unnormalized.
  value = - numeric_limits<double>::max() * 10.0;
  isUnnormNumberResult = isUnnormNumber( value ); 
  CPPUNIT_ASSERT( isUnnormNumberResult );


  //------------------------------------------------------------
  // Test CppAD<double> values.
  //------------------------------------------------------------

  CppAD::AD<double> oneAD  = 1.0;
  CppAD::AD<double> zeroAD = 0.0;
  CppAD::AD<double> valueAD;

  // Check a number that is normalized.
  valueAD = oneAD;
  isUnnormNumberResult = isUnnormNumber( valueAD ); 
  CPPUNIT_ASSERT( !isUnnormNumberResult );

  // Check plus infinity.
  valueAD = oneAD / zeroAD;
  isUnnormNumberResult = isUnnormNumber( valueAD ); 
  CPPUNIT_ASSERT( isUnnormNumberResult );

  // Check minus infinity.
  valueAD = - oneAD / zeroAD;
  isUnnormNumberResult = isUnnormNumber( valueAD ); 
  CPPUNIT_ASSERT( isUnnormNumberResult );

}

