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
 * File: hasUnnormNumberTest.cpp
 *
 *
 * Unit test for the function hasUnnormNumber.
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
#include "../../../spk/hasUnnormNumber.h"
#include "../../../spk/SpkValarray.h"
#include "hasUnnormNumberTest.h"


using SPK_VA::valarray;

using namespace std;
using namespace CppUnit;


/*************************************************************************
 *
 * CppUnit framework functions.
 *
 *************************************************************************/

void hasUnnormNumberTest::setUp()
{
  // initializations
}
void hasUnnormNumberTest::tearDown()
{
  // clean up
}

Test* hasUnnormNumberTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "hasUnnormNumberTest" );

  suiteOfTests->addTest( new TestCaller<hasUnnormNumberTest>(
    "basicTest", &hasUnnormNumberTest::basicTest ) );

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

void hasUnnormNumberTest::basicTest()
{
  //------------------------------------------------------------
  // Test double vectors
  //------------------------------------------------------------

  bool hasUnnormNumberResult;

  int nElem = 3;

  double one  = 1.0;
  double zero = 0.0;

  valarray<double> vector( 3 );

  // Check a vector that is all normalized.
  vector = one;
  hasUnnormNumberResult = hasUnnormNumber( vector ); 
  CPPUNIT_ASSERT( !hasUnnormNumberResult );

  // Check a vector that has one element equal to plus infinity.
  vector = one;
  vector[1] = one / zero;
  hasUnnormNumberResult = hasUnnormNumber( vector ); 
  CPPUNIT_ASSERT( hasUnnormNumberResult );

  // Check a vector that is all equal to plus infinity.
  vector = one / zero;
  hasUnnormNumberResult = hasUnnormNumber( vector ); 
  CPPUNIT_ASSERT( hasUnnormNumberResult );

  // Check a vector that is all equal to minus infinity.
  vector = - one / zero;
  hasUnnormNumberResult = hasUnnormNumber( vector ); 
  CPPUNIT_ASSERT( hasUnnormNumberResult );

  // Check a vector that is all equal to a positive number that is unnormalized.
  vector = numeric_limits<double>::max() * 10.0;
  hasUnnormNumberResult = hasUnnormNumber( vector ); 
  CPPUNIT_ASSERT( hasUnnormNumberResult );

  // Check a vector that is all equal to a negative number that is unnormalized.
  vector = - numeric_limits<double>::max() * 10.0;
  hasUnnormNumberResult = hasUnnormNumber( vector ); 
  CPPUNIT_ASSERT( hasUnnormNumberResult );


  //------------------------------------------------------------
  // Test CppAD<double> vectors.
  //------------------------------------------------------------

  CppAD::AD<double> oneAD  = 1.0;
  CppAD::AD<double> zeroAD = 0.0;

  valarray< CppAD::AD<double> > vectorAD( 3 );

  // Check a vector that is all normalized.
  vectorAD = oneAD;
  hasUnnormNumberResult = hasUnnormNumber( vectorAD ); 
  CPPUNIT_ASSERT( !hasUnnormNumberResult );

  // Check a vector that has one element equal to plus infinity.
  vectorAD = oneAD;
  vectorAD[1] = oneAD / zeroAD;
  hasUnnormNumberResult = hasUnnormNumber( vectorAD ); 
  CPPUNIT_ASSERT( hasUnnormNumberResult );

  // Check a vector that is all equal to plus infinity.
  vectorAD = oneAD / zeroAD;
  hasUnnormNumberResult = hasUnnormNumber( vectorAD ); 
  CPPUNIT_ASSERT( hasUnnormNumberResult );

  // Check a vector that is all equal to minus infinity.
  vectorAD = - oneAD / zeroAD;
  hasUnnormNumberResult = hasUnnormNumber( vectorAD ); 
  CPPUNIT_ASSERT( hasUnnormNumberResult );

  // Check a vector that is all equal to a positive number that is unnormalized.
  vectorAD = numeric_limits<double>::max() * 10.0;
  hasUnnormNumberResult = hasUnnormNumber( vectorAD ); 
  CPPUNIT_ASSERT( hasUnnormNumberResult );

  // Check a vector that is all equal to a negative number that is unnormalized.
  vectorAD = - numeric_limits<double>::max() * 10.0;
  hasUnnormNumberResult = hasUnnormNumber( vectorAD ); 
  CPPUNIT_ASSERT( hasUnnormNumberResult );

}

