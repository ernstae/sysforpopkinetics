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
 * File: WarningsManagerTest.cpp
 *
 *
 * Unit test for the class WarningsManager.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK test suite header files.
#include "WarningsManagerTest.h"

// SPK library header files.
#include "../../../spk/WarningsManager.h"

// CppUnit framework header files.
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

// Standard library header files.
#include <cassert>
#include <iostream>

using namespace CppUnit;


/*************************************************************************
 *
 * Function: setUp
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void WarningsManagerTest::setUp()
{
    // initializations
}


/*************************************************************************
 *
 * Function: tearDown
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void WarningsManagerTest::tearDown()
{
    // clean up
}


/*************************************************************************
 *
 * Function: suite
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

Test* WarningsManagerTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "WarningsManagerTest" );

  suiteOfTests->addTest(new TestCaller<WarningsManagerTest>(
    "basicTest", 
    &WarningsManagerTest::basicTest));

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: basicTest
 *
 *
 * This test performs a basic check all of the functionalities of the
 * WarningsManager class.
 *
 *************************************************************************/

void WarningsManagerTest::basicTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  // Make sure there are no warnings in the list after they have all
  // been cleared.
  WarningsManager::clearAllWarnings();
  CPPUNIT_ASSERT_MESSAGE(
    "There were still warnings in the list after they had all been removed.",
    WarningsManager::anyWarnings() == false );


  // Add some test warnings to the list of warnings.
  string warning1 = "Test warning 1 (generated by WarningsManagerTest).";
  string warning2 = "Test warning 2 (generated by WarningsManagerTest).";
  string warnings;
  WarningsManager::addWarning( warning1, __LINE__, __FILE__);
  WarningsManager::addWarning( warning2, __LINE__, __FILE__);
  

  // Get the added warning list
  string warningList;
  int nWarnings = WarningsManager::getWarningList( warningList );

  // Add the warning list to the original list of warnings.
  WarningsManager::addWarningList( warningList, nWarnings );

  // Get all the warnings
  WarningsManager::getAllWarnings( warnings );

  // Uncomment these statements to see the warnings.
  /* 
  cout << "########################################" << endl;
  cout << warnings;
  cout << "########################################" << endl;
  */

  // Make sure there are warnings in the list after some have been
  // added.
  CPPUNIT_ASSERT_MESSAGE(
    "There were no warnings in the list after two had been added.",
    WarningsManager::anyWarnings() );

  // Make sure there are no warnings in the list after they have all
  // been cleared.
  WarningsManager::clearAllWarnings();
  CPPUNIT_ASSERT_MESSAGE(
    "There were still warnings in the list after they had all been removed.",
    WarningsManager::anyWarnings() == false );

  // Uncomment these statements to see the warnings.
  /*
  WarningsManager::getAllWarnings( warnings );
  cout << "########################################" << endl;
  cout << warnings;
  cout << "########################################" << endl;
  */

}


