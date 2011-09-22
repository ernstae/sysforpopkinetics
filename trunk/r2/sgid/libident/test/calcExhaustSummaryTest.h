/*************************************************************************
 *
 * File: calcExhaustSummaryTest.h
 *
 *
 * Unit test for the function calcExhaustSummary.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef CALCEXHAUSTSUMMARY_TEST_H
#define CALCEXHAUSTSUMMARY_TEST_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// CppUnit framework header files.
#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>

// Standard library header files.
#include <string>


/*------------------------------------------------------------------------
 * Class declaration
 *------------------------------------------------------------------------*/

class calcExhaustSummaryTest : public CppUnit::TestFixture
{
public: 
  // CppUnit framework functions.
  static CppUnit::Test* suite();
  void setUp();
  void tearDown();

private:
  // Test cases for this unit test.
  void paperTwoCompExample_Test();
  void oralAbsorpTwoCompModel_twoSolutions_Test();
};

#endif
