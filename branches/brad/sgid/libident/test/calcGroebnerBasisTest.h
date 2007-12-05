/*************************************************************************
 *
 * File: calcGroebnerBasisTest.h
 *
 *
 * Unit test for the function calcGroebnerBasis.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef CALCGROEBNERBASIS_TEST_H
#define CALCGROEBNERBASIS_TEST_H

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

class calcGroebnerBasisTest : public CppUnit::TestFixture
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
