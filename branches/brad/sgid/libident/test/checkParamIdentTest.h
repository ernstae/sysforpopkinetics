/*************************************************************************
 *
 * File: checkParamIdentTest.h
 *
 *
 * Unit test for the function checkParamIdent.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef CHECKPARAMIDENT_TEST_H
#define CHECKPARAMIDENT_TEST_H

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

class checkParamIdentTest : public CppUnit::TestFixture
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
