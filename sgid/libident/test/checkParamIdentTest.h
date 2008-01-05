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
  void paperTwoCompExample_globallyIdent_Test();
  void paperTwoCompExample_realNumberPrinting_Test();

  void paperThreeCompAppendixExample_notAlgebraicallyObserv_Test();

  void oralAbsorpTwoCompModel_twoSolutions_Test();

  void multipleGroebnerBasis_eightSolutions_Test();

  void fourCompModel_sixSolutions_Test();

  void noExhaustSummModel_zeroSolutions_Test();

  void threeCompModel_infiniteSolutions_Test();
};

#endif
