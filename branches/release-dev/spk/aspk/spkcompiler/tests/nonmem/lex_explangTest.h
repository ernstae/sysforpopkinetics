/**
 * \file lex_explangTest.h
 * \brief A test suite for the NONMEM Lexical Analyzer.
 */
#ifndef TEST_LEX_EXPLANGTEST_H
#define TEST_LEX_EXPLANGTEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

class lex_explangTest : public CppUnit::TestFixture {

public:
  static CppUnit::Test * suite();
  virtual void setUp();
  virtual void tearDown();
  void testWhiteSpaces();
  void testComment();
  void testIllegalComment();
  void testNamedConstant();
  void testNameLength();
  void testEngineeringNotation();
  void testFloatingPoint();
  void testDigitString();
  void testExit();
  void testControl();
  void testBool();
  void testBinaryFunc();
  void testUnaryFunc();
  void testArray();
  void testLogical();
};

#endif
