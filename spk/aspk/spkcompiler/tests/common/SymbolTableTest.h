/**
 * @file SymbolTableTest.h
 * @brief A test suite for SymbolTable class.
 */
#ifndef TEST_SYMBOLTABLE_H
#define TEST_SYMBOLTABLE_H

#include <cppunit/TestFixture.h>
#include "spkcompiler/SymbolTable.h"

/**
 * A class that organizes a suite of tests for SymbolTable class.
 */
class SymbolTableTest : public CppUnit::TestFixture {
public:

  virtual void setUp();
  virtual void tearDown();

  void testInsertNMVector();
  void testInsertNMMatrix();
  void testInsertUserVar();
  void testInsertLabel();
  void testEnd(); 

  static CppUnit::Test * suite();
};

#endif
