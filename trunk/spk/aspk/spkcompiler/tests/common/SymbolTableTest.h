/**
 * @file SymbolTableTest.h
 * @brief A test suite for SymbolTable class.
 */
#ifndef TEST_SYMBOLTABLE_H
#define TEST_SYMBOLTABLE_H

#include <cppunit/TestFixture.h>
#include "SymbolTable.h"

/**
 * A class that organizes a suite of tests for SymbolTable class.
 */
class SymbolTableTest : public CppUnit::TestFixture {
  SymbolTable * table;
  
public:

  virtual void setUp();
  virtual void tearDown();
  
  /**
   * @test Tests inserting new symbols
   */
  void testRegister();

  /**
   * @test Tests defining variables associated with symbols that are already in the table
   */
  void testDefine();

  /**
   * @test Tests uniqueness of keys in the table.
   */
  void testReRegister();

  static CppUnit::Test * suite();
};

#endif
