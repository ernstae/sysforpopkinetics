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

   enum Symbol::ObjectType scalar;
   enum Symbol::ObjectType vec;
   enum Symbol::ObjectType matrix;
   enum Symbol::Structure  full;
   enum Symbol::Structure  diagonal;
   enum Symbol::Structure  triangle;
   enum Symbol::Ownership  user;
   enum Symbol::Ownership  system;
   enum Symbol::Access     readonly;
   enum Symbol::Access     readwrite;

public:

  virtual void setUp();
  virtual void tearDown();

  void testInsertVector();
  void testInsertSymmetricMatrix();
  void testInsertScalar();
  void testInsertLabel();
  void testEmpty(); 

  static CppUnit::Test * suite();
};

#endif
