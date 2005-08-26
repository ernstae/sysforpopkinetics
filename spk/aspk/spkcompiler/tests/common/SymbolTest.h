/**
 * @file SymbolTest.h
 * @brief A test suite for Symbol class.
 *
 */
#ifndef TEST_SYMBOL_H
#define TEST_SYMBOL_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>
#include "../../spkcompiler/Symbol.h"

/**
 * A class that organizes a suite of tests for Symbol class.
 */
class SymbolTest : public CppUnit::TestFixture {

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

    void testDefaultConstructor();
    void testConstructor();
    void testCopyConstructor();
    void testAssign();
    void testCreateLabel();
    void testCreateScalar();
    void testCreateVector();
    void testCreateSymmetricMatrix();
    void testEquality();
    void testEmpty();

    static CppUnit::Test * suite();
};
#endif
