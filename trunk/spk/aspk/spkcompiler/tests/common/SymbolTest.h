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

   enum Symbol::SymbolType datalabel;
   enum Symbol::SymbolType nonmem;
   enum Symbol::SymbolType userdef;
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

    /**
     * @test Tests the default constructor.
     */
    void testDefaultConstructor();

    /**
     * @test Tests the non-default constructors
     */
    void testConstructor();

    void testCopyConstructor();

    /**
     * @test Tests the assignment operator
     */
    void testAssign();

    /**
     * @test Tests the static function that creates a Symbol object for a data label.
     */
    void testCreateLabel();

    /**
     * @test Tests the static function that creates a Symbol object for a NM variable.
     */
    void testCreateNMVar();

    /**
     * @test Tests the static function that creates a Symbol object for a user variable.
     */
    void testCreateUserVar();

    /**
     * @test Test the == and !- operators.
     */
    void testEquality();

    /**
     * @test Test the empty() function.
     */
    void testEmpty();

    static CppUnit::Test * suite();
};
#endif
