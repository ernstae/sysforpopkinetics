/**
 * @file SymbolTest.h
 * @brief A test suite for Symbol class.
 *
 */
#ifndef TEST_SYMBOL_H
#define TEST_SYMBOL_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests for Symbol class.
 */
class SymbolTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();

    /**
     * @test Tests constructors
     */
    void testCreate();

    /**
     * @test Tests defining scalar variables
     */
    void testDefineScalar();
    
    /**
     * @test Tests defining vector variables
     */
    void testDefineVector();

    /**
     * @test Tests defining matrix variables
     */
    void testDefineMatrix();

    /**
     * @test Tests converting an enum to a string
     */
    void testToString();

    /**
     * @test Tests converting a string to an enum
     */
    void testToEnum();

    static CppUnit::Test * suite();
};

#endif
