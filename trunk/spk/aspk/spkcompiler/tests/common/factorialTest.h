/**
 * @file factorialTest.h
 * @brief A test suite for factorial().
 *
 */
#ifndef TEST_FACTORIAL_NAMESPACE_H
#define TEST_FACTORIAL_NAMESPACE_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests factorial(string).
 */
class factorialTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();
    
    void testZero();
    void testNonzero();

    static CppUnit::Test * suite();
};

#endif
