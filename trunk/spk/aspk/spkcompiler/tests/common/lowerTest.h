/**
 * @file lowerTest.h
 * @brief A test suite for lower().
 *
 */
#ifndef TEST_LOWER_NAMESPACE_H
#define TEST_LOWER_NAMESPACE_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests lower(string).
 */
class lowerTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();
    
    void testMix();
    void testEmpty();

    static CppUnit::Test * suite();
};

#endif
