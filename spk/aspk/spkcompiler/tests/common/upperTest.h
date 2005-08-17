/**
 * @file upperTest.h
 * @brief A test suite for upper().
 *
 */
#ifndef TEST_UPPER_H
#define TEST_UPPER_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests upper(string).
 */
class upperTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();
    
    void testMix();
    void testEmpty();

    static CppUnit::Test * suite();
};

#endif
