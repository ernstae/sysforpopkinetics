/**
 * @file seriesTest.h
 * @brief A test suite for series().
 *
 */
#ifndef TEST_SERIES_NAMESPACE_H
#define TEST_SERIES_NAMESPACE_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests series().
 */
class seriesTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();
    
    void testZero();
    void testNonzero();

    static CppUnit::Test * suite();
};

#endif
