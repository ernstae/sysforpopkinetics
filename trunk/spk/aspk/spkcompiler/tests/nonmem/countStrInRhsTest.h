/**
 * @file countStrInRhsTest.h
 * @brief A test suite for countStrInRhs().
 *
 */
#ifndef COUNTSTRINRHS_TEST_H
#define COUNTSTRINRHS_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests countStrInRhs()
 */
class countStrInRhsTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();
    
    void testSimple();

    static CppUnit::Test * suite();
};

#endif
