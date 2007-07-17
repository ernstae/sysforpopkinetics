/**
 * @file countStrInLhsTest.h
 * @brief A test suite for countStrInLhs().
 *
 */
#ifndef COUNTVARINLHS_TEST_H
#define COUNTVARINLHS_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests countStrInLhs(var, assignments).
 */
class countStrInLhsTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();
    
    void testSimple();
    static CppUnit::Test * suite();
};

#endif
