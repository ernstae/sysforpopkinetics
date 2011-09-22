/**
 * @file cholskeyTest.h
 * @brief A test suite for cholesky().
 *
 */
#ifndef TEST_CHOLESKY_H
#define TEST_CHOLESKY_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests cholesky().
 */
class choleskyTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();
    
    void test3by3();

    static CppUnit::Test * suite();
};

#endif
