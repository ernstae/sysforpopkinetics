/**
 * @file SpkMLToCppTest.h
 * @brief A test suite for SpkMLToCpp class.
 *
 */
#ifndef TEST_SPKMLTOCPP_H
#define TEST_SPKMLTOCPP_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests for SpkMLToCpp class.
 */
class SpkMLToCppTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();
    
    void test();
    static CppUnit::Test * suite();
};

#endif
