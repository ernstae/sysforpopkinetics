/**
 * @file emit_driverTest.h
 * @brief A test suite for emit_driver() function.
 *
 */
#ifndef TEST_EMIT_DRIVER_H
#define TEST_EMIT_DRIVER_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests for emit_driver() function.
 */
class emit_driverTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();
    
    void test();
    static CppUnit::Test * suite();
};

#endif
