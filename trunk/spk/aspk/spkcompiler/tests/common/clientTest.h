/**
 * @file clientTest.h
 * @brief A test suite for components decleared/defined in
 * the "client" namespace.
 *
 */
#ifndef TEST_CLIENT_NAMESPACE_H
#define TEST_CLIENT_NAMESPACE_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests for components
 * declared/defined in the "client" namespace.
 */
class clientTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();
    
    void testEnumulator();

    static CppUnit::Test * suite();
};

#endif
