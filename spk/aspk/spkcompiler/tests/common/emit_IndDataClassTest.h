/**
 * @file emit_IndDataClassTest.h
 * @brief A test suite for emit_IndDataClass() function.
 *
 */
#ifndef TEST_EMIT_INDDATACLASS_H
#define TEST_EMIT_INDDATACLASS_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests for emit_IndDataClass() function.
 */
class emit_IndDataClassTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();
    
    void test();
    static CppUnit::Test * suite();
};

#endif
