/**
 * @file emit_IndDataTest.h
 * @brief A test suite for emit_IndDataClass(),
 * emit_initIndDataObjects() and emit_releaseIndDataObjects()
 * functions.
 *
 */
#ifndef TEST_EMIT_INDDATA_H
#define TEST_EMIT_INDDATA_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests for emit_IndData() function.
 */
class emit_IndDataTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();
    
    void test();
    static CppUnit::Test * suite();
};

#endif
