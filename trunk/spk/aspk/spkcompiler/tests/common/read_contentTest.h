/**
 * @file read_contentTest.h
 * @brief A test suite for read_content() function.
 *
 */
#ifndef TEST_READ_CONTENT_H
#define TEST_READ_CONTENT_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests for read_content() function.
 */
class read_contentTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();
    
    void test();
    static CppUnit::Test * suite();
};

#endif
