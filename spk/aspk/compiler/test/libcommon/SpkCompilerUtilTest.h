/**
 * @file SpkCompilerUtilTest.h
 *
 * A test suite for SpkCompilerUtil class.
 */
#ifndef SPKCOMPILERUTIL_TEST_H
#define SPKCOMPILERUTIL_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests for SpkCompilerUtil class.
 */
class SpkCompilerUtilTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();

    /**
     * @test Tests constructors
     */
    void testCreate();

    
    /**
     * @test Tests createXmlString() and createCStrings(), and
     * their corresponding resource release functions.
     */
    void testStringConversion();

    static CppUnit::Test * suite();
};

#endif
