/**
 * @file ExpTreeGeneratorTest.h
 *
 * A test suite for ExpTreeGenerator class.
 */
#ifndef EXPTREEGENERATOR_TEST_H
#define EXPTREEGENERATOR_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests for ExpTreeGenerator class.
 */
class ExpTreeGeneratorTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();

    /**
     * @test Tests constructors
     */
    void testCreate();

    /**
     * @test Tests getRoot() which returns a pointer to the DOMDocument.
     */
    void testGetRoot();

    /**
     * @test Tests createNode() and releaseNodes()
     */
    void testNodeCarrier();

    static CppUnit::Test * suite();
};

#endif
