/**
 * @file ParseTreeTest.h
 *
 * A test suite for ParseTree class.
 */
#ifndef PARSETREE_TEST_H
#define PARSETREE_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests for ParseTree class.
 */
class ParseTreeTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();

    /**
     * @test Tests constructors
     */
    void testCreate();

    /**
     * @test Tests handler() which returns a pointer to the DOMDocument.
     */
    void testHandler();

    /**
     * @test Tests createNode() and releaseNodes()
     */
    void testNodeCarrier();

    static CppUnit::Test * suite();
};

#endif
