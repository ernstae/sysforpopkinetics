/**
 * @file ExpNodeCarrierTest.h
 *
 * A test suite for ExpNodeCarrier class.
 */
#ifndef EXPNODECARRIER_TEST_H
#define EXPNODECARRIER_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests for ExpNodeCarrier class.
 */
class ExpNodeCarrierTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();

    /**
     * @test Tests if the carrier carries a node.
     */
    void testExpNodeCarrier();

    static CppUnit::Test * suite();
};

#endif
