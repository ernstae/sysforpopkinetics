/**
 * @file ClientTranslatorTest.h
 * @brief A test suite for ClientTranslator abstract class.
 *
 */
#ifndef TEST_CLIENTTRANSLATOR_H
#define TEST_CLIENTTRANSLATOR_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

/**
 * A class that organizes a suite of tests for ClientTranslator
 * abstract class.
 */
class ClientTranslatorTest : public CppUnit::TestFixture {
public:
    virtual void setUp();
    virtual void tearDown();
    
    void testInterface();
    static CppUnit::Test * suite();
};

#endif
