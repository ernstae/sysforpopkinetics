/**
 * \file NonmemTranslatorTest.h
 * \brief A test suite for NonmemTranslator class.
 */
#ifndef TEST_NONMETRANSLATOR_H
#define TEST_NONMETRANSLTOR_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>
#include "NonmemTranslator.h"

class NonmemTranslatorTest : public CppUnit::TestFixture {

public:
    static CppUnit::Test * suite();
    virtual void setUp();
    virtual void tearDown();

    void testConstructor();
    void testTranslate();
};

#endif
