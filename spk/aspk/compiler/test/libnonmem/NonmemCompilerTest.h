/**
 * \file NonmemCompilerTest.h
 * \brief A test suite for NonmemCompiler class.
 */
#ifndef TEST_NONMEMOMPILER_H
#define TEST_NONMEMCOMPILER_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>
#include "NonmemSpkMLToCpp.h"

class NonmemCompilerTest : public CppUnit::TestFixture {

  NonmemSpkMLToCpp * compiler;
public:
    static CppUnit::Test * suite();
    virtual void setUp();
    virtual void tearDown();

    void testTranslate();
};

#endif
