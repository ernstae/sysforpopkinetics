/**
 * @file emit_nonmem_modelTest.h
 * 
 * @brief A test suite for emit_nonmem_model() function.
 *
 */
#ifndef TEST_EMIT_NONMEM_MODEL_H
#define TEST_EMIT_NONMEM_MODEL_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>
#include <spkcompiler/SpkParameters.h>
#include <spkcompiler/nonmem/NonmemTranslator.h>

/**
 * A class that organizes a suite of tests for emit_nonmem_model() function.
 */
class emit_nonmem_modelTest : public CppUnit::TestFixture {

public:
    virtual void setUp();
    virtual void tearDown();
    
    void test();
    static CppUnit::Test * suite();
};

#endif
