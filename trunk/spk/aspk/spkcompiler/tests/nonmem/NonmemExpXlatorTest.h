/**
 * \file NonmemExpXlatorTest.h
 * \brief A test suite for Nonmem Expression Translator.
 */
#ifndef TEST_NONMEM_EXPXLATOR_H
#define TEST_NONMEM_EXPXLATOR_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include "ExpTreeGenerator.h"

class NonmemExpXlatorTest : public CppUnit::TestFixture {

  FILE * file;
  ExpTreeGenerator expTreeUtil;
public:
    static CppUnit::Test * suite();
    virtual void setUp();
    virtual void tearDown();

    void testScalarAssignmentToScalar();
    void testVectorElementAssignmentToScalar();
    void testFunctions();
    void testIfStmt();
    void testIfThenStmt();
};

#endif
