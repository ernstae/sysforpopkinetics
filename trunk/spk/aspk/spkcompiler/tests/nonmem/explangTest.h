/**
 * \file explangTest.h
 * \brief A test suite for the NONMEM parser, nm_parse().
 */
#ifndef TEST_EXPLANG_TEST_H
#define TEST_EXPLANG_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

#include <spkcompiler/ExpTreeGenerator.h>

class explangTest : public CppUnit::TestFixture {

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
