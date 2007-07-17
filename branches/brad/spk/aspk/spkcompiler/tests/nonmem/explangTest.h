/**
 * \file explangTest.h
 * \brief A test suite for the NONMEM parser, nm_parse().
 */
#ifndef TEST_EXPLANG_TEST_H
#define TEST_EXPLANG_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/TestSuite.h>

class explangTest : public CppUnit::TestFixture {

  FILE * file;
public:
    static CppUnit::Test * suite();
    virtual void setUp();
    virtual void tearDown();

    void testHAHN1_1();
    void testScalarAssignmentToScalar();
    void testVectorElementAssignmentToScalar();
    void testFunctions();
    void testIfStmt();
    void testIfThenStmt();
    void testIsTInRhs();
    void testLinInterp();
    void testNestedIf();    // if_construct nested in if_construct without ELSE but comments
    void testNestedIf1();   // if_stmt nested in if-stmt
    void testNestedIf2();   // if_stmt nested in if_construct without ELSE
    void testNestedIf3();   // if_stmt nested in if_construct followed by ELSE
    void testNestedIf4();   // if_stmt nested in if_construct following ELSE
    void testNestedIf5();   // two if_stmts nested in if_construct on both sides of ELSE
    void testNestedIf6();   // if_construct nested in if_construct without ELSE
    void testNestedIf7();   // if_construct nested in if_construct followed by ELSE
    void testNestedIf8();   // if_construct nested in if_construct following ELSE
    void testNestedIf9();   // two if_constructs nested in if_construct on both sides of ELSE
    void testNestedIf10();  // if_construct with ELSE nested in if_construct without ELSE
    void testNestedIf11();  // if_construct with ELSE nested in if_construct followed by ELSE
    void testNestedIf12();  // if_construct with ELSE nested in if_construct following by ELSE
    void testNestedIf13();  // two if_constructs nested in if_construct on both sides of ELSE,
                            // the one followed by the ELSE having ELSE
    void testNestedIf14();  // two if_constructs nested in if_construct on both sides of ELSE,
                            // the one following the ELSE having ELSE 
    void testNestedIf15();  // two if_constructs nested in if_construct on both sides of ELSE,
                            // both having ELSE
    void testMissingOperator();
    void testReservedPhrase();
    void testMatrixNotSupported();
    void testLeftSideOfEquation();
    void testFloatingIndex();
    void testZeroIndex();
};

#endif
