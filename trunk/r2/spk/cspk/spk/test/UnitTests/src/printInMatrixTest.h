/*************************************************************************
 *
 * File: printInMatrixTest.h
 *
 *
 * Tests printInMatrix() which prints out an array in the matrix form.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#ifndef PRINT_IN_MATRIX_TEST_H
#define PRINT_IN_MATRIX_TEST_H

#include <iostream>
#include <string>
#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>

class printInMatrixTest : public CppUnit::TestFixture
{
public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    // add test cases as void member functions
    void emptyCase();
    void legalNonEmptyCase();
};

#endif
