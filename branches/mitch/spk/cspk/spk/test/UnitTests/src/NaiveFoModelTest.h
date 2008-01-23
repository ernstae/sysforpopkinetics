/*************************************************************************
 *
 * File: NaiveFoModelTest.h
 *
 *
 * Declares NaiveFoModelTest class.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#ifndef NAIVE_FOMODEL_TEST_H
#define NAIVE_FOMODEL_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>

class NaiveFoModelTest : public CppUnit::TestFixture
{
public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    // add test cases as void member functions
    void modelValidation();
    void foModelValidation();
};

#endif
