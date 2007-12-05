/*************************************************************************
 *
 * File: choleskyTest.h
 *
 *
 * .
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#ifndef CHOLESKY_TEST_H
#define CHOLESKY_TEST_H

#include <cppunit/TestFixture.h>
#include <cppunit/Test.h>

class choleskyTest : public CppUnit::TestFixture
{
public: 
    static CppUnit::Test* suite();

    void setUp();
    void tearDown();

    // add test cases as void member functions
    void threeByThree();
    void fourByFour();
};

#endif
