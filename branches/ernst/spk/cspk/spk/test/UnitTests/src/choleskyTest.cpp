/*************************************************************************
 *
 * File: cholesky.cpp
 *
 *
 * Test cases for cholesky()
 *
 * Author: Sachiko Honda, based on det() written by Mitchell Watrous
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/SpkValarray.h"
#include "../../../spk/cholesky.h"
#include "choleskyTest.h"
#include "../../../spk/printInMatrix.h"

using SPK_VA::valarray;
using namespace std;
using namespace CppUnit;

void choleskyTest::setUp()
{
    // initializations
}
void choleskyTest::tearDown()
{
    // clean up
}

Test* choleskyTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "choleskyTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<choleskyTest>("threeByThree", &choleskyTest::threeByThree));
    suiteOfTests->addTest(new TestCaller<choleskyTest>("fourByFour",   &choleskyTest::fourByFour));

    return suiteOfTests;
}

void choleskyTest::threeByThree()
{
    const int n = 3;
    double a[] = { 2, 0, 1, 0, 3, 0, 1, 0, 1 };
    double e[] = { 1.41421, 0, 0.707107, 0, 1.73205, 0, 0, 0, 0.707107 };
    valarray<double> A( a, n*n );
    valarray<double> AChol = cholesky(A, n);
    valarray<double> expected( e, n*n );
    /*
    cout << "A = " << endl;
    printInMatrix(A,n);
    cout << "AChol = " << endl;
    printInMatrix(AChol,n);
    cout << "expected = " << endl;
    printInMatrix(expected,n);
    */
    CPPUNIT_ASSERT_MESSAGE( "AChol.size() == n*n", AChol.size() == n*n ); 
    const double tol = 1e-5;
    for( int i=0; i<n*n; i++ )
    {
        CPPUNIT_ASSERT_DOUBLES_EQUAL( e[i], AChol[i], tol );
    }
    
}
void choleskyTest::fourByFour()
{
    const int n = 4;
    double a[] = { 1.0, 0.0, 0.0, 0.5, 0.0, 2.0,     1.0,      0.0, 0.0, 1.0, 2.0,     0.0, 0.5, 0.0, 0.0, 1.0 };
    double e[] = { 1.0, 0.0, 0.0, 0.5, 0.0, 1.41421, 0.707107, 0.0, 0.0, 0.0, 1.22474, 0.0, 0.0, 0.0, 0.0, 0.866025 };
    valarray<double> A( a, n*n );
    valarray<double> AChol = cholesky(A, n);
    valarray<double> expected( e, n*n );
    /*
    cout << "A = " << endl;
    printInMatrix(A,n);
    cout << "AChol = " << endl;
    printInMatrix(AChol,n);
    cout << "expected = " << endl;
    printInMatrix(expected,n);
    */
    CPPUNIT_ASSERT_MESSAGE( "AChol.size() == n*n",  AChol.size() == n*n ); 
    const double tol = 1e-5;
    for( int i=0; i<n*n; i++ )
    {
        CPPUNIT_ASSERT_DOUBLES_EQUAL( e[i], AChol[i], tol );
    }
}
