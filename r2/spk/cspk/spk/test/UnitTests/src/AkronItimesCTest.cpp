/*
%************************************************************************
%                                                                       *
%  From:   Resource Facility for Population Kinetics                    *
%          Department of Bioengineering Box 352255                      *
%          University of Washington                                     *
%          Seattle, WA 98195-2255                                       *
%                                                                       *
%  Copyright (C) 2002, University of Washington,                        *
%  Resource Facility for Population Kinetics. All Rights Reserved.      *
%                                                                       *
%  This software was developed with support from NIH grant RR-12609.    *
%  Please cite this grant in any publication for which this software    *
%  is used and send a notification to the address given above.          *
%                                                                       *
%  Check for updates and notices at:                                    *
%  http://www.rfpk.washington.edu                                       *
%                                                                       *
%************************************************************************

*/
/*************************************************************************
 *
 * File: AkronItimesCTest.cpp
 *
 *
 * Test cases for AkronItimesC
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/AkronItimesC.h"
#include "../../../spk/symmetrize.h"
#include "../../../spk/AkronBtimesC.h"
#include "../../../spk/identity.h"
#include "../../../spk/printInMatrix.h"
#include "AkronItimesCTest.h"

using namespace std;
using namespace CppUnit;

void AkronItimesCTest::setUp()
{
  /*
    _rows.push_back(0); _cols.push_back(0);
    _rows.push_back(0); _cols.push_back(1);
    _rows.push_back(1); _cols.push_back(0);
    _rows.push_back(1); _cols.push_back(1);
    _rows.push_back(1); _cols.push_back(3);
    _rows.push_back(3); _cols.push_back(1);
    _rows.push_back(3); _cols.push_back(3);
    _n = _rows.size();
  */
}
void AkronItimesCTest::tearDown()
{
    // clean up
}

Test* AkronItimesCTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("AkronItimesCTest");;

  // duplicate the following example for each test case, replacing the case name.
  
    suiteOfTests->addTest(new TestCaller<AkronItimesCTest>(
		       "testAkronItimesCVal", 
		       &AkronItimesCTest::testAkronItimesCVal));
    suiteOfTests->addTest(new TestCaller<AkronItimesCTest>(
		       "testAkronItimesCRef", 
		       &AkronItimesCTest::testAkronItimesCRef));
    suiteOfTests->addTest(new TestCaller<AkronItimesCTest>(
		       "testAkronItimesCValarray", 
		       &AkronItimesCTest::testAkronItimesCValarray));
    suiteOfTests->addTest(new TestCaller<AkronItimesCTest>(
		       "testAkronItimesCValarraySpecExample", 
		       &AkronItimesCTest::testAkronItimesCValarraySpecExample));
    return suiteOfTests;
}
void AkronItimesCTest::testAkronItimesCVal()
{
    const int m = 2;
    const int n = 3;
    const int k = 1;
    int i;
    double seq[] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30};
    DoubleMatrix A(m,n);
    DoubleMatrix I = identity(n);
    DoubleMatrix C(n*n,k);

    std::copy(seq, seq+m*n, A.data());
    std::copy(seq, seq+n*n*k, C.data());

    DoubleMatrix AIC = AkronItimesC(A,I,C);
    CPPUNIT_ASSERT_EQUAL(m*n, AIC.nr());
    CPPUNIT_ASSERT_EQUAL(k, AIC.nc());

    DoubleMatrix ABC = AkronBtimesC(A,I,C);
    CPPUNIT_ASSERT_EQUAL(m*n, ABC.nr());
    CPPUNIT_ASSERT_EQUAL(k, ABC.nc());
    //std::cout << "ABC=" << ABC << std::endl;
    //std::cout << "AIC=" << AIC << std::endl;
    for( i = 0; i < m*n*k; i++ )
    {
        CPPUNIT_ASSERT_EQUAL(ABC.data()[i], AIC.data()[i]);
    }

}
void AkronItimesCTest::testAkronItimesCRef()
{
    const int m = 2;
    const int n = 3;
    const int k = 1;
    int i;
    double seq[] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30};
    DoubleMatrix A(m,n);
    DoubleMatrix I = identity(n);
    DoubleMatrix C(n*n,k);

    std::copy(seq, seq+m*n, A.data());
    std::copy(seq, seq+n*n*k, C.data());

    DoubleMatrix AIC;
    AkronItimesC(A,I,C,AIC);
    CPPUNIT_ASSERT_EQUAL(m*n, AIC.nr());
    CPPUNIT_ASSERT_EQUAL(k, AIC.nc());

    DoubleMatrix ABC = AkronBtimesC(A,I,C);
    CPPUNIT_ASSERT_EQUAL(m*n, ABC.nr());
    CPPUNIT_ASSERT_EQUAL(k, ABC.nc());
    //std::cout << "ABC=" << ABC << std::endl;
    //std::cout << "AIC=" << AIC << std::endl;
    for( i = 0; i < m*n*k; i++ )
    {
        CPPUNIT_ASSERT_EQUAL(ABC.data()[i], AIC.data()[i]);
    }

}
void AkronItimesCTest::testAkronItimesCValarray()
{
    const int m = 2;
    const int n = 3;
    const int k = 1;
    int i;
    double seq[] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30};
    valarray<double> A(seq, m * n);
    valarray<double> I(n*n);
    I = 0.0;
    I[ slice(0, n, n+1) ] = 1.0;

    valarray<double> C(seq, n * n * k);

    valarray<double> AIC = AkronItimesC(A, n, I, n, C, k);
    //cout << "AIC = " << DoubleMatrix( AIC, k ) << endl;
    CPPUNIT_ASSERT_EQUAL( m*n*k, static_cast<int>( AIC.size() ) );

    valarray<double> ABC = AkronBtimesC(A, n, I, n, C, k);
    CPPUNIT_ASSERT_EQUAL( m*n*k, static_cast<int>( ABC.size() ) );
    //cout << "ABC = " << DoubleMatrix( ABC, k ) << endl;

    //[ 3.0000000000000000e+001 ]
    //[ 3.6000000000000000e+001 ]
    //[ 4.2000000000000000e+001 ]
    //[ 3.9000000000000000e+001 ]
    //[ 4.8000000000000000e+001 ]
    //[ 5.7000000000000000e+001 ]
    for( i = 0; i < m*n*k; i++ )
    {
        CPPUNIT_ASSERT_EQUAL(ABC[i], AIC[i]);
    }

}
void AkronItimesCTest::testAkronItimesCValarraySpecExample()
{
  int i;
  valarray<double> A( 3 * 2 );
  valarray<double> I( 3 * 3 );
  valarray<double> C( 2 * 3 * 2 );
  valarray<double> D( 9 * 2 );;

  // Let A be a matrix:
  // [ 1 4 ]
  // [ 2 5 ]
  // [ 3 6 ]
  //
  // The column-major representation of A is:
  // [ 1, 2, 3, 4, 5, 6 ]
  // 
  for( i=0; i<3*2; i++ )
	 A[i] = i+1;

  // Let I be a matrix:
  // [ 1 0 0 ]
  // [ 0 1 0 ]
  // [ 0 0 1 ]
  //
  // The column-major representation of I is:
  // [ 1, 0, 0, 0, 1, 0, 0, 0, 1 ]
  //
  I = 0.0;
  I[ slice( 0, 3, 3+1 ) ] = 1.0;

  // Let C be a matrix:
  // [ 1  7 ]
  // [ 2  8 ]
  // [ 3  9 ]
  // [ 4 10 ]
  // [ 5 11 ]
  // [ 6 12 ]
  //
  // The column-major representation of C is:
  // [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ]
  //
  for( i=0; i<2*3*2; i++ )
	 C[i] = i+1;

  //
  // This should yield in a (3 * 3) by 2 matrix:
  // 
  D = AkronItimesC( A, 2, I, 3, C, 2 );

  //  cout << endl << "D = " << endl;
  //printInMatrix( D, 2 );

  //
  // For automated testing
  //
  valarray<double> ABC = AkronBtimesC( A, 2, I, 3, C, 2 );
  //cout << endl << "ABC = " << endl;
  //printInMatrix( ABC, 2 );
  for( i=0; i<ABC.size(); i++ )
  {
    CPPUNIT_ASSERT_EQUAL( ABC[i], D[i] );
  }
}
