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
 * File: IkronBtimesCTest.cpp
 *
 *
 * Test cases for IkronBtimesCTest
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
#include "../../../spk/IkronBtimesC.h"
#include "../../../spk/symmetrize.h"
#include "../../../spk/identity.h"
#include "../../../spk/transpose.h"
#include "../../../spk/AkronBtimesC.h"
#include "IkronBtimesCTest.h"

using namespace std;
using namespace CppUnit;

void IkronBtimesCTest::setUp()
{
    _rows.push_back(0); _cols.push_back(0);
    _rows.push_back(0); _cols.push_back(1);
    _rows.push_back(1); _cols.push_back(0);
    _rows.push_back(1); _cols.push_back(1);
    _rows.push_back(1); _cols.push_back(3);
    _rows.push_back(3); _cols.push_back(1);
    _rows.push_back(3); _cols.push_back(3);
    _n = _rows.size();

}
void IkronBtimesCTest::tearDown()
{
    // clean up
}

Test* IkronBtimesCTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "IkronBtimesCTest" );

    suiteOfTests->addTest(new TestCaller<IkronBtimesCTest>(
			 "testIkronBtimesC", 
                         &IkronBtimesCTest::testIkronBtimesCVal));
    suiteOfTests->addTest(new TestCaller<IkronBtimesCTest>(
                         "testIkronBtimesC", 
                         &IkronBtimesCTest::testIkronBtimesCRef));
    suiteOfTests->addTest(new TestCaller<IkronBtimesCTest>(
                         "testIkronBtimesCValarray", 
                         &IkronBtimesCTest::testIkronBtimesCValarray));
    suiteOfTests->addTest(new TestCaller<IkronBtimesCTest>(
                         "testIkronBtimesCValarraySpecExample", 
                         &IkronBtimesCTest::testIkronBtimesCValarraySpecExample));

    return suiteOfTests;
}

void IkronBtimesCTest::testIkronBtimesCVal()
{
    using namespace std;

    const int m = 1;
    const int n = m;
    const int x = 2;
    const int y = 3;
    const int k = 4;
    double seq[] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
    31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50};
    DoubleMatrix I = identity(m);
    DoubleMatrix B(x,y);
    DoubleMatrix C(n*y,k);

    std::copy(seq, seq+x*y, B.data());
    std::copy(seq, seq+n*y*k, C.data());

    DoubleMatrix IBC = IkronBtimesC(I,B,C);

    DoubleMatrix ABC = AkronBtimesC(I,B,C);

    for( int i = 0; i < m*n*k; i++ )
    {
        CPPUNIT_ASSERT_EQUAL(ABC.data()[i], IBC.data()[i]);
    }
}
void IkronBtimesCTest::testIkronBtimesCRef()
{
    using namespace std;

    const int m = 1;
    const int n = m;
    const int x = 2;
    const int y = 3;
    const int k = 4;
    double seq[] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
    31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50};
    DoubleMatrix I = identity(m);
    DoubleMatrix B(x,y);
    DoubleMatrix C(n*y,k);

    std::copy(seq, seq+x*y, B.data());
    std::copy(seq, seq+n*y*k, C.data());

    DoubleMatrix IBC;
    IkronBtimesC(I,B,C,IBC);

    DoubleMatrix ABC = AkronBtimesC(I,B,C);

    for( int i = 0; i < m*n*k; i++ )
    {
        CPPUNIT_ASSERT_EQUAL(ABC.data()[i], IBC.data()[i]);
    }
}
void IkronBtimesCTest::testIkronBtimesCValarray()
{
    using namespace std;

    const int m = 1;
    const int n = m;
    const int x = 2;
    const int y = 3;
    const int k = 4;
    double seq[] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
    31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50};
    valarray<double> I(m * m);
    I = 0.0;
    I[ slice( 0, m, m+1 ) ] = 1.0;

    valarray<double> B(seq, x * y);
    valarray<double> C(seq, n * y * k);

    valarray<double> IBC = IkronBtimesC(I, m, B, y, C, k);

    valarray<double> ABC = AkronBtimesC(I, m, B, y, C, k);

    for( int i = 0; i < m*n*k; i++ )
    {
        CPPUNIT_ASSERT_EQUAL(ABC[i], IBC[i]);
    }
}
void IkronBtimesCTest::testIkronBtimesCValarraySpecExample()
    {
      int i;
      valarray<double> I( 3 * 3 );
      valarray<double> B( 3 * 2 );
      valarray<double> C( 3 * 2 * 2 );
      valarray<double> D( 3 * 3 * 2 );

      // Let I be a matrix:
      // [ 1 0 0 ]
      // [ 0 1 0 ]
      // [ 0 0 1 ]
      //
      // The column-major representation of A is:
      // [ 1 0 0 0 1 0 0 0 1 ]
      // 
      I = 0.0;
      I[ slice( 0, 3, 3+1 ) ] = 1.0;

      // Let B be a matrix:
      // [ 1 4 ]
      // [ 2 5 ]
      // [ 3 6 ]
      //
      // The column-major representation of B is:
      // [ 1 2 3 4 5 6 ]
      //
      for( i=0; i< 3 * 2; i++ )
         B[i] = i+1;

      // Let C be a matrix:
      // [ 1  10 ]
      // [ 2  11 ]
      // [ 3  12 ]
      // [ 4  13 ]
      // [ 5  14 ]
      // [ 6  15 ]
      // [ 7  16 ]
      // [ 8  17 ]
      // [ 9  18 ]
      //
      // The column-major representation of C is:
      // [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 ]
      //
      for( i=0; i<3*2*2; i++ )
	     C[i] = i+1;

      //
      // This should yield in a (3 * 3) by 2 matrix:
      // 
      D = IkronBtimesC( I, 3, B, 2, C, 2 );
      valarray<double> ABC = AkronBtimesC( I, 3, B, 2, C, 2 );

      /* Suppress screen output for automated testing
      cout << "D = [ ";
      for( i=0; i < D.size(); i++ )
      {
        cout << D[i] << ", ";
      }
      cout << " ]" << endl;
      */

      for( i=0; i < D.size(); i++ )
      {
        CPPUNIT_ASSERT_EQUAL( ABC[i], D[i] );
      }

    }
