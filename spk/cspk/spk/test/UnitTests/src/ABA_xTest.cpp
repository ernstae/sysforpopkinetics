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
 * File: ABA_x.cpp
 *
 *
 * Test cases for ABA_x
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
#include "../../../spk/ABA_x.h"
#include "ABA_xTest.h"

using namespace CppUnit;

void ABA_xTest::setUp()
{
    // initializations
}
void ABA_xTest::tearDown()
{
    // clean up
}

Test* ABA_xTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "ABA_xTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<ABA_xTest>("testABA_x", &ABA_xTest::testABA_x));
    suiteOfTests->addTest(new TestCaller<ABA_xTest>("testABA_xValarray", &ABA_xTest::testABA_xValarray));
    return suiteOfTests;
}

void ABA_xTest::testABA_x()
{
    using namespace std;

    const double x[] = {1.0, 2.0};

    //
    // Set A to:
    //   [ 0     x[1] ]
    //   [ x[0]  1    ]
    //
    DoubleMatrix A(2,2);
    A.data()[0] = 0;
    A.data()[1] = x[0];
    A.data()[2] = x[1];
    A.data()[3] = 1.0;

    //
    // Set B to:
    //
    //   [ 1     x[0]      ]
    //   [ x[1]  x[1]*x[1] ]
    //
    DoubleMatrix B(2,2);
    B.data()[0] = 1.0;
    B.data()[1] = x[1];
    B.data()[2] = x[0];
    B.data()[3] = x[1]*x[1];

    //
    // Therefore A_x is:
    //
    //    [ 0  0 ]
    //    [ 0  1 ]
    //    [ 1  0 ]
    //    [ 0  0 ]
    //
    DoubleMatrix A_x(4,2);
    A_x.fill(0);
    A_x.data()[2] = 1.0;
    A_x.data()[5] = 1.0;

    //
    // Therefore B_x is:
    //
    //    [ 0      0  ]
    //    [ 1      0  ]
    //    [ 0      1  ]
    //    [ 0  2*x[1] ]
    //
    DoubleMatrix B_x(4,2);
    B_x.fill(0);
    B_x.data()[1] = 1.0;
    B_x.data()[6] = 1.0;
    B_x.data()[7] = 2.0*x[1];

    //
    // C=ABA_x should be:
    //
    //   [ 2 * x[0] * x[1]^2              2 * x[0]^2 * x[1]         ]
    //   [ 2 * x[1]^2                     4 * x[0] * x[1]           ]
    //   [ 2 * x[0] * x[1] + x[1]^2       x[0]^2 + 2 * x[0] * x[1]  ]
    //   [ x[1]                           6 * x[1] + x[0]           ]
    //
    DoubleMatrix expectedC(4,2);
    expectedC.data()[0] = 2.0 * x[0] * x[1]*x[1];
    expectedC.data()[1] = 2.0 * x[1]*x[1];
    expectedC.data()[2] = 2.0 * x[0] * x[1] + x[1]*x[1];
    expectedC.data()[3] = x[1];
    expectedC.data()[4] = 2.0 * x[0]*x[0] * x[1];
    expectedC.data()[5] = 4.0 * x[0] * x[1];
    expectedC.data()[6] = x[0]*x[0] + 2.0 * x[0] * x[1];
    expectedC.data()[7] = 6.0 * x[1] + x[0];

    /*
    At_x= {
    [ 0 , 0 ]
    [ 1 , 0 ]
    [ 0 , 1 ]
    [ 0 , 0 ]
    }

    AkronBtimesC(At * B, Ia, A_x)= {
    [ 4 , 0 ]
    [ 0 , 2 ]
    [ 6 , 0 ]
    [ 0 , 4 ]
    }

    AkronBtimesC(At, At, B_x)= {
    [ 0 , 4 ]
    [ 0 , 6 ]
    [ 2 , 4 ]
    [ 2 , 6 ]
    }

    AkronBtimesC(Ia, At * B', At_x)= {
    [ 4 , 0 ]
    [ 8 , 0 ]
    [ 0 , 1 ]
    [ 0 , 3 ]
    }

    C = {
    {
    [ 8 , 4 ]
    [ 8 , 8 ]
    [ 8 , 5 ]
    [ 2 , 13 ]
    }
    */
    DoubleMatrix C = ABA_x(A, B, A_x, B_x);

    for( int i=0; i<8; i++ )
    {
        CPPUNIT_ASSERT_EQUAL(expectedC.data()[i], C.data()[i] );
    }

}
void ABA_xTest::testABA_xValarray()
{
  using SPK_VA::valarray;
  using std::cout;
  using std::endl;

  const int nX = 2;
  const int nRowsA = 2;
  const int nColsA = 2;
  const int nRowsB = 2;
  const int nColsB = 2;

  valarray<double> x(nX);
  x[0] = 1.0;
  x[1] = 2.0;

  //
  // Set A to:
  //   [ 0     x[1] ]
  //   [ x[0]  1    ]
  //
  valarray<double> A( nRowsA * nColsA );
  A[0] = 0;
  A[1] = x[0];
  A[2] = x[1];
  A[3] = 1.0;

  //
  // Set B to:
  //
  //   [ 1     x[0]      ]
  //   [ x[1]  x[1]*x[1] ]
  //
  valarray<double> B( nRowsB * nColsB );
  B[0] = 1.0;
  B[1] = x[1];
  B[2] = x[0];
  B[3] = x[1] * x[1];

  //
  // Therefore A_x is:
  //
  //    [ 0  0 ]
  //    [ 0  1 ]
  //    [ 1  0 ]
  //    [ 0  0 ]
  //
  valarray<double> A_x( (nRowsA * nColsA) * nX );
  A_x = 0.0;
  A_x[2] = 1.0;
  A_x[5] = 1.0;

  //
  // Therefore B_x is:
  //
  //    [ 0      0  ]
  //    [ 1      0  ]
  //    [ 0      1  ]
  //    [ 0  2*x[1] ]
  //
  valarray<double> B_x( (nRowsB * nColsB) * nX );
  B_x = 0.0;
  B_x[1] = 1.0;
  B_x[6] = 1.0;
  B_x[7] = 2.0 * x[1];

  //
  // C=ABA_x should be:
  //
  //   [ 2 * x[0] * x[1]^2              2 * x[0]^2 * x[1]         ]
  //   [ 2 * x[1]^2                     4 * x[0] * x[1]           ]
  //   [ 2 * x[0] * x[1] + x[1]^2       x[0]^2 + 2 * x[0] * x[1]  ]
  //   [ x[1]                           6 * x[1] + x[0]           ]
  //
  valarray<double> expectedC( nRowsA * nRowsB * nX );
  expectedC[0] = 2.0 * x[0] * x[1] * x[1];
  expectedC[1] = 2.0 * x[1] * x[1];
  expectedC[2] = 2.0 * x[0] * x[1] + x[1] * x[1];
  expectedC[3] = x[1];
  expectedC[4] = 2.0 * x[0] * x[0] * x[1];
  expectedC[5] = 4.0 * x[0] * x[1];
  expectedC[6] = x[0] * x[0] + 2.0 * x[0] * x[1];
  expectedC[7] = 6.0 * x[1] + x[0];

  /*
  At_x= {
  [ 0 , 0 ]
  [ 1 , 0 ]
  [ 0 , 1 ]
  [ 0 , 0 ]
  }

  AkronBtimesC(At * B, Ia, A_x)= {
  [ 4 , 0 ]
  [ 0 , 2 ]
  [ 6 , 0 ]
  [ 0 , 4 ]
  }

  AkronBtimesC(At, At, B_x)= {
  [ 0 , 4 ]
  [ 0 , 6 ]
  [ 2 , 4 ]
  [ 2 , 6 ]
  }

  AkronBtimesC(Ia, At * B', At_x)= {
  [ 4 , 0 ]
  [ 8 , 0 ]
  [ 0 , 1 ]
  [ 0 , 3 ]
  }

  C = {
  {
  [ 8 , 4 ]
  [ 8 , 8 ]
  [ 8 , 5 ]
  [ 2 , 13 ]
  }
  */
  valarray<double> C = ABA_x(A, nColsA, B, nColsB, A_x, B_x, nX);

  for( int i=0; i<8; i++ )
  {
      CPPUNIT_ASSERT_EQUAL(expectedC[i], C[i] );
  }

}


