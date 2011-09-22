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
 * File: detTest.cpp
 *
 *
 * Unit test for the det function.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <cmath>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/det.h"
#include "detTest.h"

using namespace CppUnit;

void detTest::setUp()
{
    // initializations
}
void detTest::tearDown()
{
    // clean up
}

Test* detTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "detTest" );

    // duplicate the following example for each test case, replacing the case name.

    suiteOfTests->addTest(new TestCaller<detTest>("twoByTwo", &detTest::twoByTwo));
    suiteOfTests->addTest(new TestCaller<detTest>("threeByThree", &detTest::threeByThree));
    suiteOfTests->addTest(new TestCaller<detTest>("twoByTwoValarray", &detTest::twoByTwoValarray));
    suiteOfTests->addTest(new TestCaller<detTest>("threeByThreeValarray", &detTest::threeByThreeValarray));

#ifdef NDEBUG
    //
    // det() function has an assert() statement testing the symmetricity of the matrix.
    // It is, therefore, tripped and terminates the program when the following tests are run
    // in debug mode.  So, run these only when release mode.
    // 
    suiteOfTests->addTest(new TestCaller<detTest>("noSymPosDetTest", &detTest::noSymPosDetTest));
    suiteOfTests->addTest(new TestCaller<detTest>("noSymPosDetValarray", &detTest::noSymPosDetValarray));
#endif

    return suiteOfTests;
}
/*
--------------------------------------------------------------------------------------
                   Begin: DoubleMatrix version
--------------------------------------------------------------------------------------
*/
void detTest::noSymPosDetTest()
{
  // Initialize the matrix A to:
  //
  // A = [ -1.0    0.0  ]
  //     [ -1.0    1.0  ]
  //
  // which is neither symmetric nor positive definite.
  int nARow = 2;
  int nACol = nARow;
  DoubleMatrix A( nARow, nACol );
  double * a = A.data();
  a[0] = -1.0;
  a[1] = -1.0;
  a[2] =  0.0;
  a[3] =  1.0;

  double b;
  long int c;
  try{
    det(A, &b, &c );
  }
  catch( const SpkException& )
  {
    CPPUNIT_ASSERT_MESSAGE( "Good.  It did indeed throw SpkException as expected.", true );
    return;
  }
  CPPUNIT_ASSERT_MESSAGE( "No!  It should have thrown SpkException!.", false );

}
void detTest::twoByTwo()
{
  //***********************************************************
  // Preliminaries.
  //***********************************************************

  using namespace std;

  //***********************************************************
  // Two-by-two test.
  //***********************************************************

  // Populate the matrix A as follows:
  //
  //         /                  \
  //        |  1000000.0   -1.0  |
  //    A = |       -1.0    2.0  |  .
  //         \                  /
  //
  int nARow = 2;
  int nACol= nARow;
  DoubleMatrix dmatA(nARow, nACol);
  double* pdblAData = dmatA.data();
  pdblAData[0 + 0 * nARow] = 1000000.0;
  pdblAData[0 + 1 * nARow] =      -1.0;
  pdblAData[1 + 0 * nARow] =      -1.0;
  pdblAData[1 + 1 * nARow] =       2.0;
  
  // This is the value for the determinant calculated by O-Matrix.
  double omDetAResult = 1.999999000000001e+006;

  // This is an estimate for the precision of the determinant value
  // calculated by O-Matrix.  This estimate was calculated using the
  // following O-Matrix command:
  //
  //    detAPrecision = 10 * max(abs(A)) * cond(A) * DOUBLE_EPSILON
  //
  double detAPrecision = 1.110223579739167e-003;

  doTheTest(dmatA, "A", omDetAResult, detAPrecision);

}
void detTest::threeByThree()
{
  //***********************************************************
  // Three-by-three test.
  //***********************************************************

  // Populate the matrix B as follows:
  //
  //         /                  \
  //        |  11.0  12.0  13.0  |
  //    B = |  12.0  22.0  23.0  |  .
  //        |  13.0  23.0  33.0  |
  //         \                  /
  //
  int nBRow = 3;
  int nBCol= nBRow;
  DoubleMatrix dmatB(nBRow, nBCol);
  double* pdblBData = dmatB.data();
  for (int j = 0; j < nBCol; j++) {
    pdblBData[j + j * nBRow] = (j + 1) * 10 + (j + 1);      // Set B(j, j).
    for (int i = 0; i < j; i++) {
      pdblBData[i + j * nBRow] = (i + 1) * 10 + (j + 1);    // Set B(i, j).
      pdblBData[j + i * nBRow] = pdblBData[i + j * nBRow];  // Set B(j, i).
    }
  }

  // This is the value for the determinant calculated by O-Matrix.
  double omDetBResult = 8.729999999999994e+002;

  // This is an estimate for the precision of the determinant value
  // calculated by O-Matrix.  This estimate was calculated using the
  // following O-Matrix command:
  //
  //    detBPrecision = 10 * max(abs(B)) * cond(B) * DOUBLE_EPSILON
  //
  double detBPrecision = 1.51532375504678e-012;

  doTheTest(dmatB, "B", omDetBResult, detBPrecision);
}


/*************************************************************************
 *
 * Function: doTheTest
 *
 *************************************************************************/

void detTest::doTheTest(const DoubleMatrix& dmatA, char* name, 
                      double knownDet, double knownPrecision)
{
  using namespace std;

  double b;
  long int c;
  det( dmatA , &b, &c );
  double detValue = b * pow( static_cast<long double>(2.0), static_cast<long double>(c) );
 
  CPPUNIT_ASSERT_DOUBLES_EQUAL(knownDet, detValue, knownPrecision);
}
/*
--------------------------------------------------------------------------------------
                   End: DoubleMatrix version
--------------------------------------------------------------------------------------
*/


/*
--------------------------------------------------------------------------------------
                   Begin: valarray version
--------------------------------------------------------------------------------------
*/
using SPK_VA::valarray;
void detTest::noSymPosDetValarray()
{
  // Initialize the matrix A to:
  //
  // A = [ -1.0    0.0  ]
  //     [ -1.0    1.0  ]
  //
  // which is not symmetric nor positive definite.
  int nARow = 2;
  int nACol = nARow;
  valarray<double> A( nARow * nACol );
  A[0] = -1.0;
  A[1] = -1.0;
  A[2] =  0.0;
  A[3] =  1.0;

  double b;
  long int c;
  try{
    det(A, nACol, &b, &c );
  }
  catch( const SpkException& )
  {
    CPPUNIT_ASSERT_MESSAGE(  "Good.  It did indeed throw SpkException as expected.", true );
    return;
  }
  CPPUNIT_ASSERT_MESSAGE( "No!  It should have thrown SpkException!.", false );
}

void detTest::twoByTwoValarray()
{
  //***********************************************************
  // Preliminaries.
  //***********************************************************

  using namespace std;

  //***********************************************************
  // Two-by-two test.
  //***********************************************************

  // Populate the matrix A as follows:
  //
  //         /                  \
  //        |  1000000.0   -1.0  |
  //    A = |       -1.0    2.0  |  .
  //         \                  /
  //
  int nARow = 2;
  int nACol= nARow;
  valarray<double> A(nARow * nACol);
  A[0 + 0 * nARow] = 1000000.0;
  A[0 + 1 * nARow] =      -1.0;
  A[1 + 0 * nARow] =      -1.0;
  A[1 + 1 * nARow] =       2.0;
  
  // This is the value for the determinant calculated by O-Matrix.
  double omDetAResult = 1.999999000000001e+006;

  // This is an estimate for the precision of the determinant value
  // calculated by O-Matrix.  This estimate was calculated using the
  // following O-Matrix command:
  //
  //    detAPrecision = 10 * max(abs(A)) * cond(A) * DOUBLE_EPSILON
  //
  double detAPrecision = 1.110223579739167e-003;

  doTheTest(A, nACol, "A", omDetAResult, detAPrecision);

}
void detTest::threeByThreeValarray()
{
  //***********************************************************
  // Three-by-three test.
  //***********************************************************

  // Populate the matrix B as follows:
  //
  //         /                  \
  //        |  11.0  12.0  13.0  |
  //    B = |  12.0  22.0  23.0  |  .
  //        |  13.0  23.0  33.0  |
  //         \                  /
  //
  int nBRow = 3;
  int nBCol= nBRow;
  valarray<double> B(nBRow * nBCol);
  for (int j = 0; j < nBCol; j++) {
    B[j + j * nBRow] = (j + 1) * 10 + (j + 1);      // Set B(j, j).
    for (int i = 0; i < j; i++) {
      B[i + j * nBRow] = (i + 1) * 10 + (j + 1);    // Set B(i, j).
      B[j + i * nBRow] = B[i + j * nBRow];          // Set B(j, i).
    }
  }

  // This is the value for the determinant calculated by O-Matrix.
  double omDetBResult = 8.729999999999994e+002;

  // This is an estimate for the precision of the determinant value
  // calculated by O-Matrix.  This estimate was calculated using the
  // following O-Matrix command:
  //
  //    detBPrecision = 10 * max(abs(B)) * cond(B) * DOUBLE_EPSILON
  //
  double detBPrecision = 1.51532375504678e-012;

  doTheTest(B, nBCol, "B", omDetBResult, detBPrecision);
}


/*************************************************************************
 *
 * Function: doTheTest
 *
 *************************************************************************/

void detTest::doTheTest(const valarray<double>& A, int n, char* name, 
                      double knownDet, double knownPrecision)
{
  using namespace std;

  double b;
  long c;
  det( A, n, &b, &c );
  double detValue = b * pow( static_cast<long double>(2.0), static_cast<long double>(c) );
 
  CPPUNIT_ASSERT_DOUBLES_EQUAL(knownDet, detValue, knownPrecision);
}

/*
--------------------------------------------------------------------------------------
                   End: valarray version
--------------------------------------------------------------------------------------
*/
