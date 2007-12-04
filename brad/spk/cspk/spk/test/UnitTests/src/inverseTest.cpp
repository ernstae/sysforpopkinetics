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
 * File: inverseTest.cpp
 *
 *
 * Test cases for inverse() function
 *
 * Author: Mitch Watrous & Sachiko Honda
 *
 *************************************************************************/
/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

#include <iostream>
#include <iomanip>
#include <cfloat>
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/inverse.h"
#include "../../../spk/multiply.h"

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include "inverseTest.h"

using SPK_VA::valarray;
using namespace CppUnit;

void inverseTest::setUp()
{
    // initializations
}
void inverseTest::tearDown()
{
    // clean up
}

Test* inverseTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("inverseTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<inverseTest>("twoByTwo", &inverseTest::twoByTwo));
    suiteOfTests->addTest(new TestCaller<inverseTest>("threeByThree", &inverseTest::threeByThree));

    suiteOfTests->addTest(new TestCaller<inverseTest>("twoByTwoValarray", &inverseTest::twoByTwoValarray));
    suiteOfTests->addTest(new TestCaller<inverseTest>("threeByThreeValarray", &inverseTest::threeByThreeValarray));

    //
    // det() function has an assert() statement testing the symmetricity of the matrix.
    // It is, therefore, tripped and terminates the program when the following tests are run
    // in debug mode.  So, run these only when release mode.
    // 
    suiteOfTests->addTest(new TestCaller<inverseTest>("noSymPosDetTest", &inverseTest::noSymPosDetTest));
    suiteOfTests->addTest(new TestCaller<inverseTest>("noSymPosDetValarray", &inverseTest::noSymPosDetValarray));
    return suiteOfTests;
}

/*
--------------------------------------------------------------------------------------------
              DoubleMatrix version
--------------------------------------------------------------------------------------------
*/
void inverseTest::noSymPosDetTest()
{
  // Initialize the matrix A to:
  //
  //    A = [ -1.0   0.0  ]
  //        [  1.0   1.0  ]
  // which is neither symmetric nor positive definite.
  // 
  int nARow = 2;
  int nACol = nARow;
  DoubleMatrix dmatA( nARow, nACol );
  double * A = dmatA.data();
  A[0] = -1.0;
  A[1] =  1.0;
  A[2] =  0.0;
  A[3] =  1.0;

  try{
    DoubleMatrix dmatAinv = inverse( dmatA );
  }
  catch( const SpkException& )
  {
    CPPUNIT_ASSERT_MESSAGE( "Good.  It did indeed throw SpkException as expected.", true);
    return;
  }
  CPPUNIT_ASSERT_MESSAGE( "No.  It should have thrown SpkException!", false );
}
void inverseTest::twoByTwo()
{
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
  
  // This is the value for the condition number of matrix A,
  // calculated using the O-Matrix cond function.
  double dblACond = 5.000002500011250e+005;

  doTheTest(dmatA, "A", dblACond);

}
void inverseTest::threeByThree()
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

  // This is the value for the condition number of matrix B,
  // calculated using the O-Matrix cond function.
  double dblBCond = 2.068003484416479e+001;

  doTheTest(dmatB, "B", dblBCond);
}

void inverseTest::twoByTwoValarray()
{
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
  valarray<double> a (nARow * nACol);
  a[0 + 0 * nARow] = 1000000.0;
  a[0 + 1 * nARow] =      -1.0;
  a[1 + 0 * nARow] =      -1.0;
  a[1 + 1 * nARow] =       2.0;
  
  // This is the value for the condition number of matrix A,
  // calculated using the O-Matrix cond function.
  double dblACond = 5.000002500011250e+005;

  doTheTest(DoubleMatrix( a, nACol) , "A", dblACond);

}
/*************************************************************************
 *
 * Function: doTheTest
 *
 *
 * Arguments
 * ---------
 *
 * dmatA        is the DoubleMatrix to be inverted.
 *
 * cAName       is the character string name for dmatA.
 *
 * dblACond     is the condition number of dmatA.
 *
 *************************************************************************/

void inverseTest::doTheTest(const DoubleMatrix& dmatA, const char* cAName, 
                 const double dblACond)
{
  using namespace std;

  int nARow = dmatA.nr();
  int nACol = dmatA.nc();

  DoubleMatrix dmatAInv       = inverse(dmatA);
  DoubleMatrix dmatATimesAInv = multiply(dmatA, dmatAInv);

  // Create an identity matrix with the same dimensions as A.
  DoubleMatrix dmatI = DoubleMatrix(nARow, nACol);
  double* pdblIData = dmatI.data();
  int i, j;
  for (j = 0; j < nACol; j++) {
    for (i = 0; i < nARow; i++) {
      pdblIData[i * nARow + j] = 0.0;
    }
    pdblIData[j * nARow + j] = 1.0;
  }

  // Create a scale matrix for the product of A and inverse(A).
  // Set all of its elements equal to the condition number of A.
  DoubleMatrix dmatScale(dmatA);
  double* pdblScaleData = dmatScale.data();
  for (i = 0; i < nARow * nACol; i++) {
    pdblScaleData[i] = dblACond;
  }
  
  // Compare A * inverse(A) with the identity matrix I.
  for( i=0; i<nARow*nACol; i++ )
  {
      CPPUNIT_ASSERT_DOUBLES_EQUAL(dmatATimesAInv.data()[i], dmatI.data()[i], dmatScale.data()[i]);
  }

  // This is an extra test to verify that the function isDmatEpsEqual
  // will detect the case where one of the off-diagonal elements of 
  // A * inverse(A), which should be equal to zero, is greater than
  // what is allowed by the function isDblEpsEqual.
  double* pdblATimesAInvData = dmatATimesAInv.data();
  pdblATimesAInvData[0 * nARow + nACol] = 2.0 * dblACond * 
    DBL_EPS_EQUAL_MULT * DBL_EPSILON;
  for( i=0; i<nARow*nACol; i++ )
  {
    CPPUNIT_ASSERT_DOUBLES_EQUAL(dmatATimesAInv.data()[i], dmatI.data()[i], dmatScale.data()[i]);
  }

}
/*
--------------------------------------------------------------------------------------------
              valarray version
--------------------------------------------------------------------------------------------
*/

void inverseTest::noSymPosDetValarray()
{
  // Initialize the matrix A to:
  //
  //    A = [ -1.0   0.0  ]
  //        [  1.0   1.0  ]
  // which is neither symmetric nor positive definite.
  // 
  int nARow = 2;
  int nACol = nARow;
  valarray<double> A( nARow * nACol );
  A[0] = -1.0;
  A[1] =  1.0;
  A[2] =  0.0;
  A[3] =  1.0;

  try{
    valarray<double> Ainv = inverse( A, nACol );
  }
  catch( const SpkException& )
  {
    CPPUNIT_ASSERT_MESSAGE("Good.  It did indeed throw SpkException as expected.", true);
    return;
  }
  CPPUNIT_ASSERT_MESSAGE("No.  It should have thrown SpkException!", false );
}

void inverseTest::threeByThreeValarray()
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
  valarray<double> b(nBRow * nBCol);
  for (int j = 0; j < nBCol; j++) {
    b[j + j * nBRow] = (j + 1) * 10 + (j + 1);      // Set B(j, j).
    for (int i = 0; i < j; i++) {
      b[i + j * nBRow] = (i + 1) * 10 + (j + 1);    // Set B(i, j).
      b[j + i * nBRow] = b[i + j * nBRow];  // Set B(j, i).
    }
  }

  // This is the value for the condition number of matrix B,
  // calculated using the O-Matrix cond function.
  double dblBCond = 2.068003484416479e+001;

  doTheTest(DoubleMatrix( b, nBCol ), "B", dblBCond);
}
