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
/*
 * AkronBtimesC test driver
 *
 * Compares the results from AkronBtimesC(A,B,C) against 
 * (A kron B)*C.  Terminates the test if a difference between
 * two elements, one from each equation, exceeds a certain
 * limit.
 *
 * Author: Sachiko Honda
 */

#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include <cstdlib>
#include <ctime>
#include <cassert>
#include <limits.h>
#include <math.h>
#include <float.h>

#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/AkronBtimesC.h"

#include "AkronBtimesCTest.h"
#include "../../../spk/multiply.h"
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"
#include "../../../spk/SpkValarray.h"

using SPK_VA::valarray;
using namespace std;
using namespace CppUnit;

void AkronBtimesCTest::setUp()
{
    _rows.push_back(0);  _cols.push_back(0);
    _rows.push_back(0);  _cols.push_back(1);
    _rows.push_back(1);  _cols.push_back(0);
    _rows.push_back(1);  _cols.push_back(1);
    _rows.push_back(1);  _cols.push_back(3);
    _rows.push_back(3);  _cols.push_back(1);
    _rows.push_back(3);  _cols.push_back(3);

}
void AkronBtimesCTest::tearDown()
{
    // clean up
}

Test* AkronBtimesCTest::suite()
{

  TestSuite *suiteOfTests = new TestSuite("AkronBtimesCTest");

  // duplicate the following example for each test case, replacing the case name.
  suiteOfTests->addTest(new TestCaller<AkronBtimesCTest>(
                       "testReturnByValue",
                       &AkronBtimesCTest::testReturnByValue));
  suiteOfTests->addTest(new TestCaller<AkronBtimesCTest>(
                       "testReturnThroughReference", 
                       &AkronBtimesCTest::testReturnThroughReference));

  suiteOfTests->addTest(new TestCaller<AkronBtimesCTest>(
                       "testValarraySpecExample", 
                       &AkronBtimesCTest::testValarraySpecExample));
 
  suiteOfTests->addTest(new TestCaller<AkronBtimesCTest>(
                       "testValarray",
                       &AkronBtimesCTest::testValarray));

  return suiteOfTests;
}

void AkronBtimesCTest::testReturnByValue()
{
  CPPUNIT_ASSERT(_rows.size()==_cols.size());
  
  const int MAX = _rows.size();
  int       iARows, iACols, iBRows, iBCols, iCRows, iCCols;
  double    *pdA, *pdB, *pdC;
  
  for(int k=0; k<1; k++)
    {
      for(int j=0; j<1; j++)
        {
	  for(int i=0; i<MAX; i++)
	    {
	      srand( 54321 );	// Always the same for repeatability
	      iARows = _rows[i];
	      iACols = _cols[i];
	      iBRows = _rows[i];
	      iBCols = _cols[i];
	      iCRows = iACols * iBCols;
	      iCCols = _cols[i];
	      
	      DoubleMatrix dmatA( iARows, iACols );
	      DoubleMatrix dmatB( iBRows, iBCols );
	      DoubleMatrix dmatC( iCRows, iCCols );
	      DoubleMatrix dmatD, dmatKronD;

	      pdA = dmatA.data();
	      pdB = dmatB.data();
	      pdC = dmatC.data();
	      int x;
	      for( x=0; x<iARows*iACols; x++ )
		pdA[x] = (double)rand() / 10000.0;
	      
	      for( x=0; x<iBRows*iBCols; x++ )
		pdB[x] = (double)rand() / 10000.0;
	      
	      for( x=0; x<iCRows*iCCols; x++ )
		pdC[x] = (double)rand() / 10000.0;
	      
	      dmatD     = AkronBtimesC( dmatA, dmatB, dmatC );
	      dmatKronD = multiply(kron(dmatA,dmatB), dmatC);
	      for( x=0; x<dmatD.nr()*dmatD.nc(); x++)
                {
		  double expected = dmatKronD.data()[x];
		  double actual   = dmatD.data()[x];
		  CPPUNIT_ASSERT(
				 fabs(actual-expected)/(expected) <= 0.001 );
                }
	    }
        }
    }
  return;
}

void AkronBtimesCTest::testReturnThroughReference()
{
  CPPUNIT_ASSERT(_rows.size()==_cols.size());
  
  const int MAX = _rows.size();
  int       iARows, iACols, iBRows, iBCols, iCRows, iCCols;
  double    *pdA, *pdB, *pdC;
  
  for(int k=0; k<1; k++)
    {
      for(int j=0; j<1; j++)
        {
            for(int i=0; i<MAX; i++)
	      {
                srand( 54321 );	// Always the same for repeatability
                iARows = _rows[k];
                iACols = _cols[k];
                iBRows = _rows[j];
                iBCols = _cols[j];
                iCRows = iACols * iBCols;
                iCCols = _cols[i];
		
                DoubleMatrix dmatA( iARows, iACols );
                DoubleMatrix dmatB( iBRows, iBCols );
                DoubleMatrix dmatC( iCRows, iCCols );
                DoubleMatrix dmatD, dmatKronD;
		
                pdA = dmatA.data();
                pdB = dmatB.data();
                pdC = dmatC.data();
                int x;
                for( x=0; x<iARows*iACols; x++ )
		  pdA[x] = (double)rand() / 10000.0;
		
                for( x=0; x<iBRows*iBCols; x++ )
		  pdB[x] = (double)rand() / 10000.0;
		
                for( x=0; x<iCRows*iCCols; x++ )
		  pdC[x] = (double)rand() / 10000.0;
		
                AkronBtimesC( dmatA, dmatB, dmatC, dmatD );
                dmatKronD = multiply(kron(dmatA,dmatB), dmatC);
                for( x=0; x<dmatD.nr()*dmatD.nc(); x++)
		  {
		    double expected = dmatKronD.data()[x];
		    double actual   = dmatD.data()[x];
                    CPPUNIT_ASSERT(fabs(actual-expected)/expected <= 0.001 );
		  }
	      }
        }
    }
  return;
  
}
/*
 * C = A kron B
 */

const DoubleMatrix AkronBtimesCTest::kron( const DoubleMatrix &dmatA, const DoubleMatrix &dmatB )
{
    int    iARows  = dmatA.nr();
    int    iACols  = dmatA.nc();
    int    iA, jA;
    const double *pdA    = dmatA.data();
    double aij;

    int    iBRows   = dmatB.nr();
    int    iBCols   = dmatB.nc();
    int    iB, jB;
    const double *pdB = dmatB.data();
    double bij;

    CPPUNIT_ASSERT_MESSAGE( "iARows*iACols < INT_MAX", iARows*iACols < INT_MAX );
    int iCRows   = iARows * iBRows;
    int iCCols   = iACols * iBCols;
    int iC, jC;
    DoubleMatrix dmatC(iCRows, iCCols);
    double *pdC = dmatC.data();


    // for each element of A
    for( iA = 0; iA < iARows; iA++ )
    {   
        for( jA = 0; jA < iACols; jA++ )
        {
            // A[iA, jA]
            aij = pdA[iA + jA * iARows];

            // for each element of B
            for( iB = 0; iB < iBRows;iB++ )
            {
                for( jB = 0; jB < iBCols; jB++ )
                {
                    // B[iB, jB]
                    bij = pdB[iB + jB * iBRows];

                    // corresponding element of C[iC,jC]
                    iC = iB + iA * iBRows;
                    jC = jB + jA * iBCols;

                    pdC[iC + jC * iCRows] = aij * bij;

                }
            }
        }
    }
    return dmatC;
}
void AkronBtimesCTest::testValarray()
{
  CPPUNIT_ASSERT(_rows.size()==_cols.size());
  
  const int MAX = _rows.size();
  int       iARows, iACols, iBRows, iBCols, iCRows, iCCols, iDRows, iDCols;
  
  for(int k=0; k<1; k++)
    {
      for(int j=0; j<1; j++)
        {
	  for(int i=0; i<MAX; i++)
	    {
	      srand( 54321 );	// Always the same for repeatability
	      iARows = _rows[i];
	      iACols = _cols[i];
	      iBRows = _rows[i];
	      iBCols = _cols[i];
	      iCRows = iACols * iBCols;
	      iCCols = _cols[i];
	      iDRows = iARows * iBRows;
	      iDCols = iCCols;
	      
	      valarray<double> A( iARows * iACols );
	      valarray<double> B( iBRows * iBCols );
	      valarray<double> C( iCRows * iCCols );
	      valarray<double> D( iDRows * iDCols );
              valarray<double> AkronB( iARows * iBRows * iACols * iBCols );
	      valarray<double> AkBC( iDRows * iDCols );

	      int x;
	      for( x=0; x<iARows*iACols; x++ )
		A[x] = (double)rand() / 10000.0;
	      
	      for( x=0; x<iBRows*iBCols; x++ )
		B[x] = (double)rand() / 10000.0;
	      
	      for( x=0; x<iCRows*iCCols; x++ )
		C[x] = (double)rand() / 10000.0;
	      
              AkronB = kron(A, iACols, B, iBCols);
	      AkBC   = multiply( AkronB, iACols * iBCols, C, iCCols);
	      //cout << "expected: " << DoubleMatrix( KronD, iDCols ) << endl;
	      
	      D     = AkronBtimesC( A, iACols, B, iBCols, C, iCCols );
	      //cout << "actual:   " << DoubleMatrix( D, iDCols ) << endl;
	      
	      for( x = 0; x < iDRows * iDCols; x++)
		  {
		    double expected = AkBC[x];
		    double actual   = D[x];
		    CPPUNIT_ASSERT( fabs(actual-expected)/expected <= 0.001 );
		  }
	    }
        }
    }
  return;
}
void AkronBtimesCTest::testValarraySpecExample()
{
  int i;
  valarray<double> A( 3 * 2 );
  valarray<double> B( 2 * 3 );
  valarray<double> C( 2 * 3 * 2 );
  valarray<double> D( 6 * 2 );

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

  // Let B be a matrix:
  // [ 1 3 5 ]
  // [ 2 4 6 ]
  //
  // The column-major representation of B is:
  // [ 1, 2, 3, 4, 5, 6 ]
  //
  for( i=0; i<2*3; i++ )
	 B[i] = i+1;

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
  // This should yield in a (3 * 2) by 2 matrix:
  // [ 218  488  ]
  // [ 284  644  ]
  // [ 289  667  ]
  // [ 376  880  ]
  // [ 360  846  ]
  // [ 468  1116 ]
  // 
  D = AkronBtimesC( A, 2, B, 3, C, 2 );

  /*
  cout << endl << "D = [ ";
  for( i=0; i < D.size(); i++ )
  {
    cout << D[i] << ", ";
  }
  cout << " ]" << endl;
  */

  /*
    This section is added for automated testing.
  */
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 218, D[0], 1e-13 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 284, D[1], 1e-13 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 289, D[2], 1e-13 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 376, D[3], 1e-13 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 360, D[4], 1e-13 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 468, D[5], 1e-13 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 488, D[6], 1e-13 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 644, D[7], 1e-13 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 667, D[8], 1e-13 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 880, D[9], 1e-13 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 846, D[10], 1e-13 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( 1116, D[11], 1e-13 );
}


const valarray<double> AkronBtimesCTest::kron( const valarray<double> &A, int iACols,
                                               const valarray<double> &B, int iBCols )
{
    int    iARows  = ( iACols==0? 0 : A.size() / iACols );
    CPPUNIT_ASSERT( iARows * iACols == A.size() );
    int    iA, jA;
    double aij;

    int    iBRows   = (iBCols==0? 0 : B.size() / iBCols );
    CPPUNIT_ASSERT( iBRows * iBCols == B.size() );
    int    iB, jB;
    double bij;

    CPPUNIT_ASSERT_MESSAGE( "iARows*iACols < INT_MAX", iARows*iACols < INT_MAX );
    int iCRows   = iARows * iBRows;
    int iCCols   = iACols * iBCols;
    int iC, jC;
    valarray<double> C(iCRows * iCCols);


    // for each element of A
    for( iA = 0; iA < iARows; iA++ )
    {   
        for( jA = 0; jA < iACols; jA++ )
        {
            // A[iA, jA]
            aij = A[iA + jA * iARows];

            // for each element of B
            for( iB = 0; iB < iBRows;iB++ )
            {
                for( jB = 0; jB < iBCols; jB++ )
                {
                    // B[iB, jB]
                    bij = B[iB + jB * iBRows];

                    // corresponding element of C[iC,jC]
                    iC = iB + iA * iBRows;
                    jC = jB + jA * iBCols;

                    C[iC + jC * iCRows] = aij * bij;

                }
            }
        }
    }
    return C;
}
