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
 * File: transposeTest.cpp
 *
 *
 * Test cases for tranpose()
 *
 * Author: sachiko honda
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/transpose.h"
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/SpkValarray.h"
#include "transposeTest.h"

using SPK_VA::valarray;
using namespace CppUnit;

void transposeTest::setUp()
{
    _rows.push_back(0);  _cols.push_back(0);
    _rows.push_back(0);  _cols.push_back(1);
    _rows.push_back(1);  _cols.push_back(0);
    _rows.push_back(1);  _cols.push_back(1);
    _rows.push_back(1);  _cols.push_back(3);
    _rows.push_back(3);  _cols.push_back(1);
    _rows.push_back(3);  _cols.push_back(3);

}
void transposeTest::tearDown()
{
    // clean up
}

Test* transposeTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("transposeTest");

  // duplicate the following example for each test case, replacing the case name.
  suiteOfTests->addTest(new TestCaller<transposeTest>("nonselfTranspose",  &transposeTest::nonselfTranspose));
  suiteOfTests->addTest(new TestCaller<transposeTest>("refTranspose",      &transposeTest::refTranspose));
  suiteOfTests->addTest(new TestCaller<transposeTest>("valarrayTranspose", &transposeTest::valarrayTranspose));
  
  return suiteOfTests;
}
void transposeTest::nonselfTranspose()
{
  using namespace std;

  assert(_rows.size()==_cols.size());
  int max = _rows.size();
  int m,n;
  int i,k;
  /*
   * A^T^T = A 
   */
  for( i=0; i<max; i++ )
    {
      m = _rows[i];
      n = _cols[i];
      DoubleMatrix A(m,n);
      for( k=0; k<m*n; k++ )
        {
            A.data()[k] = rand()/10000+.1;
        }
      DoubleMatrix B = transpose(transpose(A));
      // A and B must be exactly equal.
      for( k=0; k<m*n; k++ )
        {
	  CPPUNIT_ASSERT_EQUAL(A.data()[k], B.data()[k]);
        }
      CPPUNIT_ASSERT_EQUAL(A.nc(), B.nc());
      CPPUNIT_ASSERT_EQUAL(A.nr(), B.nr());
    }
  
  return;
}

void transposeTest::refTranspose()
{
  using namespace std;
	
    assert(_rows.size()==_cols.size());
    int max = _rows.size();
    int m,n;
    int i,k;

    /*
     * A^T^T = A 
     */
    for( i=0; i<max; i++ )
    {
        m = _rows[i];
        n = _cols[i];
        DoubleMatrix A(m,n);
        
        for( k=0; k<m*n; k++ )
        {
            A.data()[k] = rand()/10000+.1;
        }
        DoubleMatrix At, Att;
        transpose(A,At);
        transpose(At,Att);

        CPPUNIT_ASSERT_EQUAL(A.nc(), Att.nc());
        CPPUNIT_ASSERT_EQUAL(A.nr(), Att.nr());

        // A and B must be exactly equal.
        for( k=0; k<m*n; k++ )
        {
            CPPUNIT_ASSERT_EQUAL(A.data()[k], Att.data()[k]);
        }
    }

	return;
}
void transposeTest::valarrayTranspose()
{
	using namespace std;

    assert(_rows.size()==_cols.size());
    int max = _rows.size();
    int m,n;
    int i,k;
    //
    // A^T^T = A 
    //
    for( i=0; i<max; i++ )
    {
        m = _rows[i];
        n = _cols[i];
        if( m * n > 0 )
        {
          valarray<double> A(m * n);
          for( k=0; k<m*n; k++ )
          {
              A[k] = rand()/10000+.1;
          }
          valarray<double> B = transpose(transpose(A, n), m);
          // A and B must be exactly equal.
          for( k=0; k<m*n; k++ )
          {
              CPPUNIT_ASSERT_EQUAL(A[k], B[k]);
          }
          CPPUNIT_ASSERT_EQUAL(A.size(), B.size());
        }
    }
	return;
}
