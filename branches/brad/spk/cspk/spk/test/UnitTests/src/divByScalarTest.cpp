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
 * File: divByScalarTest.cpp
 *
 *
 * Test cases for a element-wise division of a matrix.
 *
 * Author: <AUTHOR_NAME>
 *
 *************************************************************************/
#pragma warning ( disable : 4786 )

#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestFixture.h>
#include <cppunit/TestCaller.h>
#include <cppunit/TestSuite.h>

#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/divByScalar.h"
#include "../../../spk/isDblEpsEqual.h"
#include "divByScalarTest.h"

#include <ctime>
#include <cmath>
#include <vector>
#include "../../../spk/DBL_EPS_EQUAL_MULT.h"

using namespace CppUnit;

void divByScalarTest::setUp()
{
  _rows.push_back(0); _cols.push_back(0);
  _rows.push_back(0); _cols.push_back(1);
  _rows.push_back(1); _cols.push_back(0);
  _rows.push_back(1); _cols.push_back(1);
  _rows.push_back(3); _cols.push_back(1);
  _rows.push_back(1); _cols.push_back(3);
  
  _n=_rows.size();
  
  double *pdA;
  DoubleMatrix *A;
  
  // seed the random number generator
  srand( time(0) );
  _scalar = 0.0;
  do{
    _scalar = rand() / 1000.0;
  }while( _scalar == 0.0 );
  
  for( int i=0; i<_n; i++ )
    {
      A  = new DoubleMatrix( _rows[i], _cols[i] );
      pdA = A->data();
      for( int j=0; j<_rows[i]*_cols[i]; j++ ){
	pdA[j] = rand() / 10000.0;
      }
      
      _As.push_back(A);
    }
}
void divByScalarTest::tearDown()
{
  for( int i=0; i<_n; i++ )
    {
      delete _As[i];
    }
}

Test* divByScalarTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite ( "divByScalarTest" );
  
  // duplicate the following example for each test case, replacing the case name.
  suiteOfTests->addTest(new TestCaller<divByScalarTest>("testMatScalarVal", &divByScalarTest::testMatScalarVal));
  suiteOfTests->addTest(new TestCaller<divByScalarTest>("testMatMatVal",    &divByScalarTest::testMatMatVal));
  suiteOfTests->addTest(new TestCaller<divByScalarTest>("testMatScalarVal", &divByScalarTest::testMatScalarRef));
  suiteOfTests->addTest(new TestCaller<divByScalarTest>("testMatMatVal",    &divByScalarTest::testMatMatRef));
  
  return suiteOfTests;
}

void divByScalarTest::testMatScalarVal()
{
  // Testing division by a scalar
  for( int i=0; i<_n; i++ )
    {
      // Compute B = A / scalar;
      DoubleMatrix dmatB = divByScalar(*_As[i], _scalar);
      double* pdB  = dmatB.data();
      double* pdA  = _As[i]->data();
      
      // Compute A = dmatAA = B * scalar
      for(int j=0; j<_rows[i]*_cols[i]; j++ )
	{
	  double actual   = pdA[j];
	  double expected = pdB[j] * _scalar;
	  double scale;

	  if ( expected != 0.0 )
	  {
	    scale = expected;
	  }
	  else
	  {
	    scale = actual;
	  }

	  CPPUNIT_ASSERT( isDblEpsEqual( actual, expected, scale ) );
	}
    }
  
}
void divByScalarTest::testMatMatVal()
{
  // Testing elementwise division by another matrix
  for( int i=0; i<_n; i++ )
    {
      // Compute B = A / scalar;
      
      DoubleMatrix Scalars(_As[i]->nr(), _As[i]->nc());
      DoubleMatrix dmatB(_As[i]->nr(), _As[i]->nc());
      
      double* pdScalars = Scalars.data();
      double* pdA  = _As[i]->data();
      
      Scalars.fill(_scalar);
      
      dmatB = divByScalar(*_As[i], Scalars);
      double* pdB  = dmatB.data();
      
      // Compute A = dmatAA = B * scalar
      for(int j=0; j<_rows[i]*_cols[i]; j++ )
        {
	  double actual   = pdA[j];
	  double expected = pdB[j]*pdScalars[j];
	  double scale;

	  if ( expected != 0.0 )
	  {
	    scale = expected;
	  }
	  else
	  {
	    scale = actual;
	  }

	  CPPUNIT_ASSERT( isDblEpsEqual( actual, expected, scale ) );
	}
    }
  
}
void divByScalarTest::testMatScalarRef()
{
  // Testing division by a scalar
  for( int i=0; i<_n; i++ )
    {
      // Compute B = A / scalar;
      DoubleMatrix dmatB(_As[i]->nr(), _As[i]->nc());
      divByScalar(*_As[i], _scalar, dmatB);
      double* pdB  = dmatB.data();
      double* pdA  = _As[i]->data();
      
      
      // Compute A = dmatAA = B * scalar
      for(int j=0; j<_rows[i]*_cols[i]; j++ )
        {
	  double actual   = pdA[j];
	  double expected = pdB[j]*_scalar;
	  double scale;

	  if ( expected != 0.0 )
	  {
	    scale = expected;
	  }
	  else
	  {
	    scale = actual;
	  }

	  CPPUNIT_ASSERT( isDblEpsEqual( actual, expected, scale ) );
	}
    }
  
}
void divByScalarTest::testMatMatRef()
{
  // Testing elementwise division by another matrix
  for( int i=0; i<_n; i++ )
    {
      // Compute B = A / scalar;
      
      DoubleMatrix Scalars(_As[i]->nr(), _As[i]->nc());
      DoubleMatrix dmatB(_As[i]->nr(), _As[i]->nc());
      
      double* pdScalars = Scalars.data();
      double* pdB  = dmatB.data();
      double* pdA  = _As[i]->data();
      
      Scalars.fill(_scalar);
      
      divByScalar(*_As[i], Scalars, dmatB);
      
      // Compute A = dmatAA = B * scalar
      for(int j=0; j<_rows[i]*_cols[i]; j++ )
        {
	  double actual   = pdA[j];
	  double expected = pdB[j]*pdScalars[j];
	  double scale;

	  if ( expected != 0.0 )
	  {
	    scale = expected;
	  }
	  else
	  {
	    scale = actual;
	  }

	  CPPUNIT_ASSERT( isDblEpsEqual( actual, expected, scale ) );
	}
    }  
}

