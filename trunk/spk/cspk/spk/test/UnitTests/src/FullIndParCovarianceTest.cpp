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
 * File: FullIndParCovarianceTest.cpp
 *
 *
 * Test cases for FullIndParCovariance class
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
#pragma warning (disable: 4786)

#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include <spk/DoubleMatrix.h>
#include <spk/SpkModel.h>
#include "FullIndParCovarianceTest.h"

#include <spk/identity.h>
#include <spk/SpkModel.h>

static const int nPOPPAR = 4;
static const int nINDPAR = 3;
static const int nY      = 3;

using namespace std;
using namespace CppUnit;

class  PopFullIndParCovTestModel : public FullIndParCovariance
{
  int _nPopParIn, _nIndParIn;
  const int _nData;

public:
   PopFullIndParCovTestModel( int nPopParIn, int nIndParIn ) 
     : _nPopParIn(nPopParIn), _nIndParIn(nIndParIn), _nData(nY)
  {
  }
  ~ PopFullIndParCovTestModel(){}
private:
  virtual void doCov( valarray<double>& R ) const
  {
    R.resize( _nData * _nData );
    identity(_nData, R );
  }
  virtual bool doCov_popPar( valarray<double>& R_a ) const
  {
    assert( _nPopParIn <= _nData*_nData );
    R_a.resize( _nData*_nData * _nPopParIn );
    R_a = 0.0;
    for( int j=0; j<_nPopParIn; j++ )
    {
      for( int i=0; i<_nData*_nData; i++ )
      {
        if( i == j )
          R_a[i+j*_nData*_nData] = 1.0;
      }
    }
    return true;
  }
  virtual void doInv( valarray<double>& Rinv ) const
  {
    Rinv.resize( _nData * _nData );
    Rinv = identity(_nData).toValarray();
  }
  virtual bool doInv_popPar( valarray<double>& Rinv_a ) const
  {
    assert( _nPopParIn <= _nData*_nData );
    Rinv_a.resize( _nData*_nData * _nPopParIn );
    Rinv_a = 0.0;
    for( int j=0; j<_nPopParIn; j++ )
    {
      for( int i=0; i<_nData*_nData; i++ )
      {
        if( i == j )
          Rinv_a[i+j*_nData*_nData] = 1.0;
      }
    }
    return true;
  }
};


void FullIndParCovarianceTest::setUp()
{
    // initializations
}
void FullIndParCovarianceTest::tearDown()
{
    // clean up
}

Test* FullIndParCovarianceTest::suite()
{
	TestSuite *suiteOfTests = new TestSuite( "FullIndParCovarianceTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<FullIndParCovarianceTest>(
                         "popCovTest", 
                         &FullIndParCovarianceTest::popCovTest));
    suiteOfTests->addTest(new TestCaller<FullIndParCovarianceTest>(
                         "popCov_popParTest",  
                         &FullIndParCovarianceTest::popCov_popParTest));
    suiteOfTests->addTest(new TestCaller<FullIndParCovarianceTest>(
                         "popCov_indParTest",  
                         &FullIndParCovarianceTest::popCov_indParTest));
    suiteOfTests->addTest(new TestCaller<FullIndParCovarianceTest>(
                         "popInvTest",  
                         &FullIndParCovarianceTest::popInvTest));
    suiteOfTests->addTest(new TestCaller<FullIndParCovarianceTest>(
                         "popInv_popParTest",  
                         &FullIndParCovarianceTest::popInv_popParTest));
    suiteOfTests->addTest(new TestCaller<FullIndParCovarianceTest>(
                         "popInv_indParTest",  
                         &FullIndParCovarianceTest::popInv_indParTest));

    return suiteOfTests;
}

void FullIndParCovarianceTest::popCovTest()
{
  PopFullIndParCovTestModel indParCovModel(nPOPPAR, nINDPAR);

  DoubleMatrix expected = identity( nY );

  valarray<double> ret;
  indParCovModel.cov(ret);

  for( int j=0; j<nY; j++ )
  {
    for( int i=0; i<nY; i++ )
    {
      CPPUNIT_ASSERT_EQUAL( expected.data()[i], ret[i] );
    }
  }
}
void FullIndParCovarianceTest::popCov_popParTest()
{
   PopFullIndParCovTestModel indParCovModel(nPOPPAR, nINDPAR);

  valarray<double> ret;
  indParCovModel.cov_popPar(ret);

  CPPUNIT_ASSERT_EQUAL( nY*nY * nPOPPAR, static_cast<int>( ret.size() ) );

  for( int j=0; j<nPOPPAR; j++ )
  {
    for( int i=0; i<nY*nY; i++ )
    {
      if( i==j )
        CPPUNIT_ASSERT_EQUAL( 1.0, ret[i+j*nY*nY] );
      else
        CPPUNIT_ASSERT_EQUAL( 0.0, ret[i+j*nY*nY] );
    }
  }
}
void FullIndParCovarianceTest::popCov_indParTest()
{
  PopFullIndParCovTestModel indParCovModel(nPOPPAR, nINDPAR);

  valarray<double> ret;
  try{
    indParCovModel.cov_indPar(ret);
  }
  catch(...)
  {
    //
    // It failed as expected.  That's good.
    //
    CPPUNIT_ASSERT(true);
    return;
  }
  CPPUNIT_ASSERT(false);
}
void FullIndParCovarianceTest::popInvTest()
{
   PopFullIndParCovTestModel indParCovModel(nPOPPAR, nINDPAR);

  DoubleMatrix expected = identity( nY );

  valarray<double> ret;
  indParCovModel.inv(ret);

  for( int j=0; j<nY; j++ )
  {
    for( int i=0; i<nY; i++ )
    {
      CPPUNIT_ASSERT_EQUAL( expected.data()[i], ret[i] );
    }
  }

}
void FullIndParCovarianceTest::popInv_popParTest()
{
   PopFullIndParCovTestModel indParCovModel(nPOPPAR, nINDPAR);

  valarray<double> ret;
  indParCovModel.inv_popPar(ret);

  CPPUNIT_ASSERT_EQUAL( nY*nY * nPOPPAR, static_cast<int>( ret.size() ) );

  for( int j=0; j<nPOPPAR; j++ )
  {
    for( int i=0; i<nY*nY; i++ )
    {
      if( i==j )
        CPPUNIT_ASSERT_EQUAL( 1.0, ret[i+j*nY*nY] );
      else
        CPPUNIT_ASSERT_EQUAL( 0.0, ret[i+j*nY*nY] );
    }
  }
}
void FullIndParCovarianceTest::popInv_indParTest()
{
   PopFullIndParCovTestModel indParCovModel(nPOPPAR, nINDPAR);

  valarray<double> ret;
  try{
    indParCovModel.inv_indPar(ret);
  }
  catch(...)
  {
    //
    // It failed as expected.  That's good.
    //
    CPPUNIT_ASSERT(true);
    return;
  }
  CPPUNIT_ASSERT(false);
}
