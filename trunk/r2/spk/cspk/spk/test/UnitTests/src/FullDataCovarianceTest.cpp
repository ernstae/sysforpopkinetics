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
 * File: FullDataCovarianceTest.cpp
 *
 *
 * Test cases for FullDataCovariance class
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include <spk/DoubleMatrix.h>
#include <spk/SpkModel.h>
#include "FullDataCovarianceTest.h"

#include <spk/identity.h>
#include <spk/SpkModel.h>

static const int nPOPPAR = 4;
static const int nINDPAR = 3;
static const int nY      = 3;

using namespace std;
using namespace CppUnit;

class PopFullDataCovTestModel : public FullDataCovariance
{
  int nPopPar, nIndPar;
  const int nData;

public:
  PopFullDataCovTestModel( int nPopParIn, int nIndParIn ) 
    : nPopPar(nPopParIn), nIndPar(nIndParIn), nData(nY)
  {
  }
  ~PopFullDataCovTestModel(){}
private:
  virtual void doCov( valarray<double>& R ) const
  {
    R.resize( nData * nData );
    identity(nData, R);
  }
  virtual bool doCov_popPar( valarray<double>& R_a ) const
  {
    assert( nPopPar <= nData*nData );
    R_a.resize( nData*nData * nPopPar );
    R_a = 0.0;
    for( int j=0; j<nPopPar; j++ )
    {
      for( int i=0; i<nData*nData; i++ )
      {
        if( i == j )
          R_a[i+j*nData*nData] = 1.0;
      }
    }
    return true;
  }
  virtual bool doCov_indPar( valarray<double>& R_b ) const
  {
    assert( nIndPar <= nData*nData );

    R_b.resize( nData*nData * nIndPar );
    R_b = 0.0;
    for( int j=0; j<nIndPar; j++ )
    {
      for( int i=0; i<nData*nData; i++ )
      {
        if( i == j )
          R_b[i+j*nData*nData] = 1.0;
      }
    }
    return true;
  }
  virtual void doInv( valarray<double>& Rinv ) const
  {
    Rinv.resize( nData * nData );
    identity(nData, Rinv);
  }
  virtual bool doInv_popPar( valarray<double>& Rinv_a ) const
  {
    assert( nPopPar <= nData*nData );
    Rinv_a.resize( nData*nData * nPopPar );
    Rinv_a = 0.0;
    for( int j=0; j<nPopPar; j++ )
    {
      for( int i=0; i<nData*nData; i++ )
      {
        if( i == j )
          Rinv_a[i+j*nData*nData] = 1.0;
      }
    }
    return true;
  }
  virtual bool doInv_indPar( valarray<double>& Rinv_b ) const
  {
    assert( nIndPar <= nData*nData );
    Rinv_b.resize( nData*nData * nIndPar );
    Rinv_b = 0.0;
    for( int j=0; j<nIndPar; j++ )
    {
      for( int i=0; i<nData*nData; i++ )
      {
        if( i == j )
          Rinv_b[i+j*nData*nData] = 1.0;
      }
    }
    return true;
  }
};

class IndFullDataCovTestModel : public FullDataCovariance
{
  int nPopPar, nIndPar;
  const int nData;

public:
  IndFullDataCovTestModel( int nPopParIn, int nIndParIn ) 
    : nPopPar(nPopParIn), nIndPar(nIndParIn), nData(nY)
  {
  }
  ~IndFullDataCovTestModel(){}
private:
  virtual void doCov( valarray<double>& R ) const
  {
    R.resize( nData * nData );
    identity( nData, R );
  }
  virtual bool doCov_indPar( valarray<double>& R_b ) const
  {
    assert( nIndPar <= nData*nData );
    R_b.resize( nData*nData * nIndPar );
    R_b = 0.0;
    for( int j=0; j<nIndPar; j++ )
    {
      for( int i=0; i<nData*nData; i++ )
      {
        if( i == j )
          R_b[i+j*nData*nData] = 1.0;
      }
    }
    return true;
  }
  virtual bool doCov_popPar( valarray<double>& R_alp ) const
  {
    assert( nIndPar <= nData*nData );
    R_alp.resize( nData*nData * nIndPar );
    R_alp = 0.0;
    for( int j=0; j<nIndPar; j++ )
    {
      for( int i=0; i<nData*nData; i++ )
      {
        if( i == j )
          R_alp[i+j*nData*nData] = 1.0;
      }
    }
    return true;
  }
  virtual void doInv( valarray<double>& Rinv ) const
  {
    Rinv.resize( nData * nData );
    identity(nData, Rinv);
  }
  virtual bool doInv_indPar( valarray<double>& Rinv_b ) const
  {
    assert( nIndPar <= nData*nData );
    Rinv_b.resize( nData*nData * nIndPar );
    Rinv_b = 0.0;
    for( int j=0; j<nIndPar; j++ )
    {
      for( int i=0; i<nData*nData; i++ )
      {
        if( i == j )
          Rinv_b[i+j*nData*nData] = 1.0;
      }
    }
  }
  virtual bool doInv_popPar( valarray<double>& Rinv_alp ) const
  {
    assert( nIndPar <= nData*nData );
    Rinv_alp.resize( nData*nData * nIndPar );
    Rinv_alp = 0.0;
    for( int j=0; j<nIndPar; j++ )
    {
      for( int i=0; i<nData*nData; i++ )
      {
        if( i == j )
          Rinv_alp[i+j*nData*nData] = 1.0;
      }
    }
    return true;
  }
};
void FullDataCovarianceTest::setUp()
{
    // initializations
}
void FullDataCovarianceTest::tearDown()
{
    // clean up
}

Test* FullDataCovarianceTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "FullDataCovarianceTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<FullDataCovarianceTest>(
                         "popCovTest", 
                         &FullDataCovarianceTest::popCovTest));
    suiteOfTests->addTest(new TestCaller<FullDataCovarianceTest>(
                         "popCov_popParTest",  
                         &FullDataCovarianceTest::popCov_popParTest));
    suiteOfTests->addTest(new TestCaller<FullDataCovarianceTest>(
                         "popCov_indParTest",  
                         &FullDataCovarianceTest::popCov_indParTest));
    suiteOfTests->addTest(new TestCaller<FullDataCovarianceTest>(
                         "popInvTest",  
                         &FullDataCovarianceTest::popInvTest));
    suiteOfTests->addTest(new TestCaller<FullDataCovarianceTest>(
                         "popInv_popParTest",  
                         &FullDataCovarianceTest::popInv_popParTest));
     suiteOfTests->addTest(new TestCaller<FullDataCovarianceTest>(
                         "popInv_indParTest",  
                         &FullDataCovarianceTest::popInv_indParTest));


    suiteOfTests->addTest(new TestCaller<FullDataCovarianceTest>(
                         "indCov_popParTest",  
                         &FullDataCovarianceTest::indCov_popParTest));

     /*
    suiteOfTests->addTest(new TestCaller<FullDataCovarianceTest>(
                         "indInv_popParTest",  
                         &FullDataCovarianceTest::indInv_popParTest));
     */
    return suiteOfTests;
}

void FullDataCovarianceTest::popCovTest()
{
  PopFullDataCovTestModel dataCovModel(nPOPPAR, nINDPAR);

  DoubleMatrix expected = identity( nY );

  valarray<double> ret( nY * nY );
  dataCovModel.cov(ret);

  for( int j=0; j<nY; j++ )
  {
    for( int i=0; i<nY; i++ )
    {
      CPPUNIT_ASSERT_EQUAL( expected.data()[i], ret[i] );
    }
  }
}
void FullDataCovarianceTest::popCov_popParTest()
{
  PopFullDataCovTestModel dataCovModel(nPOPPAR, nINDPAR);

  valarray<double> ret( nPOPPAR * nPOPPAR );
  dataCovModel.cov_popPar(ret);

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
void FullDataCovarianceTest::popCov_indParTest()
{
  PopFullDataCovTestModel dataCovModel(nPOPPAR, nINDPAR);

  valarray<double> ret( nY * nY * nINDPAR );
  dataCovModel.cov_indPar(ret);

  CPPUNIT_ASSERT_EQUAL( nY*nY * nINDPAR, static_cast<int>( ret.size() ) );

  for( int j=0; j<nINDPAR; j++ )
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
void FullDataCovarianceTest::popInvTest()
{
  PopFullDataCovTestModel dataCovModel(nPOPPAR, nINDPAR);

  DoubleMatrix expected = identity( nY );

  valarray<double> ret( nY * nY );
  dataCovModel.inv(ret);

  for( int j=0; j<nY; j++ )
  {
    for( int i=0; i<nY; i++ )
    {
      CPPUNIT_ASSERT_EQUAL( expected.data()[i], ret[i] );
    }
  }

}
void FullDataCovarianceTest::popInv_popParTest()
{
  PopFullDataCovTestModel dataCovModel(nPOPPAR, nINDPAR);

  valarray<double> ret ( nY * nY * nPOPPAR );
  dataCovModel.inv_popPar(ret);

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
void FullDataCovarianceTest::popInv_indParTest()
{
  PopFullDataCovTestModel dataCovModel(nPOPPAR, nINDPAR);

  valarray<double> ret( nY * nY * nINDPAR );
  dataCovModel.inv_indPar(ret);

  CPPUNIT_ASSERT_EQUAL( nY*nY * nINDPAR, static_cast<int>( ret.size() ) );

  for( int j=0; j<nINDPAR; j++ )
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
void FullDataCovarianceTest::indCov_popParTest()
{
  //
  // IndFullDataCovTestModel does not define XXX_popPar() virtual member functions
  // because it's an individual model.
  // Yet, it should not stop from instantiating.
  //
  IndFullDataCovTestModel dataCovModel(nPOPPAR, nINDPAR);
  valarray<double> ret( nINDPAR * nINDPAR * nPOPPAR );
  try{
    //
    // This member function is not defined.  An attempt to call it
    // should fail.
    //
    dataCovModel.cov_popPar(ret);
    return;
  }
  catch( ... )
  {
    //
    // It failed, as expected.  That's good.
    //
    CPPUNIT_ASSERT_MESSAGE("dataCovModel.cov_popPar() threw an exception as expected", true );
    return;
  }

  //
  // No, it didn't fail in the way it was expected.  It's wrong.
  //
  CPPUNIT_ASSERT_MESSAGE( "dataCovModel.cov_popPar() didn't fail in the way it was expected.", false );
}
void FullDataCovarianceTest::indInv_popParTest()
{
  //
  // IndFullDataCovTestModel does not define XXX_popPar() virtual member functions
  // because it's an individual model.
  // Yet, it should not stop from instantiating.
  //
  IndFullDataCovTestModel dataCovModel(nPOPPAR, nINDPAR);
  valarray<double> ret( nINDPAR * nINDPAR * nPOPPAR );
  try{
    //
    // This member function is not defined.  An attempt to call it
    // should fail.
    //
    dataCovModel.inv_popPar(ret);
    return;
  }
  catch( ... )
  {
    //
    // It failed, as expected.  That's good.
    //
    CPPUNIT_ASSERT_MESSAGE( "dataCovModel.inv_popPar() threw an exception as epxected.", true );
    return;
  }
  //
  // No, it didn't fail.  It's wrong.
  //
  CPPUNIT_ASSERT_MESSAGE( "dataCovModel.inv_popPar(ret) didn't fail in the way it was expected.", false);
}
