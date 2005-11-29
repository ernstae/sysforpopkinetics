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
 * File: CovarianceTest.cpp
 *
 *
 * Exerciese the interfaces of Covariance class.
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
#include "../../../spk/Covariance.h"
#include "CovarianceTest.h"
#include "../../../spk/SpkValarray.h"

using SPK_VA::valarray;
using namespace CppUnit;

static const int NUM_COV = 2;
static const int NUM_INDPAR = 3;
static const int NUM_POPPAR = 4;
static const double VAL = 1.0;

void CovarianceTest::setUp()
{
    // initializations
}
void CovarianceTest::tearDown()
{
    // clean up
}

Test* CovarianceTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "CovarianceTest" );

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<CovarianceTest>("testMathInterfaces", &CovarianceTest::testMathInterfaces));
    suiteOfTests->addTest(new TestCaller<CovarianceTest>("testCovInterfaces",  &CovarianceTest::testCovInterfaces));

    return suiteOfTests;
}

//
// Covariance class is a pure abstract class.  Have to derive a subclass that is concrete.
//
class concreteCov : public Covariance
{
  DoubleMatrix _popPar;
  DoubleMatrix _indPar;
  int          _who;

  const int _nCov;
  const int _nPopPar;
  const int _nIndPar;
public:

  concreteCov() : _nCov(NUM_COV), _nPopPar(NUM_POPPAR), _nIndPar(NUM_INDPAR)
  {

  }
private:
  double doLogdet() const
  {
    return VAL;
  }

  double doWeightedSumOfSquares( const valarray<double>& z ) const
  {
    return VAL;
  }

  void doCov( valarray<double>& ret ) const
  {
    ret.resize(_nCov * _nCov);
    ret = VAL;
  }

  bool doCov_popPar( valarray<double>& ret ) const
  {
    ret.resize(_nCov*_nCov * _nPopPar);
    ret = VAL;
    return true;
  }

  bool doCov_indPar( valarray<double>& ret ) const
  {
    ret.resize(_nCov*_nCov * _nIndPar);
    ret = VAL;
    return true;
  }

  void doInv( valarray<double>& ret ) const
  {
    ret.resize(_nCov *_nCov);
    ret = VAL;
  }
  
  bool doInv_popPar( valarray<double>& ret ) const
  {
    ret.resize(_nCov*_nCov * _nPopPar);
    ret = VAL;
    return true;
  }

  bool doInv_indPar( valarray<double>& ret ) const
  {
    ret.resize(_nCov*_nCov * _nIndPar);
    ret = VAL;
    return true;
  }

};
void CovarianceTest::testMathInterfaces()
{
  concreteCov covOb;

  double expectedLogdet = VAL;
  double retLogdet = covOb.logdet();

  CPPUNIT_ASSERT_EQUAL(expectedLogdet, retLogdet);

  double expectedWeightedSumOfSquares = VAL;
  double retWeightedSumOfSquares = covOb.logdet();

  CPPUNIT_ASSERT_EQUAL(expectedWeightedSumOfSquares, retWeightedSumOfSquares);
}
void CovarianceTest::testCovInterfaces()
{
  int i;

  concreteCov covOb;

  // Exercising cov() interface
  valarray<double> retCov;
  DoubleMatrix expectedCov( NUM_COV, NUM_COV );
  expectedCov.fill(VAL);

  covOb.cov(retCov);
  for( i=0; i<NUM_COV*NUM_COV; i++ )
  {
    CPPUNIT_ASSERT_EQUAL(expectedCov.data()[i], retCov[i]);
  }

  // Exercising cov_popPar() interface
  valarray<double> retCov_popPar;
  DoubleMatrix expectedCov_popPar( NUM_COV * NUM_COV, NUM_POPPAR);
  expectedCov_popPar.fill(VAL);

  covOb.cov_popPar(retCov_popPar);
  for( i=0; i<NUM_COV*NUM_COV; i++ )
  {
    CPPUNIT_ASSERT_EQUAL(expectedCov_popPar.data()[i], retCov_popPar[i]);
  }

  // Exercising cov_indPar() interface
  valarray<double> retCov_indPar;
  DoubleMatrix expectedCov_indPar( NUM_COV * NUM_COV, NUM_INDPAR);
  expectedCov_indPar.fill(VAL);

  covOb.cov_indPar(retCov_indPar);
  for( i=0; i<NUM_COV*NUM_COV; i++ )
  {
    CPPUNIT_ASSERT_EQUAL(expectedCov_indPar.data()[i], retCov_indPar[i]);
  }


  // Exercising inv() interface
  valarray<double> retInv;
  DoubleMatrix expectedInv( NUM_COV, NUM_COV );
  expectedInv.fill(VAL);

  covOb.inv(retInv);
  for( i=0; i<NUM_COV*NUM_COV; i++ )
  {
    CPPUNIT_ASSERT_EQUAL(expectedInv.data()[i], retInv[i]);
  }

  // Exercising inv_popPar() interface
  valarray<double> retInv_popPar;
  DoubleMatrix expectedInv_popPar( NUM_COV * NUM_COV, NUM_POPPAR);
  expectedInv_popPar.fill(VAL);

  covOb.inv_popPar(retInv_popPar);
  for( i=0; i<NUM_COV*NUM_COV; i++ )
  {
    CPPUNIT_ASSERT_EQUAL(expectedInv_popPar.data()[i], retInv_popPar[i]);
  }

  // Exercising inv_indPar() interface
  valarray<double> retInv_indPar;
  DoubleMatrix expectedInv_indPar( NUM_COV * NUM_COV, NUM_INDPAR);
  expectedInv_indPar.fill(VAL);

  covOb.inv_indPar(retInv_indPar);
  for( i=0; i<NUM_COV*NUM_COV; i++ )
  {
    CPPUNIT_ASSERT_EQUAL(expectedInv_indPar.data()[i], retInv_indPar[i]);
  }

}
