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
 * File: residualsTest.cpp
 *
 *
 * Test cases for residuals()
 *
 * Author: Viet Nyuyen
 * Updated by: sachiko honda
 *
 *************************************************************************/
#include <iostream>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include <spk/residuals.h>
#include <spk/SpkValarray.h>
#include "residualsTest.h"
#include <spk/identity.h>

using SPK_VA::valarray;
using namespace CppUnit;

class residualsTestModel : public SpkModel<double>
{
  valarray<double> _a, _b;
  int _i;
  
public:
  residualsTestModel(){}
  ~residualsTestModel(){}
protected:
  void doSelectIndividual(int i)
  {
    _i = i;
  }
  void doSetPopPar(const valarray<double>& alp)
  {
    _a.resize( alp.size() );
    _a = alp;
  }
  void doSetIndPar(const valarray<double>& b)
  {
    _b.resize( b.size() );
    _b = b;
  }
  void doDataMean( valarray<double>& f ) const 
  {
    f.resize( 1 );
    f = _a[ 0 ] + _b[ 0 ];
    
  }
  bool doDataMean_popPar( valarray<double>& ret ) const 
  {
    return false;
  }
  bool doDataMean_indPar( valarray<double>& ret ) const
  {
    return false;
  }
  void doDataVariance( valarray<double>& ret ) const
  {
    ret.resize( 1 );
    identity( 1, ret );
  }
  bool doDataVariance_popPar( valarray<double>& ret ) const
  {
    return false;
  }
  bool doDataVariance_indPar( valarray<double>& ret ) const
  {
    return false;
  }
  void doIndParVariance( valarray<double>& D ) const
  {
    D.resize( 1 );
    D = _a[ 1 ];
  }
  bool doIndParVariance_popPar( valarray<double>& ret ) const
  {
    return false;
  }
};

void residualsTest::setUp()
{
}
void residualsTest::tearDown()
{
    // clean up
}

Test* residualsTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("residualsTest");

  // duplicate the following example for each test case, replacing the case name.
  suiteOfTests->addTest(new TestCaller<residualsTest>("residuals",  &residualsTest::test));
  
  return suiteOfTests;
}
void residualsTest::test()
{
  using namespace std;
  residualsTestModel model;
  
  int    nInd = 100;
  int    npts = 1;
  int    nB   = 1;
  int    nAlp = 2;
  double temp;
  
  // Measurement values, y.
  valarray<double> yOut( nInd );
  
  // Number of measurements for each individual. 
  valarray<int> N( npts, nInd );
  
  // Fixed effects - 2 of them
  valarray<double> alp( nAlp );
  
  alp[ 0 ] = 1.0;
  alp[ 1 ] = 5.0;
  
  // Artificially fill yOut
  for (int i = 0; i < nInd; i++)
    {
      yOut[i] = i;
    }
  
  // Artificially fill bAll
  valarray<double> bAll( 0.0, nInd * nB );
  
  valarray<double> residualsOut = residuals(model, nInd, alp, yOut, N, bAll);
  
  CPPUNIT_ASSERT_EQUAL(nInd, static_cast<int>( residualsOut.size() ));     // dimension of residualData
  
  for (int i = 0; i < nInd; i++)
    {
      temp = i-1;
      
      // check value of residualData[i] = i - 1
      CPPUNIT_ASSERT_DOUBLES_EQUAL( temp, residualsOut[i], 0.0001 );
    }
  return;
}
