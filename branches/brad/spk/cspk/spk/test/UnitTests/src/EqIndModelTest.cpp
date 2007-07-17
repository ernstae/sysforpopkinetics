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
 * File: EqIndModelTest.cpp
 *
 *
 * Unit test for the class EqIndModel.
 *
 * Author: Jiaji Du
 *
 * Extended by Sachiko, 09/27/2002
 *
 *************************************************************************/

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/EqIndModel.h"
#include "../../../spk/SpkModel.h"
#include <iostream>
#include <iomanip>
#include <cmath>
#include "../../../spk/SpkException.h"

#include "EqIndModelTest.h"

using namespace std; 
using namespace CppUnit;

void EqIndModelTest::setUp()
{
    // initializations
}
void EqIndModelTest::tearDown()
{
    // clean up
}

Test* EqIndModelTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("EqIndModelTest");
  suiteOfTests->addTest(new TestCaller<EqIndModelTest>("testEqIndModel",  &EqIndModelTest::testEqIndModel));

  // This test has been disabled.
  /*
   suiteOfTests->addTest(new TestCaller<EqIndModelTest>("testLargeMatrix", &EqIndModelTest::testLargeMatrix));
  */

  return suiteOfTests;
}

/*************************************************************************
 *
 * Class for the population level original SpkModel object
 *
 *************************************************************************/

class TestModel : public SpkModel<double>
{
    valarray<double> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nYi;
public:
    TestModel(int nA, int nB, int nYi) : _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB) {};    
    ~TestModel(){};
private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<double>& aval)
    {
        _a = aval;
    }
    void doSetIndPar(const valarray<double>& bval)
    {
        _b = bval;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
        //
        // D = [ alp[1] ]
        //
        ret.resize(_nB * _nB);
        ret[0] = _a[1];
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        //
        // D_alp = [ 0  1 ]
        //
        ret.resize(_nB * _nB * _nA);
        ret[0] = 0.0;
        ret[1] = 1.0;
        return true;
    }
    void doDataMean( valarray<double>& ret ) const
    {
        //
        // f = [ alp[0] + exp( b[0] ) + i ]
        //
        ret.resize(_nYi);
        ret[0] = ( _a[0] + exp( _b[0] ) + _i );
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
        //
        // f_alp = [ 1   0 ]
        //
        ret.resize(_nYi * _nA);
        ret[0] = 1.0;
        ret[1] = 0.0;
        return true;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        //
        // f_b = [ exp( b[0] ) ]
        //
        ret.resize(_nYi * _nB);
        ret[0] = exp( _b[0] );
        return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        //
        // R = [ b[0] - i ]
        //
        ret.resize(_nYi * _nYi);
        ret[0] = _b[0] - _i;
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        //
        // R_alp = [ 0   0 ]
        //
        ret.resize(_nYi * _nYi * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
        return false;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        //
        // R_b = [ 1 ]
        //
        ret.resize(_nYi * _nYi *_nB);
        ret[0] = 1.0;
        return false;
    }
};

/*************************************************************************
 *
 * Function: testEqIndModel
 *
 *
 * This test implements the example problem from EqIndModel specification. 
 *
 *************************************************************************/


void EqIndModelTest::testEqIndModel()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Number of individuals.
  const int nInd = 2;

  // Number of measurements per individual (same for all)
  const int nYi = 1;

  // Number of measurements for all individuals
  const int nY = nInd * nYi;

  // Number of pop parameter
  const int nAlp = 2;

  // Number of ind parameter
  const int nB = 1;


  //------------------------------------------------------------
  // Create a TestModel instance.
  //------------------------------------------------------------

  TestModel testModel( nAlp, nB, nYi );

  valarray<double> a( nAlp );
  a[0] = 1.0;
  a[1] = 2.0;
  testModel.setPopPar( a );

  //------------------------------------------------------------
  // Create a EqIndModel instance.
  //------------------------------------------------------------

  valarray<int> N( 1, nInd );
  valarray<double> bStep( .01, nB );

  EqIndModel eqIndModel( &testModel, N, bStep, nAlp );

  //------------------------------------------------------------
  // Prepare tests.
  //------------------------------------------------------------

  cout << setiosflags(ios::scientific) << setprecision(15);
  // bStep = .01  Define
  double C = ( exp( .01 ) - exp( -.01 ) ) / 2 / .01;
  C = C * C;
  double e = 1e-13;
  //cout<< endl << endl << "C = " << C << endl << endl;

  //------------------------------------------------------------
  // Test dataMean.
  //------------------------------------------------------------

  valarray<double> f;
  eqIndModel.dataMean( f );

  //             / alp[ 0 ] + 1 \
  //    f(alp) = |               |
  //             \ alp[ 0 ] + 2 /
  
  CPPUNIT_ASSERT_EQUAL( f[ 0 ], a[ 0 ] + 1 );
  CPPUNIT_ASSERT_EQUAL( f[ 1 ], a[ 0 ] + 2 );

  DoubleMatrix dmatF( f );
  //cout << "f = " << endl;
  //dmatF.print(); 

  //------------------------------------------------------------
  // Test dataMean_indPar.
  //------------------------------------------------------------

  valarray<double> f_b;
  eqIndModel.dataMean_indPar( f_b );

  //               / 1 \
  //    f_b(alp) = |   |
  //               \ 1 /

  CPPUNIT_ASSERT_EQUAL( f_b[ 0 ], 1.0 );
  CPPUNIT_ASSERT_EQUAL( f_b[ 1 ], 1.0 );

  DoubleMatrix dmatF_b( f_b, nAlp );
  //cout << "f_b = " << endl;
  //dmatF_b.print(); 

  //------------------------------------------------------------
  // Test dataVariance.
  //------------------------------------------------------------

  valarray<double> R;
  eqIndModel.dataVariance( R );

  //             / C * a[ 1 ]                 0 \
  //    R(alp) = |                              |
  //             \ 0             C * a[ 1 ] - 1 /

  CPPUNIT_ASSERT_DOUBLES_EQUAL( R[ 0 ],     C * a[ 1 ], e );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( R[ 1 ],              0, 0 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( R[ 2 ],              0, 0 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( R[ 3 ], C * a[ 1 ] - 1, e );

  DoubleMatrix dmatR( R, nY );
  //cout << "R = " << endl;
  //dmatR.print(); 

  //------------------------------------------------------------
  // Test dataVariance_indPar.
  //------------------------------------------------------------

  valarray<double> R_b;
  eqIndModel.dataVariance_indPar( R_b );

  //               / 0    C \
  //    R_b(alp) = | 0    0 |
  //               | 0    0 |
  //               \ 0    C /

  CPPUNIT_ASSERT_DOUBLES_EQUAL( R_b[ 0 ], 0, 0 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( R_b[ 1 ], 0, 0 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( R_b[ 2 ], 0, 0 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( R_b[ 3 ], 0, 0 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( R_b[ 4 ], C, e );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( R_b[ 5 ], 0, 0 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( R_b[ 6 ], 0, 0 );
  CPPUNIT_ASSERT_DOUBLES_EQUAL( R_b[ 7 ], C, e );

  DoubleMatrix dmatR_b( R_b, nAlp );
  //cout << "R_b = " << endl;
  //dmatR_b.print(); 
}


/*************************************************************************
 *
 * Function: testLargeMatrix
 *
 *
 * This test pushes the limit on memory requirement.
 *
 *************************************************************************/

//
// [ Implementation notes by Sachiko, 10/04/02 ]
// The following set of vector sizes was large enough to
// cause "bad alloc" or similar errors on a Win2k machine with
// 128 MB of RAM.
// If in the future, this testLargeMatrix() test fails, it
// must be due to the fact you have more RAM or some basic nature
// of your machine environment has changed or quite different
// from what I had.
// Adjust the values as needed to cause memory allocation errors.
//
static const int nYi  = 50;
static const int nB   = 20;
static const int nAlp = 20;
static const int nInds= 100;

class LargeMatrixUserModel : public SpkModel<double>
{
  valarray<double> _alp, _b;
  int              _nAlp, _nB;
  int              _who;

public:
  LargeMatrixUserModel() : _alp(0), _b(0)
  {
  }
  ~LargeMatrixUserModel()
  {
  }
private:
  void doSetIndPar( const valarray<double>& bIn )
  {
    assert( bIn.size() == nB );
    _b    = bIn;
  }
  void doSetPopPar( const valarray<double>& alpIn )
  {
    assert( alpIn.size() == nAlp );
    _alp  = alpIn;
  }
  void doSelectIndividual( int who )
  {
    assert( who >= 0 );
    assert( who < nInds );
    _who = who;
  }
  void doDataMean( valarray<double>& fiOut ) const
  {
    //
    // fiOut: nYi (100)
    //
    fiOut.resize( nYi );
    fiOut = 0.0;
  }
  bool doDataMean_indPar( valarray<double>& fi_bOut ) const
  {
    //
    // fi_bOut: nYi * nB (100 x 100)
    //
    fi_bOut.resize( nYi * nB );
    fi_bOut = 0.0;
    return false;
  }
  bool doDataMean_popPar( valarray<double>& fi_alpOut ) const
  {
    //
    // fi_alpOut: nYi * nAlp (100 x 100)
    //
    fi_alpOut.resize( nYi * nAlp );
    fi_alpOut = 0.0;
    return false;
  }
  void doDataVariance( valarray<double>& RiOut ) const
  {
    //
    // RiOut: nYi * nYi (100 x 100)
    //
    RiOut.resize( nYi * nYi );
    RiOut = 0.0;
  }
  bool doDataVariance_indPar( valarray<double>& Ri_bOut ) const
  {
    //
    // Ri_bOut: nYi * nYi x nB (100 * 100 x 100)
    //
    Ri_bOut.resize( nYi * nYi * nB );
    Ri_bOut = 0.0;
    return false;
  }
  bool doDataVariance_popPar( valarray<double>& Ri_alpOut ) const
  {
    //
    // Ri_alpOut: nYi * nYi x nAlp (100 * 100 x 100)
    //
    Ri_alpOut.resize( nYi * nYi * nAlp );
    Ri_alpOut = 0.0;
    return false;
  }
  void doIndParVariance( valarray<double>& DOut ) const
  {
    //
    // DOut: nB x nB (100 x 100)
    //
    DOut.resize( nB * nB );
    DOut = 0.0;
  }
  bool doIndParVariance_popPar( valarray<double>& D_alpOut ) const
  {
    //
    // D_alpOut: nB * nB x nAlp (100 * 100 x 100)
    //
    D_alpOut.resize( nB * nB * nAlp);
    D_alpOut = 0.0;
    return false;
  }
};

void EqIndModelTest::testLargeMatrix()
{
  using std::cout;
  using std::cerr;
  using std::endl;

  valarray<int> N( nInds );
  N = nYi;

  valarray<double> bStep( nB );
  bStep = 0.1;

  valarray<double> alpIn( nAlp );
  alpIn = 0.1;

  valarray<double> R_bOut;

  LargeMatrixUserModel model;

  EqIndModel foModel( &model, N, bStep, nAlp );
  foModel.setIndPar( alpIn );

  try{
    //
    // This routine requires the largest memory chunk for double-precision matrices.
    // So, execute it to force generating allocation errors.
    //
    foModel.dataVariance_indPar( R_bOut );
  }
  catch( const SpkException& e )
  {
    if( e.find( SpkError::SPK_INSUFFICIENT_MEM_ERR ) >= 0 )
      CPPUNIT_ASSERT_MESSAGE( "Caught an exception successfully.  Good.", true );
    else
      CPPUNIT_ASSERT_MESSAGE( "It should have caught an SPK_INSUFFICIENT_MEM_ERR error.", false );
    return;
  }
  CPPUNIT_ASSERT_MESSAGE( "It should have caught an SpkException object.", false );
}
