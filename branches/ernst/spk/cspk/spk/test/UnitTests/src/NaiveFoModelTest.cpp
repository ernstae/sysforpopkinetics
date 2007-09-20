/*************************************************************************
 *
 * File: NaiveFoModelTest.cpp
 *
 *
 * Test cases for NaiveFoModel class, which implements
 * a straight translation of the following assumption:
 *
 *  f(alp,b) = f(alp,0) + df(alp,b)_b * b
 *  R(alp,b) = R(alp,0)
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
#include "../../../spk/NaiveFoModel.h"
#include "NaiveFoModelTest.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/allZero.h"

using SPK_VA::valarray;
using namespace std;
using namespace CppUnit;

class FoModelTestPop : public SpkModel<double>
{
private:
  valarray<double> _alp, _b;
  int _who;
  const int _nAlp, _nB;
  const int _nYi;

public:
  FoModelTestPop( int nAlp, int nB, int nYi )
    : _nAlp(nAlp), _nB(nB), _nYi(nYi), _alp(nAlp)
  {
    assert( nAlp == 3 );
    assert( nB   == 2 );
    assert( nYi  == 2 );
  }
protected:
  void doSetPopPar( const valarray<double> & alpIn )
  {
    assert( alpIn.size() == _nAlp );
    _alp  = alpIn;
  }
  void doSetIndPar( const valarray<double> & bIn )
  {
    _b.resize( bIn.size() );
    _b    = bIn;
  }
  void doSelectIndividual( int index )
  {
    assert( index >= 0 );
    _who = index;
  }

  void doDataMean( valarray<double> & fOut ) const
  {
    //
    // fi(alp, b) = [   2 * alp(1)    ]
    //              [ alp(2) * alp(2) ]
    //
    fOut.resize(_nYi);
    fOut[0] = 2.0 * _alp[0];
    fOut[1] = _alp[1] * _alp[1];
  }
  bool doDataMean_indPar( valarray<double> & f_bOut ) const
  {
    //
    // fi_b(alp, b) = [ 0  0 ]
    //                [ 0  0 ]
    //
    f_bOut.resize(_nYi*_nB, 0.0);
    int n = f_bOut.size();
    return false;
  }

  bool doDataMean_popPar( valarray<double> & f_alpOut ) const
  {
    //
    // f_alpOut(alp, b) = [ 2    0       0 ]
    //                    [ 0  2*alp(2)  0 ]
    //
    f_alpOut.resize(_nYi*_nAlp, 0.0);
    f_alpOut[0] = 2;
    f_alpOut[3] = 2 * _alp[1];

    return false;
  }

  void doDataVariance( valarray<double> & ROut ) const
  {
    //
    // R(alp,b) = [ b(1)   0     ]
    //            [  0    alp(2) ]
    //
    ROut.resize( _nYi*_nYi, 0.0 );
    ROut[0] = _b[0];
    ROut[1] = 0;
    ROut[2] = 0;
    ROut[3] = _alp[1];

  }
  bool doDataVariance_indPar( valarray<double> & R_bOut ) const
  {
    //
    // R_b(alp,b) = [ 1   0 ]
    //              [ 0   0 ]
    //              [ 0   0 ]
    //              [ 0   0 ]
    //
    R_bOut.resize( _nYi*_nYi * _nB, 0.0 );
    R_bOut[0] = 1;

    return true;
  }
  bool doDataVariance_popPar( valarray<double> & R_alpOut ) const
  {
    //
    // R_alp(alp,b) = [ 0   0   0 ]
    //                [ 0   0   0 ]
    //                [ 0   0   0 ]
    //                [ 0   1   0 ]
    //
    R_alpOut.resize( _nYi*_nYi * _nAlp, 0.0 );
    R_alpOut[7] = 1;

    return true;
  }

  void doIndParVariance( valarray<double> & DOut ) const
  {
    //
    // D(alp) = [ alp(2)    0    ]
    //          [ 0       alp(1) ]
    //
    DOut.resize( _nB * _nB, 0.0 );
    DOut[0] = _alp[1];
    DOut[1] = 0;
    DOut[2] = 0;
    DOut[3] = _alp[0];
  }
  bool doIndParVariance_popPar( valarray<double> & D_alpOut ) const
  {
    //
    // D_alp(alp) = [ 0  1  0 ]
    //              [ 0  0  0 ]
    //              [ 0  0  0 ]
    //              [ 1  0  0 ]
    //
    D_alpOut.resize( _nB * _nB * _nAlp, 0.0 );
    D_alpOut[3] = 1.0;
    D_alpOut[4] = 1.0;
    return true;
  }
};

void NaiveFoModelTest::setUp()
{
    // initializations
}
void NaiveFoModelTest::tearDown()
{
    // clean up
}

Test* NaiveFoModelTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("NaiveFoModelTest");

    // duplicate the following example for each test case, replacing the case name.
    suiteOfTests->addTest(new TestCaller<NaiveFoModelTest>("modelValidation", &NaiveFoModelTest::modelValidation));
    suiteOfTests->addTest(new TestCaller<NaiveFoModelTest>("foModelValidation", &NaiveFoModelTest::foModelValidation));


    return suiteOfTests;
}

void NaiveFoModelTest::modelValidation()
{
  int i;

  const int nAlp = 3;
  const int nB   = 2;
  const int nYi  = 2;

  valarray<double> alpIn( 0.0, nAlp );
  alpIn[0] = 1;
  alpIn[1] = 2;
  alpIn[2] = 3;

  valarray<double> bIn( nB );
  bIn[0] = 2;
  bIn[1] = 1;

  FoModelTestPop model( nAlp, nB, nYi );

  model.setPopPar( alpIn );
  model.selectIndividual( 0 );
  model.setIndPar( bIn );

  valarray<double> fOut;
  model.dataMean( fOut );
  CPPUNIT_ASSERT_EQUAL( 2.0 * alpIn[0], fOut[0] );
  CPPUNIT_ASSERT_EQUAL( alpIn[1] * alpIn[1], fOut[1] );
  CPPUNIT_ASSERT_EQUAL( nYi, static_cast<int>( fOut.size() ) );

  valarray<double> f_bOut;
  model.dataMean_indPar( f_bOut );
  CPPUNIT_ASSERT_EQUAL( nYi * nB, static_cast<int>( f_bOut.size() ) );
  CPPUNIT_ASSERT_MESSAGE( "f_bOut is all zero.", allZero(f_bOut) );

  valarray<double> f_alpOut;
  model.dataMean_popPar( f_alpOut );
  CPPUNIT_ASSERT_EQUAL( nYi * nAlp, static_cast<int>( f_alpOut.size() ) );
  for( i = 0; i < nYi * nAlp; i++ )
  {
    if( i == 0 )
      CPPUNIT_ASSERT_EQUAL( 2.0,            f_alpOut[i] );
    else if( i == 3 )
      CPPUNIT_ASSERT_EQUAL( 2.0 * alpIn[1], f_alpOut[i] );
    else
      CPPUNIT_ASSERT_EQUAL( 0.0,            f_alpOut[i] );
  }

  valarray<double> ROut;
  model.dataVariance( ROut );
  CPPUNIT_ASSERT_EQUAL( nYi * nYi, static_cast<int>( ROut.size() ) );
  for( i = 0; i < nYi * nYi; i++ )
  {
    if( i == 0 )
      CPPUNIT_ASSERT_EQUAL( bIn[0],    ROut[i] );
    else if( i == 3 )
      CPPUNIT_ASSERT_EQUAL( alpIn[1],  ROut[i] );
    else
      CPPUNIT_ASSERT_EQUAL( 0.0,       ROut[i] );
  }

  valarray<double> R_bOut;
  model.dataVariance_indPar( R_bOut );
  CPPUNIT_ASSERT_EQUAL( nYi * nYi * nB, static_cast<int>( R_bOut.size( ) ) );
  for( i = 0; i < nYi * nYi * nB; i++ )
  {
    if( i == 0 )
      CPPUNIT_ASSERT_EQUAL( 1.0,       R_bOut[i] );
    else
      CPPUNIT_ASSERT_EQUAL( 0.0,       R_bOut[i] );

  }

  valarray<double> R_alpOut;
  model.dataVariance_popPar( R_alpOut );
  CPPUNIT_ASSERT_EQUAL( nYi * nYi * nAlp, static_cast<int>( R_alpOut.size() ) );
  for( i = 0; i < nYi * nYi * nAlp; i++ )
  {
    if( i == 7 )
      CPPUNIT_ASSERT_EQUAL( 1.0,       R_alpOut[i] );
    else
      CPPUNIT_ASSERT_EQUAL( 0.0,       R_alpOut[i] );
  }

  valarray<double> DOut;
  model.indParVariance( DOut );
  CPPUNIT_ASSERT_EQUAL( nB * nB, static_cast<int>( DOut.size() ) );
  for( i = 0; i < nB * nB; i++ )
  {
    if( i == 0 )
      CPPUNIT_ASSERT_EQUAL( alpIn[1], DOut[i] );
    else if( i == 3 )
      CPPUNIT_ASSERT_EQUAL( alpIn[0], DOut[i] );
    else
      CPPUNIT_ASSERT_EQUAL( 0.0,      DOut[i] );
  }

  valarray<double> D_alpOut;
  model.indParVariance_popPar( D_alpOut );
  CPPUNIT_ASSERT_EQUAL( nB * nB * nAlp, static_cast<int>( D_alpOut.size() ) );
  for( i = 0; i < nB * nB * nAlp; i++ )
  {
    if( i == 3 || i == 4 )
      CPPUNIT_ASSERT_EQUAL( 1.0,      D_alpOut[i] );
    else
      CPPUNIT_ASSERT_EQUAL( 0.0,      D_alpOut[i] );
  }
}

void NaiveFoModelTest::foModelValidation()
{
  int i;
  //
  // The following intialization of the sizes of alp, b and yi are all 
  // the same as of modelValidation().
  //
  const int nAlp = 3;
  const int nB   = 2;
  const int nYi  = 2;
  const int nIndividuals = 2;

  valarray<double> alpIn( 0.0, nAlp );

  valarray<double> bIn( nB );

  valarray<double> bStep( nB );
  bStep = 0.1;

  //
  // This all zero vector will be used to feed into FO model.
  //
  valarray<double> bZeros( nB );
  bZeros = 0.0;

  //
  // Create an object of the original population model.
  // This model will be used to obtain values to compare against.
  //
  FoModelTestPop model( nAlp, nB, nYi );

  //
  // Create an object of FoModel.
  // [ Comment by Sachiko ]
  // Why does this constructor takes alpIn?
  // Why step size?
  //
  NaiveFoModel foModel( &model, bStep ); 

  for( int who=0; who<2; who++ )
  {
    alpIn[0] = 1 + who;
    alpIn[1] = 2 + who;
    alpIn[2] = 3 + who;

    bIn[0] = 2 + who;
    bIn[1] = 1 + who;

    //
    // Set the current parameters for the FO model.
    //
    foModel.setPopPar( alpIn );
    foModel.selectIndividual( who );
    foModel.setIndPar( bIn );

    //
    // Set the current parameters for the original model.
    //
    model.setPopPar( alpIn );
    model.setIndPar( bZeros );
    model.selectIndividual(0);

    //==============================================
    //            f(alp, b)
    //==============================================
    // Variables used for f()
    valarray<double> FO_fOut, fOut, f_bOut, original_fOut;


    //
    // Evaluate the FO approx of f(alp,b).
    //
    foModel.dataMean( FO_fOut );

    //
    // Evaluate f(alp,0) + df(alp,0)_b * b using the orignal model.
    //
    model.dataMean( fOut );
    model.dataMean_indPar( f_bOut );

    original_fOut = fOut + (DoubleMatrix(f_bOut, nB) * DoubleMatrix(bIn, 1) ).toValarray();

    for( i = 0; i < static_cast<int>( original_fOut.size() ); i++ )
    {
      CPPUNIT_ASSERT_EQUAL( original_fOut[i], FO_fOut[i] );
    }

    //==============================================
    //            R(alp, b)
    //==============================================
    // Variables used for R()
    valarray<double> FO_ROut, original_ROut;

    //
    // Evaluate the approx of R(alp,b).
    //
    foModel.dataVariance( FO_ROut );

    //
    // Evaluate R(alp,0) using the orignal model.
    //
    model.dataVariance( original_ROut );

    for( i = 0; i < static_cast<int>( original_ROut.size() ); i++ )
      CPPUNIT_ASSERT_EQUAL( original_ROut[i], FO_ROut[i] );
  }
}
