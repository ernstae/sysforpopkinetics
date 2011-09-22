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
 * File: ppkaOptTest.cpp
 *
 *
 * Unit test for the function ppkaOpt.
 *
 * Author: Mitch Watrous
 * Later updated by Sachiko
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/ppkaOpt.h"
#include "ppkaOptTest.h"
#include "../../../spk/SpkException.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/randNormal.h"

using SPK_VA::valarray;
using namespace CppUnit;

void ppkaOptTest::setUp()
{
    // initializations
}
void ppkaOptTest::tearDown()
{
    // clean up
}

Test* ppkaOptTest::suite()
{
	TestSuite *suiteOfTests = new TestSuite( "ppkaOptTest" );

    suiteOfTests->addTest(new TestCaller<ppkaOptTest>("modifiedLaplaceTest", &ppkaOptTest::modifiedLaplaceTest));
    suiteOfTests->addTest(new TestCaller<ppkaOptTest>("expectedHessianTest", &ppkaOptTest::expectedHessianTest));

    return suiteOfTests;
}

void ppkaOptTest::modifiedLaplaceTest()
{
    ppkaOptExampleTest(MODIFIED_LAPLACE);
    ppkaOptZeroIterationsTest(MODIFIED_LAPLACE);
    ppkaOptFixedAlpTest(MODIFIED_LAPLACE);
}
void ppkaOptTest::expectedHessianTest()
{
    ppkaOptExampleTest(EXPECTED_HESSIAN);
    ppkaOptZeroIterationsTest(EXPECTED_HESSIAN);
    ppkaOptFixedAlpTest(EXPECTED_HESSIAN);
}
/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

#include "../../../spk/lTilde.h"
#include "../../../spk/pi.h"
#include "../../../spk/SpkModel.h"

#include <iostream>
#include <iomanip>
#include <string>
#include <cmath>


/*------------------------------------------------------------------------
 * Namespace Declarations
 *------------------------------------------------------------------------*/

using std::string;


/*------------------------------------------------------------------------
 * Local Function Declarations
 *------------------------------------------------------------------------*/

static void doTheTest( bool ok,
                       double dLTildeOut,
                       double dLTildeKnown,
                       const DoubleMatrix& dvecEpsilon,
                       const DoubleMatrix& dvecAlpLow,
                       const DoubleMatrix& dvecAlpUp,
                       const DoubleMatrix& dvecAlpOut,
                       const DoubleMatrix& dvecAlpHat,
                       const DoubleMatrix& dvecBLow,
                       const DoubleMatrix& dvecBUp,
                       const DoubleMatrix& dmatBOut,
                       const DoubleMatrix& dmatBHat,
                       const DoubleMatrix& drowLTilde_alpOut,
                       const DoubleMatrix& drowLTilde_alpKnown,
                       const DoubleMatrix& dmatLTilde_alp_alpOut,
                       const DoubleMatrix& dmatLTilde_alp_alpKnown );


/*************************************************************************
 *
 * Function: ppkaOptExampleTest
 *
 *
 * This test implements the example problem from the ppkaOpt specification. 
 *
 *************************************************************************/

class UserModelPpkaOptExampleTest : public SpkModel<double>
{
    valarray<double> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nYi;
public:
    UserModelPpkaOptExampleTest(int nA, int nB, int nYi)
      :
      _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
    {};    
    ~UserModelPpkaOptExampleTest(){};
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
        ret.resize(_nB);
        ret[0] = _a[1];
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        //
        // D_alp = [ 0  1 ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = 1.0;
        return true;
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {
        //
        // Dinv = [ 1.0 / alp[1] ]
        //
        assert(_a[1] != 0.0);
        ret.resize(_nB * _nB);
        ret[0] = ( 1.0 / _a[1] );
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        // Dinv_alp = [ 0    -alp[1]^(-2) ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = -1.0 / (_a[1]*_a[1]);
        return true;
    }
    void doDataMean( valarray<double>& ret ) const
    {
        //
        // f = [ alp[0]+b[0] ]
        //
        ret.resize(_nYi);
        ret[0] = ( _a[0] + _b[0] );
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
        // f_b = [ 1 ]
        //
        ret.resize(_nYi * _nB);
        ret[0] = 1.0;
        return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        //
        // R = [ 1 ]
        //
        ret.resize(_nB*_nB);
        ret[0] = 1.0;
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        //
        // R_alp = [ 0   0 ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
        return false;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        //
        // R_b = [ 0 ]
        //
        ret.resize(_nB *_nB);
        ret[0] = 0.0;
        return false;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
        //
        // Rinv = [ 1 ]
        //
        ret.resize(_nB * _nB);
        ret[0] = 1.0;
    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        // Rinv_alp = [ 0  0 ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
        return false;
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
        //
        // Rinv_b = [ 0 ]
        //
        ret.resize(_nB * _nB * _nB);
        ret[0] = 0.0;
        return false;
    }   

};


void ppkaOptTest::ppkaOptExampleTest(enum Objective whichObjective)
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i, k;

  //preTestPrinting( "Specification Example" );


  // Number of individuals.
  const int nInd = 10;

  // Number of measurements per individual (same for all)
  const int nYi = 1;

  // Number of measurements in total
  const int nY = nInd * nYi;

  const int nAlp = 2;

  const int nB = 1;

  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  UserModelPpkaOptExampleTest model( nAlp, nB, nYi );


  //------------------------------------------------------------
  // Quantities that define the problem.
  //------------------------------------------------------------

  // Mean and variance of the true transfer rate, betaTrue.
  double meanBetaTrue = 1.0;
  double varBetaTrue  = 5.0;



  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  // Measurement values, y.
  DoubleMatrix dvecY( nY, 1 );
  double* pdYData = dvecY.data();

  // Number of measurements for each individual. 
  DoubleMatrix dvecN( nInd, 1 );
  dvecN.fill( (double) 1 );

  // These will hold the generated values for the true measurement 
  // noise, eTrue, and the true random population parameters, bTrue.
  double eTrue;
  double bTrue;

  // Mean, variance, and standard deviation of eTrue and bTrue.
  double meanETrue = 0.0;
  double varETrue  = 1.0;
  double sdETrue   = sqrt( varETrue );
  double meanBTrue = 0.0;
  double varBTrue  = varBetaTrue;
  double sdBTrue   = sqrt( varBTrue );

  // Compute the measurements for each individual.
  valarray<double> sdECov(nY*nY);
  sdECov[ slice( 0, nY, nY+1 ) ] = sdETrue;

  valarray<double> sdBCov(nY*nY);
  sdBCov[ slice( 0, nY, nY+1 ) ] = sdBTrue;

  // Set the measurements for each individual.
  //
  // Note: these values were generated on a 32-bit Pentium machine
  // using the following code.
  //
  //     int seed = 2;
  //     srand(seed);
  //
  //     valarray<double> sdECov(nY*nY);
  //     sdECov[ slice( 0, nY, nY+1 ) ] = sdETrue;
  //
  //     valarray<double> sdBCov(nY*nY);
  //     sdBCov[ slice( 0, nY, nY+1 ) ] = sdBTrue;
  //
  //     y = meanBTrue + randNormal( sdBCov, nY ) + randNormal( sdECov, nY );
  //
  // The values generated on a 64-bit Athalon machine were different
  // and their optimal paramter values could not be calculated.  So,
  // these values have been set explicitly here to ensure they're the
  // same on all machines.
  //
  valarray<double> y( nY );
  y[0] = 1.88758;
  y[1] = -1.03471;
  y[2] = 1.18851;
  y[3] = -0.476253;
  y[4] = -1.45167;
  y[5] = -0.797979;
  y[6] = -0.0825739;
  y[7] = 3.04214;
  y[8] = 1.48168;
  y[9] = -1.29312;

  copy( &(y[0]), &(y[0])+nY, pdYData );
    
  //------------------------------------------------------------
  // Quantities related to the fixed population parameter, alp.
  //------------------------------------------------------------


  DoubleMatrix dvecAlpTrue( nAlp, 1 );
  DoubleMatrix dvecAlpLow ( nAlp, 1 );
  DoubleMatrix dvecAlpUp  ( nAlp, 1 );
  DoubleMatrix dvecAlpIn  ( nAlp, 1 );
  DoubleMatrix dvecAlpOut ( nAlp, 1 );
  DoubleMatrix dvecAlpStep( nAlp, 1 );

  double* pdAlpTrueData = dvecAlpTrue.data();
  double* pdAlpLowData  = dvecAlpLow .data();
  double* pdAlpUpData   = dvecAlpUp  .data();
  double* pdAlpInData   = dvecAlpIn  .data();
  double* pdAlpStepData = dvecAlpStep.data();

  // Set the values associated with alp(1).
  pdAlpTrueData[ 0 ] = meanBetaTrue;
  pdAlpLowData [ 0 ] = -10.0;
  pdAlpUpData  [ 0 ] = 10.0;
  pdAlpInData  [ 0 ] = -1.0;
  pdAlpStepData[ 0 ] = 1.0e-2;

  // Set the values associated with alp(2).
  pdAlpTrueData[ 1 ] = varBetaTrue;
  pdAlpLowData [ 1 ] = 1.0e-3;
  pdAlpUpData  [ 1 ] = 100.0;
  pdAlpInData  [ 1 ] = 0.5;
  pdAlpStepData[ 1 ] = 1.0e-2;
  

  //------------------------------------------------------------
  // Quantities related to the random population parameters, b.
  //------------------------------------------------------------


  DoubleMatrix dvecBLow ( nB, 1 );
  DoubleMatrix dvecBUp  ( nB, 1 );
  DoubleMatrix dvecBStep( nB, 1 );

  dvecBLow .fill( -1.5e+1 );
  dvecBUp  .fill( +1.0e+1 );
  dvecBStep.fill(  1.0e-2 );

  DoubleMatrix dmatBIn ( nB, nInd );
  DoubleMatrix dmatBOut( nB, nInd );

  dmatBIn.fill( 1.0 );


  //------------------------------------------------------------
  // Quantities related to the population objective function.
  //------------------------------------------------------------

  double dLTildeOut;

  DoubleMatrix drowLTilde_alpOut    ( 1,    nAlp );
  DoubleMatrix dmatLTilde_alp_alpOut( nAlp, nAlp );


  //------------------------------------------------------------
  // Remaining inputs to ppkaOpt.
  //------------------------------------------------------------

  Optimizer popOptimizer( 1.0e-6, 5, 0 );
  Optimizer indOptimizer( 1.0e-6, 40, 0 );

  // Set these to exercise the warm start capabilities of ppkaOpt.
  popOptimizer.setThrowExcepIfMaxIter( false );
  popOptimizer.setSaveStateAtEndOfOpt( true );

  
  //------------------------------------------------------------
  // Optimize the population objective function.
  //------------------------------------------------------------

  bool ok;
  try
  {
    while( true )
    {
      ppkaOpt( model,
               whichObjective,
               dvecN,
               dvecY,
               popOptimizer,
               dvecAlpLow,
               dvecAlpUp,
               dvecAlpIn,
               &dvecAlpOut,
               dvecAlpStep,
               indOptimizer,
               dvecBLow,
               dvecBUp,
               dmatBIn,
               &dmatBOut,
               dvecBStep,
               &dLTildeOut,
               &drowLTilde_alpOut,
               &dmatLTilde_alp_alpOut );

      // Exit this loop if the maximum number of iterations was
      // not exceeded, i.e., if the optimization was successful.
      if( !popOptimizer.getIsTooManyIter() )
        break;

      // Set this so that ppkaOpt performs a warm start when it
      // is called again.
      popOptimizer.setIsWarmStart( true );
    }

    ok = true;
  }
  catch( const SpkException& e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT_MESSAGE( "ppkaOpt failed!", false );
    }
  catch(...)
  {
      CPPUNIT_ASSERT_MESSAGE( "ppkaOpt failed for unknown reasons!", false );
  }


  //------------------------------------------------------------
  // Known values.
  //------------------------------------------------------------

  //************************************************************
  // Note: equations for the known (analytic) values computed 
  // here are derived in "An Introduction to Mixed Effects
  // Modeling and Marginal Likelihood Estimation with a 
  // Pharmacokinetic Example", B. M. Bell, Applied Physics
  // Laboratory, University of Washington, May 25, 2000.
  //************************************************************

  CPPUNIT_ASSERT_EQUAL( nInd, nY );
  CPPUNIT_ASSERT( nY != 1 );

  // Compute the mean of the data, yBar.
  double yBar = 0.0;
  for ( i = 0; i < nInd; i++ )
  {
    yBar += pdYData[ i ];
  }
  yBar /= nInd;

  // Compute the sample variance, sSquared.
  double sSquared = 0.0;
  for ( i = 0; i < nInd; i++ )
  {
    sSquared += pow( ( pdYData[ i ] - yBar ), 2 );
  }
  sSquared /= ( nInd - 1 );

  // The value for alpHat(1) and alpHat(2) are contained in
  // section 9 of the above reference.
  DoubleMatrix dvecAlpHat( nAlp, 1 );
  double* pdAlpHatData = dvecAlpHat.data();
  pdAlpHatData[ 0 ] = yBar;
  pdAlpHatData[ 1 ] = sSquared * (nInd - 1) / nInd - 1.0;

  // Compute bHat_i(alpHat) using equation (14) of the above reference.      
  DoubleMatrix dmatBHat( nB, nInd );
  double* pdBHatData = dmatBHat.data();
  for ( i = 0; i < nInd; i++ )
  {
    for ( k = 0; k < nB; k++ )
    {
      pdBHatData[ k + i * nB ] = ( pdYData[ i ] - pdAlpHatData[ 0 ] ) 
        / ( 1.0 + 1.0 / pdAlpHatData[ 1 ] );
    }
  }
  double* pdAlpOutData  = dvecAlpOut.data();

  // Compute ( 1 + alpOut(2) ).
  double onePlusAlp2 = 1.0 + pdAlpOutData[ 1 ];

  // Compute the sums involving ( y_i - alpOut(1) ).
  double yMinAlp1;
  double sumYMinAlp1    = 0.0;
  double sumYMinAlp1Sqd = 0.0;
  for ( i = 0; i < nInd; i++ )
  {
    yMinAlp1        = pdYData[ i ] - pdAlpOutData[ 0 ];
    sumYMinAlp1    += yMinAlp1;
    sumYMinAlp1Sqd += pow( yMinAlp1, 2 );
  }

  // Compute the known value for LTilde(alp) = -log[p(y|alp)]
  // using equation (17) of the above reference.
  double dLTildeKnown = sumYMinAlp1Sqd / ( 2.0 * ( onePlusAlp2 ) ) 
    + nInd / 2.0 * log( 2.0 * PI * ( onePlusAlp2 ) );

  // The value for LTilde_alp_alp(alp) was determined by taking the  
  // derivative of equation (17) of the above reference, i.e.,
  //
  //                          partial
  //     LTilde_alp(alp) = ------------- [- log[p(y|alp)] ] .
  //                        partial alp
  //
  DoubleMatrix drowLTilde_alpKnown( 1, nAlp );
  double* pdLTilde_alpKnownData = drowLTilde_alpKnown.data();
  pdLTilde_alpKnownData[ 0 ] = - sumYMinAlp1 / onePlusAlp2;
  pdLTilde_alpKnownData[ 1 ] = 0.5 / onePlusAlp2 
                                * ( - sumYMinAlp1Sqd / onePlusAlp2 + nInd );

  // The value for LTilde_alp_alp(alp) was determined by taking the second 
  // derivative of equation (17) of the above reference, i.e.,
  //
  //                             partial       partial   
  //     LTilde_alp_alp(alp) = ------------- ------------- [- log[p(y|alp)] ] .
  //                            partial alp   partial alp
  //
  DoubleMatrix dmatLTilde_alp_alpKnown( nAlp, nAlp );
  double* pdLTilde_alp_alpKnownData = dmatLTilde_alp_alpKnown.data();
  pdLTilde_alp_alpKnownData[ 0 ] = nInd / ( onePlusAlp2 );
  pdLTilde_alp_alpKnownData[ 1 ] = pow( onePlusAlp2, -2 ) * sumYMinAlp1;
  pdLTilde_alp_alpKnownData[ 2 ] = pdLTilde_alp_alpKnownData[ 1 ];
  pdLTilde_alp_alpKnownData[ 3 ] = 0.5 * pow( onePlusAlp2, -2 ) 
                                    * (2.0 / onePlusAlp2 * sumYMinAlp1Sqd 
                                    - nInd );


  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  doTheTest(  ok,
              dLTildeOut,
              dLTildeKnown,
              indOptimizer.getEpsilon(),
              popOptimizer.getEpsilon(),
              dvecAlpLow,
              dvecAlpUp,
              dvecAlpOut,
              dvecAlpHat,
              dvecBLow,
              dvecBUp,
              dmatBOut,
              dmatBHat,
              drowLTilde_alpOut,
              drowLTilde_alpKnown,
              dmatLTilde_alp_alpOut,
              dmatLTilde_alp_alpKnown );
  
}


/*************************************************************************
 *
 * Function: ppkaOptZeroIterationsTest
 *
 *
 * This test calls ppkaOpt with zero iterations at the population level,
 * with zero iterations at the individual level, and with zero iterations 
 * at both the population and the individual levels.
 *
 * At the individual level this test implements the example problem from 
 * the mapOpt specification. 
 *
 *************************************************************************/

class UserModelPpkaOptZeroIterationsTest : public SpkModel<double>
{
    valarray<double> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nYi;
public:
    UserModelPpkaOptZeroIterationsTest(int nA, int nB, int nYi)
      : _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
    {}; 
    ~UserModelPpkaOptZeroIterationsTest(){};
private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<double>& aval)
    {
      assert(aval.size() == _nA);
        _a = aval;
    }
    void doSetIndPar(const  valarray<double>& bval)
    {
      assert(bval.size() == _nB);
        _b = bval;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
        //
        //              / 1    0  \ 
        //     D(alp) = |         |  .
        //              \ 0   1/2 /
        //
        ret.resize(_nB * _nB);
        ret[0] = 1.0;
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = 0.5;
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        //
        //                 /  0  \ 
        //    D_alp(alp) = |  0  |  .
        //                 |  0  | 
        //                 \  0  / 
        //
        ret.resize(_nB * _nB * _nA);
        for( int i=0; i<_nB * _nB * _nA; i++ )
          ret[i] = 0.0;
        return false;
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {
        //
        //              / 1    0 \ 
        //     D(alp) = |        |  .
        //              \ 0    2 /
        //
        ret.resize(_nB * _nB);
        ret[0] = 1.0;
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = 2.0;
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        //                    /  0  \ 
        //    D^-1_alp(alp) = |  0  |  .
        //                    |  0  | 
        //                    \  0  / 
        //
        ret.resize(_nB * _nB * _nA);
        for( int i=0; i<_nB * _nB * _nA; i++ )
          ret[i] = 0.0;
        return false;
    }
    void doDataMean( valarray<double>& ret ) const
    {
        //
        //                 / b(2) \ 
        //     f(alp, b) = |      |  .
        //                 \ b(2) /
        //
        ret.resize(_nYi);
        ret[0] = _b[1];
        ret[1] = _b[1];
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
        //
        //                     / 0 \ 
        //     f_alp(alp, b) = |   |  .
        //                     \ 0 /
        //
        ret.resize(_nYi * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
        return false;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        //
        //                   / 0   1 \ 
        //     f_b(alp, b) = |       |  .
        //                   \ 0   1 /
        //
        ret.resize(_nYi * _nB);
        ret[0] = 0.0;
        ret[1] = 0.0;
        ret[2] = 1.0;
        ret[3] = 1.0;
        return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        //
        //                 /  exp[b(1)]     0  \ 
        //     R(alp, b) = |                   |  .
        //                 \  0      exp[b(1)] / 
        //
        ret.resize(_nYi * _nYi);
        ret[0] = exp( _b[0] );
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = exp( _b[0] );
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        //
        //                    /  0  \ 
        //    R_alp(alp, b) = |  0  |  .
        //                    |  0  | 
        //                    \  0  / 
        //
        ret.resize(_nYi * _nYi * _nA);
        for( int i=0; i<_nYi *_nYi * _nA; i++ )
          ret[i] = 0.0;
        return false;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        //
        //                   /  exp[b(1)]     0  \ 
        //     R_b(alp, b) = |  0             0  |   .
        //                   |  0             0  | 
        //                   \  exp[b(1)]     0  / 
        //
        ret.resize(_nYi * _nYi * _nB);
        for( int i=0; i<_nYi *_nYi * _nB; i++ )
          ret[i] = 0.0;
        ret[0] = exp( _b[0] );
        ret[3] = exp( _b[0] );
        return true;
    }   
    void doDataVarianceInv( valarray<double>& ret ) const
    {
        //
        //                    /  1.0/exp[b(1)]     0  \ 
        //     R(alp, b)^-1 = |                        |  .
        //                    \  0      1.0/exp[b(1)] / 
        //
        ret.resize(_nYi * _nYi);
        ret[0] = 1.0 / exp( _b[0] );
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = 1.0 / exp( _b[0] );
    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        //                         /  0  \ 
        //    R^(-1)_alp(alp, b) = |  0  |  .
        //                         |  0  | 
        //                         \  0  / 
        //
        ret.resize(_nYi * _nYi * _nA);
        for( int i=0; i<_nYi *_nYi * _nA; i++ )
          ret[i] = 0.0;
        return false;
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
        //
        //                        /  -1.0 / exp[b(1)]     0  \ 
        //     R^(-1)_b(alp, b) = |     0                 0  |   .
        //                        |     0                 0  | 
        //                        \  -1.0 / exp[b(1)]     0  / 
        //
        ret.resize(_nYi * _nYi * _nB);
        for( int i=0; i<_nYi *_nYi * _nB; i++ )
          ret[i] = 0.0;
        ret[0] = -1.0 / exp( _b[0] );
        ret[3] = -1.0 / exp( _b[0] );
        return true;
    }   
};


void ppkaOptTest::ppkaOptZeroIterationsTest(enum Objective whichObjective)
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // Number of individuals.
  const int nInd = 3;

  // Number of measurements for each individual. 
  const int nYPerInd = 2;

  // Number of measurements.
  const int nY = nInd * nYPerInd;

  const int nAlp = 1;

  const int nB = 2;

  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  UserModelPpkaOptZeroIterationsTest model( nAlp, nB, nYPerInd );


  //------------------------------------------------------------
  // Quantities related to the individuals in the population.
  //------------------------------------------------------------

  DoubleMatrix dvecN( nInd, 1 );
  dvecN.fill( (double) nYPerInd );


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  // Measurement values, y.
  DoubleMatrix dvecY( nY, 1 );
  dvecY.fill( 2.0 );


  //------------------------------------------------------------
  // Quantities related to the fixed population parameter, alp.
  //------------------------------------------------------------

  DoubleMatrix dvecAlpLow ( nAlp, 1 );
  DoubleMatrix dvecAlpUp  ( nAlp, 1 );
  DoubleMatrix dvecAlpIn  ( nAlp, 1 );
  DoubleMatrix dvecAlpOut ( nAlp, 1 );
  DoubleMatrix dvecAlpStep( nAlp, 1 );

  double* pdAlpLowData  = dvecAlpLow .data();
  double* pdAlpUpData   = dvecAlpUp  .data();
  double* pdAlpInData   = dvecAlpIn  .data();
  double* pdAlpOutData  = dvecAlpOut .data();
  double* pdAlpStepData = dvecAlpStep.data();

  // Set the values associated with alp(1).
  pdAlpLowData [ 0 ] = -10.0;
  pdAlpUpData  [ 0 ] = 10.0;
  pdAlpInData  [ 0 ] = 5.0;
  pdAlpStepData[ 0 ] = 1.0e-2;

  
  //------------------------------------------------------------
  // Quantities related to the random population parameters, b.
  //------------------------------------------------------------

  DoubleMatrix dvecBLow ( nB, 1 );
  DoubleMatrix dvecBUp  ( nB, 1 );
  DoubleMatrix dvecBStep( nB, 1 );

  dvecBLow .fill( -4.0 );
  dvecBUp  .fill(  4.0 );
  dvecBStep.fill(  0.001 );

  DoubleMatrix dmatBIn ( nB, nInd );
  DoubleMatrix dmatBOut( nB, nInd );

  dmatBIn.fill( 2.0 );


  //------------------------------------------------------------
  // Quantities related to the population objective function.
  //------------------------------------------------------------

  double dLTildeOut;

  DoubleMatrix drowLTilde_alpOut    ( 1,    nAlp );
  DoubleMatrix dmatLTilde_alp_alpOut( nAlp, nAlp );


  //------------------------------------------------------------
  // Remaining inputs to ppkaOpt.
  //------------------------------------------------------------

  Optimizer indOptimizer( 1.0e-6, 0, 0 );
  Optimizer popOptimizer( 1.0e-6, 0, 0 );


  //------------------------------------------------------------
  // Quantities related to the known values.
  //------------------------------------------------------------

  DoubleMatrix dvecAlpHat( nAlp, 1    );
  DoubleMatrix dmatBHat  ( nB,   nInd );

  double* pdBHatData = dmatBHat.data();

  // Inputs to lTilde.
  bool withD = true;
  double dLTildeKnown;
  DoubleMatrix drowLTilde_alpKnown( 1, nAlp );
  DoubleMatrix* pdmatNull = 0;

  // The second derivatives should all be zero.
  DoubleMatrix dmatLTilde_alp_alpKnown( nAlp, nAlp );
  dmatLTilde_alp_alpKnown.fill( 0.0 );


  //------------------------------------------------------------
  // Test zero iterations at all possible combinations of levels.
  //------------------------------------------------------------

  int j;

  for ( int i = 0; i < 3; i++ )
  {
    switch ( i )
    {

    //----------------------------------------------------------
    // Zero iterations at the population level.
    //----------------------------------------------------------

    case 0:

      //preTestPrinting( "Zero Population Level Iterations" );

      indOptimizer.setNMaxIter( 40 );     // Individual level.
      popOptimizer.setNMaxIter(  0 );     // Population level.

      // Since the number of iterations for the population level is
      // zero, alpOut and alpHat should both be equal to alpIn.
      dvecAlpHat = dvecAlpIn;

      // For each individual, the minimum value for MapObj(b) occurs 
      // when b(1) = 0 and b(2) = 1. 
      for ( j = 0; j < nInd; j++ )
      {
        pdBHatData[ 0 + j * nB ] = 0.0;
        pdBHatData[ 1 + j * nB ] = 1.0;
      }

      break;


    //----------------------------------------------------------
    // Zero iterations at the individual level.
    //----------------------------------------------------------

    case 1:

      //preTestPrinting( "Zero Individual Level Iterations" );

      indOptimizer.setNMaxIter(  0 );     // Individual level.
      popOptimizer.setNMaxIter( 40 );     // Population level.

      // Since f(alp, b), R(alp, b), and D(alp) are defined below
      // such that they are all independent of alp, the optimizer
      // will return alpIn as the value for alpOut.
      dvecAlpHat = dvecAlpIn;

      // Since the number of iterations for the individual level is
      // zero, bOut and bHat should both be equal to bIn.
      dmatBHat = dmatBIn;

      break;


    //----------------------------------------------------------
    // Zero iterations at both levels.
    //----------------------------------------------------------

    case 2:

      //preTestPrinting( "Zero Iterations at Both Levels" );

      indOptimizer.setNMaxIter(  0 );     // Individual level.
      popOptimizer.setNMaxIter(  0 );     // Population level.

      // Since the number of iterations for the population level is
      // zero, alpOut and alpHat should both be equal to alpIn.
      dvecAlpHat = dvecAlpIn;

      // Since the number of iterations for the individual level is
      // zero, bOut and bHat should both be equal to bIn.
      dmatBHat = dmatBIn;

      break;
    }


    //----------------------------------------------------------
    // Optimize the population objective function.
    //----------------------------------------------------------

    bool okPpkaOpt;
    try{
        ppkaOpt( 
            model,
            whichObjective,
            dvecN,
            dvecY,
            popOptimizer,
            dvecAlpLow,
            dvecAlpUp,
            dvecAlpIn,
            &dvecAlpOut,
            dvecAlpStep,
            indOptimizer,
            dvecBLow,
            dvecBUp,
            dmatBIn,
            &dmatBOut,
            dvecBStep,
            &dLTildeOut,
            &drowLTilde_alpOut,
            &dmatLTilde_alp_alpOut );
        okPpkaOpt = true;
    }
    catch( const SpkException& e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT_MESSAGE( "ppkaOpt failed!", false );
    }

    catch(...)
    {
        CPPUNIT_ASSERT_MESSAGE( "ppkaOpt failed for unknown reasons!", false );
    }

    //----------------------------------------------------------
    // Compute the known values for LTilde and LTilde_alp.
    //----------------------------------------------------------

    bool okLTilde;
    try{
        lTilde(  model,
                 whichObjective,
                 dvecY,
                 dvecN,
                 indOptimizer,
                 dvecAlpHat,
                 dvecBLow,
                 dvecBUp,
                 dvecBStep,
                 dmatBIn,
                 pdmatNull,
                 &dLTildeKnown,
                 &drowLTilde_alpKnown);
        okLTilde = true;
    }
    catch( const SpkException& e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT_MESSAGE( "ppkaOpt failed!", false );
    }
    catch(...)
    {
        CPPUNIT_ASSERT_MESSAGE( "ppkaOpt failed for unknown reasons!", false );
    }

    //----------------------------------------------------------
    // Compare the results.
    //----------------------------------------------------------

    doTheTest(  okPpkaOpt,
                dLTildeOut,
                dLTildeKnown,
                indOptimizer.getEpsilon(),
                popOptimizer.getEpsilon(),
                dvecAlpLow,
                dvecAlpUp,
                dvecAlpOut,
                dvecAlpHat,
                dvecBLow,
                dvecBUp,
                dmatBOut,
                dmatBHat,
                drowLTilde_alpOut,
                drowLTilde_alpKnown,
                dmatLTilde_alp_alpOut,
                dmatLTilde_alp_alpKnown );

  }
  
}


/*************************************************************************
 *
 * Function: ppkaOptFixedAlpTest
 *
 *
 * This test implements the example problem from the ppkaOpt specification
 * with one extra alp components being fixed. 
 *
 *************************************************************************/

class UserModelPpkaOptFixedAlpTest : public SpkModel<double>
{
    valarray<double> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nYi;
public:
    UserModelPpkaOptFixedAlpTest(int nA, int nB, int nYi)
      :
      _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
    {
      CPPUNIT_ASSERT_EQUAL( 3, nA );
      CPPUNIT_ASSERT_EQUAL( 1, nB );
    };    
    ~UserModelPpkaOptFixedAlpTest(){};
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
        ret.resize(_nB);
        ret[0] = _a[1];
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        //
        // D_alp = [ 0  1  0 ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = 1.0;
	ret[2] = 0.0;
        return true;
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {
        //
        // Dinv = [ 1.0 / alp[1] ]
        //
        assert(_a[1] != 0.0);
        ret.resize(_nB * _nB);
        ret[0] = ( 1.0 / _a[1] );
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        // Dinv_alp = [ 0    -alp[1]^(-2)   0 ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = -1.0 / (_a[1]*_a[1]);
	ret[2] = 0.0;
        return true;
    }
    void doDataMean( valarray<double>& ret ) const
    {
        //
        // f = [ alp[0]+b[0] ]
        //
        ret.resize(_nYi);
        ret[0] = ( _a[0] + _b[0] );
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
        //
        // f_alp = [ 1   0   0 ]
        //
        ret.resize(_nYi * _nA);
        ret[0] = 1.0;
        ret[1] = 0.0;
        ret[2] = 0.0;
        return true;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        //
        // f_b = [ 1 ]
        //
        ret.resize(_nYi * _nB);
        ret[0] = 1.0;
        return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        //
        // R = [ 1 ]
        //
        ret.resize(_nB*_nB);
        ret[0] = 1.0;
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        //
        // R_alp = [ 0  0  0 ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
        ret[2] = 0.0;
        return false;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        //
        // R_b = [ 0 ]
        //
        ret.resize(_nB *_nB);
        ret[0] = 0.0;
        return false;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
        //
        // Rinv = [ 1 ]
        //
        ret.resize(_nB * _nB);
        ret[0] = 1.0;
    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        // Rinv_alp = [ 0  0  0 ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
	ret[2] = 0.0;
        return false;
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
        //
        // Rinv_b = [ 0 ]
        //
        ret.resize(_nB * _nB * _nB);
        ret[0] = 0.0;
        return false;
    }   

};


void ppkaOptTest::ppkaOptFixedAlpTest(enum Objective whichObjective)
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i, k;

  //preTestPrinting( "Specification Example" );


  // Number of individuals.
  const int nInd = 10;

  // Number of measurements per individual (same for all)
  const int nYi = 1;

  // Number of measurements in total
  const int nY = nInd * nYi;

  // The model is really defined for 2 fixed effects, so
  // just add a dummy (fixed element) at the end of the vector.
  const int nAlp = 2 + 1;

  const int nB = 1;

  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  UserModelPpkaOptFixedAlpTest model( nAlp, nB, nYi );


  //------------------------------------------------------------
  // Quantities that define the problem.
  //------------------------------------------------------------

  // Mean and variance of the true transfer rate, betaTrue.
  double meanBetaTrue = 1.0;
  double varBetaTrue  = 5.0;



  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  // Measurement values, y.
  DoubleMatrix dvecY( nY, 1 );
  double* pdYData = dvecY.data();

  // Number of measurements for each individual. 
  DoubleMatrix dvecN( nInd, 1 );
  dvecN.fill( (double) 1 );

  // These will hold the generated values for the true measurement 
  // noise, eTrue, and the true random population parameters, bTrue.
  double eTrue;
  double bTrue;

  // Mean, variance, and standard deviation of eTrue and bTrue.
  double meanETrue = 0.0;
  double varETrue  = 1.0;
  double sdETrue   = sqrt( varETrue );
  double meanBTrue = 0.0;
  double varBTrue  = varBetaTrue;
  double sdBTrue   = sqrt( varBTrue );

  // Compute the measurements for each individual.
  valarray<double> sdECov(nY*nY);
  sdECov[ slice( 0, nY, nY+1 ) ] = sdETrue;

  valarray<double> sdBCov(nY*nY);
  sdBCov[ slice( 0, nY, nY+1 ) ] = sdBTrue;

  // Set the measurements for each individual.
  //
  // Note: these values were generated on a 32-bit Pentium machine
  // using the following code.
  //
  //     int seed = 2;
  //     srand(seed);
  //
  //     valarray<double> sdECov(nY*nY);
  //     sdECov[ slice( 0, nY, nY+1 ) ] = sdETrue;
  //
  //     valarray<double> sdBCov(nY*nY);
  //     sdBCov[ slice( 0, nY, nY+1 ) ] = sdBTrue;
  //
  //     y = meanBTrue + randNormal( sdBCov, nY ) + randNormal( sdECov, nY );
  //
  // The values generated on a 64-bit Athalon machine were different
  // and their optimal paramter values could not be calculated.  So,
  // these values have been set explicitly here to ensure they're the
  // same on all machines.
  //
  valarray<double> y( nY );
  y[0] = 1.88758;
  y[1] = -1.03471;
  y[2] = 1.18851;
  y[3] = -0.476253;
  y[4] = -1.45167;
  y[5] = -0.797979;
  y[6] = -0.0825739;
  y[7] = 3.04214;
  y[8] = 1.48168;
  y[9] = -1.29312;

  copy( &(y[0]), &(y[0])+nY, pdYData );
    
  //------------------------------------------------------------
  // Quantities related to the fixed population parameter, alp.
  //------------------------------------------------------------


  DoubleMatrix dvecAlpTrue( nAlp, 1 );
  DoubleMatrix dvecAlpLow ( nAlp, 1 );
  DoubleMatrix dvecAlpUp  ( nAlp, 1 );
  DoubleMatrix dvecAlpIn  ( nAlp, 1 );
  DoubleMatrix dvecAlpOut ( nAlp, 1 );
  DoubleMatrix dvecAlpStep( nAlp, 1 );

  double* pdAlpTrueData = dvecAlpTrue.data();
  double* pdAlpLowData  = dvecAlpLow .data();
  double* pdAlpUpData   = dvecAlpUp  .data();
  double* pdAlpInData   = dvecAlpIn  .data();
  double* pdAlpStepData = dvecAlpStep.data();

  // Set the values associated with alp(1).
  pdAlpTrueData[ 0 ] = meanBetaTrue;
  pdAlpLowData [ 0 ] = -10.0;
  pdAlpUpData  [ 0 ] = 10.0;
  pdAlpInData  [ 0 ] = -1.0;
  pdAlpStepData[ 0 ] = 1.0e-2;

  // Set the values associated with alp(2).
  pdAlpTrueData[ 1 ] = varBetaTrue;
  pdAlpLowData [ 1 ] = 1.0e-3;
  pdAlpUpData  [ 1 ] = 100.0;
  pdAlpInData  [ 1 ] = 0.5;
  pdAlpStepData[ 1 ] = 1.0e-2;
  
  // Set the values associated with alp(3).
  // This element is fixed.
  pdAlpTrueData[ 2 ] = 10.0;
  pdAlpInData  [ 2 ] = pdAlpTrueData[ 2 ];
  pdAlpLowData [ 2 ] = pdAlpInData[ 2 ];
  pdAlpUpData  [ 2 ] = pdAlpInData[ 2 ];
  pdAlpStepData[ 2 ] = 0.0;

  //------------------------------------------------------------
  // Quantities related to the random population parameters, b.
  //------------------------------------------------------------


  DoubleMatrix dvecBLow ( nB, 1 );
  DoubleMatrix dvecBUp  ( nB, 1 );
  DoubleMatrix dvecBStep( nB, 1 );

  dvecBLow .fill( -1.5e+1 );
  dvecBUp  .fill( +1.0e+1 );
  dvecBStep.fill(  1.0e-2 );

  DoubleMatrix dmatBIn ( nB, nInd );
  DoubleMatrix dmatBOut( nB, nInd );

  dmatBIn.fill( 1.0 );


  //------------------------------------------------------------
  // Quantities related to the population objective function.
  //------------------------------------------------------------

  double dLTildeOut;

  DoubleMatrix drowLTilde_alpOut    ( 1,    nAlp );
  DoubleMatrix dmatLTilde_alp_alpOut( nAlp, nAlp );


  //------------------------------------------------------------
  // Remaining inputs to ppkaOpt.
  //------------------------------------------------------------

  int popMItr = 0;
  if( whichObjective == MODIFIED_LAPLACE )
    popMItr = 11;
  else if( whichObjective == EXPECTED_HESSIAN )
    popMItr = 10;
  else
    popMItr = 10;
  Optimizer popOptimizer( 1.0e-6, popMItr, 0 );
  Optimizer indOptimizer( 1.0e-6, 40, 0 );

  // Set these to exercise the warm start capabilities of ppkaOpt.
  popOptimizer.setThrowExcepIfMaxIter( false );
  popOptimizer.setSaveStateAtEndOfOpt( true );

  
  //------------------------------------------------------------
  // Optimize The Population Objective function.
  //------------------------------------------------------------

  bool ok;
  try{
	while( true )
	{
      ppkaOpt(
               model,
               whichObjective,
               dvecN,
               dvecY,
               popOptimizer,
               dvecAlpLow,
               dvecAlpUp,
               dvecAlpIn,
               &dvecAlpOut,
               dvecAlpStep,
	       indOptimizer,
               dvecBLow,
               dvecBUp,
	       dmatBIn,
               &dmatBOut,
               dvecBStep,
               &dLTildeOut,
               &drowLTilde_alpOut,
               &dmatLTilde_alp_alpOut );
	  if( !popOptimizer.getIsTooManyIter() )
		// Finished
		break;

	  // Turn on warm start.
      popOptimizer.setIsWarmStart( true );
	}
    ok = true;
  }
  catch( const SpkException& e )
    {
      cerr << e << endl;
      CPPUNIT_ASSERT_MESSAGE( "ppkaOpt failed!", false );
    }
  catch(...)
  {
      CPPUNIT_ASSERT_MESSAGE( "ppkaOpt failed for unknown reasons!", false );
  }


  //------------------------------------------------------------
  // Known values.
  //------------------------------------------------------------

  //************************************************************
  // Note: equations for the known (analytic) values computed 
  // here are derived in "The Importance of Marginal Estimation
  // ", B. M. Bell, Applied Physics
  // Laboratory, University of Washington, May 25, 2000.
  //************************************************************

  CPPUNIT_ASSERT_EQUAL( nInd, nY );
  CPPUNIT_ASSERT( nY != 1 );

  // Compute the mean of the data, yBar.
  double yBar = 0.0;
  for ( i = 0; i < nInd; i++ )
  {
    yBar += pdYData[ i ];
  }
  yBar /= nInd;

  // Compute the sample variance, sSquared.
  double sSquared = 0.0;
  for ( i = 0; i < nInd; i++ )
  {
    sSquared += pow( ( pdYData[ i ] - yBar ), 2 );
  }
  sSquared /= ( nInd - 1 );

  // The value for alpHat(1) and alpHat(2) are contained in
  // section 9 of the above reference.
  // alp(3) is fixed so it's expected value stays the original
  // value.
  DoubleMatrix dvecAlpHat( nAlp, 1 );
  double* pdAlpHatData = dvecAlpHat.data();
  pdAlpHatData[ 0 ] = yBar;
  pdAlpHatData[ 1 ] = sSquared * (nInd - 1) / nInd - 1.0;
  pdAlpHatData[ 2 ] = pdAlpInData[ 2 ];

  //========================================================
  // Start from here! Sachiko 09/13/04
  //
  //  pDAlpHatData[ 2 ] = 
  //=======================================================

  // Compute bHat_i(alpHat) using equation (14) of the above reference.      
  DoubleMatrix dmatBHat( nB, nInd );
  double* pdBHatData = dmatBHat.data();
  for ( i = 0; i < nInd; i++ )
  {
    for ( k = 0; k < nB; k++ )
    {
      pdBHatData[ k + i * nB ] = ( pdYData[ i ] - pdAlpHatData[ 0 ] ) 
        / ( 1.0 + 1.0 / pdAlpHatData[ 1 ] );
    }
  }
  double* pdAlpOutData  = dvecAlpOut.data();

  // Compute ( 1 + alpOut(2) ).
  double onePlusAlp2 = 1.0 + pdAlpOutData[ 1 ];

  // Compute the sums involving ( y_i - alpOut(1) ).
  double yMinAlp1;
  double sumYMinAlp1    = 0.0;
  double sumYMinAlp1Sqd = 0.0;
  for ( i = 0; i < nInd; i++ )
  {
    yMinAlp1        = pdYData[ i ] - pdAlpOutData[ 0 ];
    sumYMinAlp1    += yMinAlp1;
    sumYMinAlp1Sqd += pow( yMinAlp1, 2 );
  }

  // Compute the known value for LTilde(alp) = -log[p(y|alp)]
  // using equation (17) of the above reference.
  double dLTildeKnown = sumYMinAlp1Sqd / ( 2.0 * ( onePlusAlp2 ) ) 
    + nInd / 2.0 * log( 2.0 * PI * ( onePlusAlp2 ) );

  // The value for LTilde_alp_alp(alp) was determined by taking the  
  // derivative of equation (17) of the above reference, i.e.,
  //
  //                          partial
  //     LTilde_alp(alp) = ------------- [- log[p(y|alp)] ] .
  //                        partial alp
  //
  DoubleMatrix drowLTilde_alpKnown( 1, nAlp );
  double* pdLTilde_alpKnownData = drowLTilde_alpKnown.data();
  pdLTilde_alpKnownData[ 0 ] = - sumYMinAlp1 / onePlusAlp2;
  pdLTilde_alpKnownData[ 1 ] = 0.5 / onePlusAlp2 
                                * ( - sumYMinAlp1Sqd / onePlusAlp2 + nInd );
  pdLTilde_alpKnownData[ 2 ] = 0.0;

  // The value for LTilde_alp_alp(alp) was determined by taking the second 
  // derivative of equation (17) of the above reference, i.e.,
  //
  //                             partial       partial   
  //     LTilde_alp_alp(alp) = ------------- ------------- [- log[p(y|alp)] ] .
  //                            partial alp   partial alp
  //
  DoubleMatrix dmatLTilde_alp_alpKnown( nAlp, nAlp );
  double* pdLTilde_alp_alpKnownData = dmatLTilde_alp_alpKnown.data();
  pdLTilde_alp_alpKnownData[ 0 ] = nInd / ( onePlusAlp2 );
  pdLTilde_alp_alpKnownData[ 1 ] = pow( onePlusAlp2, -2 ) * sumYMinAlp1;
  pdLTilde_alp_alpKnownData[ 2 ] = 0.0;
  pdLTilde_alp_alpKnownData[ 3 ] = pdLTilde_alp_alpKnownData[ 1 ];
  pdLTilde_alp_alpKnownData[ 4 ] = 0.5 * pow( onePlusAlp2, -2 ) 
                                    * (2.0 / onePlusAlp2 * sumYMinAlp1Sqd 
                                    - nInd );
  pdLTilde_alp_alpKnownData[ 5 ] = 0.0;


  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  doTheTest(  ok,
              dLTildeOut,
              dLTildeKnown,
              indOptimizer.getEpsilon(),
			  popOptimizer.getEpsilon(),
              dvecAlpLow,
              dvecAlpUp,
              dvecAlpOut,
              dvecAlpHat,
              dvecBLow,
              dvecBUp,
              dmatBOut,
              dmatBHat,
              drowLTilde_alpOut,
              drowLTilde_alpKnown,
              dmatLTilde_alp_alpOut,
              dmatLTilde_alp_alpKnown );
  
}


/*************************************************************************
 *
 * Function: doTheTest
 *
 *************************************************************************/

void ppkaOptTest::doTheTest( bool ok,
                       double dLTildeOut,
                       double dLTildeKnown,
                       const double epsB,
                       const double epsAlp,
                       const DoubleMatrix& dvecAlpLow,
                       const DoubleMatrix& dvecAlpUp,
                       const DoubleMatrix& dvecAlpOut,
                       const DoubleMatrix& dvecAlpHat,
                       const DoubleMatrix& dvecBLow,
                       const DoubleMatrix& dvecBUp,
                       const DoubleMatrix& dmatBOut,
                       const DoubleMatrix& dmatBHat,
                       const DoubleMatrix& drowLTilde_alpOut,
                       const DoubleMatrix& drowLTilde_alpKnown,
                       const DoubleMatrix& dmatLTilde_alp_alpOut,
                       const DoubleMatrix& dmatLTilde_alp_alpKnown )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i, k;

  string line = "----------------------------------------------------";

  int nAlp = dvecAlpOut.nr();
  int nB   = dmatBOut  .nr();
  int nInd = dmatBOut  .nc();

  // Updated 1-8-01 Alyssa
  // fixed for const correctness
  const double* pdAlpLowData = dvecAlpLow.data();
  const double* pdAlpUpData  = dvecAlpUp .data();
  const double* pdAlpOutData = dvecAlpOut.data();
  const double* pdAlpHatData = dvecAlpHat.data();

  const double* pdBLowData = dvecBLow.data();
  const double* pdBUpData  = dvecBUp .data();
  const double* pdBOutData = dmatBOut.data();
  const double* pdBHatData = dmatBHat.data();


  //------------------------------------------------------------
  // Print the results.
  //------------------------------------------------------------

  /*
  cout << endl;

  cout << "ok = " << ( ok ? "True" : "False" ) << endl;
  cout << endl;

  cout << "alpOut = " << endl;
  dvecAlpOut.print(); 
  cout << "alpHat = " << endl;
  dvecAlpHat.print(); 
  cout << endl;
  cout << "epsilon (for alphaOut) = " << epsAlp  << endl;
  cout << endl;

  if ( nInd <= 10 )
  {
    cout << "bOut = " << endl;
    dmatBOut.print(); 
    cout << "bHat = " << endl;
    dmatBHat.print(); 
    cout << endl;
    cout << "epsilon (for bOut)     = " << epsB  << endl;
    cout << endl;
  }

  cout << "LTildeOut   = " << dLTildeOut << endl;
  cout << "LTildeKnown = " << dLTildeKnown << endl;
  cout << endl;

  cout << "LTilde_alpOut  = " << endl;
  drowLTilde_alpOut.print();
  cout << "LTilde_alpKnown  = " << endl;
  drowLTilde_alpKnown.print();
  cout << endl;

  if ( nAlp <= 5 )
  {
    cout << "LTilde_alp_alpOut  = " << endl;
    dmatLTilde_alp_alpOut.print();
    cout << "LTilde_alp_alpKnown  = " << endl;
    dmatLTilde_alp_alpKnown.print();
    cout << endl;
  }

  */
  //------------------------------------------------------------
  // Check to see if the optimization completed sucessfully.
  //------------------------------------------------------------

  CPPUNIT_ASSERT(ok);

  //------------------------------------------------------------
  // Check the final fixed population parameter vector, alpOut.
  //------------------------------------------------------------

  // Check to see if any elements of alpOut fail to satisfy 
  // the convergence criteria:
  // 
  //      abs(alpOut - alpHat) <= epsAlp (alpUp - alpLow)
  //
  bool isConverged = true;
  for ( i = 0; i < nAlp; i++ )
  {
    if ( fabs(pdAlpOutData[ i ] - pdAlpHatData[ i ]) > 
            epsAlp * (pdAlpUpData[ i ] - pdAlpLowData[ i ]) )
    {
      isConverged = false;
    }
  }

  CPPUNIT_ASSERT(ok);
  CPPUNIT_ASSERT(isConverged);


  //------------------------------------------------------------
  // Check the matrix of final random population parameter vectors, bOut.
  //------------------------------------------------------------

  // For each pair of vectors bOut_i and bHat_i, i.e., for each column 
  // of bOut and bHat, check to see that the convergence criteria,
  // 
  //      abs(bOut_i - bHat_i) <= epsB (bUp - bLow)  ,
  //
  // is satisfied.
  //
  isConverged = true;
  for ( i = 0; i < nInd; i++ )
  {
    for ( k = 0; k < nB; k++ )
    {
      if ( fabs(pdBOutData[ k + i * nB ] - pdBHatData[ k + i * nB  ]) > 
        epsB * (pdBUpData[ k ] - pdBLowData[ k ]) )
      {
        isConverged = false;
      }
    }
  }
  
  CPPUNIT_ASSERT(ok);
  CPPUNIT_ASSERT(isConverged);

  
}


