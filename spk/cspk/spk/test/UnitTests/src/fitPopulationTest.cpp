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
 * File: fitPopulationTest.cpp
 *
 *
 * Unit test for the function fitPopulation.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/
#include <iostream>
#include <fstream>
#include <string>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>
#include <spk/fitPopulation.h>
#include <spk/SpkException.h>
#include <spk/Objective.h>
#include <spk/NaiveFoModel.h>

#include "fitPopulationTest.h"

using namespace std;
using namespace CppUnit;

void fitPopulationTest::setUp()
{
    // initializations
}
void fitPopulationTest::tearDown()
{
    // clean up
}

Test* fitPopulationTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("fitPopulationTest");

    suiteOfTests->addTest(new TestCaller<fitPopulationTest>(
	     "modifiedLaplaceTest", &fitPopulationTest::modifiedLaplaceTest));
    suiteOfTests->addTest(new TestCaller<fitPopulationTest>(
             "expectedHessianTest", &fitPopulationTest::expectedHessianTest));
    suiteOfTests->addTest(new TestCaller<fitPopulationTest>(
             "firstOrderTest", &fitPopulationTest::firstOrderTest));

    return suiteOfTests;
}

void fitPopulationTest::modifiedLaplaceTest()
{
    fitPopulationExampleTest(MODIFIED_LAPLACE);
	//cout << "fitPopulationExampleTest(MODIFIED_LAPLACE)" << endl; 
    fitPopulationZeroIterationsTest(MODIFIED_LAPLACE);
    //cout << "fitPopulationZeroIterationsTest(MODIFIED_LAPLACE)" << endl; 
}
void fitPopulationTest::expectedHessianTest()
{
    fitPopulationExampleTest(EXPECTED_HESSIAN);
	//cout << "fitPopulationExampleTest(EXPECTED_HESSIAN)" << endl;
    fitPopulationZeroIterationsTest(EXPECTED_HESSIAN);
	//cout << "fitPopulationZeroIterationsTest(EXPECTED_HESSIAN)" << endl;
}
void fitPopulationTest::firstOrderTest()
{
    fitPopulationExampleTest(FIRST_ORDER);
	//cout << "fitPopulationExampleTest(FIRST_ORDER)" << endl;
	fitPopulationZeroIterationsTest(FIRST_ORDER);
	//cout << "fitPopulationZeroIterationsTest(FIRST_ORDER)" << endl;
}
void fitPopulationTest::naiveFirstOrderTest()
{
    fitPopulationExampleTest(NAIVE_FIRST_ORDER);
	//cout << "fitPopulationExampleTest(NAIVE_FIRST_ORDER)" << endl;
	fitPopulationZeroIterationsTest(NAIVE_FIRST_ORDER);
	//cout << "fitPopulationZeroIterationsTest(NAIVE_FIRST_ORDER)" << endl;
}

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

#include <spk/lTilde.h>
#include <spk/pi.h>
#include <spk/SpkModel.h>
#include <spk/inverse.h>
#include <spk/EqIndModel.h>
#include <spk/mapObj.h>
#include <nag.h>
#include <nagg05.h>
#include <iomanip>
#include <cmath>


/*------------------------------------------------------------------------
 * Namespace Declarations
 *------------------------------------------------------------------------*/

using namespace std;


/*------------------------------------------------------------------------
 * Local Function Declarations
 *------------------------------------------------------------------------*/

static void doTheTest( bool ok,
                       double dLTildeOut,
                       double dLTildeKnown,
                       const valarray<double>& epsilon,
                       const valarray<double>& alpLow,
                       const valarray<double>& alpUp,
                       const valarray<double>& alpOut,
                       const valarray<double>& alpHat,
                       const valarray<double>& bLow,
                       const valarray<double>& bUp,
                       const valarray<double>& bOut,
                       const valarray<double>& bHat,
                       const valarray<double>& lTilde_alpOut,
                       const valarray<double>& lTilde_alpKnown,
                       const valarray<double>& lTilde_alp_alpOut,
                       const valarray<double>& lTilde_alp_alpKnown );


/*************************************************************************
 *
 * Function: fitPopulationExampleTest
 *
 *
 * This test implements the example problem from the fitPopulation specification. 
 *
 *************************************************************************/

class UserModelFitPopulationExampleTest : public SpkModel
{
    valarray<double> _a, _b;
	const int _nA;
    const int _nB;
    const int _nYi;
    int _i;
public:
    UserModelFitPopulationExampleTest(int nA, int nB, int nYi)
      :_nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
	{};    
    ~UserModelFitPopulationExampleTest(){};
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
        ret.resize(_nB * _nB * _nA);
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
        ret.resize(_nB * _nB * _nA);
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
        ret.resize(_nYi*_nYi);
        ret[0] = 1.0;
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
        // R_b = [ 0 ]
        //
        ret.resize(_nYi *_nYi * _nB);
        ret[0] = 0.0;
        return false;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
        //
        // Rinv = [ 1 ]
        //
        ret.resize(_nYi * _nYi );
        ret[0] = 1.0;
    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        // Rinv_alp = [ 0  0 ]
        //
        ret.resize(_nYi * _nYi * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
        return false;
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
        //
        // Rinv_b = [ 0 ]
        //
        ret.resize(_nYi * _nYi * _nB);
        ret[0] = 0.0;
        return false;
    }   

};

void fitPopulationTest::fitPopulationExampleTest(enum Objective whichObjective)
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

  UserModelFitPopulationExampleTest model( nAlp, nB, nYi );


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
  valarray<double> Y( nY );

  // Number of measurements for each individual. 
  valarray<int> N( 1, nInd );

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
  Integer seed = 0;
  g05cbc(seed);
  for ( i = 0; i < nInd; i++ )
  {
    eTrue = nag_random_normal( meanETrue, sdETrue );
    bTrue = nag_random_normal( meanBTrue, sdBTrue );

    Y[ i ] = meanBetaTrue + bTrue + eTrue;
  }


  //------------------------------------------------------------
  // Quantities related to the fixed population parameter, alp.
  //------------------------------------------------------------

  valarray<double> alpTrue( nAlp );
  valarray<double> alpLow ( nAlp );
  valarray<double> alpUp  ( nAlp );
  valarray<double> dvecAlpIn  ( nAlp );
  valarray<double> alpOut ( nAlp );
  valarray<double> dvecAlpStep( nAlp );

  // Set the values associated with alp(1).
  alpTrue[ 0 ] = meanBetaTrue;
  alpLow [ 0 ] = -10.0;
  alpUp  [ 0 ] = 10.0;
  dvecAlpIn  [ 0 ] = -1.0;
  dvecAlpStep[ 0 ] = 1.0e-2;

  // Set the values associated with alp(2).
  alpTrue[ 1 ] = varBetaTrue;
  alpLow [ 1 ] = 1.0e-3;
  alpUp  [ 1 ] = 100.0;
  dvecAlpIn  [ 1 ] = 0.5;
  dvecAlpStep[ 1 ] = 1.0e-2;
  

  //------------------------------------------------------------
  // Quantities related to the random population parameters, b.
  //------------------------------------------------------------

  valarray<double> bLow ( -1.5e+1, nB );
  valarray<double> bUp  ( +1.0e+1, nB );
  valarray<double> dvecBStep(  1.0e-2, nB );

  valarray<double> dmatBIn ( 1., nB * nInd );
  valarray<double> bOut(     nB * nInd );


  //------------------------------------------------------------
  // Quantities related to the population objective function.
  //------------------------------------------------------------

  double dLTildeOut;

  valarray<double> lTilde_alpOut    ( nAlp );
  valarray<double> lTilde_alp_alpOut( nAlp * nAlp );


  //------------------------------------------------------------
  // Remaining inputs to fitPopulation.
  //------------------------------------------------------------

  // Set the values associated with the individual objective function.
  Optimizer indOptimizer( 1.0e-6, 40, 0 );

  // Set the values associated with the population objective function.
  Optimizer popOptimizer( 1.0e-6, 10, 0 );

  // Set the parallel controls object
  DirBasedParallelControls parallelControls( false, 0, 0 );

  // Set up warm start
  popOptimizer.setupWarmStart( nAlp );

  //------------------------------------------------------------
  // Optimize the population objective function.
  //------------------------------------------------------------

  bool ok;
  try{
	  while( true )
	  {
          fitPopulation(
					     model,
					     whichObjective,
					     N,
					     Y,
					     popOptimizer,
					     alpLow,
					     alpUp,
					     dvecAlpIn,
					     dvecAlpStep,
					     &alpOut,
					     indOptimizer,
					     bLow,
					     bUp,
					     dmatBIn,            
					     dvecBStep,
					     &bOut,
					     &dLTildeOut,
					     &lTilde_alpOut,
					     &lTilde_alp_alpOut, 
					     parallelControls 
			           );
		  if( !popOptimizer.getIsTooManyIter() )
		      // Finished
			  break;

		  // Turn on warm start.
          popOptimizer.setIsWarmStart( true );
	  }
       ok = true;
  }
  catch(...)
  {
    CPPUNIT_ASSERT_MESSAGE( "fitPopulation failed", false );
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
    yBar += Y[ i ];
  }
  yBar /= nInd;

  // Compute the sample variance, sSquared.
  double sSquared = 0.0;
  for ( i = 0; i < nInd; i++ )
  {
    sSquared += pow( ( Y[ i ] - yBar ), 2 );
  }
  sSquared /= ( nInd - 1 );

  // The value for alpHat(1) and alpHat(2) are contained in
  // section 9 of the above reference.
  valarray<double> alpHat( nAlp );

  alpHat[ 0 ] = yBar;
  alpHat[ 1 ] = sSquared * (nInd - 1) / nInd - 1.0;

  // Compute bHat_i(alpHat) using equation (14) of the above reference.      
  valarray<double> bHat( nB * nInd );

  for ( i = 0; i < nInd; i++ )
  {
    for ( k = 0; k < nB; k++ )
    {
      bHat[ k + i * nB ] = ( Y[ i ] - alpHat[ 0 ] ) 
        / ( 1.0 + 1.0 / alpHat[ 1 ] );
    }
  }

  // Compute ( 1 + alpOut(2) ).
  double onePlusAlp2 = 1.0 + alpOut[ 1 ];

  // Compute the sums involving ( y_i - alpOut(1) ).
  double yMinAlp1;
  double sumYMinAlp1    = 0.0;
  double sumYMinAlp1Sqd = 0.0;
  for ( i = 0; i < nInd; i++ )
  {
    yMinAlp1        = Y[ i ] - alpOut[ 0 ];
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
  valarray<double> lTilde_alpKnown( nAlp );

  lTilde_alpKnown[ 0 ] = - sumYMinAlp1 / onePlusAlp2;
  lTilde_alpKnown[ 1 ] = 0.5 / onePlusAlp2 
                                * ( - sumYMinAlp1Sqd / onePlusAlp2 + nInd );

  // The value for LTilde_alp_alp(alp) was determined by taking the second 
  // derivative of equation (17) of the above reference, i.e.,
  //
  //                             partial       partial   
  //     LTilde_alp_alp(alp) = ------------- ------------- [- log[p(y|alp)] ] .
  //                            partial alp   partial alp
  //
  valarray<double> lTilde_alp_alpKnown( nAlp * nAlp );

  lTilde_alp_alpKnown[ 0 ] = nInd / ( onePlusAlp2 );
  lTilde_alp_alpKnown[ 1 ] = pow( onePlusAlp2, -2 ) * sumYMinAlp1;
  lTilde_alp_alpKnown[ 2 ] = lTilde_alp_alpKnown[ 1 ];
  lTilde_alp_alpKnown[ 3 ] = 0.5 * pow( onePlusAlp2, -2 ) 
                                    * (2.0 / onePlusAlp2 * sumYMinAlp1Sqd 
                                    - nInd );

  valarray<double> epsilon( 2 );
  epsilon[ 0 ] = indOptimizer.getEpsilon();
  epsilon[ 1 ] = popOptimizer.getEpsilon();

  //------------------------------------------------------------
  // Do the test.
  //------------------------------------------------------------

  doTheTest(  ok,
              dLTildeOut,
              dLTildeKnown,
              epsilon,
              alpLow,
              alpUp,
              alpOut,
              alpHat,
              bLow,
              bUp,
              bOut,
              bHat,
              lTilde_alpOut,
              lTilde_alpKnown,
              lTilde_alp_alpOut,
              lTilde_alp_alpKnown );
  
}


/*************************************************************************
 *
 * Function: fitPopulationZeroIterationsTest
 *
 *
 * This test calls fitPopulation with zero iterations at the population level,
 * with zero iterations at the individual level, and with zero iterations 
 * at both the population and the individual levels.
 *
 * At the individual level this test implements the example problem from 
 * the fitIndividual specification. 
 *
 *************************************************************************/

class UserModelFitPopulationZeroIterationsTest : public SpkModel
{
    valarray<double> _a, _b;
    int _i;
	const int _nA;
    const int _nB;
    const int _nYi;
public:
    UserModelFitPopulationZeroIterationsTest(int nA, int nB, int nYi)
      : _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
	{}; 
    ~UserModelFitPopulationZeroIterationsTest(){};
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
    void doSetIndPar(const valarray<double>& bval)
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

void fitPopulationTest::fitPopulationZeroIterationsTest(enum Objective whichObjective)
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace population_analysis;

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

  UserModelFitPopulationZeroIterationsTest model(nAlp, nB, nYPerInd);


  //------------------------------------------------------------
  // Quantities related to the individuals in the population.
  //------------------------------------------------------------

  valarray<int> N( nYPerInd, nInd );


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  // Measurement values, y.
  valarray<double> Y( 2., nY );


  //------------------------------------------------------------
  // Quantities related to the fixed population parameter, alp.
  //------------------------------------------------------------

  // Set the values associated with alp(1).
  valarray<double> alpLow ( -10.0, nAlp );
  valarray<double> alpUp  (  10.0, nAlp );
  valarray<double> dvecAlpIn  (   5.0, nAlp );
  valarray<double> alpOut (        nAlp );
  valarray<double> dvecAlpStep( 1.e-2, nAlp );

  
  //------------------------------------------------------------
  // Quantities related to the random population parameters, b.
  //------------------------------------------------------------

  valarray<double> bLow (  -4., nB );
  valarray<double> bUp  (   4., nB );
  valarray<double> dvecBStep( .001, nB );

  valarray<double> dmatBIn ( 2., nB * nInd );
  valarray<double> bOut(     nB * nInd );


  //------------------------------------------------------------
  // Quantities related to the population objective function.
  //------------------------------------------------------------

  double dLTildeOut;

  valarray<double> lTilde_alpOut    ( nAlp );
  valarray<double> lTilde_alp_alpOut( nAlp * nAlp );


  //------------------------------------------------------------
  // Remaining inputs to fitPopulation.
  //------------------------------------------------------------
  
  // Set the values associated with the individual objective function.
  Optimizer indOptimizer( 1.0e-6, 0, 0 );

  // Set the values associated with the population objective function.
  Optimizer popOptimizer( 1.0e-6, 0, 0 );

  // Set the parallel controls object
  DirBasedParallelControls parallelControls( false, 0, 0 );


  //------------------------------------------------------------
  // Quantities related to the known values.
  //------------------------------------------------------------

  valarray<double> alpHat( nAlp );
  valarray<double> bHat  ( nB * nInd );

  // Inputs to lTilde.
  bool withD = true;
  double dLTildeKnown;
  valarray<double> lTilde_alpKnown( nAlp );
  valarray<double>* pdmatNull = 0;

  // The second derivatives should all be zero.
  valarray<double> lTilde_alp_alpKnown( 0., nAlp * nAlp );


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

      indOptimizer.setNMaxIter( 40 );
	  popOptimizer.setNMaxIter(  0 );

      // Since the number of iterations for the population level is
      // zero, alpOut and alpHat should both be equal to alpIn.
      alpHat = dvecAlpIn;

      // For each individual, the minimum value for MapObj(b) occurs 
      // when b(1) = 0 and b(2) = 1. 
      for ( j = 0; j < nInd; j++ )
      {
        bHat[ 0 + j * nB ] = 0.0;
        bHat[ 1 + j * nB ] = 1.0;
      }

      break;


    //----------------------------------------------------------
    // Zero iterations at the individual level.
    //----------------------------------------------------------

    case 1:

      //preTestPrinting( "Zero Individual Level Iterations" );

	  indOptimizer.setNMaxIter(  0 );
	  popOptimizer.setNMaxIter( 40 );

      // Since f(alp, b), R(alp, b), and D(alp) are defined below
      // such that they are all independent of alp, the optimizer
      // will return alpIn as the value for alpOut.
      alpHat = dvecAlpIn;

      // Since the number of iterations for the individual level is
      // zero, bOut and bHat should both be equal to bIn.
      bHat = dmatBIn;

      break;


    //----------------------------------------------------------
    // Zero iterations at both levels.
    //----------------------------------------------------------

    case 2:

      //preTestPrinting( "Zero Iterations at Both Levels" );

      indOptimizer.setNMaxIter( 0 );
	  popOptimizer.setNMaxIter( 0 );

      // Since the number of iterations for the population level is
      // zero, alpOut and alpHat should both be equal to alpIn.
      alpHat = dvecAlpIn;

      // Since the number of iterations for the individual level is
      // zero, bOut and bHat should both be equal to bIn.
      bHat = dmatBIn;

      break;
    }


    //----------------------------------------------------------
    // Optimize the population objective function.
    //----------------------------------------------------------

    bool okFitPopulation;
    try{
        fitPopulation( 
					  model,
					  whichObjective,
					  N,
					  Y,
					  popOptimizer,
					  alpLow,
					  alpUp,
					  dvecAlpIn,
					  dvecAlpStep,
					  &alpOut,
					  indOptimizer,
					  bLow,
					  bUp,
					  dmatBIn,            
					  dvecBStep,
					  &bOut,
					  &dLTildeOut,
					  &lTilde_alpOut,
					  &lTilde_alp_alpOut, 
					  parallelControls 
			        );
        okFitPopulation = true;
    }
    catch(...)
    {
        CPPUNIT_ASSERT_MESSAGE( "fitPopulation failed", false );
    }

    //----------------------------------------------------------
    // Compute the known values for LTilde and LTilde_alp.
    //----------------------------------------------------------
  

    //----------------------------------------------------------
    // Convert to DoubleMatrix for using LTilde.
    //----------------------------------------------------------

    DoubleMatrix dvecY( Y );

    //
    // [ Revisit --- Sachiko, 10/08/02 ]
    // 
    // The following block of code converts valarray<int> to valarray<double>
    // in a very clumsy way.  DoubleMatrix::fromValarray() should be
    // extended so that it can take int or double or perhaps any type.
    // I'm working on it.
    //
    valarray<double> temp(N.size());
    for( int i=0; i<N.size(); i++ )
      temp[i] = static_cast<int>(N[i]);
    DoubleMatrix dvecN( temp );

    DoubleMatrix dvecAlpHat( alpHat );
    DoubleMatrix dvecBLow( bLow );
    DoubleMatrix dvecBUp( bUp );
    DoubleMatrix dvecBStep( dvecBStep );
	DoubleMatrix dmatBIn( dmatBIn, nInd );
    DoubleMatrix* pNull = 0;
    DoubleMatrix drowLTilde_alpKnown( lTilde_alpKnown, nAlp );

    bool okLTilde;

    //
    // [ Comment by Sachiko, 09/18/2002 ]
    //
    // When FO is specified, call lTilde in such a way it
    // exercieses the naive (straight translation of FO)
    // method because lTilde is not in the execution
    // path of the official FO (with EqIndModel).  
    // To do that, feed a NaiveFoModel object
    // as a SpkModel instance and specify NAIVE_FIRST_ORDER
    // as the objective of choice.
    // 
    if( whichObjective == FIRST_ORDER )
    {
//      NaiveFoModel foModel( &model, dvecBStep.toValarray() );
      try{
/*
          lTilde(  foModel,
                   NAIVE_FIRST_ORDER,
                   dvecY,
                   dvecN,
                   indOptimizer,
                   dvecAlpHat,
                   dvecBLow,
                   dvecBUp,
                   dvecBStep,
                   dmatBIn,
                   pNull,
                   &dLTildeKnown,
                   &drowLTilde_alpKnown);
*/
        std::valarray<int> N( nInd );
	    for( int k = 0; k < nInd; k++ )
		    N[ k ] = (int)dvecN.data()[ k ];
	    EqIndModel FoModel( &model, N, dvecBStep.toValarray(), nAlp );

        mapObj( FoModel, 
                dvecY,
                dvecAlpHat,
                &dLTildeKnown,
                &drowLTilde_alpKnown,
                false,
                true,
                &dvecN );


          okLTilde = true;
      }
      catch(...)
      {
          CPPUNIT_ASSERT_MESSAGE( "ltilde failed", false );
      }
    }
    else
    {
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
                   pNull,
                   &dLTildeKnown,
                   &drowLTilde_alpKnown);
          okLTilde = true;
      }
      catch(...)
      {
          CPPUNIT_ASSERT_MESSAGE( "ltilde failed", false );
      }
    }

    //----------------------------------------------------------
    // Convert back to valarray<double>.
    //----------------------------------------------------------

    drowLTilde_alpKnown.toValarray( lTilde_alpKnown );
   
	valarray<double> epsilon( 2 );
    epsilon[ 0 ] = indOptimizer.getEpsilon();
    epsilon[ 1 ] = popOptimizer.getEpsilon();

    //----------------------------------------------------------
    // Compare the results.
    //----------------------------------------------------------

    doTheTest(  okFitPopulation,
                dLTildeOut,
                dLTildeKnown,
                epsilon,
                alpLow,
                alpUp,
                alpOut,
                alpHat,
                bLow,
                bUp,
                bOut,
                bHat,
                lTilde_alpOut,
                lTilde_alpKnown,
                lTilde_alp_alpOut,
                lTilde_alp_alpKnown );

  }
  
}


/*************************************************************************
 *
 * Function: doTheTest
 *
 *************************************************************************/

void fitPopulationTest::doTheTest( bool ok,
								   double dLTildeOut,
								   double dLTildeKnown,
								   const valarray<double>& epsilon,
								   const valarray<double>& alpLow,
								   const valarray<double>& alpUp,
								   const valarray<double>& alpOut,
								   const valarray<double>& alpHat,
								   const valarray<double>& bLow,
								   const valarray<double>& bUp,
								   const valarray<double>& bOut,
								   const valarray<double>& bHat,
								   const valarray<double>& lTilde_alpOut,
								   const valarray<double>& lTilde_alpKnown,
								   const valarray<double>& lTilde_alp_alpOut,
								   const valarray<double>& lTilde_alp_alpKnown )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i, k;

  string line = "----------------------------------------------------";

  int nAlp = alpOut.size();
  int nB   = bUp   .size();
  int nInd = bOut  .size() / nB;

  double  epsB          = epsilon[ 0 ];
  double  epsAlp        = epsilon[ 1 ];


  //------------------------------------------------------------
  // Print the results.
  //------------------------------------------------------------

  DoubleMatrix AOut, AHat, BOut, BHat, LTilde_alpOut, 
	           LTilde_alpKnown, LTilde_alp_alpOut, LTilde_alp_alpKnown;  
  //cout << endl;

  //cout << "ok = " << ( ok ? "True" : "False" ) << endl;
  //cout << endl;

  //cout << "alpOut = " << endl;
  //AOut.fromValarray( alpOut ).print(); 
  //cout << "alpHat = " << endl;
  //BOut.fromValarray( alpHat ).print(); 
  //cout << endl;
  //cout << "epsilon (for alphaOut) = " << epsAlp  << endl;
  //cout << endl;

  if ( nInd <= 10 )
  {
    //cout << "bOut = " << endl;
    //BOut.fromValarray( bOut, nInd ).print(); 
    //cout << "bHat = " << endl;
    //BHat.fromValarray( bHat, nInd ).print(); 
    //cout << endl;
    //cout << "epsilon (for bOut)     = " << epsB  << endl;
    //cout << endl;
  }

  //cout << "LTildeOut   = " << dLTildeOut << endl;
  //cout << "LTildeKnown = " << dLTildeKnown << endl;
  //cout << endl;

  //cout << "LTilde_alpOut  = " << endl;
  //LTilde_alpOut.fromValarray( lTilde_alpOut, nAlp ).print();
  //cout << "LTilde_alpKnown  = " << endl;
  //LTilde_alpKnown.fromValarray( lTilde_alpKnown, nAlp ).print();
  //cout << endl;

  if ( nAlp <= 5 )
  {
    //cout << "LTilde_alp_alpOut  = " << endl;
    //LTilde_alp_alpOut.fromValarray( lTilde_alp_alpOut, nAlp ).print();
    //cout << "LTilde_alp_alpKnown  = " << endl;
    //LTilde_alp_alpKnown.fromValarray( lTilde_alp_alpKnown, nAlp ).print();
    //cout << endl;
  }

  
  //------------------------------------------------------------
  // Check to see if the optimization completed sucessfully.
  //------------------------------------------------------------

  CPPUNIT_ASSERT( ok );

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
    if ( fabs(alpOut[ i ] - alpHat[ i ]) > 
            epsAlp * (alpUp[ i ] - alpLow[ i ]) )
    {
      isConverged = false;
    }
  }

  CPPUNIT_ASSERT( ok );
  CPPUNIT_ASSERT( isConverged );


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
      if ( fabs(bOut[ k + i * nB ] - bHat[ k + i * nB  ]) > 
        epsB * (bUp[ k ] - bLow[ k ]) )
      {
        isConverged = false;
      }
    }
  }
  
  CPPUNIT_ASSERT( ok );
  CPPUNIT_ASSERT( isConverged);

  
}
