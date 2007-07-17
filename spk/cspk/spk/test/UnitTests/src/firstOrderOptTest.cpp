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
$begin firstOrderOptTest.cpp$$

$section firstOrderOpt: Example and Test$$

$code
$verbatim%firstOrderOptTest.cpp%0%// BEGIN VERBATIM%// END VERBATIM%1%$$
$$

$end

*/
/*************************************************************************
 *
 * File: firstOrderOptTest.cpp
 *
 *
 * Unit test for the function firstOrderOpt.
 *
 * Author: Jiaji Du based on Mitch's ppkaOptTest.cpp
 *
 *************************************************************************/
/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/
// BEGIN VERBATIM 
#include <iostream>
#include <iomanip>
#include <string>
#include <cmath>
#include <iostream>
#include <fstream>
#include <string>

extern "C"{
  #include <atlas/clapack.h>
  #include <atlas/cblas.h>
}

#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/firstOrderOpt.h"
#include "../../../spk/namespace_population_analysis.h"
#include "../../../spk/SpkException.h"
#include "../../../spk/SpkValarray.h"
#include "../../../spk/Objective.h"
#include "../../../spk/lTilde.h"
#include "../../../spk/pi.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/EqIndModel.h"
#include "../../../spk/mapObj.h"
#include "../../../spk/randNormal.h"

#include "firstOrderOptTest.h"

/*------------------------------------------------------------------------
 * Namespace Declarations
 *------------------------------------------------------------------------*/

using SPK_VA::valarray;
using namespace CppUnit;
using namespace std;

void firstOrderOptTest::setUp()
{
    // initializations
}
void firstOrderOptTest::tearDown()
{
    // clean up
}

Test* firstOrderOptTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("firstOrderOptTest");

  suiteOfTests->addTest(new TestCaller<firstOrderOptTest>(
    "firstOrderOptExampleTest", 
    &firstOrderOptTest::firstOrderOptExampleTest
  ));

  return suiteOfTests;
}


/*************************************************************************
 * Function: firstOrderOptExampleTest
 *
 * This is an example and test of firstOrderOpt.cpp 
 *************************************************************************/

class UserModelFirstOrderOptExampleTest : public SpkModel<double>
{
private:
    valarray<double> alpha_(2); // current fixed effects
    valarray<double>     b_(1); // current random effects
    int                  i_; // current individual
public:
    UserModelFirstOrderOptExampleTest() :
     _(m), n_(n), Ni_(Ni), alpha_(m), b_(n_)
    {};    
    ~UserModelFirstOrderOptExampleTest(){};
private:
    void doSelectIndividual(int i)
    {	i_ = i; }
    void doSetPopPar(const valarray<double>& alpha)
    {	alpha_ = alpha; }
    void doSetIndPar(const valarray<double>& b)
    {	b_ = b; }
    void doIndParVariance( valarray<double>& D ) const
    {	D.resize(1);
	D[0] = alpha[1];
    }
    void doDataMean( valarray<double>& f ) const
    {	f.resize(1);
	f[0] = alpha_[0] + b_[0];	
    }
    void doDataVariance( valarray<double>& R ) const
    {	R.resize(1);
        R[0] = 1.0;
    }

    bool doIndParVariance_popPar( valarray<double>& D ) const
    {	CPPUNIT_ASSERT_MESSAGE(
        "firstOrderOptTest: call to doIndParVariance_popPar", false);
        return false;
    }
    void doIndParVarianceInv( valarray<double>& ) const
    {	CPPUNIT_ASSERT_MESSAGE(
        "firstOrderOptTest: call to doIndParVarianceInv", false);
        return false;
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ) const
    {	CPPUNIT_ASSERT_MESSAGE(
        "firstOrderOptTest: call to doIndParVarianceInv_popPar", false);
        return false;
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {	CPPUNIT_ASSERT_MESSAGE(
        "firstOrderOptTest: call to doDataMean_popPar", false);
        return false;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {	CPPUNIT_ASSERT_MESSAGE(
        "firstOrderOptTest: call to doDataMean_indPar", false);
        return false;
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {	CPPUNIT_ASSERT_MESSAGE(
        "firstOrderOptTest: call to doDataVariance_popPar", false);
        return false;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {	CPPUNIT_ASSERT_MESSAGE(
        "firstOrderOptTest: call to doDataVariance_indPar", false);
        return false;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {	CPPUNIT_ASSERT_MESSAGE(
        "firstOrderOptTest: call to doDataVarianceInv", false);
        return false;
    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {	CPPUNIT_ASSERT_MESSAGE(
        "firstOrderOptTest: call to doDataVarianceInv_popPar", false);
        return false;
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {	CPPUNIT_ASSERT_MESSAGE(
        "firstOrderOptTest: call to doDataVarianceInv_indPar", false);
        return false;
    }
};

void firstOrderOptTest::firstOrderOptExampleTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace population_analysis;

  int i, k;

  //preTestPrinting( "Specification Example" );

  // Number of individuals.
  const int nInd = 10;

  // Number of measurements per individual (same for all)
  const int nYi = 1;

  // Number of measurements for all individuals
  const int nY = nInd * nYi;

  // Number of pop parameter
  const int nAlp = 2;

  // Number of ind parameter
  const int nB = 1;


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  UserModelFirstOrderOptExampleTest model( nAlp, nB, nYi );


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

  DoubleMatrix drowLTilde_alpOut     ( 1,    nAlp );
  DoubleMatrix dmatLTilde_alp_alpOut ( nAlp, nAlp );
  DoubleMatrix dmatLambdaTilde_alpOut( nAlp, nInd );


  //------------------------------------------------------------
  // Remaining inputs to ppkaOpt.
  //------------------------------------------------------------

  Optimizer indOptimizer( 1.0e-6, 40, 0 );
  Optimizer popOptimizer( 1.0e-6, 5, 0 );

  // Set these to exercise the warm start capabilities of firstOrderOpt.
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
      firstOrderOpt(
                     model,
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
                     &dmatLTilde_alp_alpOut,
                     &dmatLambdaTilde_alpOut );

      // Exit this loop if the maximum number of iterations was
      // not exceeded, i.e., if the optimization was successful.
      if( !popOptimizer.getIsTooManyIter() )
        break;

      // Set this so that firstOrderOpt performs a warm start when it
      // is called again.
      popOptimizer.setIsWarmStart( true );
    }

    ok = true;
  }
  catch(...)
  {
    CPPUNIT_ASSERT(false);
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
  CPPUNIT_ASSERT( nY != 1);

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

  // The value for LTilde_alp(alp) was determined by taking the  
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

  // The value for the derivative of each individual's contribution
  // to LTilde(alp) was determined by taking the derivative of each
  // individual's contribution to equation (17) of the above
  // reference, i.e.,
  //
  //                               partial
  //     [ LTilde (alp) ]_alp = ------------- [- log[p(y |alp)] ] .
  //             i               partial alp            i
  //
  DoubleMatrix dmatLambdaTilde_alpKnown( nAlp, nInd );
  double* pdLambdaTilde_alpKnownData = dmatLambdaTilde_alpKnown.data();
  double y_iMinAlp1;
  for ( i = 0; i < nInd; i++ )
  {
    y_iMinAlp1 = pdYData[ i ] - pdAlpHatData[ 0 ];

    pdLambdaTilde_alpKnownData[ 0 + i * nAlp ] = - y_iMinAlp1 / onePlusAlp2;
    pdLambdaTilde_alpKnownData[ 1 + i * nAlp ] = 0.5 / onePlusAlp2 
                                                   * ( - pow( y_iMinAlp1, 2 )
                                                       / onePlusAlp2 + 1.0 );
  }

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
              dmatLambdaTilde_alpOut,
              dmatLambdaTilde_alpKnown,
              dmatLTilde_alp_alpOut,
              dmatLTilde_alp_alpKnown );
  
}
// END VERBATIM
