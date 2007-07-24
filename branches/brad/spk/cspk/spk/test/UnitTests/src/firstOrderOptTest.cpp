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

#include <cppad/cppad.hpp>
#include <valarray>
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

#include "../../../spk/DoubleMatrix.h"
#include "../../../spk/firstOrderOpt.h"
#include "../../../spk/SpkException.h"
#include "../../../spk/SpkModel.h"
#include "../../../spk/mapObj.h"
#include "../../../spk/randNormal.h"

#include "firstOrderOptTest.h"

/*------------------------------------------------------------------------
 * Namespace Declarations
 *------------------------------------------------------------------------*/

using std::valarray;
using namespace CppUnit;

void firstOrderOptTest::setUp()
{ // initializations
}
void firstOrderOptTest::tearDown()
{ // clean up
}

Test* firstOrderOptTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite("firstOrderOptTest");

  suiteOfTests->addTest(
    new TestCaller<firstOrderOptTest>(
        "firstOrderOptLinearTest", 
        &firstOrderOptTest::firstOrderOptLinearTest
    )
  );

  return suiteOfTests;
}


/*************************************************************************
 * Function: firstOrderOptLinearTest
 *
 * This is an example and test of firstOrderOpt.cpp 
 *************************************************************************/

template <class Scalar>
class fo_test_model : public SpkModel<Scalar>
{
private:
    valarray<Scalar> alpha_; // current fixed effects
    valarray<Scalar>     b_; // current random effects
    int                  i_;    // current individual (does not matter)
public:
    fo_test_model() : alpha_(2) , b_(1)  {};        
    // use default destruction: ~fo_test_model() {};
private:
    void doSelectIndividual(int i)
    {	i_ = i; }
    void doSetPopPar(const valarray<Scalar>& alpha)
    {	alpha_ = alpha; }
    void doSetIndPar(const valarray<Scalar>& b)
    {	b_ = b; }
    void doIndParVariance( valarray<Scalar>& D ) const
    {	D.resize(1);
	D[0] = alpha_[1];
    }
    void doDataMean( valarray<Scalar>& f ) const
    {	f.resize(1);
	f[0] = alpha_[0] + b_[0];	
    }
    void doDataVariance( valarray<Scalar>& R ) const
    {	R.resize(1);
        R[0] = Scalar(1.0);
    }
    void doDataVarianceInv( valarray<Scalar>& Rinv ) const
    {	Rinv.resize(1);
	Rinv[0] = Scalar(1.0);
    }
    bool doIndParVariance_popPar( valarray<double>& D ) const
    {	CPPUNIT_ASSERT_MESSAGE(
        "firstOrderOptTest: call to doIndParVariance_popPar", false);
        return false;
    }
    void doIndParVarianceInv( valarray<double>& ) const
    {	CPPUNIT_ASSERT_MESSAGE(
        "firstOrderOptTest: call to doIndParVarianceInv", false);
        return;
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

void firstOrderOptTest::firstOrderOptLinearTest()
{ // temporary indices
  size_t i;

  // number of inidividuals in the simulation  (can be changed)
  const size_t M = 100;

  // standard deviation of the random effects
  const double std_b = 2.;

  // simulation fixed effects values 
  valarray<double> alpha_true(2);
  alpha_true[0] = 2.;
  alpha_true[1] = std_b * std_b;

  // simulation random effects values
  valarray<double> b_all_true(M);
  b_all_true = std_b * randNormal(M);

  // simulation measurement noise values
  valarray<double> noise(M);
  noise = randNormal(M); 
   
  // model, adModel
  fo_test_model<double> model;
  fo_test_model< CppAD::AD<double> > adModel; 

  // dvecN, dvecY
  DoubleMatrix dvecN(M, 1);
  double *N = dvecN.data();
  DoubleMatrix dvecY(M, 1);
  double *Y = dvecY.data();
  for(i = 0; i < M; i++)
  {   // number of measurements for this subject
      N[i] = 1.;
      // value of measurement for this subject 
      Y[i] = alpha_true[0] + b_all_true[i] + noise[i];
  }

  // fixed effects optimizer (linear least squares problem)
  double epsilon = 1e-6;
  int    max_itr = 2;
  int    level   = 1;
  Optimizer alpOptInfo(epsilon, max_itr, level); 
  // Set these to exercise the warm start capabilities of firstOrderOpt.
  alpOptInfo.setThrowExcepIfMaxIter( false );
  alpOptInfo.setSaveStateAtEndOfOpt( true );

  // fixed effects arguements to firstOrderOpt
  size_t m = 2; // number of fixed effects
  DoubleMatrix dvecAlpLow ( m, 1 );
  DoubleMatrix dvecAlpUp  ( m, 1 );
  DoubleMatrix dvecAlpIn  ( m, 1 );
  DoubleMatrix dvecAlpOut ( m, 1 );
  DoubleMatrix dvecAlpStep( m, 1 );

  double* alpLow  = dvecAlpLow .data();
  double* alpUp   = dvecAlpUp  .data();
  double* alpIn   = dvecAlpIn  .data();
  double* alpStep = dvecAlpStep.data();

  // Set the values associated with first fixed effect
  alpLow [ 0 ] = alpha_true[0] - 10.0;
  alpUp  [ 0 ] = alpha_true[0] + 10.0;
  alpIn  [ 0 ] = alpha_true[0] - 2.;
  alpStep[ 0 ] = 1.0e-2;

  // Set the values associated with second firxed effect
  alpLow [ 1 ] = alpha_true[1] / 10.;
  alpUp  [ 1 ] = alpha_true[1] * 10.;
  alpIn  [ 1 ] = alpha_true[1] / 2.;
  alpStep[ 1 ] = alpha_true[1] / 100.;

  // random effects optimizer (not a linear least squares problem)
  epsilon = 1e-6;
  max_itr = 50;
  level   = 1;
  Optimizer bOptInfo(epsilon, max_itr, level); 
  bOptInfo.setThrowExcepIfMaxIter( true );
  
  // random effects arguments to firstOrderOpt
  size_t n = 1;   // number of random effects per individual
  DoubleMatrix dvecBLow ( n, 1 );
  DoubleMatrix dvecBUp  ( n, 1 );
  DoubleMatrix dvecBStep( n, 1 );
  DoubleMatrix dmatBIn  ( n, M );
  DoubleMatrix dmatBOut ( n, M );

  double *bLow  = dvecBLow.data();
  double *bUp   = dvecBUp .data();
  double *bStep = dvecBStep.data();
  double *bIn   = dmatBIn.data();

  bLow[0]    = - 3. * std_b;
  bUp[0]     = + 3. * std_b;
  bStep[0]   =  10e-2 * std_b; 
  dmatBIn.fill( 0.0 );

  // objective function values
  double dLTildeOut;
  DoubleMatrix drowLTilde_alpOut     ( 1, m );
  DoubleMatrix dmatLTilde_alp_alpOut ( m, m );

  try
  {   firstOrderOpt(
          model                 ,
          adModel               ,
          dvecN                 ,
          dvecY                 ,
          alpOptInfo            ,
          dvecAlpLow            ,
          dvecAlpUp             ,
          dvecAlpIn             ,
          &dvecAlpOut           ,
          dvecAlpStep           ,
          bOptInfo              ,
          dvecBLow              ,
          dvecBUp               ,
          dmatBIn               ,
          &dmatBOut             ,
          dvecBStep             ,
          &dLTildeOut           ,
          &drowLTilde_alpOut    ,
          &dmatLTilde_alp_alpOut
     );
  }
  catch(...)
  {  CPPUNIT_ASSERT_MESSAGE(
         "firstOrderOptLinearTest: an exception occurred.", 
         false
     );
  }

  //************************************************************
  // Note: equations for the known (analytic) values computed 
  // here are derived in "An Introduction to Mixed Effects
  // Modeling and Marginal Likelihood Estimation with a 
  // Pharmacokinetic Example", B. M. Bell, Applied Physics
  // Laboratory, University of Washington, May 25, 2000.
  //************************************************************

  // Compute the mean of the data, yBar.
  double yBar = 0.0;
  for( i = 0; i < M; i++ )
    yBar += Y[ i ];
  yBar /= double(M);

  // Compute the sample variance
  double sample_variance = 0.0;
  for ( i = 0; i < M; i++ )
    sample_variance += (Y[i] - yBar) * (Y[i] - yBar);
  sample_variance /= double(M);

  // The value for alpHat(0) and alpHat(1) are contained in
  // section 9 of the above reference.
  double alphaHat_0 = yBar;
  double alphaHat_1 = sample_variance - 1.0;

  // check estimates
  bool ok = true;
  ok &= fabs(*(dvecAlpOut.data()+0) / alphaHat_0 - 1.) < 1e-4;
  ok &= fabs(*(dvecAlpOut.data()+1) / alphaHat_1 - 1.) < 1e-4;
  CPPUNIT_ASSERT_MESSAGE(
      "firstOrderOptLinearTest: alphaOut is not correct.",
      ok
  );

  return;
}
// END VERBATIM
