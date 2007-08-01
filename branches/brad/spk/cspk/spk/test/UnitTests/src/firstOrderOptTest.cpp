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

// link so that Value works with double as well as AD<double>
double Value(double x)
{	return x; }

template <class Scalar>
class fo_test_model : public SpkModel<Scalar>
{
private:
    const double     sigma_; // standard deviation of measurement noise 
    valarray<Scalar> alpha_; // current fixed effects
    valarray<Scalar>     b_; // current random effects
    int                  i_; // current individual (does not matter)
public:
    fo_test_model(double sigma) : sigma_(sigma),  alpha_(2) , b_(1)  {};        
    // use default destruction: ~fo_test_model() {};
private:
    void doSelectIndividual(int i)
    {	i_ = i; }
    void doSetPopPar(const valarray<Scalar>& alpha)
    {	alpha_ = alpha; }
    void doSetIndPar(const valarray<Scalar>& b)
    {	b_ = b; }
    void doIndParVariance( valarray<Scalar>& D) const
    {	D.resize(1);
	D[0] = alpha_[1];
    }
    bool doIndParVariance_popPar( valarray<double>& D_alp ) const
    {	D_alp.resize(2);
        D_alp[0] = 0.;
        D_alp[1] = 1.;
        return true;
    }
    void doIndParVarianceInv( valarray<double>& Dinv) const
    {	Dinv.resize(1);
	Dinv[0] = 1. / Value(alpha_[1]);
    }
    bool doIndParVarianceInv_popPar( valarray<double>& Dinv_alp ) const
    {   Dinv_alp.resize(2);
	Dinv_alp[0] = 0.;
        Dinv_alp[1] = - 1. / Value(alpha_[1] * alpha_[1]);
        return true;
    }
    void doDataMean( valarray<Scalar>& f ) const
    {	f.resize(1);
	f[0] = alpha_[0] + b_[0];	
    }
    bool doDataMean_popPar( valarray<double>& f_alp ) const
    {   f_alp.resize(2);
        f_alp[0] = 1.;
        f_alp[1] = 0.;
        return true;
    }
    bool doDataMean_indPar( valarray<double>& f_b ) const
    {   f_b.resize(1);
        f_b[0] = 1.;
        return true;
    }
    void doDataVariance( valarray<Scalar>& R ) const
    {	R.resize(1);
        R[0] = Scalar(sigma_ * sigma_);
    }
    bool doDataVariance_popPar( valarray<double>& R_alp ) const
    {   R_alp.resize(2);
        R_alp[0] = 0.;
        R_alp[1] = 0.;
        return false;
    }
    bool doDataVariance_indPar( valarray<double>& R_b ) const
    {   R_b.resize(1);
        R_b[0] = 0.;
        return false;
    }
    void doDataVarianceInv( valarray<Scalar>& Rinv ) const
    {	Rinv.resize(1);
	Rinv[0] = Scalar(1.0 / (sigma_ * sigma_));
    }
    bool doDataVarianceInv_popPar( valarray<double>& Rinv_alp ) const
    {   Rinv_alp.resize(2);
        Rinv_alp[0] = 0.;
        Rinv_alp[1] = 0.;
        return false;
    }
    bool doDataVarianceInv_indPar( valarray<double>& Rinv_b ) const
    {   Rinv_b.resize(1);
        Rinv_b[0] = 0.;
        return false;
    }
};

void firstOrderOptTest::firstOrderOptLinearTest()
{ // temporary indices
  size_t i;

  // number of inidividuals in the simulation  (can be changed)
  const size_t M = 10;

  // simulation value for standard deviation of the random effects
  const double std_b =  .5;

  // simulation value for standard deviation of measurement noise
  const double sigma = 1.;

  // simulation value for mean of data
  const double mean_y = 3.;

  // simulation fixed effects values 
  valarray<double> alpha_true(2);
  alpha_true[0] = mean_y;
  alpha_true[1] = std_b * std_b;

  // simulation random effects values
  valarray<double> b_all_true(M);
  b_all_true = std_b * randNormal(M);

  // simulation measurement noise values
  valarray<double> noise(M);
  noise = randNormal(M); 
   
  // model, adModel
  fo_test_model<double> model( sigma );
  fo_test_model< CppAD::AD<double> > adModel( sigma ); 

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
  double epsilon = 1e-5;
  int    max_itr = 20;
  int    level   = 0;
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
  level   = 0;
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
  bStep[0]   =  1e-2 * std_b; 
  dmatBIn.fill( 0.0 );

  // objective function values
  double dLtildeOut;
  DoubleMatrix drowLtilde_alpOut     ( 1, m );
  DoubleMatrix dmatLtilde_alp_alpOut ( m, m );

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
          &dLtildeOut           ,
          &drowLtilde_alpOut    ,
          &dmatLtilde_alp_alpOut
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

  // check fixed effects estimates, The value for alpHat(0) and alpHat(1) 
  // are contained in section 9 of the above reference.
  bool ok_alpha     = true;
  double check      = yBar;
  double alphaHat_0 = *(dvecAlpOut.data()+0);
  ok_alpha          &= fabs(alphaHat_0 / check - 1.) < 1e-4;
  check              = sample_variance - sigma * sigma;
  double alphaHat_1 = *(dvecAlpOut.data()+1);
  ok_alpha          &= fabs(alphaHat_1 / check - 1.) < 1e-4;
  CPPUNIT_ASSERT_MESSAGE(
      "firstOrderOptLinearTest: alphaOut is not correct.",
      ok_alpha
  );

  /* check random effects estimates
  mapObj = .5*b_i^2 / alp_1 + .5*[ ( y_i - alp_0 - b_i) / sigma ]^2
  0      = b_i / alp_1 - (y_i - alp_0 - b_i) / sigma^2
  0      = b_i (1 / alp_1 + 1 / sigma^2 ) - (y_i - alp_0 ) / sigma^2
  b_i    =  (y_i - alp_0 ) / ( sigma^2 / alp_1 + 1)
  */
  bool ok_b = true;
  for ( i = 0; i < M ; i++ )
  {   check = (Y[i] - alphaHat_0) / (sigma * sigma / alphaHat_1 + 1.);
      if( check < bLow[0] )
          check = bLow[0];
      if( check > bUp[0] )
          check = bUp[0];
      ok_b &= fabs(*(dmatBOut.data() + i) / check - 1.) < 1e-4;
  }
  CPPUNIT_ASSERT_MESSAGE(
      "firstOrderOptLinearTest: BOut is not correct.",
      ok_b
  ); 
  /* check fixed effects objective value
  Vi     = Ri + fi_b * D * fi_b' = Ri + D
  Ltilde = .5 * sum_i logdet(2*pi*Vi) + (yi-alp_0)' Vi^{-1} (yi-alp_0)
  */
  double pi             = 4. * atan(1.);
  double Ltilde         = 0.;
  double Ltilde_alp_0   = 0.;
  double Ltilde_alp_1   = 0.;
  double Hessian_00     = 0.;
  double Hessian_11     = 0.;
  for( i = 0; i < M; i++)
  {  double Vi       = sigma * sigma + alphaHat_1;
     double Vi_alp_1 = 1;
     double ri       = Y[i] - alphaHat_0;
     double ri_alp_0 = -1;
     Ltilde         += .5 * ( log( 2 * pi * Vi ) + ri * ri / Vi );
     Ltilde_alp_0   += ri * ri_alp_0 / Vi;
     Hessian_00     += ri_alp_0 * ri_alp_0 / Vi;
     Ltilde_alp_1   += .5 * Vi_alp_1 / Vi - .5 * ri * ri * Vi_alp_1 / (Vi * Vi);
     Hessian_11     += -.5 * Vi_alp_1 * Vi_alp_1 / (Vi * Vi)
                     + ri * ri * Vi_alp_1 * Vi_alp_1 / (Vi * Vi * Vi);
  }
  bool ok_L = true;
  ok_L &= fabs(dLtildeOut / Ltilde - 1) < 1e-4;
  ok_L &= fabs(*(dmatLtilde_alp_alpOut.data()+0) / Hessian_00 - 1) < 1e-4;
  ok_L &= fabs(*(dmatLtilde_alp_alpOut.data()+1) ) < 1e-4;
  ok_L &= fabs(*(dmatLtilde_alp_alpOut.data()+2) ) < 1e-4;
  ok_L &= fabs(*(dmatLtilde_alp_alpOut.data()+3) / Hessian_11 - 1) < 1e-4;
  // do not require much relative accuracy on derivatives because are zero
  // at the true minimizer.
  ok_L &= fabs(*(drowLtilde_alpOut.data()+0) / Ltilde_alp_0 - 1 ) < 1e-2;
  ok_L &= fabs(*(drowLtilde_alpOut.data()+1) / Ltilde_alp_1 - 1 ) < 1e-2;
  CPPUNIT_ASSERT_MESSAGE(
      "firstOrderOptLinearTest: Ltilde or its derivative is not correct.",
      ok_L
  ); 
  return;
}
// END VERBATIM
