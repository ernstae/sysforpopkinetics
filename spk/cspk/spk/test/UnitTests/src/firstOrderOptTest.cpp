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
 * Author: Brad Bell
 *
 *************************************************************************/
/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

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
#include "../../../spk/WarningsManager.h"

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
        "firstOrderOptAnalyticTest", 
        &firstOrderOptTest::firstOrderOptAnalyticTest
    )
  );
  suiteOfTests->addTest(
    new TestCaller<firstOrderOptTest>(
        "firstOrderOptExampleTest", 
        &firstOrderOptTest::firstOrderOptExampleTest
    )
  );
  suiteOfTests->addTest(
    new TestCaller<firstOrderOptTest>(
        "firstOrderOptBackupTest", 
        &firstOrderOptTest::firstOrderOptBackupTest
    )
  );
  suiteOfTests->addTest(
    new TestCaller<firstOrderOptTest>(
        "firstOrderOptPostHocFailsTest", 
        &firstOrderOptTest::firstOrderOptPostHocFailsTest
    )
  );
  suiteOfTests->addTest(
    new TestCaller<firstOrderOptTest>(
        "firstOrderOptRestartTest", 
        &firstOrderOptTest::firstOrderOptRestartTest
    )
  );
  suiteOfTests->addTest(
    new TestCaller<firstOrderOptTest>(
        "firstOrderOptZeroIterationsTest", 
        &firstOrderOptTest::firstOrderOptZeroIterationsTest
    )
  );

  return suiteOfTests;
}


// BEGIN VERBATIM 
/*************************************************************************
 * Function: firstOrderOptAnalyticTest
 *
 * This is an example and test of firstOrderOpt.cpp  written by Brad Bell
 *************************************************************************/

// link so that Value works with double as well as AD<double>
double Value(double x)
{   return x; }

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
    {   i_ = i; }
    void doSetPopPar(const valarray<Scalar>& alpha)
    {   alpha_ = alpha; }
    void doSetIndPar(const valarray<Scalar>& b)
    {   b_ = b; }
    void doIndParVariance( valarray<Scalar>& D) const
    {   D.resize(1);
        D[0] = alpha_[1];
    }
    bool doIndParVariance_popPar( valarray<double>& D_alp ) const
    {   D_alp.resize(2);
        D_alp[0] = 0.;
        D_alp[1] = 1.;
        return true;
    }
    void doIndParVarianceInv( valarray<double>& Dinv) const
    {   Dinv.resize(1);
        Dinv[0] = 1. / Value(alpha_[1]);
    }
    bool doIndParVarianceInv_popPar( valarray<double>& Dinv_alp ) const
    {   Dinv_alp.resize(2);
        Dinv_alp[0] = 0.;
        Dinv_alp[1] = - 1. / Value(alpha_[1] * alpha_[1]);
        return true;
    }
    void doDataMean( valarray<Scalar>& f ) const
    {   f.resize(1);
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
    {   R.resize(1);
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
    {   Rinv.resize(1);
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

void firstOrderOptTest::firstOrderOptAnalyticTest()
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
  DoubleMatrix dmatLtilde_alpOut     ( m, M );

  try
  {   firstOrderOpt(
          model                  ,
          adModel                ,
          dvecN                  ,
          dvecY                  ,
          alpOptInfo             ,
          dvecAlpLow             ,
          dvecAlpUp              ,
          dvecAlpIn              ,
          &dvecAlpOut            ,
          dvecAlpStep            ,
          bOptInfo               ,
          dvecBLow               ,
          dvecBUp                ,
          dmatBIn                ,
          &dmatBOut              ,
          dvecBStep              ,
          &dLtildeOut            ,
          &drowLtilde_alpOut     ,
          &dmatLtilde_alp_alpOut ,
          &dmatLtilde_alpOut  
     );
  }
  catch(...)
  {  CPPUNIT_ASSERT_MESSAGE(
         "firstOrderOptAnalyticTest: an exception occurred.", 
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
  if( check < alpLow[1] )
      check = alpLow[1];
  if( check > alpUp[1] )
      check = alpUp[1];
  double alphaHat_1 = *(dvecAlpOut.data()+1);
  ok_alpha          &= fabs(alphaHat_1 / check - 1.) < 1e-4;
  CPPUNIT_ASSERT_MESSAGE(
      "firstOrderOptAnalyticTest: alphaOut is not correct.",
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
      "firstOrderOptAnalyticTest: BOut is not correct.",
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
  bool ok_L = true;
  for( i = 0; i < M; i++)
  {  double Vi       = sigma * sigma + alphaHat_1;
     double Vi_alp_1 = 1;
     double ri       = Y[i] - alphaHat_0;
     double ri_alp_0 = -1;
     Ltilde         += .5 * ( log( 2 * pi * Vi ) + ri * ri / Vi );
     double temp     = ri * ri_alp_0 / Vi;
     ok_L           &= fabs(*(dmatLtilde_alpOut.data()+0+2*i) - temp) < 1e-4;
     Ltilde_alp_0   += temp;
     Hessian_00     += ri_alp_0 * ri_alp_0 / Vi;
     temp            = .5 * Vi_alp_1 / Vi - .5 * ri * ri * Vi_alp_1 / (Vi * Vi);
     ok_L           &= fabs(*(dmatLtilde_alpOut.data()+1+2*i) - temp) < 1e-4;
     Ltilde_alp_1   += temp;
     Hessian_11     += -.5 * Vi_alp_1 * Vi_alp_1 / (Vi * Vi)
                     + ri * ri * Vi_alp_1 * Vi_alp_1 / (Vi * Vi * Vi);
  }
  ok_L &= fabs(dLtildeOut / Ltilde - 1) < 1e-4;
  ok_L &= fabs(*(dmatLtilde_alp_alpOut.data()+0) / Hessian_00 - 1) < 1e-4;
  ok_L &= fabs(*(dmatLtilde_alp_alpOut.data()+1) ) < 1e-4;
  ok_L &= fabs(*(dmatLtilde_alp_alpOut.data()+2) ) < 1e-4;
  ok_L &= fabs(*(dmatLtilde_alp_alpOut.data()+3) / Hessian_11 - 1) < 1e-4;
  // do use relative accuracy on derivatives because are zero
  // at the true minimizer.
  ok_L &= fabs(*(drowLtilde_alpOut.data()+0) - Ltilde_alp_0 ) < 1e-8;
  ok_L &= fabs(*(drowLtilde_alpOut.data()+1) - Ltilde_alp_1 ) < 1e-8;
  CPPUNIT_ASSERT_MESSAGE(
      "firstOrderOptAnalyticTest: Ltilde or its derivative is not correct.",
      ok_L
  ); 
  return;
}
// END VERBATIM
#include "../../../spk/namespace_population_analysis.h"

/*************************************************************************
 * Function: doTheTest
 *************************************************************************/
void firstOrderOptTest_doTheTest( 
     bool ok,
     double dLTildeOut,
     double dLTildeKnown,
     const double        epsB,
     const double        epsAlp,
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
     const DoubleMatrix& dmatLambdaTilde_alpOut,
     const DoubleMatrix& dmatLambdaTilde_alpKnown,
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


/*************************************************************************
 * Function: firstOrderOptExampleTest
 *
 *
 * example problem from the firstOrderOpt specification. 
 *************************************************************************/

template <class Scalar>
class UserModelFirstOrderOptExampleTest : public SpkModel<Scalar>
{
    valarray<Scalar> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nYi;
public:
    UserModelFirstOrderOptExampleTest(int nA, int nB, int nYi)
      :
      _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
    {};    
    ~UserModelFirstOrderOptExampleTest(){};
private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<Scalar>& aval)
    {
        _a = aval;
    }
    void doSetIndPar(const valarray<Scalar>& bval)
    {
        _b = bval;
    }
    void doIndParVariance( valarray<Scalar>& ret ) const
    {
        //
        // D = [ alp[1] ]
        //
        ret.resize(_nYi);
        ret[0] = _a[1];
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        //
        // D_alp = [ 0  1 ]
        //
        ret.resize(_nYi * _nA);
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
        ret[0] = ( 1.0 / Value(_a[1]) );
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        // Dinv_alp = [ 0    -alp[1]^(-2) ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = -1.0 / Value(_a[1]*_a[1]);
        return true;
    }
    void doDataMean( valarray<Scalar>& ret ) const
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
    void doDataVariance( valarray<Scalar>& ret ) const
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
    void doDataVarianceInv( valarray<Scalar>& ret ) const
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

  UserModelFirstOrderOptExampleTest<double>  
        model( nAlp, nB, nYi );
  UserModelFirstOrderOptExampleTest< CppAD::AD<double> >  
        adModel( nAlp, nB, nYi );


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
                     adModel,
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
  double pi = 4. * atan(1.);
  double dLTildeKnown = sumYMinAlp1Sqd / ( 2.0 * ( onePlusAlp2 ) ) 
    + nInd / 2.0 * log( 2.0 * pi * ( onePlusAlp2 ) );

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

  firstOrderOptTest_doTheTest(  ok,
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

/*************************************************************************
 * Function: firstOrderOptBackupTest
 *
 *
 * This test causes the optimizer to back up by setting the values for
 * the data mean f equal to infinity the tenth time that eval() is
 * called.
 *
 * This test re-uses the example problem from the firstOrderOpt
 * specification.
 *
 *************************************************************************/

template <class Scalar>
class UserModelFirstOrderOptBackupTest : public SpkModel<Scalar>
{
    valarray<Scalar> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nYi;
    mutable int _nEvalCall;
public:
    UserModelFirstOrderOptBackupTest(int nA, int nB, int nYi)
      :
      _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB), _nEvalCall(0)
    {};    
    ~UserModelFirstOrderOptBackupTest(){};
private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<Scalar>& aval)
    {
        _a = aval;
    }
    void doSetIndPar(const valarray<Scalar>& bval)
    {
        _b = bval;
    }
    void doIndParVariance( valarray<Scalar>& ret ) const
    {
        //
        // D = [ alp[1] ]
        //
        ret.resize(_nYi);
        ret[0] = _a[1];
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        //
        // D_alp = [ 0  1 ]
        //
        ret.resize(_nYi * _nA);
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
        ret[0] = ( 1.0 / Value(_a[1]) );
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        // Dinv_alp = [ 0    -alp[1]^(-2) ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = -1.0 / Value(_a[1]*_a[1]);
        return true;
    }
    void doDataMean( valarray<Scalar>& ret ) const
    {
        ret.resize(_nYi);

        // If this is not the tenth time this function has
        // been called, then set
        //
        // f = [ alp[0]+b[0] ] .
        //
        _nEvalCall++;
        if ( _nEvalCall != 100 )
        {
          ret[0] = ( _a[0] + _b[0] );
        }
        else
        {
          // If this is the tenth time this function has been called,
          // then throw an exception that looks like f was equal to a
          // NaN or infinite in order to make the optimizer back up.
          throw SpkException(
            SpkError::SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR,
            "The data mean was infinite.",
            __LINE__,
            __FILE__ );
        }
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
    void doDataVariance( valarray<Scalar>& ret ) const
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
    void doDataVarianceInv( valarray<Scalar>& ret ) const
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

void firstOrderOptTest::firstOrderOptBackupTest()
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

  UserModelFirstOrderOptBackupTest<double>  
        model( nAlp, nB, nYi );
  UserModelFirstOrderOptBackupTest< CppAD::AD<double> >  
        adModel( nAlp, nB, nYi );


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

  // Remove any warnings from previous unit tests.
  WarningsManager::clearAllWarnings();

  try
  {
    firstOrderOpt(
                   model,
                   adModel,
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
  }
  catch( const SpkException& e )
  {
    // Uncomment this line to see the list of exceptions.
    //cout << "e = " << e << endl;

    string warnings;
    WarningsManager::getAllWarnings( warnings );

    // Uncomment these statements to see the warnings.
    /*
    cout << "########################################" << endl;
    cout << warnings;
    cout << "########################################" << endl;
    */

    // See if the population optimizer backed up warning message was
    // issued.
    string::size_type msgPos = warnings.find( "Backed up population optimization", 0 );
    if( msgPos == string::npos )
    {
      CPPUNIT_ASSERT_MESSAGE( 
        "The population level optimizer did not back up.",
        false );
    }
  }
  catch( ... )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "An unexpected exception occurred during the call to firstOrderOpt.",
      false );
  }

}

/*************************************************************************
 * Function: firstOrderOptPostHocFailsTest
 *
 *
 * This test causes the post-hoc step to fail for the first, second,
 * or fourth individuals by setting the value for their derivatives of
 * the data mean f_b equal to infinity.
 *
 * This test re-uses the example problem from the firstOrderOpt
 * specification.
 *
 *************************************************************************/

template <class Scalar>
class UserModelFirstOrderOptPostHocFailsTest : public SpkModel<Scalar>
{
    valarray<Scalar> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nYi;
    mutable int _nEvalCall;
public:
    UserModelFirstOrderOptPostHocFailsTest(int nA, int nB, int nYi)
      :
      _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB), _nEvalCall(0)
    {};    
    ~UserModelFirstOrderOptPostHocFailsTest(){};
private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<Scalar>& aval)
    {
        _a = aval;
    }
    void doSetIndPar(const valarray<Scalar>& bval)
    {
        _b = bval;
    }
    void doIndParVariance( valarray<Scalar>& ret ) const
    {
        //
        // D = [ alp[1] ]
        //
        ret.resize(_nYi);
        ret[0] = _a[1];
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        //
        // D_alp = [ 0  1 ]
        //
        ret.resize(_nYi * _nA);
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
        ret[0] = ( 1.0 / Value(_a[1]) );
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        // Dinv_alp = [ 0    -alp[1]^(-2) ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = -1.0 / Value(_a[1]*_a[1]);
        return true;
    }
    void doDataMean( valarray<Scalar>& ret ) const
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
        ret.resize(_nYi * _nB);

        // If this is not the first, second, or fourth individual,
        // then set
        //
        // f_b = [ 1 ]
        //
        if ( !( _i == 0 || _i == 1 || _i == 3 ) )
        {
          ret[0] = 1.0;
        }
        else
        {
          // If this is the first, second, or fourth individual, then
          // throw an exception that looks like the derivative of f
          // was equal to a NaN or infinite in order to make the
          // post-hoc optimization fail for this individual.
          throw SpkException(
            SpkError::SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR,
            "The derivative of the data mean was infinite.",
            __LINE__,
            __FILE__ );
        }
        return true;
    }
    void doDataVariance( valarray<Scalar>& ret ) const
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
    void doDataVarianceInv( valarray<Scalar>& ret ) const
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

void firstOrderOptTest::firstOrderOptPostHocFailsTest()
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

  UserModelFirstOrderOptPostHocFailsTest<double>  
        model( nAlp, nB, nYi );
  UserModelFirstOrderOptPostHocFailsTest< CppAD::AD<double> >  
        adModel( nAlp, nB, nYi );


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

  // Remove any warnings from previous unit tests.
  WarningsManager::clearAllWarnings();

  try
  {
    firstOrderOpt(
                   model,
                   adModel,
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
  }
  catch( const SpkException& e )
  {
  }
  catch( ... )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "An unexpected exception occurred during the call to firstOrderOpt.",
      false );
  }


  //------------------------------------------------------------
  // Check that the post-hoc step failed for some of the individuals.
  //------------------------------------------------------------

  string warnings;
  WarningsManager::getAllWarnings( warnings );

  // Uncomment these statements to see the warnings.
  /*
  cout << "########################################" << endl;
  cout << warnings;
  cout << "########################################" << endl;
  */

  // See if the post-hoc optimization failed for some of the
  // individuals.
  string::size_type msgPos = warnings.find( "The post-hoc individual level optimization failed for the following", 0 );
  if( msgPos == string::npos )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The post-hoc individual level optimization did not fail.",
      false );
  }

}

/*************************************************************************
 * Function: firstOrderOptRestartTest
 *
 * This test re-uses the example problem from the firstOrderOpt
 * specification to check that the restart file machinery works for
 * the firstOrderOpt objective function.
 *************************************************************************/

void firstOrderOptTest::firstOrderOptRestartTest()
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

  UserModelFirstOrderOptExampleTest<double>
         model( nAlp, nB, nYi );
  UserModelFirstOrderOptExampleTest< CppAD::AD<double> >
         adModel( nAlp, nB, nYi );


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
  // Prepare the first population level optimizer controller.
  //------------------------------------------------------------

  // This file will hold the restart information.
  const string restartFile = "firstOrderTest_restart_info.xml";

  // Set these flags so that the restart information will not be read
  // from the restart file, but it will be saved to the file.
  bool readRestartInfo  = false;
  bool writeRestartInfo = true;

  // Set the optimizer control information so that the optimizer will
  // not converge the first time it is called.
  double epsilon = 1.e-6;
  int nMaxIter   = 5; 
  int level      = 0;

  // Instantiate the first population level optimizer controller.
  Optimizer firstPopOptimizer(
    epsilon,
    nMaxIter,
    level, 
    restartFile,
    readRestartInfo,
    writeRestartInfo ); 

  // Set these flags so that no exception will be thrown and so that
  // the state information required for a warm start will be saved.
  firstPopOptimizer.setThrowExcepIfMaxIter( false );
  firstPopOptimizer.setSaveStateAtEndOfOpt( true );


  //------------------------------------------------------------
  // Prepare the individual level optimizer controller.
  //------------------------------------------------------------

  Optimizer indOptimizer( 1.0e-6, 40, 0 );


  //------------------------------------------------------------
  // Call firstOrderOpt for the first time.
  //------------------------------------------------------------

  try
  {
    firstOrderOpt(
                   model,
                   adModel,
                   dvecN,
                   dvecY,
                   firstPopOptimizer,
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
  }
  catch(...)
  {
    CPPUNIT_ASSERT(false);
  }


  //------------------------------------------------------------
  // Prepare the second population level optimizer controller.
  //------------------------------------------------------------

  // Increase the number of iterations so that the optimizer will be
  // able to converge successfully.
  nMaxIter = 50; 

  // Set these flags so that the restart information will be retrieved
  // from the restart file, and so it will be saved to the file.
  readRestartInfo  = true;
  writeRestartInfo = true;

  // Instantiate the second population level optimizer controller.
  Optimizer secondPopOptimizer(
    epsilon,
    nMaxIter,
    level,
    restartFile,
    readRestartInfo,
    writeRestartInfo ); 

  // Set these flags so that a warm start will be performed using the
  // state information from the restart file and so that the state
  // information required for a warm start will be saved.
  secondPopOptimizer.setIsWarmStart( true );
  secondPopOptimizer.setSaveStateAtEndOfOpt( true );


  //------------------------------------------------------------
  // Call firstOrderOpt for the second time.
  //------------------------------------------------------------

  try
  {
    firstOrderOpt(
                   model,
                   adModel,
                   dvecN,
                   dvecY,
                   secondPopOptimizer,
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
  }
  catch(...)
  {
    CPPUNIT_ASSERT(false);
  }

  bool ok = true;


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
  double pi             = 4. * atan(1.);
  double dLTildeKnown = sumYMinAlp1Sqd / ( 2.0 * ( onePlusAlp2 ) ) 
    + nInd / 2.0 * log( 2.0 * pi * ( onePlusAlp2 ) );

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

  firstOrderOptTest_doTheTest(  ok,
              dLTildeOut,
              dLTildeKnown,
              indOptimizer.getEpsilon(),
              secondPopOptimizer.getEpsilon(),
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

  
  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Delete the restart file.
  if ( remove( restartFile.c_str() ) )
  {
    CPPUNIT_ASSERT_MESSAGE( "Unable to delete the restart file.", false );
  }
}
/*************************************************************************
 * Function: firstOrderOptZeroIterationsTest
 *
 * This test calls firstOrdeOpt with zero iterations at the population level,
 * with zero iterations at the individual level, and with zero iterations 
 * at both the population and the individual levels.
 *
 * At the individual level this test implements the example problem from 
 * the mapOpt specification. 
 *************************************************************************/

#include "../../../spk/EqIndModel.h"

template <class Scalar>
class UserModelFirstOrderOptZeroIterationsTest : public SpkModel<Scalar>
{
    valarray<Scalar> _a, _b;
    int _i;
    const int _nA;
    const int _nB;
    const int _nYi;
public:
    UserModelFirstOrderOptZeroIterationsTest(int nA, int nB, int nYi)
      : _nA(nA), _nB(nB), _nYi(nYi), _a(nA), _b(nB)
    {}; 
    ~UserModelFirstOrderOptZeroIterationsTest(){};
private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<Scalar>& aval)
    {
      assert(aval.size() == _nA);
        _a = aval;
    }
    void doSetIndPar(const  valarray<Scalar>& bval)
    {
      assert(bval.size() == _nB);
        _b = bval;
    }
    void doIndParVariance( valarray<Scalar>& ret ) const
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
        //                 / 1    0 \ 
        //     D^-1(alp) = |        |  .
        //                 \ 0    2 /
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
    void doDataMean( valarray<Scalar>& ret ) const
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
    void doDataVariance( valarray<Scalar>& ret ) const
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
        ret[0] = exp( Value(_b[0]) );
        ret[3] = exp( Value(_b[0]) );
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
        ret[0] = 1.0 / exp( Value(_b[0]) );
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = 1.0 / exp( Value(_b[0]) );
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
        ret[0] = -1.0 / exp( Value(_b[0]) );
        ret[3] = -1.0 / exp( Value(_b[0]) );
        return true;
    }   
};

void firstOrderOptTest::firstOrderOptZeroIterationsTest()
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

  UserModelFirstOrderOptZeroIterationsTest<double>
        model( nAlp, nB, nYPerInd);
  UserModelFirstOrderOptZeroIterationsTest< CppAD::AD<double> >
        adModel( nAlp, nB, nYPerInd);


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

  DoubleMatrix drowLTilde_alpOut     ( 1,    nAlp );
  DoubleMatrix dmatLTilde_alp_alpOut ( nAlp, nAlp );
  DoubleMatrix dmatLambdaTilde_alpOut( nAlp, nInd );

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
  double* pdLTilde_alpKnownData;

  // The derivatives of each individual's contribution to lTilde
  // should be equal.
  DoubleMatrix dmatLambdaTilde_alpKnown( nAlp, nInd );
  double* pdLambdaTilde_alpKnownData;

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

    bool okFirstOrderOpt;
    try
    {
      firstOrderOpt( 
                     model,
                     adModel,
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
                     &dmatLambdaTilde_alpOut 
                   );
      okFirstOrderOpt = true;
    }
    catch(...)
    {
        CPPUNIT_ASSERT(false);
    }

    //----------------------------------------------------------
    // Compute the known values for LTilde and LTilde_alp.
    //----------------------------------------------------------

    bool okLTilde;
    int k;
    
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
    try{


/*
        lTilde(  model,
                 NAIVE_FIRST_ORDER,
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
*/
    std::valarray<int> N( nInd );
        for( k = 0; k < nInd; k++ )
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
      CPPUNIT_ASSERT( false );
    }
    
    pdLTilde_alpKnownData      = drowLTilde_alpKnown.data();
    pdLambdaTilde_alpKnownData = dmatLambdaTilde_alpKnown.data();

    // Calculate the derivatives of each individual's contribution to
    // lTilde, which should be equal since they all have the same data
    // and should all have the same b values.
    for ( j = 0; j < nAlp; j++ )
    {
      for ( k = 0; k < nInd; k++ )
      {
        pdLambdaTilde_alpKnownData[ j + k * nAlp ] = pdLTilde_alpKnownData[ j ] / nInd;
      }
    }


    //----------------------------------------------------------
    // Compare the results.
    //----------------------------------------------------------

    firstOrderOptTest_doTheTest(  okFirstOrderOpt,
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
}
