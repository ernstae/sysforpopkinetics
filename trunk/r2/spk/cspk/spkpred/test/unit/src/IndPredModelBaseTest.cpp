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
 * File: IndPredModelBaseTest.cpp
 *
 *
 * Unit test for the class IndPredModelBase.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK Pred test suite header files.
#include "IndPredModelBaseTest.h"
#include "compareToKnown.h"

// SPK Pred library header files.
#include "../../../spkpred/DiagCov.h"
#include "../../../spkpred/FullCov.h"
#include "../../../spkpred/IndPredModel.h"
#include "../../../spkpred/IndPredModelBase.h"

// SPK library header files.
#include <spk/AkronBtimesC.h>
#include <spk/doubleToScalarArray.h>
#include <spk/fitIndividual.h>
#include <spk/identity.h>
#include <spk/inverse.h>
#include <spk/mapObj.h>
#include <spk/multiply.h>
#include <spk/pi.h>
#include <spk/replaceSubblock.h>
#include <spk/scalarToDoubleArray.h>
#include <spk/simulate.h>
#include <spk/SpkValarray.h>
#include <spk/transpose.h>
#include <spk/WarningsManager.h>

// CppAD header files.
#include <CppAD/CppAD.h>

// CppUnit framework header files.
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

// Standard library header files.
#include <iomanip>
#include <cassert>
#include <cmath>
#include <ctime>
#include <iostream>
#include <vector>

using namespace CppAD;
using namespace CppUnit;
using std::vector;
using SPK_VA::valarray;


/*------------------------------------------------------------------------
 * Local function definitions
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  // Calculates the MAP Bayesian objective function,
  //
  //                   1                       1           T  -1
  //     MapObj(b)  =  - logdet[ 2 pi R(b) ] + - [y - f(b)]  R  (b) [y - f(b)]
  //                   2                       2
  //                                                               
  //                   1                       1            T  -1
  //                +  - logdet[ 2 pi D ]    + - [bMean - b]  D   [bMean - b]  .
  //                   2                       2
  //
  template<class Scalar>
  void mapObjTemplate( SpkModel<Scalar>&       model, 
               const SPK_VA::valarray<Scalar>& y,
               const SPK_VA::valarray<Scalar>& b,
               Scalar*                         pMapObjOut,
               bool                            withD,
               const SPK_VA::valarray<Scalar>* pBMean )
  {
    //------------------------------------------------------------
    // Preliminaries.
    //------------------------------------------------------------

    using namespace std;

    int nB = b.size();
    int nY = y.size();

    const double logTwoPi = std::log( 2.0 * PI );


    //------------------------------------------------------------
    // Evaluate the model functions.
    //------------------------------------------------------------

    valarray<Scalar> f( nY );
    valarray<Scalar> R( nY * nY );

    model.setIndPar( b );

    // Evaluate
    //
    //     f(b)  .
    //
    model.dataMean( f );

    // Evaluate
    //
    //     R(b)  .
    //
    model.dataVariance( R );


    //------------------------------------------------------------
    // Calculate the residuals.
    //------------------------------------------------------------

    valarray<Scalar> yMinusF( nY );
    valarray<Scalar> bMeanMinusB( nB );

    // Calculate the data residuals,
    // 
    //     y - f(b)  .
    //
    yMinusF = y - f;

    // Set the mean value for b equal to zero if it not specified.
    valarray<Scalar> bMean( nB );
    if( pBMean )
    {
      bMean = *pBMean;
    }
    else
    {
      bMean = 0.0;
    }

    // Calculate the parameter residuals,
    //
    //     bMean - b  .
    //
    bMeanMinusB = bMean - b;


    //------------------------------------------------------------
    // Determine if the data variance is diagonal.
    //------------------------------------------------------------

    bool isDataVarianceDiag = true;

    int i;
    int j;

    // See if the data variance is diagonal.
    //
    // Only check its lower triangle, because it is symmetric.
    for ( i = 1; i < nY; i++ )
    {
      for ( j = 0; j < i; j++ )
      {
        // If any lower triangle element is non-zero, then the matrix
        // is not diagonal and this loop can be exited.
        if ( R[i + j * nY ] != 0.0 )
        {
          isDataVarianceDiag = false;
          break;
        }
      }
    }


    //------------------------------------------------------------
    // Calculate the data related terms in the objective function.
    //------------------------------------------------------------

    valarray<Scalar> x( nY );

    int signdet;
    Scalar logdet;

    // Calculate the product of the variance inverse and the residuals
    // by solving the equation
    //
    //            -1                
    //     x  =  R  (b) [y - f(b)]
    //
    // by solving for x in the linear equation
    //
    //     R(b) x  =  [y - f(b)]
    //
    signdet = CppAD::LuSolve( nY, 1, R, yMinusF, x, logdet );

    // Check the sign of the determinant of the data variance.
    if ( signdet <= 0.0 )
    {
      throw SpkException(
        SpkError::SPK_NOT_POS_DEF_ERR, 
        "The individual's data variance is not positive definite.",
        __LINE__,
        __FILE__ );
    }

    // Calculate the first term in MapObj(b),
    //
    //                                  -                                     -
    //     1                         1 |               nY                      |
    //     - logdet[ 2 pi R(b) ]  =  - |  log[ ( 2 pi )   ]  + logdet[ R(b) ]  |.
    //     2                         2 |                                       |
    //                                  -                                     -
    //
    //                                  -                                   -
    //                               1 |                                     |
    //                            =  - |  nY * log( 2 pi ) + logdet[ R(b) ]  |.
    //                               2 |                                     |
    //                                  -                                   -
    //
    Scalar mapObjOutTemp = 0.5 * ( nY * logTwoPi + logdet );

    // Calculate the second term in MapObj(b),
    //
    //     1           T  -1                   1           T      
    //     - [y - f(b)]  R  (b) [y - f(b)]  =  - [y - f(b)]   x  ,
    //     2                                   2
    //
    // where
    //            -1                
    //     x  =  R  (b) [y - f(b)]
    //
    Scalar weightedSumOfSquares = 0.0;
    for ( i = 0; i < nY; i++ )
    {
      weightedSumOfSquares += yMinusF[i] * x[i];
    }
    mapObjOutTemp += 0.5 * weightedSumOfSquares;


    //------------------------------------------------------------
    // Finish up.
    //------------------------------------------------------------

    if ( pMapObjOut )
    {
      *pMapObjOut = mapObjOutTemp;
    }

  }

} // [End: unnamed namespace]


/*------------------------------------------------------------------------
 * Local variable declarations
 *------------------------------------------------------------------------*/

namespace oneexpfpred_indpredmodelbasetest
{
  double timeStep = 0.25;
  double dose     = 320.0;
  double wt       = 79.6;
}

namespace railexample_indpredmodelbasetest
{
  //------------------------------------------------------------
  // Quantities related to the population.
  //------------------------------------------------------------

  // Set the number of individuals.
  const int nInd = 6;


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  // Set the number of data values per individual.
  const int nY_i = 3;

  // Set the number of data values for all of the individuals. 
  valarray<int> N( nY_i, nInd );

  // Create a C style array with the data values for all of the
  // individuals.
  double YCArray[nInd * nY_i] = {
      5.5000E+01,
      5.3000E+01,
      5.4000E+01,
      2.6000E+01,
      3.7000E+01,
      3.2000E+01,
      7.8000E+01,
      9.1000E+01,
      8.5000E+01,
      9.2000E+01,
      1.0000E+02,
      9.6000E+01,
      4.9000E+01,
      5.1000E+01,
      5.0000E+01,
      8.0000E+01,
      8.5000E+01,
      8.3000E+01 };

  // Set the data values for all of the individuals.
  valarray<double> Y( YCArray, nY_i * nInd  );

  // This will contain the data values for a particular individual.
  valarray<double> Y_i( nY_i );
}


/*------------------------------------------------------------------------
 * Local class declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{

  //**********************************************************************
  //
  // Class:  OneExpF_AdditivePlusThetaDepY_Pred
  //
  //
  // This class evaluates Pred block expressions that correspond to a
  // single exponential for the mean of the individuals' data,
  //
  //           ds * exp[ - theta(0) * time ]
  //    f  =  -------------------------------  ,
  //                [theta(1) * w ]
  //
  // and a combined model for the variance of the data that is additive
  // with a theta dependent term,
  //
  //    y  =  f + eta(0) + theta(2) * eta(1)  .
  //
  //**********************************************************************

  template<class Value>
  class OneExpF_AdditivePlusThetaDepY_Pred : public PredBase<Value>
  {
    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    OneExpF_AdditivePlusThetaDepY_Pred( int nY_iIn )
    :
    nY_i ( nY_iIn ),
    timeStep( oneexpfpred_indpredmodelbasetest::timeStep ),
    dose    ( oneexpfpred_indpredmodelbasetest::dose     ),
    wt      ( oneexpfpred_indpredmodelbasetest::wt       )
    {}

    ~OneExpF_AdditivePlusThetaDepY_Pred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;

  public:
    Value timeStep;
    Value dose;
    Value wt;

    Value ds;
    Value w;
    Value ka;
    Value ke;
    Value cl;
    Value d;
    Value e;


    //**********************************************************
    // 
    // Function: eval
    //
    //**********************************************************

    bool eval(
      int thetaOffset, int thetaLen,
      int etaOffset,   int etaLen,
      int epsOffset,   int epsLen,
      int fOffset,     int fLen,
      int yOffset,     int yLen,
      int i,
      int j,
      const std::vector<Value>& indepVar,
      std::vector<Value>& depVar )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace oneexpfpred_indpredmodelbasetest;

      Value time = timeStep * j;

      ds = dose * wt;
      w = wt;


      //--------------------------------------------------------
      // Evaluate the mean of the data and its predicted value.
      //--------------------------------------------------------

      // Calculate
      //
      //           ds * exp[ - theta(0) * time ]
      //    f  =  -------------------------------  ,
      //                [theta(1) * w ]
      //
      // and
      //
      //    y  =  f + eta(0) + theta(2) * eta(1)  .
      //
      ke = indepVar[thetaOffset + 0];
      cl = indepVar[thetaOffset + 1] * w;
      d = CppAD::exp(-ke * time);
      e = cl;
      depVar[fOffset + j] = ds * d / e;
      depVar[yOffset + j] = depVar[fOffset + j] + indepVar[etaOffset + 0]
        + indepVar[thetaOffset + 2] * indepVar[etaOffset + 1];


      //--------------------------------------------------------
      // Finish up.
      //--------------------------------------------------------

      // Return true to indicate that this is not a Missing Data 
      // Variable (MDV).
      return true;
    }


    //**********************************************************
    // 
    // Function: getNObservs
    //
    //**********************************************************

    int getNObservs( int i ) const
    {
      return nY_i;
    }


    //------------------------------------------------------------
    // Disallowed, implicitly generated member functions.
    //------------------------------------------------------------

  protected:
    OneExpF_AdditivePlusThetaDepY_Pred(){}
    OneExpF_AdditivePlusThetaDepY_Pred( const OneExpF_AdditivePlusThetaDepY_Pred& ){}
    OneExpF_AdditivePlusThetaDepY_Pred & operator=( const OneExpF_AdditivePlusThetaDepY_Pred& ){}
  };


  //**********************************************************************
  //
  // Class:  RailExample_Pred
  //
  //
  // This test causes the optimizer to back up by setting the values for F
  // equal to NaN or infinity the tenth time that eval() is called.
  //
  // This class evaluates Pred block expressions that correspond to
  // the Rail Example that is included in the NLME distribution.
  //
  // The PRED block for the Rail Example after it has been converted
  // to individual notation is
  //
  //     $PRED 
  //     F = THETA(1)
  //     Y = F + ETA(1)
  //
  //**********************************************************************

  template<class Value>
  class RailExample_Pred : public PredBase<Value>
  {
    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    RailExample_Pred( int nY_iIn )
    :
    nY_i      ( nY_iIn ),
    nEvalCall ( 0 )
    {}

    ~RailExample_Pred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;
    int nEvalCall;


    //**********************************************************
    // 
    // Function: eval
    //
    //**********************************************************

    bool eval(
      int thetaOffset, int thetaLen,
      int etaOffset,   int etaLen,
      int epsOffset,   int epsLen,
      int fOffset,     int fLen,
      int yOffset,     int yLen,
      int i,
      int j,
      const std::vector<Value>& indepVar,
      std::vector<Value>& depVar )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace railexample_indpredmodelbasetest;


      //--------------------------------------------------------
      // Evaluate the mean of the data and its predicted value.
      //--------------------------------------------------------

      // If this is not the tenth time this function has
      // been called, then set
      //           
      //    f  =  theta(0)  ,
      //
      // and
      //
      //    y  =  f + eta(0)  .
      //
      nEvalCall++;
      if ( nEvalCall != 10 )
      {
        depVar[fOffset + j] = indepVar[thetaOffset + 0];
      }
      else
      {
        // If this is the tenth time this function has been called,
        // then set f to be NaN to make the optimizer back up.
        Value zero = Value( 0 );
        depVar[fOffset + j] = zero / zero;
      }
      depVar[yOffset + j] = depVar[fOffset + j] + indepVar[etaOffset + 0];


      //--------------------------------------------------------
      // Finish up.
      //--------------------------------------------------------

      // Return true to indicate that this is not a Missing Data 
      // Variable (MDV).
      return true;
    }


    //**********************************************************
    // 
    // Function: getNObservs
    //
    //**********************************************************

    int getNObservs( int i ) const
    {
      return nY_i;
    }


    //------------------------------------------------------------
    // Disallowed, implicitly generated member functions.
    //------------------------------------------------------------

  protected:
    RailExample_Pred(){}
    RailExample_Pred( const RailExample_Pred& ){}
    RailExample_Pred & operator=( const RailExample_Pred& ){}
  };


} // [End: unnamed namespace]


/*************************************************************************
 *
 * Function: setUp
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void IndPredModelBaseTest::setUp()
{
    // initializations
}


/*************************************************************************
 *
 * Function: tearDown
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

void IndPredModelBaseTest::tearDown()
{
    // clean up
}


/*************************************************************************
 *
 * Function: suite
 *
 *
 * CppUnit framework function.
 *
 *************************************************************************/

Test* IndPredModelBaseTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "IndPredModelBaseTest" );

  suiteOfTests->addTest(new TestCaller<IndPredModelBaseTest>(
    "OneExpF_AdditivePlusThetaDepY_dataMean_Test", 
    &IndPredModelBaseTest::OneExpF_AdditivePlusThetaDepY_dataMean_Test ));

  suiteOfTests->addTest(new TestCaller<IndPredModelBaseTest>(
    "OneExpF_AdditivePlusThetaDepY_dataVariance_Test", 
    &IndPredModelBaseTest::OneExpF_AdditivePlusThetaDepY_dataVariance_Test ));

  suiteOfTests->addTest(new TestCaller<IndPredModelBaseTest>(
    "OneExpF_AdditivePlusThetaDepY_dataVarianceInv_Test", 
    &IndPredModelBaseTest::OneExpF_AdditivePlusThetaDepY_dataVarianceInv_Test ));

  suiteOfTests->addTest(new TestCaller<IndPredModelBaseTest>(
    "OneExpF_AdditivePlusThetaDepY_mapObj_Test", 
    &IndPredModelBaseTest::OneExpF_AdditivePlusThetaDepY_mapObj_Test ));

  suiteOfTests->addTest(new TestCaller<IndPredModelBaseTest>(
    "RailExample_OptimizerBackup_Test", 
    &IndPredModelBaseTest::RailExample_OptimizerBackup_Test ));

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: OneExpF_AdditivePlusThetaDepY_dataMean_Test
 *
 *
 * The goal of this test is to check that the individual level
 * Pred model works for the case of the 
 *
 *     OneExpF_AdditivePlusThetaDepY_Pred
 *
 * Pred block expression evaluator, for which analytical results 
 * can be calculated and which does not use any data values. 
 *
 *************************************************************************/

void IndPredModelBaseTest::OneExpF_AdditivePlusThetaDepY_dataMean_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpfpred_indpredmodelbasetest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of data values for this individual.
  int nY_iKnown = 5;

  OneExpF_AdditivePlusThetaDepY_Pred< AD< double > > predEvaluatorAD( nY_iKnown );

  OneExpF_AdditivePlusThetaDepY_Pred< AD< AD<double> > > predEvaluatorADAD( nY_iKnown );

  OneExpF_AdditivePlusThetaDepY_Pred< AD< AD< AD<double> > > > predEvaluatorADADAD( nY_iKnown );


  //------------------------------------------------------------
  // Prepare the variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of independent variables.
  const int nTheta = 3;
  const int nEta   = 2;

  // Set the current value for theta.
  valarray<double> thetaCurr( nTheta );
  thetaCurr[0] = 3.0;
  thetaCurr[1] = 0.08;
  thetaCurr[2] = 100.0;

  // Set the limits for theta.
  valarray<double> thetaLow( nTheta );
  valarray<double> thetaUp ( nTheta );
  thetaLow[0] = -20.0;
  thetaUp [0] = 100.0;
  thetaLow[1] = 0.005;
  thetaUp [1] = 0.7;
  thetaLow[2] = 0.001;
  thetaUp [2] = 500.0;


  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrix.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  covStruct omegaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nOmegaPar = nEta;

  // Set the diagonal elements for the current value for omega.
  valarray<double> omegaMinRep( nOmegaPar );
  omegaMinRep[0] = 0.25;
  omegaMinRep[1] = 0.001;


  //------------------------------------------------------------
  // Construct the individual level Pred model.
  //------------------------------------------------------------

  IndPredModelBase< AD<double> > modelAD(
    predEvaluatorAD,
    predEvaluatorADAD,
    predEvaluatorADADAD,
    nTheta,
    thetaLow,
    thetaUp,
    thetaCurr,
    nEta,
    omegaStruct,
    omegaMinRep );


  //------------------------------------------------------------
  // Get information related to the individual.
  //------------------------------------------------------------

  // Get the number elements in the individual parameter.
  int nIndPar = modelAD.getNIndPar();

  // Get the number of observations for this individual.
  int iCurr = 0;
  int nY_i = predEvaluatorAD.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare various quantities for the test.
  //------------------------------------------------------------

  valarray< AD<double> > bCurrAD          ( nIndPar );
  valarray< AD<double> > dataMeanAD       ( nY_i );
  valarray< AD<double> > dataMean_indParAD( nY_i * nIndPar );

  valarray<double> bCurr                 ( nIndPar );
  valarray<double> dataMean              ( nY_i );
  valarray<double> dataMean_indPar       ( nY_i * nIndPar );

  // Get the current value for the individual parameter, which was set
  // when the model was constructed.
  modelAD.getIndPar( bCurrAD );

  // Get a double version of the parameters.
  scalarToDoubleArray( bCurrAD, bCurr );


  //------------------------------------------------------------
  // Calculate the data mean and its derivative.
  //------------------------------------------------------------

  // Start recording by declaring the independent variables.
  //
  // When the model function is evaluated, independent variables that
  // are at higher levels of taping than this function's independent
  // variables will be used, e.g., AD< AD<double> >.
  CppAD::Independent( bCurrAD );

  // Set the model's individual parameter equal to this function's
  // individual parameter after it has been declared the independent
  // variable for this function.
  //
  // This is done so that the model function calculations are
  // performed using this function's independent variable, which will
  // allow for their derivatives to be calculated in this function.
  modelAD.setIndPar( bCurrAD );

  // Evaluate the model function.
  modelAD.dataMean( dataMeanAD );

  // Stop recording at this level by constructing a differentiable
  // function object for the model function.
  CppAD::ADFun<double> dataMeanADFun( bCurrAD, dataMeanAD );

  // Calculate the derivative of the model function.
  //
  // Note that the elements of the derivative returned by the Jacobian
  // function are in row major order.
  dataMean_indPar = dataMeanADFun.Jacobian( bCurr );

  // Put the derivative elements in column major order.
  dataMean_indPar = transpose( dataMean_indPar, nY_i );


  //------------------------------------------------------------
  // Convert the calculated values to the type used for the tests.
  //------------------------------------------------------------

  scalarToDoubleArray( dataMeanAD, dataMean );


  //------------------------------------------------------------
  // Prepare known values related to the individual parameter.
  //------------------------------------------------------------

  valarray<double> omegaCurr( nEta * nEta );

  // Create a covariance matrix that is equal to the one that the
  // IndPredModelBase is maintaining internally.
  DiagCov omega( nEta );
  omega.expandCovMinRep( omegaMinRep, omegaCurr );
  omega.setCov( omegaCurr );

  valarray<double> omegaKnown         ( nEta * nEta );
  valarray<double> omegaParKnown      ( nOmegaPar );
  valarray<double> omega_omegaParKnown( nEta * nEta * nOmegaPar );

  // Get known values for omega and its derivative.
  omega.cov    ( omegaKnown );
  omega.cov_par( omega_omegaParKnown );

  // Get known values for omega's parameter and its limits.
  omega.calcPar     ( omegaCurr,        omegaParKnown );

  // The individual parameter is composed of the parameters that are
  // optimized over when performing individual level estimation,
  //
  //             -          -
  //            |   theta    |
  //     b   =  |            |  .
  //      i     |  omegaPar  |
  //             -          -
  //
  int nIndParKnown = nTheta + nOmegaPar;

  int thetaOffsetInIndPar    = 0;
  int omegaParOffsetInIndPar = nTheta;

  valarray<double> bCurrKnown( nIndPar );

  // Set the known value for the individual parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    bCurrKnown[k + thetaOffsetInIndPar] = thetaCurr[k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    bCurrKnown[k + omegaParOffsetInIndPar] = omegaParKnown[k];
  }


  //------------------------------------------------------------
  // Prepare known values related to the model functions.
  //------------------------------------------------------------

  valarray<double> dataMeanKnown              ( nY_i );
  valarray<double> dataMean_indParKnown       ( nY_i * nIndPar );
  valarray<double> dataVarianceKnown          ( nY_i * nY_i );
  valarray<double> dataVarianceInvKnown       ( nY_i * nY_i );
  valarray<double> dataVariance_indParKnown   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInv_indParKnown( nY_i * nY_i * nIndPar );

  double time;
  double ds;
  double w;
  double ka;
  double ke;
  double cl;
  double d;
  double e;

  // Calculate the known value for the mean of the data:
  //
  //                     ds * exp[ - theta    * time    ]
  //                                      (0)       (j)
  //    f    ( b  )  =  ----------------------------------  .
  //     i(j)   i               [ theta    * w ]
  //                                   (1)
  //
  for ( j = 0; j < nY_i; j++ )
  {
    time = timeStep * j;
    ds = dose * wt;
    w = wt;

    ke = thetaCurr[0];
    cl = thetaCurr[1] * w;
    d = std::exp(-ke * time);
    e = cl;
    dataMeanKnown[j] = ds * d / e;
  }

  int col;

  // Calculate the known value for the derivative of the data mean.
  dataMean_indParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)
    //     d       f    ( b  )  =  - time     f    ( b  )  .
    //      theta   i(j)   i             (j)   i(j)   i
    //
    col = k + thetaOffsetInIndPar;

    dataMean_indParKnown[j + col * nY_i] = - timeStep * j * dataMeanKnown[j];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                              - wt  f    ( b  )
    //      (1)                            i(j)   i
    //     d       f    ( b  )  = --------------------  .
    //      theta   i(j)   i  
    //                              [ theta    * wt ]
    //                                     (1)
    //
    col = k + thetaOffsetInIndPar;

    dataMean_indParKnown[j + col * nY_i] = - wt * dataMeanKnown[j] /
      ( thetaCurr[1] * wt );
  }

  // Calculate the known value for the variance of the data.
  dataVarianceKnown = 0.0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the values for the diagonal elements of the data variance:
    //
    //                                                       2
    //     R      ( b  )  =  omega       +  omega       theta     .
    //      i(j,j)   i            (0,0)          (1,1)       (2)
    //
    dataVarianceKnown[j + j * nY_i] = omegaKnown[0 + 0 * nEta] +
      omegaKnown[1 + 1 * nEta] * thetaCurr[2] * thetaCurr[2];
  }

  // Calculate the known value for the derivative of the variance
  // of the data.
  dataVariance_indParKnown = 0.0;
  int nRow = nY_i * nY_i;
  int row;
  k = 2;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (2)
    //     d       R      ( b  )  =  2  omega       theta     .
    //      theta   i(j,j)   i               (1,1)       (2)
    //
    row = j * nY_i + j;
    col = k + thetaOffsetInIndPar;
  
    dataVariance_indParKnown[row + col * nRow] = 
      2.0 * omegaKnown[1 + 1 * nEta] * thetaCurr[2];
  }
  int nOmega_omegaParRow = nEta * nEta;
  int omega_omegaParRow;
  int omega_omegaParCol;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)                          (0)                
    //     d          R      ( b  )  =  d         omega     ( omegaPar )  .
    //      omegaPar   i(j,j)   i        omegaPar      (0,0)
    //
    row = j * nY_i + j;
    col = k + omegaParOffsetInIndPar;

    omega_omegaParRow = k * nEta + k;
    omega_omegaParCol = k;
    
    dataVariance_indParKnown[row + col * nRow] = 
      omega_omegaParKnown[omega_omegaParRow + omega_omegaParCol * nOmega_omegaParRow];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (1)                              2     (1)                
    //     d          R      ( b  )  =  theta     d         omega     ( omegaPar )  .
    //      omegaPar   i(j,j)   i            (2)   omegaPar      (1,1)
    //
    row = j * nY_i + j;
    col = k + omegaParOffsetInIndPar;

    omega_omegaParRow = k * nEta + k;
    omega_omegaParCol = k;
    
    dataVariance_indParKnown[row + col * nRow] = 
      thetaCurr[2] * thetaCurr[2] *
      omega_omegaParKnown[omega_omegaParRow + omega_omegaParCol * nOmega_omegaParRow];
  }

  // Calculate the known value for the inverse of the variance
  // of the data.
  dataVarianceInvKnown = inverse( dataVarianceKnown, nY_i );

  // Calculate the known value for the derivative of the inverse
  // of the variance of the data using Lemma 10 of B. M. Bell, 
  // "Approximating the marginal likelihood estimate for models
  // with random parameters", Applied Mathematics and Computation, 
  // 119 (2001), pp. 57-73, which states that
  //
  //          -1               -1              -1
  //     d   A  ( x )  =  - [ A  ( x )  kron  A  ( x ) ]  d   A ( x )  .
  //      x                                                x
  //
  dataVarianceInv_indParKnown = AkronBtimesC(
    dataVarianceInvKnown,     nY_i,
    dataVarianceInvKnown,     nY_i,
    dataVariance_indParKnown, nIndPar );
  dataVarianceInv_indParKnown *= -1.0;


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  compareToKnown( 
    dataMean,
    dataMeanKnown,
    "dataMean",
    tol );

  compareToKnown( 
    dataMean_indPar,
    dataMean_indParKnown,
    "dataMean_indPar",
    tol );

}


/*************************************************************************
 *
 * Function: OneExpF_AdditivePlusThetaDepY_dataVariance_Test
 *
 *
 * The goal of this test is to check that the individual level
 * Pred model works for the case of the 
 *
 *     OneExpF_AdditivePlusThetaDepY_Pred
 *
 * Pred block expression evaluator, for which analytical results 
 * can be calculated and which does not use any data values. 
 *
 *************************************************************************/

void IndPredModelBaseTest::OneExpF_AdditivePlusThetaDepY_dataVariance_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpfpred_indpredmodelbasetest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of data values for this individual.
  int nY_iKnown = 5;

  OneExpF_AdditivePlusThetaDepY_Pred< AD< double > > predEvaluatorAD( nY_iKnown );

  OneExpF_AdditivePlusThetaDepY_Pred< AD< AD<double> > > predEvaluatorADAD( nY_iKnown );

  OneExpF_AdditivePlusThetaDepY_Pred< AD< AD< AD<double> > > > predEvaluatorADADAD( nY_iKnown );


  //------------------------------------------------------------
  // Prepare the variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of independent variables.
  const int nTheta = 3;
  const int nEta   = 2;

  // Set the current value for theta.
  valarray<double> thetaCurr( nTheta );
  thetaCurr[0] = 3.0;
  thetaCurr[1] = 0.08;
  thetaCurr[2] = 100.0;

  // Set the limits for theta.
  valarray<double> thetaLow( nTheta );
  valarray<double> thetaUp ( nTheta );
  thetaLow[0] = -20.0;
  thetaUp [0] = 100.0;
  thetaLow[1] = 0.005;
  thetaUp [1] = 0.7;
  thetaLow[2] = 0.001;
  thetaUp [2] = 500.0;


  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrix.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  covStruct omegaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nOmegaPar = nEta;

  // Set the diagonal elements for the current value for omega.
  valarray<double> omegaMinRep( nOmegaPar );
  omegaMinRep[0] = 0.25;
  omegaMinRep[1] = 0.001;


  //------------------------------------------------------------
  // Construct the individual level Pred model.
  //------------------------------------------------------------

  IndPredModelBase< AD<double> > modelAD(
    predEvaluatorAD,
    predEvaluatorADAD,
    predEvaluatorADADAD,
    nTheta,
    thetaLow,
    thetaUp,
    thetaCurr,
    nEta,
    omegaStruct,
    omegaMinRep );


  //------------------------------------------------------------
  // Get information related to the individual.
  //------------------------------------------------------------

  // Get the number elements in the individual parameter.
  int nIndPar = modelAD.getNIndPar();

  // Get the number of observations for this individual.
  int iCurr = 0;
  int nY_i = predEvaluatorAD.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare various quantities for the test.
  //------------------------------------------------------------

  valarray< AD<double> > bCurrAD              ( nIndPar );
  valarray< AD<double> > dataMeanAD           ( nY_i );
  valarray< AD<double> > dataVarianceAD       ( nY_i * nY_i );
  valarray< AD<double> > dataVariance_indParAD( nY_i * nY_i * nIndPar );
  valarray< AD<double> > dataVarianceInvAD    ( nY_i * nY_i );

  valarray<double> bCurr                 ( nIndPar );
  valarray<double> dataMean_indPar       ( nY_i * nIndPar );
  valarray<double> dataVariance_indPar   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInv_indPar( nY_i * nY_i * nIndPar );

  // Get the current value for the individual parameter, which was set
  // when the model was constructed.
  modelAD.getIndPar( bCurrAD );

  // Get a double version of the parameters.
  scalarToDoubleArray( bCurrAD, bCurr );


  //------------------------------------------------------------
  // Calculate the data variance and its derivative.
  //------------------------------------------------------------

  // Start recording by declaring the independent variables.
  //
  // When the model function is evaluated, independent variables that
  // are at higher levels of taping than this function's independent
  // variables will be used, e.g., AD< AD<double> >.
  CppAD::Independent( bCurrAD );

  // Set the model's individual parameter equal to this function's
  // individual parameter after it has been declared the independent
  // variable for this function.
  //
  // This is done so that the model function calculations are
  // performed using this function's independent variable, which will
  // allow for their derivatives to be calculated in this function.
  modelAD.setIndPar( bCurrAD );

  // Evaluate the model function.
  modelAD.dataVariance( dataVarianceAD );

  // Stop recording at this level by constructing a differentiable
  // function object for the model function.
  CppAD::ADFun<double> dataVarianceADFun( bCurrAD, dataVarianceAD );

  // Calculate the derivative of the model function.  
  //
  // Note that the elements of the derivative returned by the Jacobian
  // function are in row major order.
  dataVariance_indPar = dataVarianceADFun.Jacobian( bCurr );

  // Put the derivative elements in column major order.
  dataVariance_indPar = transpose( dataVariance_indPar, nY_i * nY_i );


  //------------------------------------------------------------
  // Convert the calculated values to the type used for the tests.
  //------------------------------------------------------------

  valarray<double> dataMean              ( nY_i );
  valarray<double> dataVariance          ( nY_i * nY_i );
  valarray<double> dataVarianceInv       ( nY_i * nY_i );

  scalarToDoubleArray( dataMeanAD,        dataMean        );
  scalarToDoubleArray( dataVarianceAD,    dataVariance    );
  scalarToDoubleArray( dataVarianceInvAD, dataVarianceInv );


  //------------------------------------------------------------
  // Prepare known values related to the individual parameter.
  //------------------------------------------------------------

  valarray<double> omegaCurr( nEta * nEta );

  // Create a covariance matrix that is equal to the one that the
  // IndPredModelBase is maintaining internally.
  DiagCov omega( nEta );
  omega.expandCovMinRep( omegaMinRep, omegaCurr );
  omega.setCov( omegaCurr );

  valarray<double> omegaKnown         ( nEta * nEta );
  valarray<double> omegaParKnown      ( nOmegaPar );
  valarray<double> omega_omegaParKnown( nEta * nEta * nOmegaPar );

  // Get known values for omega and its derivative.
  omega.cov    ( omegaKnown );
  omega.cov_par( omega_omegaParKnown );

  // Get known values for omega's parameter and its limits.
  omega.calcPar     ( omegaCurr,        omegaParKnown );

  // The individual parameter is composed of the parameters that are
  // optimized over when performing individual level estimation,
  //
  //             -          -
  //            |   theta    |
  //     b   =  |            |  .
  //      i     |  omegaPar  |
  //             -          -
  //
  int nIndParKnown = nTheta + nOmegaPar;

  int thetaOffsetInIndPar    = 0;
  int omegaParOffsetInIndPar = nTheta;

  valarray<double> bCurrKnown( nIndPar );

  // Set the known value for the individual parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    bCurrKnown[k + thetaOffsetInIndPar] = thetaCurr[k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    bCurrKnown[k + omegaParOffsetInIndPar] = omegaParKnown[k];
  }


  //------------------------------------------------------------
  // Prepare known values related to the model functions.
  //------------------------------------------------------------

  valarray<double> dataMeanKnown              ( nY_i );
  valarray<double> dataMean_indParKnown       ( nY_i * nIndPar );
  valarray<double> dataVarianceKnown          ( nY_i * nY_i );
  valarray<double> dataVarianceInvKnown       ( nY_i * nY_i );
  valarray<double> dataVariance_indParKnown   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInv_indParKnown( nY_i * nY_i * nIndPar );

  double time;
  double ds;
  double w;
  double ka;
  double ke;
  double cl;
  double d;
  double e;

  // Calculate the known value for the mean of the data:
  //
  //                     ds * exp[ - theta    * time    ]
  //                                      (0)       (j)
  //    f    ( b  )  =  ----------------------------------  .
  //     i(j)   i               [ theta    * w ]
  //                                   (1)
  //
  for ( j = 0; j < nY_i; j++ )
  {
    time = timeStep * j;
    ds = dose * wt;
    w = wt;

    ke = thetaCurr[0];
    cl = thetaCurr[1] * w;
    d = std::exp(-ke * time);
    e = cl;
    dataMeanKnown[j] = ds * d / e;
  }

  int col;

  // Calculate the known value for the derivative of the data mean.
  dataMean_indParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)
    //     d       f    ( b  )  =  - time     f    ( b  )  .
    //      theta   i(j)   i             (j)   i(j)   i
    //
    col = k + thetaOffsetInIndPar;

    dataMean_indParKnown[j + col * nY_i] = - timeStep * j * dataMeanKnown[j];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                              - wt  f    ( b  )
    //      (1)                            i(j)   i
    //     d       f    ( b  )  = --------------------  .
    //      theta   i(j)   i  
    //                              [ theta    * wt ]
    //                                     (1)
    //
    col = k + thetaOffsetInIndPar;

    dataMean_indParKnown[j + col * nY_i] = - wt * dataMeanKnown[j] /
      ( thetaCurr[1] * wt );
  }

  // Calculate the known value for the variance of the data.
  dataVarianceKnown = 0.0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the values for the diagonal elements of the data variance:
    //
    //                                                       2
    //     R      ( b  )  =  omega       +  omega       theta     .
    //      i(j,j)   i            (0,0)          (1,1)       (2)
    //
    dataVarianceKnown[j + j * nY_i] = omegaKnown[0 + 0 * nEta] +
      omegaKnown[1 + 1 * nEta] * thetaCurr[2] * thetaCurr[2];
  }

  // Calculate the known value for the derivative of the variance
  // of the data.
  dataVariance_indParKnown = 0.0;
  int nRow = nY_i * nY_i;
  int row;
  k = 2;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (2)
    //     d       R      ( b  )  =  2  omega       theta     .
    //      theta   i(j,j)   i               (1,1)       (2)
    //
    row = j * nY_i + j;
    col = k + thetaOffsetInIndPar;
  
    dataVariance_indParKnown[row + col * nRow] = 
      2.0 * omegaKnown[1 + 1 * nEta] * thetaCurr[2];
  }
  int nOmega_omegaParRow = nEta * nEta;
  int omega_omegaParRow;
  int omega_omegaParCol;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)                          (0)                
    //     d          R      ( b  )  =  d         omega     ( omegaPar )  .
    //      omegaPar   i(j,j)   i        omegaPar      (0,0)
    //
    row = j * nY_i + j;
    col = k + omegaParOffsetInIndPar;

    omega_omegaParRow = k * nEta + k;
    omega_omegaParCol = k;
    
    dataVariance_indParKnown[row + col * nRow] = 
      omega_omegaParKnown[omega_omegaParRow + omega_omegaParCol * nOmega_omegaParRow];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (1)                              2     (1)                
    //     d          R      ( b  )  =  theta     d         omega     ( omegaPar )  .
    //      omegaPar   i(j,j)   i            (2)   omegaPar      (1,1)
    //
    row = j * nY_i + j;
    col = k + omegaParOffsetInIndPar;

    omega_omegaParRow = k * nEta + k;
    omega_omegaParCol = k;
    
    dataVariance_indParKnown[row + col * nRow] = 
      thetaCurr[2] * thetaCurr[2] *
      omega_omegaParKnown[omega_omegaParRow + omega_omegaParCol * nOmega_omegaParRow];
  }

  // Calculate the known value for the inverse of the variance
  // of the data.
  dataVarianceInvKnown = inverse( dataVarianceKnown, nY_i );

  // Calculate the known value for the derivative of the inverse
  // of the variance of the data using Lemma 10 of B. M. Bell, 
  // "Approximating the marginal likelihood estimate for models
  // with random parameters", Applied Mathematics and Computation, 
  // 119 (2001), pp. 57-73, which states that
  //
  //          -1               -1              -1
  //     d   A  ( x )  =  - [ A  ( x )  kron  A  ( x ) ]  d   A ( x )  .
  //      x                                                x
  //
  dataVarianceInv_indParKnown = AkronBtimesC(
    dataVarianceInvKnown,     nY_i,
    dataVarianceInvKnown,     nY_i,
    dataVariance_indParKnown, nIndPar );
  dataVarianceInv_indParKnown *= -1.0;


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  compareToKnown( 
    dataVariance,
    dataVarianceKnown,
    "dataVariance",
    tol );

  compareToKnown( 
    dataVariance_indPar,
    dataVariance_indParKnown,
    "dataVariance_indPar",
    tol );

}


/*************************************************************************
 *
 * Function: OneExpF_AdditivePlusThetaDepY_dataVarianceInv_Test
 *
 *
 * The goal of this test is to check that the individual level
 * Pred model works for the case of the 
 *
 *     OneExpF_AdditivePlusThetaDepY_Pred
 *
 * Pred block expression evaluator, for which analytical results 
 * can be calculated and which does not use any data values. 
 *
 *************************************************************************/

void IndPredModelBaseTest::OneExpF_AdditivePlusThetaDepY_dataVarianceInv_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpfpred_indpredmodelbasetest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of data values for this individual.
  int nY_iKnown = 5;

  OneExpF_AdditivePlusThetaDepY_Pred< AD< double > > predEvaluatorAD( nY_iKnown );

  OneExpF_AdditivePlusThetaDepY_Pred< AD< AD<double> > > predEvaluatorADAD( nY_iKnown );

  OneExpF_AdditivePlusThetaDepY_Pred< AD< AD< AD<double> > > > predEvaluatorADADAD( nY_iKnown );


  //------------------------------------------------------------
  // Prepare the variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of independent variables.
  const int nTheta = 3;
  const int nEta   = 2;

  // Set the current value for theta.
  valarray<double> thetaCurr( nTheta );
  thetaCurr[0] = 3.0;
  thetaCurr[1] = 0.08;
  thetaCurr[2] = 100.0;

  // Set the limits for theta.
  valarray<double> thetaLow( nTheta );
  valarray<double> thetaUp ( nTheta );
  thetaLow[0] = -20.0;
  thetaUp [0] = 100.0;
  thetaLow[1] = 0.005;
  thetaUp [1] = 0.7;
  thetaLow[2] = 0.001;
  thetaUp [2] = 500.0;


  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrix.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  covStruct omegaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nOmegaPar = nEta;

  // Set the diagonal elements for the current value for omega.
  valarray<double> omegaMinRep( nOmegaPar );
  omegaMinRep[0] = 0.25;
  omegaMinRep[1] = 0.001;


  //------------------------------------------------------------
  // Construct the individual level Pred model.
  //------------------------------------------------------------

  IndPredModelBase< AD<double> > modelAD(
    predEvaluatorAD,
    predEvaluatorADAD,
    predEvaluatorADADAD,
    nTheta,
    thetaLow,
    thetaUp,
    thetaCurr,
    nEta,
    omegaStruct,
    omegaMinRep );


  //------------------------------------------------------------
  // Get information related to the individual.
  //------------------------------------------------------------

  // Get the number elements in the individual parameter.
  int nIndPar = modelAD.getNIndPar();

  // Get the number of observations for this individual.
  int iCurr = 0;
  int nY_i = predEvaluatorAD.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare various quantities for the test.
  //------------------------------------------------------------

  valarray< AD<double> > bCurrAD                 ( nIndPar );
  valarray< AD<double> > dataMeanAD              ( nY_i );
  valarray< AD<double> > dataVarianceAD          ( nY_i * nY_i );
  valarray< AD<double> > dataVarianceInvAD       ( nY_i * nY_i );
  valarray< AD<double> > dataVarianceInv_indParAD( nY_i * nY_i * nIndPar );

  valarray<double> bCurr                 ( nIndPar );
  valarray<double> dataMean_indPar       ( nY_i * nIndPar );
  valarray<double> dataVariance_indPar   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInv_indPar( nY_i * nY_i * nIndPar );

  // Get the current value for the individual parameter, which was set
  // when the model was constructed.
  modelAD.getIndPar( bCurrAD );

  // Get a double version of the parameters.
  scalarToDoubleArray( bCurrAD, bCurr );


  //------------------------------------------------------------
  // Calculate the data variance inverse and its derivative.
  //------------------------------------------------------------

  // Start recording by declaring the independent variables.
  //
  // When the model function is evaluated, independent variables that
  // are at higher levels of taping than this function's independent
  // variables will be used, e.g., AD< AD<double> >.
  CppAD::Independent( bCurrAD );

  // Set the model's individual parameter equal to this function's
  // individual parameter after it has been declared the independent
  // variable for this function.
  //
  // This is done so that the model function calculations are
  // performed using this function's independent variable, which will
  // allow for their derivatives to be calculated in this function.
  modelAD.setIndPar( bCurrAD );

  // Evaluate the model function.
  modelAD.dataVarianceInv( dataVarianceInvAD );

  // Stop recording at this level by constructing a differentiable
  // function object for the model function.
  CppAD::ADFun<double> dataVarianceInvADFun( bCurrAD, dataVarianceInvAD );

  // Calculate the derivative of the model function.
  //
  // Note that the elements of the derivative returned by the Jacobian
  // function are in row major order.
  dataVarianceInv_indPar = dataVarianceInvADFun.Jacobian( bCurr );

  // Put the derivative elements in column major order.
  dataVarianceInv_indPar = transpose( dataVarianceInv_indPar, nY_i * nY_i );


  //------------------------------------------------------------
  // Convert the calculated values to the type used for the tests.
  //------------------------------------------------------------

  valarray<double> dataMean              ( nY_i );
  valarray<double> dataVariance          ( nY_i * nY_i );
  valarray<double> dataVarianceInv       ( nY_i * nY_i );

  scalarToDoubleArray( dataMeanAD,        dataMean        );
  scalarToDoubleArray( dataVarianceAD,    dataVariance    );
  scalarToDoubleArray( dataVarianceInvAD, dataVarianceInv );


  //------------------------------------------------------------
  // Prepare known values related to the individual parameter.
  //------------------------------------------------------------

  valarray<double> omegaCurr( nEta * nEta );

  // Create a covariance matrix that is equal to the one that the
  // IndPredModelBase is maintaining internally.
  DiagCov omega( nEta );
  omega.expandCovMinRep( omegaMinRep, omegaCurr );
  omega.setCov( omegaCurr );

  valarray<double> omegaKnown         ( nEta * nEta );
  valarray<double> omegaParKnown      ( nOmegaPar );
  valarray<double> omega_omegaParKnown( nEta * nEta * nOmegaPar );

  // Get known values for omega and its derivative.
  omega.cov    ( omegaKnown );
  omega.cov_par( omega_omegaParKnown );

  // Get known values for omega's parameter and its limits.
  omega.calcPar     ( omegaCurr,        omegaParKnown );

  // The individual parameter is composed of the parameters that are
  // optimized over when performing individual level estimation,
  //
  //             -          -
  //            |   theta    |
  //     b   =  |            |  .
  //      i     |  omegaPar  |
  //             -          -
  //
  int nIndParKnown = nTheta + nOmegaPar;

  int thetaOffsetInIndPar    = 0;
  int omegaParOffsetInIndPar = nTheta;

  valarray<double> bCurrKnown( nIndPar );

  // Set the known value for the individual parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    bCurrKnown[k + thetaOffsetInIndPar] = thetaCurr[k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    bCurrKnown[k + omegaParOffsetInIndPar] = omegaParKnown[k];
  }


  //------------------------------------------------------------
  // Prepare known values related to the model functions.
  //------------------------------------------------------------

  valarray<double> dataMeanKnown              ( nY_i );
  valarray<double> dataMean_indParKnown       ( nY_i * nIndPar );
  valarray<double> dataVarianceKnown          ( nY_i * nY_i );
  valarray<double> dataVarianceInvKnown       ( nY_i * nY_i );
  valarray<double> dataVariance_indParKnown   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInv_indParKnown( nY_i * nY_i * nIndPar );

  double time;
  double ds;
  double w;
  double ka;
  double ke;
  double cl;
  double d;
  double e;

  // Calculate the known value for the mean of the data:
  //
  //                     ds * exp[ - theta    * time    ]
  //                                      (0)       (j)
  //    f    ( b  )  =  ----------------------------------  .
  //     i(j)   i               [ theta    * w ]
  //                                   (1)
  //
  for ( j = 0; j < nY_i; j++ )
  {
    time = timeStep * j;
    ds = dose * wt;
    w = wt;

    ke = thetaCurr[0];
    cl = thetaCurr[1] * w;
    d = std::exp(-ke * time);
    e = cl;
    dataMeanKnown[j] = ds * d / e;
  }

  int col;

  // Calculate the known value for the derivative of the data mean.
  dataMean_indParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)
    //     d       f    ( b  )  =  - time     f    ( b  )  .
    //      theta   i(j)   i             (j)   i(j)   i
    //
    col = k + thetaOffsetInIndPar;

    dataMean_indParKnown[j + col * nY_i] = - timeStep * j * dataMeanKnown[j];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                              - wt  f    ( b  )
    //      (1)                            i(j)   i
    //     d       f    ( b  )  = --------------------  .
    //      theta   i(j)   i  
    //                              [ theta    * wt ]
    //                                     (1)
    //
    col = k + thetaOffsetInIndPar;

    dataMean_indParKnown[j + col * nY_i] = - wt * dataMeanKnown[j] /
      ( thetaCurr[1] * wt );
  }

  // Calculate the known value for the variance of the data.
  dataVarianceKnown = 0.0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the values for the diagonal elements of the data variance:
    //
    //                                                       2
    //     R      ( b  )  =  omega       +  omega       theta     .
    //      i(j,j)   i            (0,0)          (1,1)       (2)
    //
    dataVarianceKnown[j + j * nY_i] = omegaKnown[0 + 0 * nEta] +
      omegaKnown[1 + 1 * nEta] * thetaCurr[2] * thetaCurr[2];
  }

  // Calculate the known value for the derivative of the variance
  // of the data.
  dataVariance_indParKnown = 0.0;
  int nRow = nY_i * nY_i;
  int row;
  k = 2;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (2)
    //     d       R      ( b  )  =  2  omega       theta     .
    //      theta   i(j,j)   i               (1,1)       (2)
    //
    row = j * nY_i + j;
    col = k + thetaOffsetInIndPar;
  
    dataVariance_indParKnown[row + col * nRow] = 
      2.0 * omegaKnown[1 + 1 * nEta] * thetaCurr[2];
  }
  int nOmega_omegaParRow = nEta * nEta;
  int omega_omegaParRow;
  int omega_omegaParCol;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)                          (0)                
    //     d          R      ( b  )  =  d         omega     ( omegaPar )  .
    //      omegaPar   i(j,j)   i        omegaPar      (0,0)
    //
    row = j * nY_i + j;
    col = k + omegaParOffsetInIndPar;

    omega_omegaParRow = k * nEta + k;
    omega_omegaParCol = k;
    
    dataVariance_indParKnown[row + col * nRow] = 
      omega_omegaParKnown[omega_omegaParRow + omega_omegaParCol * nOmega_omegaParRow];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (1)                              2     (1)                
    //     d          R      ( b  )  =  theta     d         omega     ( omegaPar )  .
    //      omegaPar   i(j,j)   i            (2)   omegaPar      (1,1)
    //
    row = j * nY_i + j;
    col = k + omegaParOffsetInIndPar;

    omega_omegaParRow = k * nEta + k;
    omega_omegaParCol = k;
    
    dataVariance_indParKnown[row + col * nRow] = 
      thetaCurr[2] * thetaCurr[2] *
      omega_omegaParKnown[omega_omegaParRow + omega_omegaParCol * nOmega_omegaParRow];
  }

  // Calculate the known value for the inverse of the variance
  // of the data.
  dataVarianceInvKnown = inverse( dataVarianceKnown, nY_i );

  // Calculate the known value for the derivative of the inverse
  // of the variance of the data using Lemma 10 of B. M. Bell, 
  // "Approximating the marginal likelihood estimate for models
  // with random parameters", Applied Mathematics and Computation, 
  // 119 (2001), pp. 57-73, which states that
  //
  //          -1               -1              -1
  //     d   A  ( x )  =  - [ A  ( x )  kron  A  ( x ) ]  d   A ( x )  .
  //      x                                                x
  //
  dataVarianceInv_indParKnown = AkronBtimesC(
    dataVarianceInvKnown,     nY_i,
    dataVarianceInvKnown,     nY_i,
    dataVariance_indParKnown, nIndPar );
  dataVarianceInv_indParKnown *= -1.0;


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  compareToKnown( 
    dataVarianceInv,
    dataVarianceInvKnown,
    "dataVarianceInv",
    tol );

  compareToKnown( 
    dataVarianceInv_indPar,
    dataVarianceInv_indParKnown,
    "dataVarianceInv_indPar",
    tol );

}


/*************************************************************************
 *
 * Function: OneExpF_AdditivePlusThetaDepY_mapObj_Test
 *
 *
 * The goal of this test is to check that the individual level
 * Pred model works for the case of the 
 *
 *     OneExpF_AdditivePlusThetaDepY_Pred
 *
 * Pred block expression evaluator, for which analytical results 
 * can be calculated and which does not use any data values. 
 *
 *************************************************************************/

void IndPredModelBaseTest::OneExpF_AdditivePlusThetaDepY_mapObj_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpfpred_indpredmodelbasetest;

  int j;
  int k;

  clock_t clockStart;
  clock_t clockStop;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluators.
  //------------------------------------------------------------

  // Set the number of data values for this individual.
  int nY_iKnown = 5;

  OneExpF_AdditivePlusThetaDepY_Pred< double > predEvaluator( nY_iKnown );

  OneExpF_AdditivePlusThetaDepY_Pred< AD<double> > predEvaluatorAD( nY_iKnown );

  OneExpF_AdditivePlusThetaDepY_Pred< AD< AD<double> > > predEvaluatorADAD( nY_iKnown );

  OneExpF_AdditivePlusThetaDepY_Pred< AD< AD< AD<double> > > > predEvaluatorADADAD( nY_iKnown );


  //------------------------------------------------------------
  // Prepare the variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of independent variables.
  const int nTheta = 3;
  const int nEta   = 2;

  // Set the current value for theta.
  valarray<double> thetaCurr( nTheta );
  thetaCurr[0] = 3.0;
  thetaCurr[1] = 0.08;
  thetaCurr[2] = 100.0;

  // Set the limits for theta.
  valarray<double> thetaLow( nTheta );
  valarray<double> thetaUp ( nTheta );
  thetaLow[0] = -20.0;
  thetaUp [0] = 100.0;
  thetaLow[1] = 0.005;
  thetaUp [1] = 0.7;
  thetaLow[2] = 0.001;
  thetaUp [2] = 500.0;


  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrix.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  covStruct omegaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nOmegaPar = nEta;

  // Set the diagonal elements for the current value for omega.
  valarray<double> omegaMinRep( nOmegaPar );
  omegaMinRep[0] = 0.25;
  omegaMinRep[1] = 0.001;


  //------------------------------------------------------------
  // Construct the individual level Pred models.
  //------------------------------------------------------------

  // This model will use the CppAD tape of the calculation of the MAP
  // Bayesian objective function in order to calculate the derivative
  // of the objective.
  IndPredModelBase< AD<double> > modelAD(
    predEvaluatorAD,
    predEvaluatorADAD,
    predEvaluatorADADAD,
    nTheta,
    thetaLow,
    thetaUp,
    thetaCurr,
    nEta,
    omegaStruct,
    omegaMinRep );

  // This model uses the existing version of the MAP Bayesian
  // objective function in order to calculate the derivative of the
  // objective.  This involves lots of linear algebra calculations
  // using the derivatives of the model functions f, R, and D.
  IndPredModel modelKnown(
    predEvaluator,
    predEvaluatorAD,
    predEvaluatorADAD,
    nTheta,
    thetaLow,
    thetaUp,
    thetaCurr,
    nEta,
    omegaStruct,
    omegaMinRep );


  //------------------------------------------------------------
  // Get information related to the individual.
  //------------------------------------------------------------

  // Get the number elements in the individual parameter.
  int nIndPar = modelAD.getNIndPar();

  // Get the number of observations for this individual.
  int iCurr = 0;
  int nY_i = predEvaluatorADAD.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare various quantities for the test.
  //------------------------------------------------------------

  // Note that the objective function is a scalar, i.e., its vector
  // only contains one element.
  valarray< AD<double> > bCurrAD          ( nIndPar );
  valarray< AD<double> > mapObjAD         ( 1 );
  valarray< AD<double> > mapObj_indParAD  ( 1 * nIndPar );
  valarray< AD<double> > dataMeanAD       ( nY_i );
  valarray< AD<double> > dataVarianceInvAD( nY_i * nY_i );

  valarray<double> bCurr        ( nIndPar );
  valarray<double> mapObj_indPar( nIndPar );

  // Get the current value for the individual parameter, which was set
  // when the model was constructed.
  modelAD.getIndPar( bCurrAD );

  // Get a double version of the parameters.
  scalarToDoubleArray( bCurrAD, bCurr );


  //------------------------------------------------------------
  // Simulate a set of data for the test.
  //------------------------------------------------------------

  valarray<double> y_i( nY_i );

  // Set the seed for the simulation.
  int seed = 0;

  // Simulate data for this individual.
  simulate( modelKnown,
            nY_i,
            bCurr,
            y_i,
            seed );

  // Get a AD<double> version of the data.
  valarray< AD<double> > y_iAD( nY_i );
  doubleToScalarArray( y_i, y_iAD );


  //------------------------------------------------------------
  // Calculate the objective function and its derivative using AD.
  //------------------------------------------------------------

  // Set this so the terms involving D do not appear in the objective
  // functions.
  bool withD = false;

  int dummy;

  // To get enough clock ticks in order to see a difference, increase
  // this to 10000.
  int dummyMax = 1;

  // Get the number of clock ticks elapsed since this process started.
  clockStart = clock();

  for ( dummy = 0; dummy < dummyMax; dummy++ )
  {

    // Start recording by declaring the independent variables.
    //
    // When the model functions are evaluated, independent variables
    // that are at higher levels of taping than this function's
    // independent variables will be used, e.g., AD< AD<double> >.
    CppAD::Independent( bCurrAD );
    
    // Set the model's individual parameter equal to this function's
    // individual parameter after it has been declared the independent
    // variable for this function.
    //
    // This is done so that the model function calculations are
    // performed using this function's independent variable, which will
    // allow for their derivatives to be calculated in this function.
    modelAD.setIndPar( bCurrAD );
    
    // Set this so that the mean value for b will be assumed to be zero.
    SPK_VA::valarray< AD<double> >* pBMeanAD = 0;
    
    // Evaluate the MAP Bayesian objective function using the templated
    // version of mapObj() defined in this test.
    mapObjTemplate(
      modelAD,
      y_iAD,
      bCurrAD,
      &mapObjAD[0],
      withD,
      pBMeanAD );
    
    // Stop recording at this level by constructing a differentiable
    // function object for the objective function.
    CppAD::ADFun<double> mapObjADFun( bCurrAD, mapObjAD );
    
    // Calculate the derivative of the objective function.
    //
    // Note that the elements of the derivative returned by the Jacobian
    // function are in row major order.  It is not necessary to put them
    // in column major order, however, because the objective function is
    // a scalar.
    mapObj_indPar = mapObjADFun.Jacobian( bCurr );
    
    // Modify the first element of theta slightly in order to invalidate
    // the cache and force recalculation of the model functions.
    valarray< AD<double> > bTempAD( bCurrAD );
    bTempAD[0] = 1.00001 * bTempAD[0];
    modelAD.setIndPar( bTempAD );
  }

  // Print the total number of clock ticks that have elapsed.
  clockStop = clock();
  //cout << "Total clock ticks = " << clockStop - clockStart << endl;


  //------------------------------------------------------------
  // Convert the calculated values to the type used for the tests.
  //------------------------------------------------------------

  // Note that the objective function is a scalar, i.e., its vector
  // only contains one element.
  //
  // Also note that the variable name mapObj could not be used for
  // this value because there is already a function with that name.
  valarray<double> mapObjCalc( 1 );

  scalarToDoubleArray( mapObjAD, mapObjCalc );


  //------------------------------------------------------------
  // Calculate the known objective function and its derivative.
  //------------------------------------------------------------

  bool isFO = false;

  DoubleMatrix* pdvecN = 0;
  DoubleMatrix* pdvecBMean = 0;

  DoubleMatrix dvecY_i( y_i );
  DoubleMatrix dvecBCurr( bCurr );

  double mapObjOut;

  DoubleMatrix drowMapObj_indParOut( 1, nIndPar );

  // Get the number of clock ticks elapsed since this process started.
  clockStart = clock();

  for ( dummy = 0; dummy < dummyMax; dummy++ )
  {
    // Set the model's individual parameter.
    modelKnown.setIndPar( bCurr );
  
    // Evaluate the MAP Bayesian objective function and its derivative
    // using the existing version of mapObj().
    mapObj(
      modelKnown,
      dvecY_i,
      dvecBCurr,
      &mapObjOut,
      &drowMapObj_indParOut,
      withD,
      isFO,
      pdvecN,
      pdvecBMean );
  
    // Modify the first element of theta slightly in order to invalidate
    // the cache and force recalculation of the model functions.
    valarray<double> bTemp( bCurr );
    bTemp[0] = 1.00001 * bTemp[0];
    modelKnown.setIndPar( bTemp );
  }

  // Print the total number of clock ticks that have elapsed.
  clockStop = clock();
  //cout << "Total clock ticks = " << clockStop - clockStart << endl;


  //------------------------------------------------------------
  // Convert the calculated values to the type used for the tests.
  //------------------------------------------------------------

  // Note that the objective function is a scalar, i.e., its vector
  // only contains one element.
  valarray<double> mapObjKnown( 1 );

  mapObjKnown[0] = mapObjOut;

  valarray<double> mapObj_indParKnown( nIndPar );
  mapObj_indParKnown = drowMapObj_indParOut.toValarray();


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-12;

  compareToKnown( 
    mapObjCalc,
    mapObjKnown,
    "mapObj",
    tol );

  compareToKnown( 
    mapObj_indPar,
    mapObj_indParKnown,
    "mapObj_indPar",
    tol );

}


/*************************************************************************
 *
 * Function: RailExample_OptimizerBackup_Test
 *
 *
 * This test causes the optimizer to back up by setting the values for F
 * equal to NaN or infinity the tenth time that eval() is called.
 *
 * This test uses a Pred block expression evaluator that corresponds
 * to the Rail Example that is included in the NLME distribution.
 *
 * The PRED block for the Rail Example after it has been converted
 * to individual notation is
 *
 *     $PRED 
 *     F = THETA(1)
 *     Y = F + ETA(1)
 *
 *************************************************************************/

void IndPredModelBaseTest::RailExample_OptimizerBackup_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace railexample_indpredmodelbasetest;

  int i;
  int j;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluators.
  //------------------------------------------------------------

  RailExample_Pred< double > predEvaluator( nY_i );

  RailExample_Pred< AD<double> > predEvaluatorAD( nY_i );

  RailExample_Pred< AD< AD<double> > > predEvaluatorADAD( nY_i );


  //------------------------------------------------------------
  // Prepare the model variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of model independent variables.
  const int nTheta = 1;
  const int nEta   = nTheta;
  const int nEps   = 0;

  // Set the initial value for theta.
  valarray<double> thetaIn( nTheta );
  thetaIn[0] = 72.0;

  // Set the limits for theta.
  valarray<double> thetaLow( nTheta );
  valarray<double> thetaUp ( nTheta );
  thetaLow[0] = 7.2;
  thetaUp[0]  = 720.0;

  // Set the initial value for eta equal to all zeros.
  valarray<double> etaIn( 0.0, nEta );


  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrices.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  covStruct omegaStruct = FULL;

  // Construct omega.
  FullCov omega( nEta );

  // Set the number elements for this parameterization.
  int nOmegaPar = omega.getNPar();

  // Set the initial lower triangle elements for the initial value for
  // omega equal to those for an identity matrix.
  valarray<double> omegaMinRep( nOmegaPar );
  omegaMinRep[0] = 1.0;
  assert( nOmegaPar == 1 );


  //------------------------------------------------------------
  // Construct the individual level Pred model.
  //------------------------------------------------------------

  // Construct the individual model.
  IndPredModel indModel(
    predEvaluator,
    predEvaluatorAD,
    predEvaluatorADAD,
    nTheta,
    thetaLow,
    thetaUp,
    thetaIn,
    nEta,
    omegaStruct,
    omegaMinRep );


  //------------------------------------------------------------
  // Quantities related to the individual parameters, b.
  //------------------------------------------------------------

  // Get the number of individual parameters.
  const int nB = indModel.getNIndPar();
  assert( nB == nTheta + nOmegaPar );

  valarray<double> bLow ( nB );
  valarray<double> bUp  ( nB );
  valarray<double> bStep( nB );
  valarray<double> bIn  ( nB );
  valarray<double> bOut ( nB );

  // Get the current value for the individual parameters.
  indModel.getIndPar( bIn );

  // Get the limits for the individual parameters.
  indModel.getIndParLimits( bLow, bUp );

  // Get the step sizes for the individual parameters.
  indModel.getIndParStep( bStep );


  //------------------------------------------------------------
  // Remaining inputs to fitIndividual.
  //------------------------------------------------------------

  // Set the values for optimization of the individual objective
  // functions.
  double indEpsilon = 1.e-4; 
  int indNMaxIter   = 50; 
  int indLevel      = 0;

  Optimizer indOptInfo( indEpsilon, indNMaxIter, indLevel ); 


  //------------------------------------------------------------
  // Perform the individual estimation.
  //------------------------------------------------------------

  // Set the flag that indicates that the Map Bayesian terms should
  // not be included in the individual objective function MapObj(b).
  bool withD =false;

  double* pdNull = 0;
  valarray<double>* pVANull = 0;

  // Get the first individual's data values.
  i = 0;
  Y_i = Y[ slice( i * nY_i, nY_i, 1 ) ];

  // Clear all of the warnings so that only the ones from this test
  // will appear in the list of warnings.
  WarningsManager::clearAllWarnings();

  try
  {
    // Calculate this individual's optimal parameter value.
    fitIndividual(
      indModel,
      Y_i,
      indOptInfo,
      bLow,
      bUp,
      bIn,
      bStep,
      &bOut,
      pdNull,
      pVANull,
      pVANull,
      withD );
  }
  catch( const SpkException& e )
  {
    // Uncomment this line to see the list of exceptions.
    // cout << "e = " << e << endl;

    string warnings;
    WarningsManager::getAllWarnings( warnings );

    // Uncomment these statements to see the warnings.
    /*
    cout << "########################################" << endl;
    cout << warnings;
    cout << "########################################" << endl;
    */

    // See if the individual optimizer backed up warning message was issued.
    string::size_type msgPos = warnings.find( "Backed up individual optimization", 0 );
    if( msgPos == string::npos )
    {
      CPPUNIT_ASSERT_MESSAGE( 
        "The individual level optimizer did not back up.",
        false );
    }
  }
  catch( ... )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "An unexpected exception occurred during the evaluation of the data mean.",
      false );
  }

}

