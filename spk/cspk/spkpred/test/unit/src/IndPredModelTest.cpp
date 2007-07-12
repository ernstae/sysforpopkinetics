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
 * File: IndPredModelTest.cpp
 *
 *
 * Unit test for the class IndPredModel.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK Pred test suite header files.
#include "IndPredModelTest.h"
#include "compareToKnown.h"

// SPK Pred library header files.
#include "../../../spkpred/DiagCov.h"
#include "../../../spkpred/IndPredModel.h"

// SPK library header files.
#include <spk/AkronBtimesC.h>
#include <spk/identity.h>
#include <spk/inverse.h>
#include <spk/multiply.h>
#include <spk/replaceSubblock.h>
#include <spk/SpkValarray.h>

// CppAD header files.
#include <CppAD/CppAD.h>

// CppUnit framework header files.
#include <cppunit/TestSuite.h>
#include <cppunit/TestCaller.h>

// Standard library header files.
#include <iomanip>
#include <cassert>
#include <cmath>
#include <iostream>
#include <vector>

using namespace CppAD;
using namespace CppUnit;
using std::vector;
using SPK_VA::valarray;


/*------------------------------------------------------------------------
 * Local variable declarations
 *------------------------------------------------------------------------*/

namespace oneexpfpred_indpredmodeltest
{
  double timeStep = 0.25;
  double dose     = 320.0;
  double wt       = 79.6;
}


/*------------------------------------------------------------------------
 * Local class declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{

  //**********************************************************************
  //
  // Class:  OneExpF_ModelBasedExpY_Pred
  //
  //
  // This class evaluates Pred block expressions that correspond to a
  // single exponential for the mean of the individuals' data,
  //
  //           ds * exp[ - theta(0) * time ]
  //    f  =  -------------------------------  ,
  //                [theta(1) * w ]
  //
  // and model based weighting of the data using an exponential 
  // parameterization,
  //
  //    y  =  f * exp[ eta(0) ]  .
  //
  //**********************************************************************

  template<class Value>
  class OneExpF_ModelBasedExpY_Pred : public PredBase<Value>
  {
    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    OneExpF_ModelBasedExpY_Pred( int nY_iIn )
    :
    nY_i ( nY_iIn )
    {}

    ~OneExpF_ModelBasedExpY_Pred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;

  public:
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

      using namespace oneexpfpred_indpredmodeltest;

      double time = timeStep * j;

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
      //    y  =  f * exp[ eta(0) ]  .
      //
      ke = indepVar[thetaOffset + 0];
      cl = indepVar[thetaOffset + 1] * w;
      d = CppAD::exp(-ke * time);
      e = cl;
      depVar[fOffset + j] = ds * d / e;
      depVar[yOffset + j] = depVar[fOffset + j] * CppAD::exp(indepVar[etaOffset + 0]); 


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
    OneExpF_ModelBasedExpY_Pred(){}
    OneExpF_ModelBasedExpY_Pred( const OneExpF_ModelBasedExpY_Pred& ){}
    OneExpF_ModelBasedExpY_Pred & operator=( const OneExpF_ModelBasedExpY_Pred& ){}
  };


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
    nY_i ( nY_iIn )
    {}

    ~OneExpF_AdditivePlusThetaDepY_Pred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;

  public:
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

      using namespace oneexpfpred_indpredmodeltest;

      double time = timeStep * j;

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
  // Class:  OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Pred
  //
  //
  // This class is used to test the case where not all of the data
  // records for the individuals are observation records.
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
  class OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Pred : public PredBase<Value>
  {
    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Pred( int nY_iIn )
    :
    nY_i ( nY_iIn )
    {}

    ~OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Pred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;

  public:
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

      using namespace oneexpfpred_indpredmodeltest;

      // For this test, every other data record is an observation
      // event and every other record is missing its DV data item,
      // i.e., its MDV dat item is equal to 1.  That means that there
      // are twice as many data records as observation records.
      bool isObsEvent;

      // If j is even, then it will be considered to be an
      // observation event.
      if ( j / 2 * 2 == j )
      {
        isObsEvent = true;
      }
      else
      {
        isObsEvent = false;
      }

      // Set the position of this DV value in the list of DV values
      // for this individual that are not missing.
      int m = j / 2;

      double time = timeStep * m;

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
      depVar[fOffset + m] = ds * d / e;
      depVar[yOffset + m] = depVar[fOffset + m] + indepVar[etaOffset + 0]
        + indepVar[thetaOffset + 2] * indepVar[etaOffset + 1]; 


      //--------------------------------------------------------
      // Finish up.
      //--------------------------------------------------------

      // Return true if the DV value was not missing.
      if ( isObsEvent )
      {
        return true;
      }
      else
      {
        return false;
      }
    }


    //**********************************************************
    // 
    // Function: getNRecords
    //
    //**********************************************************

    int getNRecords( int i ) const
    {
      // For this test, there are twice as many data records as
      // observation records.
      return 2 * nY_i;
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
    OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Pred(){}
    OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Pred( const OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Pred& ){}
    OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Pred & operator=( const OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Pred& ){}
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

void IndPredModelTest::setUp()
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

void IndPredModelTest::tearDown()
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

Test* IndPredModelTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "IndPredModelTest" );

  suiteOfTests->addTest(new TestCaller<IndPredModelTest>(
    "OneExpF_ModelBasedExpY_Test", 
    &IndPredModelTest::OneExpF_ModelBasedExpY_Test ));

  suiteOfTests->addTest(new TestCaller<IndPredModelTest>(
    "OneExpF_AdditivePlusThetaDepY_Test", 
    &IndPredModelTest::OneExpF_AdditivePlusThetaDepY_Test ));

  suiteOfTests->addTest(new TestCaller<IndPredModelTest>(
    "OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Test", 
    &IndPredModelTest::OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Test ));

  suiteOfTests->addTest(new TestCaller<IndPredModelTest>(
    "isCachingProperlyTest", 
    &IndPredModelTest::isCachingProperlyTest ));

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: OneExpF_ModelBasedExpY_Test
 *
 *
 * The goal of this test is to check that the individual level
 * Pred model works for the case of the 
 *
 *     OneExpF_ModelBasedExpY_Pred
 *
 * Pred block expression evaluator, for which analytical results 
 * can be calculated and which does not use any data values. 
 *
 *************************************************************************/

void IndPredModelTest::OneExpF_ModelBasedExpY_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpfpred_indpredmodeltest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of data values for this individual.
  int nY_iKnown = 3;

  OneExpF_ModelBasedExpY_Pred< double > predEvaluator( nY_iKnown );

  OneExpF_ModelBasedExpY_Pred< AD<double> > predEvaluatorAD( nY_iKnown );

  OneExpF_ModelBasedExpY_Pred< AD< AD<double> > > predEvaluatorADAD( nY_iKnown );


  //------------------------------------------------------------
  // Prepare the variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of independent variables.
  const int nTheta = 2;
  const int nEta   = 1;

  // Set the current value for theta.
  valarray<double> thetaCurr( nTheta );
  thetaCurr[0] = 3.0;
  thetaCurr[1] = 0.08;

  // Set the limits for theta.
  valarray<double> thetaLow( nTheta );
  valarray<double> thetaUp ( nTheta );
  thetaLow[0] = -20.0;
  thetaUp [0] = 100.0;
  thetaLow[1] = 0.005;
  thetaUp [1] = 0.7;


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


  //------------------------------------------------------------
  // Construct the individual level Pred model.
  //------------------------------------------------------------

  IndPredModel model(
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
  int nIndPar = model.getNIndPar();

  // Get the number of observations for this individual.
  int iCurr = 0;
  int nY_i = predEvaluator.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare various quantities for the test.
  //------------------------------------------------------------

  // Get the current value for the individual parameter.
  valarray<double> bCurr( nIndPar );
  model.getIndPar( bCurr );

  valarray<double> dataMean              ( nY_i );
  valarray<double> dataMean_indPar       ( nY_i * nIndPar );
  valarray<double> dataVariance          ( nY_i * nY_i );
  valarray<double> dataVariance_indPar   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInv       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_indPar( nY_i * nY_i * nIndPar );

  bool ok;

  // Evaluate these quantities at the current parameter value,
  // which is set when the model is constructed.
  model.dataMean                   ( dataMean );
  model.dataVariance               ( dataVariance );
  model.dataVarianceInv            ( dataVarianceInv );
  ok = model.dataMean_indPar       ( dataMean_indPar );
  ok = model.dataVariance_indPar   ( dataVariance_indPar );
  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );

  valarray<double> indParLow ( nIndPar );
  valarray<double> indParUp  ( nIndPar );
  valarray<double> indParStep( nIndPar );

  // Get the limits for the individual parameters.
  model.getIndParLimits( indParLow, indParUp );

  // Get the step sizes for the individual parameters.
  model.getIndParStep( indParStep );


  //------------------------------------------------------------
  // Prepare known values related to the individual parameter.
  //------------------------------------------------------------

  valarray<double> omegaCurr( nEta * nEta );

  // Create a covariance matrix that is equal to the one that the
  // IndPredModel is maintaining internally.
  DiagCov omega( nEta );
  omega.expandCovMinRep( omegaMinRep, omegaCurr );
  omega.setCov( omegaCurr );

  valarray<double> omegaKnown         ( nEta * nEta );
  valarray<double> omegaParKnown      ( nOmegaPar );
  valarray<double> omegaParLowKnown   ( nOmegaPar );
  valarray<double> omegaParUpKnown    ( nOmegaPar );
  valarray<double> omega_omegaParKnown( nEta * nEta * nOmegaPar );

  // Get known values for omega and its derivative.
  omega.cov    ( omegaKnown );
  omega.cov_par( omega_omegaParKnown );

  // Get known values for omega's parameter and its limits.
  omega.calcPar     ( omegaCurr,        omegaParKnown );
  omega.getParLimits( omegaParLowKnown, omegaParUpKnown );

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
  valarray<double> dataVariance_indParKnown   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInvKnown       ( nY_i * nY_i );
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

    dataMean_indParKnown[j + col * nY_i] = - timeStep * j * dataMean[j];
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

    dataMean_indParKnown[j + col * nY_i] = - wt * dataMean[j] /
      ( thetaCurr[1] * wt );
  }

  // Calculate the known value for the variance of the data.
  dataVarianceKnown = 0.0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the values for the diagonal elements of the data variance:
    //
    //                                                   2
    //     R      ( b  )  =  omega       [ f    ( b  ) ]    .
    //      i(j,j)   i            (0,0)     i(j)   i
    //
    dataVarianceKnown[j + j * nY_i] = omegaKnown[0 + 0 * nEta] *
      dataMean[j] * dataMean[j];
  }

  // Calculate the known value for the derivative of the variance
  // of the data.
  dataVariance_indParKnown = 0.0;
  int nRow = nY_i * nY_i;
  int row;
  for ( k = 0; k < nTheta; k++ )
  {
    for ( j = 0; j < nY_i; j++ )
    {
      // Set the known values for this column,
      //
      //      (k)
      //     d       R      ( b  )
      //      theta   i(j,j)   i
      //
      //                                         (k)               
      //         =  2  omega       f    ( b  )  d       f    ( b  )  .
      //                    (0,0)   i(j)   i     theta   i(j)   i
      //
      row = j * nY_i + j;
      col = k + thetaOffsetInIndPar;
    
      dataVariance_indParKnown[row + col * nRow] = 
        2 * omegaKnown[0 + 0 * nEta] * 
        dataMean_indPar[j + k * nY_i] * dataMean[j];
    }
  }
  int nOmega_omegaParRow = nEta * nEta;
  int omega_omegaParRow;
  int omega_omegaParCol;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (k)
    //     d          R      ( b  )  =  
    //      omegaPar   i(j,j)   i
    //
    //             -             -  2   
    //            |               |     (k)               
    //         =  |  f    ( b  )  |    d         omega     ( omegaPar )  .
    //            |   i(j)   i    |     omegaPar      (k,k)
    //             -             -    
    //
    row = j * nY_i + j;
    col = k + omegaParOffsetInIndPar;

    omega_omegaParRow = k * nEta + k;
    omega_omegaParCol = k;
    
    dataVariance_indParKnown[row + col * nRow] = 
      dataMean[j] * dataMean[j] *
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
  // Prepare known values related to SPK estimation.
  //------------------------------------------------------------

  valarray<double> indParLowKnown ( nIndPar );
  valarray<double> indParUpKnown  ( nIndPar );

  // Set the known limits for the individual parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    indParLowKnown[k + thetaOffsetInIndPar] = thetaLow[k];
    indParUpKnown [k + thetaOffsetInIndPar] = thetaUp [k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    indParLowKnown[k + omegaParOffsetInIndPar] = omegaParLowKnown[k];
    indParUpKnown [k + omegaParOffsetInIndPar] = omegaParUpKnown [k];
  }
  
  // Since the step sizes may change as IndPredModel evolves, only 
  // check that they are positive.
  for ( k = 0; k < nIndPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The step size for an individual parameter element is invalid.",
      indParStep[k] != 0.0 );
  }


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of individual parameters is not correct.",
    nIndPar == nIndParKnown );

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of data values for this individual is not correct.",
    nY_i == nY_iKnown );

  compareToKnown( 
    bCurr,
    bCurrKnown,
    "bCurr",
    tol );

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

  compareToKnown( 
    indParLow,
    indParLowKnown,
    "indParLow",
    tol );

  compareToKnown( 
    indParUp,
    indParUpKnown,
    "indParUp",
    tol );
}


/*************************************************************************
 *
 * Function: OneExpF_AdditivePlusThetaDepY_Test
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

void IndPredModelTest::OneExpF_AdditivePlusThetaDepY_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpfpred_indpredmodeltest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of data values for this individual.
  int nY_iKnown = 5;

  OneExpF_AdditivePlusThetaDepY_Pred< double > predEvaluator( nY_iKnown );

  OneExpF_AdditivePlusThetaDepY_Pred< AD<double> > predEvaluatorAD( nY_iKnown );

  OneExpF_AdditivePlusThetaDepY_Pred< AD< AD<double> > > predEvaluatorADAD( nY_iKnown );


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

  IndPredModel model(
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
  int nIndPar = model.getNIndPar();

  // Get the number of observations for this individual.
  int iCurr = 0;
  int nY_i = predEvaluator.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare various quantities for the test.
  //------------------------------------------------------------

  // Get the current value for the individual parameter.
  valarray<double> bCurr( nIndPar );
  model.getIndPar( bCurr );

  valarray<double> dataMean              ( nY_i );
  valarray<double> dataMean_indPar       ( nY_i * nIndPar );
  valarray<double> dataVariance          ( nY_i * nY_i );
  valarray<double> dataVariance_indPar   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInv       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_indPar( nY_i * nY_i * nIndPar );

  bool ok;

  // Evaluate these quantities at the current parameter value,
  // which is set when the model is constructed.
  model.dataMean                   ( dataMean );
  model.dataVariance               ( dataVariance );
  model.dataVarianceInv            ( dataVarianceInv );
  ok = model.dataMean_indPar       ( dataMean_indPar );
  ok = model.dataVariance_indPar   ( dataVariance_indPar );
  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );

  valarray<double> indParLow ( nIndPar );
  valarray<double> indParUp  ( nIndPar );
  valarray<double> indParStep( nIndPar );

  // Get the limits for the individual parameters.
  model.getIndParLimits( indParLow, indParUp );

  // Get the step sizes for the individual parameters.
  model.getIndParStep( indParStep );

  valarray<double> standardPar       ( nIndPar );
  valarray<double> standardPar_indPar( nIndPar * nIndPar );
  valarray<bool>   standardParMask   ( nIndPar );

  // Get the current value for the standard parameter and its
  // derivative.
  model.getStandardPar       ( standardPar );
  model.getStandardPar_indPar( standardPar_indPar );

  // Set the individual parameter mask.
  valarray<bool> indParMask( nIndPar );
  indParMask[0] = true;
  indParMask[1] = false;
  indParMask[2] = false;
  indParMask[3] = true;
  indParMask[4] = false;

  // Calculate the mask for the standard parameters.
  model.getStandardParMask( indParMask, standardParMask );


  //------------------------------------------------------------
  // Prepare known values related to the individual parameter.
  //------------------------------------------------------------

  valarray<double> omegaCurr( nEta * nEta );

  // Create a covariance matrix that is equal to the one that the
  // IndPredModel is maintaining internally.
  DiagCov omega( nEta );
  omega.expandCovMinRep( omegaMinRep, omegaCurr );
  omega.setCov( omegaCurr );

  valarray<double> omegaKnown         ( nEta * nEta );
  valarray<double> omegaParKnown      ( nOmegaPar );
  valarray<double> omegaParLowKnown   ( nOmegaPar );
  valarray<double> omegaParUpKnown    ( nOmegaPar );
  valarray<double> omega_omegaParKnown( nEta * nEta * nOmegaPar );

  // Get known values for omega and its derivative.
  omega.cov    ( omegaKnown );
  omega.cov_par( omega_omegaParKnown );

  // Get known values for omega's parameter and its limits.
  omega.calcPar     ( omegaCurr,        omegaParKnown );
  omega.getParLimits( omegaParLowKnown, omegaParUpKnown );

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

  valarray<double> standardParKnown       ( nIndPar );
  valarray<double> standardPar_indParKnown( nIndPar * nIndPar );
  valarray<bool>   standardParMaskKnown   ( nIndPar );

  // Set the known value for the standard parameter.
  //
  //                      -                 -
  //                     |     thetaCurr     |
  //     standardPar  =  |                   |  .
  //                     |  omegaMinRepCurr  |
  //                      -                 -
  //
  for ( k = 0; k < nTheta; k++ )
  {
    standardParKnown[k + thetaOffsetInIndPar] = thetaCurr[k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    standardParKnown[k + omegaParOffsetInIndPar] = omegaMinRep[k];
  }

  valarray<double> omegaMinRep_omegaParKnown( nOmegaPar * nOmegaPar );

  // Get the known derivative of the minimal representation of
  // omega.
  omega.calcCovMinRep_par(
    omega_omegaParKnown,
    nOmegaPar,
    omegaMinRep_omegaParKnown );

  // Create an nTheta by nTheta identity matrix.
  valarray<double> identityNTheta( nTheta * nTheta );
  identity( nTheta, identityNTheta );

  // Set the known value for the derivative of the standard parameter,
  //
  //     d   standardPar
  //      b
  //             -                                                 -
  //            |    I                                         0    |
  //            |     nTheta                                        |
  //            |                                                   |
  //         =  |    0          d        omegaMinRep( omegaPar )    |  ,
  //            |                omegaPar                           |
  //             -                                                 -
  //
  standardPar_indParKnown = 0.0;
  replaceSubblock(
    standardPar_indParKnown,
    nIndPar,
    identityNTheta,
    nTheta,
    thetaOffsetInIndPar,
    thetaOffsetInIndPar );
  replaceSubblock(
    standardPar_indParKnown,
    nIndPar,
    omegaMinRep_omegaParKnown,
    nOmegaPar,
    omegaParOffsetInIndPar,
    omegaParOffsetInIndPar );

  // Set the known value for the mask for the standard parameters.
  standardParMaskKnown = indParMask;


  //------------------------------------------------------------
  // Prepare known values related to the model functions.
  //------------------------------------------------------------

  valarray<double> dataMeanKnown              ( nY_i );
  valarray<double> dataMean_indParKnown       ( nY_i * nIndPar );
  valarray<double> dataVarianceKnown          ( nY_i * nY_i );
  valarray<double> dataVariance_indParKnown   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInvKnown       ( nY_i * nY_i );
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

    dataMean_indParKnown[j + col * nY_i] = - timeStep * j * dataMean[j];
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

    dataMean_indParKnown[j + col * nY_i] = - wt * dataMean[j] /
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
  // Prepare known values related to SPK estimation.
  //------------------------------------------------------------

  valarray<double> indParLowKnown ( nIndPar );
  valarray<double> indParUpKnown  ( nIndPar );

  // Set the known limits for the individual parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    indParLowKnown[k + thetaOffsetInIndPar] = thetaLow[k];
    indParUpKnown [k + thetaOffsetInIndPar] = thetaUp [k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    indParLowKnown[k + omegaParOffsetInIndPar] = omegaParLowKnown[k];
    indParUpKnown [k + omegaParOffsetInIndPar] = omegaParUpKnown [k];
  }
  
  // Since the step sizes may change as IndPredModel evolves, only 
  // check that they are positive.
  for ( k = 0; k < nIndPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The step size for an individual parameter element is invalid.",
      indParStep[k] != 0.0 );
  }


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of individual parameters is not correct.",
    nIndPar == nIndParKnown );

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of data values for this individual is not correct.",
    nY_i == nY_iKnown );

  compareToKnown( 
    bCurr,
    bCurrKnown,
    "bCurr",
    tol );

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

  compareToKnown( 
    indParLow,
    indParLowKnown,
    "indParLow",
    tol );

  compareToKnown( 
    indParUp,
    indParUpKnown,
    "indParUp",
    tol );

  compareToKnown( 
    standardPar,
    standardParKnown,
    "standardPar",
    tol );

  compareToKnown( 
    standardPar_indPar,
    standardPar_indParKnown,
    "standardPar_indPar",
    tol );

  compareToKnown( 
    standardParMask,
    standardParMaskKnown,
    "standardParMask" );

}


/*************************************************************************
 *
 * Function: OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Test
 *
 *
 * The goal of this test is to check that the individual level Pred
 * model works for the case where not all of the data records for the
 * individuals are observation records and for the case of the
 *
 *     OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Pred
 *
 * Pred block expression evaluator, for which analytical results 
 * can be calculated and which does not use any data values. 
 *
 *************************************************************************/

void IndPredModelTest::OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpfpred_indpredmodeltest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of data values for this individual.
  int nY_iKnown = 5;

  OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Pred< double > predEvaluator( nY_iKnown );

  OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Pred< AD<double> > predEvaluatorAD( nY_iKnown );

  OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Pred< AD< AD<double> > > predEvaluatorADAD( nY_iKnown );


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

  IndPredModel model(
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
  int nIndPar = model.getNIndPar();

  // Get the number of observations for this individual.
  int iCurr = 0;
  int nY_i = predEvaluator.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare various quantities for the test.
  //------------------------------------------------------------

  // Get the current value for the individual parameter.
  valarray<double> bCurr( nIndPar );
  model.getIndPar( bCurr );

  valarray<double> dataMean              ( nY_i );
  valarray<double> dataMean_indPar       ( nY_i * nIndPar );
  valarray<double> dataVariance          ( nY_i * nY_i );
  valarray<double> dataVariance_indPar   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInv       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_indPar( nY_i * nY_i * nIndPar );

  bool ok;

  // Evaluate these quantities at the current parameter value,
  // which is set when the model is constructed.
  model.dataMean                   ( dataMean );
  model.dataVariance               ( dataVariance );
  model.dataVarianceInv            ( dataVarianceInv );
  ok = model.dataMean_indPar       ( dataMean_indPar );
  ok = model.dataVariance_indPar   ( dataVariance_indPar );
  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );

  valarray<double> indParLow ( nIndPar );
  valarray<double> indParUp  ( nIndPar );
  valarray<double> indParStep( nIndPar );

  // Get the limits for the individual parameters.
  model.getIndParLimits( indParLow, indParUp );

  // Get the step sizes for the individual parameters.
  model.getIndParStep( indParStep );

  valarray<double> standardPar       ( nIndPar );
  valarray<double> standardPar_indPar( nIndPar * nIndPar );

  // Get the current value for the standard parameter and its
  // derivative.
  model.getStandardPar       ( standardPar );
  model.getStandardPar_indPar( standardPar_indPar );


  //------------------------------------------------------------
  // Prepare known values related to the individual parameter.
  //------------------------------------------------------------

  valarray<double> omegaCurr( nEta * nEta );

  // Create a covariance matrix that is equal to the one that the
  // IndPredModel is maintaining internally.
  DiagCov omega( nEta );
  omega.expandCovMinRep( omegaMinRep, omegaCurr );
  omega.setCov( omegaCurr );

  valarray<double> omegaKnown         ( nEta * nEta );
  valarray<double> omegaParKnown      ( nOmegaPar );
  valarray<double> omegaParLowKnown   ( nOmegaPar );
  valarray<double> omegaParUpKnown    ( nOmegaPar );
  valarray<double> omega_omegaParKnown( nEta * nEta * nOmegaPar );

  // Get known values for omega and its derivative.
  omega.cov    ( omegaKnown );
  omega.cov_par( omega_omegaParKnown );

  // Get known values for omega's parameter and its limits.
  omega.calcPar     ( omegaCurr,        omegaParKnown );
  omega.getParLimits( omegaParLowKnown, omegaParUpKnown );

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

  valarray<double> standardParKnown       ( nIndPar );
  valarray<double> standardPar_indParKnown( nIndPar * nIndPar );

  // Set the known value for the standard parameter.
  //
  //                      -                 -
  //                     |     thetaCurr     |
  //     standardPar  =  |                   |  .
  //                     |  omegaMinRepCurr  |
  //                      -                 -
  //
  for ( k = 0; k < nTheta; k++ )
  {
    standardParKnown[k + thetaOffsetInIndPar] = thetaCurr[k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    standardParKnown[k + omegaParOffsetInIndPar] = omegaMinRep[k];
  }

  valarray<double> omegaMinRep_omegaParKnown( nOmegaPar * nOmegaPar );

  // Get the known derivative of the minimal representation of
  // omega.
  omega.calcCovMinRep_par(
    omega_omegaParKnown,
    nOmegaPar,
    omegaMinRep_omegaParKnown );

  // Create an nTheta by nTheta identity matrix.
  valarray<double> identityNTheta( nTheta * nTheta );
  identity( nTheta, identityNTheta );

  // Set the known value for the derivative of the standard parameter,
  //
  //     d   standardPar
  //      b
  //             -                                                 -
  //            |    I                                         0    |
  //            |     nTheta                                        |
  //            |                                                   |
  //         =  |    0          d        omegaMinRep( omegaPar )    |  ,
  //            |                omegaPar                           |
  //             -                                                 -
  //
  standardPar_indParKnown = 0.0;
  replaceSubblock(
    standardPar_indParKnown,
    nIndPar,
    identityNTheta,
    nTheta,
    thetaOffsetInIndPar,
    thetaOffsetInIndPar );
  replaceSubblock(
    standardPar_indParKnown,
    nIndPar,
    omegaMinRep_omegaParKnown,
    nOmegaPar,
    omegaParOffsetInIndPar,
    omegaParOffsetInIndPar );


  //------------------------------------------------------------
  // Prepare known values related to the model functions.
  //------------------------------------------------------------

  valarray<double> dataMeanKnown              ( nY_i );
  valarray<double> dataMean_indParKnown       ( nY_i * nIndPar );
  valarray<double> dataVarianceKnown          ( nY_i * nY_i );
  valarray<double> dataVariance_indParKnown   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInvKnown       ( nY_i * nY_i );
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

    dataMean_indParKnown[j + col * nY_i] = - timeStep * j * dataMean[j];
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

    dataMean_indParKnown[j + col * nY_i] = - wt * dataMean[j] /
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
  // Prepare known values related to SPK estimation.
  //------------------------------------------------------------

  valarray<double> indParLowKnown ( nIndPar );
  valarray<double> indParUpKnown  ( nIndPar );

  // Set the known limits for the individual parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    indParLowKnown[k + thetaOffsetInIndPar] = thetaLow[k];
    indParUpKnown [k + thetaOffsetInIndPar] = thetaUp [k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    indParLowKnown[k + omegaParOffsetInIndPar] = omegaParLowKnown[k];
    indParUpKnown [k + omegaParOffsetInIndPar] = omegaParUpKnown [k];
  }
  
  // Since the step sizes may change as IndPredModel evolves, only 
  // check that they are positive.
  for ( k = 0; k < nIndPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The step size for an individual parameter element is invalid.",
      indParStep[k] != 0.0 );
  }


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of individual parameters is not correct.",
    nIndPar == nIndParKnown );

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of data values for this individual is not correct.",
    nY_i == nY_iKnown );

  compareToKnown( 
    bCurr,
    bCurrKnown,
    "bCurr",
    tol );

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

  compareToKnown( 
    indParLow,
    indParLowKnown,
    "indParLow",
    tol );

  compareToKnown( 
    indParUp,
    indParUpKnown,
    "indParUp",
    tol );

  compareToKnown( 
    standardPar,
    standardParKnown,
    "standardPar",
    tol );

  compareToKnown( 
    standardPar_indPar,
    standardPar_indParKnown,
    "standardPar_indPar",
    tol );
}


/*************************************************************************
 *
 * Function: isCachingProperlyTest
 *
 *
 * The goal of this test is to check that the individual level 
 * Pred model is caching properly.
 *
 *************************************************************************/

void IndPredModelTest::isCachingProperlyTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpfpred_indpredmodeltest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of data values for this individual.
  int nY_iKnown = 3;

  OneExpF_ModelBasedExpY_Pred< double > predEvaluator( nY_iKnown );

  OneExpF_ModelBasedExpY_Pred< AD<double> > predEvaluatorAD( nY_iKnown );

  OneExpF_ModelBasedExpY_Pred< AD< AD<double> > > predEvaluatorADAD( nY_iKnown );


  //------------------------------------------------------------
  // Prepare the variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of independent variables.
  const int nTheta = 2;
  const int nEta   = 1;

  // Set the current value for theta.
  valarray<double> thetaCurr( nTheta );
  thetaCurr[0] = 3.0;
  thetaCurr[1] = 0.08;

  // Set the limits for theta.
  valarray<double> thetaLow( nTheta );
  valarray<double> thetaUp ( nTheta );
  thetaLow[0] = -20.0;
  thetaUp [0] = 100.0;
  thetaLow[1] = 0.005;
  thetaUp [1] = 0.7;


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


  //------------------------------------------------------------
  // Construct the individual level Pred model.
  //------------------------------------------------------------

  IndPredModel model(
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
  int nIndPar = model.getNIndPar();

  // Get the number of observations for this individual.
  int iCurr = 0;
  int nY_i = predEvaluator.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare to see if values are being cached.
  //------------------------------------------------------------

  valarray<double> dataMean              ( nY_i );
  valarray<double> dataMean_indPar       ( nY_i * nIndPar );
  valarray<double> dataVariance          ( nY_i * nY_i );
  valarray<double> dataVariance_indPar   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInv       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_indPar( nY_i * nY_i * nIndPar );

  valarray<double> indParLow( nIndPar );
  valarray<double> indParUp ( nIndPar );

  // Get the limits for the individual parameters.
  model.getIndParLimits( indParLow, indParUp );

  bool ok;

  // Evaluate these quantities at the current parameter value,
  // which is set when the model is constructed.
  model.dataMean                   ( dataMean );
  model.dataVariance               ( dataVariance );
  model.dataVarianceInv            ( dataVarianceInv );
  ok = model.dataMean_indPar       ( dataMean_indPar );
  ok = model.dataVariance_indPar   ( dataVariance_indPar );
  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );


  //------------------------------------------------------------
  // See if cached values are used when the parameter changes.
  //------------------------------------------------------------

  // Evaluate the quantities at a different parameter value.
  // The cached values should not be used in this case.
  model.setIndPar( indParLow );

  model.dataMean( dataMean );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataMean was used when it was not valid.",
    model.getUsedCachedDataMean() == false );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached f and h value was used when it was not valid.",
    model.getUsedCachedFAndH() == false );

  model.dataVariance( dataVariance );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVariance was used when it was not valid.",
    model.getUsedCachedDataVariance() == false );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omega was used when it was not valid.",
    model.getUsedCachedOmega() == false );

  model.dataVarianceInv( dataVarianceInv );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVarianceInv was used when it was not valid.",
    model.getUsedCachedDataVarianceInv() == false );

  ok = model.dataMean_indPar( dataMean_indPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataMean_indPar was used when it was not valid.",
    model.getUsedCachedDataMean_indPar() == false );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached fAndH_theta value was used when it was not valid.",
    model.getUsedCachedFAndH_theta() == false );

  ok = model.dataVariance_indPar( dataVariance_indPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVariance_indPar was used when it was not valid.",
    model.getUsedCachedDataVariance_indPar() == false );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omega_omegaPar was used when it was not valid.",
    model.getUsedCachedOmega_omegaPar() == false );

  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVarianceInv_indPar was used when it was not valid.",
    model.getUsedCachedDataVarianceInv_indPar() == false );


  //------------------------------------------------------------
  // See if cached values are used when the parameter does not change.
  //------------------------------------------------------------

  // Evaluate the quantities at the same parameter value.
  // The cached values should be used in this case.
  model.setIndPar( indParLow );

  model.dataMean( dataMean );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataMean was not used when it was valid.",
    model.getUsedCachedDataMean() == true );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached f and h value was not used when it was valid.",
    model.getUsedCachedFAndH() == true );

  model.dataVariance( dataVariance );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVariance was not used when it was valid.",
    model.getUsedCachedDataVariance() == true );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omega was not used when it was valid.",
    model.getUsedCachedOmega() == true );

  model.dataVarianceInv( dataVarianceInv );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVarianceInv was not used when it was valid.",
    model.getUsedCachedDataVarianceInv() == true );

  ok = model.dataMean_indPar( dataMean_indPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataMean_indPar was not used when it was valid.",
    model.getUsedCachedDataMean_indPar() == true );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached fAndH_theta was not used when they were valid.",
    model.getUsedCachedFAndH_theta() == true );

  ok = model.dataVariance_indPar( dataVariance_indPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVariance_indPar was not used when it was valid.",
    model.getUsedCachedDataVariance_indPar() == true );

  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVarianceInv_indPar was not used when it was valid.",
    model.getUsedCachedDataVarianceInv_indPar() == true );


  //------------------------------------------------------------
  // See if these cached values are not used when the parameter does not change.
  //------------------------------------------------------------

  // Because the cached value for dataVariance_indPar contains the
  // derivatives of omega, this cached value is not required and
  // therefore is not used in this case.
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omega_omegaPar was used when it should not have been.",
    model.getUsedCachedOmega_omegaPar() == false );
}

 
