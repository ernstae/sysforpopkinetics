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
 * File: PopPredModelTest.cpp
 *
 *
 * Unit test for the class PopPredModel.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK Pred test suite header files.
#include "PopPredModelTest.h"
#include "compareToKnown.h"

// SPK Pred library header files.
#include "../../../spkpred/DiagCov.h"
#include "../../../spkpred/FullCov.h"
#include "../../../spkpred/PopPredModel.h"

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

namespace oneexpfpred_poppredmodeltest
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
  // Class:  NoEta_OneExpF_ModelBasedExpY_Pred
  //
  //
  // This class evaluates Pred block expressions that correspond to a
  // single exponential for the mean of the individuals' data without
  // any eta elements, 
  //
  //           ds * exp[ - theta(0) * time ]
  //    f  =  -------------------------------  ,
  //                [theta(1) * w ]
  //
  // and model based weighting of the data using an exponential 
  // parameterization,
  //
  //    y  =  f * exp[ eps(0) ]  .
  //
  //**********************************************************************

  template<class Value>
  class NoEta_OneExpF_ModelBasedExpY_Pred : public PredBase<Value>
  {
    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    NoEta_OneExpF_ModelBasedExpY_Pred( int nY_iIn )
    :
    nY_i ( nY_iIn )
    {}

    ~NoEta_OneExpF_ModelBasedExpY_Pred(){}


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

      using namespace oneexpfpred_poppredmodeltest;

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
      //    y  =  f * exp[ eps(0) ]  .
      //
      ke = indepVar[thetaOffset + 0];
      cl = indepVar[thetaOffset + 1] * w;
      d = CppAD::exp(-ke * time);
      e = cl;
      depVar[fOffset + j] = ds * d / e;
      depVar[yOffset + j] = depVar[fOffset + j] * CppAD::exp(indepVar[epsOffset + 0]); 


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
    NoEta_OneExpF_ModelBasedExpY_Pred(){}
    NoEta_OneExpF_ModelBasedExpY_Pred( const NoEta_OneExpF_ModelBasedExpY_Pred& ){}
    NoEta_OneExpF_ModelBasedExpY_Pred & operator=( const NoEta_OneExpF_ModelBasedExpY_Pred& ){}
  };


  //**********************************************************************
  //
  // Class:  NoEta_OneExpF_AdditivePlusThetaDepY_Pred
  //
  //
  // This class evaluates Pred block expressions that correspond to a
  // single exponential for the mean of the individuals' data without
  // any eta elements, 
  //
  //           ds * exp[ - theta(0) * time ]
  //    f  =  -------------------------------  ,
  //                [theta(1) * w ]
  //
  // and a combined model for the variance of the data that is additive
  // with a theta dependent term,
  //
  //    y  =  f + eps(0) + theta(2) * eps(1)  .
  //
  //**********************************************************************

  template<class Value>
  class NoEta_OneExpF_AdditivePlusThetaDepY_Pred : public PredBase<Value>
  {
    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    NoEta_OneExpF_AdditivePlusThetaDepY_Pred( int nY_iIn )
    :
    nY_i ( nY_iIn )
    {}

    ~NoEta_OneExpF_AdditivePlusThetaDepY_Pred(){}


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

      using namespace oneexpfpred_poppredmodeltest;

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
      //    y  =  f + eps(0) + theta(2) * eps(1)  .
      //
      ke = indepVar[thetaOffset + 0];
      cl = indepVar[thetaOffset + 1] * w;
      d = CppAD::exp(-ke * time);
      e = cl;
      depVar[fOffset + j] = ds * d / e;
      depVar[yOffset + j] = depVar[fOffset + j] + indepVar[epsOffset + 0]
        + indepVar[thetaOffset + 2] * indepVar[epsOffset + 1]; 


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
    NoEta_OneExpF_AdditivePlusThetaDepY_Pred(){}
    NoEta_OneExpF_AdditivePlusThetaDepY_Pred( const NoEta_OneExpF_AdditivePlusThetaDepY_Pred& ){}
    NoEta_OneExpF_AdditivePlusThetaDepY_Pred & operator=( const NoEta_OneExpF_AdditivePlusThetaDepY_Pred& ){}
  };


  //**********************************************************************
  //
  // Class:  OneExpF_ModelBasedExpY_Pred
  //
  //
  // This class evaluates Pred block expressions that correspond to a
  // single exponential for the mean of the individuals' data,
  //
  //           ds * exp[ -(theta(0) + eta(0)) * time ]
  //    f  =  -----------------------------------------  ,
  //                [theta(1) * w  +  eta(1)]
  //
  // and model based weighting of the data using an exponential 
  // parameterization,
  //
  //    y  =  f * exp[ eps(0) ]  .
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

      using namespace oneexpfpred_poppredmodeltest;

      double time = timeStep * j;

      ds = dose * wt;
      w = wt;


      //--------------------------------------------------------
      // Evaluate the mean of the data and its predicted value.
      //--------------------------------------------------------

      // Calculate
      //
      //           ds * exp[ -(theta(0) + eta(0)) * time ]
      //    f  =  -----------------------------------------  ,
      //                [theta(1) * w  +  eta(1)]
      //
      // and
      //
      //    y  =  f * exp[ eps(0) ]  .
      //
      ke = indepVar[thetaOffset + 0] + indepVar[etaOffset + 0];
      cl = indepVar[thetaOffset + 1] * w + indepVar[etaOffset + 1];
      d = CppAD::exp(-ke * time);
      e = cl;
      depVar[fOffset + j] = ds * d / e;
      depVar[yOffset + j] = depVar[fOffset + j] * CppAD::exp(indepVar[epsOffset + 0]); 


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
  //           ds * exp[ -(theta(0) + eta(0)) * time ]
  //    f  =  -----------------------------------------  ,
  //                [theta(1) * w  +  eta(1)]
  //
  // and a combined model for the variance of the data that is additive
  // with a theta dependent term,
  //
  //    y  =  f + eps(0) + theta(2) * eps(1)  .
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

      using namespace oneexpfpred_poppredmodeltest;

      double time = timeStep * j;

      ds = dose * wt;
      w = wt;


      //--------------------------------------------------------
      // Evaluate the mean of the data and its predicted value.
      //--------------------------------------------------------

      // Calculate
      //
      //           ds * exp[ -(theta(0) + eta(0)) * time ]
      //    f  =  -----------------------------------------  ,
      //                [theta(1) * w  +  eta(1)]
      //
      // and
      //
      //    y  =  f + eps(0) + theta(2) * eps(1)  .
      //
      ke = indepVar[thetaOffset + 0] + indepVar[etaOffset + 0];
      cl = indepVar[thetaOffset + 1] * w + indepVar[etaOffset + 1];
      d = CppAD::exp(-ke * time);
      e = cl;
      depVar[fOffset + j] = ds * d / e;
      depVar[yOffset + j] = depVar[fOffset + j] + indepVar[epsOffset + 0]
        + indepVar[thetaOffset + 2] * indepVar[epsOffset + 1]; 


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
  //           ds * exp[ -(theta(0) + eta(0)) * time ]
  //    f  =  -----------------------------------------  ,
  //                [theta(1) * w  +  eta(1)]
  //
  // and a combined model for the variance of the data that is additive
  // with a theta dependent term,
  //
  //    y  =  f + eps(0) + theta(2) * eps(1)  .
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

      using namespace oneexpfpred_poppredmodeltest;

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
      //           ds * exp[ -(theta(0) + eta(0)) * time ]
      //    f  =  -----------------------------------------  ,
      //                [theta(1) * w  +  eta(1)]
      //
      // and
      //
      //    y  =  f + eps(0) + theta(2) * eps(1)  .
      //
      ke = indepVar[thetaOffset + 0] + indepVar[etaOffset + 0];
      cl = indepVar[thetaOffset + 1] * w + indepVar[etaOffset + 1];
      d = CppAD::exp(-ke * time);
      e = cl;
      depVar[fOffset + m] = ds * d / e;
      depVar[yOffset + m] = depVar[fOffset + m] + indepVar[epsOffset + 0]
        + indepVar[thetaOffset + 2] * indepVar[epsOffset + 1]; 


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

void PopPredModelTest::setUp()
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

void PopPredModelTest::tearDown()
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

Test* PopPredModelTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "PopPredModelTest" );

  suiteOfTests->addTest(new TestCaller<PopPredModelTest>(
    "NoEta_OneExpF_ModelBasedExpY_Test", 
    &PopPredModelTest::NoEta_OneExpF_ModelBasedExpY_Test ));

  suiteOfTests->addTest(new TestCaller<PopPredModelTest>(
    "NoEta_OneExpF_AdditivePlusThetaDepY_Test", 
    &PopPredModelTest::NoEta_OneExpF_AdditivePlusThetaDepY_Test ));

  suiteOfTests->addTest(new TestCaller<PopPredModelTest>(
    "OneExpF_ModelBasedExpY_Test", 
    &PopPredModelTest::OneExpF_ModelBasedExpY_Test ));

  suiteOfTests->addTest(new TestCaller<PopPredModelTest>(
    "OneExpF_AdditivePlusThetaDepY_Test", 
    &PopPredModelTest::OneExpF_AdditivePlusThetaDepY_Test ));

  suiteOfTests->addTest(new TestCaller<PopPredModelTest>(
    "OneExpF_AdditivePlusThetaDepY_FullSigma_Test", 
    &PopPredModelTest::OneExpF_AdditivePlusThetaDepY_FullSigma_Test ));

  suiteOfTests->addTest(new TestCaller<PopPredModelTest>(
    "OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Test", 
    &PopPredModelTest::OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Test ));

  suiteOfTests->addTest(new TestCaller<PopPredModelTest>(
    "isCachingProperlyTest", 
    &PopPredModelTest::isCachingProperlyTest ));

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: NoEta_OneExpF_ModelBasedExpY_Test
 *
 *
 * The goal of this test is to check that the individual level
 * Pred model works for the case of the 
 *
 *     NoEta_OneExpF_ModelBasedExpY_Pred
 *
 * Pred block expression evaluator, for which analytical results 
 * can be calculated and which does not use any data values. 
 *
 *************************************************************************/

void PopPredModelTest::NoEta_OneExpF_ModelBasedExpY_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpfpred_poppredmodeltest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluators.
  //------------------------------------------------------------

  // Set the number of data values for this individual.
  int nY_iKnown = 3;

  NoEta_OneExpF_ModelBasedExpY_Pred< double > predEvaluator( nY_iKnown );
  NoEta_OneExpF_ModelBasedExpY_Pred< AD<double> > predEvaluatorAD( nY_iKnown );
  NoEta_OneExpF_ModelBasedExpY_Pred< AD< AD<double> > > predEvaluatorADAD( nY_iKnown );


  //------------------------------------------------------------
  // Prepare the variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of independent variables.
  const int nTheta = 2;
  const int nEta   = 0;
  const int nEps   = 1;

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

  // Set the current value for eta.
  valarray<double> etaCurr( nEta );


  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrices.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  covStruct omegaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nOmegaPar = nEta;

  // Set the diagonal elements for the current value for omega.
  valarray<double> omegaMinRep( nOmegaPar );

  // Set the structure of sigma, the covariance matrix for eps.
  covStruct sigmaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nSigmaPar = nEps;

  // Set the diagonal elements for the current value for sigma.
  valarray<double> sigmaMinRep( nSigmaPar );
  sigmaMinRep[0] = 0.25;


  //------------------------------------------------------------
  // Construct the population level Pred model.
  //------------------------------------------------------------

  PopPredModel model(
    predEvaluator,
    predEvaluatorAD,
    predEvaluatorADAD,
    nTheta,
    thetaLow,
    thetaUp,
    thetaCurr,
    nEta,
    etaCurr,
    nEps,
    omegaStruct,
    omegaMinRep,
    sigmaStruct,
    sigmaMinRep );


  //------------------------------------------------------------
  // Get information related to the individual.
  //------------------------------------------------------------

  // Get the number elements in the population parameter.
  int nPopPar = model.getNPopPar();

  // Get the number of observations for this individual.
  int iCurr = 0;
  int nY_i = predEvaluator.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare various quantities for the test.
  //------------------------------------------------------------

  // Get the current value for the population parameter.
  valarray<double> alphaCurr( nPopPar );
  model.getPopPar( alphaCurr );

  valarray<double> dataMean              ( nY_i );
  valarray<double> dataMean_popPar       ( nY_i * nPopPar );
  valarray<double> dataVariance          ( nY_i * nY_i );
  valarray<double> dataVariance_popPar   ( nY_i * nY_i * nPopPar );
  valarray<double> dataVarianceInv       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_popPar( nY_i * nY_i * nPopPar );

  bool ok;

  // Evaluate these quantities at the current population and 
  // individual parameter values, which are set when the model
  // is constructed.
  model.dataMean                   ( dataMean );
  model.dataVariance               ( dataVariance );
  model.dataVarianceInv            ( dataVarianceInv );
  ok = model.dataMean_popPar       ( dataMean_popPar );
  ok = model.dataVariance_popPar   ( dataVariance_popPar );
  ok = model.dataVarianceInv_popPar( dataVarianceInv_popPar );

  valarray<double> popParLow ( nPopPar );
  valarray<double> popParUp  ( nPopPar );
  valarray<double> popParStep( nPopPar );

  // Get the limits for the population parameters.
  model.getPopParLimits( popParLow, popParUp );

  // Get the step sizes for the population parameters.
  model.getPopParStep( popParStep );


  //------------------------------------------------------------
  // Prepare known values related to the population parameter.
  //------------------------------------------------------------

  valarray<double> omegaCurr( nEta * nEta );

  // Create a covariance matrix that is equal to the one that the
  // PopPredModel is maintaining internally.
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

  valarray<double> sigmaCurr( nEps * nEps );

  // Create a covariance matrix that is equal to the one that the
  // PopPredModel is maintaining internally.
  DiagCov sigma( nEps );
  sigma.expandCovMinRep( sigmaMinRep, sigmaCurr );
  sigma.setCov( sigmaCurr );

  valarray<double> sigmaKnown         ( nEps * nEps );
  valarray<double> sigmaParKnown      ( nSigmaPar );
  valarray<double> sigmaParLowKnown   ( nSigmaPar );
  valarray<double> sigmaParUpKnown    ( nSigmaPar );
  valarray<double> sigma_sigmaParKnown( nEps * nEps * nSigmaPar );

  // Get known values for sigma and its derivative.
  sigma.cov    ( sigmaKnown );
  sigma.cov_par( sigma_sigmaParKnown );

  // Get known values for sigma's parameter and its limits.
  sigma.calcPar     ( sigmaCurr,        sigmaParKnown );
  sigma.getParLimits( sigmaParLowKnown, sigmaParUpKnown );

  // The population parameter is composed of the parameters that are
  // optimized over when performing population level estimation,
  //
  //                -          -
  //               |   theta    |
  //               |            |
  //     alpha  =  |  omegaPar  |  .
  //               |            |
  //               |  sigmaPar  |
  //                -          -
  //
  int nPopParKnown = nTheta + nOmegaPar + nSigmaPar;

  int thetaOffsetInPopPar    = 0;
  int omegaParOffsetInPopPar = nTheta;
  int sigmaParOffsetInPopPar = nTheta + nOmegaPar;

  valarray<double> alphaCurrKnown( nPopParKnown );

  // Set the known value for the population parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    alphaCurrKnown[k + thetaOffsetInPopPar] = thetaCurr[k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    alphaCurrKnown[k + omegaParOffsetInPopPar] = omegaParKnown[k];
  }
  for ( k = 0; k < nSigmaPar; k++ )
  {
    alphaCurrKnown[k + sigmaParOffsetInPopPar] = sigmaParKnown[k];
  }


  //------------------------------------------------------------
  // Prepare known values related to the model functions.
  //------------------------------------------------------------

  valarray<double> dataMeanKnown              ( nY_i );
  valarray<double> dataMean_popParKnown       ( nY_i * nPopPar );
  valarray<double> dataVarianceKnown          ( nY_i * nY_i );
  valarray<double> dataVariance_popParKnown   ( nY_i * nY_i * nPopPar );
  valarray<double> dataVarianceInvKnown       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_popParKnown( nY_i * nY_i * nPopPar );

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
  //                            ds * exp[ - theta    * time    ]
  //                                             (0)       (j)
  //    f    ( alpha, b  )  =  ----------------------------------  .
  //     i(j)          i               [ theta    * w ]
  //                                          (1)
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

  // Calculate the known value for the derivative of the data mean
  // with respect to the population parameter.
  dataMean_popParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)
    //     d       f    ( alpha, b  )  =  - time     f    ( alpha, b  )  .
    //      theta   i(j)          i             (j)   i(j)          i
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - timeStep * j * dataMean[j];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                     - wt  f    ( alpha, b  )
    //      (1)                                   i(j)          i
    //     d       f    ( alpha, b  )  =  --------------------------  .
    //      theta   i(j)          i  
    //                                     [ theta    * wt ]
    //                                            (1)
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - wt * dataMean[j] /
      ( thetaCurr[1] * wt );
  }

  // Calculate the known value for the variance of the data.
  dataVarianceKnown = 0.0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the values for the diagonal elements of the data variance:
    //
    //                                                                 2
    //     R      ( alpha, b  )  =  sigma       [ f    ( alpha, b  ) ]   .
    //      i(j,j)          i            (0,0)     i(j)          i
    //
    dataVarianceKnown[j + j * nY_i] = sigmaKnown[0 + 0 * nEps] *
      dataMean[j] * dataMean[j];
  }

  // Calculate the known value for the derivative of the variance
  // of the data with respect to the population parameter.
  dataVariance_popParKnown = 0.0;
  int nRow = nY_i * nY_i;
  int row;
  for ( k = 0; k < nTheta; k++ )
  {
    for ( j = 0; j < nY_i; j++ )
    {
      // Set the known values for this column,
      //
      //      (k)
      //     d       R      ( alpha, b  )
      //      theta   i(j,j)          i
      //
      //                                                (k)               
      //         =  2  sigma       f    ( alpha, b  )  d       f    ( alpha, b  )  .
      //                    (0,0)   i(j)          i     theta   i(j)          i
      //
      row = j * nY_i + j;
      col = k + thetaOffsetInPopPar;
    
      dataVariance_popParKnown[row + col * nRow] = 
        2 * sigmaKnown[0 + 0 * nEps] * 
        dataMean_popPar[j + k * nY_i] * dataMean[j];
    }
  }
  int nSigma_sigmaParRow = nEps * nEps;
  int sigma_sigmaParRow;
  int sigma_sigmaParCol;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (k)
    //     d          R      ( alpha, b  )  =  
    //      sigmaPar   i(j,j)          i
    //
    //             -                    -  2   
    //            |                      |     (k)               
    //         =  |  f    ( alpha, b  )  |    d         sigma     ( sigmaPar )  .
    //            |   i(j)          i    |     sigmaPar      (k,k)
    //             -                    -    
    //
    row = j * nY_i + j;
    col = k + sigmaParOffsetInPopPar;

    sigma_sigmaParRow = k * nEps + k;
    sigma_sigmaParCol = k;
    
    dataVariance_popParKnown[row + col * nRow] = 
      dataMean[j] * dataMean[j] *
      sigma_sigmaParKnown[sigma_sigmaParRow + sigma_sigmaParCol * nSigma_sigmaParRow];
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
  dataVarianceInv_popParKnown = AkronBtimesC(
    dataVarianceInvKnown,     nY_i,
    dataVarianceInvKnown,     nY_i,
    dataVariance_popParKnown, nPopPar );
  dataVarianceInv_popParKnown *= -1.0;


  //------------------------------------------------------------
  // Prepare known values related to SPK estimation.
  //------------------------------------------------------------

  valarray<double> popParLowKnown ( nPopPar );
  valarray<double> popParUpKnown  ( nPopPar );

  // Set the known limits for the population parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    popParLowKnown[k + thetaOffsetInPopPar] = thetaLow[k];
    popParUpKnown [k + thetaOffsetInPopPar] = thetaUp [k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    popParLowKnown[k + omegaParOffsetInPopPar] = omegaParLowKnown[k];
    popParUpKnown [k + omegaParOffsetInPopPar] = omegaParUpKnown [k];
  }
  for ( k = 0; k < nSigmaPar; k++ )
  {
    popParLowKnown[k + sigmaParOffsetInPopPar] = sigmaParLowKnown[k];
    popParUpKnown [k + sigmaParOffsetInPopPar] = sigmaParUpKnown [k];
  }
  
  // Since the step sizes may change as PopPredModel evolves, only 
  // check that they are positive.
  for ( k = 0; k < nPopPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The step size for a population parameter element is invalid.",
      popParStep[k] != 0.0 );
  }


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of population parameters is not correct.",
    nPopPar == nPopParKnown );

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of data values for this individual is not correct.",
    nY_i == nY_iKnown );

  compareToKnown( 
    alphaCurr,
    alphaCurrKnown,
    "alphaCurr",
    tol );

  compareToKnown( 
    dataMean,
    dataMeanKnown,
    "dataMean",
    tol );

  compareToKnown( 
    dataMean_popPar,
    dataMean_popParKnown,
    "dataMean_popPar",
    tol );

  compareToKnown( 
    dataVariance,
    dataVarianceKnown,
    "dataVariance",
    tol );

  compareToKnown( 
    dataVariance_popPar,
    dataVariance_popParKnown,
    "dataVariance_popPar",
    tol );

  compareToKnown( 
    dataVarianceInv,
    dataVarianceInvKnown,
    "dataVarianceInv",
    tol );

  compareToKnown( 
    dataVarianceInv_popPar,
    dataVarianceInv_popParKnown,
    "dataVarianceInv_popPar",
    tol );

  compareToKnown( 
    popParLow,
    popParLowKnown,
    "popParLow",
    tol );

  compareToKnown( 
    popParUp,
    popParUpKnown,
    "popParUp",
    tol );
}


/*************************************************************************
 *
 * Function: NoEta_OneExpF_AdditivePlusThetaDepY_Test
 *
 *
 * The goal of this test is to check that the population level
 * Pred model works for the case of the 
 *
 *     NoEta_OneExpF_AdditivePlusThetaDepY_Pred
 *
 * Pred block expression evaluator, for which analytical results 
 * can be calculated and which does not use any data values. 
 *
 *************************************************************************/

void PopPredModelTest::NoEta_OneExpF_AdditivePlusThetaDepY_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpfpred_poppredmodeltest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluators.
  //------------------------------------------------------------

  // Set the number of data values for this individual.
  int nY_iKnown = 5;

  NoEta_OneExpF_AdditivePlusThetaDepY_Pred< double > predEvaluator( nY_iKnown );
  NoEta_OneExpF_AdditivePlusThetaDepY_Pred< AD<double> > predEvaluatorAD( nY_iKnown );
  NoEta_OneExpF_AdditivePlusThetaDepY_Pred< AD< AD<double> > > predEvaluatorADAD( nY_iKnown );


  //------------------------------------------------------------
  // Prepare the variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of independent variables.
  const int nTheta = 3;
  const int nEta   = 0;
  const int nEps   = 2;

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

  // Set the current value for eta.
  valarray<double> etaCurr( nEta );


  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrices.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  covStruct omegaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nOmegaPar = nEta;

  // Set the diagonal elements for the current value for omega.
  valarray<double> omegaMinRep( nOmegaPar );

  // Set the structure of sigma, the covariance matrix for eps.
  covStruct sigmaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nSigmaPar = nEps;

  // Set the diagonal elements for the current value for sigma.
  valarray<double> sigmaMinRep( nSigmaPar );
  sigmaMinRep[0] = 0.25;
  sigmaMinRep[1] = 0.001;


  //------------------------------------------------------------
  // Construct the population level Pred model.
  //------------------------------------------------------------

  PopPredModel model(
    predEvaluator,
    predEvaluatorAD,
    predEvaluatorADAD,
    nTheta,
    thetaLow,
    thetaUp,
    thetaCurr,
    nEta,
    etaCurr,
    nEps,
    omegaStruct,
    omegaMinRep,
    sigmaStruct,
    sigmaMinRep );


  //------------------------------------------------------------
  // Get information related to the individual.
  //------------------------------------------------------------

  // Get the number elements in the population parameter.
  int nPopPar = model.getNPopPar();

  // Get the number of observations for this individual.
  int iCurr = 0;
  int nY_i = predEvaluator.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare various quantities for the test.
  //------------------------------------------------------------

  // Get the current value for the population parameter.
  valarray<double> alphaCurr( nPopPar );
  model.getPopPar( alphaCurr );

  valarray<double> dataMean              ( nY_i );
  valarray<double> dataMean_popPar       ( nY_i * nPopPar );
  valarray<double> dataVariance          ( nY_i * nY_i );
  valarray<double> dataVariance_popPar   ( nY_i * nY_i * nPopPar );
  valarray<double> dataVarianceInv       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_popPar( nY_i * nY_i * nPopPar );

  bool ok;

  // Evaluate these quantities at the current population and 
  // individual parameter values, which are set when the model
  // is constructed.
  model.dataMean                   ( dataMean );
  model.dataVariance               ( dataVariance );
  model.dataVarianceInv            ( dataVarianceInv );
  ok = model.dataMean_popPar       ( dataMean_popPar );
  ok = model.dataVariance_popPar   ( dataVariance_popPar );
  ok = model.dataVarianceInv_popPar( dataVarianceInv_popPar );

  valarray<double> popParLow ( nPopPar );
  valarray<double> popParUp  ( nPopPar );
  valarray<double> popParStep( nPopPar );

  // Get the limits for the population parameters.
  model.getPopParLimits( popParLow, popParUp );

  // Get the step sizes for the population parameters.
  model.getPopParStep( popParStep );


  //------------------------------------------------------------
  // Prepare known values related to the population parameter.
  //------------------------------------------------------------

  valarray<double> omegaCurr( nEta * nEta );

  // Create a covariance matrix that is equal to the one that the
  // PopPredModel is maintaining internally.
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

  valarray<double> sigmaCurr( nEps * nEps );

  // Create a covariance matrix that is equal to the one that the
  // PopPredModel is maintaining internally.
  DiagCov sigma( nEps );
  sigma.expandCovMinRep( sigmaMinRep, sigmaCurr );
  sigma.setCov( sigmaCurr );

  valarray<double> sigmaKnown         ( nEps * nEps );
  valarray<double> sigmaParKnown      ( nSigmaPar );
  valarray<double> sigmaParLowKnown   ( nSigmaPar );
  valarray<double> sigmaParUpKnown    ( nSigmaPar );
  valarray<double> sigma_sigmaParKnown( nEps * nEps * nSigmaPar );

  // Get known values for sigma and its derivative.
  sigma.cov    ( sigmaKnown );
  sigma.cov_par( sigma_sigmaParKnown );

  // Get known values for sigma's parameter and its limits.
  sigma.calcPar     ( sigmaCurr,        sigmaParKnown );
  sigma.getParLimits( sigmaParLowKnown, sigmaParUpKnown );

  // The population parameter is composed of the parameters that are
  // optimized over when performing population level estimation,
  //
  //                -          -
  //               |   theta    |
  //               |            |
  //     alpha  =  |  omegaPar  |  .
  //               |            |
  //               |  sigmaPar  |
  //                -          -
  //
  int nPopParKnown = nTheta + nOmegaPar + nSigmaPar;

  int thetaOffsetInPopPar    = 0;
  int omegaParOffsetInPopPar = nTheta;
  int sigmaParOffsetInPopPar = nTheta + nOmegaPar;

  valarray<double> alphaCurrKnown( nPopParKnown );

  // Set the known value for the population parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    alphaCurrKnown[k + thetaOffsetInPopPar] = thetaCurr[k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    alphaCurrKnown[k + omegaParOffsetInPopPar] = omegaParKnown[k];
  }
  for ( k = 0; k < nSigmaPar; k++ )
  {
    alphaCurrKnown[k + sigmaParOffsetInPopPar] = sigmaParKnown[k];
  }


  //------------------------------------------------------------
  // Prepare known values related to the model functions.
  //------------------------------------------------------------

  valarray<double> dataMeanKnown              ( nY_i );
  valarray<double> dataMean_popParKnown       ( nY_i * nPopPar );
  valarray<double> dataVarianceKnown          ( nY_i * nY_i );
  valarray<double> dataVariance_popParKnown   ( nY_i * nY_i * nPopPar );
  valarray<double> dataVarianceInvKnown       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_popParKnown( nY_i * nY_i * nPopPar );

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
  //                            ds * exp[ - theta    * time    ]
  //                                             (0)       (j)
  //    f    ( alpha, b  )  =  ----------------------------------  .
  //     i(j)          i               [ theta    * w ]
  //                                          (1)
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

  // Calculate the known value for the derivative of the data mean
  // with respect to the population parameter.
  dataMean_popParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)
    //     d       f    ( alpha, b  )  =  - time     f    ( alpha, b  )  .
    //      theta   i(j)          i             (j)   i(j)          i
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - timeStep * j * dataMean[j];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                     - wt  f    ( alpha, b  )
    //      (1)                                   i(j)          i
    //     d       f    ( alpha, b  )  = ---------------------------  .
    //      theta   i(j)          i  
    //                                       [ theta    * wt ]
    //                                              (1)
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - wt * dataMean[j] /
      ( thetaCurr[1] * wt );
  }

  // Calculate the known value for the variance of the data.
  dataVarianceKnown = 0.0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the values for the diagonal elements of the data variance:
    //
    //                                                              2
    //     R      ( alpha, b  )  =  sigma       +  sigma       theta     .
    //      i(j,j)          i            (0,0)          (1,1)       (2)
    //
    dataVarianceKnown[j + j * nY_i] = sigmaKnown[0 + 0 * nEps] +
      sigmaKnown[1 + 1 * nEps] * thetaCurr[2] * thetaCurr[2];
  }

  // Calculate the known value for the derivative of the variance
  // of the data with respect to the population parameter.
  dataVariance_popParKnown = 0.0;
  int nRow = nY_i * nY_i;
  int row;
  k = 2;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (2)
    //     d       R      ( alpha, b  )  =  2  sigma       theta     .
    //      theta   i(j,j)          i               (1,1)       (2)
    //
    row = j * nY_i + j;
    col = k + thetaOffsetInPopPar;
  
    dataVariance_popParKnown[row + col * nRow] = 
      2.0 * sigmaKnown[1 + 1 * nEps] * thetaCurr[2];
  }
  int nSigma_sigmaParRow = nEps * nEps;
  int sigma_sigmaParRow;
  int sigma_sigmaParCol;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)                                 (0)                
    //     d          R      ( alpha, b  )  =  d         sigma     ( sigmaPar )  .
    //      sigmaPar   i(j,j)          i        sigmaPar      (0,0)
    //
    row = j * nY_i + j;
    col = k + sigmaParOffsetInPopPar;

    sigma_sigmaParRow = k * nEps + k;
    sigma_sigmaParCol = k;
    
    dataVariance_popParKnown[row + col * nRow] = 
      sigma_sigmaParKnown[sigma_sigmaParRow + sigma_sigmaParCol * nSigma_sigmaParRow];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (1)                                     2     (1)                
    //     d          R      ( alpha, b  )  =  theta     d         sigma     ( sigmaPar )  .
    //      sigmaPar   i(j,j)          i            (2)   sigmaPar      (1,1)
    //
    row = j * nY_i + j;
    col = k + sigmaParOffsetInPopPar;

    sigma_sigmaParRow = k * nEps + k;
    sigma_sigmaParCol = k;
    
    dataVariance_popParKnown[row + col * nRow] = 
      thetaCurr[2] * thetaCurr[2] *
      sigma_sigmaParKnown[sigma_sigmaParRow + sigma_sigmaParCol * nSigma_sigmaParRow];
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
  dataVarianceInv_popParKnown = AkronBtimesC(
    dataVarianceInvKnown,     nY_i,
    dataVarianceInvKnown,     nY_i,
    dataVariance_popParKnown, nPopPar );
  dataVarianceInv_popParKnown *= -1.0;


  //------------------------------------------------------------
  // Prepare known values related to SPK estimation.
  //------------------------------------------------------------

  valarray<double> popParLowKnown ( nPopPar );
  valarray<double> popParUpKnown  ( nPopPar );

  // Set the known limits for the population parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    popParLowKnown[k + thetaOffsetInPopPar] = thetaLow[k];
    popParUpKnown [k + thetaOffsetInPopPar] = thetaUp [k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    popParLowKnown[k + omegaParOffsetInPopPar] = omegaParLowKnown[k];
    popParUpKnown [k + omegaParOffsetInPopPar] = omegaParUpKnown [k];
  }
  for ( k = 0; k < nSigmaPar; k++ )
  {
    popParLowKnown[k + sigmaParOffsetInPopPar] = sigmaParLowKnown[k];
    popParUpKnown [k + sigmaParOffsetInPopPar] = sigmaParUpKnown [k];
  }
  
  // Since the step sizes may change as PopPredModel evolves, only 
  // check that they are positive.
  for ( k = 0; k < nPopPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The step size for a population parameter element is invalid.",
      popParStep[k] != 0.0 );
  }


  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of population parameters is not correct.",
    nPopPar == nPopParKnown );

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of data values for this individual is not correct.",
    nY_i == nY_iKnown );

  compareToKnown( 
    alphaCurr,
    alphaCurrKnown,
    "alphaCurr",
    tol );

  compareToKnown( 
    dataMean,
    dataMeanKnown,
    "dataMean",
    tol );

  compareToKnown( 
    dataMean_popPar,
    dataMean_popParKnown,
    "dataMean_popPar",
    tol );

  compareToKnown( 
    dataVariance,
    dataVarianceKnown,
    "dataVariance",
    tol );

  compareToKnown( 
    dataVariance_popPar,
    dataVariance_popParKnown,
    "dataVariance_popPar",
    tol );

  compareToKnown( 
    dataVarianceInv,
    dataVarianceInvKnown,
    "dataVarianceInv",
    tol );

  compareToKnown( 
    dataVarianceInv_popPar,
    dataVarianceInv_popParKnown,
    "dataVarianceInv_popPar",
    tol );

  compareToKnown( 
    popParLow,
    popParLowKnown,
    "popParLow",
    tol );

  compareToKnown( 
    popParUp,
    popParUpKnown,
    "popParUp",
    tol );
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

void PopPredModelTest::OneExpF_ModelBasedExpY_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpfpred_poppredmodeltest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluators.
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
  const int nEta   = 2;
  const int nEps   = 1;

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

  // Set the current value for eta.
  valarray<double> etaCurr( nEta );
  etaCurr[0] = -0.3;
  etaCurr[1] =  0.2;


  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrices.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  covStruct omegaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nOmegaPar = nEta;

  // Set the diagonal elements for the current value for omega.
  valarray<double> omegaMinRep( nOmegaPar );
  omegaMinRep[0] = 0.0001;
  omegaMinRep[1] = 0.02;

  // Set the structure of sigma, the covariance matrix for eps.
  covStruct sigmaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nSigmaPar = nEps;

  // Set the diagonal elements for the current value for sigma.
  valarray<double> sigmaMinRep( nSigmaPar );
  sigmaMinRep[0] = 0.25;


  //------------------------------------------------------------
  // Construct the population level Pred model.
  //------------------------------------------------------------

  PopPredModel model(
    predEvaluator,
    predEvaluatorAD,
    predEvaluatorADAD,
    nTheta,
    thetaLow,
    thetaUp,
    thetaCurr,
    nEta,
    etaCurr,
    nEps,
    omegaStruct,
    omegaMinRep,
    sigmaStruct,
    sigmaMinRep );


  //------------------------------------------------------------
  // Get information related to the individual.
  //------------------------------------------------------------

  // Get the number elements in the population parameter.
  int nPopPar = model.getNPopPar();

  // Get the number elements in the individual parameter.
  int nIndPar = model.getNIndPar();

  // Get the number of observations for this individual.
  int iCurr = 0;
  int nY_i = predEvaluator.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare various quantities for the test.
  //------------------------------------------------------------

  // Get the current value for the population parameter.
  valarray<double> alphaCurr( nPopPar );
  model.getPopPar( alphaCurr );

  // Get the current value for the individual parameter.
  valarray<double> bCurr( nIndPar );
  model.getIndPar( bCurr );

  valarray<double> dataMean       ( nY_i );
  valarray<double> dataMean_popPar( nY_i * nPopPar );
  valarray<double> dataMean_indPar( nY_i * nIndPar );

  valarray<double> dataVariance          ( nY_i * nY_i );
  valarray<double> dataVariance_popPar   ( nY_i * nY_i * nPopPar );
  valarray<double> dataVariance_indPar   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInv       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_popPar( nY_i * nY_i * nPopPar );
  valarray<double> dataVarianceInv_indPar( nY_i * nY_i * nIndPar );

  valarray<double> indParVariance          ( nIndPar * nIndPar );
  valarray<double> indParVariance_popPar   ( nIndPar * nIndPar * nPopPar );
  valarray<double> indParVarianceInv       ( nIndPar * nIndPar );
  valarray<double> indParVarianceInv_popPar( nIndPar * nIndPar * nPopPar );

  bool ok;

  // Evaluate these quantities at the current population and 
  // individual parameter values, which are set when the model
  // is constructed.
  model.dataMean         ( dataMean );
  model.dataVariance     ( dataVariance );
  model.dataVarianceInv  ( dataVarianceInv );
  model.indParVariance   ( indParVariance );
  model.indParVarianceInv( indParVarianceInv );

  ok = model.dataMean_popPar( dataMean_popPar );
  ok = model.dataMean_indPar( dataMean_indPar );

  ok = model.dataVariance_popPar   ( dataVariance_popPar );
  ok = model.dataVariance_indPar   ( dataVariance_indPar );
  ok = model.dataVarianceInv_popPar( dataVarianceInv_popPar );
  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );

  ok = model.indParVariance_popPar   ( indParVariance_popPar );
  ok = model.indParVarianceInv_popPar( indParVarianceInv_popPar );

  valarray<double> popParLow ( nPopPar );
  valarray<double> popParUp  ( nPopPar );
  valarray<double> popParStep( nPopPar );

  // Get the limits for the population parameters.
  model.getPopParLimits( popParLow, popParUp );

  // Get the step sizes for the population parameters.
  model.getPopParStep( popParStep );

  valarray<double> indParLow ( nIndPar );
  valarray<double> indParUp  ( nIndPar );
  valarray<double> indParStep( nIndPar );

  // Get the limits for the individual parameters.
  model.getIndParLimits( indParLow, indParUp );

  // Get the step sizes for the individual parameters.
  model.getIndParStep( indParStep );


  //------------------------------------------------------------
  // Prepare known values related to the population parameter.
  //------------------------------------------------------------

  valarray<double> omegaCurr( nEta * nEta );

  // Create a covariance matrix that is equal to the one that the
  // PopPredModel is maintaining internally.
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

  valarray<double> sigmaCurr( nEps * nEps );

  // Create a covariance matrix that is equal to the one that the
  // PopPredModel is maintaining internally.
  DiagCov sigma( nEps );
  sigma.expandCovMinRep( sigmaMinRep, sigmaCurr );
  sigma.setCov( sigmaCurr );

  valarray<double> sigmaKnown         ( nEps * nEps );
  valarray<double> sigmaParKnown      ( nSigmaPar );
  valarray<double> sigmaParLowKnown   ( nSigmaPar );
  valarray<double> sigmaParUpKnown    ( nSigmaPar );
  valarray<double> sigma_sigmaParKnown( nEps * nEps * nSigmaPar );

  // Get known values for sigma and its derivative.
  sigma.cov    ( sigmaKnown );
  sigma.cov_par( sigma_sigmaParKnown );

  // Get known values for sigma's parameter and its limits.
  sigma.calcPar     ( sigmaCurr,        sigmaParKnown );
  sigma.getParLimits( sigmaParLowKnown, sigmaParUpKnown );

  // The population parameter is composed of the parameters that are
  // optimized over when performing population level estimation,
  //
  //                -          -
  //               |   theta    |
  //               |            |
  //     alpha  =  |  omegaPar  |  .
  //               |            |
  //               |  sigmaPar  |
  //                -          -
  //
  int nPopParKnown = nTheta + nOmegaPar + nSigmaPar;

  int thetaOffsetInPopPar    = 0;
  int omegaParOffsetInPopPar = nTheta;
  int sigmaParOffsetInPopPar = nTheta + nOmegaPar;

  valarray<double> alphaCurrKnown( nPopParKnown );

  // Set the known value for the population parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    alphaCurrKnown[k + thetaOffsetInPopPar] = thetaCurr[k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    alphaCurrKnown[k + omegaParOffsetInPopPar] = omegaParKnown[k];
  }
  for ( k = 0; k < nSigmaPar; k++ )
  {
    alphaCurrKnown[k + sigmaParOffsetInPopPar] = sigmaParKnown[k];
  }


  //------------------------------------------------------------
  // Prepare known values related to the individual parameter.
  //------------------------------------------------------------

  // The individual parameter is the parameter that is optimized
  // over when performing individual level estimation,
  //
  //     b   =  eta  .
  //      i
  //
  int nIndParKnown = nEta;

  valarray<double> bCurrKnown( nIndParKnown );

  // Set the known value for the individual parameter.
  bCurrKnown = etaCurr;


  //------------------------------------------------------------
  // Prepare known values related to the model functions.
  //------------------------------------------------------------

  valarray<double> dataMeanKnown       ( nY_i );
  valarray<double> dataMean_popParKnown( nY_i * nPopPar );
  valarray<double> dataMean_indParKnown( nY_i * nIndPar );

  valarray<double> dataVarianceKnown          ( nY_i * nY_i );
  valarray<double> dataVariance_popParKnown   ( nY_i * nY_i * nPopPar );
  valarray<double> dataVariance_indParKnown   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInvKnown       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_popParKnown( nY_i * nY_i * nPopPar );
  valarray<double> dataVarianceInv_indParKnown( nY_i * nY_i * nIndPar );

  valarray<double> indParVarianceKnown          ( nIndPar * nIndPar );
  valarray<double> indParVariance_popParKnown   ( nIndPar * nIndPar * nPopPar );
  valarray<double> indParVarianceInvKnown       ( nIndPar * nIndPar );
  valarray<double> indParVarianceInv_popParKnown( nIndPar * nIndPar * nPopPar );

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
  //                                      -                                     -   
  //                                     |                                       |
  //                            ds * exp |  - [ theta    + eta    ]  *  time     |
  //                                     |           (0)      (0)           (j)  |
  //                                      -                                     -   
  //    f    ( alpha, b  )  =  ----------------------------------------------------  .
  //     i(j)          i
  //                                      [ theta    * w   +  eta    ]
  //                                             (1)             (1)
  //
  for ( j = 0; j < nY_i; j++ )
  {
    time = timeStep * j;
    ds = dose * wt;
    w = wt;

    ke = thetaCurr[0] + etaCurr[0];
    cl = thetaCurr[1] * w + etaCurr[1];
    d = std::exp(-ke * time);
    e = cl;
    dataMeanKnown[j] = ds * d / e;
  }

  int col;

  // Calculate the known value for the derivative of the data mean
  // with respect to the population parameter.
  dataMean_popParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)
    //     d       f    ( alpha, b  )  =  - time     f    ( alpha, b  )  .
    //      theta   i(j)          i             (j)   i(j)          i
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - timeStep * j * dataMean[j];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                       - wt  f    ( alpha, b  )
    //      (1)                                     i(j)          i
    //     d       f    ( alpha, b  )  =  -------------------------------  .
    //      theta   i(j)          i  
    //                                     [ theta    * wt   +  eta    ]
    //                                            (1)              (1)
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - wt * dataMean[j] /
      ( thetaCurr[1] * wt + etaCurr[1] );
  }

  // Calculate the known value for the derivative of the data mean
  // with respect to the individual parameter.
  dataMean_indParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)
    //     d     f    ( alpha, b  )  =  - time     f    ( alpha, b  )  .
    //      eta   i(j)          i             (j)   i(j)          i
    //
    dataMean_indParKnown[j + k * nY_i] = - timeStep * j * dataMean[j];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                       -  f    ( alpha, b  )
    //      (1)                                  i(j)          i
    //     d     f    ( alpha, b  )  =  -------------------------------  .
    //      eta   i(j)          i  
    //                                   [ theta    * wt   +  eta    ]
    //                                          (1)              (1)
    //
    dataMean_indParKnown[j + k * nY_i] = - dataMean[j] /
      ( thetaCurr[1] * wt + etaCurr[1] );
  }

  // Calculate the known value for the variance of the data.
  dataVarianceKnown = 0.0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the values for the diagonal elements of the data variance:
    //
    //                                                                 2
    //     R      ( alpha, b  )  =  sigma       [ f    ( alpha, b  ) ]   .
    //      i(j,j)          i            (0,0)     i(j)          i
    //
    dataVarianceKnown[j + j * nY_i] = sigmaKnown[0 + 0 * nEps] *
      dataMean[j] * dataMean[j];
  }

  // Calculate the known value for the derivative of the variance
  // of the data with respect to the population parameter.
  dataVariance_popParKnown = 0.0;
  int nRow = nY_i * nY_i;
  int row;
  for ( k = 0; k < nTheta; k++ )
  {
    for ( j = 0; j < nY_i; j++ )
    {
      // Set the known values for this column,
      //
      //      (k)
      //     d       R      ( alpha, b  )
      //      theta   i(j,j)          i
      //
      //                                                (k)               
      //         =  2  sigma       f    ( alpha, b  )  d       f    ( alpha, b  )  .
      //                    (0,0)   i(j)          i     theta   i(j)          i
      //
      row = j * nY_i + j;
      col = k + thetaOffsetInPopPar;
    
      dataVariance_popParKnown[row + col * nRow] = 
        2 * sigmaKnown[0 + 0 * nEps] * 
        dataMean_popPar[j + k * nY_i] * dataMean[j];
    }
  }
  int nSigma_sigmaParRow = nEps * nEps;
  int sigma_sigmaParRow;
  int sigma_sigmaParCol;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (k)
    //     d          R      ( alpha, b  )  =  
    //      sigmaPar   i(j,j)          i
    //
    //             -                    -  2   
    //            |                      |     (k)               
    //         =  |  f    ( alpha, b  )  |    d         sigma     ( sigmaPar )  .
    //            |   i(j)          i    |     sigmaPar      (k,k)
    //             -                    -    
    //
    row = j * nY_i + j;
    col = k + sigmaParOffsetInPopPar;

    sigma_sigmaParRow = k * nEps + k;
    sigma_sigmaParCol = k;
    
    dataVariance_popParKnown[row + col * nRow] = 
      dataMean[j] * dataMean[j] *
      sigma_sigmaParKnown[sigma_sigmaParRow + sigma_sigmaParCol * nSigma_sigmaParRow];
  }

  // Calculate the known value for the derivative of the variance
  // of the data with respect to the individual parameter.
  dataVariance_indParKnown = 0.0;
  nRow = nY_i * nY_i;
  for ( k = 0; k < nEta; k++ )
  {
    for ( j = 0; j < nY_i; j++ )
    {
      // Set the known values for this column,
      //
      //      (k)
      //     d     R      ( alpha, b  )
      //      eta   i(j,j)          i
      //
      //                                                (k)               
      //         =  2  sigma       f    ( alpha, b  )  d     f    ( alpha, b  )  .
      //                    (0,0)   i(j)          i     eta   i(j)          i
      //
      row = j * nY_i + j;
      col = k;
    
      dataVariance_indParKnown[row + col * nRow] = 
        2 * sigmaKnown[0 + 0 * nEps] * 
        dataMean_indPar[j + k * nY_i] * dataMean[j];
    }
  }

  // Calculate the known value for the inverse of the variance
  // of the data.
  dataVarianceInvKnown = inverse( dataVarianceKnown, nY_i );

  // Calculate the known value for the derivative with respect
  // to the population parameter of the inverse of the variance
  // of the data using Lemma 10 of B. M. Bell, "Approximating
  // the marginal likelihood estimate for models with random
  // parameters", Applied Mathematics and Computation, 119 
  // (2001), pp. 57-73, which states that
  //
  //          -1               -1              -1
  //     d   A  ( x )  =  - [ A  ( x )  kron  A  ( x ) ]  d   A ( x )  .
  //      x                                                x
  //
  dataVarianceInv_popParKnown = AkronBtimesC(
    dataVarianceInvKnown,     nY_i,
    dataVarianceInvKnown,     nY_i,
    dataVariance_popParKnown, nPopPar );
  dataVarianceInv_popParKnown *= -1.0;

  // Calculate the known value for the derivative with respect
  // to the individual parameter of the inverse of the variance
  // of the data using Lemma 10 of B. M. Bell, "Approximating
  // the marginal likelihood estimate for models with random
  // parameters", Applied Mathematics and Computation, 119 
  // (2001), pp. 57-73, which states that
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

  // Set the known value for the variance of the individual parameter,
  //
  //     D ( alpha )  =  omega( omegaPar )  .
  //
  indParVarianceKnown = omegaKnown;

  // Calculate the known value for the derivative with respect
  // to the population parameter of the variance of the individual
  // parameter,
  //
  //     d       D ( alpha )
  //      alpha
  //
  //             -                                                              -
  //            |  0, 0, ... , 0,                                 0, 0, ... , 0  |
  //            |                                                                |
  //         =  |  0, 0, ... , 0,  d          omega ( omega  ) ,  0, 0, ... , 0  |  ,
  //            |                   omegaPar                                     |  
  //            |  0, 0, ... , 0,                                 0, 0, ... , 0  |
  //             -                                                              -
  //
  indParVariance_popParKnown = 0.0;
  nRow = nIndPar * nIndPar;
  for ( k = 0; k < nOmegaPar; k++ )
  {
    for ( j = 0; j < nRow; j++ )
    {
      row = j;
      col = k + omegaParOffsetInPopPar;

      indParVariance_popParKnown[row + col * nRow] = omega_omegaParKnown[j + k * nRow];
    }
  }

  // Calculate the known value for the inverse of the variance
  // of the individual parameter.
  indParVarianceInvKnown = inverse( indParVarianceKnown, nIndPar );

  // Calculate the known value for the derivative with respect
  // to the population parameter of the inverse of the variance
  // of the individual parameter using Lemma 10 of B. M. Bell,
  // "Approximating the marginal likelihood estimate for models
  // with random parameters", Applied Mathematics and Computation,
  // 119 (2001), pp. 57-73, which states that
  //
  //          -1               -1              -1
  //     d   A  ( x )  =  - [ A  ( x )  kron  A  ( x ) ]  d   A ( x )  .
  //      x                                                x
  //
  indParVarianceInv_popParKnown = AkronBtimesC(
    indParVarianceInvKnown,     nIndPar,
    indParVarianceInvKnown,     nIndPar,
    indParVariance_popParKnown, nPopPar );
  indParVarianceInv_popParKnown *= -1.0;


  //------------------------------------------------------------
  // Prepare known values related to SPK estimation.
  //------------------------------------------------------------

  valarray<double> popParLowKnown ( nPopPar );
  valarray<double> popParUpKnown  ( nPopPar );

  // Set the known limits for the population parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    popParLowKnown[k + thetaOffsetInPopPar] = thetaLow[k];
    popParUpKnown [k + thetaOffsetInPopPar] = thetaUp [k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    popParLowKnown[k + omegaParOffsetInPopPar] = omegaParLowKnown[k];
    popParUpKnown [k + omegaParOffsetInPopPar] = omegaParUpKnown [k];
  }
  for ( k = 0; k < nSigmaPar; k++ )
  {
    popParLowKnown[k + sigmaParOffsetInPopPar] = sigmaParLowKnown[k];
    popParUpKnown [k + sigmaParOffsetInPopPar] = sigmaParUpKnown [k];
  }
  
  // Since the step sizes may change as PopPredModel evolves, only 
  // check that they are positive.
  for ( k = 0; k < nPopPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The step size for a population parameter element is invalid.",
      popParStep[k] != 0.0 );
  }

  // Since the limits may change as PopPredModel evolves, only check
  // that the lower limits are less than or equal to the upper.
  for ( k = 0; k < nIndPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The limits for an individual parameter element are invalid.",
      indParLow[k] <= indParUp [k] );
  }
  
  // Since the step sizes may change as PopPredModel evolves, only 
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
    "The number of population parameters is not correct.",
    nPopPar == nPopParKnown );

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of individual parameters is not correct.",
    nIndPar == nIndParKnown );

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of data values for this individual is not correct.",
    nY_i == nY_iKnown );

  compareToKnown( 
    alphaCurr,
    alphaCurrKnown,
    "alphaCurr",
    tol );

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
    dataMean_popPar,
    dataMean_popParKnown,
    "dataMean_popPar",
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
    dataVariance_popPar,
    dataVariance_popParKnown,
    "dataVariance_popPar",
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
    dataVarianceInv_popPar,
    dataVarianceInv_popParKnown,
    "dataVarianceInv_popPar",
    tol );

  compareToKnown( 
    dataVarianceInv_indPar,
    dataVarianceInv_indParKnown,
    "dataVarianceInv_indPar",
    tol );

  compareToKnown( 
    indParVariance,
    indParVarianceKnown,
    "indParVariance",
    tol );

  compareToKnown( 
    indParVariance_popPar,
    indParVariance_popParKnown,
    "indParVariance_popPar",
    tol );

  compareToKnown( 
    indParVarianceInv,
    indParVarianceInvKnown,
    "indParVarianceInv",
    tol );

  compareToKnown( 
    indParVarianceInv_popPar,
    indParVarianceInv_popParKnown,
    "indParVarianceInv_popPar",
    tol );
}


/*************************************************************************
 *
 * Function: OneExpF_AdditivePlusThetaDepY_Test
 *
 *
 * The goal of this test is to check that the population level
 * Pred model works for the case of the 
 *
 *     OneExpF_AdditivePlusThetaDepY_Pred
 *
 * Pred block expression evaluator, for which analytical results 
 * can be calculated and which does not use any data values. 
 *
 *************************************************************************/

void PopPredModelTest::OneExpF_AdditivePlusThetaDepY_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpfpred_poppredmodeltest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluators.
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
  const int nEps   = 2;

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

  // Set the current value for eta.
  valarray<double> etaCurr( nEta );
  etaCurr[0] = -0.3;
  etaCurr[1] =  0.2;


  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrices.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  covStruct omegaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nOmegaPar = nEta;

  // Set the diagonal elements for the current value for omega.
  valarray<double> omegaMinRep( nOmegaPar );
  omegaMinRep[0] = 0.0001;
  omegaMinRep[1] = 0.02;

  // Set the bool of fixed elements for omega.
  valarray<bool> omegaMinRepFixed( nOmegaPar );
  omegaMinRepFixed = false;

  // Set the structure of sigma, the covariance matrix for eps.
  covStruct sigmaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nSigmaPar = nEps;

  // Set the diagonal elements for the current value for sigma.
  valarray<double> sigmaMinRep( nSigmaPar );
  sigmaMinRep[0] = 0.25;
  sigmaMinRep[1] = 0.001;

  // Set the bool of fixed elements for sigma.
  valarray<bool> sigmaMinRepFixed( nSigmaPar );
  sigmaMinRepFixed = false;

  // block structure info:  only constructor is implemented - info not used
  int nOmegaBlocks = 2;
  valarray<covStruct>  omegaBlockStruct( nOmegaBlocks );
  omegaBlockStruct[0] = DIAGONAL;
  omegaBlockStruct[1] = FULL;
  valarray<int>     omegaBlockDims( nOmegaBlocks );
  omegaBlockDims[0] = 3;
  omegaBlockDims[1] = 2;
  valarray<bool>    omegaBlockSameAsPrev( nOmegaBlocks );
  omegaBlockSameAsPrev = false;

  int nsigmaBlocks = 1;
  valarray<covStruct>  sigmaBlockStruct( nsigmaBlocks );
  omegaBlockStruct[0] = DIAGONAL;
  valarray<int>     sigmaBlockDims( nsigmaBlocks );
  omegaBlockDims[0] = 1;
  valarray<bool>    sigmaBlockSameAsPrev( nsigmaBlocks );
  omegaBlockSameAsPrev = false;


  //------------------------------------------------------------
  // Construct the population level Pred model.
  //------------------------------------------------------------

  PopPredModel model(
    predEvaluator,
    predEvaluatorAD,
    predEvaluatorADAD,
    nTheta,
    thetaLow,
    thetaUp,
    thetaCurr,
    nEta,
    etaCurr,
    nEps,
    omegaStruct,
    omegaMinRep,
    omegaMinRepFixed,
    omegaBlockStruct,
    omegaBlockDims,
    omegaBlockSameAsPrev,
    sigmaStruct,
    sigmaMinRep,
    sigmaMinRepFixed,
    sigmaBlockStruct,
    sigmaBlockDims,
    sigmaBlockSameAsPrev );



  //------------------------------------------------------------
  // Get information related to the individual.
  //------------------------------------------------------------

  // Get the number elements in the population parameter.
  int nPopPar = model.getNPopPar();

  // Get the number elements in the individual parameter.
  int nIndPar = model.getNIndPar();

  // Get the number of observations for this individual.
  int iCurr = 0;
  int nY_i = predEvaluator.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare various quantities for the test.
  //------------------------------------------------------------

  // Get the current value for the population parameter.
  valarray<double> alphaCurr( nPopPar );
  model.getPopPar( alphaCurr );

  // Get the current value for the individual parameter.
  valarray<double> bCurr( nIndPar );
  model.getIndPar( bCurr );

  valarray<double> dataMean       ( nY_i );
  valarray<double> dataMean_popPar( nY_i * nPopPar );
  valarray<double> dataMean_indPar( nY_i * nIndPar );

  valarray<double> dataVariance          ( nY_i * nY_i );
  valarray<double> dataVariance_popPar   ( nY_i * nY_i * nPopPar );
  valarray<double> dataVariance_indPar   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInv       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_popPar( nY_i * nY_i * nPopPar );
  valarray<double> dataVarianceInv_indPar( nY_i * nY_i * nIndPar );

  valarray<double> indParVariance          ( nIndPar * nIndPar );
  valarray<double> indParVariance_popPar   ( nIndPar * nIndPar * nPopPar );
  valarray<double> indParVarianceInv       ( nIndPar * nIndPar );
  valarray<double> indParVarianceInv_popPar( nIndPar * nIndPar * nPopPar );

  bool ok;

  // Evaluate these quantities at the current population and 
  // individual parameter values, which are set when the model
  // is constructed.
  model.dataMean         ( dataMean );
  model.dataVariance     ( dataVariance );
  model.dataVarianceInv  ( dataVarianceInv );
  model.indParVariance   ( indParVariance );
  model.indParVarianceInv( indParVarianceInv );

  ok = model.dataMean_popPar( dataMean_popPar );
  ok = model.dataMean_indPar( dataMean_indPar );

  ok = model.dataVariance_popPar   ( dataVariance_popPar );
  ok = model.dataVariance_indPar   ( dataVariance_indPar );
  ok = model.dataVarianceInv_popPar( dataVarianceInv_popPar );
  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );

  ok = model.indParVariance_popPar   ( indParVariance_popPar );
  ok = model.indParVarianceInv_popPar( indParVarianceInv_popPar );

  valarray<double> popParLow ( nPopPar );
  valarray<double> popParUp  ( nPopPar );
  valarray<double> popParStep( nPopPar );

  // Get the limits for the population parameters.
  model.getPopParLimits( popParLow, popParUp );

  // Get the step sizes for the population parameters.
  model.getPopParStep( popParStep );

  valarray<double> indParLow ( nIndPar );
  valarray<double> indParUp  ( nIndPar );
  valarray<double> indParStep( nIndPar );

  // Get the limits for the individual parameters.
  model.getIndParLimits( indParLow, indParUp );

  // Get the step sizes for the individual parameters.
  model.getIndParStep( indParStep );


  //------------------------------------------------------------
  // Prepare known values related to the population parameter.
  //------------------------------------------------------------

  valarray<double> omegaCurr( nEta * nEta );

  // Create a covariance matrix that is equal to the one that the
  // PopPredModel is maintaining internally.
  DiagCov omega( nEta, omegaMinRepFixed );
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

  valarray<double> sigmaCurr( nEps * nEps );

  // Create a covariance matrix that is equal to the one that the
  // PopPredModel is maintaining internally.
  DiagCov sigma( nEps );
  sigma.expandCovMinRep( sigmaMinRep, sigmaCurr );
  sigma.setCov( sigmaCurr );

  valarray<double> sigmaKnown         ( nEps * nEps );
  valarray<double> sigmaParKnown      ( nSigmaPar );
  valarray<double> sigmaParLowKnown   ( nSigmaPar );
  valarray<double> sigmaParUpKnown    ( nSigmaPar );
  valarray<double> sigma_sigmaParKnown( nEps * nEps * nSigmaPar );

  // Get known values for sigma and its derivative.
  sigma.cov    ( sigmaKnown );
  sigma.cov_par( sigma_sigmaParKnown );

  // Get known values for sigma's parameter and its limits.
  sigma.calcPar     ( sigmaCurr,        sigmaParKnown );
  sigma.getParLimits( sigmaParLowKnown, sigmaParUpKnown );

  // The population parameter is composed of the parameters that are
  // optimized over when performing population level estimation,
  //
  //                -          -
  //               |   theta    |
  //               |            |
  //     alpha  =  |  omegaPar  |  .
  //               |            |
  //               |  sigmaPar  |
  //                -          -
  //
  int nPopParKnown = nTheta + nOmegaPar + nSigmaPar;

  int thetaOffsetInPopPar    = 0;
  int omegaParOffsetInPopPar = nTheta;
  int sigmaParOffsetInPopPar = nTheta + nOmegaPar;

  valarray<double> alphaCurrKnown( nPopParKnown );

  // Set the known value for the population parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    alphaCurrKnown[k + thetaOffsetInPopPar] = thetaCurr[k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    alphaCurrKnown[k + omegaParOffsetInPopPar] = omegaParKnown[k];
  }
  for ( k = 0; k < nSigmaPar; k++ )
  {
    alphaCurrKnown[k + sigmaParOffsetInPopPar] = sigmaParKnown[k];
  }


  //------------------------------------------------------------
  // Prepare known values related to the individual parameter.
  //------------------------------------------------------------

  // The individual parameter is the parameter that is optimized
  // over when performing individual level estimation,
  //
  //     b   =  eta  .
  //      i
  //
  int nIndParKnown = nEta;

  valarray<double> bCurrKnown( nIndParKnown );

  // Set the known value for the individual parameter.
  bCurrKnown = etaCurr;


  //------------------------------------------------------------
  // Prepare known values related to the model functions.
  //------------------------------------------------------------

  valarray<double> dataMeanKnown       ( nY_i );
  valarray<double> dataMean_popParKnown( nY_i * nPopPar );
  valarray<double> dataMean_indParKnown( nY_i * nIndPar );

  valarray<double> dataVarianceKnown          ( nY_i * nY_i );
  valarray<double> dataVariance_popParKnown   ( nY_i * nY_i * nPopPar );
  valarray<double> dataVariance_indParKnown   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInvKnown       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_popParKnown( nY_i * nY_i * nPopPar );
  valarray<double> dataVarianceInv_indParKnown( nY_i * nY_i * nIndPar );

  valarray<double> indParVarianceKnown          ( nIndPar * nIndPar );
  valarray<double> indParVariance_popParKnown   ( nIndPar * nIndPar * nPopPar );
  valarray<double> indParVarianceInvKnown       ( nIndPar * nIndPar );
  valarray<double> indParVarianceInv_popParKnown( nIndPar * nIndPar * nPopPar );

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
  //                                      -                                     -   
  //                                     |                                       |
  //                            ds * exp |  - [ theta    + eta    ]  *  time     |
  //                                     |           (0)      (0)           (j)  |
  //                                      -                                     -   
  //    f    ( alpha, b  )  =  ----------------------------------------------------  .
  //     i(j)          i
  //                                      [ theta    * w   +  eta    ]
  //                                             (1)             (1)
  //
  for ( j = 0; j < nY_i; j++ )
  {
    time = timeStep * j;
    ds = dose * wt;
    w = wt;

    ke = thetaCurr[0] + etaCurr[0];
    cl = thetaCurr[1] * w + etaCurr[1];
    d = std::exp(-ke * time);
    e = cl;
    dataMeanKnown[j] = ds * d / e;
  }

  int col;

  // Calculate the known value for the derivative of the data mean
  // with respect to the population parameter.
  dataMean_popParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)
    //     d       f    ( alpha, b  )  =  - time     f    ( alpha, b  )  .
    //      theta   i(j)          i             (j)   i(j)          i
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - timeStep * j * dataMean[j];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                       - wt  f    ( alpha, b  )
    //      (1)                                     i(j)          i
    //     d       f    ( alpha, b  )  =  -------------------------------  .
    //      theta   i(j)          i  
    //                                     [ theta    * wt   +  eta    ]
    //                                            (1)              (1)
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - wt * dataMean[j] /
      ( thetaCurr[1] * wt + etaCurr[1] );
  }

  // Calculate the known value for the derivative of the data mean
  // with respect to the individual parameter.
  dataMean_indParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)
    //     d     f    ( alpha, b  )  =  - time     f    ( alpha, b  )  .
    //      eta   i(j)          i             (j)   i(j)          i
    //
    dataMean_indParKnown[j + k * nY_i] = - timeStep * j * dataMean[j];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                       -  f    ( alpha, b  )
    //      (1)                                  i(j)          i
    //     d     f    ( alpha, b  )  =  -------------------------------  .
    //      eta   i(j)          i  
    //                                   [ theta    * wt   +  eta    ]
    //                                          (1)              (1)
    //
    dataMean_indParKnown[j + k * nY_i] = - dataMean[j] /
      ( thetaCurr[1] * wt + etaCurr[1] );
  }

  // Calculate the known value for the variance of the data.
  dataVarianceKnown = 0.0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the values for the diagonal elements of the data variance:
    //
    //                                                              2
    //     R      ( alpha, b  )  =  sigma       +  sigma       theta     .
    //      i(j,j)          i            (0,0)          (1,1)       (2)
    //
    dataVarianceKnown[j + j * nY_i] = sigmaKnown[0 + 0 * nEps] +
      sigmaKnown[1 + 1 * nEps] * thetaCurr[2] * thetaCurr[2];
  }

  // Calculate the known value for the derivative of the variance
  // of the data with respect to the population parameter.
  dataVariance_popParKnown = 0.0;
  int nRow = nY_i * nY_i;
  int row;
  k = 2;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (2)
    //     d       R      ( alpha, b  )  =  2  sigma       theta     .
    //      theta   i(j,j)          i               (1,1)       (2)
    //
    row = j * nY_i + j;
    col = k + thetaOffsetInPopPar;
  
    dataVariance_popParKnown[row + col * nRow] = 
      2.0 * sigmaKnown[1 + 1 * nEps] * thetaCurr[2];
  }
  int nSigma_sigmaParRow = nEps * nEps;
  int sigma_sigmaParRow;
  int sigma_sigmaParCol;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)                                 (0)                
    //     d          R      ( alpha, b  )  =  d         sigma     ( sigmaPar )  .
    //      sigmaPar   i(j,j)          i        sigmaPar      (0,0)
    //
    row = j * nY_i + j;
    col = k + sigmaParOffsetInPopPar;

    sigma_sigmaParRow = k * nEps + k;
    sigma_sigmaParCol = k;
    
    dataVariance_popParKnown[row + col * nRow] = 
      sigma_sigmaParKnown[sigma_sigmaParRow + sigma_sigmaParCol * nSigma_sigmaParRow];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (1)                                     2     (1)                
    //     d          R      ( alpha, b  )  =  theta     d         sigma     ( sigmaPar )  .
    //      sigmaPar   i(j,j)          i            (2)   sigmaPar      (1,1)
    //
    row = j * nY_i + j;
    col = k + sigmaParOffsetInPopPar;

    sigma_sigmaParRow = k * nEps + k;
    sigma_sigmaParCol = k;
    
    dataVariance_popParKnown[row + col * nRow] = 
      thetaCurr[2] * thetaCurr[2] *
      sigma_sigmaParKnown[sigma_sigmaParRow + sigma_sigmaParCol * nSigma_sigmaParRow];
  }

  // Calculate the known value for the inverse of the variance
  // of the data.
  dataVarianceInvKnown = inverse( dataVarianceKnown, nY_i );

  // Calculate the known value for the derivative with respect
  // to the population parameter of the inverse of the variance
  // of the data using Lemma 10 of B. M. Bell, "Approximating
  // the marginal likelihood estimate for models with random
  // parameters", Applied Mathematics and Computation, 119 
  // (2001), pp. 57-73, which states that
  //
  //          -1               -1              -1
  //     d   A  ( x )  =  - [ A  ( x )  kron  A  ( x ) ]  d   A ( x )  .
  //      x                                                x
  //
  dataVarianceInv_popParKnown = AkronBtimesC(
    dataVarianceInvKnown,     nY_i,
    dataVarianceInvKnown,     nY_i,
    dataVariance_popParKnown, nPopPar );
  dataVarianceInv_popParKnown *= -1.0;

  // Calculate the known value for the derivative with respect
  // to the individual parameter of the inverse of the variance
  // of the data using Lemma 10 of B. M. Bell, "Approximating
  // the marginal likelihood estimate for models with random
  // parameters", Applied Mathematics and Computation, 119 
  // (2001), pp. 57-73, which states that
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

  // Set the known value for the variance of the individual parameter,
  //
  //     D ( alpha )  =  omega( omegaPar )  .
  //
  indParVarianceKnown = omegaKnown;

  // Calculate the known value for the derivative with respect
  // to the population parameter of the variance of the individual
  // parameter,
  //
  //     d       D ( alpha )
  //      alpha
  //
  //             -                                                              -
  //            |  0, 0, ... , 0,                                 0, 0, ... , 0  |
  //            |                                                                |
  //         =  |  0, 0, ... , 0,  d          omega ( omega  ) ,  0, 0, ... , 0  |  ,
  //            |                   omegaPar                                     |  
  //            |  0, 0, ... , 0,                                 0, 0, ... , 0  |
  //             -                                                              -
  //
  indParVariance_popParKnown = 0.0;
  nRow = nIndPar * nIndPar;
  for ( k = 0; k < nOmegaPar; k++ )
  {
    for ( j = 0; j < nRow; j++ )
    {
      row = j;
      col = k + omegaParOffsetInPopPar;

      indParVariance_popParKnown[row + col * nRow] = omega_omegaParKnown[j + k * nRow];
    }
  }

  // Calculate the known value for the inverse of the variance
  // of the individual parameter.
  indParVarianceInvKnown = inverse( indParVarianceKnown, nIndPar );

  // Calculate the known value for the derivative with respect
  // to the population parameter of the inverse of the variance
  // of the individual parameter using Lemma 10 of B. M. Bell,
  // "Approximating the marginal likelihood estimate for models
  // with random parameters", Applied Mathematics and Computation,
  // 119 (2001), pp. 57-73, which states that
  //
  //          -1               -1              -1
  //     d   A  ( x )  =  - [ A  ( x )  kron  A  ( x ) ]  d   A ( x )  .
  //      x                                                x
  //
  indParVarianceInv_popParKnown = AkronBtimesC(
    indParVarianceInvKnown,     nIndPar,
    indParVarianceInvKnown,     nIndPar,
    indParVariance_popParKnown, nPopPar );
  indParVarianceInv_popParKnown *= -1.0;


  //------------------------------------------------------------
  // Prepare known values related to SPK estimation.
  //------------------------------------------------------------

  valarray<double> popParLowKnown ( nPopPar );
  valarray<double> popParUpKnown  ( nPopPar );

  // Set the known limits for the population parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    popParLowKnown[k + thetaOffsetInPopPar] = thetaLow[k];
    popParUpKnown [k + thetaOffsetInPopPar] = thetaUp [k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    popParLowKnown[k + omegaParOffsetInPopPar] = omegaParLowKnown[k];
    popParUpKnown [k + omegaParOffsetInPopPar] = omegaParUpKnown [k];
  }
  for ( k = 0; k < nSigmaPar; k++ )
  {
    popParLowKnown[k + sigmaParOffsetInPopPar] = sigmaParLowKnown[k];
    popParUpKnown [k + sigmaParOffsetInPopPar] = sigmaParUpKnown [k];
  }
  
  // Since the step sizes may change as PopPredModel evolves, only 
  // check that they are positive.
  for ( k = 0; k < nPopPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The step size for a population parameter element is invalid.",
      popParStep[k] != 0.0 ||  omegaMinRepFixed[ k - omegaParOffsetInPopPar ] );
  }

  // Since the limits may change as PopPredModel evolves, only check
  // that the lower limits are less than or equal to the upper.
  for ( k = 0; k < nIndPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The limits for an individual parameter element are invalid.",
      indParLow[k] <= indParUp [k] ||  omegaMinRepFixed[ k - omegaParOffsetInPopPar ] );
  }
  
  // Since the step sizes may change as PopPredModel evolves, only 
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
    "The number of population parameters is not correct.",
    nPopPar == nPopParKnown );

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of individual parameters is not correct.",
    nIndPar == nIndParKnown );

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of data values for this individual is not correct.",
    nY_i == nY_iKnown );

  compareToKnown( 
    alphaCurr,
    alphaCurrKnown,
    "alphaCurr",
    tol );

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
    dataMean_popPar,
    dataMean_popParKnown,
    "dataMean_popPar",
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
    dataVariance_popPar,
    dataVariance_popParKnown,
    "dataVariance_popPar",
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
    dataVarianceInv_popPar,
    dataVarianceInv_popParKnown,
    "dataVarianceInv_popPar",
    tol );

  compareToKnown( 
    dataVarianceInv_indPar,
    dataVarianceInv_indParKnown,
    "dataVarianceInv_indPar",
    tol );

  compareToKnown( 
    indParVariance,
    indParVarianceKnown,
    "indParVariance",
    tol );

  compareToKnown( 
    indParVariance_popPar,
    indParVariance_popParKnown,
    "indParVariance_popPar",
    tol );

  compareToKnown( 
    indParVarianceInv,
    indParVarianceInvKnown,
    "indParVarianceInv",
    tol );

  compareToKnown( 
    indParVarianceInv_popPar,
    indParVarianceInv_popParKnown,
    "indParVarianceInv_popPar",
    tol );
}


/*************************************************************************
 *
 * Function: OneExpF_AdditivePlusThetaDepY_FullSigma_Test
 *
 *
 * The goal of this test is to check that the population level
 * Pred model works for the case of the 
 *
 *     OneExpF_AdditivePlusThetaDepY_Pred
 *
 * Pred block expression evaluator, for which analytical results 
 * can be calculated and which does not use any data values. 
 *
 *************************************************************************/

void PopPredModelTest::OneExpF_AdditivePlusThetaDepY_FullSigma_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpfpred_poppredmodeltest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluators.
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
  const int nEps   = 2;

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

  // Set the current value for eta.
  valarray<double> etaCurr( nEta );
  etaCurr[0] = -0.3;
  etaCurr[1] =  0.2;


  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrices.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  covStruct omegaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nOmegaPar = nEta;

  // Set the diagonal elements for the current value for omega.
  valarray<double> omegaMinRep( nOmegaPar );
  omegaMinRep[0] = 0.0001;
  omegaMinRep[1] = 0.02;

  // Set the structure of sigma, the covariance matrix for eps.
  covStruct sigmaStruct = FULL;

  // Set the number elements for this parameterization.
  int nSigmaPar = nEps * ( nEps + 1 ) / 2;

  // Set the lower triangle elements for the current value for sigma.
  valarray<double> sigmaMinRep( nSigmaPar );
  sigmaMinRep[0] =  0.25;
  sigmaMinRep[1] = -0.0003;
  sigmaMinRep[2] =  0.001;


  //------------------------------------------------------------
  // Construct the population level Pred model.
  //------------------------------------------------------------

  PopPredModel model(
    predEvaluator,
    predEvaluatorAD,
    predEvaluatorADAD,
    nTheta,
    thetaLow,
    thetaUp,
    thetaCurr,
    nEta,
    etaCurr,
    nEps,
    omegaStruct,
    omegaMinRep,
    sigmaStruct,
    sigmaMinRep );


  //------------------------------------------------------------
  // Get information related to the individual.
  //------------------------------------------------------------

  // Get the number elements in the population parameter.
  int nPopPar = model.getNPopPar();

  // Get the number elements in the individual parameter.
  int nIndPar = model.getNIndPar();

  // Get the number of observations for this individual.
  int iCurr = 0;
  int nY_i = predEvaluator.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare various quantities for the test.
  //------------------------------------------------------------

  // Get the current value for the population parameter.
  valarray<double> alphaCurr( nPopPar );
  model.getPopPar( alphaCurr );

  // Get the current value for the individual parameter.
  valarray<double> bCurr( nIndPar );
  model.getIndPar( bCurr );

  valarray<double> dataMean       ( nY_i );
  valarray<double> dataMean_popPar( nY_i * nPopPar );
  valarray<double> dataMean_indPar( nY_i * nIndPar );

  valarray<double> dataVariance          ( nY_i * nY_i );
  valarray<double> dataVariance_popPar   ( nY_i * nY_i * nPopPar );
  valarray<double> dataVariance_indPar   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInv       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_popPar( nY_i * nY_i * nPopPar );
  valarray<double> dataVarianceInv_indPar( nY_i * nY_i * nIndPar );

  valarray<double> indParVariance          ( nIndPar * nIndPar );
  valarray<double> indParVariance_popPar   ( nIndPar * nIndPar * nPopPar );
  valarray<double> indParVarianceInv       ( nIndPar * nIndPar );
  valarray<double> indParVarianceInv_popPar( nIndPar * nIndPar * nPopPar );

  bool ok;

  // Evaluate these quantities at the current population and 
  // individual parameter values, which are set when the model
  // is constructed.
  model.dataMean         ( dataMean );
  model.dataVariance     ( dataVariance );
  model.dataVarianceInv  ( dataVarianceInv );
  model.indParVariance   ( indParVariance );
  model.indParVarianceInv( indParVarianceInv );

  ok = model.dataMean_popPar( dataMean_popPar );
  ok = model.dataMean_indPar( dataMean_indPar );

  ok = model.dataVariance_popPar   ( dataVariance_popPar );
  ok = model.dataVariance_indPar   ( dataVariance_indPar );
  ok = model.dataVarianceInv_popPar( dataVarianceInv_popPar );
  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );

  ok = model.indParVariance_popPar   ( indParVariance_popPar );
  ok = model.indParVarianceInv_popPar( indParVarianceInv_popPar );

  valarray<double> popParLow ( nPopPar );
  valarray<double> popParUp  ( nPopPar );
  valarray<double> popParStep( nPopPar );

  // Get the limits for the population parameters.
  model.getPopParLimits( popParLow, popParUp );

  // Get the step sizes for the population parameters.
  model.getPopParStep( popParStep );

  valarray<double> indParLow ( nIndPar );
  valarray<double> indParUp  ( nIndPar );
  valarray<double> indParStep( nIndPar );

  // Get the limits for the individual parameters.
  model.getIndParLimits( indParLow, indParUp );

  // Get the step sizes for the individual parameters.
  model.getIndParStep( indParStep );

  valarray<double> standardPar       ( nPopPar );
  valarray<double> standardPar_popPar( nPopPar * nPopPar );
  valarray<bool>   standardParMask   ( nPopPar );

  // Get the current value for the standard parameter and its
  // derivative.
  model.getStandardPar       ( standardPar );
  model.getStandardPar_popPar( standardPar_popPar );

  // Set the population parameter mask.
  valarray<bool> popParMask( nPopPar );
  popParMask[0] = true;
  popParMask[1] = false;
  popParMask[2] = false;
  popParMask[3] = true;
  popParMask[4] = false;
  popParMask[5] = false;
  popParMask[6] = true;
  popParMask[7] = false;

  // Calculate the mask for the standard parameters.
  model.getStandardParMask( popParMask, standardParMask );


  //------------------------------------------------------------
  // Prepare known values related to the population parameter.
  //------------------------------------------------------------

  valarray<double> omegaCurr( nEta * nEta );

  // Create a covariance matrix that is equal to the one that the
  // PopPredModel is maintaining internally.
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

  valarray<double> sigmaCurr( nEps * nEps );

  // Create a covariance matrix that is equal to the one that the
  // PopPredModel is maintaining internally.
  FullCov sigma( nEps );
  sigma.expandCovMinRep( sigmaMinRep, sigmaCurr );
  sigma.setCov( sigmaCurr );

  valarray<double> sigmaKnown         ( nEps * nEps );
  valarray<double> sigmaParKnown      ( nSigmaPar );
  valarray<double> sigmaParLowKnown   ( nSigmaPar );
  valarray<double> sigmaParUpKnown    ( nSigmaPar );
  valarray<double> sigma_sigmaParKnown( nEps * nEps * nSigmaPar );

  // Get known values for sigma and its derivative.
  sigma.cov    ( sigmaKnown );
  sigma.cov_par( sigma_sigmaParKnown );

  // Get known values for sigma's parameter and its limits.
  sigma.calcPar     ( sigmaCurr,        sigmaParKnown );
  sigma.getParLimits( sigmaParLowKnown, sigmaParUpKnown );

  // The population parameter is composed of the parameters that are
  // optimized over when performing population level estimation,
  //
  //                -          -
  //               |   theta    |
  //               |            |
  //     alpha  =  |  omegaPar  |  .
  //               |            |
  //               |  sigmaPar  |
  //                -          -
  //
  int nPopParKnown = nTheta + nOmegaPar + nSigmaPar;

  int thetaOffsetInPopPar    = 0;
  int omegaParOffsetInPopPar = nTheta;
  int sigmaParOffsetInPopPar = nTheta + nOmegaPar;

  valarray<double> alphaCurrKnown( nPopParKnown );

  // Set the known value for the population parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    alphaCurrKnown[k + thetaOffsetInPopPar] = thetaCurr[k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    alphaCurrKnown[k + omegaParOffsetInPopPar] = omegaParKnown[k];
  }
  for ( k = 0; k < nSigmaPar; k++ )
  {
    alphaCurrKnown[k + sigmaParOffsetInPopPar] = sigmaParKnown[k];
  }

  valarray<double> standardParKnown       ( nPopPar );
  valarray<double> standardPar_popParKnown( nPopPar * nPopPar );
  valarray<bool>   standardParMaskKnown   ( nPopPar );

  // Set the known value for the standard parameter.
  //
  //                      -                 -
  //                     |     thetaCurr     |
  //                     |                   |
  //     standardPar  =  |  omegaMinRepCurr  |  .
  //                     |                   |
  //                     |  sigmaMinRepCurr  |
  //                      -                 -
  //
  for ( k = 0; k < nTheta; k++ )
  {
    standardParKnown[k + thetaOffsetInPopPar] = thetaCurr[k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    standardParKnown[k + omegaParOffsetInPopPar] = omegaMinRep[k];
  }
  for ( k = 0; k < nSigmaPar; k++ )
  {
    standardParKnown[k + sigmaParOffsetInPopPar] = sigmaMinRep[k];
  }

  valarray<double> omegaMinRep_omegaParKnown( nOmegaPar * nOmegaPar );
  valarray<double> sigmaMinRep_sigmaParKnown( nSigmaPar * nSigmaPar );

  // Get the known derivatives of the minimal representations of
  // omega and sigma.
  omega.calcCovMinRep_par(
    omega_omegaParKnown,
    nOmegaPar,
    omegaMinRep_omegaParKnown );
  sigma.calcCovMinRep_par(
    sigma_sigmaParKnown,
    nSigmaPar,
    sigmaMinRep_sigmaParKnown );

  // Create an nTheta by nTheta identity matrix.
  valarray<double> identityNTheta( nTheta * nTheta );
  identity( nTheta, identityNTheta );

  // Set the known value for the derivative of the standard parameter,
  //
  //     d       standardPar
  //      alpha
  //
  //             -                                                                                  -
  //            |  I                      0                                                     0    |
  //            |   nTheta                                                                           |
  //            |                                                                                    |
  //         =  |  0          d        omegaMinRep( omegaPar )                                  0    |  .
  //            |              omegaPar                                                              |
  //            |                                                                                    |
  //            |  0                      0                       d        sigmaMinRep( sigmaPar )   |
  //            |                                                  sigmaPar                          |
  //             -                                                                                  -
  //
  standardPar_popParKnown = 0.0;
  replaceSubblock(
    standardPar_popParKnown,
    nPopPar,
    identityNTheta,
    nTheta,
    thetaOffsetInPopPar,
    thetaOffsetInPopPar );
  replaceSubblock(
    standardPar_popParKnown,
    nPopPar,
    omegaMinRep_omegaParKnown,
    nOmegaPar,
    omegaParOffsetInPopPar,
    omegaParOffsetInPopPar );
  replaceSubblock(
    standardPar_popParKnown,
    nPopPar,
    sigmaMinRep_sigmaParKnown,
    nSigmaPar,
    sigmaParOffsetInPopPar,
    sigmaParOffsetInPopPar );

  // Set the known value for the mask for the standard parameters.
  standardParMaskKnown = popParMask;


  //------------------------------------------------------------
  // Prepare known values related to the individual parameter.
  //------------------------------------------------------------

  // The individual parameter is the parameter that is optimized
  // over when performing individual level estimation,
  //
  //     b   =  eta  .
  //      i
  //
  int nIndParKnown = nEta;

  valarray<double> bCurrKnown( nIndParKnown );

  // Set the known value for the individual parameter.
  bCurrKnown = etaCurr;


  //------------------------------------------------------------
  // Prepare known values related to the model functions.
  //------------------------------------------------------------

  valarray<double> dataMeanKnown       ( nY_i );
  valarray<double> dataMean_popParKnown( nY_i * nPopPar );
  valarray<double> dataMean_indParKnown( nY_i * nIndPar );

  valarray<double> dataVarianceKnown          ( nY_i * nY_i );
  valarray<double> dataVariance_popParKnown   ( nY_i * nY_i * nPopPar );
  valarray<double> dataVariance_indParKnown   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInvKnown       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_popParKnown( nY_i * nY_i * nPopPar );
  valarray<double> dataVarianceInv_indParKnown( nY_i * nY_i * nIndPar );

  valarray<double> indParVarianceKnown          ( nIndPar * nIndPar );
  valarray<double> indParVariance_popParKnown   ( nIndPar * nIndPar * nPopPar );
  valarray<double> indParVarianceInvKnown       ( nIndPar * nIndPar );
  valarray<double> indParVarianceInv_popParKnown( nIndPar * nIndPar * nPopPar );

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
  //                                      -                                     -   
  //                                     |                                       |
  //                            ds * exp |  - [ theta    + eta    ]  *  time     |
  //                                     |           (0)      (0)           (j)  |
  //                                      -                                     -   
  //    f    ( alpha, b  )  =  ----------------------------------------------------  .
  //     i(j)          i
  //                                      [ theta    * w   +  eta    ]
  //                                             (1)             (1)
  //
  for ( j = 0; j < nY_i; j++ )
  {
    time = timeStep * j;
    ds = dose * wt;
    w = wt;

    ke = thetaCurr[0] + etaCurr[0];
    cl = thetaCurr[1] * w + etaCurr[1];
    d = std::exp(-ke * time);
    e = cl;
    dataMeanKnown[j] = ds * d / e;
  }

  int col;

  // Calculate the known value for the derivative of the data mean
  // with respect to the population parameter.
  dataMean_popParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)
    //     d       f    ( alpha, b  )  =  - time     f    ( alpha, b  )  .
    //      theta   i(j)          i             (j)   i(j)          i
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - timeStep * j * dataMean[j];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                       - wt  f    ( alpha, b  )
    //      (1)                                     i(j)          i
    //     d       f    ( alpha, b  )  =  -------------------------------  .
    //      theta   i(j)          i  
    //                                     [ theta    * wt   +  eta    ]
    //                                            (1)              (1)
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - wt * dataMean[j] /
      ( thetaCurr[1] * wt + etaCurr[1] );
  }

  // Calculate the known value for the derivative of the data mean
  // with respect to the individual parameter.
  dataMean_indParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)
    //     d     f    ( alpha, b  )  =  - time     f    ( alpha, b  )  .
    //      eta   i(j)          i             (j)   i(j)          i
    //
    dataMean_indParKnown[j + k * nY_i] = - timeStep * j * dataMean[j];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                       -  f    ( alpha, b  )
    //      (1)                                  i(j)          i
    //     d     f    ( alpha, b  )  =  -------------------------------  .
    //      eta   i(j)          i  
    //                                   [ theta    * wt   +  eta    ]
    //                                          (1)              (1)
    //
    dataMean_indParKnown[j + k * nY_i] = - dataMean[j] /
      ( thetaCurr[1] * wt + etaCurr[1] );
  }

  // Calculate the known value for the variance of the data.
  dataVarianceKnown = 0.0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the values for the diagonal elements of the data variance:
    //
    //     R      ( alpha, b  )
    //      i(j,j)          i
    //                                                                         2
    //          =  sigma       +  2  sigma       theta     +  sigma       theta   
    //                  (0,0)             (1,0)       (2)          (1,1)       (2)
    //
    dataVarianceKnown[j + j * nY_i] = sigmaKnown[0 + 0 * nEps] +
      2.0 * sigmaKnown[1 + 0 * nEps] * thetaCurr[2] +
      sigmaKnown[1 + 1 * nEps] * thetaCurr[2] * thetaCurr[2];
  }

  // Calculate the known value for the derivative of the variance
  // of the data with respect to the population parameter.
  dataVariance_popParKnown = 0.0;
  int nRow = nY_i * nY_i;
  int row;
  k = 2;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (2)
    //     d       R      ( alpha, b  )  =  2  sigma       +  2  sigma       theta     .
    //      theta   i(j,j)          i               (1,0)             (1,1)       (2)
    //
    row = j * nY_i + j;
    col = k + thetaOffsetInPopPar;
  
    dataVariance_popParKnown[row + col * nRow] = 
      2.0 * sigmaKnown[1 + 0 * nEps] +
      2.0 * sigmaKnown[1 + 1 * nEps] * thetaCurr[2];
  }
  // Set the known values for the partial derivatives with
  // respect to sigmaPar,
  //
  //      (k)
  //     d          R      ( alpha, b  )
  //      sigmaPar   i(j,j)          i
  //
  //             (k)                
  //         =  d         sigma     ( sigmaPar )
  //             sigmaPar      (0,0)
  //
  //                                               -             -
  //             (k)                              |               |
  //         +  d         sigma     ( sigmaPar )  |  2  theta     | .
  //             sigmaPar      (1,0)              |          (2)  |
  //                                               -             -
  //
  //                                               -          -
  //             (k)                              |       2    |
  //         +  d         sigma     ( sigmaPar )  |  theta     | .
  //             sigmaPar      (1,1)              |       (2)  |
  //                                               -          -
  //
  int nSigma_sigmaParRow = nEps * nEps;
  for ( j = 0; j < nY_i; j++ )
  {
    for ( k = 0; k < nSigmaPar; k++ )
    {
      row = j * nY_i + j;
      col = k + sigmaParOffsetInPopPar;
    
      dataVariance_popParKnown[row + col * nRow] = 
        sigma_sigmaParKnown[( 0 * nEps + 0 ) + k * nSigma_sigmaParRow] +
        sigma_sigmaParKnown[( 1 * nEps + 0 ) + k * nSigma_sigmaParRow] * 2.0 * thetaCurr[2] +
        sigma_sigmaParKnown[( 1 * nEps + 1 ) + k * nSigma_sigmaParRow] * thetaCurr[2] * thetaCurr[2];
    }
  }

  // Calculate the known value for the inverse of the variance
  // of the data.
  dataVarianceInvKnown = inverse( dataVarianceKnown, nY_i );

  // Calculate the known value for the derivative with respect
  // to the population parameter of the inverse of the variance
  // of the data using Lemma 10 of B. M. Bell, "Approximating
  // the marginal likelihood estimate for models with random
  // parameters", Applied Mathematics and Computation, 119 
  // (2001), pp. 57-73, which states that
  //
  //          -1               -1              -1
  //     d   A  ( x )  =  - [ A  ( x )  kron  A  ( x ) ]  d   A ( x )  .
  //      x                                                x
  //
  dataVarianceInv_popParKnown = AkronBtimesC(
    dataVarianceInvKnown,     nY_i,
    dataVarianceInvKnown,     nY_i,
    dataVariance_popParKnown, nPopPar );
  dataVarianceInv_popParKnown *= -1.0;

  // Calculate the known value for the derivative with respect
  // to the individual parameter of the inverse of the variance
  // of the data using Lemma 10 of B. M. Bell, "Approximating
  // the marginal likelihood estimate for models with random
  // parameters", Applied Mathematics and Computation, 119 
  // (2001), pp. 57-73, which states that
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

  // Set the known value for the variance of the individual parameter,
  //
  //     D ( alpha )  =  omega( omegaPar )  .
  //
  indParVarianceKnown = omegaKnown;

  // Calculate the known value for the derivative with respect
  // to the population parameter of the variance of the individual
  // parameter,
  //
  //     d       D ( alpha )
  //      alpha
  //
  //             -                                                              -
  //            |  0, 0, ... , 0,                                 0, 0, ... , 0  |
  //            |                                                                |
  //         =  |  0, 0, ... , 0,  d          omega ( omega  ) ,  0, 0, ... , 0  |  ,
  //            |                   omegaPar                                     |  
  //            |  0, 0, ... , 0,                                 0, 0, ... , 0  |
  //             -                                                              -
  //
  indParVariance_popParKnown = 0.0;
  nRow = nIndPar * nIndPar;
  for ( k = 0; k < nOmegaPar; k++ )
  {
    for ( j = 0; j < nRow; j++ )
    {
      row = j;
      col = k + omegaParOffsetInPopPar;

      indParVariance_popParKnown[row + col * nRow] = omega_omegaParKnown[j + k * nRow];
    }
  }

  // Calculate the known value for the inverse of the variance
  // of the individual parameter.
  indParVarianceInvKnown = inverse( indParVarianceKnown, nIndPar );

  // Calculate the known value for the derivative with respect
  // to the population parameter of the inverse of the variance
  // of the individual parameter using Lemma 10 of B. M. Bell,
  // "Approximating the marginal likelihood estimate for models
  // with random parameters", Applied Mathematics and Computation,
  // 119 (2001), pp. 57-73, which states that
  //
  //          -1               -1              -1
  //     d   A  ( x )  =  - [ A  ( x )  kron  A  ( x ) ]  d   A ( x )  .
  //      x                                                x
  //
  indParVarianceInv_popParKnown = AkronBtimesC(
    indParVarianceInvKnown,     nIndPar,
    indParVarianceInvKnown,     nIndPar,
    indParVariance_popParKnown, nPopPar );
  indParVarianceInv_popParKnown *= -1.0;


  //------------------------------------------------------------
  // Prepare known values related to SPK estimation.
  //------------------------------------------------------------

  valarray<double> popParLowKnown ( nPopPar );
  valarray<double> popParUpKnown  ( nPopPar );

  // Set the known limits for the population parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    popParLowKnown[k + thetaOffsetInPopPar] = thetaLow[k];
    popParUpKnown [k + thetaOffsetInPopPar] = thetaUp [k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    popParLowKnown[k + omegaParOffsetInPopPar] = omegaParLowKnown[k];
    popParUpKnown [k + omegaParOffsetInPopPar] = omegaParUpKnown [k];
  }
  for ( k = 0; k < nSigmaPar; k++ )
  {
    popParLowKnown[k + sigmaParOffsetInPopPar] = sigmaParLowKnown[k];
    popParUpKnown [k + sigmaParOffsetInPopPar] = sigmaParUpKnown [k];
  }
  
  // Since the step sizes may change as PopPredModel evolves, only 
  // check that they are positive.
  for ( k = 0; k < nPopPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The step size for a population parameter element is invalid.",
      popParStep[k] != 0.0 );
  }

  // Since the limits may change as PopPredModel evolves, only check
  // that the lower limits are less than or equal to the upper.
  for ( k = 0; k < nIndPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The limits for an individual parameter element are invalid.",
      indParLow[k] <= indParUp [k] );
  }
  
  // Since the step sizes may change as PopPredModel evolves, only 
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
    "The number of population parameters is not correct.",
    nPopPar == nPopParKnown );

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of individual parameters is not correct.",
    nIndPar == nIndParKnown );

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of data values for this individual is not correct.",
    nY_i == nY_iKnown );

  compareToKnown( 
    alphaCurr,
    alphaCurrKnown,
    "alphaCurr",
    tol );

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
    dataMean_popPar,
    dataMean_popParKnown,
    "dataMean_popPar",
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
    dataVariance_popPar,
    dataVariance_popParKnown,
    "dataVariance_popPar",
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
    dataVarianceInv_popPar,
    dataVarianceInv_popParKnown,
    "dataVarianceInv_popPar",
    tol );

  compareToKnown( 
    dataVarianceInv_indPar,
    dataVarianceInv_indParKnown,
    "dataVarianceInv_indPar",
    tol );

  compareToKnown( 
    indParVariance,
    indParVarianceKnown,
    "indParVariance",
    tol );

  compareToKnown( 
    indParVariance_popPar,
    indParVariance_popParKnown,
    "indParVariance_popPar",
    tol );

  compareToKnown( 
    indParVarianceInv,
    indParVarianceInvKnown,
    "indParVarianceInv",
    tol );

  compareToKnown( 
    indParVarianceInv_popPar,
    indParVarianceInv_popParKnown,
    "indParVarianceInv_popPar",
    tol );

  compareToKnown( 
    standardPar,
    standardParKnown,
    "standardPar",
    tol );

  compareToKnown( 
    standardPar_popPar,
    standardPar_popParKnown,
    "standardPar_popPar",
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
 * The goal of this test is to check that the population level Pred
 * model works for the case where not all of the data records for the
 * individuals are observation records and for the case of the
 *
 *     OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Pred
 *
 * Pred block expression evaluator, for which analytical results 
 * can be calculated and which does not use any data values. 
 *
 *************************************************************************/

void PopPredModelTest::OneExpF_AdditivePlusThetaDepY_NotAllRecAreObsRec_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpfpred_poppredmodeltest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluators.
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
  const int nEps   = 2;

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

  // Set the current value for eta.
  valarray<double> etaCurr( nEta );
  etaCurr[0] = -0.3;
  etaCurr[1] =  0.2;


  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrices.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  covStruct omegaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nOmegaPar = nEta;

  // Set the diagonal elements for the current value for omega.
  valarray<double> omegaMinRep( nOmegaPar );
  omegaMinRep[0] = 0.0001;
  omegaMinRep[1] = 0.02;

  // Set the structure of sigma, the covariance matrix for eps.
  covStruct sigmaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nSigmaPar = nEps;

  // Set the diagonal elements for the current value for sigma.
  valarray<double> sigmaMinRep( nSigmaPar );
  sigmaMinRep[0] = 0.25;
  sigmaMinRep[1] = 0.001;


  //------------------------------------------------------------
  // Construct the population level Pred model.
  //------------------------------------------------------------

  PopPredModel model(
    predEvaluator,
    predEvaluatorAD,
    predEvaluatorADAD,
    nTheta,
    thetaLow,
    thetaUp,
    thetaCurr,
    nEta,
    etaCurr,
    nEps,
    omegaStruct,
    omegaMinRep,
    sigmaStruct,
    sigmaMinRep );


  //------------------------------------------------------------
  // Get information related to the individual.
  //------------------------------------------------------------

  // Get the number elements in the population parameter.
  int nPopPar = model.getNPopPar();

  // Get the number elements in the individual parameter.
  int nIndPar = model.getNIndPar();

  // Get the number of observations for this individual.
  int iCurr = 0;
  int nY_i = predEvaluator.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare various quantities for the test.
  //------------------------------------------------------------

  // Get the current value for the population parameter.
  valarray<double> alphaCurr( nPopPar );
  model.getPopPar( alphaCurr );

  // Get the current value for the individual parameter.
  valarray<double> bCurr( nIndPar );
  model.getIndPar( bCurr );

  valarray<double> dataMean       ( nY_i );
  valarray<double> dataMean_popPar( nY_i * nPopPar );
  valarray<double> dataMean_indPar( nY_i * nIndPar );

  valarray<double> dataVariance          ( nY_i * nY_i );
  valarray<double> dataVariance_popPar   ( nY_i * nY_i * nPopPar );
  valarray<double> dataVariance_indPar   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInv       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_popPar( nY_i * nY_i * nPopPar );
  valarray<double> dataVarianceInv_indPar( nY_i * nY_i * nIndPar );

  valarray<double> indParVariance          ( nIndPar * nIndPar );
  valarray<double> indParVariance_popPar   ( nIndPar * nIndPar * nPopPar );
  valarray<double> indParVarianceInv       ( nIndPar * nIndPar );
  valarray<double> indParVarianceInv_popPar( nIndPar * nIndPar * nPopPar );

  bool ok;

  // Evaluate these quantities at the current population and 
  // individual parameter values, which are set when the model
  // is constructed.
  model.dataMean         ( dataMean );
  model.dataVariance     ( dataVariance );
  model.dataVarianceInv  ( dataVarianceInv );
  model.indParVariance   ( indParVariance );
  model.indParVarianceInv( indParVarianceInv );

  ok = model.dataMean_popPar( dataMean_popPar );
  ok = model.dataMean_indPar( dataMean_indPar );

  ok = model.dataVariance_popPar   ( dataVariance_popPar );
  ok = model.dataVariance_indPar   ( dataVariance_indPar );
  ok = model.dataVarianceInv_popPar( dataVarianceInv_popPar );
  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );

  ok = model.indParVariance_popPar   ( indParVariance_popPar );
  ok = model.indParVarianceInv_popPar( indParVarianceInv_popPar );

  valarray<double> popParLow ( nPopPar );
  valarray<double> popParUp  ( nPopPar );
  valarray<double> popParStep( nPopPar );

  // Get the limits for the population parameters.
  model.getPopParLimits( popParLow, popParUp );

  // Get the step sizes for the population parameters.
  model.getPopParStep( popParStep );

  valarray<double> indParLow ( nIndPar );
  valarray<double> indParUp  ( nIndPar );
  valarray<double> indParStep( nIndPar );

  // Get the limits for the individual parameters.
  model.getIndParLimits( indParLow, indParUp );

  // Get the step sizes for the individual parameters.
  model.getIndParStep( indParStep );


  //------------------------------------------------------------
  // Prepare known values related to the population parameter.
  //------------------------------------------------------------

  valarray<double> omegaCurr( nEta * nEta );

  // Create a covariance matrix that is equal to the one that the
  // PopPredModel is maintaining internally.
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

  valarray<double> sigmaCurr( nEps * nEps );

  // Create a covariance matrix that is equal to the one that the
  // PopPredModel is maintaining internally.
  DiagCov sigma( nEps );
  sigma.expandCovMinRep( sigmaMinRep, sigmaCurr );
  sigma.setCov( sigmaCurr );

  valarray<double> sigmaKnown         ( nEps * nEps );
  valarray<double> sigmaParKnown      ( nSigmaPar );
  valarray<double> sigmaParLowKnown   ( nSigmaPar );
  valarray<double> sigmaParUpKnown    ( nSigmaPar );
  valarray<double> sigma_sigmaParKnown( nEps * nEps * nSigmaPar );

  // Get known values for sigma and its derivative.
  sigma.cov    ( sigmaKnown );
  sigma.cov_par( sigma_sigmaParKnown );

  // Get known values for sigma's parameter and its limits.
  sigma.calcPar     ( sigmaCurr,        sigmaParKnown );
  sigma.getParLimits( sigmaParLowKnown, sigmaParUpKnown );

  // The population parameter is composed of the parameters that are
  // optimized over when performing population level estimation,
  //
  //                -          -
  //               |   theta    |
  //               |            |
  //     alpha  =  |  omegaPar  |  .
  //               |            |
  //               |  sigmaPar  |
  //                -          -
  //
  int nPopParKnown = nTheta + nOmegaPar + nSigmaPar;

  int thetaOffsetInPopPar    = 0;
  int omegaParOffsetInPopPar = nTheta;
  int sigmaParOffsetInPopPar = nTheta + nOmegaPar;

  valarray<double> alphaCurrKnown( nPopParKnown );

  // Set the known value for the population parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    alphaCurrKnown[k + thetaOffsetInPopPar] = thetaCurr[k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    alphaCurrKnown[k + omegaParOffsetInPopPar] = omegaParKnown[k];
  }
  for ( k = 0; k < nSigmaPar; k++ )
  {
    alphaCurrKnown[k + sigmaParOffsetInPopPar] = sigmaParKnown[k];
  }


  //------------------------------------------------------------
  // Prepare known values related to the individual parameter.
  //------------------------------------------------------------

  // The individual parameter is the parameter that is optimized
  // over when performing individual level estimation,
  //
  //     b   =  eta  .
  //      i
  //
  int nIndParKnown = nEta;

  valarray<double> bCurrKnown( nIndParKnown );

  // Set the known value for the individual parameter.
  bCurrKnown = etaCurr;


  //------------------------------------------------------------
  // Prepare known values related to the model functions.
  //------------------------------------------------------------

  valarray<double> dataMeanKnown       ( nY_i );
  valarray<double> dataMean_popParKnown( nY_i * nPopPar );
  valarray<double> dataMean_indParKnown( nY_i * nIndPar );

  valarray<double> dataVarianceKnown          ( nY_i * nY_i );
  valarray<double> dataVariance_popParKnown   ( nY_i * nY_i * nPopPar );
  valarray<double> dataVariance_indParKnown   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInvKnown       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_popParKnown( nY_i * nY_i * nPopPar );
  valarray<double> dataVarianceInv_indParKnown( nY_i * nY_i * nIndPar );

  valarray<double> indParVarianceKnown          ( nIndPar * nIndPar );
  valarray<double> indParVariance_popParKnown   ( nIndPar * nIndPar * nPopPar );
  valarray<double> indParVarianceInvKnown       ( nIndPar * nIndPar );
  valarray<double> indParVarianceInv_popParKnown( nIndPar * nIndPar * nPopPar );

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
  //                                      -                                     -   
  //                                     |                                       |
  //                            ds * exp |  - [ theta    + eta    ]  *  time     |
  //                                     |           (0)      (0)           (j)  |
  //                                      -                                     -   
  //    f    ( alpha, b  )  =  ----------------------------------------------------  .
  //     i(j)          i
  //                                      [ theta    * w   +  eta    ]
  //                                             (1)             (1)
  //
  for ( j = 0; j < nY_i; j++ )
  {
    time = timeStep * j;
    ds = dose * wt;
    w = wt;

    ke = thetaCurr[0] + etaCurr[0];
    cl = thetaCurr[1] * w + etaCurr[1];
    d = std::exp(-ke * time);
    e = cl;
    dataMeanKnown[j] = ds * d / e;
  }

  int col;

  // Calculate the known value for the derivative of the data mean
  // with respect to the population parameter.
  dataMean_popParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)
    //     d       f    ( alpha, b  )  =  - time     f    ( alpha, b  )  .
    //      theta   i(j)          i             (j)   i(j)          i
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - timeStep * j * dataMean[j];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                       - wt  f    ( alpha, b  )
    //      (1)                                     i(j)          i
    //     d       f    ( alpha, b  )  =  -------------------------------  .
    //      theta   i(j)          i  
    //                                     [ theta    * wt   +  eta    ]
    //                                            (1)              (1)
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - wt * dataMean[j] /
      ( thetaCurr[1] * wt + etaCurr[1] );
  }

  // Calculate the known value for the derivative of the data mean
  // with respect to the individual parameter.
  dataMean_indParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)
    //     d     f    ( alpha, b  )  =  - time     f    ( alpha, b  )  .
    //      eta   i(j)          i             (j)   i(j)          i
    //
    dataMean_indParKnown[j + k * nY_i] = - timeStep * j * dataMean[j];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                       -  f    ( alpha, b  )
    //      (1)                                  i(j)          i
    //     d     f    ( alpha, b  )  =  -------------------------------  .
    //      eta   i(j)          i  
    //                                   [ theta    * wt   +  eta    ]
    //                                          (1)              (1)
    //
    dataMean_indParKnown[j + k * nY_i] = - dataMean[j] /
      ( thetaCurr[1] * wt + etaCurr[1] );
  }

  // Calculate the known value for the variance of the data.
  dataVarianceKnown = 0.0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the values for the diagonal elements of the data variance:
    //
    //                                                              2
    //     R      ( alpha, b  )  =  sigma       +  sigma       theta     .
    //      i(j,j)          i            (0,0)          (1,1)       (2)
    //
    dataVarianceKnown[j + j * nY_i] = sigmaKnown[0 + 0 * nEps] +
      sigmaKnown[1 + 1 * nEps] * thetaCurr[2] * thetaCurr[2];
  }

  // Calculate the known value for the derivative of the variance
  // of the data with respect to the population parameter.
  dataVariance_popParKnown = 0.0;
  int nRow = nY_i * nY_i;
  int row;
  k = 2;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (2)
    //     d       R      ( alpha, b  )  =  2  sigma       theta     .
    //      theta   i(j,j)          i               (1,1)       (2)
    //
    row = j * nY_i + j;
    col = k + thetaOffsetInPopPar;
  
    dataVariance_popParKnown[row + col * nRow] = 
      2.0 * sigmaKnown[1 + 1 * nEps] * thetaCurr[2];
  }
  int nSigma_sigmaParRow = nEps * nEps;
  int sigma_sigmaParRow;
  int sigma_sigmaParCol;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (0)                                 (0)                
    //     d          R      ( alpha, b  )  =  d         sigma     ( sigmaPar )  .
    //      sigmaPar   i(j,j)          i        sigmaPar      (0,0)
    //
    row = j * nY_i + j;
    col = k + sigmaParOffsetInPopPar;

    sigma_sigmaParRow = k * nEps + k;
    sigma_sigmaParCol = k;
    
    dataVariance_popParKnown[row + col * nRow] = 
      sigma_sigmaParKnown[sigma_sigmaParRow + sigma_sigmaParCol * nSigma_sigmaParRow];
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //      (1)                                     2     (1)                
    //     d          R      ( alpha, b  )  =  theta     d         sigma     ( sigmaPar )  .
    //      sigmaPar   i(j,j)          i            (2)   sigmaPar      (1,1)
    //
    row = j * nY_i + j;
    col = k + sigmaParOffsetInPopPar;

    sigma_sigmaParRow = k * nEps + k;
    sigma_sigmaParCol = k;
    
    dataVariance_popParKnown[row + col * nRow] = 
      thetaCurr[2] * thetaCurr[2] *
      sigma_sigmaParKnown[sigma_sigmaParRow + sigma_sigmaParCol * nSigma_sigmaParRow];
  }

  // Calculate the known value for the inverse of the variance
  // of the data.
  dataVarianceInvKnown = inverse( dataVarianceKnown, nY_i );

  // Calculate the known value for the derivative with respect
  // to the population parameter of the inverse of the variance
  // of the data using Lemma 10 of B. M. Bell, "Approximating
  // the marginal likelihood estimate for models with random
  // parameters", Applied Mathematics and Computation, 119 
  // (2001), pp. 57-73, which states that
  //
  //          -1               -1              -1
  //     d   A  ( x )  =  - [ A  ( x )  kron  A  ( x ) ]  d   A ( x )  .
  //      x                                                x
  //
  dataVarianceInv_popParKnown = AkronBtimesC(
    dataVarianceInvKnown,     nY_i,
    dataVarianceInvKnown,     nY_i,
    dataVariance_popParKnown, nPopPar );
  dataVarianceInv_popParKnown *= -1.0;

  // Calculate the known value for the derivative with respect
  // to the individual parameter of the inverse of the variance
  // of the data using Lemma 10 of B. M. Bell, "Approximating
  // the marginal likelihood estimate for models with random
  // parameters", Applied Mathematics and Computation, 119 
  // (2001), pp. 57-73, which states that
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

  // Set the known value for the variance of the individual parameter,
  //
  //     D ( alpha )  =  omega( omegaPar )  .
  //
  indParVarianceKnown = omegaKnown;

  // Calculate the known value for the derivative with respect
  // to the population parameter of the variance of the individual
  // parameter,
  //
  //     d       D ( alpha )
  //      alpha
  //
  //             -                                                              -
  //            |  0, 0, ... , 0,                                 0, 0, ... , 0  |
  //            |                                                                |
  //         =  |  0, 0, ... , 0,  d          omega ( omega  ) ,  0, 0, ... , 0  |  ,
  //            |                   omegaPar                                     |  
  //            |  0, 0, ... , 0,                                 0, 0, ... , 0  |
  //             -                                                              -
  //
  indParVariance_popParKnown = 0.0;
  nRow = nIndPar * nIndPar;
  for ( k = 0; k < nOmegaPar; k++ )
  {
    for ( j = 0; j < nRow; j++ )
    {
      row = j;
      col = k + omegaParOffsetInPopPar;

      indParVariance_popParKnown[row + col * nRow] = omega_omegaParKnown[j + k * nRow];
    }
  }

  // Calculate the known value for the inverse of the variance
  // of the individual parameter.
  indParVarianceInvKnown = inverse( indParVarianceKnown, nIndPar );

  // Calculate the known value for the derivative with respect
  // to the population parameter of the inverse of the variance
  // of the individual parameter using Lemma 10 of B. M. Bell,
  // "Approximating the marginal likelihood estimate for models
  // with random parameters", Applied Mathematics and Computation,
  // 119 (2001), pp. 57-73, which states that
  //
  //          -1               -1              -1
  //     d   A  ( x )  =  - [ A  ( x )  kron  A  ( x ) ]  d   A ( x )  .
  //      x                                                x
  //
  indParVarianceInv_popParKnown = AkronBtimesC(
    indParVarianceInvKnown,     nIndPar,
    indParVarianceInvKnown,     nIndPar,
    indParVariance_popParKnown, nPopPar );
  indParVarianceInv_popParKnown *= -1.0;


  //------------------------------------------------------------
  // Prepare known values related to SPK estimation.
  //------------------------------------------------------------

  valarray<double> popParLowKnown ( nPopPar );
  valarray<double> popParUpKnown  ( nPopPar );

  // Set the known limits for the population parameter.
  for ( k = 0; k < nTheta; k++ )
  {
    popParLowKnown[k + thetaOffsetInPopPar] = thetaLow[k];
    popParUpKnown [k + thetaOffsetInPopPar] = thetaUp [k];
  }
  for ( k = 0; k < nOmegaPar; k++ )
  {
    popParLowKnown[k + omegaParOffsetInPopPar] = omegaParLowKnown[k];
    popParUpKnown [k + omegaParOffsetInPopPar] = omegaParUpKnown [k];
  }
  for ( k = 0; k < nSigmaPar; k++ )
  {
    popParLowKnown[k + sigmaParOffsetInPopPar] = sigmaParLowKnown[k];
    popParUpKnown [k + sigmaParOffsetInPopPar] = sigmaParUpKnown [k];
  }
  
  // Since the step sizes may change as PopPredModel evolves, only 
  // check that they are positive.
  for ( k = 0; k < nPopPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The step size for a population parameter element is invalid.",
      popParStep[k] != 0.0 );
  }

  // Since the limits may change as PopPredModel evolves, only check
  // that the lower limits are less than or equal to the upper.
  for ( k = 0; k < nIndPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The limits for an individual parameter element are invalid.",
      indParLow[k] <= indParUp [k] );
  }
  
  // Since the step sizes may change as PopPredModel evolves, only 
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
    "The number of population parameters is not correct.",
    nPopPar == nPopParKnown );

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of individual parameters is not correct.",
    nIndPar == nIndParKnown );

  CPPUNIT_ASSERT_MESSAGE( 
    "The number of data values for this individual is not correct.",
    nY_i == nY_iKnown );

  compareToKnown( 
    alphaCurr,
    alphaCurrKnown,
    "alphaCurr",
    tol );

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
    dataMean_popPar,
    dataMean_popParKnown,
    "dataMean_popPar",
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
    dataVariance_popPar,
    dataVariance_popParKnown,
    "dataVariance_popPar",
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
    dataVarianceInv_popPar,
    dataVarianceInv_popParKnown,
    "dataVarianceInv_popPar",
    tol );

  compareToKnown( 
    dataVarianceInv_indPar,
    dataVarianceInv_indParKnown,
    "dataVarianceInv_indPar",
    tol );

  compareToKnown( 
    indParVariance,
    indParVarianceKnown,
    "indParVariance",
    tol );

  compareToKnown( 
    indParVariance_popPar,
    indParVariance_popParKnown,
    "indParVariance_popPar",
    tol );

  compareToKnown( 
    indParVarianceInv,
    indParVarianceInvKnown,
    "indParVarianceInv",
    tol );

  compareToKnown( 
    indParVarianceInv_popPar,
    indParVarianceInv_popParKnown,
    "indParVarianceInv_popPar",
    tol );
}


/*************************************************************************
 *
 * Function: isCachingProperlyTest
 *
 *
 * The goal of this test is to check that the population level 
 * Pred model is caching properly.
 *
 *************************************************************************/

void PopPredModelTest::isCachingProperlyTest()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpfpred_poppredmodeltest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluators.
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
  const int nEps   = 2;

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

  // Set the current value for eta.
  valarray<double> etaCurr( nEta );
  etaCurr[0] = -0.3;
  etaCurr[1] =  0.2;


  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrices.
  //------------------------------------------------------------

  // Set the structure of omega, the covariance matrix for eta.
  covStruct omegaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nOmegaPar = nEta;

  // Set the diagonal elements for the current value for omega.
  valarray<double> omegaMinRep( nOmegaPar );
  omegaMinRep[0] = 0.0001;
  omegaMinRep[1] = 0.02;

  // Set the structure of sigma, the covariance matrix for eps.
  covStruct sigmaStruct = DIAGONAL;

  // Set the number elements for this parameterization.
  int nSigmaPar = nEps;

  // Set the diagonal elements for the current value for sigma.
  valarray<double> sigmaMinRep( nSigmaPar );
  sigmaMinRep[0] = 0.25;
  sigmaMinRep[1] = 0.001;


  //------------------------------------------------------------
  // Construct the population level Pred model.
  //------------------------------------------------------------

  PopPredModel model(
    predEvaluator,
    predEvaluatorAD,
    predEvaluatorADAD,
    nTheta,
    thetaLow,
    thetaUp,
    thetaCurr,
    nEta,
    etaCurr,
    nEps,
    omegaStruct,
    omegaMinRep,
    sigmaStruct,
    sigmaMinRep );


  //------------------------------------------------------------
  // Get information related to the individual.
  //------------------------------------------------------------

  // Get the number elements in the population and individual 
  // parameters.
  int nPopPar = model.getNPopPar();
  int nIndPar = model.getNIndPar();

  // Get the number of observations for this individual.
  int iCurr = 0;
  int nY_i = predEvaluator.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare to see if values are being cached.
  //------------------------------------------------------------

  valarray<double> dataMean       ( nY_i );
  valarray<double> dataMean_popPar( nY_i * nPopPar );
  valarray<double> dataMean_indPar( nY_i * nIndPar );

  valarray<double> dataVariance          ( nY_i * nY_i );
  valarray<double> dataVariance_popPar   ( nY_i * nY_i * nPopPar );
  valarray<double> dataVariance_indPar   ( nY_i * nY_i * nIndPar );
  valarray<double> dataVarianceInv       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_popPar( nY_i * nY_i * nPopPar );
  valarray<double> dataVarianceInv_indPar( nY_i * nY_i * nIndPar );

  valarray<double> indParVariance          ( nIndPar * nIndPar );
  valarray<double> indParVariance_popPar   ( nIndPar * nIndPar * nPopPar );
  valarray<double> indParVarianceInv       ( nIndPar * nIndPar );
  valarray<double> indParVarianceInv_popPar( nIndPar * nIndPar * nPopPar );

  valarray<double> popParLow( nPopPar );
  valarray<double> popParUp ( nPopPar );

  // Get the limits for the population parameters.
  model.getPopParLimits( popParLow, popParUp );

  bool ok;

  // Evaluate these quantities at the current population and 
  // individual parameter values, which are set when the model
  // is constructed.
  model.dataMean         ( dataMean );
  model.dataVariance     ( dataVariance );
  model.dataVarianceInv  ( dataVarianceInv );
  model.indParVariance   ( indParVariance );
  model.indParVarianceInv( indParVarianceInv );

  ok = model.dataMean_popPar( dataMean_popPar );
  ok = model.dataMean_indPar( dataMean_indPar );

  ok = model.dataVariance_popPar   ( dataVariance_popPar );
  ok = model.dataVariance_indPar   ( dataVariance_indPar );
  ok = model.dataVarianceInv_popPar( dataVarianceInv_popPar );
  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );

  ok = model.indParVariance_popPar   ( indParVariance_popPar );
  ok = model.indParVarianceInv_popPar( indParVarianceInv_popPar );


  //------------------------------------------------------------
  // See if cached values are used when the parameter changes.
  //------------------------------------------------------------

  // Evaluate the quantities at a different parameter value.
  // The cached values should not be used in this case.
  model.setPopPar( popParLow );

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
    "The cached value for sigma was used when it was not valid.",
    model.getUsedCachedSigma() == false );

  model.dataVarianceInv( dataVarianceInv );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVarianceInv was used when it was not valid.",
    model.getUsedCachedDataVarianceInv() == false );

  ok = model.dataMean_popPar( dataMean_popPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataMean_popPar was used when it was not valid.",
    model.getUsedCachedDataMean_popPar() == false );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached fAndH_theta value was used when it was not valid.",
    model.getUsedCachedFAndH_theta() == false );

  ok = model.dataMean_indPar( dataMean_indPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataMean_indPar was used when it was not valid.",
    model.getUsedCachedDataMean_indPar() == false );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached fAndH_eta value was used when it was not valid.",
    model.getUsedCachedFAndH_eta() == false );

  ok = model.dataVariance_popPar( dataVariance_popPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVariance_popPar was used when it was not valid.",
    model.getUsedCachedDataVariance_popPar() == false );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for sigma_sigmaPar was used when it was not valid.",
    model.getUsedCachedSigma_sigmaPar() == false );

  ok = model.dataVariance_indPar( dataVariance_indPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVariance_indPar was used when it was not valid.",
    model.getUsedCachedDataVariance_indPar() == false );

  ok = model.dataVarianceInv_popPar( dataVarianceInv_popPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVarianceInv_popPar was used when it was not valid.",
    model.getUsedCachedDataVarianceInv_popPar() == false );

  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVarianceInv_indPar was used when it was not valid.",
    model.getUsedCachedDataVarianceInv_indPar() == false );

  model.indParVariance( indParVariance );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for indParVariance was used when it was not valid.",
    model.getUsedCachedIndParVariance() == false );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omega was used when it was not valid.",
    model.getUsedCachedOmega() == false );

  model.indParVarianceInv( indParVarianceInv );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for indParVarianceInv was used when it was not valid.",
    model.getUsedCachedIndParVarianceInv() == false );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaInv was used when it was not valid.",
    model.getUsedCachedOmegaInv() == false );

  ok = model.indParVariance_popPar( indParVariance_popPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for indParVariance_popPar was used when it was not valid.",
    model.getUsedCachedIndParVariance_popPar() == false );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omega_omegaPar was used when it was not valid.",
    model.getUsedCachedOmega_omegaPar() == false );

  ok = model.indParVarianceInv_popPar( indParVarianceInv_popPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for indParVarianceInv_popPar was used when it was not valid.",
    model.getUsedCachedIndParVarianceInv_popPar() == false );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaInv_omegaPar was used when it was not valid.",
    model.getUsedCachedOmegaInv_omegaPar() == false );


  //------------------------------------------------------------
  // See if cached values are used when the parameter does not change.
  //------------------------------------------------------------

  // Evaluate the quantities at the same parameter value.
  // The cached values should be used in this case.
  model.setPopPar( popParLow );

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
    "The cached fAndH_theta were not used when they were valid.",
    model.getUsedCachedFAndH_theta() == true );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for sigma was not used when it was valid.",
    model.getUsedCachedSigma() == true );

  model.dataVarianceInv( dataVarianceInv );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVarianceInv was not used when it was valid.",
    model.getUsedCachedDataVarianceInv() == true );

  ok = model.dataMean_popPar( dataMean_popPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataMean_popPar was not used when it was valid.",
    model.getUsedCachedDataMean_popPar() == true );

  ok = model.dataMean_indPar( dataMean_indPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataMean_indPar was not used when it was valid.",
    model.getUsedCachedDataMean_indPar() == true );

  ok = model.dataVariance_popPar( dataVariance_popPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVariance_popPar was not used when it was valid.",
    model.getUsedCachedDataVariance_popPar() == true );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached fAndH_eta were not used when they were valid.",
    model.getUsedCachedFAndH_eta() == true );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for sigma_sigmaPar was not used when it was valid.",
    model.getUsedCachedSigma_sigmaPar() == true );

  ok = model.dataVariance_indPar( dataVariance_indPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVariance_indPar was not used when it was valid.",
    model.getUsedCachedDataVariance_indPar() == true );

  ok = model.dataVarianceInv_popPar( dataVarianceInv_popPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVarianceInv_popPar was not used when it was valid.",
    model.getUsedCachedDataVarianceInv_popPar() == true );

  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for dataVarianceInv_indPar was not used when it was valid.",
    model.getUsedCachedDataVarianceInv_indPar() == true );

  model.indParVariance( indParVariance );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for indParVariance was not used when it was valid.",
    model.getUsedCachedIndParVariance() == true );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omega was not used when it was valid.",
    model.getUsedCachedOmega() == true );

  model.indParVarianceInv( indParVarianceInv );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for indParVarianceInv was not used when it was valid.",
    model.getUsedCachedIndParVarianceInv() == true );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaInv was not used when it was valid.",
    model.getUsedCachedOmegaInv() == true );

  ok = model.indParVariance_popPar( indParVariance_popPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for indParVariance_popPar was not used when it was valid.",
    model.getUsedCachedIndParVariance_popPar() == true );

  ok = model.indParVarianceInv_popPar( indParVarianceInv_popPar );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for indParVarianceInv_popPar was not used when it was valid.",
    model.getUsedCachedIndParVarianceInv_popPar() == true );
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omegaInv was not used when it was valid.",
    model.getUsedCachedOmegaInv() == true );


  //------------------------------------------------------------
  // See if these cached values are not used when the parameter does not change.
  //------------------------------------------------------------

  // Because the cached value for indParVariance_indPar contains the
  // derivatives of omega, this cached value is not required and
  // therefore is not used in this case.
  CPPUNIT_ASSERT_MESSAGE( 
    "The cached value for omega_omegaPar was used when it should not have been.",
    model.getUsedCachedOmega_omegaPar() == false );
}

 
