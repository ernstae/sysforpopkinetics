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
 * File: PopPredModelBaseTest.cpp
 *
 *
 * Unit test for the class PopPredModelBase.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK Pred test suite header files.
#include "PopPredModelBaseTest.h"
#include "compareToKnown.h"

// SPK Pred library header files.
#include "../../../spkpred/DiagCov.h"
#include "../../../spkpred/FullCov.h"
#include "../../../spkpred/OdePredBase.h"
#include "../../../spkpred/PopPredModel.h"
#include "../../../spkpred/PopPredModelBase.h"

// SPK library header files.
#include <spk/AkronBtimesC.h>
#include <spk/doubleToScalarArray.h>
#include <spk/fitPopulation.h>
#include <spk/identity.h>
#include <spk/inverse.h>
#include <spk/mapObj.h>
#include <spk/multiply.h>
#include <spk/Objective.h>
#include <spk/Optimizer.h>
#include <spk/pi.h>
#include <spk/replaceSubblock.h>
#include <spk/scalarToDoubleArray.h>
#include <spk/simulate.h>
#include <spk/SpkValarray.h>
#include <spk/transpose.h>

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
 * Local variable declarations
 *------------------------------------------------------------------------*/

namespace oneexpf_onebolus_poppredmodelbasetest
{
  double timeStep = 0.25;
  double dose     = 320.0;
  double wt       = 79.6;
}

namespace railexample_poppredmodelbasetest
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
  // Class:  OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_OdePred
  //
  //
  // This class provides an ODE-based version of the Pred block
  // expression evaluator.
  //
  // For this test there is a single bolus at time zero and there is
  // an observation record at the same time that comes after the bolus
  // dose record in the data file, which means that calculated mass at
  // that time includes the mass added by the dose.
  //
  // In particular, it evaluates PK, DES, and ERROR block expressions
  // that correspond to a single exponential for the mean of the
  // individuals' data,
  //
  //           ds * exp[ -(theta(0) + eta(0)) * T ]
  //    f  =  --------------------------------------  ,
  //                 [theta(1) * w  +  eta(1)]
  //
  // and a combined model for the variance of the data that is additive
  // with a theta dependent term,
  //
  //    y  =  f + eps(0) + theta(2) * eps(1)  .
  //
  //**********************************************************************

  template<class Value>
  class OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_OdePred : public OdePredBase<Value>
  {
  public:
    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_OdePred(
      int                        nY_iIn,
      bool                       isPkBlockAFuncOfTIn,
      int                        nCompIn,
      int                        defaultDoseCompIn,
      int                        defaultObservCompIn,
      const std::valarray<bool>& compInitialOffIn,
      const std::valarray<bool>& compNoOffIn,
      const std::valarray<bool>& compNoDoseIn,
      double                     tolRelIn )
    :
    OdePredBase<Value> ( isPkBlockAFuncOfTIn,
                         nCompIn,
                         defaultDoseCompIn,
                         defaultObservCompIn,
                         compInitialOffIn,
                         compNoOffIn,
                         compNoDoseIn,
                         tolRelIn ),
    nY_i               ( nY_iIn ),
    nComp              ( nCompIn ),
    timeStep           ( oneexpf_onebolus_poppredmodelbasetest::timeStep ),
    dose               ( oneexpf_onebolus_poppredmodelbasetest::dose     ),
    wt                 ( oneexpf_onebolus_poppredmodelbasetest::wt       )
    {}

    ~OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_OdePred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;
    const int nComp;

  public:
    Value timeStep;
    Value dose;
    Value wt;

    Value ds;
    Value w;
    Value ke;


    //**********************************************************
    // 
    // Function: readDataRecord
    //
    //**********************************************************

  protected:
    void readDataRecord( int i, int j )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace oneexpf_onebolus_poppredmodelbasetest;


      //--------------------------------------------------------
      // Set the current values for the predefined data items.
      //--------------------------------------------------------

      // Set an arbitrary value for the observed value.
      this->setDV( 123456789.0 );

      // Set the type of event.
      if ( j == 0 )
      {
        //------------------------------------------------------
        // Instantaneous bolus dose event.
        //------------------------------------------------------

        // If this is the first time point, set these flags to
        // indicate this is a dose event.
        this->setMDV ( 1 );
        this->setEVID( this->DOSE_EVENT );

        // Set the compartment number.
        this->setCMT( 1 );

        // Save the values for the weighted dose and the weight.
        ds = dose * wt;
        w = wt;

        // Set the amount for this instantaneous bolus dose.
        this->setAMT( ds );

        // Set the time for the dose.
        this->setTIME( 0.0 );
      }
      else
      {
        //------------------------------------------------------
        // Observation events.
        //------------------------------------------------------

        // If this is not the first data record odd, then set these
        // flags to indicate this is an observation event.
        this->setMDV ( 0 );
        this->setEVID( this->OBSERV_EVENT );

        // Set the compartment number.
        this->setCMT( 1 );

        // Set the time for the observations.
        this->setTIME( timeStep * ( j - 1 ) );
      }
    }


    //**********************************************************
    // 
    // Function: evalPk
    //
    //**********************************************************

    void evalPk(
      int thetaOffset, int thetaLen,
      int etaOffset,   int etaLen,
      int i,
      int j,
      const std::vector<Value>& indepVar )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace oneexpf_onebolus_poppredmodelbasetest;


      //--------------------------------------------------------
      // Set the current values for the Pk block parameters.
      //--------------------------------------------------------

      int p = 0;
      Value compScaleParam_p;

      // Calculate the PK parameters for
      //
      //           ds * exp[ -(theta(0) + eta(0)) * T ]
      //    f  =  --------------------------------------  ,
      //                [theta(1) * w  +  eta(1)]
      //
      ke               = indepVar[thetaOffset + 0] + indepVar[etaOffset + 0];
      compScaleParam_p = indepVar[thetaOffset + 1] * w + indepVar[etaOffset + 1];

      setCompScaleParam( p, compScaleParam_p );
    }


    //**********************************************************
    // 
    // Function: evalDes
    //
    //**********************************************************

    void evalDes(
      int thetaOffset, int thetaLen,
      int i,
      int j,
      const std::vector<Value>& indepVar )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace oneexpf_onebolus_poppredmodelbasetest;


      //--------------------------------------------------------
      // Evaluate the differential equations.
      //--------------------------------------------------------

      // Get the current amount in compartment 1.
      int p = 0;
      Value compAmount_p;
      getCompAmount( p, compAmount_p );

      // Since
      //
      //           ds * exp[ -(theta(0) + eta(0)) * T ]
      //    f  =  --------------------------------------  .
      //                [theta(1) * w  +  eta(1)]
      //
      // the derivative with respect to T of f is equal to
      //
      //     d              - (theta(0) + eta(0)) * ds * exp[ -(theta(0) + eta(0)) * T ]
      //    ----- f(T)  =  -------------------------------------------------------------  .
      //     d T                          [theta(1) * w  +  eta(1)]
      //
      //                =  - (theta(0) + eta(0)) * f(T)
      //
      //                =  - ke * f(T)
      //
      // Note that f represents the amount in compartment 1 as a
      // function of T,
      //
      //    f(T)  =  A (T) .
      //              1
      //
      Value compAmount_t_p;
      compAmount_t_p = - ke * compAmount_p;
      setCompAmount_t( p, compAmount_t_p );
    }


    //**********************************************************
    // 
    // Function: evalError
    //
    //**********************************************************

    void evalError(
      int thetaOffset, int thetaLen,
      int etaOffset,   int etaLen,
      int epsOffset,   int epsLen,
      int i,
      int j,
      const std::vector<Value>& indepVar )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace oneexpf_onebolus_poppredmodelbasetest;


      //--------------------------------------------------------
      // Set the current values for the intra-individual error.
      //--------------------------------------------------------

      Value f_i_j;
      Value y_i_j;

      getF( f_i_j );

      // Set
      //
      //    y  =  f + eps(0) + theta(2) * eps(1)  .
      //
      y_i_j = f_i_j + indepVar[epsOffset + 0]
        + indepVar[thetaOffset + 2] * indepVar[epsOffset + 1];
      setY( y_i_j );
    }


    //**********************************************************
    // 
    // Function: getNRecords
    //
    //**********************************************************

  public:
    virtual int getNRecords( int i ) const
    {
      return 1     +   // Instantanous bolus dose records.
             nY_i;     // Observation records.
    }


    //**********************************************************
    // 
    // Function: getNObservs
    //
    //**********************************************************

    int getNObservs( int i ) const
    {
      return nY_i;     // Observation records.
    }


    //------------------------------------------------------------
    // Disallowed, implicitly generated member functions.
    //------------------------------------------------------------

  protected:
    OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_OdePred(){}
    OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_OdePred( const OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_OdePred& ){}
    OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_OdePred & operator=( const OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_OdePred& ){}
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
  // to population notation is
  //
  //     $PRED 
  //     F = THETA(1) + ETA(1)
  //     Y = F + EPS(1)
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

      using namespace railexample_poppredmodelbasetest;


      //--------------------------------------------------------
      // Evaluate the mean of the data and its predicted value.
      //--------------------------------------------------------

      // If this is not the tenth time this function has
      // been called, then set
      //           
      //    f  =  theta(0) + eta(0)  ,
      //
      // and
      //
      //    y  =  f + eps(0)  .
      //
      nEvalCall++;
      if ( nEvalCall != 10 )
      {
        depVar[fOffset + j] = indepVar[thetaOffset + 0] + indepVar[etaOffset + 0];
      }
      else
      {
        // If this is the tenth time this function has been called,
        // then set f to be NaN to make the optimizer back up.
        Value zero = Value( 0 );
        depVar[fOffset + j] = zero / zero;
      }
      depVar[yOffset + j] = depVar[fOffset + j] + indepVar[epsOffset + 0]; 


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

void PopPredModelBaseTest::setUp()
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

void PopPredModelBaseTest::tearDown()
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

Test* PopPredModelBaseTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "PopPredModelBaseTest" );

  suiteOfTests->addTest(new TestCaller<PopPredModelBaseTest>(
    "OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_Test", 
    &PopPredModelBaseTest::OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_Test ));

  suiteOfTests->addTest(new TestCaller<PopPredModelBaseTest>(
    "RailExample_OptimizerBackup_Test", 
    &PopPredModelBaseTest::RailExample_OptimizerBackup_Test ));

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_Test
 *
 *
 * The goal of this test is to check that the ODE-based version of the
 * Pred block expression evaluator works for the case of
 *
 *     OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_OdePred.
 *
 *************************************************************************/

void PopPredModelBaseTest::OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpf_onebolus_poppredmodelbasetest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of data values for this individual.
  int nY_iKnown = 5;

  // Set this equal to false since the PK block expressions are not
  // functions of T.
  bool isPkBlockAFuncOfTime = false;

  // Set the number of compartments, including the output compartment.
  int nComp = 2;

  // Set the default compartments for doses and observations.
  int defaultDoseComp   = 1;
  int defaultObservComp = 1;

  // These flags indicate which compartments are initially off, cannot
  // be turned off, and cannot receive a dose.
  std::valarray<bool> compInitialOff( nComp );
  std::valarray<bool> compNoOff     ( nComp );
  std::valarray<bool> compNoDose    ( nComp );

  // Set the flags for compartment 1.
  compInitialOff[1 - 1] = false;
  compNoOff     [1 - 1] = false;
  compNoDose    [1 - 1] = false;

  // Set the flags for compartment 2 (the output compartment).
  compInitialOff[2 - 1] = true;
  compNoOff     [2 - 1] = false;
  compNoDose    [2 - 1] = true;

  // Set the relative tolerance for the ODE integration.
  double tolRel = 1.0e-6;

  // Construct the ODE-based versions of the Pred block evaluator.
  OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_OdePred< AD<double> > predEvaluatorAD(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_OdePred< AD< AD<double> > > predEvaluatorADAD(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_dataMean_OdePred< AD< AD< AD<double> > > > predEvaluatorADADAD(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );


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

  PopPredModelBase< AD<double> > model(
    predEvaluatorAD,
    predEvaluatorADAD,
    predEvaluatorADADAD,
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
  int nY_i = predEvaluatorAD.getNObservs( iCurr );


  //------------------------------------------------------------
  // Prepare various quantities for the test.
  //------------------------------------------------------------

  valarray< AD<double> > alphaCurrAD      ( nPopPar );
  valarray< AD<double> > bCurrAD          ( nIndPar );
  valarray< AD<double> > dataMeanAD       ( nY_i );
  valarray< AD<double> > dataMean_popParAD( nY_i * nPopPar );

  valarray<double> alphaCurr      ( nPopPar );
  valarray<double> bCurr          ( nIndPar );
  valarray<double> dataMean       ( nY_i );
  valarray<double> dataMean_popPar( nY_i * nPopPar );

  // Get the current value for the population parameter.
  model.getPopPar( alphaCurrAD );

  // Get the current value for the individual parameter.
  model.getIndPar( bCurrAD );

  // Get a double version of the population parameters.
  scalarToDoubleArray( alphaCurrAD, alphaCurr );

  // Get a double version of the individual parameters.
  scalarToDoubleArray( bCurrAD, bCurr );


  //------------------------------------------------------------
  // Calculate the data mean and its derivative.
  //------------------------------------------------------------

  // Start recording by declaring the independent variables.
  //
  // When the model function is evaluated, independent variables that
  // are at higher levels of taping than this function's independent
  // variables will be used, e.g., AD< AD<double> >.
  CppAD::Independent( alphaCurrAD );

  // Set the model's population parameter equal to this function's
  // population parameter after it has been declared the independent
  // variable for this function.
  //
  // This is done so that the model function calculations are
  // performed using this function's independent variable, which will
  // allow for their derivatives to be calculated in this function.
  model.setPopPar( alphaCurrAD );

  // Evaluate the model function.
  model.dataMean( dataMeanAD );

  // Stop recording at this level by constructing a differentiable
  // function object for the model function.
  CppAD::ADFun<double> dataMeanADFun( alphaCurrAD, dataMeanAD );

  // Calculate the derivative of the model function.
  //
  // Note that the elements of the derivative returned by the Jacobian
  // function are in row major order.
  dataMean_popPar = dataMeanADFun.Jacobian( alphaCurr );

  // Put the derivative elements in column major order.
  dataMean_popPar = transpose( dataMean_popPar, nY_i );


  //------------------------------------------------------------
  // Convert the calculated values to the type used for the tests.
  //------------------------------------------------------------

  scalarToDoubleArray( dataMeanAD, dataMean );


  //------------------------------------------------------------
  // Prepare known values related to the population parameter.
  //------------------------------------------------------------

  valarray<double> omegaCurr( nEta * nEta );

  // Create a covariance matrix that is equal to the one that the
  // OdePredBase is maintaining internally.
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
  // OdePredBase is maintaining internally.
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

  double T;
  double ds;
  double w;
  double ka;
  double ke;
  double cl;
  double d;
  double e;

  // For this test there is a single bolus at time zero and there is
  // an observation record at the same time that comes after the bolus
  // dose record in the data file, which means that calculated mass at
  // that time includes the mass added by the dose.
  //
  // Calculate the known value for the mean of the data:
  //
  //                                      -                                  -   
  //                                     |                                    |
  //                            ds * exp |  - [ theta    + eta    ]  *  T     |
  //                                     |           (0)      (0)        (j)  |
  //                                      -                                  -   
  //    f    ( alpha, b  )  =  -------------------------------------------------  ,
  //     i(j)          i
  //                                      [ theta    * w   +  eta    ]
  //                                             (1)             (1)
  //
  //
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time taking into account the fact that the observations
    // start at time zero.
    T = timeStep * j;
  
    ds = dose * wt;
    w = wt;

    ke = thetaCurr[0] + etaCurr[0];
    cl = thetaCurr[1] * w + etaCurr[1];
    d = std::exp(-ke * T );
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
    //     d       f    ( alpha, b  )  =  - T     f    ( alpha, b  )  .
    //      theta   i(j)          i          (j)   i(j)          i
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - timeStep * j * dataMeanKnown[j];
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

    dataMean_popParKnown[j + col * nY_i] = - wt * dataMeanKnown[j] /
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
    //     d     f    ( alpha, b  )  =  - T     f    ( alpha, b  )  .
    //      eta   i(j)          i          (j)   i(j)          i
    //
    dataMean_indParKnown[j + k * nY_i] = - timeStep * j * dataMeanKnown[j];
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
    dataMean_indParKnown[j + k * nY_i] = - dataMeanKnown[j] /
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

  // Calculate the known value for the derivative of the variance
  // of the data with respect to the individual parameter.
  dataVariance_indParKnown = 0.0;

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
  

  //------------------------------------------------------------
  // Compare the calculated and known values.
  //------------------------------------------------------------

  double tol = 1.0e-14;

  // Set the tolerance for the model function comparisions to be 
  // the tolerance for the integration of the ODE's.
  tol = tolRel;

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
 * to population notation is
 *
 *     $PRED 
 *     F = THETA(1) + ETA(1)
 *     Y = F + EPS(1)
 *
 *************************************************************************/

void PopPredModelBaseTest::RailExample_OptimizerBackup_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace railexample_poppredmodelbasetest;

  int i;
  int j;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluators.
  //------------------------------------------------------------

  RailExample_Pred< double > predEvaluator( nY_i );

  RailExample_Pred< AD<double> > predEvaluatorAD( nY_i );

  RailExample_Pred< AD< AD<double> > > predEvaluatorADAD( nY_i );

  RailExample_Pred< AD< AD< AD<double> > > > predEvaluatorADADAD( nY_i );


  //------------------------------------------------------------
  // Prepare the population model variables that appear in the Pred block.
  //------------------------------------------------------------

  // Set the number of population model independent variables.
  const int nTheta = 1;
  const int nEta   = nTheta;
  const int nEps   = 1;

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
  // Initialize quantities related to the population covariance matrices.
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

  // Set the structure of sigma, the covariance matrix for eps.
  covStruct sigmaStruct = DIAGONAL;

  // Construct sigma.
  DiagCov sigma( nEps );

  // Set the number elements for this parameterization.
  int nSigmaPar = sigma.getNPar();

  // Set the diagonal elements for the initial value for Sigma.
  valarray<double> sigmaMinRep( nSigmaPar );
  sigmaMinRep[0] = 32.0;
  assert( nSigmaPar == 1 );


  //------------------------------------------------------------
  // Prepare the population level Pred models.
  //------------------------------------------------------------

  PopPredModel popModel(
    predEvaluator,
    predEvaluatorAD,
    predEvaluatorADAD,
    nTheta,
    thetaLow,
    thetaUp,
    thetaIn,
    nEta,
    etaIn,
    nEps,
    omegaStruct,
    omegaMinRep,
    sigmaStruct,
    sigmaMinRep );

  PopPredModelBase< AD< double > > popModelAD(
    predEvaluatorAD,
    predEvaluatorADAD,
    predEvaluatorADADAD,
    nTheta,
    thetaLow,
    thetaUp,
    thetaIn,
    nEta,
    etaIn,
    nEps,
    omegaStruct,
    omegaMinRep,
    sigmaStruct,
    sigmaMinRep );


  //------------------------------------------------------------
  // Quantities related to the population parameter, alp.
  //------------------------------------------------------------

  // Get the number of population parameters.
  const int nAlp = popModel.getNPopPar();

  assert( nAlp == nTheta + nOmegaPar + nSigmaPar );

  valarray<double> alpLow ( nAlp );
  valarray<double> alpUp  ( nAlp );
  valarray<double> alpStep( nAlp );
  valarray<double> alpIn  ( nAlp );

  // Get the current value for the population parameters.
  popModel.getPopPar( alpIn );

  // Get the limits for the population parameters.
  popModel.getPopParLimits( alpLow, alpUp );

  // Get the step sizes for the population parameters.
  popModel.getPopParStep( alpStep );

  // This will hold all of the populations' parameter values.  
  valarray<double> alpOut( nAlp );


  //------------------------------------------------------------
  // Quantities related to the individual parameters, b.
  //------------------------------------------------------------

  // Get the number of individual parameters.
  const int nB = popModel.getNIndPar();
  assert( nB == nEta );

  valarray<double> bLow ( nB );
  valarray<double> bUp  ( nB );
  valarray<double> bStep( nB );
  valarray<double> bIn_i( nB );
  valarray<double> bAllIn( nB * nInd );

  // Get the current value for the individual parameters.
  popModel.getIndPar( bIn_i );

  // Set all of the individuals' initial parameter values.
  for ( i = 0; i < nInd; i++ )
  {
    bAllIn[ slice( i * nB, nB, 1 ) ] = bIn_i;
  }

  // Get the limits for the individual parameters.
  popModel.getIndParLimits( bLow, bUp );

  // Get the step sizes for the individual parameters.
  popModel.getIndParStep( bStep );

  // This will hold all of the individuals' parameter values.  
  valarray<double> bAllOut( nB * nInd );


  //------------------------------------------------------------
  // Remaining inputs to fitPopulation.
  //------------------------------------------------------------

  // Choose the population method to use.
  enum Objective method = MODIFIED_LAPLACE;

  // Set the values for optimization of the individual objective
  // functions.
  double indEpsilon = 1.e-3; 
  int indNMaxIter   = 50; 
  int indLevel      = 0;
  Optimizer indOptInfo( indEpsilon, indNMaxIter, indLevel ); 

  // Set the values for optimization of the population objective
  // function.
  double popEpsilon = 1.e-3; 
  int popNMaxIter   = 50; 
  int popLevel      = 0;
  Optimizer popOptInfo( popEpsilon, popNMaxIter, popLevel ); 


  //------------------------------------------------------------
  // Perform the population estimation method.
  //------------------------------------------------------------

  double* pdNull = 0;
  valarray<double>* pVANull = 0;

  // Set the parallel controls object
  bool isUsingPvm    = false;
  bool isPvmParallel = false;
  DirBasedParallelControls parallelControls( false, 0, 0 );

  try
  {
    fitPopulation( popModel,
                   popModelAD,
                   method,
                   N,
                   Y,
                   popOptInfo,
                   alpLow,
                   alpUp,
                   alpIn,
                   alpStep,
                   &alpOut,
                   indOptInfo,
                   bLow,
                   bUp,
                   bAllIn,            
                   bStep,
                   &bAllOut,
                   pdNull,
                   pVANull,
                   pVANull,
                   isUsingPvm,
                   isPvmParallel,
                   parallelControls );
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

    // See if the population optimizer backed up warning message was issued.
    msgPos = warnings.find( "Backed up population optimization", 0 );
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
      "An unexpected exception occurred during the evaluation of the data mean.",
      false );
  }

}

