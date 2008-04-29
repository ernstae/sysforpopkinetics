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
 * File: OdePredBaseTest.cpp
 *
 *
 * Unit test for the class OdePredBase.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include Files
 *------------------------------------------------------------------------*/

// SPK Pred test suite header files.
#include "OdePredBaseTest.h"
#include "compareToKnown.h"

// SPK Pred library header files.
#include "../../../spkpred/DiagCov.h"
#include "../../../spkpred/FullCov.h"
#include "../../../spkpred/OdePredBase.h"
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

namespace oneexpf_onebolus_odepredbasetest
{
  double timeStep = 0.25;
  double dose     = 320.0;
  double wt       = 79.6;
}

namespace threecomp_onebolus_odepredbasetest
{
  double timeStep = 0.25;

  double bolus1 = 200.0;
  double bolus2 = 100.0;
}

namespace fourcomp_multinfus_odepredbasetest
{
  double timeStep = 0.25;

  // Set the amount and rate so that the duration of the regular
  // infusion is equal to 1.
  double amount = 1.0;
  double rate   = 1.0;
}


/*------------------------------------------------------------------------
 * Local class declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{

  //**********************************************************************
  //
  // Class:  NoEta_OneExpF_OneBolus_ModelBasedExpY_OdePred
  //
  //
  // This class provides an ODE-based version of the Pred block
  // expression evaluator.
  //
  // For this test there is a single bolus at time zero.
  //
  // In particular, it evaluates PK, DES, and ERROR block expressions
  // that correspond to a single exponential for the mean of the
  // individuals' data without any eta elements,
  //
  //           ds * exp[ - theta(0) * T ]
  //    f  =  ----------------------------  ,
  //               [theta(1) * w]
  //
  // and model based weighting of the data using an exponential 
  // parameterization,
  //
  //    y  =  f * exp[ eps(0) ]  .
  //
  //**********************************************************************

  template<class Value>
  class NoEta_OneExpF_OneBolus_ModelBasedExpY_OdePred : public OdePredBase<Value>
  {
  public:

 //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    NoEta_OneExpF_OneBolus_ModelBasedExpY_OdePred( 
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
    nComp              ( nCompIn )
    {}

    ~NoEta_OneExpF_OneBolus_ModelBasedExpY_OdePred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;
    const int nComp;

  public:
    Value ds;
    Value w;
    Value ke;


    //**********************************************************
    // 
    // Function: readDataRecord
    //
    //**********************************************************

    void readDataRecord( int i, int j )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace oneexpf_onebolus_odepredbasetest;


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

        // Save the values for the weighted dose and the weight.
        ds = dose * wt;
        w = wt;

        // Set the amount for this instantaneous bolus dose.
        this->setAMT( ds );
      }
      else
      {
        //------------------------------------------------------
        // Observation events.
        //------------------------------------------------------

        // Set these flags to indicate this is an observation event.
        this->setMDV ( 0 );
        this->setEVID( this->OBSERV_EVENT );
      }

      // Set the compartment number.
      this->setCMT( 1 );

      // Set the time.
      this->setTIME( timeStep * j );
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

      using namespace oneexpf_onebolus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the Pk block parameters.
      //--------------------------------------------------------

      int p = 0;
      Value compScaleParam_p;

      // Calculate the PK parameters for
      //
      //           ds * exp[ - theta(0) * T ]
      //    f  =  ----------------------------  .
      //                [theta(1) * w]
      //
      ke               = indepVar[thetaOffset + 0];
      compScaleParam_p = indepVar[thetaOffset + 1] * w;

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

      using namespace oneexpf_onebolus_odepredbasetest;


      //--------------------------------------------------------
      // Evaluate the differential equations.
      //--------------------------------------------------------

      // Get the current amount in compartment 1.
      int p = 0;
      Value compAmount_p;
      getCompAmount( p, compAmount_p );

      // Since
      //
      //              ds * exp[ - theta(0) * T ]
      //    f(T)  =  ----------------------------  ,
      //                   [theta(1) * w]
      //
      // the derivative with respect to T of f is equal to
      //
      //     d                 - theta(0) * ds * exp[ - theta(0) * T ]
      //    -------- f(T)  =  --------------------------------------------  
      //     d T                         [theta(1) * w]
      //
      //                   =  - theta(0) * f(T)
      //
      //                   =  - ke * f(T)
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

      using namespace oneexpf_onebolus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the intra-individual error.
      //--------------------------------------------------------

      Value f_i_j;
      Value y_i_j;

      getF( f_i_j );

      // Set
      //
      //    y  =  f * exp[ eps(0) ]  .
      //
      y_i_j = f_i_j * CppAD::exp(indepVar[epsOffset + 0]);
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
    NoEta_OneExpF_OneBolus_ModelBasedExpY_OdePred(){}
    NoEta_OneExpF_OneBolus_ModelBasedExpY_OdePred( const NoEta_OneExpF_OneBolus_ModelBasedExpY_OdePred& ){}
    NoEta_OneExpF_OneBolus_ModelBasedExpY_OdePred & operator=( const NoEta_OneExpF_OneBolus_ModelBasedExpY_OdePred& ){}
  };


  //**********************************************************************
  //
  // Class:  NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_OdePred
  //
  //
  // This class provides an ODE-based version of the Pred block
  // expression evaluator.
  //
  // For this test there is a single bolus at time zero, and one of the
  // data record's time values is out of order.
  //
  // In particular, it evaluates PK, DES, and ERROR block expressions
  // that correspond to a single exponential for the mean of the
  // individuals' data without any eta elements,
  //
  //           ds * exp[ - theta(0) * T ]
  //    f  =  ----------------------------  ,
  //               [theta(1) * w]
  //
  // and model based weighting of the data using an exponential 
  // parameterization,
  //
  //    y  =  f * exp[ eps(0) ]  .
  //
  //**********************************************************************

  template<class Value>
  class NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_OdePred : public OdePredBase<Value>
  {
  public:

    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_OdePred( 
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
    nComp              ( nCompIn )
    {}

    ~NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_OdePred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;
    const int nComp;

  public:
    Value ds;
    Value w;
    Value ke;


    //**********************************************************
    // 
    // Function: readDataRecord
    //
    //**********************************************************

    void readDataRecord( int i, int j )
    {
      //--------------------------------------------------------
      // Preliminaries.
      //--------------------------------------------------------

      using namespace oneexpf_onebolus_odepredbasetest;


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

        // Save the values for the weighted dose and the weight.
        ds = dose * wt;
        w = wt;

        // Set the amount for this instantaneous bolus dose.
        this->setAMT( ds );
      }
      else
      {
        //------------------------------------------------------
        // Observation events.
        //------------------------------------------------------

        // Set these flags to indicate this is an observation event.
        this->setMDV ( 0 );
        this->setEVID( this->OBSERV_EVENT );
      }

      // Set the compartment number.
      this->setCMT( 1 );

      // Set the time.
      if ( j != 2 )
      {
        this->setTIME( timeStep * j );
      }
      else
      {
        // Set this data record's time equal to a value slightly
        // before the previous time to force an error in
        // getExpDesign().
        this->setTIME( timeStep * ( j - 1 ) - timeStep * 0.01 );
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

      using namespace oneexpf_onebolus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the Pk block parameters.
      //--------------------------------------------------------

      int p = 0;
      Value compScaleParam_p;

      // Calculate the PK parameters for
      //
      //           ds * exp[ - theta(0) * T ]
      //    f  =  ----------------------------  .
      //                [theta(1) * w]
      //
      ke               = indepVar[thetaOffset + 0];
      compScaleParam_p = indepVar[thetaOffset + 1] * w;

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

      using namespace oneexpf_onebolus_odepredbasetest;


      //--------------------------------------------------------
      // Evaluate the differential equations.
      //--------------------------------------------------------

      // Get the current amount in compartment 1.
      int p = 0;
      Value compAmount_p;
      getCompAmount( p, compAmount_p );

      // Since
      //
      //              ds * exp[ - theta(0) * T ]
      //    f(T)  =  ----------------------------  ,
      //                   [theta(1) * w]
      //
      // the derivative with respect to T of f is equal to
      //
      //     d                 - theta(0) * ds * exp[ - theta(0) * T ]
      //    -------- f(T)  =  --------------------------------------------  
      //     d T                         [theta(1) * w]
      //
      //                   =  - theta(0) * f(T)
      //
      //                   =  - ke * f(T)
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

      using namespace oneexpf_onebolus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the intra-individual error.
      //--------------------------------------------------------

      Value f_i_j;
      Value y_i_j;

      getF( f_i_j );

      // Set
      //
      //    y  =  f * exp[ eps(0) ]  .
      //
      y_i_j = f_i_j * CppAD::exp(indepVar[epsOffset + 0]);
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
    NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_OdePred(){}
    NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_OdePred( const NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_OdePred& ){}
    NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_OdePred & operator=( const NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_OdePred& ){}
  };


  //**********************************************************************
  //
  // Class:  OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred
  //
  //
  // This class provides an ODE-based version of the Pred block
  // expression evaluator.
  //
  // For this test there is a single bolus at time zero and there are
  // non observation predication evaluation events.
  //
  // In particular, it evaluates PK, DES, and ERROR block expressions
  // that correspond to a single exponential for the mean of the
  // individuals' data,
  //
  //           ds * exp[ -(theta(0) + eta(0)) * T ]
  //    f  =  --------------------------------------  ,
  //                [theta(1) * w  +  eta(1)]
  //
  // and a combined model for the variance of the data that is additive
  // with a theta dependent term,
  //
  //    y  =  f + eps(0) + theta(2) * eps(1)  .
  //
  //**********************************************************************

  template<class Value>
  class OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred : public OdePredBase<Value>
  {
  public:
    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred(
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
    nComp              ( nCompIn )
    {}

    ~OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;
    const int nComp;

  public:
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

      using namespace oneexpf_onebolus_odepredbasetest;


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
      }
      else if ( j / 2 * 2 != j )
      {
        //------------------------------------------------------
        // Observation events.
        //------------------------------------------------------

        // if j is odd, then set these flags to indicate this is an
        // observation event.
        this->setMDV ( 0 );
        this->setEVID( this->OBSERV_EVENT );

        // Set the compartment number.
        this->setCMT( 1 );
      }
      else
      {
        //------------------------------------------------------
        // Nonobservation prediction events.
        //------------------------------------------------------

        // Set these flags to indicate this is a prediction-at-a-
        // nonobservation-time other-type event.
        this->setMDV ( 1 );
        this->setEVID( this->OTHER_TYPE_EVENT );

        // Set the compartment number equal to zero so that none of
        // the compartments' states will change.
        this->setCMT( 0 );

        // Set the prediction compartment number.
        this->setPCMT( 1 );
      }

      // Set the time.
      this->setTIME( timeStep * j );
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

      using namespace oneexpf_onebolus_odepredbasetest;


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

      using namespace oneexpf_onebolus_odepredbasetest;


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

      using namespace oneexpf_onebolus_odepredbasetest;


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
             nY_i  +   // Observation records.
             nY_i;     // Nonobservation prediction records.
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
    OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred(){}
    OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred( const OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred& ){}
    OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred & operator=( const OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred& ){}
  };


  //**********************************************************************
  //
  // Class:  ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred
  //
  //
  // This class provides an ODE-based version of the Pred block
  // expression evaluator.
  //
  // For this test there are three occasions all having the identical
  // experimental design: there is a single bolus at time zero and
  // there are non observation predication evaluation events.
  //
  // In particular, it evaluates PK, DES, and ERROR block expressions
  // that correspond to a single exponential for the mean of the
  // individuals' data,
  //
  //           ds * exp[ -(theta(0) + eta(0)) * T ]
  //    f  =  --------------------------------------  ,
  //                [theta(1) * w  +  eta(1)]
  //
  // and a combined model for the variance of the data that is additive
  // with a theta dependent term,
  //
  //    y  =  f + eps(0) + theta(2) * eps(1)  .
  //
  //**********************************************************************

  template<class Value>
  class ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred : public OdePredBase<Value>
  {
  public:
    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred(
      int                        nY_iIn,
      int                        nOccasionIn,
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
    nOccasion          ( nOccasionIn ),
    nRecordsPerOccasion( 1                    +  // Instantanous bolus dose records.
                         nY_iIn / nOccasionIn +  // Observation records.
                         nY_iIn / nOccasionIn +  // Nonobservation prediction records.
                         1 )                     // Reset record.
    {}

    ~ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;
    const int nComp;
    const int nOccasion;
    const int nRecordsPerOccasion;

  public:
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

      using namespace oneexpf_onebolus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the predefined data items.
      //--------------------------------------------------------

      // Set an arbitrary value for the observed value.
      this->setDV( 123456789.0 );

      // Set the position this record would have if there were only
      // one occasion.
      int jEffective = j % nRecordsPerOccasion;

      // Set the type of event.
      //
      // Note that this test has multiple occasions with the
      // identical experimental design.
      if ( jEffective == 0 )
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

        // Set the time.
        this->setTIME( 0 );
      }
      else if ( jEffective == nRecordsPerOccasion - 1 )
      {
        //------------------------------------------------------
        // Reset events.
        //------------------------------------------------------

        // If this is the last record for this occasion, then set
        // these flags to indicate this is a reset event.
        this->setMDV ( 1 );
        this->setEVID( this->RESET_EVENT );

        // Set the time.
        this->setTIME( 0 );
      }
      else if ( jEffective / 2 * 2 != jEffective)
      {
        //------------------------------------------------------
        // Observation events.
        //------------------------------------------------------

        // If jEffective is odd, then set these flags to
        // indicate this is an observation event.
        this->setMDV ( 0 );
        this->setEVID( this->OBSERV_EVENT );

        // Set the compartment number.
        this->setCMT( 1 );

        // Set the time.
        this->setTIME( timeStep * jEffective );
      }
      else
      {
        //------------------------------------------------------
        // Nonobservation prediction events.
        //------------------------------------------------------

        // Set these flags to indicate this is a prediction-at-a-
        // nonobservation-time other-type event.
        this->setMDV ( 1 );
        this->setEVID( this->OTHER_TYPE_EVENT );

        // Set the compartment number equal to zero so that none of
        // the compartments' states will change.
        this->setCMT( 0 );

        // Set the prediction compartment number.
        this->setPCMT( 1 );

        // Set the time.
        this->setTIME( timeStep * jEffective );
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

      using namespace oneexpf_onebolus_odepredbasetest;


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

      using namespace oneexpf_onebolus_odepredbasetest;


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

      using namespace oneexpf_onebolus_odepredbasetest;


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
      return nOccasion     +    // Instantanous bolus dose records
                                // (one per occasion).
             nY_i          +    // Observation records.
             nY_i          +    // Nonobservation prediction records.
             nOccasion;         // Reset event records at end of each 
                                // occasion.
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
    ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred(){}
    ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred( const ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred& ){}
    ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred & operator=( const ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred& ){}
  };


  //**********************************************************************
  //
  // Class:  OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_OdePred
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
  class OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_OdePred : public OdePredBase<Value>
  {
  public:
    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_OdePred(
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
    nComp              ( nCompIn )
    {}

    ~OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_OdePred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;
    const int nComp;

  public:
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

      using namespace oneexpf_onebolus_odepredbasetest;


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

      using namespace oneexpf_onebolus_odepredbasetest;


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

      using namespace oneexpf_onebolus_odepredbasetest;


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

      using namespace oneexpf_onebolus_odepredbasetest;


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
    OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_OdePred(){}
    OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_OdePred( const OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_OdePred& ){}
    OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_OdePred & operator=( const OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_OdePred& ){}
  };


  //**********************************************************************
  //
  // Class:  ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OdePred
  //
  //
  // This class provides an ODE-based version of the Pred block
  // expression evaluator.
  //
  // For this test, compartments 1 and 2 receive bolus doses at time
  // zero (t = 0).
  //
  // In particular, it evaluates PK, DES, and ERROR block expressions
  // that correspond to the following model for the mean of the
  // individuals' data,
  //
  //     f    (theta, eta)  =  A1(t)  +  A2(t)
  //      i(j)
  //
  //                                    - k1 * t             - k2 * t
  //                        =  bolus1 e           +  bolus2 e          ,
  //
  // where
  //
  //     k1  =  theta1 + eta1 ,
  //
  //     k2  =  theta2 + eta2 ,
  //
  //     t  =  ( j + 1 ) * timeStep  ,
  //
  // and that correspond to model based weighting of the data using
  // an exponential parameterization,
  //
  //    y  =  f * exp[ eps(0) ]  
  //
  //       =  [ A1(t) + A2(t) ] * exp[ eps(0) ]  .
  //
  //**********************************************************************

  template<class Value>
  class ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OdePred : public OdePredBase<Value>
  {
  public:
    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OdePred(
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
    nComp              ( nCompIn )
    {}

    ~ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OdePred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;
    const int nComp;

  public:
    Value k1;
    Value k2;


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

      using namespace threecomp_onebolus_odepredbasetest ;


      //--------------------------------------------------------
      // Set the current values for the predefined data items.
      //--------------------------------------------------------

      // Set an arbitrary value for the observed value.
      this->setDV( 123456789.0 );

      // Set the type of event.
      if ( j < 2 )
      {
        //------------------------------------------------------
        // Instantaneous bolus dose events.
        //------------------------------------------------------

        // If this is one of the first 2 data records, then set these
        // flags to indicate this is a dose event.
        this->setMDV ( 1 );
        this->setEVID( this->DOSE_EVENT );

        // Set the amount for this instantaneous bolus dose.
        if ( j == 0  )
        {
          this->setAMT( bolus1 );
          this->setCMT( 1 );
        }
        else
        {
          this->setAMT( bolus2 );
          this->setCMT( 2 );
        }

        // Set the time for all of the infusions to be time zero.
        this->setTIME( 0.0 );
      }
      else
      {
        //------------------------------------------------------
        // Observation events.
        //------------------------------------------------------

        // Set these flags to indicate this is an observation event.
        this->setMDV ( 0 );
        this->setEVID( this->OBSERV_EVENT );

        // Set the time to be greater than zero.
        this->setTIME( ( j - 1 ) * timeStep );
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

      using namespace threecomp_onebolus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the Pk block parameters.
      //--------------------------------------------------------

        k1 = indepVar[thetaOffset + 0] + indepVar[etaOffset + 0];
        k2 = indepVar[thetaOffset + 1] + indepVar[etaOffset + 1];
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

      using namespace threecomp_onebolus_odepredbasetest;


      //--------------------------------------------------------
      // Evaluate the differential equations.
      //--------------------------------------------------------

      // Get the current amount in compartment 1.
      Value compAmount1;
      getCompAmount( 0, compAmount1 );

      // Get the current amount in compartment 2.
      Value compAmount2;
      getCompAmount( 1, compAmount2 );

      // The derivatives with respect to T of each compartment are
      //
      //     d   A1(t)  =  - k1 * A1(t)  ,
      //      t
      //
      //     d   A2(t)  =  - k2 * A1(t)  .
      //      t
      //
      setCompAmount_t( 0, - k1 * compAmount1 );
      setCompAmount_t( 1, - k2 * compAmount2 );
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

      using namespace threecomp_onebolus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the intra-individual error.
      //--------------------------------------------------------

      // Get the current amount in compartment 1.
      Value compAmount1;
      getCompAmount( 0, compAmount1 );

      // Get the current amount in compartment 2.
      Value compAmount2;
      getCompAmount( 1, compAmount2 );

      Value y_i_j;

      // Set
      //
      //    y  =  [ A1(t)  +  A2(t) ] * exp[ eps(0) ]  .
      //
      y_i_j = ( compAmount1 + compAmount2 ) * CppAD::exp(indepVar[epsOffset + 0]);
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
      // For this test, there is one instantaneous bolus dose for
      // every compartment except the output compartment.
      return 2 +           // Instantaneous bolus dose records.
             nY_i;         // Observation records.
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
    ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OdePred(){}
    ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OdePred( const ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OdePred& ){}
    ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OdePred & operator=( const ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OdePred& ){}
  };


  //**********************************************************************
  //
  // Class:  ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_OdePred
  //
  //
  // This class provides an ODE-based version of the Pred block
  // expression evaluator.
  //
  // For this test, compartments 1 and 2 receive bolus doses at time
  // zero (t = 0), and the observed values come from the output
  // compartment (number 3).
  //
  // In particular, it evaluates PK, DES, and ERROR block expressions
  // that correspond to the following model for the mean of the
  // individuals' data,
  //
  //     f    (theta, eta)  =  A3(t)
  //      i(j)
  //                                           - k1 * t  
  //                        =  bolus1 * [ 1 - e         ]
  //
  //                                                - k2 * t
  //                             +  bolus2 * [ 1 - e         ] ,
  //
  // where
  //
  //     k1  =  theta1 + eta1 ,
  //
  //     k2  =  theta2 + eta2 ,
  //
  //     t  =  ( j + 1 ) * timeStep  ,
  //
  // and that correspond to model based weighting of the data using
  // an exponential parameterization,
  //
  //    y  =  f * exp[ eps(0) ]  
  //
  //       =  [ A1(t) + A2(t) ] * exp[ eps(0) ]  .
  //
  //**********************************************************************

  template<class Value>
  class ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_OdePred : public OdePredBase<Value>
  {
  public:

    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_OdePred(
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
    nComp              ( nCompIn )
    {}

    ~ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_OdePred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;
    const int nComp;

  public:
    Value k1;
    Value k2;


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

      using namespace threecomp_onebolus_odepredbasetest ;


      //--------------------------------------------------------
      // Set the current values for the predefined data items.
      //--------------------------------------------------------

      // Set an arbitrary value for the observed value.
      this->setDV( 123456789.0 );

      // Set the type of event.
      if ( j < 2 )
      {
        //------------------------------------------------------
        // Instantaneous bolus dose events.
        //------------------------------------------------------

        // If this is one of the first 2 data records, then set these
        // flags to indicate this is a dose event.
        this->setMDV ( 1 );
        this->setEVID( this->DOSE_EVENT );

        // Set the amount for this instantaneous bolus dose.
        if ( j == 0  )
        {
          this->setAMT( bolus1 );
          this->setCMT( 1 );
        }
        else
        {
          this->setAMT( bolus2 );
          this->setCMT( 2 );
        }

        // Set the time for all of the infusions to be time zero.
        this->setTIME( 0.0 );
      }
      else
      {
        //------------------------------------------------------
        // Observation events.
        //------------------------------------------------------

        // Set these flags to indicate this is an observation event.
        this->setMDV ( 0 );
        this->setEVID( this->OBSERV_EVENT );

        // Set the time to be greater than zero.
        this->setTIME( ( j - 1 ) * timeStep );
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

      using namespace threecomp_onebolus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the Pk block parameters.
      //--------------------------------------------------------

        k1 = indepVar[thetaOffset + 0] + indepVar[etaOffset + 0];
        k2 = indepVar[thetaOffset + 1] + indepVar[etaOffset + 1];
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

      using namespace threecomp_onebolus_odepredbasetest;


      //--------------------------------------------------------
      // Evaluate the differential equations.
      //--------------------------------------------------------

      // Get the current amount in compartment 1.
      Value compAmount1;
      getCompAmount( 0, compAmount1 );

      // Get the current amount in compartment 2.
      Value compAmount2;
      getCompAmount( 1, compAmount2 );

      // The derivatives with respect to T of each compartment are
      //
      //     d   A1(t)  =  - k1 * A1(t)  ,
      //      t
      //
      //     d   A2(t)  =  - k2 * A1(t)  .
      //      t
      //
      setCompAmount_t( 0, - k1 * compAmount1 );
      setCompAmount_t( 1, - k2 * compAmount2 );
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

      using namespace threecomp_onebolus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the intra-individual error.
      //--------------------------------------------------------

      // Get the current amount in compartment 3.
      Value compAmount3;
      getCompAmount( 2, compAmount3 );

      Value y_i_j;

      // Set
      //
      //    y  =  [ A3(t) ] * exp[ eps(0) ]  .
      //
      y_i_j = compAmount3 * CppAD::exp(indepVar[epsOffset + 0]);
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
      // For this test, there is one instantaneous bolus dose for
      // every compartment except the output compartment.
      return 2 +           // Instantaneous bolus dose records.
             nY_i;         // Observation records.
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
    ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_OdePred(){}
    ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_OdePred( const ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_OdePred& ){}
    ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_OdePred & operator=( const ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_OdePred& ){}
  };


  //**********************************************************************
  //
  // Class:  FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_OdePred
  //
  //
  // This class provides an ODE-based version of the Pred block
  // expression evaluator.
  //
  // For this test, compartments 1, 2, and 3 all receive infusion
  // doses that start at time zero (t = 0).
  //
  // In particular, it evaluates PK, DES, and ERROR block expressions
  // that correspond to the following model for the mean of the
  // individuals' data,
  //
  //                            A   (t)  *  bioavailability   (theta, eta)
  //                             (p)                       (p)
  //     f    (theta, eta)  =  --------------------------------------------  ,
  //      i(j)                         scaleFactor   (theta, eta)
  //                                              (p)
  //
  // where
  //                -
  //               |  1,   if 0 < t <= 1,
  //               |
  //     comp  =  <   2,   if 1 < t <= 2,
  //               |
  //               |  3,   if 2 < t <= 3,
  //                -
  //
  //     t  =  ( j + 1 ) * timeStep  ,
  //
  //     p  =  comp - 1 ,
  //
  // and that correspond to model based weighting of the data using
  // an exponential parameterization,
  //
  //    y  =  f * exp[ eps(0) ]  .
  //
  //**********************************************************************

  template<class Value>
  class FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_OdePred : public OdePredBase<Value>
  {

    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_OdePred(
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
    nComp              ( nCompIn )
    {}

    ~FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_OdePred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;
    const int nComp;

  public:
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

      using namespace fourcomp_multinfus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the predefined data items.
      //--------------------------------------------------------

      // Set an arbitrary value for the observed value.
      this->setDV( 123456789.0 );

      // Set the compartment number so that it rotates among the first
      // 3 compartments.
      int comp = j % 3 + 1;
      this->setCMT( comp );

      // For this test there are no events for the output compartment.
      assert( comp < nComp );

      // Set the type of event.
      if ( j < 3 && comp < 4  )
      {
        //------------------------------------------------------
        // Regular infusion dose events.
        //------------------------------------------------------

        // If this is one of the first 3 data records, and if this is
        // not the output compartment (number 4), which cannot receive
        // doses, then set these flags to indicate this is a dose
        // event.
        this->setMDV ( 1 );
        this->setEVID( this->DOSE_EVENT );

        // Set the rate for this regular infusion dose.
        this->setRATE( rate);

        // Set the amount for this regular infusion dose so that the
        // infusions turn off at different times which are out of
        // order and have to be sorted by OdePredBase::getExpDesign().
        if ( comp == 1  )
        {
          // Compartment 1 should turn off at T = 2.0.
          this->setAMT( 2.0 * rate );
        }
        else if ( comp == 2  )
        {
          // Compartment 2 should turn off at T = 3.0.
          this->setAMT( 3.0 * rate );
        }
        else
        {
          // Compartment 3 should turn off at T = 1.0.
          this->setAMT( 1.0 * rate );
        }

        // Set the time for all of the infusions to be time zero.
        this->setTIME( 0.0 );
      }
      else
      {
        //------------------------------------------------------
        // Observation events.
        //------------------------------------------------------

        // Set these flags to indicate this is an observation event.
        this->setMDV ( 0 );
        this->setEVID( this->OBSERV_EVENT );

        // Set the time to be greater than zero.
        this->setTIME( ( j - 2 ) * timeStep );
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

      using namespace fourcomp_multinfus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the Pk block parameters.
      //--------------------------------------------------------

      Value compScaleParam_p;
      Value compBioavailFrac_p;

      // Calculate the PK parameters,
      //
      //     scaleFactor  (theta, eta)  =  ( theta    + eta    ) ,
      //                (p)                       (0)      (0)
      //
      //     bioavailability   (theta, eta)  = (p + 1) * ( theta    + eta    ) .
      //                    (p)                                 (1)      (1)
      //
      int p;
      for ( p = 0; p < nComp;  p++ )
      {
        compScaleParam_p   = indepVar[thetaOffset + 0] + indepVar[etaOffset + 0];
        compBioavailFrac_p = ( p + 1 ) * ( indepVar[thetaOffset + 1] + indepVar[etaOffset + 1] );

        setCompScaleParam  ( p, compScaleParam_p );
        setCompBioavailFrac( p, compBioavailFrac_p );
      }
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

      using namespace fourcomp_multinfus_odepredbasetest;


      //--------------------------------------------------------
      // Evaluate the differential equations.
      //--------------------------------------------------------

      // Since f has no explicit time dependence,
      //
      //                            A   (t)  *  bioavailability   (theta, eta)
      //                             (p)                       (p)
      //     f    (theta, eta)  =  --------------------------------------------  ,
      //      i(j)                         scaleFactor   (theta, eta)
      //                                              (p)
      //
      // the derivative with respect to T of f is equal to
      //
      //     d
      //    ----- f(T)  =  0  .
      //     d T
      //
      int p;
      for ( p = 0; p < nComp; p++ )
      {
        this->setCompAmount_t( p, 0.0 );
      }
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

      using namespace fourcomp_multinfus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the intra-individual error.
      //--------------------------------------------------------

      Value f_i_j;
      Value y_i_j;

      getF( f_i_j );

      // Set
      //
      //    y  =  f * exp[ eps(0) ]  .
      //
      y_i_j = f_i_j * CppAD::exp(indepVar[epsOffset + 0]);
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
      // For this test there is one infusion for every compartment
      // except the output compartment.
      return nComp - 1 +   // Regular infusion dose records.
             nY_i;         // Observation records.
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
    FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_OdePred(){}
    FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_OdePred( const FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_OdePred& ){}
    FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_OdePred & operator=( const FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_OdePred& ){}
  };


  //**********************************************************************
  //
  // Class:  FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_OdePred
  //
  //
  // The evalDes function for this class has been modified so that the
  // amount in each compartment and its time derivative are either Not
  // a Number (NaN) or infinite.
  //
  // This class provides an ODE-based version of the Pred block
  // expression evaluator.
  //
  // For this test, compartments 1, 2, and 3 all receive infusion
  // doses that start at time zero (t = 0).
  //
  // In particular, it evaluates PK, DES, and ERROR block expressions
  // that correspond to the following model for the mean of the
  // individuals' data,
  //
  //                            A   (t)  *  bioavailability   (theta, eta)
  //                             (p)                       (p)
  //     f    (theta, eta)  =  --------------------------------------------  ,
  //      i(j)                         scaleFactor   (theta, eta)
  //                                              (p)
  //
  // where
  //                -
  //               |  1,   if 0 < t <= 1,
  //               |
  //     comp  =  <   2,   if 1 < t <= 2,
  //               |
  //               |  3,   if 2 < t <= 3,
  //                -
  //
  //     t  =  ( j + 1 ) * timeStep  ,
  //
  //     p  =  comp - 1 ,
  //
  // and that correspond to model based weighting of the data using
  // an exponential parameterization,
  //
  //    y  =  f * exp[ eps(0) ]  .
  //
  //**********************************************************************

  template<class Value>
  class FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_OdePred : public OdePredBase<Value>
  {

    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_OdePred(
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
    nComp              ( nCompIn )
    {}

    ~FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_OdePred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;
    const int nComp;

  public:
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

      using namespace fourcomp_multinfus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the predefined data items.
      //--------------------------------------------------------

      // Set an arbitrary value for the observed value.
      this->setDV( 123456789.0 );

      // Set the compartment number so that it rotates among the first
      // 3 compartments.
      int comp = j % 3 + 1;
      this->setCMT( comp );

      // For this test there are no events for the output compartment.
      assert( comp < nComp );

      // Set the type of event.
      if ( j < 3 && comp < 4  )
      {
        //------------------------------------------------------
        // Regular infusion dose events.
        //------------------------------------------------------

        // If this is one of the first 3 data records, and if this is
        // not the output compartment (number 4), which cannot receive
        // doses, then set these flags to indicate this is a dose
        // event.
        this->setMDV ( 1 );
        this->setEVID( this->DOSE_EVENT );

        // Set the rate for this regular infusion dose.
        this->setRATE( rate);

        // Set the amount for this regular infusion dose so that the
        // infusions turn off at different times which are out of
        // order and have to be sorted by OdePredBase::getExpDesign().
        if ( comp == 1  )
        {
          // Compartment 1 should turn off at T = 2.0.
          this->setAMT( 2.0 * rate );
        }
        else if ( comp == 2  )
        {
          // Compartment 2 should turn off at T = 3.0.
          this->setAMT( 3.0 * rate );
        }
        else
        {
          // Compartment 3 should turn off at T = 1.0.
          this->setAMT( 1.0 * rate );
        }

        // Set the time for all of the infusions to be time zero.
        this->setTIME( 0.0 );
      }
      else
      {
        //------------------------------------------------------
        // Observation events.
        //------------------------------------------------------

        // Set these flags to indicate this is an observation event.
        this->setMDV ( 0 );
        this->setEVID( this->OBSERV_EVENT );

        // Set the time to be greater than zero.
        this->setTIME( ( j - 2 ) * timeStep );
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

      using namespace fourcomp_multinfus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the Pk block parameters.
      //--------------------------------------------------------

      Value compScaleParam_p;
      Value compBioavailFrac_p;

      // Calculate the PK parameters,
      //
      //     scaleFactor  (theta, eta)  =  ( theta    + eta    ) ,
      //                (p)                       (0)      (0)
      //
      //     bioavailability   (theta, eta)  = (p + 1) * ( theta    + eta    ) .
      //                    (p)                                 (1)      (1)
      //
      int p;
      for ( p = 0; p < nComp;  p++ )
      {
        compScaleParam_p   = indepVar[thetaOffset + 0] + indepVar[etaOffset + 0];
        compBioavailFrac_p = ( p + 1 ) * ( indepVar[thetaOffset + 1] + indepVar[etaOffset + 1] );

        setCompScaleParam  ( p, compScaleParam_p );
        setCompBioavailFrac( p, compBioavailFrac_p );
      }
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

      using namespace fourcomp_multinfus_odepredbasetest;


      //--------------------------------------------------------
      // Evaluate the differential equations.
      //--------------------------------------------------------

      Value zero = Value( 0 );
      Value one  = Value( 1 );

      // Set these compartments' time derivative to be Not a
      // Number (NaN).
      this->setCompAmount_t( 0, zero / zero );
      this->setCompAmount_t( 1, zero / zero );

      // Set these compartments' time derivative to be infinite.
      this->setCompAmount_t( 2, one / zero );
      this->setCompAmount_t( 3, one / zero );
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

      using namespace fourcomp_multinfus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the intra-individual error.
      //--------------------------------------------------------

      Value f_i_j;
      Value y_i_j;

      getF( f_i_j );

      // Set
      //
      //    y  =  f * exp[ eps(0) ]  .
      //
      y_i_j = f_i_j * CppAD::exp(indepVar[epsOffset + 0]);
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
      // For this test there is one infusion for every compartment
      // except the output compartment.
      return nComp - 1 +   // Regular infusion dose records.
             nY_i;         // Observation records.
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
    FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_OdePred(){}
    FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_OdePred( const FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_OdePred& ){}
    FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_OdePred & operator=( const FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_OdePred& ){}
  };


  //**********************************************************************
  //
  // Class:  FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_OdePred
  //
  //
  // This class provides an ODE-based version of the Pred block
  // expression evaluator.
  //
  // For this test, compartment 1 receives an infusion dose that
  // starts at time zero (t = 0), compartment 2 receives an infusion
  // dose that starts at time one (t = 1), and compartment 3 receives
  // an infusion dose that starts at time two (t = 2).
  //
  // In particular, it evaluates PK, DES, and ERROR block expressions
  // that correspond to the following model for the mean of the
  // individuals' data,
  //
  //                            A   (t)  *  bioavailability   (theta, eta)
  //                             (p)                       (p)
  //     f    (theta, eta)  =  --------------------------------------------  ,
  //      i(j)                         scaleFactor   (theta, eta)
  //                                              (p)
  //
  // where
  //                -
  //               |  1,   if 0 < t <= 1,
  //               |
  //     comp  =  <   2,   if 1 < t <= 2,
  //               |
  //               |  3,   if 2 < t <= 3,
  //                -
  //
  //     t  =  ( j + 1 + j / 3 ) * timeStep )  ,
  //
  //     p  =  comp - 1 ,
  //
  // and that correspond to model based weighting of the data using
  // an exponential parameterization,
  //
  //    y  =  f * exp[ eps(0) ]  .
  //
  //**********************************************************************

  template<class Value>
  class FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_OdePred : public OdePredBase<Value>
  {
    //------------------------------------------------------------
    // Constructor.
    //------------------------------------------------------------

  public:
    FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_OdePred(
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
    nComp              ( nCompIn )
    {}

    ~FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_OdePred(){}


    //------------------------------------------------------------
    // Model related quantities.
    //------------------------------------------------------------

  private:
    const int nY_i;
    const int nComp;

  public:
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

      using namespace fourcomp_multinfus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the predefined data items.
      //--------------------------------------------------------

      // Set an arbitrary value for the observed value.
      this->setDV( 123456789.0 );

      // Set the compartment number so that it rotates among all the 4
      // compartments.
      int comp = ( j + 4 ) / 4;
      this->setCMT( comp );

      // For this test there are no events for the output compartment.
      assert( comp < nComp );

      // Set the type of event.
      if ( j % 4 == 0 && comp < 4  )
      {
        //------------------------------------------------------
        // Regular infusion dose events.
        //------------------------------------------------------

        // If this is the first data record for this group of 4 data
        // records, and if this is not the output compartment (number
        // 4), which cannot receive doses, then set these flags to
        // indicate this is a dose event.
        this->setMDV ( 1 );
        this->setEVID( this->DOSE_EVENT );

        // Set the amount for this regular infusion dose.
        this->setAMT ( amount );
        this->setRATE( rate );

        // Set the time.
        this->setTIME( 4 * ( comp - 1 ) * timeStep );
      }
      else
      {
        //------------------------------------------------------
        // Observation events.
        //------------------------------------------------------

        // Set these flags to indicate this is an observation event.
        this->setMDV ( 0 );
        this->setEVID( this->OBSERV_EVENT );

        // Set the time.
        this->setTIME( ( 4 * ( comp - 1 ) + j % 4) * timeStep );
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

      using namespace fourcomp_multinfus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the Pk block parameters.
      //--------------------------------------------------------

      Value compScaleParam_p;
      Value compBioavailFrac_p;

      // Calculate the PK parameters,
      //
      //     scaleFactor  (theta, eta)  =  ( theta    + eta    ) ,
      //                (p)                       (0)      (0)
      //
      //     bioavailability   (theta, eta)  = (p + 1) * ( theta    + eta    ) .
      //                    (p)                                 (1)      (1)
      //
      int p;
      for ( p = 0; p < nComp;  p++ )
      {
        compScaleParam_p   = indepVar[thetaOffset + 0] + indepVar[etaOffset + 0];
        compBioavailFrac_p = ( p + 1 ) * ( indepVar[thetaOffset + 1] + indepVar[etaOffset + 1] );

        setCompScaleParam  ( p, compScaleParam_p );
        setCompBioavailFrac( p, compBioavailFrac_p );
      }
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

      using namespace fourcomp_multinfus_odepredbasetest;


      //--------------------------------------------------------
      // Evaluate the differential equations.
      //--------------------------------------------------------

      // Since f has no explicit time dependence,
      //
      //                            A   (t)  *  bioavailability   (theta, eta)
      //                             (p)                       (p)
      //     f    (theta, eta)  =  --------------------------------------------  ,
      //      i(j)                         scaleFactor   (theta, eta)
      //                                              (p)
      //
      // the derivative with respect to T of f is equal to
      //
      //     d
      //    ----- f(T)  =  0  .
      //     d T
      //
      int p;
      for ( p = 0; p < nComp; p++ )
      {
        this->setCompAmount_t( p, 0.0 );
      }
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

      using namespace fourcomp_multinfus_odepredbasetest;


      //--------------------------------------------------------
      // Set the current values for the intra-individual error.
      //--------------------------------------------------------

      Value f_i_j;
      Value y_i_j;

      getF( f_i_j );

      // Set
      //
      //    y  =  f * exp[ eps(0) ]  .
      //
      y_i_j = f_i_j * CppAD::exp(indepVar[epsOffset + 0]);
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
      // For this test there is one infusion for every compartment
      // except the output compartment.
      return nComp - 1 +   // Regular infusion dose records.
             nY_i;         // Observation records.
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
    FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_OdePred(){}
    FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_OdePred( const FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_OdePred& ){}
    FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_OdePred & operator=( const FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_OdePred& ){}
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

void OdePredBaseTest::setUp()
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

void OdePredBaseTest::tearDown()
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

Test* OdePredBaseTest::suite()
{
  TestSuite *suiteOfTests = new TestSuite( "OdePredBaseTest" );

  suiteOfTests->addTest(new TestCaller<OdePredBaseTest>(
    "NoEta_OneExpF_OneBolus_ModelBasedExpY_Test", 
    &OdePredBaseTest::NoEta_OneExpF_OneBolus_ModelBasedExpY_Test ));

  suiteOfTests->addTest(new TestCaller<OdePredBaseTest>(
    "NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_Test", 
    &OdePredBaseTest::NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_Test ));

  suiteOfTests->addTest(new TestCaller<OdePredBaseTest>(
    "OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_Test", 
    &OdePredBaseTest::OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_Test ));

  suiteOfTests->addTest(new TestCaller<OdePredBaseTest>(
    "OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_Test", 
    &OdePredBaseTest::OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_Test ));

  suiteOfTests->addTest(new TestCaller<OdePredBaseTest>(
    "ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_Test", 
    &OdePredBaseTest::ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_Test ));

  suiteOfTests->addTest(new TestCaller<OdePredBaseTest>(
    "ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_Test", 
    &OdePredBaseTest::ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_Test ));

  suiteOfTests->addTest(new TestCaller<OdePredBaseTest>(
    "FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_Test", 
    &OdePredBaseTest::FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_Test ));

  suiteOfTests->addTest(new TestCaller<OdePredBaseTest>(
    "FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_Test", 
    &OdePredBaseTest::FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_Test ));

  suiteOfTests->addTest(new TestCaller<OdePredBaseTest>(
    "FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_Test", 
    &OdePredBaseTest::FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_Test ));

  suiteOfTests->addTest(new TestCaller<OdePredBaseTest>(
    "ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_Test", 
    &OdePredBaseTest::ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_Test ));

  return suiteOfTests;
}


/*************************************************************************
 *
 * Function: NoEta_OneExpF_OneBolus_ModelBasedExpY_Test
 *
 *
 * The goal of this test is to check that the ODE-based version of the
 * Pred block expression evaluator works for the case of
 *
 *     NoEta_OneExpF_OneBolus_ModelBasedExpY_OdePred.
 *
 *************************************************************************/

void OdePredBaseTest::NoEta_OneExpF_OneBolus_ModelBasedExpY_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpf_onebolus_odepredbasetest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of data values for this individual.
  int nY_iKnown = 3;

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
  NoEta_OneExpF_OneBolus_ModelBasedExpY_OdePred< double > predEvaluator( 
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  NoEta_OneExpF_OneBolus_ModelBasedExpY_OdePred< AD<double> > predEvaluatorAD( 
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  NoEta_OneExpF_OneBolus_ModelBasedExpY_OdePred< AD< AD<double> > > predEvaluatorADAD( 
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
  // Prepare known values related to the model functions.
  //------------------------------------------------------------

  valarray<double> dataMeanKnown              ( nY_i );
  valarray<double> dataMean_popParKnown       ( nY_i * nPopPar );
  valarray<double> dataVarianceKnown          ( nY_i * nY_i );
  valarray<double> dataVariance_popParKnown   ( nY_i * nY_i * nPopPar );
  valarray<double> dataVarianceInvKnown       ( nY_i * nY_i );
  valarray<double> dataVarianceInv_popParKnown( nY_i * nY_i * nPopPar );

  double T;
  double ds;
  double w;
  double ka;
  double ke;
  double cl;
  double d;
  double e;

  // For this test there is a single bolus at time zero.
  //
  // Calculate the known value for the mean of the data:
  //
  //                            ds * exp[ - theta    * T    ]
  //                                             (0)    (j)
  //    f    ( alpha, b  )  =  ----------------------------------  .
  //     i(j)          i               [ theta    * w ]
  //                                          (1)
  //
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time taking into account the fact that the first
    // observation occurs at the (j + 1)-th time step.
    T = timeStep * (j + 1);

    ds = dose * wt;
    w = wt;

    ke = thetaCurr[0];
    cl = thetaCurr[1] * w;
    d = std::exp(-ke * T);
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

    dataMean_popParKnown[j + col * nY_i] = - timeStep * (j + 1) * dataMeanKnown[j];
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

    dataMean_popParKnown[j + col * nY_i] = - wt * dataMeanKnown[j] /
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
      dataMeanKnown[j] * dataMeanKnown[j];
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
        dataMean_popParKnown[j + k * nY_i] * dataMeanKnown[j];
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
      dataMeanKnown[j] * dataMeanKnown[j] *
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
  
  // Since the step sizes may change as OdePredBase evolves, only 
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

  // Set the tolerance for the model function comparisions to be 100
  // times less than the tolerance for the integration of the ODE's.
  tol = tolRel / 10.0;

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

  tol = 1.0e-14;

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
 * Function: NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_Test
 *
 *
 * The goal of this test is to check that the ODE-based version of the
 * Pred block expression evaluator works for the case of
 *
 *     NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_OdePred.
 *
 *************************************************************************/

void OdePredBaseTest::NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpf_onebolus_odepredbasetest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of data values for this individual.
  int nY_iKnown = 3;

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
  NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_OdePred< double > predEvaluator( 
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_OdePred< AD<double> > predEvaluatorAD( 
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  NoEta_OneExpF_OneBolus_OutOfOrderTimeRecord_ModelBasedExpY_OdePred< AD< AD<double> > > predEvaluatorADAD( 
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

  valarray<double> dataMean( nY_i );

  // Evaluate the data mean, which will cause the ODE's to be solved
  // and an exception to occur due to the time values being out of
  // order for one of the records.
  try
  {
    model.dataMean( dataMean );
  }
  catch( SpkException& e )
  {
    // Uncomment this line to see the list of exceptions.
    //cout << "e = " << e << endl;

    // See if there was an error during the solution of the ODE's.
    if ( e.find( SpkError::SPK_USER_INPUT_ERR ) < 0 )
    {
      CPPUNIT_ASSERT_MESSAGE( 
        "An expected SPK exception(SPK_ODE_SOLN_ERR) did not occur during the evaluation of the data mean.",
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


/*************************************************************************
 *
 * Function: OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_Test
 *
 *
 * The goal of this test is to check that the ODE-based version of the
 * Pred block expression evaluator works for the case of
 *
 *     OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred.
 *
 *************************************************************************/

void OdePredBaseTest::OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpf_onebolus_odepredbasetest;

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
  OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred< double > predEvaluator(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred< AD<double> > predEvaluatorAD(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred< AD< AD<double> > > predEvaluatorADAD(
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

  // For this test there is a single bolus at time zero and there are
  // non observation predication evaluation events.
  //
  // Calculate the known value for the mean of the data:
  //
  //                                      -                                  -   
  //                                     |                                    |
  //                            ds * exp |  - [ theta    + eta    ]  *  T     |
  //                                     |           (0)      (0)        (j)  |
  //                                      -                                  -   
  //    f    ( alpha, b  )  =  -------------------------------------------------  .
  //     i(j)          i
  //                                      [ theta    * w   +  eta    ]
  //                                             (1)             (1)
  //
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time taking into account the fact that the observations
    // occur at the (2j + 1)-th time step.
    T = timeStep * (2 * j + 1);

    ds = dose * wt;
    w = wt;

    ke = thetaCurr[0] + etaCurr[0];
    cl = thetaCurr[1] * w + etaCurr[1];
    d = std::exp(-ke * T);
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

    dataMean_popParKnown[j + col * nY_i] = - timeStep * (2 * j + 1) * dataMeanKnown[j];
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
    dataMean_indParKnown[j + k * nY_i] = - timeStep * (2 * j + 1) * dataMeanKnown[j];
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
  
  // Since the step sizes may change as OdePredBase evolves, only 
  // check that they are positive.
  for ( k = 0; k < nPopPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The step size for a population parameter element is invalid.",
      popParStep[k] != 0.0 );
  }

  // Since the limits may change as OdePredBase evolves, only check
  // that the lower limits are less than or equal to the upper.
  for ( k = 0; k < nIndPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The limits for an individual parameter element are invalid.",
      indParLow[k] <= indParUp [k] );
  }
  
  // Since the step sizes may change as OdePredBase evolves, only 
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
 * Function: OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_Test
 *
 *
 * The goal of this test is to check that the ODE-based version of the
 * Pred block expression evaluator works for the case of
 *
 *     OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_OdePred.
 *
 *************************************************************************/

void OdePredBaseTest::OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpf_onebolus_odepredbasetest;

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
  OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_OdePred< double > predEvaluator(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_OdePred< AD<double> > predEvaluatorAD(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  OneExpF_OneBolus_ObservAtBolusTime_AdditivePlusThetaDepY_OdePred< AD< AD<double> > > predEvaluatorADAD(
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
  
  // Since the step sizes may change as OdePredBase evolves, only 
  // check that they are positive.
  for ( k = 0; k < nPopPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The step size for a population parameter element is invalid.",
      popParStep[k] != 0.0 );
  }

  // Since the limits may change as OdePredBase evolves, only check
  // that the lower limits are less than or equal to the upper.
  for ( k = 0; k < nIndPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The limits for an individual parameter element are invalid.",
      indParLow[k] <= indParUp [k] );
  }
  
  // Since the step sizes may change as OdePredBase evolves, only 
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
 * Function: ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_Test
 *
 *
 * The goal of this test is to check that the ODE-based version of the
 * Pred block expression evaluator works for the case of
 *
 *     ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OdePred. 
 *
 *************************************************************************/

void OdePredBaseTest::ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace threecomp_onebolus_odepredbasetest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of compartments, including the output compartment.
  int nComp = 3;

  // Set the number of data values for this individual.
  int nY_iKnown = 5;

  // Set this equal to false since the PK block expressions are not
  // functions of T.
  bool isPkBlockAFuncOfTime = false;

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

  // Set the flags for compartment 2.
  compInitialOff[2 - 1] = false;
  compNoOff     [2 - 1] = false;
  compNoDose    [2 - 1] = false;

  // Set the flags for compartment 3 (the output compartment).
  compInitialOff[3 - 1] = true;
  compNoOff     [3 - 1] = false;
  compNoDose    [3 - 1] = true;

  // Set the relative tolerance for the ODE integration.
  double tolRel = 1.0e-6;

  // Construct the ODE-based versions of the Pred block evaluator.
  ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OdePred< double > predEvaluator(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OdePred< AD<double> > predEvaluatorAD(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OdePred< AD< AD<double> > > predEvaluatorADAD(
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
  const int nTheta = 2;
  const int nEta   = 2;
  const int nEps   = 1;

  // Set the current value for theta.
  valarray<double> thetaCurr( nTheta );
  thetaCurr[0] = 10.0;
  thetaCurr[1] =  5.0;

  // Set the limits for theta.
  valarray<double> thetaLow( nTheta );
  valarray<double> thetaUp ( nTheta );
  thetaLow[0] = -100.0;
  thetaUp [0] = +100.0;
  thetaLow[1] = -100.0;
  thetaUp [1] = +100.0;

  // Set the current value for eta.
  valarray<double> etaCurr( nEta );
  etaCurr[0] = 0.0;
  etaCurr[1] = 0.0;


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

  bool ok;

  // Evaluate these quantities at the current population and 
  // individual parameter values, which are set when the model
  // is constructed.
  model.dataMean         ( dataMean );
  model.dataVariance     ( dataVariance );
  model.dataVarianceInv  ( dataVarianceInv );

  ok = model.dataMean_popPar( dataMean_popPar );
  ok = model.dataMean_indPar( dataMean_indPar );

  ok = model.dataVariance_popPar   ( dataVariance_popPar );
  ok = model.dataVariance_indPar   ( dataVariance_indPar );
  ok = model.dataVarianceInv_popPar( dataVarianceInv_popPar );
  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );

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

  double T;
  double ds;
  double w;
  double ka;
  double ke;
  double cl;
  double d;
  double e;

  // For this test, both compartments receive bolus doses at time zero
  // (t = 0).
  //
  // Calculate the known value for the mean of the data:
  //
  //     f    (theta, eta)  =  A1(t)  +  A2(t)
  //      i(j)
  //
  //                                    - k1 * t             - k2 * t
  //                        =  bolus1 e           +  bolus2 e          ,
  //
  // where
  //
  //     k1  =  theta1 + eta1 ,
  //
  //     k2  =  theta2 + eta2 ,
  //
  // and
  //
  //     t  =  ( j + 1 ) * timeStep  .
  //
  int comp;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time.
    T = ( j + 1 ) * timeStep; 

    // Set the known value.
    dataMeanKnown[j] = bolus1 * std::exp( - ( thetaCurr[0] + etaCurr[0] ) * T ) +
                       bolus2 * std::exp( - ( thetaCurr[1] + etaCurr[1] ) * T );
  }

  int col;

  // Calculate the known value for the derivative of the data mean
  // with respect to the population parameter.
  dataMean_popParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time.
    T = ( j + 1 ) * timeStep;

    // Set the known values for this column,
    //
    //      (0)                                              - k1 * t
    //     d       f    ( alpha, b  )  =  - t  *  bolus1 *  e         . 
    //      theta   i(j)          i               
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - T * bolus1 *
      std::exp( - ( thetaCurr[0] + etaCurr[0] ) * T );
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time.
    T = ( j + 1 ) * timeStep;

    // Set the known values for this column,
    //
    //      (1)                                              - k2 * t
    //     d       f    ( alpha, b  )  =  - t  *  bolus2 *  e         . 
    //      theta   i(j)          i               
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - T * bolus2 * 
      std::exp( - ( thetaCurr[1] + etaCurr[1] ) * T );
  }

  // Calculate the known value for the derivative of the data mean
  // with respect to the individual parameter.
  dataMean_indParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time.
    T = ( j + 1 ) * timeStep;

    // Set the known values for this column,
    //
    //      (0)                                            - k1 * t
    //     d     f    ( alpha, b  )  =  - t  *  bolus1 *  e         . 
    //      eta   i(j)          i               
    //
    col = k;

    dataMean_indParKnown[j + col * nY_i] = - T * bolus1 *
      std::exp( - ( thetaCurr[0] + etaCurr[0] ) * T );
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time.
    T = ( j + 1 ) * timeStep;

    // Set the known values for this column,
    //
    //      (1)                                            - k2 * t
    //     d     f    ( alpha, b  )  =  - t  *  bolus2 *  e         . 
    //      eta   i(j)          i               
    //
    col = k;

    dataMean_indParKnown[j + col * nY_i] = - T * bolus2 *
      std::exp( - ( thetaCurr[1] + etaCurr[1] ) * T );
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
      dataMeanKnown[j] * dataMeanKnown[j];
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
        dataMean_popParKnown[j + k * nY_i] * dataMeanKnown[j];
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
      dataMeanKnown[j] * dataMeanKnown[j] *
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

  // Set the tolerance for the model function comparisions to be equal
  // to the tolerance for the integration of the ODE's.
  tol = tolRel;

  compareToKnown( 
    dataMean,
    dataMeanKnown,
    "dataMean",
    tol );

  tol = 10.0 * tolRel;

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

}


/*************************************************************************
 *
 * Function: ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_Test
 *
 *
 * The goal of this test is to check that the ODE-based version of the
 * Pred block expression evaluator works for the case of
 *
 *     ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_OdePred. 
 *
 *************************************************************************/

void OdePredBaseTest::ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace threecomp_onebolus_odepredbasetest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of compartments, including the output compartment.
  int nComp = 3;

  // Set the number of data values for this individual.
  int nY_iKnown = 5;

  // Set this equal to false since the PK block expressions are not
  // functions of T.
  bool isPkBlockAFuncOfTime = false;

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

  // Set the flags for compartment 2.
  compInitialOff[2 - 1] = false;
  compNoOff     [2 - 1] = false;
  compNoDose    [2 - 1] = false;

  // Set the flags for compartment 3 (the output compartment).
  compInitialOff[3 - 1] = false;
  compNoOff     [3 - 1] = false;
  compNoDose    [3 - 1] = true;

  // Set the relative tolerance for the ODE integration.
  double tolRel = 1.0e-6;

  // Construct the ODE-based versions of the Pred block evaluator.
  ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_OdePred< double > predEvaluator(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_OdePred< AD<double> > predEvaluatorAD(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  ThreeComp_OneBolus_ErrorBlockHasSumOfComps_AdditiveY_OutputCompUsed_OdePred< AD< AD<double> > > predEvaluatorADAD(
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
  const int nTheta = 2;
  const int nEta   = 2;
  const int nEps   = 1;

  // Set the current value for theta.
  valarray<double> thetaCurr( nTheta );
  thetaCurr[0] = 10.0;
  thetaCurr[1] =  5.0;

  // Set the limits for theta.
  valarray<double> thetaLow( nTheta );
  valarray<double> thetaUp ( nTheta );
  thetaLow[0] = -100.0;
  thetaUp [0] = +100.0;
  thetaLow[1] = -100.0;
  thetaUp [1] = +100.0;

  // Set the current value for eta.
  valarray<double> etaCurr( nEta );
  etaCurr[0] = 0.0;
  etaCurr[1] = 0.0;


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

  bool ok;

  // Evaluate these quantities at the current population and 
  // individual parameter values, which are set when the model
  // is constructed.
  model.dataMean         ( dataMean );
  model.dataVariance     ( dataVariance );
  model.dataVarianceInv  ( dataVarianceInv );

  ok = model.dataMean_popPar( dataMean_popPar );
  ok = model.dataMean_indPar( dataMean_indPar );

  ok = model.dataVariance_popPar   ( dataVariance_popPar );
  ok = model.dataVariance_indPar   ( dataVariance_indPar );
  ok = model.dataVarianceInv_popPar( dataVarianceInv_popPar );
  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );

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

  double T;
  double ds;
  double w;
  double ka;
  double ke;
  double cl;
  double d;
  double e;

  // For this test, both compartments receive bolus doses at time zero
  // (t = 0).
  //
  // Calculate the known value for the mean of the data:
  //
  //     f    (theta, eta)  =  A3(t)
  //      i(j)
  //                                           - k1 * t  
  //                        =  bolus1 * [ 1 - e         ]
  //
  //                                                - k2 * t
  //                             +  bolus2 * [ 1 - e         ] ,
  //
  // where
  //
  //     k1  =  theta1 + eta1 ,
  //
  //     k2  =  theta2 + eta2 ,
  //
  // and
  //
  //     t  =  ( j + 1 ) * timeStep  .
  //
  int comp;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time.
    T = ( j + 1 ) * timeStep; 

    // Set the known value.
    dataMeanKnown[j] = bolus1 * ( 1.0 - std::exp( - ( thetaCurr[0] + etaCurr[0] ) * T ) ) +
                       bolus2 * ( 1.0 - std::exp( - ( thetaCurr[1] + etaCurr[1] ) * T ) );
  }

  int col;

  // Calculate the known value for the derivative of the data mean
  // with respect to the population parameter.
  dataMean_popParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time.
    T = ( j + 1 ) * timeStep;

    // Set the known values for this column,
    //
    //      (0)                                            - k1 * t
    //     d       f    ( alpha, b  )  =  t  *  bolus1 *  e         . 
    //      theta   i(j)          i               
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = T * bolus1 *
      std::exp( - ( thetaCurr[0] + etaCurr[0] ) * T );
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time.
    T = ( j + 1 ) * timeStep;

    // Set the known values for this column,
    //
    //      (1)                                            - k2 * t
    //     d       f    ( alpha, b  )  =  t  *  bolus2 *  e         . 
    //      theta   i(j)          i               
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = T * bolus2 * 
      std::exp( - ( thetaCurr[1] + etaCurr[1] ) * T );
  }

  // Calculate the known value for the derivative of the data mean
  // with respect to the individual parameter.
  dataMean_indParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time.
    T = ( j + 1 ) * timeStep;

    // Set the known values for this column,
    //
    //      (0)                                          - k1 * t
    //     d     f    ( alpha, b  )  =  t  *  bolus1 *  e         . 
    //      eta   i(j)          i               
    //
    col = k;

    dataMean_indParKnown[j + col * nY_i] = T * bolus1 *
      std::exp( - ( thetaCurr[0] + etaCurr[0] ) * T );
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time.
    T = ( j + 1 ) * timeStep;

    // Set the known values for this column,
    //
    //      (1)                                          - k2 * t
    //     d     f    ( alpha, b  )  =  t  *  bolus2 *  e         . 
    //      eta   i(j)          i               
    //
    col = k;

    dataMean_indParKnown[j + col * nY_i] = T * bolus2 *
      std::exp( - ( thetaCurr[1] + etaCurr[1] ) * T );
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
      dataMeanKnown[j] * dataMeanKnown[j];
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
        dataMean_popParKnown[j + k * nY_i] * dataMeanKnown[j];
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
      dataMeanKnown[j] * dataMeanKnown[j] *
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

  // Set the tolerance for the model function comparisions to be equal
  // to the tolerance for the integration of the ODE's.
  tol = tolRel;

  compareToKnown( 
    dataMean,
    dataMeanKnown,
    "dataMean",
    tol );

  tol = 10.0 * tolRel;

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
}


/*************************************************************************
 *
 * Function: FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_Test
 *
 *
 * The goal of this test is to check that the ODE-based version of the
 * Pred block expression evaluator works for the case of
 *
 *     FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_OdePred. 
 *
 *************************************************************************/

void OdePredBaseTest::FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace fourcomp_multinfus_odepredbasetest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of compartments, including the output compartment.
  int nComp = 4;

  // Set the number of data values for this individual.  For this test
  // there are nComp - 1 intervals each with nComp - 1 observations.
  int nY_iKnown = ( nComp - 1 ) * ( nComp - 1 );

  // Set this equal to false since the PK block expressions are not
  // functions of T.
  bool isPkBlockAFuncOfTime = false;

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

  // Set the flags for compartment 2.
  compInitialOff[2 - 1] = false;
  compNoOff     [2 - 1] = false;
  compNoDose    [2 - 1] = false;

  // Set the flags for compartment 3.
  compInitialOff[3 - 1] = false;
  compNoOff     [3 - 1] = false;
  compNoDose    [3 - 1] = false;

  // Set the flags for compartment 4 (the output compartment).
  compInitialOff[4 - 1] = true;
  compNoOff     [4 - 1] = false;
  compNoDose    [4 - 1] = true;

  // Set the relative tolerance for the ODE integration.
  double tolRel = 1.0e-6;

  // Construct the ODE-based versions of the Pred block evaluator.
  FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_OdePred< double > predEvaluator(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_OdePred< AD<double> > predEvaluatorAD(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AdditiveY_OdePred< AD< AD<double> > > predEvaluatorADAD(
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
  const int nTheta = 2;
  const int nEta   = 2;
  const int nEps   = 1;

  // Set the current value for theta.
  valarray<double> thetaCurr( nTheta );
  thetaCurr[0] = 10.1;
  thetaCurr[1] = 1.2;

  // Set the limits for theta.
  valarray<double> thetaLow( nTheta );
  valarray<double> thetaUp ( nTheta );
  thetaLow[0] = -100.0;
  thetaUp [0] = +100.0;
  thetaLow[1] = -100.0;
  thetaUp [1] = +100.0;

  // Set the current value for eta.
  valarray<double> etaCurr( nEta );
  etaCurr[0] = 10.0 - thetaCurr[0];
  etaCurr[1] = 1.0  - thetaCurr[1];


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

  bool ok;

  // Evaluate these quantities at the current population and 
  // individual parameter values, which are set when the model
  // is constructed.
  model.dataMean         ( dataMean );
  model.dataVariance     ( dataVariance );
  model.dataVarianceInv  ( dataVarianceInv );

  ok = model.dataMean_popPar( dataMean_popPar );
  ok = model.dataMean_indPar( dataMean_indPar );

  ok = model.dataVariance_popPar   ( dataVariance_popPar );
  ok = model.dataVariance_indPar   ( dataVariance_indPar );
  ok = model.dataVarianceInv_popPar( dataVarianceInv_popPar );
  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );

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

  double T;
  double ds;
  double w;
  double ka;
  double ke;
  double cl;
  double d;
  double e;

  // For this test, all three compartments receive infusion doses that
  // start at time zero (t = 0).
  //
  // Calculate the known value for the mean of the data:
  //
  //                            A   (t)  *  bioavailability   (theta, eta)
  //                             (p)                       (p)
  //     f    (theta, eta)  =  --------------------------------------------  ,
  //      i(j)                         scaleFactor   (theta, eta)
  //                                              (p)
  //
  // where
  //                -
  //               |  1,   if 0 < t <= 1,
  //               |
  //     comp  =  <   2,   if 1 < t <= 2,
  //               |
  //               |  3,   if 2 < t <= 3,
  //                -
  //
  //     t  =  ( j + 1 ) * timeStep  ,
  //
  // and
  //
  //     p  =  comp - 1 .
  //
  int comp;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time.
    T = ( j + 1 ) * timeStep; 

    // Set the compartment.
    comp = j % 3 + 1;

    // Set the known value.
    if ( ( comp == 1 && T < 2.0 ) ||    // Compartment 1 turns off at T = 2.0.
         ( comp == 2 && T < 3.0 ) ||    // Compartment 2 turns off at T = 3.0.
         ( comp == 3 && T < 1.0 ) )     // Compartment 3 turns off at T = 1.0.
    {
      dataMeanKnown[j] = T * comp *
        ( thetaCurr[1] + etaCurr[1] ) / ( thetaCurr[0] + etaCurr[0] );
    }
    else
    {
      dataMeanKnown[j] = comp *
        ( thetaCurr[1] + etaCurr[1] ) / ( thetaCurr[0] + etaCurr[0] );
    }
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
    //                                       -  f    ( alpha, b  )
    //      (0)                                  i(j)          i
    //     d       f    ( alpha, b  )  =  -------------------------------  .
    //      theta   i(j)          i  
    //                                     [ theta      +  eta    ]
    //                                            (0)         (0)
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - dataMeanKnown[j] /
      ( thetaCurr[0] + etaCurr[0] );
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                          f    ( alpha, b  )
    //      (1)                                  i(j)          i
    //     d       f    ( alpha, b  )  =  -------------------------------  .
    //      theta   i(j)          i  
    //                                     [ theta      +  eta    ]
    //                                            (1)         (1)
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = dataMeanKnown[j] /
      ( thetaCurr[1] + etaCurr[1] );
  }

  // Calculate the known value for the derivative of the data mean
  // with respect to the individual parameter.
  dataMean_indParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                     -  f    ( alpha, b  )
    //      (0)                                i(j)          i
    //     d     f    ( alpha, b  )  =  -------------------------------  .
    //      eta   i(j)          i  
    //                                   [ theta      +  eta    ]
    //                                          (0)         (0)
    //
    dataMean_indParKnown[j + k * nY_i] = - dataMeanKnown[j] /
      ( thetaCurr[0] + etaCurr[0] );
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                        f    ( alpha, b  )
    //      (1)                                i(j)          i
    //     d     f    ( alpha, b  )  =  -------------------------------  .
    //      eta   i(j)          i  
    //                                   [ theta      +  eta    ]
    //                                          (1)         (1)
    //
    dataMean_indParKnown[j + k * nY_i] = dataMeanKnown[j] /
      ( thetaCurr[1] + etaCurr[1] );
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
      dataMeanKnown[j] * dataMeanKnown[j];
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
        dataMean_popParKnown[j + k * nY_i] * dataMeanKnown[j];
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
      dataMeanKnown[j] * dataMeanKnown[j] *
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

  // Set the tolerance for the model function comparisions to be 100
  // times less than the tolerance for the integration of the ODE's.
  tol = tolRel / 100.0;

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
}


/*************************************************************************
 *
 * Function: FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_Test
 *
 *
 * The evalDes function for this tests OdePredBase subclass has been
 * modified so that the amount in each compartment and its time
 * derivative are either Not a Number (NaN) or infinite.
 *
 * The goal of this test is to check that the ODE-based version of the
 * Pred block expression evaluator works for the case of
 *
 *     FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_OdePred. 
 *
 *************************************************************************/

void OdePredBaseTest::FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace fourcomp_multinfus_odepredbasetest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of compartments, including the output compartment.
  int nComp = 4;

  // Set the number of data values for this individual.  For this test
  // there are nComp - 1 intervals each with nComp - 1 observations.
  int nY_iKnown = ( nComp - 1 ) * ( nComp - 1 );

  // Set this equal to false since the PK block expressions are not
  // functions of T.
  bool isPkBlockAFuncOfTime = false;

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

  // Set the flags for compartment 2.
  compInitialOff[2 - 1] = false;
  compNoOff     [2 - 1] = false;
  compNoDose    [2 - 1] = false;

  // Set the flags for compartment 3.
  compInitialOff[3 - 1] = false;
  compNoOff     [3 - 1] = false;
  compNoDose    [3 - 1] = false;

  // Set the flags for compartment 4 (the output compartment).
  compInitialOff[4 - 1] = true;
  compNoOff     [4 - 1] = false;
  compNoDose    [4 - 1] = true;

  // Set the relative tolerance for the ODE integration.
  double tolRel = 1.0e-6;

  // Construct the ODE-based versions of the Pred block evaluator.
  FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_OdePred< double > predEvaluator(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_OdePred< AD<double> > predEvaluatorAD(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  FourComp_MultInfus_NoCompWithZeroMassAtFirstObserv_AmountAndDerivNanAndInf_AdditiveY_OdePred< AD< AD<double> > > predEvaluatorADAD(
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
  const int nTheta = 2;
  const int nEta   = 2;
  const int nEps   = 1;

  // Set the current value for theta.
  valarray<double> thetaCurr( nTheta );
  thetaCurr[0] = 10.1;
  thetaCurr[1] = 1.2;

  // Set the limits for theta.
  valarray<double> thetaLow( nTheta );
  valarray<double> thetaUp ( nTheta );
  thetaLow[0] = -100.0;
  thetaUp [0] = +100.0;
  thetaLow[1] = -100.0;
  thetaUp [1] = +100.0;

  // Set the current value for eta.
  valarray<double> etaCurr( nEta );
  etaCurr[0] = 10.0 - thetaCurr[0];
  etaCurr[1] = 1.0  - thetaCurr[1];


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

  valarray<double> dataMean( nY_i );

  // Evaluate the data mean, which will cause the ODE's to be solved
  // and an exception to occur due to the compartment time derivatives
  // all being set equal to either Not a Number (NaN) or infinite.
  try
  {
    model.dataMean( dataMean );
  }
  catch( SpkException& e )
  {
    // Uncomment this line to see the list of exceptions.
    // cout << "e = " << e << endl;

    // See if there was an error during the solution of the ODE's.
    if ( e.find( SpkError::SPK_ODE_SOLN_ERR ) < 0 )
    {
      CPPUNIT_ASSERT_MESSAGE( 
        "An expected SPK exception(SPK_ODE_SOLN_ERR) did not occur during the evaluation of the data mean.",
        false );
    }

    // See if the error involved NaN's or infinities in the mean
    // model.
    if ( e.find( SpkError::SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR ) < 0 )
    {
      CPPUNIT_ASSERT_MESSAGE( 
        "An expected SPK exception (SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR) did not occur during the evaluation of the data mean.",
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


/*************************************************************************
 *
 * Function: FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_Test
 *
 *
 * The goal of this test is to check that the ODE-based version of the
 * Pred block expression evaluator works for the case of
 *
 *     FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_OdePred. 
 *
 *************************************************************************/

void OdePredBaseTest::FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace fourcomp_multinfus_odepredbasetest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of compartments, including the output compartment.
  int nComp = 4;

  // Set the number of data values for this individual.  For this test
  // there are nComp - 1 intervals each with nComp - 1 observations.
  int nY_iKnown = ( nComp - 1 ) * ( nComp - 1 );

  // Set this equal to false since the PK block expressions are not
  // functions of T.
  bool isPkBlockAFuncOfTime = false;

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

  // Set the flags for compartment 2.
  compInitialOff[2 - 1] = false;
  compNoOff     [2 - 1] = false;
  compNoDose    [2 - 1] = false;

  // Set the flags for compartment 3.
  compInitialOff[3 - 1] = false;
  compNoOff     [3 - 1] = false;
  compNoDose    [3 - 1] = false;

  // Set the flags for compartment 4 (the output compartment).
  compInitialOff[4 - 1] = true;
  compNoOff     [4 - 1] = false;
  compNoDose    [4 - 1] = true;

  // Set the relative tolerance for the ODE integration.
  double tolRel = 1.0e-6;

  // Construct the ODE-based versions of the Pred block evaluator.
  FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_OdePred< double > predEvaluator(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_OdePred< AD<double> > predEvaluatorAD(
    nY_iKnown,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  FourComp_MultInfus_SomeCompWithZeroMassAtFirstObserv_AdditiveY_OdePred< AD< AD<double> > > predEvaluatorADAD(
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
  const int nTheta = 2;
  const int nEta   = 2;
  const int nEps   = 1;

  // Set the current value for theta.
  valarray<double> thetaCurr( nTheta );
  thetaCurr[0] = 10.1;
  thetaCurr[1] = 1.2;

  // Set the limits for theta.
  valarray<double> thetaLow( nTheta );
  valarray<double> thetaUp ( nTheta );
  thetaLow[0] = -100.0;
  thetaUp [0] = +100.0;
  thetaLow[1] = -100.0;
  thetaUp [1] = +100.0;

  // Set the current value for eta.
  valarray<double> etaCurr( nEta );
  etaCurr[0] = 10.0 - thetaCurr[0];
  etaCurr[1] = 1.0  - thetaCurr[1];


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

  bool ok;

  // Evaluate these quantities at the current population and 
  // individual parameter values, which are set when the model
  // is constructed.
  model.dataMean         ( dataMean );
  model.dataVariance     ( dataVariance );
  model.dataVarianceInv  ( dataVarianceInv );

  ok = model.dataMean_popPar( dataMean_popPar );
  ok = model.dataMean_indPar( dataMean_indPar );

  ok = model.dataVariance_popPar   ( dataVariance_popPar );
  ok = model.dataVariance_indPar   ( dataVariance_indPar );
  ok = model.dataVarianceInv_popPar( dataVarianceInv_popPar );
  ok = model.dataVarianceInv_indPar( dataVarianceInv_indPar );

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

  double T;
  double ds;
  double w;
  double ka;
  double ke;
  double cl;
  double d;
  double e;

  // For this test, compartment 1 receives an infusion dose that
  // starts at time zero (t = 0), compartment 2 receives an infusion
  // dose that starts at time one (t = 1), and compartment 3 receives
  // an infusion dose that starts at time two (t = 2).
  //
  // Calculate the known value for the mean of the data:
  //
  //                            A   (t)  *  bioavailability   (theta, eta)
  //                             (p)                       (p)
  //     f    (theta, eta)  =  --------------------------------------------  ,
  //      i(j)                         scaleFactor   (theta, eta)
  //                                              (p)
  //
  // where
  //                -
  //               |  1,   if 0 < t <= 1,
  //               |
  //     comp  =  <   2,   if 1 < t <= 2,
  //               |
  //               |  3,   if 2 < t <= 3,
  //                -
  //
  //     t  =  ( j + 1 + j / 3 ) * timeStep )  ,
  //
  // and
  //
  //     p  =  comp - 1 .
  //
  int comp;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time.
    T = ( j + 1 + j / 3 ) * timeStep; 

    // Set the compartment.
    comp = j / 3 + 1;

    if ( comp == 1 )
    {
      dataMeanKnown[j] =   T         * comp *
        ( thetaCurr[1] + etaCurr[1] ) / ( thetaCurr[0] + etaCurr[0] );
    }
    else if ( comp == 2 )
    {
      dataMeanKnown[j] = ( T - 1.0 ) * comp *
        ( thetaCurr[1] + etaCurr[1] ) / ( thetaCurr[0] + etaCurr[0] );
    }
    else
    {
      dataMeanKnown[j] = ( T - 2.0 ) * comp *
        ( thetaCurr[1] + etaCurr[1] ) / ( thetaCurr[0] + etaCurr[0] );
    }
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
    //                                       -  f    ( alpha, b  )
    //      (0)                                  i(j)          i
    //     d       f    ( alpha, b  )  =  -------------------------------  .
    //      theta   i(j)          i  
    //                                     [ theta      +  eta    ]
    //                                            (0)         (0)
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = - dataMeanKnown[j] /
      ( thetaCurr[0] + etaCurr[0] );
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                          f    ( alpha, b  )
    //      (1)                                  i(j)          i
    //     d       f    ( alpha, b  )  =  -------------------------------  .
    //      theta   i(j)          i  
    //                                     [ theta      +  eta    ]
    //                                            (1)         (1)
    //
    col = k + thetaOffsetInPopPar;

    dataMean_popParKnown[j + col * nY_i] = dataMeanKnown[j] /
      ( thetaCurr[1] + etaCurr[1] );
  }

  // Calculate the known value for the derivative of the data mean
  // with respect to the individual parameter.
  dataMean_indParKnown = 0.0;
  k = 0;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                     -  f    ( alpha, b  )
    //      (0)                                i(j)          i
    //     d     f    ( alpha, b  )  =  -------------------------------  .
    //      eta   i(j)          i  
    //                                   [ theta      +  eta    ]
    //                                          (0)         (0)
    //
    dataMean_indParKnown[j + k * nY_i] = - dataMeanKnown[j] /
      ( thetaCurr[0] + etaCurr[0] );
  }
  k = 1;
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the known values for this column,
    //
    //                                        f    ( alpha, b  )
    //      (1)                                i(j)          i
    //     d     f    ( alpha, b  )  =  -------------------------------  .
    //      eta   i(j)          i  
    //                                   [ theta      +  eta    ]
    //                                          (1)         (1)
    //
    dataMean_indParKnown[j + k * nY_i] = dataMeanKnown[j] /
      ( thetaCurr[1] + etaCurr[1] );
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
      dataMeanKnown[j] * dataMeanKnown[j];
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
        dataMean_popParKnown[j + k * nY_i] * dataMeanKnown[j];
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
      dataMeanKnown[j] * dataMeanKnown[j] *
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

  // Set the tolerance for the model function comparisions to be 100
  // times less than the tolerance for the integration of the ODE's.
  tol = tolRel / 100.0;

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
}


/*************************************************************************
 *
 * Function: ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_Test
 *
 *
 * The goal of this test is to check that the ODE-based version of the
 * Pred block expression evaluator works for the case of
 *
 *     ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred,
 *
 * which has three occasions all having the identical experimental
 * design.
 *
 * 
 *************************************************************************/

void OdePredBaseTest::ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_Test()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace oneexpf_onebolus_odepredbasetest;

  int j;
  int k;


  //------------------------------------------------------------
  // Prepare the Pred block expression evaluator.
  //------------------------------------------------------------

  // Set the number of occasions for this individual.
  int nOccasion = 3;

  // Set the number of data values for this individual.
  int nY_iKnown = 5 * nOccasion;

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
  ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred< double > predEvaluator(
    nY_iKnown,
    nOccasion,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred< AD<double> > predEvaluatorAD(
    nY_iKnown,
    nOccasion,
    isPkBlockAFuncOfTime,
    nComp,
    defaultDoseComp,
    defaultObservComp,
    compInitialOff,
    compNoOff,
    compNoDose,
    tolRel );

  ThreeOccasions_OneExpF_OneBolus_NonObservPred_AdditivePlusThetaDepY_OdePred< AD< AD<double> > > predEvaluatorADAD(
    nY_iKnown,
    nOccasion,
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
  //[Remove]==================================================
  //
  try
  {
  //
  //[Remove]==================================================
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
  //[Remove]==================================================
  //
  }
  catch( SpkException& e )
  {
    cout << "e = " << e << endl;
  }
  //
  //[Remove]==================================================

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

  int nObservPerOccasion = nY_i / nOccasion;

  // For this test there is a single bolus at time zero and there are
  // non observation predication evaluation events.
  //
  // Calculate the known value for the mean of the data:
  //
  //                                      -                                  -   
  //                                     |                                    |
  //                            ds * exp |  - [ theta    + eta    ]  *  T     |
  //                                     |           (0)      (0)        (j)  |
  //                                      -                                  -   
  //    f    ( alpha, b  )  =  -------------------------------------------------  .
  //     i(j)          i
  //                                      [ theta    * w   +  eta    ]
  //                                             (1)             (1)
  //
  for ( j = 0; j < nY_i; j++ )
  {
    // Set the time taking into account the fact that the observations
    // occur at the (2 * ( j % nObservPerOccasion ) + 1)-th time step.
    T = timeStep * (2 * ( j % nObservPerOccasion ) + 1);

    ds = dose * wt;
    w = wt;

    ke = thetaCurr[0] + etaCurr[0];
    cl = thetaCurr[1] * w + etaCurr[1];
    d = std::exp(-ke * T);
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

    dataMean_popParKnown[j + col * nY_i] = - timeStep * (2 * ( j % nObservPerOccasion ) + 1) * dataMeanKnown[j];
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
    dataMean_indParKnown[j + k * nY_i] = - timeStep * (2 * ( j % nObservPerOccasion ) + 1) * dataMeanKnown[j];
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
  
  // Since the step sizes may change as OdePredBase evolves, only 
  // check that they are positive.
  for ( k = 0; k < nPopPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The step size for a population parameter element is invalid.",
      popParStep[k] != 0.0 );
  }

  // Since the limits may change as OdePredBase evolves, only check
  // that the lower limits are less than or equal to the upper.
  for ( k = 0; k < nIndPar; k++ )
  {
    CPPUNIT_ASSERT_MESSAGE( 
      "The limits for an individual parameter element are invalid.",
      indParLow[k] <= indParUp [k] );
  }
  
  // Since the step sizes may change as OdePredBase evolves, only 
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


