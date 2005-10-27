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
 *//**
 * @file OdePredBase.h
 * 
 * 
 * Declares OdePredBase class.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef ODEPREDBASE_H
#define ODEPREDBASE_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "PredBase.h"
#include "OdeBreak.h"

// SPK library header files.
#include <spk/intToOrdinalString.h>
#include <spk/SpkValarray.h>

// Standard library header files.
#include <algorithm>
#include <map>
#include <vector>


/*************************************************************************
 *
 * Class: OdePredBase
 *
 *//**
 * This abstract base class is a specialization of PredBase that
 * provides additional interfaces and support functions to evaluate
 * ordinary differential equation (ODE) based compartmental models.
 *//*
 *************************************************************************/

template <class Value>
class OdePredBase : public PredBase<Value>
{
  //------------------------------------------------------------
  // Constructors.
  //------------------------------------------------------------

  /***********************************************************************
   *
   * Function: OdePredBase
   *
   *//**
   * Constructor that must be called by constructors for concrete
   * subclasses of this abstract base class.
   */
  /***********************************************************************/

public:
  OdePredBase(
    bool                        isPkBlockAFuncOfTIn,
    int                         nCompIn,
    int                         defaultDoseCompIn,
    int                         defaultObservCompIn,
    const std::valarray<bool>&  compInitialOffIn,
    const std::valarray<bool>&  compNoOffIn,
    const std::valarray<bool>&  compNoDoseIn,
    double                      tolRelIn )
    :
    isPkBlockAFuncOfTime      ( isPkBlockAFuncOfTIn ),
    nComp                     ( nCompIn ),
    defaultDoseComp           ( defaultDoseCompIn ),
    defaultObservComp         ( defaultObservCompIn ),
    compAmount                ( nCompIn ),
    compAmount_t              ( nCompIn ),
    compInfusRate             ( nCompIn ),
    compInfusDurat            ( nCompIn ),
    compAbsorpLagTime         ( nCompIn ),
    compScaleParam            ( nCompIn ),
    compBioavailFrac          ( nCompIn ),
    compInitialOff            ( nCompIn ),
    compNoOff                 ( nCompIn ),
    compNoDose                ( nCompIn ),
    compIsOff                 ( nCompIn ),
    tolRel                    ( tolRelIn )
  {
    //----------------------------------------------------------
    // Set initial values.
    //----------------------------------------------------------

    // Set initial values for the parameters associated with
    // each of the compartments.
    int p;
    for ( p = 0; p < nComp; p++ )
    {
      compAmount       [p] = 0.0;
      compAmount_t     [p] = 0.0;

      compInfusRate    [p] = 0.0;
      compInfusDurat   [p] = 0.0;
      compAbsorpLagTime[p] = 0.0;

      compScaleParam   [p] = 1.0;
      compBioavailFrac [p] = 1.0;

      compInitialOff   [p] = compInitialOffIn[p];
      compNoOff        [p] = compNoOffIn     [p];
      compNoDose       [p] = compNoDoseIn    [p];

      compIsOff        [p] = compInitialOff  [p];
    }

    // Set initial values for these parameters.
    tscale  = 1.0;
    fo = 1.0;

    // Set initial values for these data items.
    mdv  = 0;
    evid = 0;
    cmt  = 0;
    amt  = 0.0;
    rate = 0.0;
    time = 0.0;

    // Set an invalid initial value for this data item that will be
    // reset if it appears in the data record.
    pcmt = -9999;
  }


  //------------------------------------------------------------
  // Virtual functions that can be specialized.
  //------------------------------------------------------------

  /***********************************************************************
   *
   * Function: initUserEnv
   *
   *//**
   * Performs any initializations of the environment for the users of
   * this class, i.e., the concrete subclasses of this abstract base
   * class, that are required for new values of i before calls to
   * evalPk(), evalDes(), and evalError() will work properly.
   */
  /***********************************************************************/

public:
  virtual void initUserEnv( int thetaOffset, int thetaLen,
                            int etaOffset,   int etaLen,
                            int epsOffset,   int epsLen,
                            int fOffset,     int fLen,
                            int yOffset,     int yLen,
                            int i,
                            int j,
                            const std::vector<Value>& indepVar,
                            std::vector<Value>& depVar )
  {
  }


  /***********************************************************************
   *
   * Function: saveUserEnv
   *
   *//**
   * Saves any information for the environment for the users of this
   * class, i.e., the concrete subclasses of this abstract base class,
   * that are required for the current values of i and j.
   */
  /***********************************************************************/

  virtual void saveUserEnv( int thetaOffset, int thetaLen,
                            int etaOffset,   int etaLen,
                            int epsOffset,   int epsLen,
                            int fOffset,     int fLen,
                            int yOffset,     int yLen,
                            int i,
                            int j,
                            const std::vector<Value>& indepVar,
                            const std::vector<Value>& depVar )
  {
  }


  //------------------------------------------------------------
  // Pure virtual functions that need to be implemented.
  //------------------------------------------------------------

  /***********************************************************************
   *
   * Function: readDataRecord
   *
   *//**
   * Sets the data items for the j-th data record for the i-th
   * individual .
   *
   * This function should set all of the predefined data items (DV,
   * MDV, AMT, CMT, EVID, PCMT, RATE, and TIME) that appear in the
   * $INPUT record, but it should not change the values for those that
   * don't appear there.
   */
  /***********************************************************************/

protected:
  virtual void readDataRecord( int i, int j ) = 0;


  /***********************************************************************
   *
   * Function: evalPk
   *
   *//**
   * Evaluates the expressions from the PK block.
   *
   * The i and j values passed in as arguments correspond to the
   * values used for the previous call to readDataRecord() that was
   * made before this function was called.
   *
   * Note that for SPK the continuous time variable T is a member of
   * this class so that it can, for example, be passed to the linear
   * interpolator in the PK block expressions like this
   *
   *     KA = THETA(1) * linearInterpolation( DATA_ITEM_NAME, T )
   */
  /***********************************************************************/

protected:
  virtual void evalPk( int thetaOffset, int thetaLen,
                       int etaOffset,   int etaLen,
                       int i,
                       int j,
                       const std::vector<Value>& indepVar ) = 0;


  /***********************************************************************
   *
   * Function: evalDes
   *
   *//**
   * Evaluates the expressions from the DES block.
   *
   * The i and j values passed in as arguments correspond to the
   * values used for the previous call to readDataRecord() that was
   * made before this function was called.
   */
  /***********************************************************************/

 protected:
  virtual void evalDes( int thetaOffset, int thetaLen,
                        int i,
                        int j,
                        const std::vector<Value>& indepVar ) = 0;


  /***********************************************************************
   *
   * Function: evalError 
   *
   *//**
   * Evaluates the expressions from the ERROR block.
   *
   * The i and j values passed in as arguments correspond to the
   * values used for the previous call to readDataRecord() that was
   * made before this function was called.
   */
  /***********************************************************************/

  virtual void evalError( int thetaOffset, int thetaLen,
                          int etaOffset,   int etaLen,
                          int epsOffset,   int epsLen,
                          int i,
                          int j,
                          const std::vector<Value>& indepVar ) = 0;


  //------------------------------------------------------------
  // Implementations of pure virtual functions from PredBase.
  //------------------------------------------------------------

  /***********************************************************************
   *
   * Function: eval
   *
   *//**
   * This function is a specialization of the eval() function for the
   * case where the Pred block expressions are implemented by
   * ODE-based equivalents.
   *
   * This function evaluates the ODE-based equivalents of the Pred
   * block expressions for the i-th individual's j-th data record at
   * the given values for the independent variables.
   *
   * The return value for this function will be true if this is an
   * observation event, i.e., if the DV data item is an observation
   * and is not missing.
   *
   * See the specification for PredBase::eval() for more details.
   */
  /***********************************************************************/

public:
  bool eval( int thetaOffset, int thetaLen,
             int etaOffset,   int etaLen,
             int epsOffset,   int epsLen,
             int fOffset,     int fLen,
             int yOffset,     int yLen,
             int i,
             int j,
             const std::vector<Value>& indepVar,
             std::vector<Value>& depVar )
  {
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------
  
    using namespace std;
  
  
    //----------------------------------------------------------
    // Set the calculated values for the dependent variables.
    //----------------------------------------------------------
  
    // This will be true if this is an observation event, i.e., if the
    // the DV data item is an observation and is not missing.
    bool isObsEvent;
  
    // Evaluate ODE-based equivalents of the Pred block expressions
    // for the i-th individual's j-th data record at the given values
    // for the independent variables.
    isObsEvent = evalOdePred(
      thetaOffset,
      thetaLen,
      etaOffset,
      etaLen,
      epsOffset,
      epsLen,
      fOffset,
      fLen,
      yOffset,
      yLen,
      i,
      j,
      indepVar,
      depVar);
  
  
    //----------------------------------------------------------
    // Finish up.
    //----------------------------------------------------------
  
    return isObsEvent;
  
  }


  //------------------------------------------------------------
  // Miscellaneous values.
  //------------------------------------------------------------

protected:
  void getIsPkBlockAFuncOfTime( bool& isPkBlockAFuncOfTimeOut ) const
    { isPkBlockAFuncOfTimeOut = isPkBlockAFuncOfTime; }  ///< Gets the value for isPkBlockAFuncOfTime.

private:
  const bool isPkBlockAFuncOfTime;  ///< Indicates if the PK block depends on the continuous time variable.


  //------------------------------------------------------------
  // Predefined model parameters.
  //------------------------------------------------------------

protected:
  Value& getT()      { return tCurr;  }  ///< Gets a non-const reference to the value for T.
  Value& getF()      { return f_i_j;  }  ///< Gets a non-const reference to the value for F.
  Value& getY()      { return y_i_j;  }  ///< Gets a non-const reference to the value for Y.
  Value& getFO()     { return fo;     }  ///< Gets a non-const reference to the value for FO.
  Value& getTSCALE() { return tscale; }  ///< Gets a non-const reference to the value for TSCALE.

  void getT     ( Value& tOut      ) const { tOut      = tCurr;      }  ///< Gets the value for T.
  void getF     ( Value& fOut      ) const { fOut      = f_i_j;      }  ///< Gets the value for F.
  void getY     ( Value& yOut      ) const { yOut      = y_i_j;      }  ///< Gets the value for Y.
  void getFO    ( Value& foOut     ) const { foOut     = fo;         }  ///< Gets the value for FO.
  void getTSCALE( Value& tscaleOut ) const { tscaleOut = tscale;     }  ///< Gets the value for TSCALE.

  void setT     ( const Value& tIn      ) { tCurr      = tIn;      }  ///< Sets the value for T.
  void setF     ( const Value& fIn      ) { f_i_j      = fIn;      }  ///< Sets the value for F.
  void setY     ( const Value& yIn      ) { y_i_j      = yIn;      }  ///< Sets the value for Y.
  void setFO    ( const Value& foIn     ) { fo         = foIn;     }  ///< Sets the value for FO.
  void setTSCALE( const Value& tscaleIn ) { tscale     = tscaleIn; }  ///< Sets the value for TSCALE.

private:
  Value tCurr;                      ///< Current value of the continuous time variable in the ODE's.
  Value f_i_j;                      ///< Scaled amount in the observation compartment for the current record.
  Value y_i_j;                      ///< Full statistical model for the observed value for the current record.
  Value fo;                         ///< Output fraction parameter.
  Value tscale;                     ///< Time scale parameter.


  //------------------------------------------------------------
  // Predefined data items.
  //------------------------------------------------------------

protected:
  int getEVID() const { return evid; }  ///< Gets the value for EVID.
  int getCMT () const { return cmt;  }  ///< Gets the value for CMT.
  int getPCMT() const { return pcmt; }  ///< Gets the value for PCMT.

  void setEVID( const int& evidIn ) { evid = evidIn; }  ///< Sets the value for EVID.
  void setCMT ( const int& cmtIn  ) { cmt  = cmtIn;  }  ///< Sets the value for CMT.
  void setPCMT( const int& pcmtIn ) { pcmt = pcmtIn; }  ///< Sets the value for PCMT.

  void getDV  ( Value& dvOut   ) const { dvOut   = dv;   }  ///< Gets the value for DV.
  void getMDV ( Value& mdvOut  ) const { mdvOut  = mdv;  }  ///< Gets the value for MDV.
  void getAMT ( Value& amtOut  ) const { amtOut  = amt;  }  ///< Gets the value for AMT.
  void getRATE( Value& rateOut ) const { rateOut = rate; }  ///< Gets the value for RATE.
  void getTIME( Value& timeOut ) const { timeOut = time; }  ///< Gets the value for TIME.

  void setDV  ( const Value& dvIn   ) { dv   = dvIn;   }  ///< Sets the value for DV.
  void setMDV ( const Value& mdvIn  ) { mdv  = mdvIn;  }  ///< Sets the value for MDV.
  void setAMT ( const Value& amtIn  ) { amt  = amtIn;  }  ///< Sets the value for AMT.
  void setRATE( const Value& rateIn ) { rate = rateIn; }  ///< Sets the value for RATE.
  void setTIME( const Value& timeIn ) { time = timeIn; }  ///< Sets the value for TIME.

private:
  int evid;                         ///< Event identification data item.
  int cmt;                          ///< Compartment number data item.
  int pcmt;                         ///< Prediction compartment number data item.

  Value dv;                         ///< Dependent variable data item.
  Value mdv;                        ///< Missing dependent variable data item.
  Value amt;                        ///< Dose amount data item.
  Value rate;                       ///< Dose rate data item.
  Value time;                       ///< Time data item.

public:
  enum eventId {
    OBSERV_EVENT,
    DOSE_EVENT,
    OTHER_TYPE_EVENT,
    RESET_EVENT,
    RESET_AND_DOSE_EVENT };


  //------------------------------------------------------------
  // Compartment related information.
  //------------------------------------------------------------

protected:
  int getNComp() const { return nComp; }  ///< Gets the value for nComp.

  ///< Gets a const iterator for compAmount.
  typename std::vector<Value>::const_iterator getCompAmountIterator() const
  { return compAmount.begin(); }

  ///< Gets a non-const iterator for compAmount_t.
  typename std::vector<Value>::iterator getCompAmount_tIterator()
  { return compAmount_t.begin(); }

  ///< Gets a non-const reference to the p-th value for compInfusRate.
  Value& getCompInfusRate( int p ) { return compInfusRate[p]; }

  ///< Gets a non-const reference to the p-th value for compInfusDurat.
  Value& getCompInfusDurat( int p ) { return compInfusDurat[p]; }

  ///< Gets a non-const reference to the p-th value for compAbsorpLagTime.
  Value& getCompAbsorpLagTime( int p ) { return compAbsorpLagTime[p]; }

  ///< Gets a non-const reference to the p-th value for compScaleParam.
  Value& getCompScaleParam( int p ) { return compScaleParam[p]; }

  ///< Gets a non-const reference to the p-th value for compBioavailFrac.
  Value& getCompBioavailFrac( int p ) { return compBioavailFrac[p]; }

  ///< Gets p-th value for compAmount.
  void getCompAmount( int p, Value& compAmount_pOut ) const
  { compAmount_pOut = compAmount[p]; }

  ///< Gets p-th value for compAmount_t.
  void getCompAmount_t( int p, Value& compAmount_t_pOut ) const
  { compAmount_t_pOut = compAmount_t[p]; }

  ///< Gets p-th value for compInfusRate.
  void getCompInfusRate( int p, Value& compInfusRate_pOut ) const
  { compInfusRate_pOut = compInfusRate[p]; }

  ///< Gets p-th value for compInfusDurat.
  void getCompInfusDurat( int p, Value& compInfusDurat_pOut ) const
  { compInfusDurat_pOut = compInfusDurat[p]; }

  ///< Gets p-th value for compAbsorpLagTime.
  void getCompAbsorpLagTime( int p, Value& compAbsorpLagTime_pOut ) const
  { compAbsorpLagTime_pOut = compAbsorpLagTime[p]; }

  ///< Gets p-th value for compScaleParam.
  void getCompScaleParam( int p, Value& compScaleParam_pOut ) const
  { compScaleParam_pOut = compScaleParam[p]; }

  ///< Gets p-th value for compBioavailFrac.
  void getCompBioavailFrac( int p, Value& compBioavailFrac_pOut ) const
  { compBioavailFrac_pOut = compBioavailFrac[p]; }

  ///< Sets the p-th value for compAmount.
  void setCompAmount( int p, const Value& compAmountIn )
  { compAmount[p] = compAmountIn; }

  ///< Sets the p-th value for compAmount_t.
  void setCompAmount_t( int p, const Value& compAmount_tIn )
  { compAmount_t[p] = compAmount_tIn; }

  ///< Sets the p-th value for compInfusRate.
  void setCompInfusRate( int p, const Value& compInfusRateIn )
  { compInfusRate[p] = compInfusRateIn; }

  ///< Sets the p-th value for compInfusDurat.
  void setCompInfusDurat( int p, const Value& compInfusDuratIn )
  { compInfusDurat[p] = compInfusDuratIn; }

  ///< Sets the p-th value for compAbsorpLagTime.
  void setCompAbsorpLagTime( int p, const Value& compAbsorpLagTimeIn )
  { compAbsorpLagTime[p] = compAbsorpLagTimeIn; }

  ///< Sets the p-th value for compScaleParam.
  void setCompScaleParam( int p, const Value& compScaleParamIn )
  { compScaleParam[p] = compScaleParamIn; }

  ///< Sets the p-th value for compBioavailFrac.
  void setCompBioavailFrac( int p, const Value& compBioavailFracIn )
  { compBioavailFrac[p] = compBioavailFracIn; }

private:
  const int nComp;                         ///< Number of compartments (including the ouput compartment).
  const int defaultDoseComp;               ///< Default dose compartment.
  const int defaultObservComp;             ///< Default observation compartment.

  std::vector<Value> compAmount;           ///< Current amount in each compartment.
  std::vector<Value> compAmount_t;         ///< Current time derivative of the amount in each compartment.
  std::vector<Value> compInfusRate;        ///< Current infusion rate for each compartment.
  std::vector<Value> compInfusDurat;       ///< Current infusion duration for each compartment.
  std::vector<Value> compAbsorpLagTime;    ///< Current absorption lag time for each compartment.
  std::vector<Value> compScaleParam;       ///< Current scale parameter for each compartment.
  std::vector<Value> compBioavailFrac;     ///< Current bio-availability for each compartment.

  std::valarray<bool> compInitialOff;      ///< Indicates which compartments are initially off.
  std::valarray<bool> compNoOff;           ///< Indicates which compartments may not be turned on or off.
  std::valarray<bool> compNoDose;          ///< Indicates which compartments may not receive a dose.
  std::valarray<bool> compIsOff;           ///< Indicates which compartments are currently off.

  std::vector<Value> compAmountAllOdeSoln; ///< Amount in each compartment for all of the ODE solution times.


  //------------------------------------------------------------
  // Compartment related functions.
  //------------------------------------------------------------

  /***********************************************************************
   *
   * Function: turnCompOn
   *
   *//**
   * Turns on compartment number comp.
   */
  /***********************************************************************/

protected:
  void turnCompOn( int comp )
  {
    // Get the index of the compartment to turn on.
    int pOn = compIndex( comp );

    // Set the initial amount in this compartment equal to zero.
    compAmount[pOn] = 0.0;

    // If the compartment being turned on is the output compartment,
    // then set its initial amount equal to the sum of the amounts in
    // all of the other compartments,
    //
    //             On         ---         On 
    //     A     (t  )  =     \       A (t  )  ,
    //      nComp             /        p     
    //                        ---            
    //                     p < nComp         
    //
    // where nComp is the number for the output compartment.
    if ( comp == nComp )
    {
      int m = compIndex( nComp );

      int p;
      for ( p = 0; p < nComp - 1; p++ )
      {
        compAmount[m] += compAmount[p];
      }
    }

    // Set the initial derivative with respect to time of the amount
    // in the compartment.
    compAmount_t[pOn] = 0.0;

    // Set the flag to indicate the compartment is on.
    compIsOff[pOn] = false;
  }


  /***********************************************************************
   *
   * Function: turnCompOff
   *
   *//**
   * Turns off compartment number comp.
   */
  /***********************************************************************/

  void turnCompOff( int comp )
  {
    // Get the index of the compartment to turn off.
    int pOff = compIndex( comp );

    // Zero the amount in the compartment and its derivative with
    // respect to time.
    compAmount  [pOff] = 0.0;
    compAmount_t[pOff] = 0.0;

    // Set the flag to indicate the compartment is off.
    compIsOff[pOff] = true;
  }


  /***********************************************************************
   *
   * Function: compIndex
   *
   *//**
   * Returns the index for compartment number p, which is one less
   * than the absolute value of the compartment number.
   *
   * The reason that the absolute value must be taken is that negative
   * values are allowed for compartments that are being turned off.
   */
  /***********************************************************************/

  int compIndex( int p ) const
  {
    assert( p != 0 );

    return abs( p ) - 1;
  }


  //------------------------------------------------------------
  // Experiment design related information.
  //------------------------------------------------------------

private:
  int nDataRec;                                   ///< Number of data records.

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - Unset Indices are Dangerous - Mitch ]
  //
  // Having unset indices for non ODE solution events and non
  // observation events seems dangerous since small subscripting
  // errors could easily cause the p to access catastropic data.
  //
  // Consider a different approach, e.g. only storing indices for
  // the events that are ODE solution events and having a second
  // array that shows where the index a given ODE solution record
  // is located in the first vector (like the optimizer parameter
  // array for elements that are not constrained.)
  //
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  std::vector<int>   dataRecOdeSolnIndex;         ///< Indices for data records in the vector of ODE solutions.
  std::vector<int>   dataRecObservIndex;          ///< Indices for data records in the vector of observations.

  int nOdeSoln;                                   ///< Number of ODE solutions to calculate.
  std::vector<int>   odeSolnComp;                 ///< ODE solution compartments.
  std::vector<int>   odeSolnBreakIndex;           ///< ODE solution break point indices.
  std::vector<Value> odeSolnTime;                 ///< ODE solution times.
  std::vector<bool>  odeSolnLeft;                 /// left continuous sol ?

  int nObserv;                                    ///< Number of observations.
  int nNonObservPred;                             ///< Number of nonobservation predictions.

  class BreakInfo                                 ///< Break point information.
  {
  public:
    BreakInfo()
    :
    time( 0.0 ), isDataRec( false ), indDataRecIndex( 0 )
    {
    }
    BreakInfo( const Value& timeIn, bool isDataRecIn, int indDataRecIndexIn )
    :
    time( timeIn ), isDataRec( isDataRecIn ), indDataRecIndex( indDataRecIndexIn )
    {
    }

    bool operator<( const BreakInfo& right ) const
    {
      return time < right.time;
    }

    Value time;                                   ///< Break point time.
    bool  isDataRec;                              ///< Indicates if break point corresponds to a data record.
    int   indDataRecIndex;                        ///< Current individual's data record index.

  };

  int nBreak;                                     ///< Number of break points.
  std::vector<BreakInfo> breakPoint;              ///< Break points.
  std::vector<Value>     breakTime;               ///< Break point times.

  int nBolus;                                     ///< Number of instantaneous bolus doses.
  std::vector<int>   bolusComp;                   ///< Instantaneous bolus dose compartments.
  std::vector<int>   bolusBreakIndex;             ///< Instantaneous bolus dose break point indices.
  std::vector<Value> bolusAmount;                 ///< Instantaneous bolus dose amounts.
  std::vector<Value> bolusTime;                   ///< Instantaneous bolus dose times.

  int nInfus;                                     ///< Number of regular infusion doses.
  std::vector<int>   infusComp;                   ///< Regular infusion dose compartments.
  std::vector<int>   infusOnBreakIndex;           ///< Regular infusion dose start break point indices.
  std::vector<int>   infusOffBreakIndex;          ///< Regular infusion dose finish break point indices.
  std::vector<Value> infusRate;                   ///< Regular infusion dose rates.
  std::vector<Value> infusDurat;                  ///< Regular infusion dose durations.
  std::vector<Value> infusTime;                   ///< Regular infusion dose times.

  class InfusOffInfo                              ///< Regular infusion dose off information.
  {
  public:
    InfusOffInfo()
    :
    time( 0.0 ), infusIndex( 0 )
    {
    }
    InfusOffInfo( const Value& timeIn, int infusIndexIn )
    :
    time( timeIn ), infusIndex( infusIndexIn )
    {
    }

    bool operator<( const InfusOffInfo& right ) const
    {
      return time < right.time;
    }

    Value time;                                   ///< Regular infusion dose off time.
    int   infusIndex;                             ///< Regular infusion dose index.

  };
  std::vector<InfusOffInfo> infusOffPoint;        ///< Regular infusion dose off information.

  int nZeroOrderBolus;                            ///< Number of zero-order bolus doses.
  std::vector<int>   zeroOrderBolusComp;          ///< Zero-order bolus dose compartments.
  std::vector<int>   zeroOrderBolusOnBreakIndex;  ///< Zero-order bolus dose start break point indices.
  std::vector<int>   zeroOrderBolusOffBreakIndex; ///< Zero-order bolus dose finish break point indices.
  std::vector<Value> zeroOrderBolusRate;          ///< Zero-order bolus dose rates.
  std::vector<Value> zeroOrderBolusDurat;         ///< Zero-order bolus dose durations.
  std::vector<Value> zeroOrderBolusTime;          ///< Zero-order bolus dose times.

  class ZeroOrderBolusOffInfo                     ///< Zero-order bolus dose off information.
  {
  public:
    ZeroOrderBolusOffInfo()
    :
    time( 0.0 ), zeroOrderBolusIndex( 0 )
    {
    }
    ZeroOrderBolusOffInfo( const Value& timeIn, int zeroOrderBolusIndexIn )
    :
    time( timeIn ), zeroOrderBolusIndex( zeroOrderBolusIndexIn )
    {
    }

    bool operator<( const ZeroOrderBolusOffInfo& right ) const
    {
      return time < right.time;
    }

    Value time;                                   ///< Zero-order bolus dose off time.
    int   zeroOrderBolusIndex;                    ///< Zero-order bolus dose index.

  };
  std::vector<ZeroOrderBolusOffInfo> zeroOrderBolusOffPoint;  ///< Zero-order bolus dose off information.

  int nTurnOnOrOff;                               ///< Number compartments turned on or off events.

  std::vector<int>   turnOnOrOffComp;             ///< Compartments to be turned on or off.
  std::vector<Value> turnOnOrOffTime;             ///< Times to turn the compartments on or off.


  //------------------------------------------------------------
  // Experiment design related functions.
  //------------------------------------------------------------

  /***********************************************************************
   *
   * Function: getExpDesign
   *
   *//**
   * Gets the experimental design information for the i-th individual.
   */
  /***********************************************************************/

protected:
  void getExpDesign( int thetaOffset, int thetaLen,
                     int etaOffset,   int etaLen,
                     int i,
                     const std::vector<Value>& indepVar )
  {
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------

    using namespace std;


    //----------------------------------------------------------
    // Validate the default compartment numbers.
    //----------------------------------------------------------

    // Check that the default observation compartment is greater than 1.
    if ( defaultObservComp < 1 )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The number for the default observation compartment was less than one.",
        __LINE__, 
        __FILE__ );
    }

    // Check that the default dose compartment is greater than 1.
    if ( defaultDoseComp < 1 )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The number for the default dose compartment was less than one.",
        __LINE__, 
        __FILE__ );
    }


    //----------------------------------------------------------
    // Prepare the individual's experiment design information.
    //----------------------------------------------------------

    // Get the total number of data records for this individual.
    nDataRec = getNRecords( i );

    // Set the sizes for the vectors of ODE solution indices and
    // observation indices.
    dataRecOdeSolnIndex.resize( nDataRec );
    dataRecObservIndex .resize( nDataRec );

    nObserv        = 0;
    nNonObservPred = 0;

    odeSolnComp         .resize( 0 );
    odeSolnTime         .resize( 0 );
    odeSolnLeft         .resize( 0 );

    breakPoint          .resize( 0 );
    breakTime           .resize( 0 );

    bolusComp           .resize( 0 );
    bolusAmount         .resize( 0 );
    bolusTime           .resize( 0 );

    infusComp           .resize( 0 );
    infusRate           .resize( 0 );
    infusDurat          .resize( 0 );
    infusTime           .resize( 0 );

    zeroOrderBolusComp  .resize( 0 );
    zeroOrderBolusRate  .resize( 0 );
    zeroOrderBolusDurat .resize( 0 );
    zeroOrderBolusTime  .resize( 0 );

    turnOnOrOffComp     .resize( 0 );
    turnOnOrOffTime     .resize( 0 );


    //----------------------------------------------------------
    // Get the individual's experiment design information.
    //----------------------------------------------------------

    int eventId;
    int comp;
    int predComp;

    int p;

    // Get all of the experiment design information for 
    // the current individual.
    int j;
    for ( j = 0; j < nDataRec; j++ )
    {
      //--------------------------------------------------------
      // Prepare the data items and PK parameters.
      //--------------------------------------------------------

      // Get the data items for the current data record.
      readDataRecord( i, j );

      // Evaluate the current values for the PK parameters because the
      // rates and durations of zero-order bolus doses can be set in
      // the PK block.
      evalPk(
        thetaOffset,
        thetaLen,
        etaOffset,
        etaLen,
        i,
        j,
        indepVar );


      //--------------------------------------------------------
      // Handle observation events.
      //--------------------------------------------------------

      if ( evid == OBSERV_EVENT )
      {
        //------------------------------------------------------
        // Handle observations.
        //------------------------------------------------------

        // If this is an observation event, then get its experiment
        // design related information.
        // Set the compartment.
        if ( cmt == 0 )
        {
          // If the compartment is not specified, then the observation
          // will come from the default compartment.
          odeSolnComp.push_back( defaultObservComp );
        }
        else
        {
          // If the compartment is specified, then the observation
          // will come from that compartment.
          odeSolnComp.push_back( cmt );
        }

        // Set the time.
        odeSolnTime.push_back( time );
        odeSolnLeft.push_back( true );

        // If this observation event is for the output compartment,
        // then see if it needs to be turned off after the
        // observation.
        if ( odeSolnComp.back() == nComp && odeSolnComp.back() < 0 )
        {
          // Set the compartment and time.
          turnOnOrOffComp.push_back( odeSolnComp.back() );
          turnOnOrOffTime.push_back( time );
        }

        // Set this data record's index in the vector of
        // ODE solutions.
        dataRecOdeSolnIndex[j] = ( odeSolnTime.size() - 1 );

        // Set this data record's index in the vector of observations.
        dataRecObservIndex[j] = nObserv;

        nObserv++;

        // Add a break point for this observation event.
        breakPoint.push_back( BreakInfo( odeSolnTime.back(), true, j ) );
      }


      //--------------------------------------------------------
      // Handle dose events.
      //--------------------------------------------------------

      if ( evid == DOSE_EVENT )
      {
        //------------------------------------------------------
        // Handle instantaneous bolus doses.
        //------------------------------------------------------

        // If this is an instantaneous bolus dose, then get its
        // experiment design related information.
        if ( amt > 0.0 && rate == 0.0 )
        {
          // Set the compartment.
          if ( cmt == 0 )
          {
            // If the compartment is not specified, then the dose goes in
            // the default comparment.
            bolusComp.push_back( defaultDoseComp );
          }
          else
          {
            // If the compartment is specified, then the dose goes in
            // that compartment.
            bolusComp.push_back( cmt );
          }
  
          // Doses are not allowed for the output compartment.
          if ( bolusComp.back() == nComp )
          {
            string message = "The " + 
              intToOrdinalString( j, ZERO_IS_FIRST_INT ) +
              " data record for the " + 
              intToOrdinalString( i, ZERO_IS_FIRST_INT ) +
              " individual specified a dose for the \noutput compartment, which is not allowed.";

            throw SpkException(
              SpkError::SPK_USER_INPUT_ERR, 
              message.c_str(),
              __LINE__, 
              __FILE__ );
          }

          // Set the amount and time, with any lag time for this
          // compartment included.
          bolusAmount.push_back( amt );
          bolusTime  .push_back( time + 
            compAbsorpLagTime[ compIndex( bolusComp.back() ) ] );

          // Add a break point for this dose.
          breakPoint.push_back( BreakInfo( bolusTime.back(), true, j ) );
        }

        //------------------------------------------------------
        // Handle regular infusion doses.
        //------------------------------------------------------

        // If this is a regular infusion dose, then get its experiment
        // design related information.
        if ( amt > 0.0 && rate > 0.0 )
        {
          // Set the compartment.
          if ( cmt == 0 )
          {
            // If the compartment is not specified, then the dose goes in
            // the default comparment.
            infusComp.push_back( defaultDoseComp );
          }
          else
          {
            // If the compartment is specified, then the dose goes in
            // that compartment.
            infusComp.push_back( cmt );
          }
  
          // Doses are not allowed for the output compartment.
          if ( infusComp.back() == nComp )
          {
            string message = "The " + 
              intToOrdinalString( j, ZERO_IS_FIRST_INT ) +
              " data record for the " + 
              intToOrdinalString( i, ZERO_IS_FIRST_INT ) +
              " individual specified a dose for the \noutput compartment, which is not allowed.";

            throw SpkException(
              SpkError::SPK_USER_INPUT_ERR, 
              message.c_str(),
              __LINE__, 
              __FILE__ );
          }

          // Set the rate, duration, and time, with any lag time for
          // this compartment included.
          infusRate .push_back( rate );
          infusDurat.push_back( amt / rate );
          infusTime .push_back( time + 
            compAbsorpLagTime[ compIndex( infusComp.back() ) ] );
  
          // Add a break point at the beginning of this regular
          // infusion dose.
          breakPoint.push_back( BreakInfo( infusTime.back(), true, j ) );

          // Add a break point at the end of this regular infusion
          // dose that does not correspond to a date record .
          breakPoint.push_back( BreakInfo( infusTime.back() +
                                           infusDurat.back(), false, j ) );
        }


        //------------------------------------------------------
        // Handle zero-order bolus doses.
        //------------------------------------------------------

        // If this is a zero-order bolus dose, then get its experiment
        // design related information.  
        // 
        // A zero-order bolus dose is just like a regular infusion
        // dose except that the duration and/or rate is calculated in
        // the PK block.
        if ( amt > 0.0 && ( rate == -2.0 || rate == -1.0 ) )
        {
          // Set the compartment.
          if ( cmt == 0 )
          {
            // If the compartment is not specified, then the dose goes in
            // the default comparment.
            zeroOrderBolusComp.push_back( defaultDoseComp );
          }
          else
          {
            // If the compartment is specified, then the dose goes in
            // that compartment.
            zeroOrderBolusComp.push_back( cmt );
          }
  
          // Doses are not allowed for the output compartment.
          if ( zeroOrderBolusComp.back() == nComp )
          {
            string message = "The " + 
              intToOrdinalString( j, ZERO_IS_FIRST_INT ) +
              " data record for the " + 
              intToOrdinalString( i, ZERO_IS_FIRST_INT ) +
              " individual specified a dose for the \noutput compartment, which is not allowed.";

            throw SpkException(
              SpkError::SPK_USER_INPUT_ERR, 
              message.c_str(),
              __LINE__, 
              __FILE__ );
          }

          p = compIndex( zeroOrderBolusComp.back() );

          // Set the rate and duration.
          if ( rate == -2.0 )
          {
            // In this case, the duration was set in the PK block.
            zeroOrderBolusDurat.push_back( compInfusDurat[p] );

            zeroOrderBolusRate.push_back( amt / zeroOrderBolusDurat.back() );
          }
          else
          {
            // In this case, the rate was set in the PK block.
            zeroOrderBolusRate.push_back( compInfusRate[p] );

            zeroOrderBolusDurat.push_back( amt / zeroOrderBolusRate.back() );
          }

          // Set the time, with any lag time for this compartment
          // included.
          zeroOrderBolusTime.push_back( time + compAbsorpLagTime[p] );
  
          // Add a break point at the beginning of this zero-order
          // bolus dose.
          breakPoint.push_back( BreakInfo( zeroOrderBolusTime.back(), true, j ) );

          // Add a break point at the end of this zero-order bolus
          // dose that does not correspond to a date record .
          breakPoint.push_back( BreakInfo( zeroOrderBolusTime.back() +
                                           zeroOrderBolusDurat.back(), false, j ) );
        }

      }


      //--------------------------------------------------------
      // Handle other-type events.
      //--------------------------------------------------------

      if ( evid == OTHER_TYPE_EVENT )
      {
        //------------------------------------------------------
        // Handle compartments being turned on and off.
        //------------------------------------------------------

        // If this is a turn-a-compartment-on-or-off event, then get
        // its experiment design related information.
        if ( cmt != 0 )
        {
          // Check that this compartment may be turned on or off.
          if( compNoOff[ compIndex( cmt ) ] )
          {
            string message = "The " + 
              intToOrdinalString( j, ZERO_IS_FIRST_INT ) +
              " data record for the " + 
              intToOrdinalString( i, ZERO_IS_FIRST_INT ) +
              " individual specified that the " + 
              intToOrdinalString( cmt, ONE_IS_FIRST_INT ) +
              "\ncompartment should be turned " +
              ( cmt > 0 ? "on" : "off" ) +
              ", which is not allowed.";

            throw SpkException(
              SpkError::SPK_USER_INPUT_ERR, 
              message.c_str(),
              __LINE__, 
              __FILE__ );
          }

          // Set the compartment and the time.
          turnOnOrOffComp.push_back( cmt );
          turnOnOrOffTime.push_back( time );
        }

        //------------------------------------------------------
        // Handle nonobservation prediction evaluations.
        //------------------------------------------------------

        // If this is an nonobservation prediction evaluation event,
        // i.e., if the PCMT data item is present in the data record
        // and its value has been reset from its initial value, then
        // get its experiment design related information.
        //
        // A nonobservation prediction evaluation is handled just like
        // an observation event, but it won't appear in the objective
        // function calculation.
        if ( pcmt >= 0 )
        {
          // Set the compartment.
          if ( pcmt == 0 )
          {
            // If the compartment is not specified, then the
            // nonobservation prediction evaluation will be for the
            // default compartment.
            odeSolnComp.push_back( defaultObservComp );
          }
          else
          {
            // If the compartment is specified, then the
            // nonobservation prediction evaluation will be
            // for that compartment.
            odeSolnComp.push_back( pcmt );
          }
  
          // Set the time.
          odeSolnTime.push_back( time );
          odeSolnLeft.push_back( true );
  
          // Set this data record's index in the vector of
          // ODE solutions.
          dataRecOdeSolnIndex[j] = ( odeSolnTime.size() - 1 );

          nNonObservPred++;

          // Add a break point for this nonobservation prediction
          // evaluation event.
          breakPoint.push_back( BreakInfo( odeSolnTime.back(), true, j ) );
        }

      }

    }


    //----------------------------------------------------------
    // Set the sizes of the experiment design information.
    //----------------------------------------------------------

    nOdeSoln        = odeSolnTime       .size();
    nBreak          = breakPoint        .size();
    nBolus          = bolusTime         .size();
    nInfus          = infusTime         .size();
    nZeroOrderBolus = zeroOrderBolusTime.size();
    nTurnOnOrOff    = turnOnOrOffTime   .size();

    compAmountAllOdeSoln.resize( nComp * nOdeSoln );

    odeSolnBreakIndex          .resize( nOdeSoln );
    bolusBreakIndex            .resize( nBolus );
    infusOnBreakIndex          .resize( nInfus );
    infusOffBreakIndex         .resize( nInfus );
    zeroOrderBolusOnBreakIndex .resize( nZeroOrderBolus );
    zeroOrderBolusOffBreakIndex.resize( nZeroOrderBolus );


    //----------------------------------------------------------
    // Get the regular infusion dose off times and sort them.
    //----------------------------------------------------------

    int q;

    // If there are any regular infusion doses, then get their off
    // times and sort them.
    if ( nInfus > 0 )
    {
      infusOffPoint.resize( 0 );

      // Save the regular infusion dose off times and their indices.
      for ( q = 0; q < nInfus; q++ )
      {
        infusOffPoint.push_back( 
          InfusOffInfo( ( infusTime[q] + infusDurat[q] ), q ) );
      }

      // Sort the off times.        
      sort( infusOffPoint.begin(), infusOffPoint.end() );
    }


    //----------------------------------------------------------
    // Get the zero-order bolus dose off times and sort them.
    //----------------------------------------------------------

    // If there are any zero-order bolus doses, then get their off
    // times and sort them.
    if ( nZeroOrderBolus > 0 )
    {
      zeroOrderBolusOffPoint.resize( 0 );

      // Save the zero-order bolus dose off times and their indices.
      for ( q = 0; q < nZeroOrderBolus; q++ )
      {
        zeroOrderBolusOffPoint.push_back( 
          ZeroOrderBolusOffInfo( ( zeroOrderBolusTime[q] + zeroOrderBolusDurat[q] ), q ) );
      }

      // Sort the off times.        
      sort( zeroOrderBolusOffPoint.begin(), zeroOrderBolusOffPoint.end() );
    }


    //----------------------------------------------------------
    // Assign break point indices to the various quantities.
    //----------------------------------------------------------

    // Sort the break point information in order by break point times
    // because the break point handler expects the break point times
    // to be in nondecreasing order.
    sort( breakPoint.begin(), breakPoint.end() );

    int odeSolnCounter           = 0;
    int bolusCounter             = 0;
    int infusOnCounter           = 0;
    int infusOffCounter          = 0;
    int zeroOrderBolusOnCounter  = 0;
    int zeroOrderBolusOffCounter = 0;

    // Assign break point indices to the various quantities making
    // sure that they are unique.
    int k;
    for ( k = 0; k < nBreak; k++ )
    {
      //--------------------------------------------------------
      // Assign indices for ODE solution times.
      //--------------------------------------------------------

      // Check for ODE solution times at this break time.
      if ( odeSolnCounter < nOdeSoln )
      {
        if ( odeSolnTime[odeSolnCounter] == breakPoint[k].time )
        {
          odeSolnBreakIndex[odeSolnCounter] = k;
          odeSolnCounter++;

          continue;
        }
      }

      //--------------------------------------------------------
      // Assign indices for instantaneous bolus doses.
      //--------------------------------------------------------

      // Check for instantaneous bolus doses at this break time.
      if ( bolusCounter < nBolus )
      {
        if ( bolusTime[bolusCounter] == breakPoint[k].time )
        {
          bolusBreakIndex[bolusCounter] = k;
          bolusCounter++;

          continue;
        }
      }

      //--------------------------------------------------------
      // Assign indices for regular infusion doses.
      //--------------------------------------------------------

      // Check for regular infusion doses turning on.
      if ( infusOnCounter < nInfus )
      {
        if ( infusTime[infusOnCounter] == breakPoint[k].time )
        {
          infusOnBreakIndex[infusOnCounter] = k;
          infusOnCounter++;

          continue;
        }
      }

      // Check for regular infusion doses turning off.
      if ( infusOffCounter < nInfus )
      {
        if ( infusOffPoint[infusOffCounter].time == breakPoint[k].time )
        {
          infusOffBreakIndex[ infusOffPoint[infusOffCounter].infusIndex ] =  k;
          infusOffCounter++;

          continue;
        }
      }

      //--------------------------------------------------------
      // Assign indices for zero-order bolus doses.
      //--------------------------------------------------------

      // Check for zero-order bolus doses turning on.
      if ( zeroOrderBolusOnCounter < nZeroOrderBolus )
      {
        if ( zeroOrderBolusTime[zeroOrderBolusOnCounter] == breakPoint[k].time )
        {
          zeroOrderBolusOnBreakIndex[zeroOrderBolusOnCounter] = k;
          zeroOrderBolusOnCounter++;

          continue;
        }
      }

      // Check for zero-order bolus doses turning off.
      if ( zeroOrderBolusOffCounter < nZeroOrderBolus )
      {
        if ( zeroOrderBolusOffPoint[zeroOrderBolusOffCounter].time == breakPoint[k].time )
        {
          zeroOrderBolusOffBreakIndex[ 
            zeroOrderBolusOffPoint[zeroOrderBolusOffCounter].zeroOrderBolusIndex ] =  k;
          zeroOrderBolusOffCounter++;

          continue;
        }
      }
    }

    // Check that there have been the proper number of break point
    // indices assigned.
    if ( nBreak                  != odeSolnCounter           +
                                    bolusCounter             +
                                    infusOnCounter           +
                                    infusOffCounter          +
                                    zeroOrderBolusOnCounter  +
                                    zeroOrderBolusOffCounter    ||
         infusOnCounter          != infusOffCounter             ||
         nInfus                  != infusOnCounter              ||
         zeroOrderBolusOnCounter != zeroOrderBolusOffCounter    ||
         nZeroOrderBolus         != zeroOrderBolusOnCounter )
    {
      string message = "The proper number of break points could not be determined for the \n" + 
        intToOrdinalString( i, ZERO_IS_FIRST_INT ) +
        " individuals data records.";

      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        message.c_str(),
        __LINE__, 
        __FILE__ );
    }


    //----------------------------------------------------------
    // Remove any break point times beyond the last ODE solution time.
    //----------------------------------------------------------

    // Delete any break point times that are greater than or equal to
    // the last ODE solution time because the break point handler
    // expects the last break point time to be less than the last ODE
    // solution time.
    while ( nBreak > 0 )
    {
      if ( breakPoint[nBreak - 1].time >= odeSolnTime[nOdeSoln - 1] )
      {
        breakPoint.pop_back();
        nBreak--;
      }
      else
      {
        break;
      }
    }

    // Set the vector of break times for OdeBreak().
    breakTime.resize( nBreak );
    for ( k = 0; k < nBreak; k++ )
    {
      breakTime[k] = breakPoint[k].time;
    }


    //----------------------------------------------------------
    // Check the sizes of the experiment design information.
    //----------------------------------------------------------

    assert( nOdeSoln                   == nObserv + nNonObservPred );

    assert( odeSolnComp        .size() == nOdeSoln );

    assert( bolusComp          .size() == nBolus );
    assert( bolusAmount        .size() == nBolus );

    assert( infusComp          .size() == nInfus );
    assert( infusRate          .size() == nInfus );
    assert( infusDurat         .size() == nInfus );

    assert( zeroOrderBolusComp .size() == nZeroOrderBolus );
    assert( zeroOrderBolusRate .size() == nZeroOrderBolus );
    assert( zeroOrderBolusDurat.size() == nZeroOrderBolus );

    assert( turnOnOrOffComp    .size() == nTurnOnOrOff );

  }


  //------------------------------------------------------------
  // Quantities related to evaluating the differential equations.
  //------------------------------------------------------------

private:
  Value tolRel;           ///< Relative tolerance parameter for the differential equations.

  int iCurr;              ///< Current individual index.
  int jCurr;              ///< Current individual's data record index.
  int kCurr;              ///< Current break point index.

  int thetaOffsetForOde;  ///< Local copy of thetaOffset for call to Ode().
  int thetaLenForOde;     ///< Local copy of thetaLen for call to Ode().
  int etaOffsetForOde;    ///< Local copy of etaOffset for call to Ode().
  int etaLenForOde;       ///< Local copy of etaLen for call to Ode().

  const std::vector<Value>* pIndepVarForOde;   ///< Pointer to indepVar for call to Ode().


  //------------------------------------------------------------
  // Functions required by OdeBreak.
  //------------------------------------------------------------

  /***********************************************************************
   *
   * Function: Break
   *
   *//**
   * This function is called by OdeBreak to set the index for the
   * current break point kIn and the current value for the amounts in
   * all of the compartments compAmountIn.
   *
   * The output value bolusAmountOut will be set equal to the bolus
   * amounts added to each of the compartments at the beginning of
   * this break time interval.
   */
  /***********************************************************************/

public:
  void Break(
    size_t                    kIn,
    const std::vector<Value>& compAmountIn,
    std::vector<Value>&       bolusAmountOut )
  {
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------

    using namespace std;

    assert( kIn >= 0 && kIn < nBreak );

    assert( compAmountIn  .size() == nComp );
    assert( bolusAmountOut.size() == nComp );


    //----------------------------------------------------------
    // Do any preparations required for this break point.
    //----------------------------------------------------------

    // Set the current break point index.
    kCurr = kIn;

    // If this break point corresponds to a data record, then do
    // preparations related to the data record.
    if ( breakPoint[kCurr].isDataRec )
    {
      // Set the current individual's data record index.
      jCurr = breakPoint[kCurr].indDataRecIndex;

      // Get the data items for the current data record.
      readDataRecord( iCurr, jCurr );
    }


    //----------------------------------------------------------
    // Set the instantaneous bolus doses for this break point.
    //----------------------------------------------------------

    // Zero the instantaneous bolus doses for all of the compartments.
    int p;
    for ( p = 0; p < nComp; p++ )
    {
      bolusAmountOut[p] = 0.0;
    }

    // Find any instantaneous bolus doses for the current break point.
    int q = 0;
    while ( q < nBolus )
    {
      // If this instantaneous bolus dose occurs at the beginning of
      // the current break point, then add in its contribution.
      if ( bolusBreakIndex[q] == kCurr )
      {
        p = compIndex( bolusComp[q] );

        // Add the amount for this instantaneous bolus, multiplied by
        // its bioavailability fraction, to its compartment's bolus
        // amount.
        bolusAmountOut[p] += compBioavailFrac[p] * bolusAmount[q];

        // Turn the compartment on if it is currently off.
        if ( compIsOff[p] )
        {
          turnCompOn( bolusComp[q] );
        }
      }

      q++;
    }


    //----------------------------------------------------------
    // Turn on compartments that will get regular infusion doses.
    //----------------------------------------------------------

    // Find any regular infusion doses that begin at the current break
    // point.
    q = 0;
    while ( q < nInfus )
    {
      // If this infusion begins at this break point, then see if its
      // compartment needs to be turned on.
      if ( infusOnBreakIndex[q] == kCurr )
      {
        // Turn the compartment on if it is currently off.
        if ( compIsOff[ compIndex( infusComp[q] ) ] )
        {
          turnCompOn( infusComp[q] );
        }
      }

      q++;
    }


    //----------------------------------------------------------
    // Turn on compartments that will get zero-order bolus doses.
    //----------------------------------------------------------

    // Find any zero-order bolus doses that begin at the current break
    // point.
    q = 0;
    while ( q < nZeroOrderBolus )
    {
      // If this zero-order bolus begins at this break point, then see
      // if its compartment needs to be turned on.
      if ( zeroOrderBolusOnBreakIndex[q] == kCurr )
      {
        // Turn the compartment on if it is currently off.
        if ( compIsOff[ compIndex( zeroOrderBolusComp[q] ) ] )
        {
          turnCompOn( zeroOrderBolusComp[q] );
        }
      }

      q++;
    }


    //----------------------------------------------------------
    // Turn on or off any other compartments that require it.
    //----------------------------------------------------------

    // Find any compartments that need to be turned on or off at the
    // current break point.  This loop assumes the turn-compartments-on-
    // or-off times are in nondecreasing order.
    q = 0;
    while ( q < nTurnOnOrOff && turnOnOrOffTime[q] <= breakTime[kCurr] )
    {
      // If this turn-compartment-on-or-off time is at the beginning
      // of the current break point, then turn on or off the
      // appropriate compartment.
      if ( turnOnOrOffTime[q] == breakTime[kCurr] )
      {
        p = compIndex( turnOnOrOffComp[q] );

        // If the compartment number is positive, then this
        // compartment should be turned on.
        if ( turnOnOrOffComp[q] > 0 )
        {
          // Only turn the compartment on if it's currently off.
          if ( compIsOff[p] )
          {
            turnCompOn( turnOnOrOffComp[q] );
          }
        }

        // If the compartment number is negative, then this
        // compartment should be turned off.
        if ( turnOnOrOffComp[q] < 0 )
        {
          // Only turn the compartment off if it's currently on.
          if ( !compIsOff[p] )
          {
            turnCompOff( turnOnOrOffComp[q] );
          }
        }
      }

      q++;
    }


    //----------------------------------------------------------
    // Add the sum of the instantaneous bolus doses to the output compartment.
    //----------------------------------------------------------

    int m = compIndex( nComp );

    // The output compartment contains the total amount eliminated
    // from the system since the time tOn when the output compartment
    // was turned on,
    //
    //                               -                                         -
    //                      ---     |      On      ---          On              | 
    //     A     (t)  =     \       |  A (t  )     \    doses (t   <= t  <  t)  |  ,
    //      nComp           /       |   p       +  /         p         q        | 
    //                      ---     |              ---                          |   
    //                   p < nComp  |               q                           |   
    //                               -                                         -
    //
    //                       ---         
    //                   -   \       A (t)  
    //                       /        p        
    //                       ---            
    //                     p < nComp          
    //
    // where nComp is the number for the output compartment and the
    // sum over doses represents all of the doses to a particular
    // compartment between time tOn and the current time t.
    //
    // The amount in the output compartment is the total of the
    // amounts in all of the compartments at tOn plus the total amount
    // added to all of the compartments since that time minus the
    // total of the amounts in all of the compartments at time t.
    //
    // If the output compartment is turned on, then add all of the
    // instantaneous bolus doses to the instantaneous bolus dose that
    // it will receive.
    if ( !compIsOff[m] )
    {
      for ( p = 0; p < nComp - 1; p++ )
      {
        bolusAmountOut[m] += bolusAmountOut[p];
      }
  
      // Set the fraction of the bolus dose that actually makes it to
      // the output compartment.
      bolusAmountOut[m] *= fo;
    }

  }


  /***********************************************************************
   *
   * Function: Ode
   *
   *//**
   * This function is called by OdeBreak to set the current value for
   * the continuous time variable that appears in the ordinary
   * differential equations tIn and the current value for the amounts
   * in all of the compartments compAmountIn.
   *
   * The output value compAmount_tOut will be set equal to the
   * derivative with respect to time of the amounts in each of the
   * compartments for the current time.
   */
  /***********************************************************************/

  // Empty stub functions put in by Brad on 05-09-15 so Runge45 method would 
  // work with new version of OdeBreak. These should eventually use
  // Ode together with CppAD to get the derivatives necessary for Rosen34.

  void Ode_ind(
    Value                     tIn                 ,
    const std::vector<Value>& compAmountIn        ,
    std::vector<Value>&       compAmount_tOut_ind )
  { throw SpkException(
      SpkError::SPK_UNKNOWN_ERR                  , 
      "Stiff integration is not yet implemented" ,
        __LINE__                                 , 
        __FILE__                                 
    );
  }
  void Ode_dep(
    Value                     tIn                 ,
    const std::vector<Value>& compAmountIn        ,
    std::vector<Value>&       compAmount_tOut_dep )
  { throw SpkException(
      SpkError::SPK_UNKNOWN_ERR                  , 
      "Stiff integration is not yet implemented" ,
        __LINE__                                 , 
        __FILE__                                 
    );
  }

  void Ode(
    Value                     tIn,
    const std::vector<Value>& compAmountIn,
    std::vector<Value>&       compAmount_tOut )
  {
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------

    using namespace std;

    assert( compAmountIn   .size() == nComp );
    assert( compAmount_tOut.size() == nComp );

    // Set the current value for the continuous time variable that
    // appears in the ordinary differential equations.
    tCurr = tIn;

    // Set the current value for the amounts in the compartments.
    compAmount = compAmountIn;


    //----------------------------------------------------------
    // Evaluate the ordinary differential equations.
    //----------------------------------------------------------

    // Evaluate the expressions from the PK block because they may
    // depend on the continuous time variable T or on the values for
    // some of the data items in the current data record.
    evalPk(
      thetaOffsetForOde,
      thetaLenForOde,
      etaOffsetForOde,
      etaLenForOde,
      iCurr,
      jCurr,
      *pIndepVarForOde );

    // Evaluate the ordinary differential equations that appear in the
    // DES block.
    evalDes(
      thetaOffsetForOde,
      thetaLenForOde,
      iCurr,
      jCurr,
      *pIndepVarForOde );

    // Set the current derivative with respect to time of the amounts
    // in the compartments.
    compAmount_tOut = compAmount_t;


    //----------------------------------------------------------
    // Add in the regular infusion doses' derivative contributions.
    //----------------------------------------------------------

    // Find any regular infusion doses that are active for the current
    // break time interval.
    int q = 0;
    while ( q < nInfus )
    {
      // If this infusion is active, then add in its contribution.
      if ( infusOnBreakIndex[q] <= kCurr && infusOffBreakIndex[q] > kCurr )
      {
        int p = compIndex( infusComp[q] );

        // Add the rate for this infusion, multiplied by its
        // bioavailability fraction, to the derivative of its
        // compartment's amount with respect to time.
        compAmount_tOut[p] += compBioavailFrac[p] * infusRate[q];
      }

      q++;
    }


    //----------------------------------------------------------
    // Add in the zero-order bolus doses' derivative contributions.
    //----------------------------------------------------------

    // Find any zero-order bolus doses that are active for the current
    // break time interval.
    q = 0;
    while ( q < nZeroOrderBolus )
    {
      // If this zero-order bolus is active, then add in its contribution.
      if ( zeroOrderBolusOnBreakIndex[q] <= kCurr && zeroOrderBolusOffBreakIndex[q] > kCurr )
      {
        int p = compIndex( zeroOrderBolusComp[q] );

        // Add the rate for this zero-order bolus, multiplied by its
        // bioavailability fraction, to the derivative of its
        // compartment's amount with respect to time.
        compAmount_tOut[p] += compBioavailFrac[p] * zeroOrderBolusRate[q];
      }

      q++;
    }


    //----------------------------------------------------------
    // Set the derivatives equal to zero for compartments that are off.
    //----------------------------------------------------------

    // If any compartments are currently turned off, then zero the
    // derivatives with respect to time of their amounts.
    int p;
    for ( p = 0; p < nComp - 1; p++ )
    {
      if ( compIsOff[p] )
      {
        compAmount_tOut[p] = 0.0;
      }

      // Check that the amounts in compartments that are off are equal
      // to zero.
      assert( !compIsOff[p] || compAmount[p] == 0.0 );
    }


    //----------------------------------------------------------
    // Calculate the derivative of the output compartment.
    //----------------------------------------------------------

    int m = compIndex( nComp );
  
    // Zero the derivative of the output compartment. 
    compAmount_tOut[m] = 0.0;
  
    // The output compartment contains the total amount eliminated
    // from the system from the time tOn when the output compartment
    // was turned on.  Its derivative with respect to time is
    //
    //      d                    ---     d          
    //     ---  A     (t)  =     \    - ---  A (t)  ,
    //      dt   nComp           /       dt   p        
    //                           ---                   
    //                        p < nComp                  
    //
    // where nComp is the number for the output compartment.
    //
    // If the output compartment is turned on, then set its derivative
    // equal to minus the sum of the derivatives for all of the other
    // compartments.
    if ( !compIsOff[m] )
    {
      for ( p = 0; p < nComp - 1; p++ )
      {
        // Note that the output compartment derivative is the negative
        // value of the sum of the other compartments' derivatives.
        compAmount_tOut[m] -= compAmount_tOut[p];
      }
  
      // Set the fraction of the infusion dose derivative that
      // actually makes it to the output compartment.
      compAmount_tOut[m] *= fo;
    }

  }


  //------------------------------------------------------------
  // Functions related to evaluating the compartment amounts.
  //------------------------------------------------------------

  /***********************************************************************
   *
   * Function: evalAllAmounts
   *
   *//**
   * Evaluates the amounts in all of the compartments for all of the
   * ODE solution times for the i-th individual at the given values
   * for the independent variables.
   */
  /***********************************************************************/

private:
  void evalAllAmounts(
    int thetaOffset,
    int thetaLen,
    int etaOffset,
    int etaLen,
    int i,
    const std::vector<Value>& indepVar )
  {
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------

    using namespace std;


    //----------------------------------------------------------
    // Prepare the individual's information.
    //----------------------------------------------------------

    // Set the current individual index.
    iCurr = i;

    // Get the individual's experiment design information.
    getExpDesign(
      thetaOffset,
      thetaLen,
      etaOffset,
      etaLen,
      iCurr,
      indepVar );


    //----------------------------------------------------------
    // Set local copies of variables needed by the function Ode().
    //----------------------------------------------------------

    thetaOffsetForOde = thetaOffset;
    thetaLenForOde    = thetaLen;
    etaOffsetForOde   = etaOffset;
    etaLenForOde      = etaLen;

    pIndepVarForOde   = &indepVar;


    //----------------------------------------------------------
    // Calculate all of the amounts for all of the compartments.
    //----------------------------------------------------------

    // Set the Eval class for OdeBreak to be this class so that the
    // functions Break() and Ode() will have access to this class's
    // functions and members.
    OdePredBase& odeBreakEvalClass = *this;

    // Since only the relative tolerance is currently provided to this
    // class, set the absolute tolerances for integration of the
    // ordinary differential equations equal to zero.
    std::vector<Value> tolAbs( nComp, 0.0 );

    // This message will be used if an error occurs.
    string message = "during the evaluation of the compartment amounts for \nall of the requested times for the " +
      intToOrdinalString( iCurr, ZERO_IS_FIRST_INT ) +
      " individual.";

    // Calculate the amounts in all of the compartments at all of the
    // ODE solution times by solving the differential equations from
    // the DES block numerically.
    try
    { std::string method = "Runge45";
      OdeBreak(
        odeBreakEvalClass,
        compAmountAllOdeSoln, 
        method,
        breakTime,
        odeSolnTime,
	odeSolnLeft,
        tolAbs,
        tolRel
      );
    }
    catch( SpkException& e )
    {
      throw e.push(
        SpkError::SPK_UNKNOWN_ERR, 
        ( "An error occurred " + message ).c_str(),
        __LINE__,
        __FILE__ );
    }
    catch( const std::exception& stde )
    {
      throw SpkException(
        stde,
        ( "A standard exception was thrown " + message ).c_str(),
        __LINE__, 
        __FILE__ );
    }  
    catch( ... )
    {
      throw SpkException(
        SpkError::SPK_UNKNOWN_ERR, 
        ( "An unknown exception was thrown " + message ).c_str(),
        __LINE__, 
        __FILE__ );
    }

  }


  /***********************************************************************
   *
   * Function: evalOdePred
   *
   *//**
   * Evaluates ODE-based equivalents of the Pred block expressions for
   * the i-th individual's j-th data record at the given values for
   * the independent variables.'
   *
   * Returns true if this is an observation event.
   */
  /***********************************************************************/

protected:
  bool evalOdePred( int thetaOffset, int thetaLen,
                    int etaOffset,   int etaLen,
                    int epsOffset,   int epsLen,
                    int fOffset,     int fLen,
                    int yOffset,     int yLen,
                    int i,
                    int j,
                    const std::vector<Value>& indepVar,
                    std::vector<Value>& depVar )
  {
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------

    using namespace std;


    //----------------------------------------------------------
    // Do preparations related to new individuals.
    //----------------------------------------------------------

    // If this is the first data record for this individual, then
    // calculate all of their compartment amounts.
    if ( j == 0 )
    {
      // Perform initializations required for new values of i before
      // calls to evalPk(), evalDes(), and evalError() will work.
      initUserEnv(
        thetaOffset,
        thetaLen,
        etaOffset,
        etaLen,
        epsOffset,
        epsLen,
        fOffset,
        fLen,
        yOffset,
        yLen,
        i,
        j,
        indepVar,
        depVar);

      // Calculate the amounts in all of the compartments for all of
      // the ODE solution times for this individual.
      evalAllAmounts(
        thetaOffset,
        thetaLen,
        etaOffset,
        etaLen,
        i,
        indepVar );
    }


    //----------------------------------------------------------
    // Set the calculated values for the dependent variables.
    //----------------------------------------------------------

    bool isObsEvent = false;

    // Get the data items for the current data record.
    readDataRecord( i, j );

    // If this is an observation event or a nonobservation prediction
    // evaluation event, then calculate the values for F and Y.
    if ( evid == OBSERV_EVENT || 
       ( evid == OTHER_TYPE_EVENT && pcmt >= 0 ) )
    {
      //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      // [Revisit - Unset Indices are Dangerous - Mitch ]
      //
      // Having unset indices for non ODE solution events and non
      // observation events seems dangerous since small subscripting
      // errors could easily cause the p to access catastropic data.
      //
      // Consider a different approach, e.g. only storing indices for
      // the events that are ODE solution events and having a second
      // array that shows where the index a given ODE solution record
      // is located in the first vector (like the optimizer parameter
      // array for elements that are not constrained.)
      //
      //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      // Get the ODE solution index for this data record.
      int s = dataRecOdeSolnIndex[ j ];

      // Set the current amounts in all of the compartments equal to
      // their amounts at the time for this data record.
      int p;
      for ( p = 0; p < nComp; p++ )
      {
        compAmount[p] = compAmountAllOdeSoln[p + s * nComp];
      }

      // Get the compartment index for this ODE solution.
      p = compIndex( odeSolnComp[ s ] );

      // Set the scaled amount in the observation compartment.
      f_i_j = compAmountAllOdeSoln[p + s * nComp] /
        compScaleParam[p];

      // Evaluate the intra-individual error model.
      evalError( 
        thetaOffset,
        thetaLen,
        etaOffset,
        etaLen,
        epsOffset,
        epsLen,
        i,
        j,
        indepVar );

      // If this is an observation event, then set the calculated
      // values for f_i_j and y_i_j in the vector of dependent variables.
      if ( evid == OBSERV_EVENT )
      {
        // Get the observation index for this data record.
        int m = dataRecObservIndex[ j ];

        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        // [Revisit - Take out Redundant y Values in Dependent Variables - Mitch]
        // In the near future take out the redundant y values in the
        // vector of dependent variables, i.e., set
        //
        //     w( z )  =  y( theta, eta, eps )  .
        //
        //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        //
        // Set the dependent variables,
	//
        //                 -                      -
        //                |  y( theta, eta, eps )  |
        //     w( z )  =  |                        |  .
        //                |  y( theta, eta, eps )  |
        //                 -                      -
	//
        depVar[ fOffset + m ] = y_i_j;
        depVar[ yOffset + m ] = y_i_j;

        isObsEvent = true;
      }
    }


    //----------------------------------------------------------
    // Finish up.
    //----------------------------------------------------------

    // Save information for the environment for subclasses of this
    // base class that are required for the current values of i and j.
    saveUserEnv(
      thetaOffset,
      thetaLen,
      etaOffset,
      etaLen,
      epsOffset,
      epsLen,
      fOffset,
      fLen,
      yOffset,
      yLen,
      i,
      j,
      indepVar,
      depVar);

    // Return a value that indicates if this is an observation event
    // or not.
    if ( isObsEvent )
    {
      return true;
    }
    else
    {
      return false;
    }

  }


  //------------------------------------------------------------
  // Destructors.
  //------------------------------------------------------------

public:
  virtual ~OdePredBase(){}


  //------------------------------------------------------------
  // Constructors and operators that should not be called.
  //------------------------------------------------------------

private:
  OdePredBase();
  OdePredBase( const OdePredBase& );
  OdePredBase & operator=( const OdePredBase& );
};
#endif
