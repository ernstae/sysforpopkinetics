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
#include <spk/WarningsManager.h>

// CppAD header files.
#include <CppAD/CppAD.h>

// Standard library header files.
#include <algorithm>
#include <map>
#include <sstream>
#include <vector>


/*------------------------------------------------------------------------
 * Local functions
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  /***********************************************************************
   *
   * Function: isEqualtoLastElem
   *
   * Returns true if value is equal to the last element of valueVector.
   *
  /***********************************************************************/

  template<class Value>
  bool isEqualtoLastElem(
    const Value&              value,
    const std::vector<Value>& valueVector )
  {
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------

    using namespace std;


    //----------------------------------------------------------
    // Handle the case where there are no values in the vector.
    //----------------------------------------------------------

    // If no values have been added to the vector, then the input
    // value cannot be equal to the last one.
    if ( valueVector.size() == 0 )
    {
      return false;
    }


    //----------------------------------------------------------
    // Handle the case where there are values in the vector.
    //----------------------------------------------------------

    // Return true if the input value is equal to the last value.
    return ( valueVector.back() == value );
  }

} // [End: unnamed namespace]


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
    bool                           isPkBlockAFuncOfTIn,
    int                            nCompIn,
    int                            defaultDoseCompIn,
    int                            defaultObservCompIn,
    const SPK_VA::valarray<bool>&  compInitialOffIn,
    const SPK_VA::valarray<bool>&  compNoOffIn,
    const SPK_VA::valarray<bool>&  compNoDoseIn,
    Value                          tolRelIn )
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
    compIsAmountInf           ( nCompIn ),
    compIsAmountNan           ( nCompIn ),
    compIsAmount_tInf         ( nCompIn ),
    compIsAmount_tNan         ( nCompIn ),
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
      compAmount       [p] = Value( 0 );
      compAmount_t     [p] = Value( 0 );

      compInfusRate    [p] = Value( 0 );
      compInfusDurat   [p] = Value( 0 );
      compAbsorpLagTime[p] = Value( 0 );

      compScaleParam   [p] = Value( 1 );
      compBioavailFrac [p] = Value( 1 );

      compIsAmountInf  [p] = false;
      compIsAmountNan  [p] = false;
      compIsAmount_tInf[p] = false;
      compIsAmount_tNan[p] = false;

      compInitialOff   [p] = compInitialOffIn[p];
      compNoOff        [p] = compNoOffIn     [p];
      compNoDose       [p] = compNoDoseIn    [p];

      compIsOff        [p] = compInitialOff  [p];
    }

    // Set initial values for these parameters.
    tscale  = Value( 1 );
    fo = Value( 1 );

    // Set initial values for these data items.
    mdv  = 0;
    evid = 0;
    cmt  = 0;
    amt  = Value( 0 );
    rate = Value( 0 );
    time = Value( 0 );

    // Set an invalid initial value for this data item that will be
    // reset if it appears in the data record.
    pcmt = -9999;

  }


  //------------------------------------------------------------
  // Virtual functions that can be specialized.
  //------------------------------------------------------------

  /***********************************************************************
   *
   * Function: initPredSubAD
   *
   *//**
   * Initialize values for a CppAD based subclass of this abstract
   * base class by copying the current values from this object to the
   * CppAD version that is pointed to by pPredBaseADIn.
   *
   * This function assumes that the object pointed to by pPredBaseADIn
   * is a subclass of OdePredBase.
   */
  /***********************************************************************/

public:
  virtual void initPredSubAD( PredBase< CppAD::AD<Value> >* pPredBaseADIn )
  {
    // Convert the input PredBase pointer to be of the same type as
    // this class.
    OdePredBase< CppAD::AD<Value> >* pOdePredBaseAD =
      dynamic_cast< OdePredBase< CppAD::AD<Value> >* >( pPredBaseADIn );

    int p;
    int s;

    // Make enough room for all of the ODE solutions.
    pOdePredBaseAD->resizeCompAmountAllOdeSoln( nComp * nOdeSoln );

    // Copy the current values from this object to the CppAD version
    // that is pointed to by pPredBaseADIn.
    for ( p = 0; p < nComp; p++ )
    {
      pOdePredBaseAD->setCompInfusRate    ( p, compInfusRate    [p] );
      pOdePredBaseAD->setCompInfusDurat   ( p, compInfusDurat   [p] );
      pOdePredBaseAD->setCompAbsorpLagTime( p, compAbsorpLagTime[p] );
      pOdePredBaseAD->setCompScaleParam   ( p, compScaleParam   [p] );
      pOdePredBaseAD->setCompBioavailFrac ( p, compBioavailFrac [p] );

      for ( s = 0; s < nOdeSoln; s++ )
      {
        pOdePredBaseAD->setCompAmountAllOdeSoln( p, s, compAmountAllOdeSoln[p + s * nComp] );
      }
    }

  }


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


  /***********************************************************************
   *
   * Function: evalAllF
   *
   *//**
   * This function is a specialization of the evalAllF() function for
   * the case where the Pred block expressions are implemented by
   * ODE-based equivalents.
   *
   * <code>evalAllF</code> evaluates the PK, DES, and ERROR blocks'
   * expressions for all of the i-th individual's data records at the
   * given values for the independent variables.
   *
   * If the Dependent Variable (DV) data item for the j-th data record is
   * not missing, i.e., its Missing Dependent Variable (MDV) data item
   * is equal to zero, then the value for fOut will be set equal to 
   * the calculated value for y,
   * \f[
   *   fOut_{i(j)} = y_{i(j)}( f_{i(j)}(theta, eta), theta, eta, eps)  .
   * \f]
   *
   * The variable f is the model for the expected value
   * for the DV data item, and the variable y is the full
   * statistical model for the DV that includes the noise in the data.
   *
   * This function should set the value for DV, and it should also set
   * the values for all of the other data items that appear in the
   * $INPUT record.
   *
   * @param thetaOffset     The index to the head of THETA vector within indepVar.
   * @param thetaLen        The length of THETA vector.
   *                        The vector elements are assumed to be placed
   *                        from indepVar[thetaOffset] 
   *                        to indepVar[thetaOffset + thetaLen].
   * @param etaOffset       The index to the head of ETA vector within indepVar.
   * @param etaLen          The length of ETA vector.  
   *                        The vector elements are assumed to be placed
   *                        from indepVar[etaOffset] 
   *                        to indepVar[etaOffset + etaLen].
   * @param epsOffset       The index to the head of EPS vector within indepVar.
   * @param epsLen          The length of EPS vector.
   *                        The vector elements are assumed to be placed
   *                        from indepVar[thetaOffset] 
   *                        to indepVar[thetaOffset + thetaLen].
   * @param fLen            The total length of y vector.
   * @param i               The index to the individual of interest 
   *                        within the population (0 indicates the first
   *                        individual.)
   *                        first data record.)
   * @param indepVar        The vector containing independent variables:
   *                        THETA, ETA and EPS.
   * @param fOut            (output) The vector whose m-th element will be 
   *                        replaced with the calculated value for
   *                        y if the DV data item for this data
   *                        record is not missing, 
   *                        \f[
   *                          \mbox{fOut} = y(f(theta, eta), theta, eta, eps)  .
   *                        \f]
   *                        The number m is the position of this DV value in
   *                        the list of DV values for this individual that
   *                        are not missing.  Note that m is a number that
   *                        must be determined by the implementation of
   *                        this function, i.e., it is not an input to this 
   *                        function.
   */
  /***********************************************************************/

  virtual void evalAllF( int thetaOffset, int thetaLen,
                         int etaOffset,   int etaLen,
                         int epsOffset,   int epsLen,
                         int fLen,
                         int i,
                         const std::vector<Value>& indepVar,
                         std::vector<Value>& fOut )
  {
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------

    using namespace std;

    // Set the number of records for this individual.
    int nRecord = this->getNRecords( i );

    // Set the number of observation records for this individual.
    int nObserv = this->getNObservs( i );


    //----------------------------------------------------------
    // Check the inputs.
    //----------------------------------------------------------

    // See if there was the correct number of output f values.
    if ( fOut.size() != fLen )
    {
      string message = "The number of output f values does not match the expected number \nfor the " + 
        intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual.";

      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        message.c_str(),
        __LINE__, 
        __FILE__ );
    }


    //----------------------------------------------------------
    // Calculate all of this individual's compartments' amounts.
    //----------------------------------------------------------

    int j;

    // Set quantities related to the vector of dependent variables
    // for the current individual,
    //
    //                 -                      -
    //     w( z )  =  |  f( theta, eta, eps )  |  .
    //                 -                      -
    const int fOffset = 0;
    const int yOffset = 0;
    int yLen = 0;

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
      fOut );

    // Calculate the amounts in all of the compartments for all of
    // the ODE solution times for this individual.
    evalAllAmounts(
      thetaOffset,
      thetaLen,
      etaOffset,
      etaLen,
      i,
      indepVar );


    //----------------------------------------------------------
    // Set all of the f values for this individual.
    //----------------------------------------------------------

    std::string taskMessage;

    // This will be equal to the number of f values that were set.
    int nFValueSet = 0;

    // Evaluate the expressions from the PK, DES, and ERROR blocks for
    // all of the data records for the current individual.
    for ( j = 0; j < nRecord; j++ )
    {
      taskMessage = "during the evaluation of the mean for the \n" + 
        intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual's " +
        intToOrdinalString( j, ZERO_IS_FIRST_INT ) + " data record.";

      // Evaluate the PK, DES, and ERROR block expressions for this
      // data record.  The calculated value will be set if this data
      // record is an observation record.
      //
      try
      {
        //----------------------------------------------------------
        // Prepare to set the calculated values for the dependent variables.
        //----------------------------------------------------------

        // Get the data items for the current data record.
        readDataRecord( i, j );

        // Set the continuous time variable T equal to the TIME value
        // for this data record.
        setT( time );

        // Set current values for any variables defined in the PK block.
        evalPk(
          thetaOffset,
          thetaLen,
          etaOffset,
          etaLen,
          i,
          j,
          indepVar );
  
        // Set current values for any variables defined in the DES block.
        evalDes(
          thetaOffset,
          thetaLen,
          iCurr,
          jCurr,
          indepVar );

        // If this is data record has an ODE solution, then calculate the
        // values.
        if ( dataRecHasOdeSoln[j] )
        {
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

          // Set f equal to the scaled amount in the observation
          // compartment,
          //
          //                  compAmount ( theta, eta )
          //                            p   
          //     f_i_j  =  -------------------------------  .
          //                compScaleParam ( theta, eta )
          //                              p
          //
          f_i_j = compAmountAllOdeSoln[p + s * nComp] /
            compScaleParam[p];

          // Evaluate the intra-individual error model using the just
          // set value for f_i_j so that y_i_j may be a function of
          // the scaled amounts in the compartments,
          //
          //     y_i_j  =  y    ( f    ( theta, eta ), theta, eta, eps )  .
          //                i(j)   i(j)
          //
          //                        compAmount ( theta, eta )
          //                                  p   
          //            =  y    ( ------------------------------- , theta, eta, eps )  .
          //                i(j)   compScaleParam ( theta, eta )
          //                                    p
          //
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

          // Set f_i_j equal to the predicted value for the data,
          //
          //     f      =  y    ( theta, eta, eps )  .
          //      i(j)      i(j)
          //
          f_i_j = y_i_j;

          // If this is an observation event, then set the calculated
          // value in the vector of output variables.
          if ( evid == OBSERV_EVENT )
          {
            // Get the observation index for this data record.
            int m = dataRecObservIndex[j];

            // Set this element.
            fOut[m] = f_i_j;

            // Increment the counter.
            nFValueSet++;
          }
        }
      }
      catch( SpkException& e )
      {
        throw e.push(
          SpkError::SPK_MODEL_DATA_MEAN_ERR,
          ( "An error occurred " + taskMessage ).c_str(),
          __LINE__,
          __FILE__ );
      }
      catch( const std::exception& stde )
      {
        throw SpkException(
          stde,
          ( "A standard exception was thrown " + taskMessage ).c_str(),
          __LINE__, 
          __FILE__ );
      }  
      catch( ... )
      {
        throw SpkException(
          SpkError::SPK_UNKNOWN_ERR, 
          ( "An unknown exception was thrown " + taskMessage ).c_str(),
          __LINE__, 
          __FILE__ );
      }

      // If the current record is an observation record, then check the
      // calculated value to see if it is valid.
      if ( evid == OBSERV_EVENT )
      {
        // Make sure that the value is not a NaN.
        if ( isNotANumber( f_i_j ) )
        {
          string message = "The mean of the " +
            intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual's " +
            intToOrdinalString( j, ZERO_IS_FIRST_INT ) + " data record was Not a Number (NaN).";
    
          throw SpkException(
            SpkError::SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR,
            message.c_str(),
            __LINE__,
            __FILE__ );
        }
    
        // Make sure that the value is finite.
        //
        // Note that this check is done after the NaN check because
        // NaN's are unnormalized.
        if ( isUnnormNumber( f_i_j ) )
        {
          string message = "The mean of the " +
            intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual's " +
            intToOrdinalString( j, ZERO_IS_FIRST_INT ) + " data record was infinite.";
    
          throw SpkException(
            SpkError::SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR,
            message.c_str(),
            __LINE__,
            __FILE__ );
        }
      }


      //----------------------------------------------------------
      // Finish working with this data record.
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
        fOut );

    }


    //----------------------------------------------------------
    // Finish up.
    //----------------------------------------------------------
    
    // See if there was the correct number of observation records.
    if ( nFValueSet != nObserv )
    {
      string message = "The number of data records that are observation records does not match the expected \nnumber of observation records for the " + 
        intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual.";

      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        message.c_str(),
        __LINE__, 
        __FILE__ );
    }

  }


  /***********************************************************************
   *
   * Function: evalAllY
   *
   *//**
   * This function is a specialization of the evalAllY() function for
   * the case where the Pred block expressions are implemented by
   * ODE-based equivalents.
   *
   * <code>evalAllY</code> evaluates the PK, DES, and ERROR blocks'
   * expressions for all of the i-th individual's data records at the
   * given values for the independent variables.
   *
   * If the Dependent Variable (DV) data item for the j-th data record is
   * not missing, i.e., its Missing Dependent Variable (MDV) data item
   * is equal to zero, then the value for yOut will be set equal to 
   * the calculated value for y,
   * \f[
   *   yOut_{i(j)} = y_{i(j)}( fIn_{i(j)}(theta, eta), theta, eta, eps)  .
   * \f]
   *
   * The variable f is the model for the expected value
   * for the DV data item, and the variable y is the full
   * statistical model for the DV that includes the noise in the data.
   *
   * This function should set the value for DV, and it should also set
   * the values for all of the other data items that appear in the
   * $INPUT record.
   *
   * @param thetaOffset     The index to the head of THETA vector within indepVar.
   * @param thetaLen        The length of THETA vector.
   *                        The vector elements are assumed to be placed
   *                        from indepVar[thetaOffset] 
   *                        to indepVar[thetaOffset + thetaLen].
   * @param etaOffset       The index to the head of ETA vector within indepVar.
   * @param etaLen          The length of ETA vector.  
   *                        The vector elements are assumed to be placed
   *                        from indepVar[etaOffset] 
   *                        to indepVar[etaOffset + etaLen].
   * @param epsOffset       The index to the head of EPS vector within indepVar.
   * @param epsLen          The length of EPS vector.
   *                        The vector elements are assumed to be placed
   *                        from indepVar[thetaOffset] 
   *                        to indepVar[thetaOffset + thetaLen].
   * @param yLen            The total length of y vector.
   * @param i               The index to the individual of interest 
   *                        within the population (0 indicates the first
   *                        individual.)
   * @param indepVar        The vector containing independent variables:
   *                        THETA, ETA and EPS.
   * @param fIn             The vector containing the variable f, 
   *                        which is the model for the expected value for the 
   *                        DV data item,
   *                        \f[
   *                          \mbox{fIn} = f(theta, eta)  .
   *                        \f]
   *                        It must have the same length as yOut.
   *                        This function assumes that fIn has been evaluated 
   *                        at the same values for theta and eta as are input 
   *                        to this function.
   * @param yOut            (output) The vector whose m-th element will be 
   *                        replaced with the calculated value for
   *                        y if the DV data item for this data
   *                        record is not missing, 
   *                        \f[
   *                          \mbox{yOut} = y(f(theta, eta), theta, eta, eps)  .
   *                        \f]
   *                        The number m is the position of this DV value in
   *                        the list of DV values for this individual that
   *                        are not missing.  Note that m is a number that
   *                        must be determined by the implementation of
   *                        this function, i.e., it is not an input to this 
   *                        function.
   */
  /***********************************************************************/

  virtual void evalAllY( int thetaOffset, int thetaLen,
                         int etaOffset,   int etaLen,
                         int epsOffset,   int epsLen,
                         int yLen,
                         int i,
                         const std::vector<Value>& indepVar,
                         const std::vector<Value>& fIn,
                         std::vector<Value>& yOut )
  {
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------

    using namespace std;

    // Set the number of records for this individual.
    int nRecord = this->getNRecords( i );

    // Set the number of observation records for this individual.
    int nObserv = this->getNObservs( i );


    //----------------------------------------------------------
    // Check the inputs.
    //----------------------------------------------------------

    // See if there was the correct number of input f values.
    if ( fIn.size() != yLen )
    {
      string message = "The number of input f values does not match the expected number \nfor the " + 
        intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual.";

      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        message.c_str(),
        __LINE__, 
        __FILE__ );
    }

    // See if there was the correct number of output y values.
    if ( yOut.size() != yLen )
    {
      string message = "The number of output y values does not match the expected number \nfor the " + 
        intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual.";

      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        message.c_str(),
        __LINE__, 
        __FILE__ );
    }


    //----------------------------------------------------------
    // Prepare to set all of the y values for this individual.
    //----------------------------------------------------------

    int j;

    // Set quantities related to the vector of dependent variables
    // for the current individual,
    //
    //                 -                      -
    //     w( z )  =  |  y( theta, eta, eps )  |  .
    //                 -                      -
    const int fOffset = 0;
    const int yOffset = 0;
    int fLen = 0;

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
      yOut );

    // Get the individual's experiment design information.
    getExpDesign(
      thetaOffset,
      thetaLen,
      etaOffset,
      etaLen,
      i,
      indepVar );


    //----------------------------------------------------------
    // Set all of the y values for this individual.
    //----------------------------------------------------------

    std::string taskMessage;

    // This will be equal to the number of y values that were set.
    int nYValueSet = 0;

    // Evaluate the expressions from the PK, DES, and ERROR blocks for
    // all of the data records for the current individual.
    for ( j = 0; j < nRecord; j++ )
    {
      taskMessage = "during the evaluation of the intra-individual error for the \n" + 
        intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual's " +
        intToOrdinalString( j, ZERO_IS_FIRST_INT ) + " data record.";

      // Evaluate the PK, DES, and ERROR block expressions for this
      // data record.  The calculated value will be set if this data
      // record is an observation record.
      //
      try
      {
        //----------------------------------------------------------
        // Prepare to set the calculated values for the dependent variables.
        //----------------------------------------------------------

        // Get the data items for the current data record.
        readDataRecord( i, j );

        // Set the continuous time variable T equal to the TIME value
        // for this data record.
        setT( time );

        // Set current values for any variables defined in the PK block.
        evalPk(
          thetaOffset,
          thetaLen,
          etaOffset,
          etaLen,
          i,
          j,
          indepVar );
  
        // Set current values for any variables defined in the DES block.
        evalDes(
          thetaOffset,
          thetaLen,
          iCurr,
          jCurr,
          indepVar );

        // If this is data record has an ODE solution, then calculate the
        // values.
        if ( dataRecHasOdeSoln[j] )
        {
          // Get the ODE solution index for this data record.
          int s = dataRecOdeSolnIndex[ j ];

          // Set the current amounts in all of the compartments equal to
          // their amounts at the time for this data record.
          int p;
          for ( p = 0; p < nComp; p++ )
          {
            compAmount[p] = compAmountAllOdeSoln[p + s * nComp];
          }

          // If this is an observation event, then set the calculated
          // value in the vector of output variables.
          if ( evid == OBSERV_EVENT )
          {
            // Get the observation index for this data record.
            int m = dataRecObservIndex[j];
    
            // Get the compartment index for this ODE solution.
            p = compIndex( odeSolnComp[ s ] );
    
            // Set f equal to the scaled amount in the observation
            // compartment,
            //
            //                  compAmount ( theta, eta )
            //                            p   
            //     f_i_j  =  -------------------------------  .
            //                compScaleParam ( theta, eta )
            //                              p
            //
            f_i_j = compAmountAllOdeSoln[p + s * nComp] /
              compScaleParam[p];

            // Evaluate the intra-individual error model using the just
            // set value for f_i_j so that y_i_j may be a function of
            // fIn_i_m,
            //
            //     y_i_j  =  y    ( fIn    (theta, eta), theta, eta, eps )  .
            //                i(j)     i(m)
            //
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
    
            // Set f_i_j equal to the predicted value for the data,
            //
            //     f      =  y    ( fIn    (theta, eta), theta, eta, eps )  .
            //      i(j)      i(j)     i(m)
            //           
            f_i_j = y_i_j;
    
            // Set this element.
            yOut[m] = y_i_j;
    
            // Increment the counter.
            nYValueSet++;
          }
        }
      }
      catch( SpkException& e )
      {
        throw e.push(
          SpkError::SPK_MODEL_DATA_MEAN_ERR,
          ( "An error occurred " + taskMessage ).c_str(),
          __LINE__,
          __FILE__ );
      }
      catch( const std::exception& stde )
      {
        throw SpkException(
          stde,
          ( "A standard exception was thrown " + taskMessage ).c_str(),
          __LINE__, 
          __FILE__ );
      }  
      catch( ... )
      {
        throw SpkException(
          SpkError::SPK_UNKNOWN_ERR, 
          ( "An unknown exception was thrown " + taskMessage ).c_str(),
          __LINE__, 
          __FILE__ );
      }

      // If the current record is an observation record, then check the
      // calculated value to see if it is valid.
      if ( evid == OBSERV_EVENT )
      {
        // Make sure that the value is not a NaN.
        if ( isNotANumber( y_i_j ) )
        {
          string message = "The intra-individual error for the " +
            intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual's " +
            intToOrdinalString( j, ZERO_IS_FIRST_INT ) + " data record \nwas Not a Number (NaN).";
    
          throw SpkException(
            SpkError::SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR,
            message.c_str(),
            __LINE__,
            __FILE__ );
        }

        // Make sure that the value is finite.
        //
        // Note that this check is done after the NaN check because
        // NaN's are unnormalized.
        if ( isUnnormNumber( y_i_j ) )
        {
          string message = "The intra-individual error for the " +
            intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual's " +
            intToOrdinalString( j, ZERO_IS_FIRST_INT ) + " data record \nwas infinite.";
    
          throw SpkException(
            SpkError::SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR,
            message.c_str(),
            __LINE__,
            __FILE__ );
        }
      }


      //----------------------------------------------------------
      // Finish working with this data record.
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
        yOut );

    }


    //----------------------------------------------------------
    // Finish up.
    //----------------------------------------------------------
    
    // See if there was the correct number of observation records.
    if ( nYValueSet != nObserv )
    {
      string message = "The number of data records that are observation records does not match the expected \nnumber of observation records for the " + 
        intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual.";

      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        message.c_str(),
        __LINE__, 
        __FILE__ );
    }

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
   * Note that this function is not used in this class and so does
   * nothing.
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
    return false;
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

  int getDefaultDoseComp()   const { return defaultDoseComp; }    ///< Gets the default dose compartment.
  int getDefaultObservComp() const { return defaultObservComp; }  ///< Gets the default observation compartment.

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

  ///< Gets (p,s)-th value for compAmountAllOdeSoln.
  void getCompAmountAllOdeSoln( int p, int s, Value& compAmountAllOdeSoln_p_sOut ) const
  { compAmountAllOdeSoln_p_sOut = compAmountAllOdeSoln[p + s * nComp]; }

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

public:
  ///< Sets the p-th value for compAmount.
  void setCompAmount( int p, const Value& compAmountIn )
  { compAmount[p] = compAmountIn; }

  ///< Sets the (p,s)-th value for compAmountAllOdeSoln.
  void setCompAmountAllOdeSoln( int p, int s, const Value& compAmountAllOdeSolnIn )
  { compAmountAllOdeSoln[p + s * nComp] = compAmountAllOdeSolnIn; }

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

  // Resizes the vector of amounts in each compartment for all of the
  // ODE solution times.
  void resizeCompAmountAllOdeSoln( int nElem )
  { compAmountAllOdeSoln.resize( nElem ); }

private:
  const int nComp;                         ///< Number of compartments (including the ouput compartment).
  const int defaultDoseComp;               ///< Default dose compartment.
  const int defaultObservComp;             ///< Default observation compartment.

  int nCompToSolve;                        ///< Number of compartments to calculate ODE solutions for.
  bool isOutputCompUsed;                   ///< Indicates if output compartment is used and an ODE solution should be calculated for it.

  std::vector<Value> compAmount;           ///< Current amount in each compartment.
  std::vector<Value> compAmount_t;         ///< Current time derivative of the amount in each compartment.
  std::vector<Value> compInfusRate;        ///< Current infusion rate for each compartment.
  std::vector<Value> compInfusDurat;       ///< Current infusion duration for each compartment.
  std::vector<Value> compAbsorpLagTime;    ///< Current absorption lag time for each compartment.
  std::vector<Value> compScaleParam;       ///< Current scale parameter for each compartment.
  std::vector<Value> compBioavailFrac;     ///< Current bio-availability for each compartment.

  Value tNanOrInf;                         ///< Time for which an amount or time derivative is Not a Number (NaN) or infinite.

  std::vector<bool> compIsAmountInf;       ///< Indicates if current compartment amount is infinite.
  std::vector<bool> compIsAmountNan;       ///< Indicates if current compartment amount is Not a Number (NaN).
  std::vector<bool> compIsAmount_tInf;     ///< Indicates if current compartment time derivative is infinite.
  std::vector<bool> compIsAmount_tNan;     ///< Indicates if current compartment time derivative is Not a Number (NaN). 

  SPK_VA::valarray<bool> compInitialOff;   ///< Indicates which compartments are initially off.
  SPK_VA::valarray<bool> compNoOff;        ///< Indicates which compartments may not be turned on or off.
  SPK_VA::valarray<bool> compNoDose;       ///< Indicates which compartments may not receive a dose.
  SPK_VA::valarray<bool> compIsOff;        ///< Indicates which compartments are currently off.

  std::vector<Value> compAmountAllOdeSoln;             ///< Amount in each compartment for all of the ODE solution times.
  std::vector<Value> compAmountAllOdeSolnNoOutputComp; ///< Amount in each compartment except the output for all of the ODE solution times.


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
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------

    using namespace std;


    //----------------------------------------------------------
    // Turn the compartment on.
    //----------------------------------------------------------

    // Get the index of the compartment to turn on.
    int pOn = compIndex( comp );

    // Set the initial amount in this compartment equal to zero.
    compAmount[pOn] = Value( 0 );

    // Only turn on the output compartment if it is being used.
    if ( comp == nComp && ! isOutputCompUsed )
    {
      string message = "A request was made to turn on the output compartment, which had been \nremoved from the ODE's for the " +
        intToOrdinalString( iCurr, ZERO_IS_FIRST_INT ) +
        " individual.";
    
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        message.c_str(),
        __LINE__, 
        __FILE__ );
    }

    // Set the initial derivative with respect to time of the amount
    // in the compartment.
    compAmount_t[pOn] = Value( 0 );

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
    //----------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------

    using namespace std;


    //----------------------------------------------------------
    // Turn the compartment off.
    //----------------------------------------------------------

    // Get the index of the compartment to turn off.
    int pOff = compIndex( comp );

    // Only turn off the output compartment if it is being used.
    if ( comp == nComp && !isOutputCompUsed )
    {
      string message = "A request was made to turn off the output compartment, which had been \nremoved from the ODE's for the " +
        intToOrdinalString( iCurr, ZERO_IS_FIRST_INT ) +
        " individual.";
    
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        message.c_str(),
        __LINE__, 
        __FILE__ );
    }

    // Zero the amount in the compartment and its derivative with
    // respect to time.
    compAmount  [pOff] = Value( 0 );
    compAmount_t[pOff] = Value( 0 );

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

  std::vector<int>   dataRecOdeSolnIndex;         ///< Indices for data records in the vector of ODE solutions.
  std::vector<int>   dataRecObservIndex;          ///< Indices for data records in the vector of observations.
  std::vector<bool>  dataRecHasOdeSoln;           ///< Indicates if data record has an ODE solution.

  int nOdeSoln;                                   ///< Number of ODE solutions to calculate.
  int nOdeSolnRemoved;                            ///< Number of duplicate ODE solution times removed.
  std::vector<int>   odeSolnComp;                 ///< ODE solution compartments.
  std::vector<int>   odeSolnBreakIndex;           ///< ODE solution break point indices.
  std::vector<Value> odeSolnTime;                 ///< ODE solution times.
  std::vector<int>   odeSolnOccasionIndex;        ///< ODE solution occasion indices.
  std::vector<bool>  odeSolnIsLeftCont;           ///< Indicates if ODE solution is left continuous.

  int nOccasion;                                  ///< Number of occasions.

  int nObserv;                                    ///< Number of observations.
  int nDosePred;                                  ///< Number of predictions at dose times.
  int nNonObservPred;                             ///< Number of nonobservation predictions.

  class BreakInfo                                 ///< Break point information.
  {
  public:
    BreakInfo()
    :
    time( Value( 0 ) ), isDataRec( false ), indDataRecIndex( 0 ), occasionIndex( 0 )
    {
    }
    BreakInfo( const Value& timeIn, bool isDataRecIn, int indDataRecIndexIn, int occasionIndexIn )
    :
    time( timeIn ), isDataRec( isDataRecIn ), indDataRecIndex( indDataRecIndexIn ), occasionIndex( occasionIndexIn )
    {
    }

    bool operator<( const BreakInfo& right ) const
    {
      if ( occasionIndex < right.occasionIndex )
      {
        return true;
      }
      else if ( occasionIndex > right.occasionIndex )
      {
        return false;
      }
      else
      {
        return time < right.time;
      }
    }

    Value time;                                   ///< Break point time.
    int   occasionIndex;                          ///< Break point occasion index.
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
    time( Value( 0 ) ), infusIndex( 0 ), occasionIndex( 0 )
    {
    }
    InfusOffInfo( const Value& timeIn, int infusIndexIn, int occasionIndexIn )
    :
    time( timeIn ), infusIndex( infusIndexIn ), occasionIndex( occasionIndexIn )
    {
    }

    bool operator<( const InfusOffInfo& right ) const
    {
      if ( occasionIndex < right.occasionIndex )
      {
        return true;
      }
      else if ( occasionIndex > right.occasionIndex )
      {
        return false;
      }
      else
      {
        return time < right.time;
      }
    }

    Value time;                                   ///< Regular infusion dose off time.
    int   occasionIndex;                          ///< Break point occasion index.
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
    time( Value( 0 ) ), zeroOrderBolusIndex( 0 ), occasionIndex( 0 )
    {
    }
    ZeroOrderBolusOffInfo( const Value& timeIn, int zeroOrderBolusIndexIn, int occasionIndexIn )
    :
    time( timeIn ), zeroOrderBolusIndex( zeroOrderBolusIndexIn ), occasionIndex( occasionIndexIn )
    {
    }

    bool operator<( const ZeroOrderBolusOffInfo& right ) const
    {
      if ( occasionIndex < right.occasionIndex )
      {
        return true;
      }
      else if ( occasionIndex > right.occasionIndex )
      {
        return false;
      }
      else
      {
        return time < right.time;
      }
    }

    Value time;                                   ///< Zero-order bolus dose off time.
    int   occasionIndex;                          ///< Break point occasion index.
    int   zeroOrderBolusIndex;                    ///< Zero-order bolus dose index.

  };
  std::vector<ZeroOrderBolusOffInfo> zeroOrderBolusOffPoint;  ///< Zero-order bolus dose off information.

  int nTurnOnOrOff;                               ///< Number compartments turned on or off events.

  std::vector<int>   turnOnOrOffComp;             ///< Compartments to be turned on or off.
  std::vector<Value> turnOnOrOffTime;             ///< Times to turn the compartments on or off.
  std::vector<int>   turnOnOrOffOccasionIndex;    ///< Occasion indices for times to turn compartments on or off.

  static bool issuedStartTimeWarning;


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
    nDataRec = this->getNRecords( i );

    // Set the sizes for the vectors of ODE solution indices,
    // observation indices, and the flags that indicate is the data
    // record has an ODE solution.
    dataRecOdeSolnIndex.resize( nDataRec );
    dataRecObservIndex .resize( nDataRec );
    dataRecHasOdeSoln  .resize( nDataRec );

    nOdeSolnRemoved = 0;
    nObserv         = 0;
    nDosePred       = 0;
    nNonObservPred  = 0;

    // There is always at least one occasion.  There can be multiple
    // occasions if there is a reset event record for this individual.
    nOccasion = 1;

    odeSolnComp         .resize( 0 );
    odeSolnTime         .resize( 0 );
    odeSolnIsLeftCont   .resize( 0 );

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

    Value timePrev;

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

      // Check for reset event records.
      if ( evid == RESET_EVENT )
      { 
        // Increment this counter because each reset event marks the
        // start of a new occasion.
        nOccasion++;
      }
      // Check that the time values do not decrease.
      else if ( j > 0 && time < timePrev )
      {
        ostringstream message;
      
        message << "The " << intToOrdinalString( j, ZERO_IS_FIRST_INT )
                << " data record for the "
                << intToOrdinalString( i, ZERO_IS_FIRST_INT )
                << " individual had a time value that was \nless than the time value for the previous data record.";
      
        throw SpkException(
          SpkError::SPK_USER_INPUT_ERR, 
          message.str().c_str(),
          __LINE__, 
          __FILE__ );
      }
  
      // Save the current time to check the next record.
      timePrev = time;

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

      // Initially assume that the data record does not have an ODE
      // solution.
      dataRecHasOdeSoln[j] = false;


      //--------------------------------------------------------
      // Do some miscellaneous sanity checks.
      //--------------------------------------------------------

      // Check that the compartment number data item is valid.
      if ( cmt > nComp )
      {
        ostringstream message;

        message << "The " << intToOrdinalString( j, ZERO_IS_FIRST_INT )
                << " data record for the "
                << intToOrdinalString( i, ZERO_IS_FIRST_INT )
                << " individual has a compartment number \nequal to "
                << cmt
                << ", which is greater than the maximum compartment number, "
                << nComp
                << ".";

        throw SpkException(
          SpkError::SPK_USER_INPUT_ERR, 
          message.str().c_str(),
          __LINE__, 
          __FILE__ );
      }

      // Check that the mdv and evid values are consistent.
      if ( mdv == 1 && evid == OBSERV_EVENT )
      {
        ostringstream message;

        message << "The " << intToOrdinalString( j, ZERO_IS_FIRST_INT )
                << " data record for the "
                << intToOrdinalString( i, ZERO_IS_FIRST_INT )
                            << " individual has contradictory \nMDV and EVID values.  The MDV value is 1, which indicates this \nis not an observation record.  The EVID value is 0, however, \nwhich indicates this is an observation record.";

        throw SpkException(
          SpkError::SPK_USER_INPUT_ERR, 
          message.str().c_str(),
          __LINE__, 
          __FILE__ );
      }


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
        //
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

        // Set the occasion index.
        odeSolnOccasionIndex.push_back( nOccasion - 1 );

        // The ODE solution should be left continuous unless there is
        // already an instantaneous bolus dose at the same time, which
        // means that the bolus data record came before this one.
        odeSolnIsLeftCont.push_back( !isEqualtoLastElem( time, bolusTime ) );

        // If this observation event is for the output compartment,
        // then see if it needs to be turned off after the
        // observation.
        if ( odeSolnComp.back() == nComp && odeSolnComp.back() < 0 )
        {
          // Set the compartment and time.
          turnOnOrOffComp.push_back( odeSolnComp.back() );
          turnOnOrOffTime.push_back( time );

          // Set the occasion index.
          turnOnOrOffOccasionIndex.push_back( nOccasion - 1 );
        }

        // Set this data record's index in the vector of
        // ODE solutions.
        dataRecOdeSolnIndex[j] = ( odeSolnTime.size() - 1 );
        dataRecHasOdeSoln  [j] = true;

        // Set this data record's index in the vector of observations.
        dataRecObservIndex[j] = nObserv;

        nObserv++;

        // Add a break point for this observation event.
        breakPoint.push_back( BreakInfo( odeSolnTime.back(), true, j, nOccasion - 1 ) );
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
        if ( amt > Value( 0 ) && rate == Value( 0 ) )
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
          breakPoint.push_back( BreakInfo( bolusTime.back(), true, j, nOccasion - 1 ) );
        }

        //------------------------------------------------------
        // Handle regular infusion doses.
        //------------------------------------------------------

        // If this is a regular infusion dose, then get its experiment
        // design related information.
        if ( amt > Value( 0 ) && rate > Value( 0 ) )
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
          breakPoint.push_back( BreakInfo( infusTime.back(), true, j, nOccasion - 1 ) );

          // Add a break point at the end of this regular infusion
          // dose that does not correspond to a date record .
          breakPoint.push_back( BreakInfo( infusTime.back() +
                                           infusDurat.back(), false, j, nOccasion - 1 ) );
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
        if ( amt > Value( 0 ) && ( rate == Value( -2 ) || rate == Value( -1 ) ) )
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
          if ( rate == Value( -2 ) )
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
          breakPoint.push_back( BreakInfo( zeroOrderBolusTime.back(), true, j, nOccasion - 1 ) );

          // Add a break point at the end of this zero-order bolus
          // dose that does not correspond to a date record .
          breakPoint.push_back( BreakInfo( zeroOrderBolusTime.back() +
                                           zeroOrderBolusDurat.back(), false, j, nOccasion - 1 ) );
        }

        //------------------------------------------------------
        // Add an ODE solution time for doses with nonzero amounts.
        //------------------------------------------------------

        // If this dose has a nonzero amount, then add an ODE solution
        // time at the same time as the dose.
        //
        // This will cause a prediction evaluation to be calculated
        // there, but it won't appear in the objective function
        // calculation.
        if ( amt > Value( 0 ) )
        {
          // Set the compartment.
          if ( pcmt > 0 )
          {
            // If the PCMT data item is present in the data record, if
            // its value has been reset from its initial value, and if
            // the prediction compartment is specified, then the
            // prediction evaluation will be for that compartment.
            odeSolnComp.push_back( pcmt );
          }
          else
          {
            // If the predicition compartment is not specified, then
            // the prediction evaluation will be for the default
            // observation compartment.
            odeSolnComp.push_back( defaultObservComp );
          }

          // Set the time.
          odeSolnTime.push_back( time );

          // Set the occasion index.
          odeSolnOccasionIndex.push_back( nOccasion - 1 );

          // The ODE solution should be left continuous unless there is
          // already an instantaneous bolus dose at the same time, which
          // means that the bolus data record came before this one.
          odeSolnIsLeftCont.push_back( !isEqualtoLastElem( time, bolusTime ) );

          // Set this data record's index in the vector of
          // ODE solutions.
          dataRecOdeSolnIndex[j] = ( odeSolnTime.size() - 1 );
          dataRecHasOdeSoln  [j] = true;

          nDosePred++;

          // Add another break point for this nonobservation
          // prediction evaluation event.  There are now two break
          // points at the same time for this dose.
          breakPoint.push_back( BreakInfo( odeSolnTime.back(), true, j, nOccasion - 1 ) );
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

          // Set the occasion index.
          turnOnOrOffOccasionIndex.push_back( nOccasion - 1 );
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
  
          // Set the occasion index.
          odeSolnOccasionIndex.push_back( nOccasion - 1 );

          // The ODE solution should be left continuous unless there is
          // already an instantaneous bolus dose at the same time, which
          // means that the bolus data record came before this one.
          odeSolnIsLeftCont.push_back( !isEqualtoLastElem( time, bolusTime ) );

          // Set this data record's index in the vector of
          // ODE solutions.
          dataRecOdeSolnIndex[j] = ( odeSolnTime.size() - 1 );
          dataRecHasOdeSoln  [j] = true;

          nNonObservPred++;

          // Add a break point for this nonobservation prediction
          // evaluation event.
          breakPoint.push_back( BreakInfo( odeSolnTime.back(), true, j, nOccasion - 1 ) );
        }

      }


      //--------------------------------------------------------
      // Handle reset events.
      //--------------------------------------------------------

      if ( evid == RESET_EVENT )
      {
        // Add a break point at the beginning of this occasion.
        breakPoint.push_back( BreakInfo( time, true, j, nOccasion - 1 ) );
      }


      //--------------------------------------------------------
      // Handle multiple ODE solutions at the same time.
      //--------------------------------------------------------

      int jReset;
      bool keepGoingBack;

      // Get the current number of ODE solutions.
      nOdeSoln = odeSolnTime.size();

      // Remove any ODE solution times that are equal.
      if ( nOdeSoln > 1 )
      {
        if ( odeSolnTime[nOdeSoln - 1] == odeSolnTime[nOdeSoln - 2] )
        {
          // If the last two ODE solution times are equal, then remove
          // the last one.
          odeSolnTime         .pop_back();
          odeSolnComp         .pop_back();
          odeSolnIsLeftCont   .pop_back();
          odeSolnOccasionIndex.pop_back();

          nOdeSoln--;
          nOdeSolnRemoved++;
    
          // Find any data records with ODE solution indices equal to
          // the removed ODE solution time's index and reset them to
          // point to the ODE solution that was not removed.
          jReset = j;
          keepGoingBack = true;
          while ( jReset >= 0 && keepGoingBack )
          {
            // Only check the data records that have ODE solutions.
            if ( dataRecHasOdeSoln[jReset] )
            {
              if ( dataRecOdeSolnIndex[jReset] == nOdeSoln )
              {
                // If the ODE solution index is for the removed
                // solution time, change it to be the time that was
                // not removed.
                dataRecOdeSolnIndex[jReset] = nOdeSoln - 1;
              }
              else
              {
                // If the ODE solution index is not for the removed
                // solution time, there is no need to keep going back.
                keepGoingBack = false;
              }
            }
            jReset--;
          }

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
    // See if the output compartment is used or not.
    //----------------------------------------------------------

    int q;
    int s;

    // This will be set equal to true if the output compartment is
    // used in this experiment design.
    isOutputCompUsed = false;

    // See if the output compartment is initially on.
    if ( !compInitialOff[ compIndex( nComp ) ] )
    {
      isOutputCompUsed = true;
    }
    else
    {
      // If the output compartment is initially off, see if it is
      // turned on at a later time.
      for ( q = 0; q < nTurnOnOrOff; q++ )
      {
        if ( turnOnOrOffComp[q] == nComp && turnOnOrOffComp[q] > 0 )
        {
          isOutputCompUsed = true;
        }
      }
    }

    // Do preparations related to whether or not the output
    // compartment is used.
    if ( isOutputCompUsed )
    {
      // Include the output compartment in the number of compartments
      // to integrate.
      nCompToSolve = nComp;
    }
    else
    {
      // Don't include the output compartment in the number of
      // compartments to integrate.
      nCompToSolve = nComp - 1;

      // Resize the matrix of amounts in all of compartments for all
      // of the ODE solution times except for the output compartment .
      compAmountAllOdeSolnNoOutputComp.resize( nCompToSolve * nOdeSoln );

      // Make sure that no ODE solutions are requested for the output
      // compartment.
      for ( s = 0; s < nOdeSoln; s++ )
      {
        if ( odeSolnComp[s] == nComp )
        {
          string message = "An ODE solution was requested for the output compartment, which was \nnever turned on for the " + 
            intToOrdinalString( i, ZERO_IS_FIRST_INT ) +
            " individual.";
        
          throw SpkException(
            SpkError::SPK_USER_INPUT_ERR, 
            message.c_str(),
            __LINE__, 
            __FILE__ );
        }
      }
    }


    //----------------------------------------------------------
    // Get the regular infusion dose off times and sort them.
    //----------------------------------------------------------

    // If there are any regular infusion doses, then get their off
    // times and sort them.
    if ( nInfus > 0 )
    {
      infusOffPoint.resize( 0 );

      // Save the regular infusion dose off times and their indices.
      for ( q = 0; q < nInfus; q++ )
      {
        infusOffPoint.push_back( 
          InfusOffInfo( ( infusTime[q] + infusDurat[q] ), q, nOccasion - 1 ) );
      }

      // Sort the off times.
      //
      // Note that the less than operators have been modified so that
      // the times will be grouped by occasion and then sorted in
      // nondecreasing order.
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
          ZeroOrderBolusOffInfo( ( zeroOrderBolusTime[q] + zeroOrderBolusDurat[q] ), q, nOccasion - 1 ) );
      }

      // Sort the off times.        
      //
      // Note that the less than operators have been modified so that
      // the times will be grouped by occasion and then sorted in
      // nondecreasing order.
      sort( zeroOrderBolusOffPoint.begin(), zeroOrderBolusOffPoint.end() );
    }


    //----------------------------------------------------------
    // Assign break point indices to the various quantities.
    //----------------------------------------------------------

    // Sort the break point information in order by break point times
    // because the break point handler expects the break point times
    // to be in nondecreasing order.
    //
    // Note that the less than operators have been modified so that
    // the times will be grouped by occasion and then sorted in
    // nondecreasing order.
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
                                    nOdeSolnRemoved          +
                                    bolusCounter             +
                                    infusOnCounter           +
                                    infusOffCounter          +
                                    zeroOrderBolusOnCounter  +
                                    zeroOrderBolusOffCounter +
                                    nOccasion - 1               ||
         infusOnCounter          != infusOffCounter             ||
         nInfus                  != infusOnCounter              ||
         zeroOrderBolusOnCounter != zeroOrderBolusOffCounter    ||
         nZeroOrderBolus         != zeroOrderBolusOnCounter )
    {
      string message = "The proper number of break points could not be determined for the \n" + 
        intToOrdinalString( i, ZERO_IS_FIRST_INT ) +
        " individual\'s data records.";

      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        message.c_str(),
        __LINE__, 
        __FILE__ );
    }


    //----------------------------------------------------------
    // Remove break point times beyond each occasion's last solution time.
    //----------------------------------------------------------

    int b;

    // Delete any break point times that are greater than or equal to
    // the last ODE solution time for each occasion because the break
    // point handler expects the last break point time to be less than
    // the last ODE solution time for each occasion.
    k = 0;
    s = 0;
    for ( b = 0; b < nOccasion; b++ )
    {
      // Find the last ODE solution time for this occasion.
      while ( s < nOdeSoln && odeSolnOccasionIndex[s] == b )
      {
        s++;
      }
      s--;

      // Look for break points for this occasion that have times
      // greater than the last ODE solution time for this occasion.
      while ( k < nBreak && breakPoint[k].occasionIndex == b )
      {
        if ( breakPoint[k].time >= odeSolnTime[s] )
        {
          // Delete this break point if its time is too great. 
          breakPoint.erase( breakPoint.begin() + k );
          nBreak--;
        }
        else
        {
          // Go on to the next break point.
          k++;
        }
      }

      // Go on to the first ODE solution from the next occasion.
      s++;
    }

    // Set the vector of break times for OdeBreak().
    breakTime.resize( nBreak );
    for ( k = 0; k < nBreak; k++ )
    {
      breakTime[k] = breakPoint[k].time;
    }


    //----------------------------------------------------------
    // Validate the experiment design information.
    //----------------------------------------------------------

    // Check for a nonzero start time for this individual.
    if ( breakTime[0] != Value( 0 ) )
    {
      // Issue a warning if necessary.
      if ( !issuedStartTimeWarning )
      {
        WarningsManager::addWarning( 
          "The integration of the ordinary differential equations begins at the first \ntime available for an individual.  The starting time for the model predictions \nis not TIME = 0 for at least one individual in this data set. If this is not \nwhat was intended, adding a data point at TIME = 0 will solve this problem.",
          __LINE__,
          __FILE__ );
    
        issuedStartTimeWarning = true;
      }
    }
    

    //----------------------------------------------------------
    // Check the sizes of the experiment design information.
    //----------------------------------------------------------

    assert( nOdeSoln == nObserv + nDosePred + nNonObservPred - nOdeSolnRemoved );

    assert( odeSolnComp        .size() == nOdeSoln );
    assert( odeSolnIsLeftCont  .size() == nOdeSoln );

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

    assert( compAmountIn  .size() == nCompToSolve );
    assert( bolusAmountOut.size() == nCompToSolve );


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

      // Check for reset event records.
      if ( evid == RESET_EVENT )
      { 
        // Set initial values for the parameters associated with
        // each of the compartments.
        int p;
        for ( p = 0; p < nComp; p++ )
        {
          compAmount       [p] = Value( 0 );
          compAmount_t     [p] = Value( 0 );
        
          compInfusRate    [p] = Value( 0 );
          compInfusDurat   [p] = Value( 0 );
        
          compIsOff        [p] = compInitialOff  [p];
        }
      }

    }


    //----------------------------------------------------------
    // Set the instantaneous bolus doses for this break point.
    //----------------------------------------------------------

    // Zero the instantaneous bolus doses for all of the compartments.
    int p;
    for ( p = 0; p < nCompToSolve; p++ )
    {
      bolusAmountOut[p] = Value( 0 );
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
    // current break point.
    q = 0;
    while ( q < nTurnOnOrOff )
    {
      // If this turn-compartment-on-or-off event is for the current
      // break point's time and occasion index, then turn on or off
      // the appropriate compartment.
      if ( turnOnOrOffTime[q]          == breakPoint[kCurr].time          && 
           turnOnOrOffOccasionIndex[q] == breakPoint[kCurr].occasionIndex )
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

    assert( compAmountIn   .size() == nCompToSolve );
    assert( compAmount_tOut.size() == nCompToSolve );


    //----------------------------------------------------------
    // Do any preparations required for this time.
    //----------------------------------------------------------

    // Set the current value for the continuous time variable that
    // appears in the ordinary differential equations.
    tCurr = tIn;

    int p;

    // Set the current value for the amounts in the compartments.
    if ( isOutputCompUsed )
    {
      // Set all of the compartments.
      compAmount = compAmountIn;
    }
    else
    {
      // Set all of the compartments except the output compartment.
      for ( p = 0; p < nCompToSolve; p++ )
      {
        compAmount[p] = compAmountIn[p];
      }

      // Set the output compartment.
      compAmount[nComp - 1] = Value( 0 );
    }


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
    if ( isOutputCompUsed )
    {
      // Set all of the compartments.
      compAmount_tOut = compAmount_t;
    }
    else
    {
      // Set all of the compartments except the output compartment.
      for ( p = 0; p < nCompToSolve; p++ )
      {
        compAmount_tOut[p] = compAmount_t[p];
      }
    }


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
        p = compIndex( infusComp[q] );

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
    for ( p = 0; p < nCompToSolve - 1; p++ )
    {
      if ( compIsOff[p] )
      {
        compAmount_tOut[p] = Value( 0 );
      }

      // Check that the amounts in compartments that are off are equal
      // to zero.
      assert( !compIsOff[p] || compAmount[p] == Value( 0 ) );
    }


    //----------------------------------------------------------
    // Calculate the derivative of the output compartment.
    //----------------------------------------------------------

    // Get the index for the output compartment.
    int m = compIndex( nComp );
  
    // The output compartment contains the total amount eliminated
    // from the system from the time tOn when the output compartment
    // was turned on.  Its derivative with respect to time is
    //
    //                           ---
    //      d                    \         d          
    //     ---  A     (t)  =     /      - ---  A (t)  ,
    //      dt   nComp           ---       dt   p        
    //                        p < nComp
    //
    // where nComp is the number for the output compartment.
    //
    // If the output compartment is being used and is turned on, then
    // set its derivative equal to minus the sum of the derivatives
    // for all of the other compartments.
    if ( isOutputCompUsed && !compIsOff[m] )
    {
      // Zero the derivative of the output compartment. 
      compAmount_tOut[m] = Value( 0 );
  
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


    //----------------------------------------------------------
    // Check for invalid compartment amounts or time derivatives.
    //----------------------------------------------------------

    bool isThereANanOrInf = false;

    // Check the compartment amounts and time derivatives for elements
    // that are infinite or are Not a Number (NaN).
    for ( p = 0; p < nCompToSolve; p++ )
    {
      compIsAmountNan[p]   = isNotANumber( compAmount[p] );
      compIsAmount_tNan[p] = isNotANumber( compAmount_t[p] );

      // NaN's are also unnormalized, so only set the flags for
      // infinite values when the values are not NaN.
      compIsAmountInf[p]   = !compIsAmountNan[p]   && isUnnormNumber( compAmount[p] );
      compIsAmount_tInf[p] = !compIsAmount_tNan[p] && isUnnormNumber( compAmount_t[p] );

      isThereANanOrInf = isThereANanOrInf            ||  
        compIsAmountInf[p]   || compIsAmountNan[p]   ||
        compIsAmount_tInf[p] || compIsAmount_tNan[p];
    }

    // If an invalid value is found, then set the derivative's
    // elements equal to 0/0, which evaluates to NaN, and which 
    // will make the ODE integrator decrease its step size.
    if ( isThereANanOrInf )
    {
      tNanOrInf = tCurr;

      Value zero = Value( 0 );

      for ( p = 0; p < nCompToSolve; p++ )
      {
        compAmount_tOut[p] = zero / zero;
      }
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

    // Zero the compartment amounts and their derivatives to make sure
    // there are no left over NaN's or infinite values from previous
    // individuals' integrations.
    int p;
    for ( p = 0; p < nCompToSolve; p++ )
    {
      compAmount  [p] = Value( 0 );
      compAmount_t[p] = Value( 0 );
    }


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

    int b;
    int k;

    // Set the Eval class for OdeBreak to be this class so that the
    // functions Break() and Ode() will have access to this class's
    // functions and members.
    OdePredBase& odeBreakEvalClass = *this;

    // Since only the relative tolerance is currently provided to this
    // class, set the absolute tolerances for integration of the
    // ordinary differential equations equal to zero.
    std::vector<Value> tolAbs( nCompToSolve, Value( 0 ) );

    // This message will be used if an error occurs.
    string message = "during the evaluation of the compartment amounts for \nall of the requested times for the " +
      intToOrdinalString( iCurr, ZERO_IS_FIRST_INT ) +
      " individual.";

    // Calculate the amounts in all of the compartments at all of the
    // ODE solution times by solving the differential equations from
    // the DES block numerically.
    try
    {
      std::string method = "Runge45";

      //--------------------------------------------------------
      // Handle a single occasion.
      //--------------------------------------------------------

      if ( nOccasion == 1 )
      {
        if ( isOutputCompUsed )
        {
          OdeBreak(
            odeBreakEvalClass,
            compAmountAllOdeSoln,
            method,
            breakTime,
            odeSolnTime,
            odeSolnIsLeftCont,
            tolAbs,
            tolRel );
        }
        else
        {
          OdeBreak(
            odeBreakEvalClass,
            compAmountAllOdeSolnNoOutputComp,
            method,
            breakTime,
            odeSolnTime,
            odeSolnIsLeftCont,
            tolAbs,
            tolRel );
        }
      }

      //--------------------------------------------------------
      // Handle multiple occasions.
      //--------------------------------------------------------

      else
      {
        std::vector<Value> breakTime_b;
        std::vector<Value> compAmountAllOdeSoln_b;
        std::vector<Value> compAmountAllOdeSolnNoOutputComp_b;
        std::vector<Value> odeSolnTime_b;

        std::vector<bool>  odeSolnIsLeftCont_b;

        int nOdeSoln_b;
        int odeSolnCounter = 0;

        int s;
        int ss;

        // Calculate the solutions for each occasion.
        k = 0;
        s = 0;
        for ( b = 0; b < nOccasion; b++ )
        {
          // Get the break times for this occasion.
          breakTime_b.clear();
          while ( k < nBreak )
          {
            // Add this break time if it's from this occasion.
            if ( breakPoint[k].occasionIndex == b )
            {
              breakTime_b.push_back( breakPoint[k].time );
              k++;
            }
            else
            {
              // Go on if this break time is from another occasion.
              break;
            }
          }
  
          // Get the ODE solution times for this occasion.
          odeSolnTime_b      .clear();
          odeSolnIsLeftCont_b.clear();
          while ( s < nOdeSoln )
          {
            // Add this ODE solution time if it's from this occasion.
            if ( odeSolnOccasionIndex[s] == b )
            {
              odeSolnTime_b      .push_back( odeSolnTime[s] );
              odeSolnIsLeftCont_b.push_back( odeSolnIsLeftCont[s] );
              s++;
            }
            else
            {
              // Go on if this ODE solution time is from another occasion.
              break;
            }
          }

          // Get the number of solution times for this occasion.
          nOdeSoln_b = odeSolnTime_b.size();
  
          // Skip the call the OdeBreak() for occasions without
          // ODE solution times.
          if ( nOdeSoln_b > 0 )
          {
            if ( isOutputCompUsed )
            {
              // Resize the matrix of amounts for this occasion in all of
              // compartments for all of the ODE solution times.
              compAmountAllOdeSoln_b.resize( nComp * nOdeSoln_b );
    
              OdeBreak(
                odeBreakEvalClass,
                compAmountAllOdeSoln_b,
                method,
                breakTime_b,
                odeSolnTime_b,
                odeSolnIsLeftCont_b,
                tolAbs,
                tolRel );
  
              // Put this occasion's amounts onto the vector with all of
              // the amounts for all of the ODE solution times.
              for ( p = 0; p < nComp; p++ )
              {
                for ( ss = 0; ss < nOdeSoln_b; ss++ )
                {
                  compAmountAllOdeSoln[p + ( odeSolnCounter + ss ) * nComp] = 
                    compAmountAllOdeSoln_b[p + ss * nComp];
                }
              }
            }
            else
            {
              // Resize the matrix of amounts for this occasion in all of
              // compartments for all of the ODE solution times except for 
              // the output compartment .
              compAmountAllOdeSolnNoOutputComp_b.resize( nCompToSolve * nOdeSoln_b );
    
              OdeBreak(
                odeBreakEvalClass,
                compAmountAllOdeSolnNoOutputComp_b,
                method,
                breakTime_b,
                odeSolnTime_b,
                odeSolnIsLeftCont_b,
                tolAbs,
                tolRel );
  
              // Put this occasion's amounts onto the vector with all of
              // the amounts for all of the ODE solution times except for 
              // the output compartment .
              for ( p = 0; p < nCompToSolve; p++ )
              {
                for ( ss = 0; ss < nOdeSoln_b; ss++ )
                {
                  compAmountAllOdeSolnNoOutputComp[p + ( odeSolnCounter + ss ) * nCompToSolve] = 
                    compAmountAllOdeSolnNoOutputComp_b[p + ss * nCompToSolve];
                }
              }
            }
          }

          // Add the number of ODE solutions that were in this
          // occasion.
          odeSolnCounter += nOdeSoln_b;
        }
      }
    }
    catch( SpkException& e )
    {
      // See if there was an error during the solution of the
      // ODE's and that there were no standard errors.
      if ( e.find( SpkError::SPK_ODE_SOLN_ERR ) >= 0 &&
           e.find( SpkError::SPK_STD_ERR      ) <  0    )
      {
        ostringstream messageOSS;
        int commaCounter;
        int nCompBad;

        // See if any amounts were infinite.
        nCompBad = count( compIsAmountInf.begin(), compIsAmountInf.end(), true );
        if ( nCompBad > 0 )
        {
          messageOSS.str( "" );
          messageOSS << "The following compartments' amounts were infinite at time "
                     << tNanOrInf << ": \n";
          commaCounter = 0;
          for ( p = 0; p < nCompToSolve; p++ )
          {
            if ( compIsAmountInf[p] )
            {
              messageOSS << p + 1 << ( commaCounter++ < nCompBad - 1 ? ", " : "" );
            }
          }
          messageOSS << ".";

          e.push(
            SpkError::SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR,
            messageOSS.str().c_str(),
            __LINE__,
            __FILE__ );
        }

        // See if any amounts were Nan's.
        nCompBad = count( compIsAmountNan.begin(), compIsAmountNan.end(), true );
        if ( nCompBad > 0 )
        {
          messageOSS.str( "" );
          messageOSS << "The following compartments' amounts were Not a Number (NaN) at time "
                     << tNanOrInf << ": \n";
          commaCounter = 0;
          for ( p = 0; p < nCompToSolve; p++ )
          {
            if ( compIsAmountNan[p] )
            {
              messageOSS << p + 1 << ( commaCounter++ < nCompBad - 1 ? ", " : "" );
            }
          }
          messageOSS << ".";

          e.push(
            SpkError::SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR,
            messageOSS.str().c_str(),
            __LINE__,
            __FILE__ );
        }

        // See if any time derivatives were infinite.
        nCompBad = count( compIsAmount_tInf.begin(), compIsAmount_tInf.end(), true );
        if ( nCompBad > 0 )
        {
          messageOSS.str( "" );
          messageOSS << "The following compartments' time derivatives were infinite at time "
                     << tNanOrInf << ": \n";
          commaCounter = 0;
          for ( p = 0; p < nCompToSolve; p++ )
          {
            if ( compIsAmount_tInf[p] )
            {
              messageOSS << p + 1 << ( commaCounter++ < nCompBad - 1 ? ", " : "" );
            }
          }
          messageOSS << ".";

          e.push(
            SpkError::SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR,
            messageOSS.str().c_str(),
            __LINE__,
            __FILE__ );
        }

        // See if any time derivatives were Nan's.
        nCompBad = count( compIsAmount_tNan.begin(), compIsAmount_tNan.end(), true );
        if ( nCompBad > 0 )
        {
          messageOSS.str( "" );
          messageOSS << "The following compartments' time derivatives were Not a Number (NaN) \nat time "
                     << tNanOrInf << ":  ";
          commaCounter = 0;
          for ( p = 0; p < nCompToSolve; p++ )
          {
            if ( compIsAmount_tNan[p] )
            {
              messageOSS << p + 1 << ( commaCounter++ < nCompBad - 1 ? ", " : "" );
            }
          }
          messageOSS << ".";

          e.push(
            SpkError::SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR,
            messageOSS.str().c_str(),
            __LINE__,
            __FILE__ );
        }

        throw e.push(
          SpkError::SPK_UNKNOWN_ERR, 
          ( "An error occurred " + message ).c_str(),
          __LINE__,
          __FILE__ );
      }
      else
      {
        throw e.push(
          SpkError::SPK_UNKNOWN_ERR, 
          ( "An error occurred " + message ).c_str(),
          __LINE__,
          __FILE__ );
      }
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


    //----------------------------------------------------------
    // Set the values for the output compartment if necessary.
    //----------------------------------------------------------

    // If the output compartment isn't used, then the amounts in all
    // of the compartments at all of the ODE solution times still need
    // to be set.
    if ( !isOutputCompUsed )
    {
      int s;

      // Copy all of the compartment values except the output
      // compartment's.
      for ( p = 0; p < nComp - 1; p++ )
      {
        for ( s = 0; s < nOdeSoln; s++ )
        {
          compAmountAllOdeSoln[p + s * nComp] = 
            compAmountAllOdeSolnNoOutputComp[p + s * nCompToSolve];
        }
      }

      // Set the output compartment's values equal to zero.
      p = nComp - 1;
      for ( s = 0; s < nOdeSoln; s++ )
      {
        compAmountAllOdeSoln[p + s * nComp] = Value( 0 );
      }
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

template <class Value>
bool OdePredBase<Value>::issuedStartTimeWarning = false;

#endif
