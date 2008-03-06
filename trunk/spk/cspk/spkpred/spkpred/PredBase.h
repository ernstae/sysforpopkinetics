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
 * @file PredBase.h
 * 
 * 
 * Declares PredBase class.
 *//*
 * Author: Sachiko Honda
 *
 * Modified later by: Mitch Watrous
 *
 *************************************************************************/

#ifndef PREDBASE_H
#define PREDBASE_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include <spk/intToOrdinalString.h>
#include <spk/isNotANumber.h>
#include <spk/isUnnormNumber.h>
#include <spk/SpkError.h>
#include <spk/SpkException.h>

// CppAD header files.
#include <CppAD/CppAD.h>

#include <vector>
#include <string>


/*************************************************************************
 *
 * Class: PredBase
 *
 *//**
 * This abstract base class provides the interfaces needed to evaluate
 * Pred block based models.
 *//*
 *************************************************************************/

template <class Value>
class PredBase
{
  /***********************************************************************
   *
   * Function: initPredSubAD
   *
   *//**
   * Initialize values for a CppAD based subclass of this abstract
   * base class.
   *
   * The default version of this function provided with this base
   * class does nothing.
   */
  /***********************************************************************/

public:
  virtual void initPredSubAD( PredBase< CppAD::AD<Value> >* pPredBaseADIn )
  {
  }


  /***********************************************************************
   *
   * Function: getVariableInfo
   *
   *//**
   * Gets a string containing information about the variables that are
   * members of this class and their current values.
   *
   * An improved version of this default version should be implemented
   * in the subclasses of this base class.
   */
  /***********************************************************************/

public:
  virtual void getVariableInfo( std::string& messageStr ) const
  {
    messageStr = "No variable information is currently available.";
  }


  /***********************************************************************
   *
   * Function: eval
   *
   *//**
   * <code>eval</code> evaluates the Pred block expressions for the
   * i-th individual's j-th data record at the given values for the
   * independent variables.
   *
   * If the Dependent Variable (DV) data item for this data record is
   * not missing, i.e., its Missing Dependent Variable (MDV) data item
   * is equal to zero, then the calculated value for y
   * will be set in the vector of dependent variables.
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
   * @param fOffset         The index to the head of the vector of f  
   *                        values within depVar.  Note:  this parameter is no 
   *                        longer used because in the current implementation
   *                        depVar = y.
   * @param fLen            The total length of f vector.
   * @param yOffset         The index to the head of the vector of y 
   *                        values within depVar.  Note:  this parameter is no 
   *                        longer used because in the current implementation
   *                        depVar = y.
   * @param yLen            The total length of y vector.
   * @param i               The index to the individual of interest 
   *                        within the population (0 indicates the first
   *                        individual.)
   * @param j               The index to the data record of interest 
   *                        for the i-th individual (0 indicates their 
   *                        first data record.)
   * @param indepVar        The vector containing independent variables:
   *                        THETA, ETA and EPS.
   * @param depVar          (output) The vector whose m-th element will be 
   *                        replaced with the calculated value for
   *                        y if the DV data item for this data
   *                        record is not missing.  Note:  in the current
   *                        implementation
   *                        \f[
   *                          \mbox{depVar} = y  .
   *                        \f]
   *                        The number m is the position of this DV value in
   *                        the list of DV values for this individual that
   *                        are not missing.  Note that m is a number that
   *                        must be determined by the implementation of
   *                        this function, i.e., it is not an input to this 
   *                        function.
   *
   * @return true           if the DV value for the i-th individual's j-th 
   *                        data record was not missing.
   * @return false          if the DV value for the i-th individual's j-th 
   *                        data record was missing.
   */
  /***********************************************************************/

  virtual bool eval( int thetaOffset, int thetaLen,
                     int etaOffset,   int etaLen,
                     int epsOffset,   int epsLen,
                     int fOffset,     int fLen,
                     int yOffset,     int yLen,
                     int i,
                     int j,
                     const std::vector<Value>& indepVar,
                     std::vector<Value>& depVar ) = 0;


  /***********************************************************************
   *
   * Function: evalAllF
   *
   *//**
   * <code>evalAllF</code> evaluates the Pred block expressions for
   * all of the i-th individual's data records at the given values for
   * the independent variables.
   *
   * If the Dependent Variable (DV) data item for the j-th data record is
   * not missing, i.e., its Missing Dependent Variable (MDV) data item
   * is equal to zero, then the value for fOut will be set equal to 
   * the calculated value for y,
   * \f[
   *   fOut_{i(j)} = y_{i(j)}(theta, eta, eps)  .
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
   * @param fLen            The total length of f vector.
   * @param i               The index to the individual of interest 
   *                        within the population (0 indicates the first
   *                        individual.)
   * @param indepVar        The vector containing independent variables:
   *                        THETA, ETA and EPS.
   * @param fOut            (output) The vector whose m-th element will be 
   *                        replaced with the calculated value for
   *                        y if the DV data item for this data
   *                        record is not missing, 
   *                        \f[
   *                          \mbox{fOut} = y(theta, eta, eps)  .
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

    // Set the number of records for this individual.
    int nRecord = getNRecords( i );

    // Set the number of observation records for this individual.
    int nObserv = getNObservs( i );


    //----------------------------------------------------------
    // Check the inputs.
    //----------------------------------------------------------

    // See if there was the correct number of output f values.
    if ( fOut.size() != fLen )
    {
      std::string message = "The number of output f values does not match the expected number \nfor the " + 
        intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual.";

      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        message.c_str(),
        __LINE__, 
        __FILE__ );
    }


    //----------------------------------------------------------
    // Set the f values.
    //----------------------------------------------------------

    std::string taskMessage;
    bool isObsRecord;

    int j;

    // This will be equal to the number of f values that were set.
    int nFValueSet = 0;

    // Set quantities related to the vector of dependent variables
    // for the current individual,
    //
    //                 -                 -
    //     w( z )  =  |  f( theta, eta )  |  .
    //                 -                 -
    const int fOffset = 0;
    const int yOffset = 0;

    // Evaluate the expressions from the Pred block for all of
    // the data records for the current individual.
    for ( j = 0; j < nRecord; j++ )
    {
      taskMessage = "during the evaluation of the mean for the \n" + 
        intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual's " +
        intToOrdinalString( j, ZERO_IS_FIRST_INT ) + " data record.";

      // Evaluate the Pred block expressions for this data record.
      // The predicted value will be set during the call to eval()
      // if this data record is an observation record.
      //
      try
      {
        // Note that this call to eval sets
        //
        //     fOut     =  y    ( theta, eta, eps )  .
        //         i(j)     i(j)
        //
        isObsRecord = eval(
          thetaOffset,
          thetaLen,
          etaOffset,
          etaLen,
          epsOffset,
          epsLen,
          fOffset,
          nObserv,
          yOffset,
          nObserv,
          i,
          j,
          indepVar,
          fOut );
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
      // calculated predicted value to see if it is valid.
      if ( isObsRecord )
      {
        // Make sure that the value is not a NaN.
        if ( isNotANumber( fOut[ nFValueSet ] ) )
        {
          std::string message = "The mean of the " +
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
        if ( isUnnormNumber( fOut[ nFValueSet ] ) )
        {
          std::string message = "The mean of the " +
            intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual's " +
            intToOrdinalString( j, ZERO_IS_FIRST_INT ) + " data record was infinite.";
    
          throw SpkException(
            SpkError::SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR,
            message.c_str(),
            __LINE__,
            __FILE__ );
        }
    
        // Increment the counter.
        nFValueSet++;
      }
    }

    // See if there was the correct number of observation records.
    if ( nFValueSet != nObserv )
    {
      std::string message = "The number of data records that are observation records does not match the expected \nnumber of observation records for the " + 
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
   * <code>evalAllY</code> evaluates the Pred block expressions for
   * all of the i-th individual's data records at the given values for
   * the independent variables.
   *
   * If the Dependent Variable (DV) data item for the j-th data record is
   * not missing, i.e., its Missing Dependent Variable (MDV) data item
   * is equal to zero, then the value for yOut will be set equal to 
   * the calculated value for y,
   * \f[
   *   yOut_{i(j)} = y_{i(j)}(theta, eta, eps)  .
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
   *                        to this function.  Note that this is not currently 
   *                        used by this base class's version, but it is provided 
   *                        in this function's interface for subclasses to use.
   * @param yOut            (output) The vector whose m-th element will be 
   *                        replaced with the calculated value for
   *                        y if the DV data item for this data
   *                        record is not missing, 
   *                        \f[
   *                          \mbox{yOut} = y(theta, eta, eps)  .
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

    // Set the number of records for this individual.
    int nRecord = getNRecords( i );

    // Set the number of observation records for this individual.
    int nObserv = getNObservs( i );


    //----------------------------------------------------------
    // Check the inputs.
    //----------------------------------------------------------

    // See if there was the correct number of input f values.
    if ( fIn.size() != yLen )
    {
      std::string message = "The number of input f values does not match the expected number \nfor the " + 
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
      std::string message = "The number of output y values does not match the expected number \nfor the " + 
        intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual.";

      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        message.c_str(),
        __LINE__, 
        __FILE__ );
    }


    //----------------------------------------------------------
    // Set the y values.
    //----------------------------------------------------------

    std::string taskMessage;
    bool isObsRecord;

    int j;

    // This will be equal to the number of y values that were set.
    int nYValueSet = 0;

    // Set quantities related to the vector of dependent variables
    // for the current individual,
    //
    //                 -                      -
    //     w( z )  =  |  y( theta, eta, eps )  |  .
    //                 -                      -
    const int fOffset = 0;
    const int yOffset = 0;

    // Evaluate the expressions from the Pred block for all of
    // the data records for the current individual.
    for ( j = 0; j < nRecord; j++ )
    {
      taskMessage = "during the evaluation of the intra-individual error \nfor the " + 
        intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual's " +
        intToOrdinalString( j, ZERO_IS_FIRST_INT ) + " data record.";

      // Evaluate the Pred block expressions for this data record.
      // The predicted value will be set during the call to eval()
      // if this data record is an observation record.
      //
      try
      {
        // Note that this call to eval sets
        //
        //     yOut     =  y    ( theta, eta, eps )  .
        //         i(j)     i(j)
        //
        isObsRecord = eval(
          thetaOffset,
          thetaLen,
          etaOffset,
          etaLen,
          epsOffset,
          epsLen,
          fOffset,
          nObserv,
          yOffset,
          nObserv,
          i,
          j,
          indepVar,
          yOut );
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
      // calculated predicted value to see if it is valid.
      if ( isObsRecord )
      {
        // Make sure that the value is not a NaN.
        if ( isNotANumber( yOut[ nYValueSet ] ) )
        {
          std::string message = "The intra-individual error for the " +
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
        if ( isUnnormNumber( yOut[ nYValueSet ] ) )
        {
          std::string message = "The intra-individual error for the " +
            intToOrdinalString( i, ZERO_IS_FIRST_INT ) + " individual's " +
            intToOrdinalString( j, ZERO_IS_FIRST_INT ) + " data record \nwas infinite.";
    
          throw SpkException(
            SpkError::SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR,
            message.c_str(),
            __LINE__,
            __FILE__ );
        }
    
        // Increment the counter.
        nYValueSet++;
      }
    }

    // See if there was the correct number of observation records.
    if ( nYValueSet != nObserv )
    {
      std::string message = "The number of data records that are observation records does not match the expected \nnumber of observation records for the " + 
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
   * Function: getNRecords
   *
   *//**
   * getNRecords( int i ) returns the number of data records
   * for the i-th individual.  For the individual analysis,
   * i should be always 0.
   *
   * Previously, the PredBase class assumed that all data records
   * were observation records, and it did not have a getNRecords()
   * function.
   *
   * In order to be backward compatible, the implementation for this
   * function provided in this abstract base class returns the number
   * of observation records as the number of data records.
   *
   * Since there are usually more data records than observation
   * records, this function's implementation will usually be wrong.
   * If that is the case, then its implementation should be replaced
   * in the concrete subclass by an implementation that returns the
   * right number of data records.
   *
   * @param i The index to the individual of interest.
   *          0 indicates the first individual in the population.
   *          For the individual analysis, this value is ignored.
   * 
   * @return The number of data records for the i-th individual.
   *         If this function is called in the context of individual
   *         (only) analysis, then the number of the only individual's
   *         data records is returned.
   */
  /***********************************************************************/

  virtual int getNRecords( int i ) const
  {
    // For the purposes of backwards compatability , return the number
    // of observation records as the number of data records.  See the
    // description for this function for more details.
    return getNObservs( i );
  }


  /***********************************************************************
   *
   * Function: getNObservs
   *
   *//**
   * getNObservs( int i ) returns the number of observation records
   * for the i-th individual.  For the individual analysis,
   * i should be always 0.
   *
   * @param i The index to the individual of interest.
   *          0 indicates the first individual in the population.
   *          For the individual analysis, this value is ignored.
   * 
   * @return The number of observation records for the i-th individual.
   *         If this function is called in the context of individual
   *         (only) analysis, then the number of the only individual's
   *         observation records is returned.
   */
  /***********************************************************************/

  virtual int getNObservs( int i ) const = 0;

public:
  virtual ~PredBase(){}

protected:
  PredBase(){}
  PredBase( const PredBase& ){}
  PredBase & operator=( const PredBase& ){}
};
#endif
