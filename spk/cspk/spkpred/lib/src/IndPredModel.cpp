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
 * File: IndPredModel.cpp
 *
 *
 * This SpkModel subclass evaluates individual level models that
 * correspond to the expressions in an NM-TRAN $PRED block.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

// [Remove]=========================================
//
#include <string>
namespace // [Begin: unnamed namespace]
{
  using std::string;

  // MAKE THIS A SEPARATE FUNCTION.
  string intToOrderString( int i );
  
} // [End: unnamed namespace]

//
// [Remove]=========================================


/*************************************************************************
 *
 * Class: IndPredModel
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class specification
 *------------------------------------------------------------------------*/



/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "DiagCov.h"
#include "IndPredModel.h"
#include "isEqual.h"
#include "PredBase.h"

// SPK library header files.
#include <spk/AkronBtimesC.h>
#include <spk/allZero.h>
#include <spk/multiply.h>
#include <spk/SpkException.h>
#include <spk/SpkModel.h>
#include <spk/SpkValarray.h>

// [Remove]=========================================
//
// Currently CppAD does not install using automake and its directory
// structure which means the paths for CppAD header files here and in
// CppAD itself will have to be changed to be consistent with the rest
// of the libraries that make up CSPK.
//
// [Remove]=========================================
// CppAD header files.
#include <../cppad/CppAD.h>
#include <../cppad/include/Independent.h>

// Standard library header files.
#include <cassert>
#include <sstream>
#include <vector>

using namespace CppAD;
using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: IndPredModel
 *
 *
 * Constructor.
 *
 *************************************************************************/

IndPredModel::IndPredModel(
    PredBase< AD<double> >&  predEvaluatorIn,
    int                      nThetaIn,
    const valarray<double>&  thetaLowIn,
    const valarray<double>&  thetaUpIn,
    const valarray<double>&  thetaCurrIn,
    int                      nEtaIn,
    covStruct                omegaStructIn,
    const valarray<double>&  omegaParCurrIn )
  :
  predEvaluator                     ( predEvaluatorIn ),
  nTheta                            ( nThetaIn ),
  nEta                              ( nEtaIn ),
  thetaOffsetInIndPar               ( 0 ),
  omegaParOffsetInIndPar            ( nThetaIn ),
  nZ                                ( nThetaIn + nEtaIn ),
  thetaOffsetInZ                    ( 0 ),
  etaOffsetInZ                      ( nThetaIn ),
  fOffsetInW                        ( 0 ),
  thetaCurr                         ( nTheta ),
  etaCurr                           ( nEta ),
  zCurr                             ( nZ ),
  isDataMeanCurrOk                  ( false ),
  isDataMean_indParCurrOk           ( false ),
  isDataVarianceCurrOk              ( false ),
  isDataVariance_indParCurrOk       ( false ),
  isDataVarianceInvCurrOk           ( false ),
  isDataVarianceInv_indParCurrOk    ( false ),
  isPredADFunCurrOk                 ( false ),
  isPredFirstDerivCurrOk            ( false ),
  isPredSecondDerivCurrOk           ( false ),
  usedCachedDataMean                ( false ),
  usedCachedDataMean_indPar         ( false ),
  usedCachedDataVariance            ( false ),
  usedCachedDataVariance_indPar     ( false ),
  usedCachedDataVarianceInv         ( false ),
  usedCachedDataVarianceInv_indPar  ( false ),
  usedCachedPredADFunCurr           ( false ),
  usedCachedPredFirstDerivCurr      ( false ),
  usedCachedPredSecondDerivCurr     ( false )
{
  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrix.
  //------------------------------------------------------------

  // Construct omega, the covariance matrix for eta, with the
  // appropriate structure.
  if ( omegaStructIn == DIAGONAL )
  {
    pOmegaCurr = new DiagCov( nEta );
  }
  else
  {
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // [Revisit - Implement Full Covariance Matrices - Mitch]
    // Currently, full covariance matrices are not supported.
    /*
    pOmegaCurr = new FullCov( nEta );
    */
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR, 
      "Full covariance matrices are not currently supported.",
      __LINE__, 
      __FILE__ );
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  }

  // Get the number of parameters required by the structure of
  // this covariance matrix.
  nOmegaPar = pOmegaCurr->getNPar();

  // Set the initial value for the omega parameters.
  omegaParCurr.resize( nOmegaPar);
  omegaParCurr = omegaParCurrIn;
  pOmegaCurr->setPar( omegaParCurr );
  assert( omegaParCurrIn.size() == nOmegaPar );


  //------------------------------------------------------------
  // Initialize quantities related to the individual parameter.
  //------------------------------------------------------------

  // The individual parameter is composed of the parameters that are
  // optimized over when performing individual level estimation,
  //
  //             -              -
  //            |   thetaCurr    |
  //     b   =  |                |  .
  //      i     |  omegaParCurr  |
  //             -              -
  //
  nIndPar = nTheta + nOmegaPar;


  //------------------------------------------------------------
  // Initialize quantities related to the Pred block parameters.
  //------------------------------------------------------------

  // Set the current value for theta.
  thetaCurr = thetaCurrIn;
  assert( thetaCurrIn.size() == nTheta );

  // Set the lower limit for theta.
  thetaLow = thetaLowIn;
  assert( thetaLowIn.size() == nTheta );

  // Set the upper limit for theta.
  thetaUp = thetaUpIn;
  assert( thetaUpIn.size() == nTheta );

  // For individual level Pred models, eta is always equal to zero.
  etaCurr = 0.0;


  //------------------------------------------------------------
  // Initialize quantities related to the individual.
  //------------------------------------------------------------

  // For individual level Pred models, there is only a single
  // individual.
  nInd = 1;

  // Set the current individual to a number different than zero
  // so that selectIndividual() won't use the cached values.
  iCurr = 1;

  // Set the current individual.
  doSelectIndividual( 0 );
}


/*************************************************************************
 *
 * Function: ~IndPredModel
 *
 *
 * Destructor.
 *
 *************************************************************************/

IndPredModel::~IndPredModel()
{
  // Free the memory allocated for the omega covariance matrix.
  if ( pOmegaCurr )
  {
    delete pOmegaCurr;
  }

  // Free the memory allocated for the Pred block automatic
  // differentiation function object.
  if ( pPredADFunCurr )
  {
    delete pPredADFunCurr;
  }
}


/*************************************************************************
 *
 * Function: doSelectIndividual
 *
 *
 * Sets the index i for the current individual.
 *
 *************************************************************************/

void IndPredModel::doSelectIndividual( int iIn )
{
  //------------------------------------------------------------
  // See if the individual has changed.
  //------------------------------------------------------------

  // Don't do anything if the individual has not changed.
  if ( iIn == iCurr )
  { 
    return;
  }

  // Since the individual has changed, any cached values are no
  // longer valid.
  invalidateCache();


  //------------------------------------------------------------
  // Update the individual's information .
  //------------------------------------------------------------

  // Set the individual's index .
  iCurr = iIn;
  assert( 0 <= iIn && iIn < nInd );

  // Set the number of events for this individual.  Note that
  // for now all events are assumed to be observation events.
  nEventCurr = predEvaluator.getNObservs( iCurr );


  //------------------------------------------------------------
  // Update the Pred block dependent variable information.
  //------------------------------------------------------------

  // Set quantities related to the vector of dependent variables
  // for the current individual,
  //
  //                 -                 -
  //                |  f( theta )       |
  //     w( z )  =  |                   |  .
  //                |  y( theta, eta )  |
  //                 -                 -
  //
  // This vector contains quantities that are set when the 
  // expressions in the Pred block are evaluated.
  nW         = 2 * nEventCurr;
  yOffsetInW = nEventCurr;

  // Set the size of the vector of dependent variables.
  wCurr.resize( nW );

}


/*************************************************************************
 *
 * Function: doSetIndPar
 *
 *
 * Sets the current value for the individual parameter,
 *
 *             -              -
 *            |   thetaCurr    |
 *     b   =  |                |  .
 *      i     |  omegaParCurr  |
 *             -              -
 *
 * These are the parameters that are optimized over when performing
 * individual level estimation.
 *
 *************************************************************************/

void IndPredModel::doSetIndPar( const valarray<double>& bIn ) 
{ 
  //------------------------------------------------------------
  // See if the individual parameter has changed.
  //------------------------------------------------------------

  // Don't do anything if the individual parameter has not changed.
  if ( isEqual( bIn, bCurr ) )
  { 
    return;
  }

  // Since the individual parameter has changed, any cached 
  // values are no longer valid.
  invalidateCache();


  //------------------------------------------------------------
  // Update the individual parameter.
  //------------------------------------------------------------

  bCurr = bIn; 
  assert( bIn.size() == nIndPar );


  //------------------------------------------------------------
  // Update the parameters that appear in the Pred block.
  //------------------------------------------------------------

  int k;

  // Set the elements of theta.
  for ( k = 0; k < nTheta; k++ )
  {
    thetaCurr[k] = bCurr[k + thetaOffsetInIndPar];
  }


  //------------------------------------------------------------
  // Update the covariance matrix parameters.
  //------------------------------------------------------------

  // Set the parameters for the omega covariance matrix.
  for ( k = 0; k < nOmegaPar; k++ )
  {
    omegaParCurr[k] = bCurr[k + omegaParOffsetInIndPar];
  }

  // Pass the new omega parameters to the covariance object.
  pOmegaCurr->setPar( omegaParCurr );

}


/*************************************************************************
 *
 * Function: invalidateCache
 *
 *
 * Invalidates all of the values stored in the cache.
 *
 *************************************************************************/

void IndPredModel::invalidateCache() const
{
  isDataMeanCurrOk               = false;
  isDataMean_indParCurrOk        = false;
  isDataVarianceCurrOk           = false;
  isDataVariance_indParCurrOk    = false;
  isDataVarianceInvCurrOk        = false;
  isDataVarianceInv_indParCurrOk = false;
  isPredADFunCurrOk              = false;
  isPredFirstDerivCurrOk         = false;
  isPredSecondDerivCurrOk        = false;
  isOmegaCurrOk                  = false;
  isOmega_omegaParCurrOk         = false;
}


/*************************************************************************
 *
 * Function: evalAllPred
 *
 *
 * This function evaluates the predicted values for the data for
 * all of the events for the current individual.
 *
 * It does this by evaluating the expressions from the Pred block
 * for each event.
 *
 * Note that this function combines the parameters theta and eta
 * into a single vector of independent variables,
 *
 *            -       -
 *           |  theta  |
 *     z  =  |         |  .
 *           |  eta    |
 *            -       -
 *
 * In addition, this function combines the model functions f and y
 * into a single vector of dependent variables,
 *
 *                 -                 -
 *                |  f( theta )       |
 *     w( z )  =  |                   |  .
 *                |  y( theta, eta )  |
 *                 -                 -
 *
 *************************************************************************/

void IndPredModel::evalAllPred() const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  // Don't reevaluate the expressions in the Pred block if the
  // current version of the automatic differentiation function
  // object is valid.
  if ( isPredADFunCurrOk )
  {
    return;
  }


  //------------------------------------------------------------
  // Prepare the independent variables.
  //------------------------------------------------------------

  int k;

  // Set the current values for the theta dependent variables.
  for ( k = 0; k < nTheta; k++ )
  {
    zCurr[k + thetaOffsetInZ] = thetaCurr[k];
  }

  // Set the current values for the eta dependent variables.
  for ( k = 0; k < nEta; k++ )
  {
    zCurr[k + etaOffsetInZ] = etaCurr[k];
  }

  // Set these to zero since there are no eps variables for
  // individual level Pred models.
  const int nEps         = 0;
  const int epsOffsetInZ = 0;

  // Declare the independent variables.  This specifies the
  // domain for the differentiable function object that will
  // be constructed after the expressions in the Pred block
  // have been evaluated.
  CppAD::Independent( zCurr );


  //------------------------------------------------------------
  // Evaluate all of the predicted values for the data.
  //------------------------------------------------------------

  string taskIndAndValMessage;
  bool isObsEvent;
  double fCurr;
  double yCurr;
  int j;

  // Evaluate the expressions from the Pred block for all of
  // events for the current individual.
  for ( j = 0; j < nEventCurr; j++ )
  {
    //----------------------------------------------------------
    // Evaluate the predicted value for the data for this event.
    //----------------------------------------------------------

    // This message will be used if an error occurs.
    taskIndAndValMessage = "during the evaluation of the " +
        intToOrderString( j + 1 ) + " predicted value for the " +
        intToOrderString( iCurr + 1 ) + " individual's data.";

    // Evaluate the Pred block expressions for this event.
    try
    {
      isObsEvent = predEvaluator.eval(
        thetaOffsetInZ,
        nTheta,
        etaOffsetInZ,
        nEta,
        epsOffsetInZ,
        nEps,
        fOffsetInW,
        nEventCurr,
        yOffsetInW,
        nEventCurr,
        iCurr,
        j,
        zCurr,
        wCurr );
    }
    catch( SpkException& e )
    {
      // [Revisit - SPK Error Codes Don't Really Apply - Mitch]
      // This error code should be replaced with one that is accurate.
      throw e.push(
        SpkError::SPK_MODEL_DATA_MEAN_ERR,
        ( "An SpkException was thrown" + taskIndAndValMessage ).c_str(),
        __LINE__,
        __FILE__ );
    }
    catch( const std::exception& stde )
    {
      throw SpkException(
        stde,
        ( "A standard exception was thrown " + taskIndAndValMessage ).c_str(),
        __LINE__, 
        __FILE__ );
    }  
    catch( ... )
    {
      throw SpkException(
        SpkError::SPK_UNKNOWN_ERR, 
        ( "An unknown exception was thrown " + taskIndAndValMessage ).c_str(),
        __LINE__, 
        __FILE__ );
    }

    // [Revisit - Only Observation Events are Currently Allowed - Mitch]
    // Eventually this class will need to handle more than just
    // observation events.
    //
    // For now, all events are assumed to be observation events.
    if ( !isObsEvent )
    {
      // [Revisit - SPK Error Codes Don't Really Apply - Mitch]
      // This error code should be replaced with one that is accurate.
      throw SpkException(
        SpkError::SPK_MODEL_DATA_MEAN_ERR,
        ( "A non-observation event was encountered " + 
          taskIndAndValMessage ).c_str(),
        __LINE__,
        __FILE__ );
    }


    //----------------------------------------------------------
    // Validate the predicted value for the data for this event.
    //----------------------------------------------------------

    // Get the current values for the mean of the data,
    //
    //     f   ( theta )  ,
    //      (j)
    //
    // and the predicted value for the data,
    //
    //     y   ( theta, eta )  .
    //      (j)
    //
    fCurr = Value( wCurr[j + fOffsetInW] );
    yCurr = Value( wCurr[j + yOffsetInW] );

    // Make sure that the values are not infinite.
    if ( fabs( fCurr ) == numeric_limits<double>::infinity() ||
         fabs( yCurr ) == numeric_limits<double>::infinity() )
    {
      // [Revisit - SPK Error Codes Don't Really Apply - Mitch]
      // This error code should be replaced with one that is accurate.
      throw SpkException(
        SpkError::SPK_MODEL_DATA_MEAN_ERR,
        ( "An infinite value was generated " + taskIndAndValMessage ).c_str(),
        __LINE__,
        __FILE__ );
    }

    // Make sure that the values are not NaN's.
    if ( fCurr != fCurr ||
         yCurr != yCurr )
    {
      // [Revisit - SPK Error Codes Don't Really Apply - Mitch]
      // This error code should be replaced with one that is accurate.
      throw SpkException(
        SpkError::SPK_MODEL_DATA_MEAN_ERR,
        ( "A value that is Not a Number (NaN) was generated " + 
          taskIndAndValMessage ).c_str(),
        __LINE__,
        __FILE__ );
    }

  }


  //------------------------------------------------------------
  // Define the Pred block automatic differentiation function object.
  //------------------------------------------------------------

  // If a current version of the Pred block automatic differentiation
  // function object already exists, then delete it.
  if ( pPredADFunCurr )
  {
    delete pPredADFunCurr;
  }

  // Construct a differentiable function object that corresponds 
  // to the mapping of z to w, which will be represented here as
  //
  //     w  =  pred( z )  .
  //
  // This function is defined by the relationships between z and
  // w contained in predEvaluator.eval().
  pPredADFunCurr = new ADFun<double>( zCurr, wCurr );


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isPredADFunCurrOk = true;
}


/*************************************************************************
 *
 * Function: evalPredFirstDeriv
 *
 *
 * Evaluates the following first derivatives of the Pred block expressions:
 *
 *     d       f( theta )  ,
 *      theta
 *
 * and
 *                                           |
 *     h( theta )  =  d     y( theta, eta )  |          .
 *                     eta                   | eta = 0
 * 
 *************************************************************************/

void IndPredModel::evalPredFirstDeriv() const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  // Don't reevaluate the first derivatives of the expressions in
  // the Pred block if the current versions are valid.
  if ( isPredFirstDerivCurrOk )
  {
    return;
  }


  //------------------------------------------------------------
  // Prepare to calculate the first derivatives.
  //------------------------------------------------------------

  // Before first derivatives can be calculated, the predicted
  // values for all of the events for the current individual 
  // must be evaluated.
  evalAllPred();

  // Make sure these are the proper sizes.
  f_thetaCurr.resize( nEventCurr * nTheta );
  hCurr      .resize( nEventCurr * nEta );

  // Set the lengths of the Taylor coefficient column vectors.
  std::vector<double> u1( pPredADFunCurr->Domain() );
  std::vector<double> v1( pPredADFunCurr->Range() );


  //------------------------------------------------------------
  // Calculate the first derivatives of the dependent variables.
  //------------------------------------------------------------

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //
  // Note
  // ----
  //
  // This function uses the Pred block automatic differentiation
  // function object to evaluate the first derivatives of f and
  // y at the current values for theta.
  //
  // It does that by setting 
  //
  //                -
  //               |   1, if m = k,
  //     u1     =  <
  //       (m)     |   0, otherwise,
  //                -
  //
  // for each element in u1 in sequence and then evaluating
  //
  //     v1  =  d  [ pred( u0 ) ]  u1
  //             z
  //
  //             (k)
  //         =  d     pred( z )  .
  //             z
  //
  // Note that the value
  //
  //     u0  =  z  
  //
  // is set when the function object is constructed.
  //
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  int j;
  int k;

  // Initialize the first order Taylor coefficients for U.
  for ( k = 0; k < nZ; k++ )
  {
    u1[k] = 0.0;
  }

  // Calculate the first derivative
  //
  //     d       f( theta )  .
  //      theta
  //
  for ( k = 0; k < nTheta; k++ )
  {
    u1[k + thetaOffsetInZ] = 1.0;

    // Evaluate the first order Taylor coefficients for pred(U).
    v1 = pPredADFunCurr->Forward( 1, u1 );

    // Set the elements of the first derivative:
    //
    //      (k)
    //     d       f   ( theta )  .
    //      theta   (j)
    //
    for ( j = 0; j < nEventCurr; j++ )
    {
      f_thetaCurr[j + k * nEventCurr] = v1[j + fOffsetInW];
    }

    u1[k + thetaOffsetInZ] = 0.0;
  }

  // Calculate the first derivative
  //
  //                                           |
  //     h( theta )  =  d     y( theta, eta )  |          .
  //                     eta                   | eta = 0
  //
  for ( k = 0; k < nEta; k++ )
  {
    u1[k + etaOffsetInZ] = 1.0;

    // Evaluate the first order Taylor coefficients for pred(U).
    v1 = pPredADFunCurr->Forward( 1, u1 );

    // Set the elements of the first derivative:
    //
    //                          (k)                      |              
    //     h     ( theta )  =  d     y   ( theta, eta )  |          .
    //      (j,k)               eta   (j)                | eta = 0
    //
    for ( j = 0; j < nEventCurr; j++ )
    {
      hCurr[j + k * nEventCurr] = v1[j + yOffsetInW];
    }

    u1[k + etaOffsetInZ] = 0.0;
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isPredFirstDerivCurrOk = true;
}


/*************************************************************************
 *
 * Function: evalPredSecondDeriv
 *
 *
 * Evaluates the following second derivative of the Pred block expressions:
 *
 *              -                                   - 
 *             |                         |           |
 *     d       |  d     y( theta, eta )  |           |  .
 *      theta  |   eta                   | eta = 0   |
 *              -                                   - 
 *
 * In addition, this function evaluates the following first derivatives
 * of the Pred block expressions:
 *
 *     d       f( theta )  ,
 *      theta
 *
 * and
 *                                           |
 *     h( theta )  =  d     y( theta, eta )  |          .
 *                     eta                   | eta = 0
 * 
 *************************************************************************/

void IndPredModel::evalPredSecondDeriv() const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------


  //------------------------------------------------------------
  // Use the cached values if possible.
  //------------------------------------------------------------

  // Don't reevaluate the second derivatives of the expressions in
  // the Pred block if the current versions are valid.
  if ( isPredSecondDerivCurrOk )
  {
    return;
  }


  //------------------------------------------------------------
  // Prepare to calculate the first and second derivatives.
  //------------------------------------------------------------

  // Before first or second derivatives can be calculated, the
  // predicted values for all of the events for the current 
  // individual must be evaluated.
  evalAllPred();

  // Set the number of rows in the second derivatives that will 
  // be calculated.  Note that before the second derivatives are
  // calculated, the first derivatives are converted to a column
  // vector that contains the derivative's elements in row major
  // order, i.e., an rvec operation is performed on them.
  int nH_thetaRow = nEventCurr * nEta;

  // Make sure these are the proper sizes.
  f_thetaCurr.resize( nEventCurr * nTheta );
  hCurr      .resize( nEventCurr * nEta );
  h_thetaCurr.resize( nEventCurr * nEta * nTheta );

  // Set the lengths of the Taylor coefficient column vectors.
  std::vector<double> u1( pPredADFunCurr->Domain() );
  std::vector<double> u2( pPredADFunCurr->Domain() );
  std::vector<double> v1( pPredADFunCurr->Range() );
  std::vector<double> v2( pPredADFunCurr->Range() );

  // These will hold one-half times the diagonal elements of
  // the second derivatives.
  std::vector<double> y_theta_thetaDiagTerm( nTheta * nEventCurr );
  std::vector<double> y_eta_etaDiagTerm    ( nEta   * nEventCurr );


  //------------------------------------------------------------
  // Calculate the first derivatives and second derivative diagonals.
  //------------------------------------------------------------

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //
  // Note
  // ----
  //
  // This function uses the Pred block automatic differentiation
  // function object to evaluate the first derivatives of f and
  // y at the current values for theta.
  //
  // It does that by setting 
  //
  //                -
  //               |   1, if m = k,
  //     u1     =  <
  //       (m)     |   0, otherwise,
  //                -
  //
  // for each element in u1 in sequence and then evaluating
  //
  //     v1  =  d  [ pred( u0 ) ]  u1
  //             z
  //
  //             (k)
  //         =  d     pred( z )  .
  //             z
  //
  // In addition, this function evaluates the diagonals of the
  // second derivative of y at the current values for theta.
  //
  // It does that by setting 
  //
  //     u2     =   0, for all m,
  //       (m)
  //
  // and then evaluating
  //                    -                                                               -
  //                1  |                                 T                               |
  //     v2     =  --- |  d  [ pred   ( u0 ) ]  u2  +  u1   d  d  [ pred   ( u0 ) ]  u1  |  .
  //       (p)      2  |   z       (p)                       z  z       (p)              |
  //                    -                                                               -
  //
  //                1   (k)  (k)
  //            =  --- d    d    pred   ( u0 )  .
  //                2   z    z       (p)
  //
  // Note that the value
  //
  //     u0  =  z  
  //
  // is set when the function object is constructed.
  //
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  int j;
  int k;
  int p;

  // Initialize the first and second order Taylor coefficients for U.
  for ( k = 0; k < nZ; k++ )
  {
    u1[k] = 0.0;
    u2[k] = 0.0;
  }

  // Calculate the first derivative
  //
  //     d       f( theta )  
  //      theta
  //
  // and one-half times the diagonal elements of the second derivatives,
  //
  //      1   (k)    (k)      
  //     --- d      d       y   ( theta, eta )  , for all k and p.
  //      2   theta  theta   (p)
  //
  for ( k = 0; k < nTheta; k++ )
  {
    u1[k + thetaOffsetInZ] = 1.0;

    // Evaluate the first order Taylor coefficients for pred(U).
    v1 = pPredADFunCurr->Forward( 1, u1 );

    // Set the elements of the first derivative:
    //
    //      (k)
    //     d       f   ( theta )  .
    //      theta   (j)
    //
    for ( j = 0; j < nEventCurr; j++ )
    {
      f_thetaCurr[j + k * nEventCurr] = v1[j + fOffsetInW];
    }

    // Evaluate the second order Taylor coefficients for pred(U).
    v2 = pPredADFunCurr->Forward( 2, u2 );
  
    // Set the elements of one-half times the diagonal elements of
    // the second derivatives:
    //
    //                                      1   (k)    (k)
    //     y_theta_thetaDiagTerm        =  --- d      d       y   ( theta, eta )  .
    //                          (k, p)      2   theta  theta   (p)
    //
    for ( p = 0; p < nEventCurr; p++ )
    {
      y_theta_thetaDiagTerm[k + p * nTheta] = v2[p + yOffsetInW];
    }

    u1[k + thetaOffsetInZ] = 0.0;
  }

  // Calculate the first derivative
  //
  //                                           |
  //     h( theta )  =  d     y( theta, eta )  |
  //                     eta                   | eta = 0
  //
  // and one-half times the diagonal elements of the second derivatives,
  //
  //      1   (k)  (k)    
  //     --- d    d     y   ( theta, eta )  , for all k and p.
  //      2   eta  eta   (p)
  //
  for ( k = 0; k < nEta; k++ )
  {
    u1[k + etaOffsetInZ] = 1.0;

    // Evaluate the first order Taylor coefficients for pred(U).
    v1 = pPredADFunCurr->Forward( 1, u1 );

    // Set the elements of the first derivative:
    //
    //                          (k)                      |
    //     h     ( theta )  =  d     y   ( theta, eta )  |          .
    //      (j,k)               eta   (j)                | eta = 0
    //
    for ( j = 0; j < nEventCurr; j++ )
    {
      hCurr[j + k * nEventCurr] = v1[j + yOffsetInW];
    }

    // Evaluate the second order Taylor coefficients for pred(U).
    v2 = pPredADFunCurr->Forward( 2, u2 );
  
    // Set the elements of one-half times the diagonal elements of
    // the second derivatives:
    //
    //                                  1   (k)  (k)
    //     y_eta_etaDiagTerm        =  --- d    d     y   ( theta, eta )  .
    //                      (k, p)      2   eta  eta   (p)
    //
    for ( p = 0; p < nEventCurr; p++ )
    {
      y_eta_etaDiagTerm[k + p * nEta] = v2[p + yOffsetInW];
    }

    u1[k + etaOffsetInZ] = 0.0;
  }


  //------------------------------------------------------------
  // Calculate some second derivatives of some dependent variables.
  //------------------------------------------------------------

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //
  // Note
  // ----
  //
  // This function uses the Pred block automatic differentiation
  // function object to evaluate some of the second derivatives 
  // of y at the current values for theta.
  //
  // It does that by setting 
  //
  //     u2     =   0, for all m,
  //       (m)
  //
  // and
  //                -
  //               |   1, if m = j,
  //               |
  //     u1     =  <   1, if m = k,
  //       (m)     |
  //               |   0, otherwise,
  //                -
  //
  // for the appropriate pairs of elements in u1 in sequence and
  // then evaluating
  //
  //                    -                                                               -
  //                1  |                                 T                               |
  //     v2     =  --- |  d  [ pred   ( u0 ) ]  u2  +  u1   d  d  [ pred   ( u0 ) ]  u1  |  .
  //       (p)      2  |   z       (p)                       z  z       (p)              |
  //                    -                                                               -
  //
  //                    - 
  //                1  |   (j)  (j)                    (j)  (k)
  //            =  --- |  d    d    pred   ( u0 )  +  d    d    pred   ( u0 )
  //                2  |   z    z       (p)            z    z       (p)
  //                    -
  //
  //                                                                        - 
  //                     (k)  (j)                    (k)  (k)                |
  //                +   d    d    pred   ( u0 )  +  d    d    pred   ( u0 )  |  .
  //                     z    z       (p)            z    z       (p)        |
  //                                                                        -
  //
  //                
  //                 (k)  (j)
  //            =   d    d    pred   ( u0 )
  //                 z    z       (p)
  //               
  //
  //                       -                                                      - 
  //                   1  |    (j)  (j)                    (k)  (k)                |
  //                + --- |   d    d    pred   ( u0 )  +  d    d    pred   ( u0 )  |  .
  //                   2  |    z    z       (p)            z    z       (p)        |
  //                       -                                                      -
  // 
  // This makes use of the fact that
  //    
  //      (j)  (k)                    (k)  (j)
  //     d    d    pred   ( u0 )  =  d    d    pred   ( u0 )  .
  //      z    z       (p)            z    z       (p)
  //
  // Note that the value
  //
  //     u0  =  z  
  //
  // is set when the function object is constructed.
  //
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  int row;
  int col;

  // Calculate the second derivative
  //              -                                   - 
  //             |                         |           |
  //     d       |  d     y( theta, eta )  |           |  .
  //      theta  |   eta                   | eta = 0   |
  //              -                                   - 
  //
  for ( k = 0; k < nTheta; k++ )
  {
    u1[k + thetaOffsetInZ] = 1.0;

    for ( j = 0; j < nEta; j++ )
    {
      u1[j + etaOffsetInZ] = 1.0;
    
      // Evaluate the first and second order Taylor coefficients
      // for pred(U).
      v1 = pPredADFunCurr->Forward( 1, u1 );
      v2 = pPredADFunCurr->Forward( 2, u2 );

      // Set the elements of the second derivative:
      //
      //                -                                      -
      //      (k)      |   (j)                      |           |
      //     d         |  d     y   ( theta, eta )  |           |
      //      theta    |   eta   (p)                | eta = 0   |
      //                -                                      -
      //
      //              -                                                       -
      //             |                 -                                   -   |
      //             |                |                         |           |  |
      //          =  |  d       rvec  |  d     y( theta, eta )  |           |  |                 .
      //             |   theta        |   eta                   | eta = 0   |  |
      //             |                 -                                   -   |
      //              -                                                       -  (p*nEta + j, k)
      //
      // Note that an rvec operation is performed on the first
      // derivatives before the second derivatives are calculated.
      for ( p = 0; p < nEventCurr; p++ )
      {
        // Set the position of this element in the matrix of 
        // second derivatives
        row = p * nEta + j;
        col = k;

        h_thetaCurr[row + col * nH_thetaRow] = v2[p + yOffsetInW]
          - y_theta_thetaDiagTerm[k + p * nTheta] 
          - y_eta_etaDiagTerm[j + p * nEta];
      }

      u1[j + etaOffsetInZ] = 0.0;
    }

    u1[k + thetaOffsetInZ] = 0.0;
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isPredFirstDerivCurrOk  = true;
  isPredSecondDerivCurrOk = true;
}


/*************************************************************************
 *
 * Function: doDataMean
 *
 *
 * Sets ret equal to the current value for the mean of the current
 * individual's data
 *
 *     f ( b  )  ,
 *      i   i
 *
 * where
 *
 *     i  = index for the current individual and
 *
 *     b  = current value for the individual parameter.
 *      i
 *
 *************************************************************************/

void IndPredModel::doDataMean( valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nRow = nEventCurr;
  int nCol = 1;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - Dummy Values for Integration Testing - Mitch]
  // For the purposes of integration testing, this function 
  // currently returns a dummy value that has the correct
  // dimensions but was not calculated using the Pred block.
  ret = 1.0;
  return;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if ( isDataMeanCurrOk )
  {
    ret = dataMeanCurr;
    return;
  }

  dataMeanCurr.resize( nRow * nCol );


  //------------------------------------------------------------
  // Prepare to calculate the value.
  //------------------------------------------------------------

  // Evaluate the predicted values for all of the events 
  // for the current individual.
  evalAllPred();


  //------------------------------------------------------------
  // Calculate the value.
  //------------------------------------------------------------

  int j;

  // Set the values for the mean of the data:
  //
  //     f    ( b  )  =  f   ( theta )  .
  //      i(j)   i        (j)
  //
  for ( j = 0; j < nEventCurr; j++ )
  {
    dataMeanCurr[j] = Value( wCurr[j + fOffsetInW] );
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isDataMeanCurrOk = true;
  ret = dataMeanCurr;
}


/*************************************************************************
 *
 * Function: doDataMean_indPar
 *
 *
 * Sets ret equal to the current value for the derivative with respect
 * to the individual parameter of the mean of the current individual's
 * data,
 *
 *     d   f ( b  )  ,
 *      b   i   i
 *
 * where
 *
 *     i  = index for the current individual and
 *
 *     b  = current value for the individual parameter.
 *      i
 *
 * This function returns a value of true if this derivative has 
 * at least one nonzero element.
 *
 *************************************************************************/

bool IndPredModel::doDataMean_indPar( valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nRow = nEventCurr;
  int nCol = nIndPar;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - Dummy Values for Integration Testing - Mitch]
  // For the purposes of integration testing, this function 
  // currently returns a dummy value that has the correct
  // dimensions but was not calculated using the Pred block.
  ret = 0.0;
  return false;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if ( isDataMean_indParCurrOk )
  {
    ret = dataMean_indParCurr;

    // Return a value of true if this derivative has at least one
    // nonzero element.
    return !allZero( dataMean_indParCurr );
  }

  dataMean_indParCurr.resize( nRow * nCol );


  //------------------------------------------------------------
  // Prepare to calculate the value.
  //------------------------------------------------------------

  // Evaluate the first derivatives of the Pred block expressions.
  evalPredFirstDeriv();


  //------------------------------------------------------------
  // Calculate the value.
  //------------------------------------------------------------

  int col;
  int j;
  int k;

  // Set all of the elements of the derivative of the data 
  // mean equal to zero.
  dataMean_indParCurr = 0.0;

  // Set the values for the partial derivatives of the elements
  // of the data mean:
  //
  //      (k)
  //     d     f    ( b  )  .
  //      b     i(j)   i
  //
  for ( j = 0; j < nEventCurr; j++ )
  {
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    //
    // Note
    // ----
    //
    // Since the individual parameter is
    //
    //             -          -
    //            |   theta    |
    //     b   =  |            |  ,
    //            |  omegaPar  |
    //             -          -
    //
    // and since the data mean does not depend on omegaPar,
    // the derivative of this element is
    //
    //     d   f    ( b  )
    //      b   i(j)   i
    //
    //             -                                      -
    //            |                                        |
    //         =  |  d       f    ( b  ) ,  0, 0, ... , 0  |  .
    //            |   theta   i(j)   i                     |
    //             -                                      - 
    //
    // where there are nOmegaPar zeroes.
    //
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    // Set the partial derivatives that depend on theta:
    //
    //      (k)                      
    //     d       f    ( b  )  .
    //      theta   i(j)   i       
    //
    for ( k = 0; k < nTheta; k++ )
    {
      col = k + thetaOffsetInIndPar;

      dataMean_indParCurr[j + col * nRow] = f_thetaCurr[j + col * nRow];
    }

  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isDataMean_indParCurrOk = true;
  ret = dataMean_indParCurr;

  // Return a value of true if this derivative has at least one
  // nonzero element.
  return !allZero( dataMean_indParCurr );
}


/*************************************************************************
 *
 * Function: doDataVariance
 *
 *
 * Sets ret equal to the current value for the variance of the current
 * individual's data,
 *
 *     R ( b  )  ,
 *      i   i
 *
 * where
 *
 *     i  = index for the current individual and
 *
 *     b  = current value for the individual parameter.
 *      i
 *
 *************************************************************************/

void IndPredModel::doDataVariance( valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nRow = nEventCurr;
  int nCol = nEventCurr;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - Dummy Values for Integration Testing - Mitch]
  // For the purposes of integration testing, this function 
  // currently returns a dummy value that has the correct
  // dimensions but was not calculated using the Pred block.
  ret = 1.0;
  return;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if ( isDataVarianceCurrOk )
  {
    ret = dataVarianceCurr;
    return;
  }

  dataVarianceCurr.resize( nRow * nCol );


  //------------------------------------------------------------
  // Prepare to calculate the value.
  //------------------------------------------------------------

  // Evaluate the first derivatives of the Pred block expressions.
  evalPredFirstDeriv();

  // Save the current value for omega.
  pOmegaCurr->cov( omegaCurr );


  //------------------------------------------------------------
  // Calculate the value.
  //------------------------------------------------------------

  int j;
  int m;
  int n;

  // Set all of the elements of the data variance equal to zero.
  dataVarianceCurr = 0.0;

  // Set the values for the diagonal elements of the data variance:
  //
  //                       ----
  //     R      ( b  )  =  \      h     ( theta )  omega       h     ( theta )  .
  //      i(j,j)   i       /       (m,j)                (m,n)   (n,j)
  //                       ----
  //                        m,n
  //
  for ( j = 0; j < nEventCurr; j++ )
  {
    for ( m = 0; m < nEta; m++ )
    {
      for ( n = 0; n < nEta; n++ )
      {
        dataVarianceCurr[j + j * nEventCurr] += 
          hCurr[m + j * nEventCurr] * omegaCurr[m + n * nEta] *
          hCurr[n + j * nEventCurr];
      }
    }
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isDataVarianceCurrOk = true;
  ret = dataVarianceCurr;
}


/*************************************************************************
 *
 * Function: doDataVariance_indPar
 *
 *
 * Sets ret equal to the current value for the derivative with 
 * respect to the individual parameter of the variance of the
 * current individual's data,
 *                                 -           -
 *                                |             |
 *     d   R ( b  )  =  d   rvec  |   R ( b  )  |.
 *      b   i   i        b        |    i   i    |
 *                                 -           -
 *
 * where
 *
 *     i  = index for the current individual and
 *
 *     b  = current value for the individual parameter.
 *      i
 *
 * This function returns a value of true if this derivative has 
 * at least one nonzero element.
 *
 *************************************************************************/

bool IndPredModel::doDataVariance_indPar( valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nRow = nEventCurr * nEventCurr;
  int nCol = nIndPar;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - Dummy Values for Integration Testing - Mitch]
  // For the purposes of integration testing, this function 
  // currently returns a dummy value that has the correct
  // dimensions but was not calculated using the Pred block.
  ret = 0.0;
  return false;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if ( isDataVariance_indParCurrOk )
  {
    ret = dataVariance_indParCurr;

    // Return a value of true if this derivative has at least one
    // nonzero element.
    return !allZero( dataVariance_indParCurr );
  }

  dataVariance_indParCurr.resize( nRow * nCol );


  //------------------------------------------------------------
  // Prepare to calculate the value.
  //------------------------------------------------------------

  // Evaluate the second derivatives of the Pred block expressions.
  evalPredSecondDeriv();

  // Save the current value for omega and its derivative.
  pOmegaCurr->cov( omegaCurr );
  pOmegaCurr->cov( omega_omegaParCurr );

  // These will hold the columns of h that correspond to each event.
  valarray<double> hCol      ( nEta );
  valarray<double> hColTrans ( nEta );
  valarray<double> hCol_theta( nEta * nTheta );

  // Set the number of rows in the derivative of h.
  int nH_thetaRow = nEventCurr * nEta;

  // These are temporary variables used below.
  valarray<double> hColTransTimesOmega      ( nEta );
  valarray<double> dataVarianceDiag_theta   ( nTheta );
  valarray<double> dataVarianceDiag_omegaPar( nOmegaPar );


  //------------------------------------------------------------
  // Calculate the value.
  //------------------------------------------------------------

  int h_thetaRow;
  int h_thetaCol;
  int row;
  int col;
  int j;
  int k;
  int m;
  int n;

  // Set all of the elements of the derivative of the data 
  // variance equal to zero.
  dataVariance_indParCurr = 0.0;

  // Set the values for the partial derivatives of the diagonal
  // elements of the data variance:
  //
  //      (k)
  //     d     R      ( b  )  .
  //      b     i(j,j)   i
  //
  for ( j = 0; j < nEventCurr; j++ )
  {
    // Set the row for this element in the rvec version of R.
    row = j * nEventCurr + j;

    // Get the column of h that corresponds to this event, 
    //
    //               -                       -
    //              |     h      ( theta )    |
    //              |      (0, j)             |
    //              |          .              |
    //     hCol  =  |          .              |  ,
    //              |          .              |
    //              |          .              |
    //              |  h           ( theta )  |
    //              |   (nEta-1, j)           |
    //               -                       -
    //
    // and its transpose and derivative.
    for ( m = 0; m < nEta; m++ )
    {
      // Get this column and its transpose.
      hCol[m]      = hCurr[j + m * nEta];
      hColTrans[m] = hCol[m];

      // Get the derivative with respect to theta of this column.
      for ( k = 0; k < nTheta; k++ )
      {
        h_thetaRow = j * nEta + m;
        h_thetaCol = k;

        hCol_theta[m + k * nEta ] = 
          h_thetaCurr[h_thetaRow + h_thetaCol * nH_thetaRow];
      }
    }
    

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    //
    // Note
    // ----
    //
    // Since the individual parameter is
    //
    //             -          -
    //            |   theta    |
    //     b   =  |            |  ,
    //            |  omegaPar  |
    //             -          -
    //
    // and since this diagonal element of the data variance is 
    // equal to
    //
    //                           T             
    //     R      ( b  )  =  hCol ( theta )  omega( omegaPar )  hCol( theta )  ,
    //      i(j,j)   i
    //
    // the derivative of this diagonal element is
    //
    //     d   R      ( b  )
    //      b   i(j,j)   i
    //
    //             -                                                    -
    //            |                                                      |  
    //         =  |  d       R      ( b  )  ,  d          R      ( b  )  |  .
    //            |   theta   i(j,j)   i        omegaPar   i(j,j)   i    |  
    //             -                                                    - 
    //
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    // Calculate
    //
    //         T
    //     hCol   omega  d       hCol  .
    //                    theta
    //
    hColTransTimesOmega = multiply(
      hColTrans,
      nEta,
      omegaCurr,
      nEta );
    dataVarianceDiag_theta = multiply( 
      hColTransTimesOmega,
      nEta,
      hCol_theta,
      nTheta );

    // Set the partial derivatives that depend on theta:
    //
    //                                -                               -
    //      (k)                      |         T                       |
    //     d       R      ( b  )  =  |  2  hCol   omega  d       hCol  |     .
    //      theta   i(j,j)   i       |                    theta        |
    //                                -                               - (k) 
    //
    for ( k = 0; k < nTheta; k++ )
    {
      col = k + thetaOffsetInIndPar;

      dataVariance_indParCurr[row + col * nRow] =
        2.0 * dataVarianceDiag_theta[k];
    }

    // Calculate
    //
    //           T            T
    //     ( hCol   kron  hCol  )  d        omega  .
    //                              omegaPar
    //
    dataVarianceDiag_omegaPar = AkronBtimesC( 
      hColTrans,
      nEta,
      hColTrans,
      nEta,
      omega_omegaParCurr,
      nOmegaPar );

    // Set the partial derivatives that depend on omegaPar:
    //
    //                                   -                                        -
    //      (k)                         |        T            T                    |
    //     d          R      ( b  )  =  |  ( hCol   kron  hCol  )  d        omega  |     .
    //      omegaPar   i(j,j)   i       |                           omegaPar       |
    //                                   -                                        - (k) 
    //
    // where kron represents the Kronecker product operator.
    for ( k = 0; k < nOmegaPar; k++ )
    {
      col = k + omegaParOffsetInIndPar;

      dataVariance_indParCurr[row + col * nRow] =
        dataVarianceDiag_omegaPar[k];
    }

  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isDataVariance_indParCurrOk = true;
  ret = dataVariance_indParCurr;

  // Return a value of true if this derivative has at least one
  // nonzero element.
  return !allZero( dataVariance_indParCurr );
}


/*************************************************************************
 *
 * Function: doDataVarianceInv
 *
 *
 * Sets ret equal to the current value for the inverse of the variance
 * of the current individual's data,
 *
 *      -1
 *     R  ( b  )  ,
 *      i    i
 *
 * where
 *
 *     i  = index for the current individual and
 *
 *     b  = current value for the individual parameter.
 *      i
 *
 *************************************************************************/

void IndPredModel::doDataVarianceInv( valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nRow = nEventCurr;
  int nCol = nEventCurr;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - Dummy Values for Integration Testing - Mitch]
  // For the purposes of integration testing, this function 
  // currently returns a dummy value that has the correct
  // dimensions but was not calculated using the Pred block.
  ret = 1.0;
  return;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if ( isDataVarianceInvCurrOk )
  {
    ret = dataVarianceInvCurr;
    return;
  }

  dataVarianceInvCurr.resize( nRow * nCol );


  //------------------------------------------------------------
  // Calculate the value.
  //------------------------------------------------------------

  int j;

  // Set the inverse equal to the data variance itself since their
  // off-diagonal elements are both all zeroes.
  doDataVariance( dataVarianceInvCurr );

  // Set the values for the diagonal elements of the inverse of
  // the data variance:
  //
  //                             1
  //      -1
  //     R      ( b  )  =  ---------------  .
  //      i(j,j)   i       
  //                        R      ( b  )
  //                         i(j,j)   i
  //
  for ( j = 0; j < nEventCurr; j++ )
  {
    assert( dataVarianceInvCurr[j + j * nEventCurr] != 0.0 );

    dataVarianceInvCurr[j + j * nEventCurr] = 
      1.0 / dataVarianceInvCurr[j + j * nEventCurr];
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isDataVarianceInvCurrOk = true;
  ret = dataVarianceInvCurr;
}


/*************************************************************************
 *
 * Function: doDataVarianceInv_indPar
 *
 *
 * Sets ret equal to the current value for the derivative with respect
 * to the individual parameter of the inverse of the variance of the
 * current individual's data,
 *                                  -            -
 *          -1                     |    -1        |
 *     d   R  ( b  )  =  d   rvec  |   R  ( b  )  |.
 *      b   i    i        b        |    i    i    |
 *                                  -            -
 *
 * where
 *
 *     i  = index for the current individual and
 *
 *     b  = current value for the individual parameter.
 *      i
 *
 * This function returns a value of true if this derivative has 
 * at least one nonzero element.
 *
 *************************************************************************/

bool IndPredModel::doDataVarianceInv_indPar( valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nRow = nEventCurr * nEventCurr;
  int nCol = nIndPar;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - Dummy Values for Integration Testing - Mitch]
  // For the purposes of integration testing, this function 
  // currently returns a dummy value that has the correct
  // dimensions but was not calculated using the Pred block.
  ret = 0.0;
  return false;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if ( isDataVarianceInv_indParCurrOk )
  {
    ret = dataVarianceInv_indParCurr;

    // Return a value of true if this derivative has at least one
    // nonzero element.
    return !allZero( dataVarianceInv_indParCurr );
  }

  dataVarianceInv_indParCurr.resize( nRow * nCol );


  //------------------------------------------------------------
  // Prepare to calculate the value.
  //------------------------------------------------------------

  // Calculate the current value for the data variance if it has
  // not been calculated already.
  if ( isDataVarianceCurrOk )
  {
    valarray<double> dataVarianceTemp( nEventCurr * nEventCurr );
    doDataVariance( dataVarianceTemp );
  }


  //------------------------------------------------------------
  // Calculate the value.
  //------------------------------------------------------------

  int row;
  int col;
  int j;
  int k;

  // Set the derivative of the inverse equal to the derivative of
  // the data variance itself since they have zero valued elements
  // in the same places.
  doDataVariance_indPar( dataVarianceInv_indParCurr );

  // Set the values for the partial derivatives of the diagonal
  // elements of the inverse of the data variance:
  //
  //                                  (k)
  //                              -  d     R      ( b  )
  //                                  b     i(j,j)   i  
  //      (k)   -1
  //     d     R      ( b  )  =  ------------------------  .
  //      b     i(j,j)   i       
  //                                   R      ( b  )
  //                                    i(j,j)   i
  //
  for ( j = 0; j < nEventCurr; j++ )
  {
    // Set the row for this element in the rvec version of the 
    // inverse of R.
    row = j * nEventCurr + j;

    // Set the partial derivatives.
    for ( k = 0; k < nIndPar; k++ )
    {
      assert( dataVarianceCurr[j + j * nEventCurr] != 0.0 );

      col = k;

      dataVarianceInv_indParCurr[row + col * nRow] = 
        - dataVariance_indParCurr[row + col * nRow]
        / dataVarianceCurr[j + j * nEventCurr];
    }
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isDataVarianceInv_indParCurrOk = true;
  ret = dataVarianceInv_indParCurr;

  // Return a value of true if this derivative has at least one
  // nonzero element.
  return !allZero( dataVarianceInv_indParCurr );
}


/*************************************************************************
 *
 * Function: getIndParLimits
 *
 *
 * Gets the lower and upper limits for the elements of the individual
 * parameter at the current individual parameter value.
 *
 * These limits can be used during the optimization of objective
 * functions that depend on these parameters.
 *
 * This function assumes that the current value for the individual
 * parameter is approximately equal to its final or true value.
 *
 *************************************************************************/

void IndPredModel::getIndParLimits(
  valarray<double>&  indParLow,
  valarray<double>&  indParUp ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  indParLow.resize( nIndPar );
  indParUp .resize( nIndPar );


  //------------------------------------------------------------
  // Set limits for the parameters that appear in the Pred block.
  //------------------------------------------------------------

  int k;

  // Set the limits for the elements of theta.
  for ( k = 0; k < nTheta; k++ )
  {
    indParLow[k + thetaOffsetInIndPar] = thetaLow[k];
    indParUp [k + thetaOffsetInIndPar] = thetaUp [k];
  }


  //------------------------------------------------------------
  // Set limits for the covariance matrix parameters.
  //------------------------------------------------------------

  // Get the current limits from the covariance matrix.
  valarray<double> omegaParLow( nOmegaPar );
  valarray<double> omegaParUp ( nOmegaPar );
  pOmegaCurr->getParLimits( omegaParLow, omegaParUp );

  // Set the limits for the parameters for the omega covariance matrix.
  for ( k = 0; k < nOmegaPar; k++ )
  {
    indParLow[k + omegaParOffsetInIndPar] = omegaParLow[k];
    indParUp [k + omegaParOffsetInIndPar] = omegaParUp [k];
  }

}


/*************************************************************************
 *
 * Function: getIndParStep
 *
 *
 * Gets a vector of step sizes for approximating derivatives with 
 * respect to the elements of the individual parameter.
 *
 * These step sizes can be used to approximate the derivatives of
 * objective functions that depend on the individual parameters.
 *
 *************************************************************************/

void IndPredModel::getIndParStep( valarray<double>&  indParStep ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  indParStep.resize( nIndPar );


  //------------------------------------------------------------
  // Set the step sizes.
  //------------------------------------------------------------

  int k;

  // Get the current limits for the individual parameter.
  valarray<double> indParLow( nIndPar );
  valarray<double> indParUp ( nIndPar );
  getIndParLimits( indParLow, indParUp );

  // Set the step sizes for the individual parameter.
  for ( k = 0; k < nIndPar; k++ )
  {
    indParStep[k] = ( indParUp[k] - indParLow[k] ) / 1000.0;
  }

}


/*************************************************************************
 *
 * Function: getNIndPar
 *
 *
 * Returns the number of elements in the individual parameter.
 *
 *************************************************************************/

int IndPredModel::getNIndPar() const
{
  return nIndPar;
}


/*=========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 =========================================================================*/

namespace // [Begin: unnamed namespace]
{

/*************************************************************************
 *
 * Function: intToOrderString
 *
 *
 * Returns a string that corresponds to the order of the integer i.
 *
 *************************************************************************/

string intToOrderString( int i )
{
  std::ostringstream order;

  switch( i )
  {
  case 1:
    order << "1st";
  case 2:
    order << "2nd";
  case 3:
    order << "3rd";
  default:
    order << i << "th";
  }

    return order.str();
}


} // [End: unnamed namespace]
