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
 * @file: IndPredModel.cpp
 *
 *
 * Implements IndPredModel class.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "DiagCov.h"
#include "FullCov.h"
#include "BlkDiagCov.h"
#include "IndPredModel.h"
#include "isEqual.h"
#include "PredBase.h"

// SPK library header files.
#include <spk/AkronBtimesC.h>
#include <spk/allZero.h>
#include <spk/intToOrdinalString.h>
#include <spk/isNotANumber.h>
#include <spk/isUnnormNumber.h>
#include <spk/multiply.h>
#include <spk/SpkException.h>
#include <spk/SpkModel.h>
#include <spk/SpkValarray.h>

// CppAD header files.
#include <CppAD/CppAD.h>
//#include <CppAD/include/Independent.h>

// Standard library header files.
#include <cassert>
#include <limits>
#include <sstream>
#include <vector>

using namespace CppAD;
using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: IndPredModel
 *
 *//**
 * Constructor for individual level Pred models.
 *
 * After this constructor has completed the current individual parameter
 * will be 
 * \f[
 *     b_i =
 *       \left[ 
 *         \begin{array}{c}
 *           \mbox{thetaCurrIn} \\
 *           \mbox{omegaParCurr}
 *         \end{array}
 *       \right] ,
 * \f]
 * where omegaParCurr is the covariance matrix parameter that 
 * corresponds to the minimal representation for omega that is
 * contained in omegaMinRepIn.
 */
/*************************************************************************/
//Constructor (original)
IndPredModel::IndPredModel(
    PredBase< AD<double> >&          predEvaluatorIn,
    int                              nThetaIn,
    const SPK_VA::valarray<double>&  thetaLowIn,
    const SPK_VA::valarray<double>&  thetaUpIn,
    const SPK_VA::valarray<double>&  thetaCurrIn,
    int                              nEtaIn,
    covStruct                        omegaStructIn,
    const SPK_VA::valarray<double>&  omegaMinRepIn )
  :
  nTheta                            ( nThetaIn ),
  nEta                              ( nEtaIn ),
  thetaOffsetInIndPar               ( 0 ),
  omegaParOffsetInIndPar            ( nThetaIn ),
  nZ                                ( nThetaIn + nEtaIn ),
  thetaOffsetInZ                    ( 0 ),
  etaOffsetInZ                      ( nThetaIn ),
  fOffsetInW                        ( 0 ),
  pOmegaCurr                        ( 0 ),
  pPredADFunCurr                    ( 0 ),
  predEvaluator                     ( predEvaluatorIn ),
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
  usedCachedPredADFun               ( false ),
  usedCachedPredFirstDeriv          ( false ),
  usedCachedPredSecondDeriv         ( false )
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
  else if ( omegaStructIn == FULL )
  {
    pOmegaCurr = new FullCov( nEta );
  }
  else
  {
    throw SpkException(
     SpkError::SPK_USER_INPUT_ERR, 
     "Unknown covariance matrix type requested for Omega.",
     __LINE__, 
     __FILE__ );
  }

  try
  {
    // Get the number of parameters required by the structure of
    // this covariance matrix.
    nOmegaPar = pOmegaCurr->getNPar();
    
    // Save the omega value maintained by this class.
    omegaCurr.resize( nEta * nEta );
    pOmegaCurr->expandCovMinRep( omegaMinRepIn, omegaCurr );
    assert( omegaMinRepIn.size() == nOmegaPar );
    
    // Set the omega value maintained by the covariance class.
    pOmegaCurr->setCov( omegaCurr );
    
    // Save the initial value for the omega parameters.
    omegaParCurr.resize( nOmegaPar);
    pOmegaCurr->calcPar( omegaCurr, omegaParCurr );
  }
  catch( SpkException& e )
  {
    throw e.push(
      SpkError::SPK_UNKNOWN_ERR, 
      "The initialization of the Omega covariance matrix failed.",
      __LINE__, 
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    throw SpkException(
      stde,
      "An standard exception was thrown during the initialization of the Omega covariance matrix.",
      __LINE__, 
      __FILE__ );
  }  
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR, 
      "An unknown exception was thrown during the initialization of the Omega covariance matrix.",
      __LINE__, 
      __FILE__ );
  }


  //------------------------------------------------------------
  // Initialize quantities related to the Pred block parameters.
  //------------------------------------------------------------

  // Set the current value for theta.
  thetaCurr.resize( nTheta );
  thetaCurr = thetaCurrIn;
  assert( thetaCurrIn.size() == nTheta );

  // Set the lower limit for theta.
  thetaLow.resize( nTheta );
  thetaLow = thetaLowIn;
  assert( thetaLowIn.size() == nTheta );

  // Set the upper limit for theta.
  thetaUp.resize( nTheta );
  thetaUp = thetaUpIn;
  assert( thetaUpIn.size() == nTheta );

  // For individual level Pred models, eta is always equal to zero.
  etaCurr.resize( nEta );
  etaCurr = 0.0;


  //------------------------------------------------------------
  // Initialize quantities related to the individual.
  //------------------------------------------------------------

  // Set the current individual to a number different than zero
  // so that selectIndividual() won't use the cached values.
  iCurr = 1;

  // Set the current individual.
  doSelectIndividual( 0 );


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
  bCurr.resize( nIndPar );

  int k;

  // Set the elements that correspond to theta.
  for ( k = 0; k < nTheta; k++ )
  {
    bCurr[k + thetaOffsetInIndPar] = thetaCurr[k];
  }

  // Set the parameters for the omega covariance matrix.
  for ( k = 0; k < nOmegaPar; k++ )
  {
    bCurr[k + omegaParOffsetInIndPar] = omegaParCurr[k];
  }

}

//Constructor (with inputs for FIXed elements)
//[Revist - get rid of 2nd constructor - Dave]
IndPredModel::IndPredModel(
    PredBase< AD<double> >&          predEvaluatorIn,
    int                              nThetaIn,
    const SPK_VA::valarray<double>&  thetaLowIn,
    const SPK_VA::valarray<double>&  thetaUpIn,
    const SPK_VA::valarray<double>&  thetaCurrIn,
    int                              nEtaIn,
    covStruct                        omegaStructIn,
    const SPK_VA::valarray<double>&  omegaMinRepIn,
    const SPK_VA::valarray<bool>&    omegaMinRepFixedIn )
  :
  nTheta                            ( nThetaIn ),
  nEta                              ( nEtaIn ),
  thetaOffsetInIndPar               ( 0 ),
  omegaParOffsetInIndPar            ( nThetaIn ),
  nZ                                ( nThetaIn + nEtaIn ),
  thetaOffsetInZ                    ( 0 ),
  etaOffsetInZ                      ( nThetaIn ),
  fOffsetInW                        ( 0 ),
  pOmegaCurr                        ( 0 ),
  pPredADFunCurr                    ( 0 ),
  predEvaluator                     ( predEvaluatorIn ),
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
  usedCachedPredADFun               ( false ),
  usedCachedPredFirstDeriv          ( false ),
  usedCachedPredSecondDeriv         ( false )
{
  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrix.
  //------------------------------------------------------------

  // Construct omega, the covariance matrix for eta, with the
  // appropriate structure.
  if ( omegaStructIn == DIAGONAL )
  {
    pOmegaCurr = new DiagCov( nEta, omegaMinRepFixedIn );
  }
  else if ( omegaStructIn == FULL )
  {
    pOmegaCurr = new FullCov( nEta, omegaMinRepFixedIn );
  }
  else
  {
    throw SpkException(
     SpkError::SPK_USER_INPUT_ERR, 
     "Unknown covariance matrix type requested for Omega.",
     __LINE__, 
     __FILE__ );
  }

  try
  {
    // Get the number of parameters required by the structure of
    // this covariance matrix.
    nOmegaPar = pOmegaCurr->getNPar();
    
    // Save the omega value maintained by this class.
    omegaCurr.resize( nEta * nEta );
    pOmegaCurr->expandCovMinRep( omegaMinRepIn, omegaCurr );
    assert( omegaMinRepIn.size() == nOmegaPar );
    
    // Set the omega value maintained by the covariance class.
    pOmegaCurr->setCov( omegaCurr );
    
    // Save the initial value for the omega parameters.
    omegaParCurr.resize( nOmegaPar);
    pOmegaCurr->calcPar( omegaCurr, omegaParCurr );
  }
  catch( SpkException& e )
  {
    throw e.push(
      SpkError::SPK_UNKNOWN_ERR, 
      "The initialization of the Omega covariance matrix failed.",
      __LINE__, 
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    throw SpkException(
      stde,
      "An standard exception was thrown during the initialization of the Omega covariance matrix.",
      __LINE__, 
      __FILE__ );
  }  
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR, 
      "An unknown exception was thrown during the initialization of the Omega covariance matrix.",
      __LINE__, 
      __FILE__ );
  }


  //------------------------------------------------------------
  // Initialize quantities related to the Pred block parameters.
  //------------------------------------------------------------

  // Set the current value for theta.
  thetaCurr.resize( nTheta );
  thetaCurr = thetaCurrIn;
  assert( thetaCurrIn.size() == nTheta );

  // Set the lower limit for theta.
  thetaLow.resize( nTheta );
  thetaLow = thetaLowIn;
  assert( thetaLowIn.size() == nTheta );

  // Set the upper limit for theta.
  thetaUp.resize( nTheta );
  thetaUp = thetaUpIn;
  assert( thetaUpIn.size() == nTheta );

  // For individual level Pred models, eta is always equal to zero.
  etaCurr.resize( nEta );
  etaCurr = 0.0;


  //------------------------------------------------------------
  // Initialize quantities related to the individual.
  //------------------------------------------------------------

  // Set the current individual to a number different than zero
  // so that selectIndividual() won't use the cached values.
  iCurr = 1;

  // Set the current individual.
  doSelectIndividual( 0 );


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
  bCurr.resize( nIndPar );

  int k;

  // Set the elements that correspond to theta.
  for ( k = 0; k < nTheta; k++ )
  {
    bCurr[k + thetaOffsetInIndPar] = thetaCurr[k];
  }

  // Set the parameters for the omega covariance matrix.
  for ( k = 0; k < nOmegaPar; k++ )
  {
    bCurr[k + omegaParOffsetInIndPar] = omegaParCurr[k];
  }

}

//Constructor (with inputs for block structure)
IndPredModel::IndPredModel(
    PredBase< AD<double> >&          predEvaluatorIn,
    int                              nThetaIn,
    const SPK_VA::valarray<double>&  thetaLowIn,
    const SPK_VA::valarray<double>&  thetaUpIn,
    const SPK_VA::valarray<double>&  thetaCurrIn,
    int                              nEtaIn,
    covStruct                        omegaStructIn,
    const SPK_VA::valarray<double>&  omegaMinRepIn,
    const SPK_VA::valarray<bool>&    omegaMinRepFixedIn,
    const SPK_VA::valarray<covStruct>&  omegaBlockStruct,
    const SPK_VA::valarray<int>&     omegaBlockDims,
    const SPK_VA::valarray<bool>&    omegaBlockSameAsPrev )
  :
  nTheta                            ( nThetaIn ),
  nEta                              ( nEtaIn ),
  thetaOffsetInIndPar               ( 0 ),
  omegaParOffsetInIndPar            ( nThetaIn ),
  nZ                                ( nThetaIn + nEtaIn ),
  thetaOffsetInZ                    ( 0 ),
  etaOffsetInZ                      ( nThetaIn ),
  fOffsetInW                        ( 0 ),
  pOmegaCurr                        ( 0 ),
  pPredADFunCurr                    ( 0 ),
  predEvaluator                     ( predEvaluatorIn ),
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
  usedCachedPredADFun               ( false ),
  usedCachedPredFirstDeriv          ( false ),
  usedCachedPredSecondDeriv         ( false )
{
  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrix.
  //------------------------------------------------------------

  // Construct omega, the covariance matrix for eta, with the
  // appropriate structure.
  if ( omegaStructIn == DIAGONAL )
  {
    pOmegaCurr = new DiagCov( nEta, omegaMinRepFixedIn );
  }
  else if ( omegaStructIn == FULL )
  {
    pOmegaCurr = new FullCov( nEta, omegaMinRepFixedIn );
  }
  else if ( omegaStructIn == BLOCKDIAG )
  {
    pOmegaCurr = new BlkDiagCov( nEta,
				 omegaMinRepFixedIn,
				 omegaBlockStruct,
				 omegaBlockDims,
				 omegaBlockSameAsPrev );
  }
  else
  {
    throw SpkException(
     SpkError::SPK_USER_INPUT_ERR, 
     "Unknown covariance matrix type requested for Omega.",
     __LINE__, 
     __FILE__ );
  }

  try
  {
    // Get the number of parameters required by the structure of
    // this covariance matrix.
    nOmegaPar = pOmegaCurr->getNPar();
    
    // Save the omega value maintained by this class.
    omegaCurr.resize( nEta * nEta );
    pOmegaCurr->expandCovMinRep( omegaMinRepIn, omegaCurr );
    assert( omegaMinRepIn.size() == nOmegaPar );
    
    // Set the omega value maintained by the covariance class.
    pOmegaCurr->setCov( omegaCurr );
    
    // Save the initial value for the omega parameters.
    omegaParCurr.resize( nOmegaPar);
    pOmegaCurr->calcPar( omegaCurr, omegaParCurr );
  }
  catch( SpkException& e )
  {
    throw e.push(
      SpkError::SPK_UNKNOWN_ERR, 
      "The initialization of the Omega covariance matrix failed.",
      __LINE__, 
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    throw SpkException(
      stde,
      "An standard exception was thrown during the initialization of the Omega covariance matrix.",
      __LINE__, 
      __FILE__ );
  }  
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR, 
      "An unknown exception was thrown during the initialization of the Omega covariance matrix.",
      __LINE__, 
      __FILE__ );
  }


  //------------------------------------------------------------
  // Initialize quantities related to the Pred block parameters.
  //------------------------------------------------------------

  // Set the current value for theta.
  thetaCurr.resize( nTheta );
  thetaCurr = thetaCurrIn;
  assert( thetaCurrIn.size() == nTheta );

  // Set the lower limit for theta.
  thetaLow.resize( nTheta );
  thetaLow = thetaLowIn;
  assert( thetaLowIn.size() == nTheta );

  // Set the upper limit for theta.
  thetaUp.resize( nTheta );
  thetaUp = thetaUpIn;
  assert( thetaUpIn.size() == nTheta );

  // For individual level Pred models, eta is always equal to zero.
  etaCurr.resize( nEta );
  etaCurr = 0.0;


  //------------------------------------------------------------
  // Initialize quantities related to the individual.
  //------------------------------------------------------------

  // Set the current individual to a number different than zero
  // so that selectIndividual() won't use the cached values.
  iCurr = 1;

  // Set the current individual.
  doSelectIndividual( 0 );


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
  bCurr.resize( nIndPar );

  int k;

  // Set the elements that correspond to theta.
  for ( k = 0; k < nTheta; k++ )
  {
    bCurr[k + thetaOffsetInIndPar] = thetaCurr[k];
  }

  // Set the parameters for the omega covariance matrix.
  for ( k = 0; k < nOmegaPar; k++ )
  {
    bCurr[k + omegaParOffsetInIndPar] = omegaParCurr[k];
  }

}


/*************************************************************************
 *
 * Function: ~IndPredModel
 *
 *//**
 * Destructor.
 */
/*************************************************************************/

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
 *//**
 * Sets the index i for the current individual.
 */
/*************************************************************************/

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

  // Set the number of data records for this individual.
  nDataRecordCurr = predEvaluator.getNRecords( iCurr );

  // Set the number of observation records for this individual.
  nObsRecordCurr  = predEvaluator.getNObservs( iCurr );


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
  nW         = 2 * nObsRecordCurr;
  yOffsetInW = nObsRecordCurr;

  // Set the size of the vector of dependent variables.
  wCurr.resize( nW );

}


/*************************************************************************
 *
 * Function: doSetIndPar
 *
 *//**
 * Sets the current value for the individual parameter,
 * \f[
 *     b_i =
 *       \left[ 
 *         \begin{array}{c}
 *           \mbox{thetaCurr} \\
 *           \mbox{omegaParCurr}
 *         \end{array}
 *       \right] .
 * \f]
 * These are the parameters that are optimized over when performing
 * individual level estimation.
 */
/*************************************************************************/

void IndPredModel::doSetIndPar( const SPK_VA::valarray<double>& bIn ) 
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
 *//**
 * Invalidates all of the values stored in the cache.
 */
/*************************************************************************/

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
}


/*************************************************************************
 *
 * Function: getUsedCachedDataMean
 *
 *************************************************************************/

bool IndPredModel::getUsedCachedDataMean() const
{
  return usedCachedDataMean;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataMean_indPar
 *
 *************************************************************************/

bool IndPredModel::getUsedCachedDataMean_indPar() const
{
  return usedCachedDataMean_indPar;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataVariance
 *
 *************************************************************************/

bool IndPredModel::getUsedCachedDataVariance() const
{
  return usedCachedDataVariance;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataVariance_indPar
 *
 *************************************************************************/

bool IndPredModel::getUsedCachedDataVariance_indPar() const
{
  return usedCachedDataVariance_indPar;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataVarianceInv
 *
 *************************************************************************/

bool IndPredModel::getUsedCachedDataVarianceInv() const
{
  return usedCachedDataVarianceInv;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataVarianceInv_indPar
 *
 *************************************************************************/

bool IndPredModel::getUsedCachedDataVarianceInv_indPar() const
{
  return usedCachedDataVarianceInv_indPar;
}


/*************************************************************************
 *
 * Function: getUsedCachedPredADFun
 *
 *************************************************************************/

bool IndPredModel::getUsedCachedPredADFun() const
{
  return usedCachedPredADFun;
}


/*************************************************************************
 *
 * Function: getUsedCachedPredFirstDeriv
 *
 *************************************************************************/

bool IndPredModel::getUsedCachedPredFirstDeriv() const
{
  return usedCachedPredFirstDeriv;
}


/*************************************************************************
 *
 * Function: getUsedCachedPredSecondDeriv
 *
 *************************************************************************/

bool IndPredModel::getUsedCachedPredSecondDeriv() const
{
  return usedCachedPredSecondDeriv;
}


/*************************************************************************
 *
 * Function: getUsedCachedOmega
 *
 *************************************************************************/

bool IndPredModel::getUsedCachedOmega() const
{
  return pOmegaCurr->getUsedCachedCov();
}


/*************************************************************************
 *
 * Function: getUsedCachedOmega_omegaPar
 *
 *************************************************************************/

bool IndPredModel::getUsedCachedOmega_omegaPar() const
{
  return pOmegaCurr->getUsedCachedCov_par();
}


/*************************************************************************
 *
 * Function: evalAllPred
 *
 *//**
 * This function evaluates the predicted values for the data for
 * all of the observation records for the current individual.
 *
 * It does this by evaluating the expressions from the Pred block for
 * every data record and then setting the predicted value if the data
 * record is an observation record.
 *
 * Note that this function combines the parameters theta and eta
 * into a single vector of independent variables,
 * \f[
 *     z =
 *       \left[ 
 *         \begin{array}{c}
 *           \mbox{theta} \\
 *           \mbox{eta}
 *         \end{array}
 *       \right] .
 * \f]
 * In addition, this function combines the model functions f and y
 * into a single vector of dependent variables,
 * \f[
 *     w(z) =
 *       \left[ 
 *         \begin{array}{c}
 *           f(\mbox{theta}) \\
 *           y(\mbox{theta}, \mbox{eta})
 *         \end{array}
 *       \right] .
 * \f]
 */
/*************************************************************************/

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
    usedCachedPredADFun = true;

    return;
  }
  else
  {
    usedCachedPredADFun = false;
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
  Independent( zCurr );


  //------------------------------------------------------------
  // Evaluate all of the predicted values for the data.
  //------------------------------------------------------------

  // This message will be used if an error occurs.
  string taskMessage;

  bool isObsRecord;
  double fCurr;
  double yCurr;
  int j;

  // This will keep track of the number of predicted values
  // that have been set for the current individual.
  int nPredValSet = 0;

  // Evaluate the expressions from the Pred block for all of
  // the data records for the current individual.
  for ( j = 0; j < nDataRecordCurr; j++ )
  {
    taskMessage = "during the evaluation of the predicted value for the \n" + 
      intToOrdinalString( j, ZERO_IS_FIRST_INT ) +
       " data record for the individual.";

    // Evaluate the Pred block expressions for this data record.
    // The predicted value will be set during the call to eval()
    // if this data record is an observation record.
    try
    {
      isObsRecord = predEvaluator.eval(
        thetaOffsetInZ,
        nTheta,
        etaOffsetInZ,
        nEta,
        epsOffsetInZ,
        nEps,
        fOffsetInW,
        nObsRecordCurr,
        yOffsetInW,
        nObsRecordCurr,
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
      // Make sure that the value is finite.
      if ( isUnnormNumber( wCurr[ nPredValSet ] ) )
      {
        // [Revisit - SPK Error Codes Don't Really Apply - Mitch]
        // This error code should be replaced with one that is accurate.
        throw SpkException(
          SpkError::SPK_MODEL_DATA_MEAN_ERR,
          ( "An infinite value was generated " + taskMessage ).c_str(),
          __LINE__,
          __FILE__ );
      }
    
      // Make sure that the value is not a NaN.
      if ( isNotANumber( wCurr[ nPredValSet ] ) )
      {
        // [Revisit - SPK Error Codes Don't Really Apply - Mitch]
        // This error code should be replaced with one that is accurate.
        throw SpkException(
          SpkError::SPK_MODEL_DATA_MEAN_ERR,
          ( "A value that is Not a Number (NaN) was generated " + 
            taskMessage ).c_str(),
          __LINE__,
          __FILE__ );
      }

      // Increment the counter.
      nPredValSet++;
    }

  }

  // See if there was the correct number of observation records.
  if ( nPredValSet != nObsRecordCurr )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR, 
      "The number of data records that are observation records does not match the expected \nnumber of observation records.",
      __LINE__, 
      __FILE__ );
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
 *//**
 * Evaluates first derivatives of the Pred block expressions.
 *
 * In particular, this function evaluates the following first derivatives:
 * \f[
 *     \partial_{\mbox{theta}} \; f(\mbox{theta}) ,
 * \f]
 * and
 * \f[
 *     h(\mbox{theta}) =
 *       \partial_{\mbox{eta}} \; y(\mbox{theta}, \mbox{eta})
 *         \left|_{\mbox{eta}=0} \right. .
 * \f]
 */ 
/*************************************************************************/

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
    usedCachedPredFirstDeriv = true;

    return;
  }
  else
  {
    usedCachedPredFirstDeriv = false;
  }


  //------------------------------------------------------------
  // Prepare to calculate the first derivatives.
  //------------------------------------------------------------

  // Before first derivatives can be calculated, the predicted values
  // for all of the observation records for the current individual
  // must be evaluated.
  evalAllPred();

  // Make sure these are the proper sizes.
  f_thetaCurr.resize( nObsRecordCurr * nTheta );
  hCurr      .resize( nObsRecordCurr * nEta );

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
    for ( j = 0; j < nObsRecordCurr; j++ )
    {
      f_thetaCurr[j + k * nObsRecordCurr] = v1[j + fOffsetInW];
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
    for ( j = 0; j < nObsRecordCurr; j++ )
    {
      hCurr[j + k * nObsRecordCurr] = v1[j + yOffsetInW];
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
 *//**
 * Evaluates first and second derivatives of the Pred block expressions.
 *
 * In particular, this function evaluates the following second derivative:
 * \f[
 *     \partial_{\mbox{theta}}
 *       \left[
 *         \partial_{\mbox{eta}} \; y(\mbox{theta}, \mbox{eta})
 *           \left|_{\mbox{eta}=0} \right. 
 *       \right] .
 * \f]
 *
 * In addition, this function evaluates the following first derivatives:
 * \f[
 *     \partial_{\mbox{theta}} \; f(\mbox{theta}) ,
 * \f]
 * and
 * \f[
 *     h(\mbox{theta}) =
 *       \partial_{\mbox{eta}} \; y(\mbox{theta}, \mbox{eta})
 *         \left|_{\mbox{eta}=0} \right. .
 * \f]
 */ 
/*************************************************************************/

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
    usedCachedPredSecondDeriv = true;

    return;
  }
  else
  {
    usedCachedPredSecondDeriv = false;
  }


  //------------------------------------------------------------
  // Prepare to calculate the first and second derivatives.
  //------------------------------------------------------------

  // Before first or second derivatives can be calculated, the
  // predicted values for all of the observation records for the
  // current individual must be evaluated.
  evalAllPred();

  // Set the number of rows in the second derivatives that will 
  // be calculated.  Note that before the second derivatives are
  // calculated, the first derivatives are converted to a column
  // vector that contains the derivative's elements in row major
  // order, i.e., an rvec operation is performed on them.
  int nH_thetaRow = nObsRecordCurr * nEta;

  // Make sure these are the proper sizes.
  f_thetaCurr.resize( nObsRecordCurr * nTheta );
  hCurr      .resize( nObsRecordCurr * nEta );
  h_thetaCurr.resize( nObsRecordCurr * nEta * nTheta );

  // Set the lengths of the Taylor coefficient column vectors.
  std::vector<double> u1( pPredADFunCurr->Domain() );
  std::vector<double> u2( pPredADFunCurr->Domain() );
  std::vector<double> v1( pPredADFunCurr->Range() );
  std::vector<double> v2( pPredADFunCurr->Range() );

  // These will hold one-half times the diagonal elements of
  // the second derivatives.
  std::vector<double> y_theta_thetaDiagTerm( nTheta * nObsRecordCurr );
  std::vector<double> y_eta_etaDiagTerm    ( nEta   * nObsRecordCurr );


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
    for ( j = 0; j < nObsRecordCurr; j++ )
    {
      f_thetaCurr[j + k * nObsRecordCurr] = v1[j + fOffsetInW];
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
    for ( p = 0; p < nObsRecordCurr; p++ )
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
    for ( j = 0; j < nObsRecordCurr; j++ )
    {
      hCurr[j + k * nObsRecordCurr] = v1[j + yOffsetInW];
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
    for ( p = 0; p < nObsRecordCurr; p++ )
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
      for ( p = 0; p < nObsRecordCurr; p++ )
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
 *//**
 * Sets ret equal to the current value for the mean of the current
 * individual's data,
 * \f[
 *     f_i(b_i) ,
 * \f]
 * where
 *     \f$ i      \f$  = index for the current individual and
 *     \f$ b_i    \f$  = current value for the individual parameter.
 */
/*************************************************************************/

void IndPredModel::doDataMean( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nRow = nObsRecordCurr;
  int nCol = 1;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  if ( isDataMeanCurrOk )
  {
    ret = dataMeanCurr;
    usedCachedDataMean = true;

    return;
  }
  else
  {
    usedCachedDataMean = false;
  }

  dataMeanCurr.resize( nRow * nCol );


  //------------------------------------------------------------
  // Prepare to calculate the value.
  //------------------------------------------------------------

  // Evaluate the predicted values for all of the observation 
  // records for the current individual.
  evalAllPred();


  //------------------------------------------------------------
  // Calculate the value.
  //------------------------------------------------------------

  // This message will be used if a value is not valid.
  string taskMessage;
  int j;

  // Set the values for the mean of the data:
  //
  //     f    ( b  )  =  f   ( theta )  .
  //      i(j)   i        (j)
  //
  for ( j = 0; j < nObsRecordCurr; j++ )
  {
    // Set this element.
    dataMeanCurr[j] = Value( wCurr[j + fOffsetInW] );

    taskMessage = "during the evaluation of the mean of the " +
      intToOrdinalString( j, ZERO_IS_FIRST_INT ) +
      " value for the individual's data.";

    // Make sure that the value is finite.
    if ( isUnnormNumber( dataMeanCurr[j] ) )
    {
      // [Revisit - SPK Error Codes Don't Really Apply - Mitch]
      // This error code should be replaced with one that is accurate.
      throw SpkException(
        SpkError::SPK_MODEL_DATA_MEAN_ERR,
        ( "An infinite value was generated " + taskMessage ).c_str(),
        __LINE__,
        __FILE__ );
    }

    // Make sure that the value is not a NaN.
    if ( isNotANumber( dataMeanCurr[j] ) )
    {
      // [Revisit - SPK Error Codes Don't Really Apply - Mitch]
      // This error code should be replaced with one that is accurate.
      throw SpkException(
        SpkError::SPK_MODEL_DATA_MEAN_ERR,
        ( "A value that is Not a Number (NaN) was generated " + 
          taskMessage ).c_str(),
        __LINE__,
        __FILE__ );
    }

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
 *//**
 * Sets ret equal to the current value for the derivative with respect
 * to the individual parameter of the mean of the current individual's
 * data,
 * \f[
 *     \partial_b f_i(b_i) ,
 * \f]
 * where
 *     \f$ i      \f$  = index for the current individual and
 *     \f$ b_i    \f$  = current value for the individual parameter.
 *
 * This function returns a value of true if this derivative has 
 * at least one nonzero element.
 */
/*************************************************************************/

bool IndPredModel::doDataMean_indPar( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nRow = nObsRecordCurr;
  int nCol = nIndPar;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  if ( isDataMean_indParCurrOk )
  {
    ret = dataMean_indParCurr;
    usedCachedDataMean_indPar = true;

    // Return a value of true if this derivative has at least one
    // nonzero element.
    return !allZero( dataMean_indParCurr );
  }
  else
  {
    usedCachedDataMean_indPar = false;
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

  int j;
  int k;
  int q;

  // Set all of the elements of the derivative of the data 
  // mean equal to zero.
  dataMean_indParCurr = 0.0;

  // Set the values for the partial derivatives of the elements
  // of the data mean:
  //
  //      (q)
  //     d     f    ( b  )  .
  //      b     i(j)   i
  //
  for ( j = 0; j < nObsRecordCurr; j++ )
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

    // Set the partial derivatives of the elements that depend on theta:
    //
    //      (k)
    //     d       f    ( b  )  .
    //      theta   i(j)   i
    //
    for ( k = 0; k < nTheta; k++ )
    {
      q = k + thetaOffsetInIndPar;

      dataMean_indParCurr[j + q * nRow] = f_thetaCurr[j + q * nRow];
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
 *//**
 * Sets ret equal to the current value for the variance of the current
 * individual's data,
 * \f[
 *     R_i(b_i) ,
 * \f]
 * where
 *     \f$ i      \f$  = index for the current individual and
 *     \f$ b_i    \f$  = current value for the individual parameter.
 */
/*************************************************************************/

void IndPredModel::doDataVariance( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nRow = nObsRecordCurr;
  int nCol = nObsRecordCurr;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  if ( isDataVarianceCurrOk )
  {
    ret = dataVarianceCurr;
    usedCachedDataVariance = true;

    return;
  }
  else
  {
    usedCachedDataVariance = false;
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
  //      i(j,j)   i       /       (j,m)                (m,n)   (j,n)
  //                       ----
  //                        m,n
  //
  for ( j = 0; j < nObsRecordCurr; j++ )
  {
    // Set this element.
    for ( m = 0; m < nEta; m++ )
    {
      for ( n = 0; n < nEta; n++ )
      {
        dataVarianceCurr[j + j * nObsRecordCurr] += 
          hCurr[j + m * nObsRecordCurr] * omegaCurr[m + n * nEta] *
          hCurr[j + n * nObsRecordCurr];
      }
    }
  }


  //------------------------------------------------------------
  // Validate the value.
  //------------------------------------------------------------

  // This message will be used if a value is not valid.
  string taskMessage = 
    "during the evaluation of the variance of the individual's data.";

  // Check the diagonal elements of the data variance:
  //
  //     R      ( b  )  .
  //      i(j,j)
  //
  for ( j = 0; j < nObsRecordCurr; j++ )
  {
    // Make sure that the value is finite.
    if ( isUnnormNumber( dataVarianceCurr[j + j * nObsRecordCurr] ) )
    {
      // [Revisit - SPK Error Codes Don't Really Apply - Mitch]
      // This error code should be replaced with one that is accurate.
      throw SpkException(
        SpkError::SPK_MODEL_DATA_MEAN_ERR,
        ( "An infinite value was generated " + taskMessage ).c_str(),
        __LINE__,
        __FILE__ );
    }

    // Make sure that the value is not a NaN.
    if ( isNotANumber( dataVarianceCurr[j + j * nObsRecordCurr] ) )
    {
      // [Revisit - SPK Error Codes Don't Really Apply - Mitch]
      // This error code should be replaced with one that is accurate.
      throw SpkException(
        SpkError::SPK_MODEL_DATA_MEAN_ERR,
        ( "A value that is Not a Number (NaN) was generated " + 
          taskMessage ).c_str(),
        __LINE__,
        __FILE__ );
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
 *//**
 * Sets ret equal to the current value for the derivative with 
 * respect to the individual parameter of the variance of the
 * current individual's data,
 * \f[
 *     \partial_b R_i(b_i) =
 *       \partial_b \mbox{rvec} \left[ R_i(b_i) \right] ,
 * \f]
 * where
 *     \f$ i      \f$  = index for the current individual and
 *     \f$ b_i    \f$  = current value for the individual parameter.
 *
 * This function returns a value of true if this derivative has 
 * at least one nonzero element.
 */
/*************************************************************************/

bool IndPredModel::doDataVariance_indPar( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nRow = nObsRecordCurr * nObsRecordCurr;
  int nCol = nIndPar;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  if ( isDataVariance_indParCurrOk )
  {
    ret = dataVariance_indParCurr;
    usedCachedDataVariance_indPar = true;

    // Return a value of true if this derivative has at least one
    // nonzero element.
    return !allZero( dataVariance_indParCurr );
  }
  else
  {
    usedCachedDataVariance_indPar = false;
  }

  dataVariance_indParCurr.resize( nRow * nCol );


  //------------------------------------------------------------
  // Prepare to calculate the value.
  //------------------------------------------------------------

  // Evaluate the second derivatives of the Pred block expressions.
  evalPredSecondDeriv();

  // Save the current value for omega and its derivative.
  pOmegaCurr->cov( omegaCurr );
  pOmegaCurr->cov_par( omega_omegaParCurr );

  // These will hold the columns of h that correspond to each
  // observation record.
  valarray<double> hColTrans ( nEta );
  valarray<double> hCol_theta( nEta * nTheta );

  // Set the number of rows in the derivative of h.
  int nH_thetaRow = nObsRecordCurr * nEta;

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
  int j;
  int k;
  int m;
  int n;
  int q;

  // Set all of the elements of the derivative of the data 
  // variance equal to zero.
  dataVariance_indParCurr = 0.0;

  // Set the values for the partial derivatives of the diagonal
  // elements of the data variance:
  //
  //      (q)
  //     d     R      ( b  )  .
  //      b     i(j,j)   i
  //
  for ( j = 0; j < nObsRecordCurr; j++ )
  {
    // Set the row for this element in the rvec version of R.
    row = j * nObsRecordCurr + j;

    // Set the transpose and the derivative of the column of h
    // that corresponds to this observation record,
    //
    //               -                       -
    //              |     h      ( theta )    |
    //              |      (j, 0)             |
    //              |          .              |
    //     hCol  =  |          .              |  .
    //              |          .              |
    //              |          .              |
    //              |  h           ( theta )  |
    //              |   (j, nEta-1)           |
    //               -                       -
    //
    for ( m = 0; m < nEta; m++ )
    {
      // Set the transpose of this column.
      hColTrans[m] = hCurr[j + m * nObsRecordCurr];

      // Set the derivative with respect to theta of this column.
      for ( k = 0; k < nTheta; k++ )
      {
        // Note that an rvec operation is performed on the elements
        // of h before the derivative is calculated.
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

    // Set the partial derivatives of the elements that depend on theta:
    //
    //                                -                               -
    //      (k)                      |         T                       |
    //     d       R      ( b  )  =  |  2  hCol   omega  d       hCol  |     .
    //      theta   i(j,j)   i       |                    theta        |
    //                                -                               - (k) 
    //
    for ( k = 0; k < nTheta; k++ )
    {
      q = k + thetaOffsetInIndPar;

      dataVariance_indParCurr[row + q * nRow] =
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

    // Set the partial derivatives of the elements that depend on omegaPar:
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
      q = k + omegaParOffsetInIndPar;

      dataVariance_indParCurr[row + q * nRow] =
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
 *//**
 * Sets ret equal to the current value for the inverse of the variance
 * of the current individual's data,
 * \f[
 *     R^{-1}_i(b_i) ,
 * \f]
 * where
 *     \f$ i      \f$  = index for the current individual and
 *     \f$ b_i    \f$  = current value for the individual parameter.
 */
/*************************************************************************/

void IndPredModel::doDataVarianceInv( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nRow = nObsRecordCurr;
  int nCol = nObsRecordCurr;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  if ( isDataVarianceInvCurrOk )
  {
    ret = dataVarianceInvCurr;
    usedCachedDataVarianceInv = true;

    return;
  }
  else
  {
    usedCachedDataVarianceInv = false;
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
  for ( j = 0; j < nObsRecordCurr; j++ )
  {
    assert( dataVarianceInvCurr[j + j * nObsRecordCurr] != 0.0 );

    dataVarianceInvCurr[j + j * nObsRecordCurr] = 
      1.0 / dataVarianceInvCurr[j + j * nObsRecordCurr];
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
 *//**
 * Sets ret equal to the current value for the derivative with respect
 * to the individual parameter of the inverse of the variance of the
 * current individual's data,
 * \f[
 *     \partial_b R^{-1}_i(b_i) =
 *       \partial_b \mbox{rvec} \left[ R^{-1}_i(b_i) \right] ,
 * \f]
 * where
 *     \f$ i      \f$  = index for the current individual and
 *     \f$ b_i    \f$  = current value for the individual parameter.
 *
 * This function returns a value of true if this derivative has 
 * at least one nonzero element.
 */
/*************************************************************************/

bool IndPredModel::doDataVarianceInv_indPar( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nRow = nObsRecordCurr * nObsRecordCurr;
  int nCol = nIndPar;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  if ( isDataVarianceInv_indParCurrOk )
  {
    ret = dataVarianceInv_indParCurr;
    usedCachedDataVarianceInv_indPar = true;

    // Return a value of true if this derivative has at least one
    // nonzero element.
    return !allZero( dataVarianceInv_indParCurr );
  }
  else
  {
    usedCachedDataVarianceInv_indPar = false;
  }

  dataVarianceInv_indParCurr.resize( nRow * nCol );


  //------------------------------------------------------------
  // Prepare to calculate the value.
  //------------------------------------------------------------

  // Calculate the current value for the data variance if it has
  // not been calculated already.
  if ( isDataVarianceCurrOk )
  {
    valarray<double> dataVarianceTemp( nObsRecordCurr * nObsRecordCurr );
    doDataVariance( dataVarianceTemp );
  }


  //------------------------------------------------------------
  // Calculate the value.
  //------------------------------------------------------------

  int row;
  int j;
  int q;

  // Set the derivative of the inverse equal to the derivative of
  // the data variance itself since they have zero valued elements
  // in the same places.
  doDataVariance_indPar( dataVarianceInv_indParCurr );

  // Set the values for the partial derivatives of the diagonal
  // elements of the inverse of the data variance:
  //
  //                                  (q)
  //                              -  d     R      ( b  )
  //                                  b     i(j,j)   i  
  //      (q)   -1
  //     d     R      ( b  )  =  ------------------------  .
  //      b     i(j,j)   i                           2
  //                                   R      ( b  )
  //                                    i(j,j)   i
  //
  for ( j = 0; j < nObsRecordCurr; j++ )
  {
    // Set the row for this element in the rvec version of the 
    // inverse of R.
    row = j * nObsRecordCurr + j;

    // Set the partial derivatives.
    for ( q = 0; q < nIndPar; q++ )
    {
      assert( dataVarianceCurr[j + j * nObsRecordCurr] != 0.0 );

      dataVarianceInv_indParCurr[row + q * nRow] = 
        - dataVariance_indParCurr[row + q * nRow]
        / ( dataVarianceCurr[j + j * nObsRecordCurr] * 
            dataVarianceCurr[j + j * nObsRecordCurr] );
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
 *//**
 * Gets the lower and upper limits for the elements of the individual
 * parameter at the current individual parameter value.
 *
 * These limits can be used during the optimization of objective
 * functions that depend on these parameters.
 *
 * This function assumes that the current value for the individual
 * parameter is approximately equal to its final or true value.
 */
/*************************************************************************/

void IndPredModel::getIndParLimits(
  SPK_VA::valarray<double>&  indParLow,
  SPK_VA::valarray<double>&  indParUp ) const
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
 *//**
 * Gets a vector of step sizes for approximating derivatives with 
 * respect to the elements of the individual parameter.
 *
 * These step sizes can be used to approximate the derivatives of
 * objective functions that depend on the individual parameters.
 */
/*************************************************************************/

void IndPredModel::getIndParStep( SPK_VA::valarray<double>&  indParStep ) const
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
 *//**
 * Returns the number of elements in the individual parameter.
 */
/*************************************************************************/

int IndPredModel::getNIndPar() const
{
  return nIndPar;
}


/*************************************************************************
 *
 * Function: getIndPar
 *
 *//**
 * Gets the current value for the individual parameter.
 */
/*************************************************************************/

void IndPredModel::getIndPar( SPK_VA::valarray<double>& ret ) const 
{
  ret.resize( nIndPar );

  ret = bCurr;
}


/*************************************************************************
 *
 * Function: getTheta
 *
 *//**
 * Gets the current value for theta.
 */
/*************************************************************************/

void IndPredModel::getTheta( SPK_VA::valarray<double>& ret ) const 
{
  ret.resize( nTheta );

  ret = thetaCurr;
}


/*************************************************************************
 *
 * Function: getEta
 *
 *//**
 * Gets the current value for eta.
 */
/*************************************************************************/

void IndPredModel::getEta( SPK_VA::valarray<double>& ret ) const 
{
  ret.resize( nEta );

  ret = etaCurr;
}


/*************************************************************************
 *
 * Function: getOmega
 *
 *//**
 * Gets the minimal representation for the current value for omega.
 */
/*************************************************************************/

void IndPredModel::getOmega( SPK_VA::valarray<double>& ret ) const 
{
  ret.resize( pOmegaCurr->getNPar() );

  // Get the current value for omega.
  pOmegaCurr->cov( omegaCurr );

  // Return its minimal representation.
  pOmegaCurr->calcCovMinRep( omegaCurr, ret );
}


/*************************************************************************
 *
 * Function: getStandardPar
 *
 *//**
 * Gets the current values for the standard parameters.
 *
 * In particular, this function gets the current values for theta and 
 * the minimal representation for omega combined into a single vector,
 * \f[
 *     \mbox{standardPar} =
 *       \left[ 
 *         \begin{array}{c}
 *           \mbox{thetaCurr} \\
 *           \mbox{omegaMinRepCurr}
 *         \end{array}
 *       \right] .
 * \f]
 */
/*************************************************************************/

void IndPredModel::getStandardPar( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  ret.resize( nIndPar );


  //------------------------------------------------------------
  // Prepare the covariance matrix.
  //------------------------------------------------------------

  // Get the current value for omega.
  pOmegaCurr->cov( omegaCurr );

  // Get omega's minimal representation.
  valarray<double> omegaMinRepTemp( nOmegaPar );
  pOmegaCurr->calcCovMinRep( omegaCurr, omegaMinRepTemp );


  //------------------------------------------------------------
  // Set the vector of standard parameters.
  //------------------------------------------------------------

  int k;

  // Set the elements that contain theta.
  for ( k = 0; k < nTheta; k++ )
  {
    ret[k + thetaOffsetInIndPar] = thetaCurr[k];
  }

  // Set the elements that contain the minimal representation
  // for omega.
  for ( k = 0; k < nOmegaPar; k++ )
  {
    ret[k + omegaParOffsetInIndPar] = omegaMinRepTemp[k];
  }

}


/*************************************************************************
 *
 * Function: getStandardPar_indPar
 *
 *//**
 * Gets the current values for the derivatives of the standard parameters.
 *
 * In particular, this function gets the current values for the derivative 
 * with respect to the individual parameter of theta and the minimal 
 * representation for omega combined into a single vector,
 * \f[
 *     \partial_b \; \mbox{standardPar} = \partial_b 
 *       \left[ 
 *         \begin{array}{c}
 *           \mbox{thetaCurr} \\
 *           \mbox{omegaMinRepCurr}
 *         \end{array}
 *       \right] .
 * \f]
 */
/*************************************************************************/

void IndPredModel::getStandardPar_indPar( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  ret.resize( nIndPar * nIndPar );


  //------------------------------------------------------------
  // Prepare the derivative of the covariance matrix.
  //------------------------------------------------------------

  // Get the current value for the derivative of omega.
  pOmegaCurr->cov_par( omega_omegaParCurr );

  // Get the derivative of omega's minimal representation.
  valarray<double> omegaMinRep_omegaParTemp( nOmegaPar * nOmegaPar );
  pOmegaCurr->calcCovMinRep_par(
    omega_omegaParCurr,
    nOmegaPar,
    omegaMinRep_omegaParTemp );


  //------------------------------------------------------------
  // Set the vector of standard parameters.
  //------------------------------------------------------------

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //
  // Note
  // ----
  //
  // Since the individual parameter is
  //
  //             -              -
  //            |   thetaCurr    |
  //     b   =  |                |  ,
  //      i     |  omegaParCurr  |
  //             -              -
  //
  // the derivative of the vector of standard parameters is
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
  // where the identity matrix is nTheta by nTheta.
  //
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  int j;
  int k;
  int row;
  int col;

  // Set all of the elements of the derivative of the standard
  // parameters equal to zero.
  ret = 0.0;

  // Set the partial derivatives of the elements that depend on 
  // theta.
  for ( k = 0; k < nTheta; k++ )
  {
    row = k + thetaOffsetInIndPar;
    col = k + thetaOffsetInIndPar;

    ret[row + col * nIndPar] = 1.0;
  }

  // Set the partial derivatives of the elements that depend on 
  // the minimal representation for omega.
  for ( j = 0; j < nOmegaPar; j++ )
  {
    for ( k = 0; k < nOmegaPar; k++ )
    {
      row = j + omegaParOffsetInIndPar;
      col = k + omegaParOffsetInIndPar;
    
      ret[row + col * nIndPar] = omegaMinRep_omegaParTemp[j + k * nOmegaPar];
    }
  }

}


/*************************************************************************
 *
 * Function: getStandardParMask
 *
 *//**
 * Gets the mask used to calculate statistics for the standard
 * parameters based on the mask for the individual parameters.
 *
 * In particular, this function gets the masks for theta and the
 * minimal representation for omega,
 * \f[
 *     \mbox{standardParMask} =
 *       \left[ 
 *         \begin{array}{c}
 *           \mbox{thetaMask} \\
 *           \mbox{omegaMinRepMask}
 *         \end{array}
 *       \right] .
 * \f]
 */
/*************************************************************************/

void IndPredModel::getStandardParMask( 
  const SPK_VA::valarray<bool>& indParMaskIn,
  SPK_VA::valarray<bool>&       standardParMaskOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  standardParMaskOut.resize( nIndPar );
  assert( indParMaskIn.size() == nIndPar );


  //------------------------------------------------------------
  // Split up the individual parameters mask into pieces.
  //------------------------------------------------------------

  int k;

  valarray<bool> thetaMask      ( nTheta );
  valarray<bool> omegaParMask( nOmegaPar );

  // Get the elements that correspond to theta.
  for ( k = 0; k < nTheta; k++ )
  {
    thetaMask[k] = indParMaskIn[k + thetaOffsetInIndPar];
  }

  // Get the elements that correspond to the parameters for omega.
  for ( k = 0; k < nOmegaPar; k++ )
  {
    omegaParMask[k] = indParMaskIn[k + omegaParOffsetInIndPar];
  }


  //------------------------------------------------------------
  // Prepare the mask for the covariance minimal representation.
  //------------------------------------------------------------

  // Get the mask for omega's minimal representation.
  valarray<bool> omegaMinRepMask( nOmegaPar );
  pOmegaCurr->calcCovMinRepMask( omegaParMask, omegaMinRepMask );


  //------------------------------------------------------------
  // Set the mask for the standard parameters.
  //------------------------------------------------------------

  // Set the elements that correspond to theta.
  for ( k = 0; k < nTheta; k++ )
  {
    standardParMaskOut[k + thetaOffsetInIndPar] = thetaMask[k];
  }

  // Set the elements that correspond to the minimal representation
  // for omega.
  for ( k = 0; k < nOmegaPar; k++ )
  {
    standardParMaskOut[k + omegaParOffsetInIndPar] = omegaMinRepMask[k];
  }

}
