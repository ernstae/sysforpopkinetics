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
 * @file: IndPredModelBase.cpp
 *
 *
 * Implements IndPredModelBase class.
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
#include "IndPredModelBase.h"
#include "isEqual.h"
#include "PredBase.h"

// SPK library header files.
#include <spk/doubleToScalarArray.h>
#include <spk/intToOrdinalString.h>
#include <spk/isNotANumber.h>
#include <spk/isUnnormNumber.h>
#include <spk/scalarToDouble.h>
#include <spk/SpkException.h>
#include <spk/SpkValarray.h>

// CppAD header files.
#include <CppAD/CppAD.h>

// Standard library header files.
#include <cassert>
#include <limits>
#include <sstream>
#include <vector>

using namespace CppAD;
using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: IndPredModelBase
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
template<class Scalar>
IndPredModelBase<Scalar>::IndPredModelBase(
    PredBase< Scalar >&                          predEvaluatorIn,
    PredBase< CppAD::AD<Scalar> >&               predEvaluatorADIn,
    PredBase< CppAD::AD< CppAD::AD<Scalar> > >&  predEvaluatorADADIn,
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
  nEps                              ( 0 ),
  thetaOffsetInIndPar               ( 0 ),
  omegaParOffsetInIndPar            ( nThetaIn ),
  nZ                                ( nThetaIn + nEtaIn ),
  thetaOffsetInZ                    ( 0 ),
  etaOffsetInZ                      ( nThetaIn ),
  epsOffsetInZ                      ( nThetaIn + nEtaIn ),
  omegaParOffsetInZ                 ( nThetaIn + nEtaIn ),
  pOmegaCurr                        ( 0 ),
  predEvaluator                     ( predEvaluatorIn ),
  predEvaluatorAD                   ( predEvaluatorADIn ),
  predEvaluatorADAD                 ( predEvaluatorADADIn ),
  zCurr                             ( nZ ),
  zCurrAD                           ( nZ ),
  zCurrADAD                         ( nZ ),
  isDataMeanCurrOk                  ( false ),
  isDataMean_indParCurrOk           ( false ),
  isDataVarianceCurrOk              ( false ),
  isDataVariance_indParCurrOk       ( false ),
  isDataVarianceInvCurrOk           ( false ),
  isDataVarianceInv_indParCurrOk    ( false ),
  isFAndHCurrOk                     ( false ),
  isFAndH_thetaCurrOk               ( false ),
  usedCachedDataMean                ( false ),
  usedCachedDataMean_indPar         ( false ),
  usedCachedDataVariance            ( false ),
  usedCachedDataVariance_indPar     ( false ),
  usedCachedDataVarianceInv         ( false ),
  usedCachedDataVarianceInv_indPar  ( false ),
  usedCachedFAndH                   ( false ),
  usedCachedFAndH_theta             ( false )
{
  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrix.
  //------------------------------------------------------------

  // Construct omega, the covariance matrix for eta, with the
  // appropriate structure.
  if ( omegaStructIn == DIAGONAL )
  {
    pOmegaCurr = new DiagCovBase<Scalar>( nEta );
  }
  else if ( omegaStructIn == FULL )
  {
    pOmegaCurr = new FullCovBase<Scalar>( nEta );
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
  doubleToScalarArray( thetaCurrIn, thetaCurr );
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

  // Resize AD<Scalar> and AD< AD<Scalar> > versions of these
  // parameters.
  thetaCurrAD  .resize( nTheta );
  etaCurrAD    .resize( nEta );
  thetaCurrADAD.resize( nTheta );
  etaCurrADAD  .resize( nEta );


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
 * Function: IndPredModelBase
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

//Constructor (with inputs for FIXed elements)
//[Revist - get rid of 2nd constructor - Dave]
template<class Scalar>
IndPredModelBase<Scalar>::IndPredModelBase(
    PredBase< Scalar >&                          predEvaluatorIn,
    PredBase< CppAD::AD<Scalar> >&               predEvaluatorADIn,
    PredBase< CppAD::AD< CppAD::AD<Scalar> > >&  predEvaluatorADADIn,
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
  nEps                              ( 0 ),
  thetaOffsetInIndPar               ( 0 ),
  omegaParOffsetInIndPar            ( nThetaIn ),
  nZ                                ( nThetaIn + nEtaIn ),
  thetaOffsetInZ                    ( 0 ),
  etaOffsetInZ                      ( nThetaIn ),
  epsOffsetInZ                      ( nThetaIn + nEtaIn ),
  omegaParOffsetInZ                 ( nThetaIn + nEtaIn ),
  pOmegaCurr                        ( 0 ),
  predEvaluator                     ( predEvaluatorIn ),
  predEvaluatorAD                   ( predEvaluatorADIn ),
  predEvaluatorADAD                 ( predEvaluatorADADIn ),
  zCurr                             ( nZ ),
  zCurrAD                           ( nZ ),
  zCurrADAD                         ( nZ ),
  isDataMeanCurrOk                  ( false ),
  isDataMean_indParCurrOk           ( false ),
  isDataVarianceCurrOk              ( false ),
  isDataVariance_indParCurrOk       ( false ),
  isDataVarianceInvCurrOk           ( false ),
  isDataVarianceInv_indParCurrOk    ( false ),
  isFAndHCurrOk                     ( false ),
  isFAndH_thetaCurrOk               ( false ),
  usedCachedDataMean                ( false ),
  usedCachedDataMean_indPar         ( false ),
  usedCachedDataVariance            ( false ),
  usedCachedDataVariance_indPar     ( false ),
  usedCachedDataVarianceInv         ( false ),
  usedCachedDataVarianceInv_indPar  ( false ),
  usedCachedFAndH                   ( false ),
  usedCachedFAndH_theta             ( false )
{
  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrix.
  //------------------------------------------------------------

  // Construct omega, the covariance matrix for eta, with the
  // appropriate structure.
  if ( omegaStructIn == DIAGONAL )
  {
    pOmegaCurr = new DiagCovBase<Scalar>( nEta, omegaMinRepFixedIn );
  }
  else if ( omegaStructIn == FULL )
  {
    pOmegaCurr = new FullCovBase<Scalar>( nEta, omegaMinRepFixedIn );
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
  doubleToScalarArray( thetaCurrIn, thetaCurr );
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

  // Resize AD<Scalar> and AD< AD<Scalar> > versions of these
  // parameters.
  thetaCurrAD  .resize( nTheta );
  etaCurrAD    .resize( nEta );
  thetaCurrADAD.resize( nTheta );
  etaCurrADAD  .resize( nEta );


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
 * Function: IndPredModelBase
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

//Constructor (with inputs for block structure)
template<class Scalar>
IndPredModelBase<Scalar>::IndPredModelBase(
    PredBase< Scalar >&                          predEvaluatorIn,
    PredBase< CppAD::AD<Scalar> >&               predEvaluatorADIn,
    PredBase< CppAD::AD< CppAD::AD<Scalar> > >&  predEvaluatorADADIn,
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
  nEps                              ( 0 ),
  thetaOffsetInIndPar               ( 0 ),
  omegaParOffsetInIndPar            ( nThetaIn ),
  nZ                                ( nThetaIn + nEtaIn ),
  thetaOffsetInZ                    ( 0 ),
  etaOffsetInZ                      ( nThetaIn ),
  epsOffsetInZ                      ( nThetaIn + nEtaIn ),
  omegaParOffsetInZ                 ( nThetaIn + nEtaIn ),
  pOmegaCurr                        ( 0 ),
  predEvaluator                     ( predEvaluatorIn ),
  predEvaluatorAD                   ( predEvaluatorADIn ),
  predEvaluatorADAD                 ( predEvaluatorADADIn ),
  zCurr                             ( nZ ),
  zCurrAD                           ( nZ ),
  zCurrADAD                         ( nZ ),
  isDataMeanCurrOk                  ( false ),
  isDataMean_indParCurrOk           ( false ),
  isDataVarianceCurrOk              ( false ),
  isDataVariance_indParCurrOk       ( false ),
  isDataVarianceInvCurrOk           ( false ),
  isDataVarianceInv_indParCurrOk    ( false ),
  usedCachedDataMean                ( false ),
  usedCachedDataMean_indPar         ( false ),
  usedCachedDataVariance            ( false ),
  usedCachedDataVariance_indPar     ( false ),
  usedCachedDataVarianceInv         ( false ),
  usedCachedDataVarianceInv_indPar  ( false ),
  usedCachedFAndH                   ( false ),
  usedCachedFAndH_theta             ( false )
{
  //------------------------------------------------------------
  // Initialize quantities related to the covariance matrix.
  //------------------------------------------------------------

  // Construct omega, the covariance matrix for eta, with the
  // appropriate structure.
  if ( omegaStructIn == DIAGONAL )
  {
    pOmegaCurr = new DiagCovBase<Scalar>( nEta, omegaMinRepFixedIn );
  }
  else if ( omegaStructIn == FULL )
  {
    pOmegaCurr = new FullCovBase<Scalar>( nEta, omegaMinRepFixedIn );
  }
  else if ( omegaStructIn == BLOCKDIAG )
  {
    pOmegaCurr = new BlkDiagCovBase<Scalar>( nEta,
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
  doubleToScalarArray( thetaCurrIn, thetaCurr );
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

  // Resize AD<Scalar> and AD< AD<Scalar> > versions of these
  // parameters.
  thetaCurrAD  .resize( nTheta );
  etaCurrAD    .resize( nEta );
  thetaCurrADAD.resize( nTheta );
  etaCurrADAD  .resize( nEta );


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
 * Function: ~IndPredModelBase
 *
 *//**
 * Destructor.
 */
/*************************************************************************/

template<class Scalar>
IndPredModelBase<Scalar>::~IndPredModelBase()
{
  // Free the memory allocated for the omega covariance matrix.
  if ( pOmegaCurr )
  {
    delete pOmegaCurr;
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

template<class Scalar>
void IndPredModelBase<Scalar>::doSelectIndividual( int iIn )
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

template<class Scalar>
void IndPredModelBase<Scalar>::doSetIndPar( const SPK_VA::valarray<Scalar>& bIn ) 
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
 * Function: setTheta
 *
 *//**
 * Sets the current value for theta.
 */
/*************************************************************************/

template<class Scalar>
void IndPredModelBase<Scalar>::setTheta( const SPK_VA::valarray<Scalar>& thetaIn )
{
  thetaCurr = thetaIn;
  assert( thetaIn.size() == nTheta );
}


/*************************************************************************
 *
 * Function: setOmega
 *
 *//**
 * Sets the current value for omega.
 */
/*************************************************************************/

template<class Scalar>
void IndPredModelBase<Scalar>::setOmega( const SPK_VA::valarray<Scalar>& omegaIn )
{
  // Set the omega value maintained by the covariance class.
  pOmegaCurr->setCov( omegaIn );
  assert( omegaIn.size() == nEta * nEta );

  // Save the initial value for the omega parameters.
  pOmegaCurr->calcPar( omegaIn, omegaParCurr );
}


/*************************************************************************
 *
 * Function: invalidateCache
 *
 *//**
 * Invalidates all of the values stored in the cache.
 */
/*************************************************************************/

template<class Scalar>
void IndPredModelBase<Scalar>::invalidateCache() const
{
  isDataMeanCurrOk               = false;
  isDataMean_indParCurrOk        = false;
  isDataVarianceCurrOk           = false;
  isDataVariance_indParCurrOk    = false;
  isDataVarianceInvCurrOk        = false;
  isDataVarianceInv_indParCurrOk = false;
  isFAndHCurrOk                  = false;
  isFAndH_thetaCurrOk            = false;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataMean
 *
 *************************************************************************/

template<class Scalar>
bool IndPredModelBase<Scalar>::getUsedCachedDataMean() const
{
  return usedCachedDataMean;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataMean_indPar
 *
 *************************************************************************/

template<class Scalar>
bool IndPredModelBase<Scalar>::getUsedCachedDataMean_indPar() const
{
  return usedCachedDataMean_indPar;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataVariance
 *
 *************************************************************************/

template<class Scalar>
bool IndPredModelBase<Scalar>::getUsedCachedDataVariance() const
{
  return usedCachedDataVariance;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataVariance_indPar
 *
 *************************************************************************/

template<class Scalar>
bool IndPredModelBase<Scalar>::getUsedCachedDataVariance_indPar() const
{
  return usedCachedDataVariance_indPar;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataVarianceInv
 *
 *************************************************************************/

template<class Scalar>
bool IndPredModelBase<Scalar>::getUsedCachedDataVarianceInv() const
{
  return usedCachedDataVarianceInv;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataVarianceInv_indPar
 *
 *************************************************************************/

template<class Scalar>
bool IndPredModelBase<Scalar>::getUsedCachedDataVarianceInv_indPar() const
{
  return usedCachedDataVarianceInv_indPar;
}


/*************************************************************************
 *
 * Function: getUsedCachedFAndH
 *
 *************************************************************************/

template<class Scalar>
bool IndPredModelBase<Scalar>::getUsedCachedFAndH() const
{
  return usedCachedFAndH;
}


/*************************************************************************
 *
 * Function: getUsedCachedFAndH_theta
 *
 *************************************************************************/

template<class Scalar>
bool IndPredModelBase<Scalar>::getUsedCachedFAndH_theta() const
{
  return usedCachedFAndH_theta;
}


/*************************************************************************
 *
 * Function: getUsedCachedOmega
 *
 *************************************************************************/

template<class Scalar>
bool IndPredModelBase<Scalar>::getUsedCachedOmega() const
{
  return pOmegaCurr->getUsedCachedCov();
}


/*************************************************************************
 *
 * Function: getUsedCachedOmega_omegaPar
 *
 *************************************************************************/

template<class Scalar>
bool IndPredModelBase<Scalar>::getUsedCachedOmega_omegaPar() const
{
  return pOmegaCurr->getUsedCachedCov_par();
}


/*************************************************************************
 *
 * Function: evalFAndH
 *
 *//**
 * This function evaluates the mean for the current individual's data
 * \f[
 *     f_i(\mbox{theta})
 * \f]
 * and the derivative with respect to eta of their data values
 * \f[
 *     h_i(\mbox{theta}) =
 *       \partial_{\mbox{eta}} \; y_i(\mbox{theta}, \mbox{eta})
 *         \left|_{\mbox{eta}=0} \right. 
 * \f]
 * for all of the observation records for the current individual.
 *
 * This function sets
 * \f[
 *     \mbox{fCurr} = f_i(\mbox{theta}) 
 * \f]
 * and
 * \f[
 *     \mbox{hCurr} = h_i(\mbox{theta})  .
 * \f]
 *
 * Note that these quantities are functions of theta, which is a
 * component of \f$b_i\f$.
 */
/*************************************************************************/

template<class Scalar>
void IndPredModelBase<Scalar>::evalFAndH() const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Use the cached values if possible.
  //------------------------------------------------------------

  if ( isFAndHCurrOk )
  {
    usedCachedFAndH = true;

    return;
  }
  else
  {
    usedCachedFAndH = false;
  }


  //------------------------------------------------------------
  // Evaluate all of the data mean values.
  //------------------------------------------------------------

  int k;

  // Make sure this is the proper size.
  fCurr.resize( nObsRecordCurr );

  // Set the current vector of variable values,
  //
  //            -       -
  //           |  theta  |
  //     z  =  |         |  .
  //           |   eta   |
  //            -       -
  //
  for ( k = 0; k < nTheta; k++ )
  {
    zCurr[k + thetaOffsetInZ] = thetaCurr[k];
  }
  for ( k = 0; k < nEta; k++ )
  {
    zCurr[k + etaOffsetInZ]   = etaCurr[k];
  }

  // Set all of the data mean values for this individual,
  //
  //     f ( theta )  .
  //      i
  //
  predEvaluator.evalAllF( 
    thetaOffsetInZ,
    nTheta,
    etaOffsetInZ,
    nEta,
    epsOffsetInZ,
    nEps,
    nObsRecordCurr,
    iCurr,
    zCurr,
    fCurr );


  //------------------------------------------------------------
  // Evaluate all of the data values.
  //------------------------------------------------------------

  // Make sure these are the proper size.
  fCurrAD.resize( nObsRecordCurr );
  yCurrAD.resize( nObsRecordCurr );

  // Set AD<Scalar> versions of these variables.
  for ( k = 0; k < nTheta; k++ )
  {
    thetaCurrAD[k]    = static_cast<Scalar>( thetaCurr[k] );
  }
  for ( k = 0; k < nEta; k++ )
  {
    etaCurrAD[k]      = static_cast<Scalar>( etaCurr[k] );
  }
  for ( k = 0; k < nObsRecordCurr; k++ )
  {
    fCurrAD[k]        = static_cast<Scalar>( fCurr[k] );
  }

  // Set any values for the AD<Scalar> version of the expression
  // evaluator that were calculated when evalAllF() was called with
  // the Scalar version.
  predEvaluator.initPredSubAD( &predEvaluatorAD );

  // Declare the independent variable for calculating h and start the
  // CppAD tape.
  //
  // This is the only level of taping for this function, beyond any
  // taping that may already be happening for the Scalar type.
  Independent( etaCurrAD );

  // Set the AD<Scalar> version of the current vector of variable
  // values,
  //
  //            -       -
  //           |  theta  |
  //     z  =  |         |  .
  //           |   eta   |
  //            -       -
  //
  for ( k = 0; k < nTheta; k++ )
  {
    zCurrAD[k + thetaOffsetInZ] = thetaCurrAD[k];
  }
  for ( k = 0; k < nEta; k++ )
  {
    zCurrAD[k + etaOffsetInZ]   = etaCurrAD[k];
  }

  // Set all of the data values for this individual using the
  // AD<Scalar> expression evaluator,
  //
  //     y ( theta, eta )  .
  //      i
  //
  try
  {
    predEvaluatorAD.evalAllY(
      thetaOffsetInZ,
      nTheta,
      etaOffsetInZ,
      nEta,
      epsOffsetInZ,
      nEps,
      nObsRecordCurr,
      iCurr,
      zCurrAD,
      fCurrAD,
      yCurrAD );
  }
  catch( ... )
  {
    // If any exceptions occur, then stop the taping by calling
    // Dependent, which is faster in this case than using the ADFun
    // constructor as is done below.
    ADFun<Scalar> yADFunTemp;
    yADFunTemp.Dependent( etaCurrAD, yCurrAD );

    // Rethrow the exception that was originally thrown.
    throw;
  }

  // Stop the taping by constructing a differentiable function object
  // that corresponds to the mapping of eta to y.  This function is
  // defined by the relationships between eta and y that were
  // performed during the call to predEvaluatorAD.evalAllY().
  ADFun<Scalar> yADFunCurr( etaCurrAD, yCurrAD );


  //------------------------------------------------------------
  // Calculate the derivative of the data values.
  //------------------------------------------------------------

  int j;

  // Make sure this is the proper size.
  hCurr.resize( nObsRecordCurr * nEta );

  // Set the lengths of the argument and result vectors for Forward.
  std::vector<Scalar> u1( yADFunCurr.Domain() );
  std::vector<Scalar> v1( yADFunCurr.Range() );

  assert( nEta           == yADFunCurr.Domain() );
  assert( nObsRecordCurr == yADFunCurr.Range() );

  // Initialize the first order arguments.
  for ( k = 0; k < nEta; k++ )
  {
    u1[k] = 0;
  }

  // Calculate the first derivative of the data values
  //
  //                                             |
  //     h ( theta )  =  d     y ( theta, eta )  |          .
  //      i               eta   i                | eta = 0
  //
  // Use forward mode to calculate this derivative because there is
  // usually only one eta element and more than one y value, which
  // means there are more dependent than independent parameters.
  for ( k = 0; k < nEta; k++ )
  {
    // Evaluate
    //
    //             (k)
    //     v1  =  d     y( theta, eta )  .
    //             eta
    //
    u1[k] = 1;
    v1    = yADFunCurr.Forward( 1, u1 );
    u1[k] = 0;

    // Set the elements of the first derivative:
    //
    //                (k)
    //     h      =  d     y   ( theta, eta )  .
    //      (j,k)     eta   (j)
    //
    for ( j = 0; j < nObsRecordCurr; j++ )
    {
      hCurr[j + k * nObsRecordCurr] = v1[j];
    }

  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isFAndHCurrOk = true;
}


/*************************************************************************
 *
 * Function: evalFAndH_theta
 *
 *//**
 * This function evaluates the derivative with respect to theta of the
 * mean for the current individual's data
 * \f[
 *     \partial_{\mbox{theta}} \; f_i(\mbox{theta})
 * \f]
 * and the derivative with respect to theta of the derivative with respect 
 * to eta of their data values
 * \f[
 *     \partial_{\mbox{theta}}
 *       \left[
 *         h_i(\mbox{theta})
 *       \right]
 *     =
 *     \partial_{\mbox{theta}}
 *       \left[
 *         \partial_{\mbox{eta}} \; y_i(\mbox{theta}, \mbox{eta})
 *           \left|_{\mbox{eta}=0} \right. 
 *       \right] 
 * \f]
 * for all of the observation records for the current individual.
 *
 * This function sets
 * \f[
 *     \mbox{f\_thetaCurr} = \partial_{\mbox{theta}} \; f_i(\mbox{theta})
 * \f]
 * and
 * \f[
 *     \mbox{h\_thetaCurr} = \partial_{\mbox{theta}} \; h_i(\mbox{theta})  .
 * \f]
 *
 * Note that these quantities are functions of theta, which is a
 * component of \f$b_i\f$.
 */
/*************************************************************************/

template<class Scalar>
void IndPredModelBase<Scalar>::evalFAndH_theta() const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Use the cached values if possible.
  //------------------------------------------------------------

  if ( isFAndH_thetaCurrOk )
  {
    usedCachedFAndH_theta = true;

    return;
  }
  else
  {
    usedCachedFAndH_theta = false;
  }


  //------------------------------------------------------------
  // Evaluate all of the data mean values.
  //------------------------------------------------------------

  int k;

  // Make sure these are the proper size.
  fCurrAD.resize( nObsRecordCurr );
  yCurrAD.resize( nObsRecordCurr );

  // Set AD<Scalar> versions of these variables.
  for ( k = 0; k < nTheta; k++ )
  {
    thetaCurrAD[k]    = static_cast<Scalar>( thetaCurr[k] );
  }
  for ( k = 0; k < nEta; k++ )
  {
    etaCurrAD[k]      = static_cast<Scalar>( etaCurr[k] );
  }

  // Declare the independent variable for calculating the derivative
  // of f and h and start the CppAD tape.
  //
  // This is the first level of taping for this function, beyond any
  // taping that may already be happening for the Scalar type.
  Independent( thetaCurrAD );

  // Set the AD<Scalar> version of the current vector of variable
  // values,
  //
  //            -       -
  //           |  theta  |
  //     z  =  |         |  .
  //           |   eta   |
  //            -       -
  //
  for ( k = 0; k < nTheta; k++ )
  {
    zCurrAD[k + thetaOffsetInZ] = thetaCurrAD[k];
  }
  for ( k = 0; k < nEta; k++ )
  {
    zCurrAD[k + etaOffsetInZ]   = etaCurrAD[k];
  }

  // Set all of the data mean values for this individual,
  //
  //     f ( theta )  .
  //      i
  //
  try
  {
    predEvaluatorAD.evalAllF( 
      thetaOffsetInZ,
      nTheta,
      etaOffsetInZ,
      nEta,
      epsOffsetInZ,
      nEps,
      nObsRecordCurr,
      iCurr,
      zCurrAD,
      fCurrAD );
  }
  catch( ... )
  {
    // If any exceptions occur, then stop the taping by calling
    // Dependent, which is faster in this case than using the ADFun
    // constructor as is done below.
    ADFun<Scalar> fADFunTemp;
    fADFunTemp.Dependent( thetaCurrAD, fCurrAD );

    // Rethrow the exception that was originally thrown.
    throw;
  }


  //------------------------------------------------------------
  // Evaluate all of the data values.
  //------------------------------------------------------------

  // Make sure these are the proper size.
  fCurrADAD.resize( nObsRecordCurr );
  yCurrADAD.resize( nObsRecordCurr );

  // Set AD<AD<Scalar>> versions of these variables.
  for ( k = 0; k < nTheta; k++ )
  {
    thetaCurrADAD[k]    = static_cast< AD<Scalar> >( thetaCurrAD[k] );
  }
  for ( k = 0; k < nEta; k++ )
  {
    etaCurrADAD[k]      = static_cast< AD<Scalar> >( etaCurrAD[k] );
  }
  for ( k = 0; k < nObsRecordCurr; k++ )
  {
    fCurrADAD[k]        = static_cast< AD<Scalar> >( fCurrAD[k] );
  }

  // Set any values for the AD<AD<Scalar>> version of the expression
  // evaluator that were calculated when evalAllF() was called with
  // the AD<Scalar> version.
  predEvaluatorAD.initPredSubAD( &predEvaluatorADAD );

  // Declare the independent variable for calculating h and start the
  // CppAD tape.
  //
  // This is the second level of taping for this function, beyond any
  // taping that may already be happening for the Scalar type.
  Independent( etaCurrADAD );

  // Set the AD<AD<Scalar>> version of the current vector of variable
  // values,
  //
  //            -       -
  //           |  theta  |
  //     z  =  |         |  .
  //           |   eta   |
  //            -       -
  //
  for ( k = 0; k < nTheta; k++ )
  {
    zCurrADAD[k + thetaOffsetInZ] = thetaCurrADAD[k];
  }
  for ( k = 0; k < nEta; k++ )
  {
    zCurrADAD[k + etaOffsetInZ]   = etaCurrADAD[k];
  }

  // Set all of the data values for this individual using the
  // AD<AD<Scalar>> expression evaluator,
  //
  //     y ( theta, eta )  .
  //      i
  //
  try
  {
    predEvaluatorADAD.evalAllY(
      thetaOffsetInZ,
      nTheta,
      etaOffsetInZ,
      nEta,
      epsOffsetInZ,
      nEps,
      nObsRecordCurr,
      iCurr,
      zCurrADAD,
      fCurrADAD,
      yCurrADAD );
  }
  catch( ... )
  {
    // If any exceptions occur, then stop the taping by calling
    // Dependent, which is faster in this case than using the ADFun
    // constructor as is done below.
    ADFun< AD<Scalar> > yADADFunTemp;
    yADADFunTemp.Dependent( etaCurrADAD, yCurrADAD );
    ADFun<Scalar> fADFunTemp;
    fADFunTemp.Dependent( thetaCurrAD, fCurrAD );

    // Rethrow the exception that was originally thrown.
    throw;
  }

  // Stop the taping by constructing a differentiable function object
  // that corresponds to the mapping of eta to y.  This function is
  // defined by the relationships between eta and y that were
  // performed during the call to predEvaluatorADAD.evalAllY().
  ADFun< AD<Scalar> > yADADFunCurr( etaCurrADAD, yCurrADAD );


  //------------------------------------------------------------
  // Calculate the derivative of the data values.
  //------------------------------------------------------------

  int j;

  // Make sure this is the proper size.
  hCurrAD.resize( nObsRecordCurr * nEta );

  // Set the lengths of the argument and result vectors for Forward.
  std::vector< AD<Scalar> > u1AD( yADADFunCurr.Domain() );
  std::vector< AD<Scalar> > v1AD( yADADFunCurr.Range() );

  assert( nEta           == yADADFunCurr.Domain() );
  assert( nObsRecordCurr == yADADFunCurr.Range() );

  // Initialize the first order arguments.
  for ( k = 0; k < nEta; k++ )
  {
    u1AD[k] = 0;
  }

  // Calculate the first derivative of the data values
  //
  //                                             |
  //     h ( theta )  =  d     y ( theta, eta )  |          .
  //      i               eta   i                | eta = 0
  //
  // Use forward mode to calculate this derivative because there is
  // usually only one eta element and more than one y value, which
  // means there are more dependent than independent parameters.
  for ( k = 0; k < nEta; k++ )
  {
    // Evaluate
    //
    //             (k)
    //     v1  =  d     y( theta, eta )  .
    //             eta
    //
    u1AD[k] = 1;
    v1AD    = yADADFunCurr.Forward( 1, u1AD );
    u1AD[k] = 0;

    // Set the elements of the first derivative:
    //
    //                (k)
    //     h      =  d     y   ( theta, eta )  .
    //      (j,k)     eta   (j)
    //
    for ( j = 0; j < nObsRecordCurr; j++ )
    {
      hCurrAD[j + k * nObsRecordCurr] = v1AD[j];
    }

  }


  //------------------------------------------------------------
  // Prepare to calculate the derivative of the data mean and value derivatives.
  //------------------------------------------------------------

  int m;

  // Set the number of dependent variables.
  int nW = nObsRecordCurr * nEta + nObsRecordCurr;

  // Set the position of the first element of f in w.
  int fOffSetInW = nObsRecordCurr * nEta;

  // Make sure these have the proper size.
  wCurrAD    .resize( nW );
  f_thetaCurr.resize( nObsRecordCurr * nTheta );
  h_thetaCurr.resize( nObsRecordCurr * nEta * nTheta );

  // In order to calculate their derivatives together, set a vector
  // that contains the first derivative of the data values h_i and the
  // data mean values f_i for the current individual,
  //
  //                      -                     -
  //                     |  cvec[ h ( theta ) ]  |
  //                     |         i             |
  //     w ( theta )  =  |                       |  ,
  //      i              |     f ( theta )       |
  //                     |      i                |
  //                      -                     -
  //
  // where the cvec function corresponds to putting the elements of
  // h_i into w_i in column major order.
  int p = 0;
  for ( j = 0; j < nObsRecordCurr; j++ )
  {
    for ( m = 0; m < nEta; m++ )
    {
      // Set the elements of w_i that contain h_i,
      //
      //    w    ( theta )  =  h      ( theta )  .
      //     i(p)               i(j,m)
      //
      wCurrAD[p++] = hCurrAD[j + m * nObsRecordCurr];
    }
  }
  for ( j = 0; j < nObsRecordCurr; j++ )
  {
    // Set the elements of w_i that contain f_i.
    wCurrAD[fOffSetInW + j] = fCurrAD[j];
  }

  // Stop the taping by constructing a differentiable function object
  // that corresponds to the mapping of theta to w.
  ADFun<Scalar> wADFunCurr( thetaCurrAD, wCurrAD );


  //------------------------------------------------------------
  // Calculate the derivative of the data mean and data value derivatives.
  //------------------------------------------------------------

  int q;

  // Set the number of rows in the second derivatives that will 
  // be calculated.  Note that before the second derivatives are
  // calculated, the first derivatives are converted to a column
  // vector that contains the derivative's elements in row major
  // order, i.e., an rvec operation is performed on them.
  int nH_thetaRow   = nObsRecordCurr * nEta;

  // Set the lengths of the argument and result vectors for Forward.
  std::vector<Scalar> u1( wADFunCurr.Domain() );
  std::vector<Scalar> v1( wADFunCurr.Range() );

  assert( nTheta == wADFunCurr.Domain() );
  assert( nW     == wADFunCurr.Range() );

  // Initialize the first order arguments.
  for ( k = 0; k < nTheta; k++ )
  {
    u1[k] = 0;
  }

  // In order to calculate 
  //
  //     d       f ( theta )
  //      theta   i
  //
  // and
  //
  //     d       h ( theta )  ,
  //      theta   i
  //
  // calculate the first derivative of the dependent variables
  //
  //                                      -                     -
  //                                     |  cvec[ h ( theta ) ]  |
  //                                     |         i             |
  //     d       w ( theta )  =  d       |                       |  .
  //      theta   i               theta  |     f ( theta )       |
  //                                     |      i                |
  //                                      -                     -
  //
  // Use forward mode to calculate this derivative because there are
  // usually more y values that theta elements, which means there are
  // more dependent than independent parameters.
  for ( k = 0; k < nTheta; k++ )
  {
    // Evaluate
    //
    //             (k)
    //     v1  =  d       w ( theta )  .
    //             theta   i
    //
    u1[k] = 1;
    v1    = wADFunCurr.Forward( 1, u1 );
    u1[k] = 0;

    p = 0;

    for ( j = 0; j < nObsRecordCurr; j++ )
    {
      for ( m = 0; m < nEta; m++ )
      {
        // Note that an rvec operation is performed on h_i before its
        // derivative is calculated.
        //                                            -             -
        //                                           |               |  
        //     d       h ( theta )  =  d       rvec  |  h ( theta )  |  .
        //      theta   i               theta        |   i           |  
        //                                            -             -
        //
        // Set the position of this element in the rvec version of h_i.
        q = m + j * nEta;

        // Set the elements of the first derivative of h,
        //
        //      (k)                 (k)  
        //     d       h        =  d       w    ( theta )  .
        //      theta   i(j,m)      theta   i(p)
        //
        h_thetaCurr[q + k * nH_thetaRow] = v1[p++];
      }
    }

    // Set the elements of the first derivative of f,
    //
    //      (k)                        (k)
    //     d       f    ( theta )  =  d       w                 ( theta )  .
    //      theta   i(j)               theta   i(fOffSetInW + j)
    //
    for ( j = 0; j < nObsRecordCurr; j++ )
    {
      f_thetaCurr[j + k * nObsRecordCurr] = v1[fOffSetInW + j];
    }

  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // See if Scalar versions of f and h need to be set.
  if ( !isFAndHCurrOk )
  {
    // Make sure these are the proper size.
    fCurr.resize( nObsRecordCurr );
    hCurr.resize( nObsRecordCurr * nEta );

    // Set the Scalar version of f.
    for ( k = 0; k < nObsRecordCurr; k++ )
    {
      fCurr[k] = Value( fCurrAD[k] );
    }

    // Set the Scalar version of h.
    for ( k = 0; k < nEta; k++ )
    {
      for ( j = 0; j < nObsRecordCurr; j++ )
      {
        hCurr[j + k * nObsRecordCurr] = Value( hCurrAD[j + k * nObsRecordCurr] );
      }
    }

    isFAndHCurrOk = true;
  }

  isFAndH_thetaCurrOk = true;
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

template<class Scalar>
void IndPredModelBase<Scalar>::doDataMean( SPK_VA::valarray<Scalar>& ret ) const 
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
  // Prepare to set the data mean.
  //------------------------------------------------------------

  // Calculate the data mean values for the current individual,
  //
  //     f ( theta )  .
  //      i
  //
  // This function sets the value for fCurr.
  evalFAndH();


  //------------------------------------------------------------
  // Set the data mean.
  //------------------------------------------------------------

  int j;

  // Set
  //
  //     f    ( b  )  =  f    ( theta )  .
  //      i(j)   i        i(j)
  //
  for ( j = 0; j < nObsRecordCurr; j++ )
  {
    dataMeanCurr[j] = fCurr[j];
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

template<class Scalar>
bool IndPredModelBase<Scalar>::doDataMean_indPar( SPK_VA::valarray<double>& ret ) const 
{
  throw SpkException(
    SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR,
    "This function should not be used because it has not been implemented for arbitrary types.",
    __LINE__,
    __FILE__ );
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

template<class Scalar>
void IndPredModelBase<Scalar>::doDataVariance( SPK_VA::valarray<Scalar>& ret ) const 
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
  // Prepare to calculate the date variance.
  //------------------------------------------------------------

  // Set the derivative of the data values for the current individual,
  //
  //                                             |
  //     h ( theta )  =  d     y ( theta, eta )  |          .
  //      i               eta   i                | eta = 0
  //
  // This function sets the value for hCurr.
  evalFAndH();

  // Get the current value for omega.
  pOmegaCurr->cov( omegaCurr );


  //------------------------------------------------------------
  // Calculate the date variance.
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

    // Make sure that the value is finite.
    //
    // Note that this check is done after the NaN check because
    // NaN's are unnormalized.
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

template<class Scalar>
bool IndPredModelBase<Scalar>::doDataVariance_indPar( SPK_VA::valarray<double>& ret ) const 
{
  throw SpkException(
    SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR,
    "This function should not be used because it has not been implemented for arbitrary types.",
    __LINE__,
    __FILE__ );
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

template<class Scalar>
void IndPredModelBase<Scalar>::doDataVarianceInv( SPK_VA::valarray<Scalar>& ret ) const 
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

template<class Scalar>
bool IndPredModelBase<Scalar>::doDataVarianceInv_indPar( SPK_VA::valarray<double>& ret ) const 
{
  throw SpkException(
    SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR,
    "This function should not be used because it has not been implemented for arbitrary types.",
    __LINE__,
    __FILE__ );
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

template<class Scalar>
void IndPredModelBase<Scalar>::getIndParLimits(
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

template<class Scalar>
void IndPredModelBase<Scalar>::getIndParStep( SPK_VA::valarray<double>&  indParStep ) const
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
template<class Scalar>
int IndPredModelBase<Scalar>::getNIndPar() const
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

template<class Scalar>
void IndPredModelBase<Scalar>::getIndPar( SPK_VA::valarray<Scalar>& ret ) const 
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

template<class Scalar>
void IndPredModelBase<Scalar>::getTheta( SPK_VA::valarray<Scalar>& ret ) const 
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

template<class Scalar>
void IndPredModelBase<Scalar>::getEta( SPK_VA::valarray<Scalar>& ret ) const 
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

template<class Scalar>
void IndPredModelBase<Scalar>::getOmega( SPK_VA::valarray<Scalar>& ret ) const 
{
  ret.resize( pOmegaCurr->getNPar() );

  // Get the current value for omega.
  pOmegaCurr->cov( omegaCurr );

  // Return its minimal representation.
  pOmegaCurr->calcCovMinRep( omegaCurr, ret );
}


/*************************************************************************
 *
 * Function: getOmega
 *
 *//**
 * Gets a non-const reference to the current value for omega.
 */
/*************************************************************************/

template<class Scalar>
const Cov<Scalar>& IndPredModelBase<Scalar>::getOmega() const
{
  return *pOmegaCurr;
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

template<class Scalar>
void IndPredModelBase<Scalar>::getStandardPar( SPK_VA::valarray<double>& ret ) const 
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
  valarray<Scalar> omegaMinRepTemp( nOmegaPar );
  pOmegaCurr->calcCovMinRep( omegaCurr, omegaMinRepTemp );


  //------------------------------------------------------------
  // Set the vector of standard parameters.
  //------------------------------------------------------------

  int k;

  // Set the elements that contain theta.
  for ( k = 0; k < nTheta; k++ )
  {
    scalarToDouble( thetaCurr[k], ret[k + thetaOffsetInIndPar] );
  }

  // Set the elements that contain the minimal representation
  // for omega.
  for ( k = 0; k < nOmegaPar; k++ )
  {
    scalarToDouble( omegaMinRepTemp[k], ret[k + omegaParOffsetInIndPar] );
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

template<class Scalar>
void IndPredModelBase<Scalar>::getStandardPar_indPar( SPK_VA::valarray<double>& ret ) const 
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

template<class Scalar>
void IndPredModelBase<Scalar>::getStandardParMask( 
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



/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

// Declare double versions of these functions.
template IndPredModelBase<double>::IndPredModelBase(
    PredBase< double >&                          predEvaluatorIn,
    PredBase< CppAD::AD<double> >&               predEvaluatorADIn,
    PredBase< CppAD::AD< CppAD::AD<double> > >&  predEvaluatorADADIn,
    int                              nThetaIn,
    const SPK_VA::valarray<double>&  thetaLowIn,
    const SPK_VA::valarray<double>&  thetaUpIn,
    const SPK_VA::valarray<double>&  thetaCurrIn,
    int                              nEtaIn,
    covStruct                        omegaStructIn,
    const SPK_VA::valarray<double>&  omegaMinRepIn );

template IndPredModelBase<double>::IndPredModelBase(
    PredBase< double >&                          predEvaluatorIn,
    PredBase< CppAD::AD<double> >&               predEvaluatorADIn,
    PredBase< CppAD::AD< CppAD::AD<double> > >&  predEvaluatorADADIn,
    int                              nThetaIn,
    const SPK_VA::valarray<double>&  thetaLowIn,
    const SPK_VA::valarray<double>&  thetaUpIn,
    const SPK_VA::valarray<double>&  thetaCurrIn,
    int                              nEtaIn,
    covStruct                        omegaStructIn,
    const SPK_VA::valarray<double>&  omegaMinRepIn,
    const SPK_VA::valarray<bool>&    omegaMinRepFixedIn );

template IndPredModelBase<double>::IndPredModelBase(
    PredBase< double >&                          predEvaluatorIn,
    PredBase< CppAD::AD<double> >&               predEvaluatorADIn,
    PredBase< CppAD::AD< CppAD::AD<double> > >&  predEvaluatorADADIn,
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
    const SPK_VA::valarray<bool>&    omegaBlockSameAsPrev );

template IndPredModelBase<double>::~IndPredModelBase();

template void IndPredModelBase<double>::doSelectIndividual( int iIn );

template void IndPredModelBase<double>::doSetIndPar( const SPK_VA::valarray<double>& indParIn );

template void IndPredModelBase<double>::setTheta( const SPK_VA::valarray<double>& thetaIn );
template void IndPredModelBase<double>::setOmega( const SPK_VA::valarray<double>& omegaIn );

template void IndPredModelBase<double>::invalidateCache() const;

template bool IndPredModelBase<double>::getUsedCachedDataMean()               const;
template bool IndPredModelBase<double>::getUsedCachedDataMean_indPar()        const;
template bool IndPredModelBase<double>::getUsedCachedDataVariance()           const;
template bool IndPredModelBase<double>::getUsedCachedDataVariance_indPar()    const;
template bool IndPredModelBase<double>::getUsedCachedDataVarianceInv()        const;
template bool IndPredModelBase<double>::getUsedCachedDataVarianceInv_indPar() const;
template bool IndPredModelBase<double>::getUsedCachedFAndH()                  const;
template bool IndPredModelBase<double>::getUsedCachedFAndH_theta()            const;
template bool IndPredModelBase<double>::getUsedCachedOmega()                  const;
template bool IndPredModelBase<double>::getUsedCachedOmega_omegaPar()         const;

template void IndPredModelBase<double>::doDataMean              ( SPK_VA::valarray<double>& ret ) const;
template bool IndPredModelBase<double>::doDataMean_indPar       ( SPK_VA::valarray<double>& ret ) const;

template void IndPredModelBase<double>::doDataVariance          ( SPK_VA::valarray<double>& ret ) const;
template bool IndPredModelBase<double>::doDataVariance_indPar   ( SPK_VA::valarray<double>& ret ) const;

template void IndPredModelBase<double>::doDataVarianceInv       ( SPK_VA::valarray<double>& ret ) const;
template bool IndPredModelBase<double>::doDataVarianceInv_indPar( SPK_VA::valarray<double>& ret ) const;

template void IndPredModelBase<double>::evalFAndH() const;

template void IndPredModelBase<double>::evalFAndH_theta()  const;

template void IndPredModelBase<double>::getIndParLimits(
    SPK_VA::valarray<double>& indParLow,
    SPK_VA::valarray<double>& indParUp ) const;

template void IndPredModelBase<double>::getIndParStep( SPK_VA::valarray<double>& indParStep ) const;

template int IndPredModelBase<double>::getNIndPar() const;

template void IndPredModelBase<double>::getIndPar( SPK_VA::valarray<double>& ret ) const;

template void IndPredModelBase<double>::getTheta( SPK_VA::valarray<double>& ret ) const;
template void IndPredModelBase<double>::getEta  ( SPK_VA::valarray<double>& ret ) const;
template void IndPredModelBase<double>::getOmega( SPK_VA::valarray<double>& ret ) const;

template const Cov<double>& IndPredModelBase<double>::getOmega() const;

template void IndPredModelBase<double>::getStandardPar       ( SPK_VA::valarray<double>& ret ) const;
template void IndPredModelBase<double>::getStandardPar_indPar( SPK_VA::valarray<double>& ret ) const;

template void IndPredModelBase<double>::getStandardParMask( 
    const SPK_VA::valarray<bool>& indParMaskIn,
    SPK_VA::valarray<bool>&       standardParMaskOut ) const;

// Declare CppAD::AD<double> versions of these functions.
template IndPredModelBase< CppAD::AD<double> >::IndPredModelBase(
    PredBase< CppAD::AD<double> >&                            predEvaluatorIn,
    PredBase< CppAD::AD< CppAD::AD<double> > >&               predEvaluatorADIn,
    PredBase< CppAD::AD< CppAD::AD< CppAD::AD<double> > > >&  predEvaluatorADADIn,
    int                              nThetaIn,
    const SPK_VA::valarray<double>&  thetaLowIn,
    const SPK_VA::valarray<double>&  thetaUpIn,
    const SPK_VA::valarray<double>&  thetaCurrIn,
    int                              nEtaIn,
    covStruct                        omegaStructIn,
    const SPK_VA::valarray<double>&  omegaMinRepIn );

template IndPredModelBase< CppAD::AD<double> >::IndPredModelBase(
    PredBase< CppAD::AD<double> >&                            predEvaluatorIn,
    PredBase< CppAD::AD< CppAD::AD<double> > >&               predEvaluatorADIn,
    PredBase< CppAD::AD< CppAD::AD< CppAD::AD<double> > > >&  predEvaluatorADADIn,
    int                              nThetaIn,
    const SPK_VA::valarray<double>&  thetaLowIn,
    const SPK_VA::valarray<double>&  thetaUpIn,
    const SPK_VA::valarray<double>&  thetaCurrIn,
    int                              nEtaIn,
    covStruct                        omegaStructIn,
    const SPK_VA::valarray<double>&  omegaMinRepIn,
    const SPK_VA::valarray<bool>&    omegaMinRepFixedIn );

template IndPredModelBase< CppAD::AD<double> >::IndPredModelBase(
    PredBase< CppAD::AD<double> >&                            predEvaluatorIn,
    PredBase< CppAD::AD< CppAD::AD<double> > >&               predEvaluatorADIn,
    PredBase< CppAD::AD< CppAD::AD< CppAD::AD<double> > > >&  predEvaluatorADADIn,
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
    const SPK_VA::valarray<bool>&    omegaBlockSameAsPrev );

template IndPredModelBase< CppAD::AD<double> >::~IndPredModelBase();

template void IndPredModelBase< CppAD::AD<double> >::doSelectIndividual( int iIn );

template void IndPredModelBase< CppAD::AD<double> >::doSetIndPar( const SPK_VA::valarray< CppAD::AD<double> >& indParIn );

template void IndPredModelBase< CppAD::AD<double> >::setTheta( const SPK_VA::valarray< CppAD::AD<double> >& thetaIn );
template void IndPredModelBase< CppAD::AD<double> >::setOmega( const SPK_VA::valarray< CppAD::AD<double> >& omegaIn );


template void IndPredModelBase< CppAD::AD<double> >::invalidateCache() const;

template bool IndPredModelBase< CppAD::AD<double> >::getUsedCachedDataMean()               const;
template bool IndPredModelBase< CppAD::AD<double> >::getUsedCachedDataMean_indPar()        const;
template bool IndPredModelBase< CppAD::AD<double> >::getUsedCachedDataVariance()           const;
template bool IndPredModelBase< CppAD::AD<double> >::getUsedCachedDataVariance_indPar()    const;
template bool IndPredModelBase< CppAD::AD<double> >::getUsedCachedDataVarianceInv()        const;
template bool IndPredModelBase< CppAD::AD<double> >::getUsedCachedDataVarianceInv_indPar() const;
template bool IndPredModelBase< CppAD::AD<double> >::getUsedCachedFAndH()                  const;
template bool IndPredModelBase< CppAD::AD<double> >::getUsedCachedFAndH_theta()            const;
template bool IndPredModelBase< CppAD::AD<double> >::getUsedCachedOmega()                  const;
template bool IndPredModelBase< CppAD::AD<double> >::getUsedCachedOmega_omegaPar()         const;

template void IndPredModelBase< CppAD::AD<double> >::doDataMean              ( SPK_VA::valarray< CppAD::AD<double> >& ret ) const;
template bool IndPredModelBase< CppAD::AD<double> >::doDataMean_indPar       ( SPK_VA::valarray<double>& ret ) const;

template void IndPredModelBase< CppAD::AD<double> >::doDataVariance          ( SPK_VA::valarray< CppAD::AD<double> >& ret ) const;
template bool IndPredModelBase< CppAD::AD<double> >::doDataVariance_indPar   ( SPK_VA::valarray<double>& ret ) const;

template void IndPredModelBase< CppAD::AD<double> >::doDataVarianceInv       ( SPK_VA::valarray< CppAD::AD<double> >& ret ) const;
template bool IndPredModelBase< CppAD::AD<double> >::doDataVarianceInv_indPar( SPK_VA::valarray<double>& ret ) const;

template void IndPredModelBase< CppAD::AD<double> >::evalFAndH() const;

template void IndPredModelBase< CppAD::AD<double> >::evalFAndH_theta()  const;

template void IndPredModelBase< CppAD::AD<double> >::getIndParLimits(
    SPK_VA::valarray<double>& indParLow,
    SPK_VA::valarray<double>& indParUp ) const;

template void IndPredModelBase< CppAD::AD<double> >::getIndParStep( SPK_VA::valarray<double>& indParStep ) const;

template int IndPredModelBase< CppAD::AD<double> >::getNIndPar() const;

template void IndPredModelBase< CppAD::AD<double> >::getIndPar( SPK_VA::valarray< CppAD::AD<double> >& ret ) const;

template void IndPredModelBase< CppAD::AD<double> >::getTheta( SPK_VA::valarray< CppAD::AD<double> >& ret ) const;
template void IndPredModelBase< CppAD::AD<double> >::getEta  ( SPK_VA::valarray< CppAD::AD<double> >& ret ) const;
template void IndPredModelBase< CppAD::AD<double> >::getOmega( SPK_VA::valarray< CppAD::AD<double> >& ret ) const;

template const Cov< CppAD::AD<double> >& IndPredModelBase< CppAD::AD<double> >::getOmega() const;

template void IndPredModelBase< CppAD::AD<double> >::getStandardPar       ( SPK_VA::valarray<double>& ret ) const;
template void IndPredModelBase< CppAD::AD<double> >::getStandardPar_indPar( SPK_VA::valarray<double>& ret ) const;

template void IndPredModelBase< CppAD::AD<double> >::getStandardParMask( 
    const SPK_VA::valarray<bool>& indParMaskIn,
    SPK_VA::valarray<bool>&       standardParMaskOut ) const;

