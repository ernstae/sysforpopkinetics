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
 * @file: PopPredModel.cpp
 *
 *
 * Implements PopPredModel class.
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
#include "PopPredModel.h"
#include "isEqual.h"
#include "PredBase.h"

// SPK library header files.
#include <spk/AkronBtimesC.h>
#include <spk/allZero.h>
#include <spk/intToOrdinalString.h>
#include <spk/multiply.h>
#include <spk/replaceSubblock.h>
#include <spk/SpkException.h>
#include <spk/SpkModel.h>
#include <spk/SpkValarray.h>

// CppAD header files.
#include <CppAD/CppAD.h>
//#include <CppAD/include/Independent.h>

// Standard library header files.
#include <cassert>
#include <limits>
#include <cmath>
#include <sstream>
#include <vector>

using namespace CppAD;
using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: PopPredModel
 *
 *//**
 * Constructor for population level Pred models.
 *
 * After this constructor has completed the current population parameter
 * will be 
 * \f[
 *     \alpha =
 *       \left[ 
 *         \begin{array}{c}
 *           \mbox{thetaCurrIn} \\
 *           \mbox{omegaParCurr} \\
 *           \mbox{sigmaParCurr}
 *         \end{array}
 *       \right] ,
 * \f]
 * where omegaParCurr and sigmaParCurr are the covariance matrix 
 * parameters that correspond to the minimal representations for
 * omega and sigma, respectively, that are contained in omegaMinRepIn
 * and sigmaMinRepIn.
 *
 * In addition, the current individual parameter will be 
 * \f[
 *     b_i = \mbox{etaCurrIn} .
 * \f]
 */
/*************************************************************************/

PopPredModel::PopPredModel(
    PredBase< AD<double> >&          predEvaluatorIn,
    int                              nThetaIn,
    const SPK_VA::valarray<double>&  thetaLowIn,
    const SPK_VA::valarray<double>&  thetaUpIn,
    const SPK_VA::valarray<double>&  thetaCurrIn,
    int                              nEtaIn,
    const SPK_VA::valarray<double>&  etaCurrIn,
    int                              nEpsIn,
    covStruct                        omegaStructIn,
    const SPK_VA::valarray<double>&  omegaMinRepIn,
    covStruct                        sigmaStructIn,
    const SPK_VA::valarray<double>&  sigmaMinRepIn )
  :
  nTheta                              ( nThetaIn ),
  nEta                                ( nEtaIn ),
  nEps                                ( nEpsIn ),
  nZ                                  ( nThetaIn + nEtaIn + nEpsIn ),
  thetaOffsetInZ                      ( 0 ),
  etaOffsetInZ                        ( nThetaIn ),
  epsOffsetInZ                        ( nThetaIn + nEtaIn ),
  fOffsetInW                          ( 0 ),
  pOmegaCurr                          ( 0 ),
  pSigmaCurr                          ( 0 ),
  pPredADFunCurr                      ( 0 ),
  predEvaluator                       ( predEvaluatorIn ),
  zCurr                               ( nZ ),
  isDataMeanCurrOk                    ( false ),
  isDataMean_popParCurrOk             ( false ),
  isDataMean_indParCurrOk             ( false ),
  isDataVarianceCurrOk                ( false ),
  isDataVariance_popParCurrOk         ( false ),
  isDataVariance_indParCurrOk         ( false ),
  isDataVarianceInvCurrOk             ( false ),
  isDataVarianceInv_popParCurrOk      ( false ),
  isDataVarianceInv_indParCurrOk      ( false ),
  isIndParVariance_popParCurrOk       ( false ),
  isIndParVarianceInv_popParCurrOk    ( false ),
  isPredADFunCurrOk                   ( false ),
  isPredFirstDerivCurrOk              ( false ),
  isPredSecondDerivCurrOk             ( false ),
  usedCachedDataMean                  ( false ),
  usedCachedDataMean_popPar           ( false ),
  usedCachedDataMean_indPar           ( false ),
  usedCachedDataVariance              ( false ),
  usedCachedDataVariance_popPar       ( false ),
  usedCachedDataVariance_indPar       ( false ),
  usedCachedDataVarianceInv           ( false ),
  usedCachedDataVarianceInv_popPar    ( false ),
  usedCachedDataVarianceInv_indPar    ( false ),
  usedCachedIndParVariance_popPar     ( false ),
  usedCachedIndParVarianceInv_popPar  ( false ),
  usedCachedPredADFun                 ( false ),
  usedCachedPredFirstDeriv            ( false ),
  usedCachedPredSecondDeriv           ( false )
{
  //------------------------------------------------------------
  // Initialize quantities related to the omega covariance matrix.
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
  
    // Create a temporary version of omega that corresponds to the
    // minimal representation passed to this constructor.
    valarray<double> omegaTemp( nEta * nEta );
    pOmegaCurr->expandCovMinRep( omegaMinRepIn, omegaTemp );
    assert( omegaMinRepIn.size() == nOmegaPar );
  
    // Set the omega value maintained by the covariance class.
    pOmegaCurr->setCov( omegaTemp );
  
    // Save the initial value for the omega parameters.
    omegaParCurr.resize( nOmegaPar);
    pOmegaCurr->calcPar( omegaTemp, omegaParCurr );
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
  // Initialize quantities related to the sigma covariance matrix.
  //------------------------------------------------------------

  // Construct sigma, the covariance matrix for eps, with the
  // appropriate structure.
  if ( sigmaStructIn == DIAGONAL )
  {
    pSigmaCurr = new DiagCov( nEps );
  }
  else if ( sigmaStructIn == FULL )
  {
    pSigmaCurr = new FullCov( nEps );
  }
  else
  {
    throw SpkException(
     SpkError::SPK_USER_INPUT_ERR, 
     "Unknown covariance matrix type requested for Sigma.",
     __LINE__, 
     __FILE__ );
  }

  try
  {
    // Get the number of parameters required by the structure of
    // this covariance matrix.
    nSigmaPar = pSigmaCurr->getNPar();
  
    // Save the sigma value maintained by this class that corresponds
    // to the minimal representation passed to this constructor.
    sigmaCurr.resize( nEps * nEps );
    pSigmaCurr->expandCovMinRep( sigmaMinRepIn, sigmaCurr );
    assert( sigmaMinRepIn.size() == nSigmaPar );
  
    // Set the sigma value maintained by the covariance class.
    pSigmaCurr->setCov( sigmaCurr );
  
    // Save the initial value for the sigma parameters.
    sigmaParCurr.resize( nSigmaPar);
    pSigmaCurr->calcPar( sigmaCurr, sigmaParCurr );
  }
  catch( SpkException& e )
  {
    throw e.push(
      SpkError::SPK_UNKNOWN_ERR, 
      "The initialization of the Sigma covariance matrix failed.",
      __LINE__, 
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    throw SpkException(
      stde,
      "An standard exception was thrown during the initialization of the Sigma covariance matrix.",
      __LINE__, 
      __FILE__ );
  }  
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR, 
      "An unknown exception was thrown during the initialization of the Sigma covariance matrix.",
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

  // Set the current value for eta.
  etaCurr.resize( nEta );
  etaCurr = etaCurrIn;
  assert( etaCurrIn.size() == nEta );

  // For population level Pred models, eps is always equal to zero.
  epsCurr.resize( nEps );
  epsCurr = 0.0;


  //------------------------------------------------------------
  // Initialize quantities related to the individual.
  //------------------------------------------------------------

  // Set the current individual to a number different than zero
  // so that selectIndividual() won't use the cached values.
  iCurr = 1;

  // Set the current individual.
  doSelectIndividual( 0 );


  //------------------------------------------------------------
  // Initialize quantities related to the population parameter.
  //------------------------------------------------------------

  // The population parameter is composed of the parameters that are
  // optimized over when performing population level estimation,
  //
  //                -              -
  //               |   thetaCurr    |
  //               |                |
  //     alpha  =  |  omegaParCurr  |  .
  //               |                |
  //               |  sigmaParCurr  |
  //                -              -
  //
  nPopPar = nTheta + nOmegaPar + nSigmaPar;
  alphaCurr.resize( nPopPar );

  // Set the offsets for the parameters.
  thetaOffsetInPopPar    = 0;
  omegaParOffsetInPopPar = nTheta;
  sigmaParOffsetInPopPar = nTheta + nOmegaPar;

  int k;

  // Set the elements that correspond to theta.
  for ( k = 0; k < nTheta; k++ )
  {
    alphaCurr[k + thetaOffsetInPopPar] = thetaCurr[k];
  }

  // Set the parameters for the omega covariance matrix.
  for ( k = 0; k < nOmegaPar; k++ )
  {
    alphaCurr[k + omegaParOffsetInPopPar] = omegaParCurr[k];
  }

  // Set the parameters for the sigma covariance matrix.
  for ( k = 0; k < nSigmaPar; k++ )
  {
    alphaCurr[k + sigmaParOffsetInPopPar] = sigmaParCurr[k];
  }


  //------------------------------------------------------------
  // Initialize quantities related to the individual parameter.
  //------------------------------------------------------------

  // The individual parameter is the parameter that is optimized
  // over when performing individual level estimation,
  //
  //     b   =  etaCurr  .
  //      i
  //
  nIndPar = nEta;
  bCurr.resize( nIndPar );

  bCurr =etaCurr;
}


/*************************************************************************
 *
 * Function: ~PopPredModel
 *
 *//**
 * Destructor.
 */
/*************************************************************************/

PopPredModel::~PopPredModel()
{
  // Free the memory allocated for the omega covariance matrix.
  if ( pOmegaCurr )
  {
    delete pOmegaCurr;
  }

  // Free the memory allocated for the sigma covariance matrix.
  if ( pSigmaCurr )
  {
    delete pSigmaCurr;
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

void PopPredModel::doSelectIndividual( int iIn )
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

  // Set the number of events for this individual.  Note that
  // for now all events are assumed to be observation events.
  nEventCurr = predEvaluator.getNObservs( iCurr );


  //------------------------------------------------------------
  // Update the Pred block dependent variable information.
  //------------------------------------------------------------

  // Set quantities related to the vector of dependent variables
  // for the current individual,
  //
  //                 -                      -
  //                |  f( theta, eta )       |
  //     w( z )  =  |                        |  .
  //                |  y( theta, eta, eps )  |
  //                 -                      -
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
 * Function: doSetPopPar
 *
 *//**
 * Sets the current value for the population parameter,
 * \f[
 *     \alpha =
 *       \left[ 
 *         \begin{array}{c}
 *           \mbox{thetaCurr} \\
 *           \mbox{omegaParCurr} \\
 *           \mbox{sigmaParCurr}
 *         \end{array}
 *       \right] .
 * \f]
 * These are the parameters that are optimized over when performing
 * population level estimation.
 */
/*************************************************************************/

void PopPredModel::doSetPopPar( const SPK_VA::valarray<double>& alphaIn ) 
{ 
  //------------------------------------------------------------
  // See if the population parameter has changed.
  //------------------------------------------------------------

  // Don't do anything if the population parameter has not changed.
  if ( isEqual( alphaIn, alphaCurr ) )
  { 
    return;
  }

  // Since the population parameter has changed, any cached 
  // values are no longer valid.
  invalidateCache();


  //------------------------------------------------------------
  // Update the population parameter.
  //------------------------------------------------------------

  alphaCurr = alphaIn; 
  assert( alphaIn.size() == nPopPar );


  //------------------------------------------------------------
  // Update the parameters that appear in the Pred block.
  //------------------------------------------------------------

  int k;

  // Set the elements of theta.
  for ( k = 0; k < nTheta; k++ )
  {
    thetaCurr[k] = alphaCurr[k + thetaOffsetInPopPar];
  }


  //------------------------------------------------------------
  // Update the parameters for the covariance matrices.
  //------------------------------------------------------------

  // Set the parameters for the omega covariance matrix.
  for ( k = 0; k < nOmegaPar; k++ )
  {
    omegaParCurr[k] = alphaCurr[k + omegaParOffsetInPopPar];
  }

  // Pass the new omega parameters to the covariance object.
  pOmegaCurr->setPar( omegaParCurr );

  // Set the parameters for the sigma covariance matrix.
  for ( k = 0; k < nSigmaPar; k++ )
  {
    sigmaParCurr[k] = alphaCurr[k + sigmaParOffsetInPopPar];
  }

  // Pass the new sigma parameters to the covariance object.
  pSigmaCurr->setPar( sigmaParCurr );

}


/*************************************************************************
 *
 * Function: doSetIndPar
 *
 *//**
 * Sets the current value for the individual parameter,
 * \f[
 *     b_i = \mbox{etaCurr} .
 * \f]
 * This is the parameter that is optimized over when performing
 * individual level estimation.
 */
/*************************************************************************/

void PopPredModel::doSetIndPar( const SPK_VA::valarray<double>& bIn ) 
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
  // Update the parameter that appears in the Pred block.
  //------------------------------------------------------------

  etaCurr = bCurr;
}


/*************************************************************************
 *
 * Function: invalidateCache
 *
 *//**
 * Invalidates all of the values stored in the cache.
 */
/*************************************************************************/

void PopPredModel::invalidateCache() const
{
  isDataMeanCurrOk                 = false;
  isDataMean_popParCurrOk          = false;
  isDataMean_indParCurrOk          = false;
  isDataVarianceCurrOk             = false;
  isDataVariance_popParCurrOk      = false;
  isDataVariance_indParCurrOk      = false;
  isDataVarianceInvCurrOk          = false;
  isDataVarianceInv_popParCurrOk   = false;
  isDataVarianceInv_indParCurrOk   = false;
  isIndParVariance_popParCurrOk    = false;
  isIndParVarianceInv_popParCurrOk = false;
  isPredADFunCurrOk                = false;
  isPredFirstDerivCurrOk           = false;
  isPredSecondDerivCurrOk          = false;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataMean
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedDataMean() const
{
  return usedCachedDataMean;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataMean_popPar
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedDataMean_popPar() const
{
  return usedCachedDataMean_popPar;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataMean_indPar
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedDataMean_indPar() const
{
  return usedCachedDataMean_indPar;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataVariance
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedDataVariance() const
{
  return usedCachedDataVariance;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataVariance_popPar
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedDataVariance_popPar() const
{
  return usedCachedDataVariance_popPar;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataVariance_indPar
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedDataVariance_indPar() const
{
  return usedCachedDataVariance_indPar;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataVarianceInv
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedDataVarianceInv() const
{
  return usedCachedDataVarianceInv;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataVarianceInv_popPar
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedDataVarianceInv_popPar() const
{
  return usedCachedDataVarianceInv_popPar;
}


/*************************************************************************
 *
 * Function: getUsedCachedDataVarianceInv_indPar
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedDataVarianceInv_indPar() const
{
  return usedCachedDataVarianceInv_indPar;
}


/*************************************************************************
 *
 * Function: getUsedCachedIndParVariance
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedIndParVariance() const
{
  return usedCachedIndParVariance;
}


/*************************************************************************
 *
 * Function: getUsedCachedIndParVariance_popPar
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedIndParVariance_popPar() const
{
  return usedCachedIndParVariance_popPar;
}


/*************************************************************************
 *
 * Function: getUsedCachedIndParVarianceInv
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedIndParVarianceInv() const
{
  return usedCachedIndParVarianceInv;
}


/*************************************************************************
 *
 * Function: getUsedCachedIndParVarianceInv_popPar
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedIndParVarianceInv_popPar() const
{
  return usedCachedIndParVarianceInv_popPar;
}


/*************************************************************************
 *
 * Function: getUsedCachedPredADFun
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedPredADFun() const
{
  return usedCachedPredADFun;
}


/*************************************************************************
 *
 * Function: getUsedCachedPredFirstDeriv
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedPredFirstDeriv() const
{
  return usedCachedPredFirstDeriv;
}


/*************************************************************************
 *
 * Function: getUsedCachedPredSecondDeriv
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedPredSecondDeriv() const
{
  return usedCachedPredSecondDeriv;
}


/*************************************************************************
 *
 * Function: getUsedCachedOmega
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedOmega() const
{
  return pOmegaCurr->getUsedCachedCov();
}


/*************************************************************************
 *
 * Function: getUsedCachedOmega_omegaPar
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedOmega_omegaPar() const
{
  return pOmegaCurr->getUsedCachedCov_par();
}


/*************************************************************************
 *
 * Function: getUsedCachedOmegaInv
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedOmegaInv() const
{
  return pOmegaCurr->getUsedCachedInv();
}


/*************************************************************************
 *
 * Function: getUsedCachedOmegaInv_omegaPar
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedOmegaInv_omegaPar() const
{
  return pOmegaCurr->getUsedCachedInv_par();
}


/*************************************************************************
 *
 * Function: getUsedCachedSigma
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedSigma() const
{
  return pSigmaCurr->getUsedCachedCov();
}


/*************************************************************************
 *
 * Function: getUsedCachedSigma_sigmaPar
 *
 *************************************************************************/

bool PopPredModel::getUsedCachedSigma_sigmaPar() const
{
  return pSigmaCurr->getUsedCachedCov_par();
}


/*************************************************************************
 *
 * Function: evalAllPred
 *
 *//**
 * This function evaluates the predicted values for the data for
 * all of the events for the current individual.
 *
 * It does this by evaluating the expressions from the Pred block
 * for each event.
 *
 * Note that this function combines the parameters theta, eta, 
 * and eps into a single vector of independent variables,
 * \f[
 *     z =
 *       \left[ 
 *         \begin{array}{c}
 *           \mbox{theta} \\
 *           \mbox{eta} \\
 *           \mbox{eps}
 *         \end{array}
 *       \right] .
 * \f]
 * In addition, this function combines the model functions f and y
 * into a single vector of dependent variables,
 * \f[
 *     w(z) =
 *       \left[ 
 *         \begin{array}{c}
 *           f(\mbox{theta}, \mbox{eta}) \\
 *           y(\mbox{theta}, \mbox{eta}, \mbox{eps})
 *         \end{array}
 *       \right] .
 * \f]
 */
/*************************************************************************/

void PopPredModel::evalAllPred() const
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

  // Set the current values for the eps dependent variables.
  for ( k = 0; k < nEps; k++ )
  {
    zCurr[k + epsOffsetInZ] = epsCurr[k];
  }

  // Declare the independent variables.  This specifies the
  // domain for the differentiable function object that will
  // be constructed after the expressions in the Pred block
  // have been evaluated.
  CppAD::Independent( zCurr );


  //------------------------------------------------------------
  // Evaluate all of the predicted values for the data.
  //------------------------------------------------------------

  // This message will be used if an error occurs.
  string taskMessage;

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

    taskMessage = "during the evaluation of the " +
      intToOrdinalString( j, ZERO_IS_FIRST_INT ) + " predicted value" + 
      "\n" + 
      "for the " + intToOrdinalString( iCurr, ZERO_IS_FIRST_INT ) +
      " individual's data.";

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
          taskMessage ).c_str(),
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
 *//**
 * Evaluates first derivatives of the Pred block expressions.
 *
 * In particular, this function evaluates the following first derivatives:
 * \f[
 *     \partial_{\mbox{theta}} \; f(\mbox{theta}, \mbox{eta}) ,
 * \f]
 * \f[
 *     \partial_{\mbox{eta}} \; f(\mbox{theta}, \mbox{eta}) ,
 * \f]
 * and
 * \f[
 *     h(\mbox{theta}, \mbox{eta}) =
 *       \partial_{\mbox{eps}} \; y(\mbox{theta}, \mbox{eta}, \mbox{eps})
 *         \left|_{\mbox{eps}=0} \right. .
 * \f]
 */
/*************************************************************************/

void PopPredModel::evalPredFirstDeriv() const
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

  // Before first derivatives can be calculated, the predicted
  // values for all of the events for the current individual 
  // must be evaluated.
  evalAllPred();

  // Make sure these are the proper sizes.
  f_thetaCurr.resize( nEventCurr * nTheta );
  f_etaCurr  .resize( nEventCurr * nEta );
  hCurr      .resize( nEventCurr * nEps );

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
  // y at the current values for theta and eta.
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
  //     d       f( theta, eta )  .
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
    //     d       f   ( theta, eta )  .
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
  //     d     f( theta, eta )  .
  //      eta
  //
  for ( k = 0; k < nEta; k++ )
  {
    u1[k + etaOffsetInZ] = 1.0;

    // Evaluate the first order Taylor coefficients for pred(U).
    v1 = pPredADFunCurr->Forward( 1, u1 );

    // Set the elements of the first derivative:
    //
    //      (k)
    //     d     f   ( theta, eta )  .
    //      eta   (j)
    //
    for ( j = 0; j < nEventCurr; j++ )
    {
      f_etaCurr[j + k * nEventCurr] = v1[j + fOffsetInW];
    }

    u1[k + etaOffsetInZ] = 0.0;
  }

  // Calculate the first derivative
  //
  //                                                     |
  //     h( theta, eta )  =  d     y( theta, eta, eps )  |          .
  //                          eps                        | eps = 0
  //
  for ( k = 0; k < nEps; k++ )
  {
    u1[k + epsOffsetInZ] = 1.0;

    // Evaluate the first order Taylor coefficients for pred(U).
    v1 = pPredADFunCurr->Forward( 1, u1 );

    // Set the elements of the first derivative:
    //
    //                               (k)                           |              
    //     h     ( theta, eta )  =  d     y   ( theta, eta, eps )  |          .
    //      (j,k)                    eps   (j)                     | eps = 0
    //
    for ( j = 0; j < nEventCurr; j++ )
    {
      hCurr[j + k * nEventCurr] = v1[j + yOffsetInW];
    }

    u1[k + epsOffsetInZ] = 0.0;
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
 *         \partial_{\mbox{eps}} \; y(\mbox{theta}, \mbox{eta}, \mbox{eps})
 *           \left|_{\mbox{eps}=0} \right. 
 *       \right] .
 * \f]
 *
 * In addition, this function evaluates the following first derivatives:
 * \f[
 *     \partial_{\mbox{theta}} \; f(\mbox{theta}, \mbox{eta}) ,
 * \f]
 * \f[
 *     \partial_{\mbox{eta}} \; f(\mbox{theta}, \mbox{eta}) ,
 * \f]
 * and
 * \f[
 *     h(\mbox{theta}, \mbox{eta}) =
 *       \partial_{\mbox{eps}} \; y(\mbox{theta}, \mbox{eta}, \mbox{eps})
 *         \left|_{\mbox{eps}=0} \right. .
 * \f]
 */
/*************************************************************************/

void PopPredModel::evalPredSecondDeriv() const
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
  // predicted values for all of the events for the current 
  // individual must be evaluated.
  evalAllPred();

  // Set the number of rows in the second derivatives that will 
  // be calculated.  Note that before the second derivatives are
  // calculated, the first derivatives are converted to a column
  // vector that contains the derivative's elements in row major
  // order, i.e., an rvec operation is performed on them.
  int nH_thetaRow = nEventCurr * nEps;
  int nH_etaRow   = nEventCurr * nEps;

  // Make sure these are the proper sizes.
  f_thetaCurr.resize( nEventCurr * nTheta );
  f_etaCurr  .resize( nEventCurr * nEta );
  hCurr      .resize( nEventCurr * nEps );
  h_thetaCurr.resize( nEventCurr * nEps * nTheta );
  h_etaCurr  .resize( nEventCurr * nEps * nEta );

  // Set the lengths of the Taylor coefficient column vectors.
  std::vector<double> u1( pPredADFunCurr->Domain() );
  std::vector<double> u2( pPredADFunCurr->Domain() );
  std::vector<double> v1( pPredADFunCurr->Range() );
  std::vector<double> v2( pPredADFunCurr->Range() );

  // These will hold one-half times the diagonal elements of
  // the second derivatives.
  std::vector<double> y_theta_thetaDiagTerm( nTheta * nEventCurr );
  std::vector<double> y_eta_etaDiagTerm    ( nEta   * nEventCurr );
  std::vector<double> y_eps_epsDiagTerm    ( nEps   * nEventCurr );


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
  //     d       f( theta, eta )  
  //      theta
  //
  // and one-half times the diagonal elements of the second derivatives,
  //
  //      1   (k)    (k)      
  //     --- d      d       y   ( theta, eta, eps )  , for all k and p.
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
    //     d       f   ( theta, eta )  .
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
    //     y_theta_thetaDiagTerm        =  --- d      d       y   ( theta, eta, eps )  .
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
  //     d     f( theta, eta )  
  //      eta
  //
  // and one-half times the diagonal elements of the second derivatives,
  //
  //      1   (k)   (k)      
  //     --- d     d     y   ( theta, eta, eps )  , for all k and p.
  //      2   eta   eta   (p)
  //
  for ( k = 0; k < nEta; k++ )
  {
    u1[k + etaOffsetInZ] = 1.0;

    // Evaluate the first order Taylor coefficients for pred(U).
    v1 = pPredADFunCurr->Forward( 1, u1 );

    // Set the elements of the first derivative:
    //
    //      (k)
    //     d     f   ( theta, eta )  .
    //      eta   (j)
    //
    for ( j = 0; j < nEventCurr; j++ )
    {
      f_etaCurr[j + k * nEventCurr] = v1[j + fOffsetInW];
    }

    // Evaluate the second order Taylor coefficients for pred(U).
    v2 = pPredADFunCurr->Forward( 2, u2 );
  
    // Set the elements of one-half times the diagonal elements of
    // the second derivatives:
    //
    //                                  1   (k)   (k)
    //     y_eta_etaDiagTerm        =  --- d     d     y   ( theta, eta, eps )  .
    //                      (k, p)      2   eta   eta   (p)
    //
    for ( p = 0; p < nEventCurr; p++ )
    {
      y_eta_etaDiagTerm[k + p * nEta] = v2[p + yOffsetInW];
    }

    u1[k + etaOffsetInZ] = 0.0;
  }

  // Calculate the first derivative
  //
  //                                                     |
  //     h( theta, eta )  =  d     y( theta, eta, eps )  |
  //                          eps                        | eps = 0
  //
  // and one-half times the diagonal elements of the second derivatives,
  //
  //      1   (k)  (k)    
  //     --- d    d     y   ( theta, eta, eps )  , for all k and p.
  //      2   eps  eps   (p)
  //
  for ( k = 0; k < nEps; k++ )
  {
    u1[k + epsOffsetInZ] = 1.0;

    // Evaluate the first order Taylor coefficients for pred(U).
    v1 = pPredADFunCurr->Forward( 1, u1 );

    // Set the elements of the first derivative:
    //
    //                               (k)                           |
    //     h     ( theta, eta )  =  d     y   ( theta, eta, eps )  |          .
    //      (j,k)                    eps   (j)                     | eps = 0
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
    //     y_eps_epsDiagTerm        =  --- d    d     y   ( theta, eta, eps )  .
    //                      (k, p)      2   eps  eps   (p)
    //
    for ( p = 0; p < nEventCurr; p++ )
    {
      y_eps_epsDiagTerm[k + p * nEps] = v2[p + yOffsetInW];
    }

    u1[k + epsOffsetInZ] = 0.0;
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
  // of y at the current values for theta and eta.
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
  //              -                                        - 
  //             |                              |           |
  //     d       |  d     y( theta, eta, eps )  |           |  .
  //      theta  |   eps                        | eps = 0   |
  //              -                                        - 
  //
  for ( k = 0; k < nTheta; k++ )
  {
    u1[k + thetaOffsetInZ] = 1.0;

    for ( j = 0; j < nEps; j++ )
    {
      u1[j + epsOffsetInZ] = 1.0;
    
      // Evaluate the first and second order Taylor coefficients
      // for pred(U).
      v1 = pPredADFunCurr->Forward( 1, u1 );
      v2 = pPredADFunCurr->Forward( 2, u2 );

      // Set the elements of the second derivative:
      //
      //                -                                           -
      //      (k)      |   (j)                           |           |
      //     d         |  d     y   ( theta, eta, eps )  |           |
      //      theta    |   eps   (p)                     | eps = 0   |
      //                -                                           -
      //
      //              -                                                            -
      //             |                 -                                        -   |
      //             |                |                              |           |  |
      //          =  |  d       rvec  |  d     y( theta, eta, eps )  |           |  |                 .
      //             |   theta        |   eps                        | eps = 0   |  |
      //             |                 -                                        -   |
      //              -                                                            -  (p*nEps + j, k)
      //
      // Note that an rvec operation is performed on the first
      // derivatives before the second derivatives are calculated.
      for ( p = 0; p < nEventCurr; p++ )
      {
        // Set the position of this element in the matrix of 
        // second derivatives
        row = p * nEps + j;
        col = k;

        h_thetaCurr[row + col * nH_thetaRow] = v2[p + yOffsetInW]
          - y_theta_thetaDiagTerm[k + p * nTheta] 
          - y_eps_epsDiagTerm[j + p * nEps];
      }

      u1[j + epsOffsetInZ] = 0.0;
    }

    u1[k + thetaOffsetInZ] = 0.0;
  }

  // Calculate the second derivative
  //            -                                        - 
  //           |                              |           |
  //     d     |  d     y( theta, eta, eps )  |           |  .
  //      eta  |   eps                        | eps = 0   |
  //            -                                        - 
  //
  for ( k = 0; k < nEta; k++ )
  {
    u1[k + etaOffsetInZ] = 1.0;

    for ( j = 0; j < nEps; j++ )
    {
      u1[j + epsOffsetInZ] = 1.0;
    
      // Evaluate the first and second order Taylor coefficients
      // for pred(U).
      v1 = pPredADFunCurr->Forward( 1, u1 );
      v2 = pPredADFunCurr->Forward( 2, u2 );

      // Set the elements of the second derivative:
      //
      //              -                                           -
      //      (k)    |   (j)                           |           |
      //     d       |  d     y   ( theta, eta, eps )  |           |
      //      eta    |   eps   (p)                     | eps = 0   |
      //              -                                           -
      //
      //              -                                                          -
      //             |               -                                        -   |
      //             |              |                              |           |  |
      //          =  |  d     rvec  |  d     y( theta, eta, eps )  |           |  |                 .
      //             |   eta        |   eps                        | eps = 0   |  |
      //             |               -                                        -   |
      //              -                                                          -  (p*nEps + j, k)
      //
      // Note that an rvec operation is performed on the first
      // derivatives before the second derivatives are calculated.
      for ( p = 0; p < nEventCurr; p++ )
      {
        // Set the position of this element in the matrix of 
        // second derivatives
        row = p * nEps + j;
        col = k;

        h_etaCurr[row + col * nH_etaRow] = v2[p + yOffsetInW]
          - y_eta_etaDiagTerm[k + p * nEta] 
          - y_eps_epsDiagTerm[j + p * nEps];
      }

      u1[j + epsOffsetInZ] = 0.0;
    }

    u1[k + etaOffsetInZ] = 0.0;
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
 *     f_i(\alpha, b_i) ,
 * \f]
 * where
 *     \f$ i      \f$  = index for the current individual,
 *     \f$ \alpha \f$  = current value for the population parameter, and
 *     \f$ b_i    \f$  = current value for the individual parameter.
 */
/*************************************************************************/

void PopPredModel::doDataMean( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nRow = nEventCurr;
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

  // Evaluate the predicted values for all of the events 
  // for the current individual.
  evalAllPred();


  //------------------------------------------------------------
  // Calculate the value.
  //------------------------------------------------------------

  // This message will be used if a value is not valid.
  string taskMessage;
  int j;

  // Set the values for the mean of the data:
  //
  //     f    ( alpha, b  )  =  f   ( theta, eta )  .
  //      i(j)          i        (j)
  //
  for ( j = 0; j < nEventCurr; j++ )
  {
    // Set this element.
    dataMeanCurr[j] = Value( wCurr[j + fOffsetInW] );

    taskMessage = "during the evaluation of the mean of the " +
      intToOrdinalString( j, ZERO_IS_FIRST_INT ) + " value" + 
      "\n" + 
      "for the " + intToOrdinalString( iCurr, ZERO_IS_FIRST_INT ) + 
      " individual's data.";

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // [Revisit - Infinite Macro Does Not Seem to Work - Mitch]
    // Remove the comments from this block once it is determined
    // how to detect values of -inf or +inf
    /*
    // Make sure that the value is not infinite.
    if ( fabs( dataMeanCurr[j] ) == numeric_limits<double>::infinity() )
    {
      // [Revisit - SPK Error Codes Don't Really Apply - Mitch]
      // This error code should be replaced with one that is accurate.
      throw SpkException(
        SpkError::SPK_MODEL_DATA_MEAN_ERR,
        ( "An infinite value was generated " + taskMessage ).c_str(),
        __LINE__,
        __FILE__ );
    }
    */
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    // Make sure that the value is not a NaN.
    if ( dataMeanCurr[j] != dataMeanCurr[j] )
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
 * Function: doDataMean_popPar
 *
 *//**
 * Sets ret equal to the current value for the derivative with respect
 * to the population parameter of the mean of the current individual's
 * data,
 * \f[
 *     \partial_{\alpha} f_i(\alpha, b_i) ,
 * \f]
 * where
 *     \f$ i      \f$  = index for the current individual,
 *     \f$ \alpha \f$  = current value for the population parameter, and
 *     \f$ b_i    \f$  = current value for the individual parameter.
 *
 * This function returns a value of true if this derivative has 
 * at least one nonzero element.
 */
/*************************************************************************/

bool PopPredModel::doDataMean_popPar( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nRow = nEventCurr;
  int nCol = nPopPar;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  if ( isDataMean_popParCurrOk )
  {
    ret = dataMean_popParCurr;
    usedCachedDataMean_popPar = true;

    // Return a value of true if this derivative has at least one
    // nonzero element.
    return !allZero( dataMean_popParCurr );
  }
  else
  {
    usedCachedDataMean_popPar = false;
  }

  dataMean_popParCurr.resize( nRow * nCol );


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
  dataMean_popParCurr = 0.0;

  // Set the values for the partial derivatives of the elements
  // of the data mean:
  //
  //      (q)
  //     d       f    ( alpha, b  )  .
  //      alpha   i(j)          i
  //
  for ( j = 0; j < nEventCurr; j++ )
  {
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    //
    // Note
    // ----
    //
    // Since the population parameter is
    //
    //                -              -
    //               |   thetaCurr    |
    //               |                |
    //     alpha  =  |  omegaParCurr  |  ,
    //               |                |
    //               |  sigmaParCurr  |
    //                -              -
    //
    // and since the data mean does not depend on omegaPar or 
    // sigmaPar, the derivative of this element is
    //
    //     d       f    ( alpha, b  )
    //      alpha   i(j)          i
    //
    //             -                                             -
    //            |                                               |
    //         =  |  d       f    ( alpha, b  ) ,  0, 0, ... , 0  |  .
    //            |   theta   i(j)          i                     |
    //             -                                             - 
    //
    // where there are nOmegaPar + nSigmaPar zeroes.
    //
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    // Set the partial derivatives of the elements that depend on theta:
    //
    //      (k)                      
    //     d       f    ( alpha, b  )  .
    //      theta   i(j)          i       
    //
    for ( k = 0; k < nTheta; k++ )
    {
      q = k + thetaOffsetInPopPar;

      dataMean_popParCurr[j + q * nRow] = f_thetaCurr[j + q * nRow];
    }

  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isDataMean_popParCurrOk = true;
  ret = dataMean_popParCurr;

  // Return a value of true if this derivative has at least one
  // nonzero element.
  return !allZero( dataMean_popParCurr );
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
 *     \partial_b f_i(\alpha, b_i) ,
 * \f]
 * where
 *     \f$ i      \f$  = index for the current individual,
 *     \f$ \alpha \f$  = current value for the population parameter, and
 *     \f$ b_i    \f$  = current value for the individual parameter.
 *
 * This function returns a value of true if this derivative has 
 * at least one nonzero element.
 */
/*************************************************************************/

bool PopPredModel::doDataMean_indPar( SPK_VA::valarray<double>& ret ) const 
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

  // Set all of the elements of the derivative of the data 
  // mean equal to zero.
  dataMean_indParCurr = 0.0;

  // Set the values for the partial derivatives of the elements
  // of the data mean:
  //
  //      (k)
  //     d     f    ( alpha, b  )  .
  //      b     i(j)          i
  //
  for ( j = 0; j < nEventCurr; j++ )
  {
    for ( k = 0; k < nIndPar; k++ )
    {
      dataMean_indParCurr[j + k * nRow] = f_etaCurr[j + k * nRow];
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
 *     R_i(\alpha, b_i) ,
 * \f]
 * where
 *     \f$ i      \f$  = index for the current individual,
 *     \f$ \alpha \f$  = current value for the population parameter, and
 *     \f$ b_i    \f$  = current value for the individual parameter.
 */
/*************************************************************************/

void PopPredModel::doDataVariance( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nRow = nEventCurr;
  int nCol = nEventCurr;


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

  // Save the current value for sigma.
  pSigmaCurr->cov( sigmaCurr );


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
  //                              ----
  //     R      ( alpha, b  )  =  \      h     ( theta, eta )  sigma       h     ( theta, eta )  .
  //      i(j,j)          i       /       (j,m)                     (m,n)   (j,n)
  //                              ----
  //                               m,n
  //
  for ( j = 0; j < nEventCurr; j++ )
  {
    // Set this element.
    for ( m = 0; m < nEps; m++ )
    {
      for ( n = 0; n < nEps; n++ )
      {
        dataVarianceCurr[j + j * nEventCurr] += 
          hCurr[j + m * nEventCurr] * sigmaCurr[m + n * nEps] *
          hCurr[j + n * nEventCurr];
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
  //     R      ( alpha, b  )  .
  //      i(j,j)          i
  //
  for ( j = 0; j < nEventCurr; j++ )
  {
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // [Revisit - Infinite Macro Does Not Seem to Work - Mitch]
    // Remove the comments from this block once it is determined
    // how to detect values of -inf or +inf
    /*
    // Make sure that the value is not infinite.
    if ( fabs( dataVarianceCurr[j + j * nEventCurr] ) == 
         numeric_limits<double>::infinity() )
    {
      // [Revisit - SPK Error Codes Don't Really Apply - Mitch]
      // This error code should be replaced with one that is accurate.
      throw SpkException(
        SpkError::SPK_MODEL_DATA_MEAN_ERR,
        ( "An infinite value was generated " + taskMessage ).c_str(),
        __LINE__,
        __FILE__ );
    }
    */
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    // Make sure that the value is not a NaN.
    if ( dataVarianceCurr[j + j * nEventCurr] != 
         dataVarianceCurr[j + j * nEventCurr] )
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
 * Function: doDataVariance_popPar
 *
 *//**
 * Sets ret equal to the current value for the derivative with 
 * respect to the population parameter of the variance of the
 * current individual's data,
 * \f[
 *     \partial_{\alpha} R_i(\alpha, b_i) =
 *       \partial_{\alpha} \mbox{rvec} \left[ R_i(\alpha, b_i) \right] ,
 * \f]
 * where
 *     \f$ i      \f$  = index for the current individual,
 *     \f$ \alpha \f$  = current value for the population parameter, and
 *     \f$ b_i    \f$  = current value for the individual parameter.
 *
 * This function returns a value of true if this derivative has 
 * at least one nonzero element.
 */
/*************************************************************************/

bool PopPredModel::doDataVariance_popPar( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nRow = nEventCurr * nEventCurr;
  int nCol = nPopPar;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  if ( isDataVariance_popParCurrOk )
  {
    ret = dataVariance_popParCurr;
    usedCachedDataVariance_popPar = true;

    // Return a value of true if this derivative has at least one
    // nonzero element.
    return !allZero( dataVariance_popParCurr );
  }
  else
  {
    usedCachedDataVariance_popPar = false;
  }

  dataVariance_popParCurr.resize( nRow * nCol );


  //------------------------------------------------------------
  // Prepare to calculate the value.
  //------------------------------------------------------------

  // Evaluate the second derivatives of the Pred block expressions.
  evalPredSecondDeriv();

  // Save the current value for sigma and its derivative.
  pSigmaCurr->cov( sigmaCurr );
  pSigmaCurr->cov_par( sigma_sigmaParCurr );

  // These will hold the columns of h that correspond to each event.
  valarray<double> hColTrans ( nEps );
  valarray<double> hCol_theta( nEps * nTheta );

  // Set the number of rows in the derivative of h.
  int nH_thetaRow = nEventCurr * nEps;

  // These are temporary variables used below.
  valarray<double> hColTransTimesSigma      ( nEps );
  valarray<double> dataVarianceDiag_theta   ( nTheta );
  valarray<double> dataVarianceDiag_sigmaPar( nSigmaPar );


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
  dataVariance_popParCurr = 0.0;

  // Set the values for the partial derivatives of the diagonal
  // elements of the data variance:
  //
  //      (q)
  //     d       R      ( alpha, b  )  .
  //      alpha   i(j,j)          i
  //
  for ( j = 0; j < nEventCurr; j++ )
  {
    // Set the row for this element in the rvec version of R.
    row = j * nEventCurr + j;

    // Set the transpose and the derivative of the column of h
    // that corresponds to this event,
    //
    //               -                            -
    //              |     h      ( theta, eta )    |
    //              |      (j, 0)                  |
    //              |          .                   |
    //     hCol  =  |          .                   |  .
    //              |          .                   |
    //              |          .                   |
    //              |  h           ( theta, eta )  |
    //              |   (j, nEps-1)                |
    //               -                            -
    //
    for ( m = 0; m < nEps; m++ )
    {
      // Set the transpose of this column.
      hColTrans[m] = hCurr[j + m * nEventCurr];

      // Set the derivative with respect to theta of this column.
      for ( k = 0; k < nTheta; k++ )
      {
        // Note that an rvec operation is performed on the elements
        // of h before the derivative is calculated.
        h_thetaRow = j * nEps + m;
        h_thetaCol = k;

        hCol_theta[m + k * nEps ] = 
          h_thetaCurr[h_thetaRow + h_thetaCol * nH_thetaRow];
      }
    }
    

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    //
    // Note
    // ----
    //
    // Since the population parameter is
    //
    //                -              -
    //               |   thetaCurr    |
    //               |                |
    //     alpha  =  |  omegaParCurr  |  ,
    //               |                |
    //               |  sigmaParCurr  |
    //                -              -
    //
    // and since this diagonal element of the data variance is 
    // equal to
    //
    //                                  T             
    //     R      ( alpha, b  )  =  hCol ( theta, eta )  sigma( sigmaPar )  hCol( theta, eta )  ,
    //      i(j,j)          i
    //
    // the derivative of this diagonal element is
    //
    //     d       R      ( alpha, b  )
    //      alpha   i(j,j)          i
    //
    //             -                                                                                  -
    //            |                                                                                    |  
    //         =  |  d       R      ( alpha, b  )  ,  0, 0, ... , 0,  d          R      ( alpha, b  )  |  ,
    //            |   theta   i(j,j)          i                        sigmaPar   i(j,j)          i    |  
    //             -                                                                                  - 
    //
    // where there are nOmegaPar zeroes.
    //
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    // Calculate
    //
    //         T
    //     hCol   sigma  d       hCol  .
    //                    theta
    //
    hColTransTimesSigma = multiply(
      hColTrans,
      nEps,
      sigmaCurr,
      nEps );
    dataVarianceDiag_theta = multiply( 
      hColTransTimesSigma,
      nEps,
      hCol_theta,
      nTheta );

    // Set the partial derivatives of the elements that depend on theta:
    //
    //                                       -                               -
    //      (k)                             |         T                       |
    //     d       R      ( alpha, b  )  =  |  2  hCol   sigma  d       hCol  |     .
    //      theta   i(j,j)          i       |                    theta        |
    //                                       -                               - (k) 
    //
    for ( k = 0; k < nTheta; k++ )
    {
      q = k + thetaOffsetInPopPar;

      dataVariance_popParCurr[row + q * nRow] =
        2.0 * dataVarianceDiag_theta[k];
    }

    // Calculate
    //
    //           T            T
    //     ( hCol   kron  hCol  )  d        sigma  .
    //                              sigmaPar
    //
    dataVarianceDiag_sigmaPar = AkronBtimesC( 
      hColTrans,
      nEps,
      hColTrans,
      nEps,
      sigma_sigmaParCurr,
      nSigmaPar );

    // Set the partial derivatives of the elements that depend on sigmaPar:
    //
    //                                          -                                        -
    //      (k)                                |        T            T                    |
    //     d          R      ( alpha, b  )  =  |  ( hCol   kron  hCol  )  d        sigma  |     .
    //      sigmaPar   i(j,j)          i       |                           sigmaPar       |
    //                                          -                                        - (k) 
    //
    // where kron represents the Kronecker product operator.
    for ( k = 0; k < nSigmaPar; k++ )
    {
      q = k + sigmaParOffsetInPopPar;

      dataVariance_popParCurr[row + q * nRow] =
        dataVarianceDiag_sigmaPar[k];
    }

  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isDataVariance_popParCurrOk = true;
  ret = dataVariance_popParCurr;

  // Return a value of true if this derivative has at least one
  // nonzero element.
  return !allZero( dataVariance_popParCurr );
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
 *     \partial_b R_i(\alpha, b_i) =
 *       \partial_b \mbox{rvec} \left[ R_i(\alpha, b_i) \right] ,
 * \f]
 * where
 *     \f$ i      \f$  = index for the current individual,
 *     \f$ \alpha \f$  = current value for the population parameter, and
 *     \f$ b_i    \f$  = current value for the individual parameter.
 *
 * This function returns a value of true if this derivative has 
 * at least one nonzero element.
 */
/*************************************************************************/

bool PopPredModel::doDataVariance_indPar( SPK_VA::valarray<double>& ret ) const 
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

  // Save the current value for sigma and its derivative.
  pSigmaCurr->cov( sigmaCurr );
  pSigmaCurr->cov_par( sigma_sigmaParCurr );

  // These will hold the columns of h that correspond to each event.
  valarray<double> hColTrans( nEps );
  valarray<double> hCol_eta ( nEps * nEta );

  // Set the number of rows in the derivative of h.
  int nH_etaRow = nEventCurr * nEps;

  // These are temporary variables used below.
  valarray<double> hColTransTimesSigma      ( nEps );
  valarray<double> dataVarianceDiag_eta     ( nEta );
  valarray<double> dataVarianceDiag_sigmaPar( nSigmaPar );


  //------------------------------------------------------------
  // Calculate the value.
  //------------------------------------------------------------

  int h_etaRow;
  int h_etaCol;
  int row;
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
  //     d     R      ( alpha, b  )  .
  //      b     i(j,j)          i
  //
  for ( j = 0; j < nEventCurr; j++ )
  {
    // Set the row for this element in the rvec version of R.
    row = j * nEventCurr + j;

    // Set the transpose and the derivative of the column of h
    // that corresponds to this event,
    //
    //               -                            -
    //              |     h      ( theta, eta )    |
    //              |      (j, 0)                  |
    //              |          .                   |
    //     hCol  =  |          .                   |  .
    //              |          .                   |
    //              |          .                   |
    //              |  h           ( theta, eta )  |
    //              |   (j, nEps-1)                |
    //               -                            -
    //
    for ( m = 0; m < nEps; m++ )
    {
      // Set the transpose of this column.
      hColTrans[m] = hCurr[j + m * nEventCurr];

      // Set the derivative with respect to eta of this column.
      for ( k = 0; k < nEta; k++ )
      {
        // Note that an rvec operation is performed on the elements
        // of h before the derivative is calculated.
        h_etaRow = j * nEps + m;
        h_etaCol = k;

        hCol_eta[m + k * nEps ] = 
          h_etaCurr[h_etaRow + h_etaCol * nH_etaRow];
      }
    }
    

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    //
    // Note
    // ----
    //
    // Since the individual parameter is
    //
    //     b   =  etaCurr  ,
    //      i
    //
    // and since this diagonal element of the data variance is 
    // equal to
    //
    //                                  T             
    //     R      ( alpha, b  )  =  hCol ( theta, eta )  sigma( sigmaPar )  hCol( theta, eta )  ,
    //      i(j,j)          i
    //
    // the derivative of this diagonal element is
    //
    //     d   R      ( alpha, b  )  =  d     R      ( alpha, b  )  .
    //      b   i(j,j)          i        eta   i(j,j)          i
    //
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    // Calculate
    //
    //         T
    //     hCol   sigma  d       hCol  .
    //                    eta
    //
    hColTransTimesSigma = multiply(
      hColTrans,
      nEps,
      sigmaCurr,
      nEps );
    dataVarianceDiag_eta = multiply( 
      hColTransTimesSigma,
      nEps,
      hCol_eta,
      nEta );

    // Set the partial derivatives of the elements that depend on eta:
    //
    //                                     -                             -
    //      (k)                           |         T                     |
    //     d     R      ( alpha, b  )  =  |  2  hCol   sigma  d     hCol  |     .
    //      eta   i(j,j)          i       |                    eta        |
    //                                     -                             - (k) 
    //
    for ( k = 0; k < nIndPar; k++ )
    {
      dataVariance_indParCurr[row + k * nRow] =
        2.0 * dataVarianceDiag_eta[k];
    }
    assert( nIndPar == nEta );

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
 *     R^{-1}_i(\alpha, b_i) ,
 * \f]
 * where
 *     \f$ i      \f$  = index for the current individual,
 *     \f$ \alpha \f$  = current value for the population parameter, and
 *     \f$ b_i    \f$  = current value for the individual parameter.
 */
/*************************************************************************/

void PopPredModel::doDataVarianceInv( SPK_VA::valarray<double>& ret ) const 
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
  //                                        1
  //      -1
  //     R      ( alpha, b  )  =  ----------------------  .
  //      i(j,j)          i       
  //                               R      ( alpha, b  )
  //                                i(j,j)          i
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
 * Function: doDataVarianceInv_popPar
 *
 *//**
 * Sets ret equal to the current value for the derivative with respect
 * to the population parameter of the inverse of the variance of the
 * current individual's data,
 * \f[
 *     \partial_{\alpha} R^{-1}_i(\alpha, b_i) =
 *       \partial_{\alpha} \mbox{rvec} \left[ R^{-1}_i(\alpha, b_i) \right] ,
 * \f]
 * where
 *     \f$ i      \f$  = index for the current individual,
 *     \f$ \alpha \f$  = current value for the population parameter, and
 *     \f$ b_i    \f$  = current value for the individual parameter.
 *
 * This function returns a value of true if this derivative has 
 * at least one nonzero element.
 */
/*************************************************************************/

bool PopPredModel::doDataVarianceInv_popPar( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int nRow = nEventCurr * nEventCurr;
  int nCol = nPopPar;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  if ( isDataVarianceInv_popParCurrOk )
  {
    ret = dataVarianceInv_popParCurr;
    usedCachedDataVarianceInv_popPar = true;

    // Return a value of true if this derivative has at least one
    // nonzero element.
    return !allZero( dataVarianceInv_popParCurr );
  }
  else
  {
    usedCachedDataVarianceInv_popPar = false;
  }

  dataVarianceInv_popParCurr.resize( nRow * nCol );


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
  doDataVariance_popPar( dataVarianceInv_popParCurr );

  // Set the values for the partial derivatives of the diagonal
  // elements of the inverse of the data variance:
  //
  //                                           (k)
  //                                       -  d       R      ( alpha, b  )
  //                                           alpha   i(j,j)          i  
  //      (k)      -1
  //     d       R      ( alpha, b  )  =  ---------------------------------  .
  //      alpha   i(j,j)          i                                2
  //                                           R      ( alpha, b  )
  //                                            i(j,j)          i
  //
  for ( j = 0; j < nEventCurr; j++ )
  {
    // Set the row for this element in the rvec version of the 
    // inverse of R.
    row = j * nEventCurr + j;

    // Set the partial derivatives.
    for ( k = 0; k < nPopPar; k++ )
    {
      assert( dataVarianceCurr[j + j * nEventCurr] != 0.0 );

      col = k;

      dataVarianceInv_popParCurr[row + col * nRow] = 
        - dataVariance_popParCurr[row + col * nRow]
        / ( dataVarianceCurr[j + j * nEventCurr] * 
            dataVarianceCurr[j + j * nEventCurr] );
    }
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isDataVarianceInv_popParCurrOk = true;
  ret = dataVarianceInv_popParCurr;

  // Return a value of true if this derivative has at least one
  // nonzero element.
  return !allZero( dataVarianceInv_popParCurr );
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
 *     \partial_b R^{-1}_i(\alpha, b_i) =
 *       \partial_b \mbox{rvec} \left[ R^{-1}_i(\alpha, b_i) \right] ,
 * \f]
 * where
 *     \f$ i      \f$  = index for the current individual,
 *     \f$ \alpha \f$  = current value for the population parameter, and
 *     \f$ b_i    \f$  = current value for the individual parameter.
 *
 * This function returns a value of true if this derivative has 
 * at least one nonzero element.
 */
/*************************************************************************/

bool PopPredModel::doDataVarianceInv_indPar( SPK_VA::valarray<double>& ret ) const 
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
  //                                         (k)
  //                                     -  d     R      ( alpha, b  )
  //                                         b     i(j,j)          i  
  //      (k)   -1
  //     d     R      ( alpha, b  )  =  -------------------------------  .
  //      b     i(j,j)          i                                2
  //                                         R      ( alpha, b  )
  //                                          i(j,j)          i
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
        / ( dataVarianceCurr[j + j * nEventCurr] * 
            dataVarianceCurr[j + j * nEventCurr] );
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
 * Function: doIndParVariance
 *
 *//**
 * Sets ret equal to the current value for the variance of the 
 * individual parameter,
 * \f[
 *     D(\alpha) ,
 * \f]
 * where
 *     \f$ \alpha \f$  = current value for the population parameter.
 *
 * Note that for the population level Pred model, the individual
 * parameter variance is equivalent to omega, i.e.,
 * \f[
 *     D(\alpha) = \Omega(\mbox{omegaPar}) .
 * \f]
 */
/*************************************************************************/

void PopPredModel::doIndParVariance( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nRow = nIndPar;
  int nCol = nIndPar;


  //------------------------------------------------------------
  // Prepare to calculate the value.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );


  //------------------------------------------------------------
  // Calculate the value.
  //------------------------------------------------------------

  // Set
  //
  //     D( alpha )  =  omega( omegaPar )  .
  //
  // Note that the Cov object maintains the current version of this
  // quantity internally.
  pOmegaCurr->cov( ret );


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Set the flag to indicate if the cached value was used.
  usedCachedIndParVariance = pOmegaCurr->getUsedCachedCov();
}


/*************************************************************************
 *
 * Function: doIndParVariance_popPar
 *
 *//**
 * Sets ret equal to the current value for the derivative with 
 * respect to the population parameter of the variance of the 
 * individual parameter,
 * \f[
 *     \partial_{\alpha} D(\alpha) =
 *       \partial_{\alpha} \mbox{rvec} \left[ D(\alpha) \right] ,
 * \f]
 * where
 *     \f$ \alpha \f$  = current value for the population parameter.
 *
 * Note that for the population level Pred model, the individual
 * parameter variance is equivalent to omega, i.e.,
 * \f[
 *     D(\alpha) = \Omega(\mbox{omegaPar}) .
 * \f]
 * This function returns a value of true if this derivative has 
 * at least one nonzero element.
 *
 */
/*************************************************************************/

bool PopPredModel::doIndParVariance_popPar( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nRow = nIndPar * nIndPar;
  int nCol = nPopPar;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  if ( isIndParVariance_popParCurrOk )
  {
    ret = indParVariance_popParCurr;
    usedCachedIndParVariance_popPar = true;

    // Return a value of true if this derivative has at least one
    // nonzero element.
    return !allZero( indParVariance_popParCurr );
  }
  else
  {
    usedCachedIndParVariance_popPar = false;
  }

  indParVariance_popParCurr.resize( nRow * nCol );


  //------------------------------------------------------------
  // Prepare to calculate the value.
  //------------------------------------------------------------

  // Save the current value for the derivative of omega.
  pOmegaCurr->cov_par( omega_omegaParCurr );


  //------------------------------------------------------------
  // Calculate the value.
  //------------------------------------------------------------

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //
  // Note
  // ----
  //
  // Since the population parameter is
  //
  //                -              -
  //               |   thetaCurr    |
  //               |                |
  //     alpha  =  |  omegaParCurr  |  ,
  //               |                |
  //               |  sigmaParCurr  |
  //                -              -
  //
  // and since the individual parameter variance D( alpha ) does 
  // not depend on theta or sigmaPar, its derivative is
  //
  //     d       D( alpha )
  //      alpha
  //
  //             -                                                               -
  //            |  0, 0, ... , 0,                                  0, 0, ... , 0  |
  //            |                                                                 |
  //         =  |  0, 0, ... , 0,  d          omega( omegaPar ) ,  0, 0, ... , 0  |  ,
  //            |                   omegaPar                                      |  
  //            |  0, 0, ... , 0,                                  0, 0, ... , 0  |
  //             -                                                               -
  //
  // where there are nTheta columns of zeroes before the derivatives
  // with respect to omegaPar and there are nSigmaPar columns of
  // zeroes after.
  //
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  int omegaParDepBlockStartRow = 0;
  int omegaParDepBlockStartCol = nTheta;

  // Set all of the elements of the derivative of the individual
  // parameter variance equal to zero.
  indParVariance_popParCurr = 0.0;

  // Set the partial derivatives of the elements that depend on omegaPar:
  //
  //     d          D( alpha )  .
  //      omegaPar
  //
  replaceSubblock( 
    indParVariance_popParCurr,
    nPopPar,
    omega_omegaParCurr,
    nOmegaPar,
    omegaParDepBlockStartRow,
    omegaParDepBlockStartCol );


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isIndParVariance_popParCurrOk = true;
  ret = indParVariance_popParCurr;

  // Return a value of true if this derivative has at least one
  // nonzero element.
  return !allZero( indParVariance_popParCurr );
}


/*************************************************************************
 *
 * Function: doIndParVarianceInv
 *
 *//**
 * Sets ret equal to the current value for the inverse of the variance
 * of the individual parameter,
 * \f[
 *     D^{-1}(\alpha) ,
 * \f]
 * where
 *     \f$ \alpha \f$  = current value for the population parameter.
 *
 * Note that for the population level Pred model, the individual
 * parameter variance is equivalent to omega, i.e.,
 * \f[
 *     D(\alpha) = \Omega(\mbox{omegaPar}) .
 * \f]
 */
/*************************************************************************/

void PopPredModel::doIndParVarianceInv( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nRow = nIndPar;
  int nCol = nIndPar;


  //------------------------------------------------------------
  // Prepare to calculate the value.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );


  //------------------------------------------------------------
  // Calculate the value.
  //------------------------------------------------------------

  // Set
  //
  //      -1                   -1
  //     D  ( alpha )  =  omega  ( omegaPar )  .
  //
  // Note that the Cov object maintains the current version of this
  // quantity internally.
  pOmegaCurr->inv( ret );


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Set the flag to indicate if the cached value was used.
  usedCachedIndParVarianceInv = pOmegaCurr->getUsedCachedInv();
}


/*************************************************************************
 *
 * Function: doIndParVarianceInv_popPar
 *
 *//**
 * Sets ret equal to the current value for the derivative with respect
 * to the population parameter of the inverse of the variance of the 
 * individual parameter,
 * \f[
 *     \partial_{\alpha} D^{-1}(\alpha) =
 *       \partial_{\alpha} \mbox{rvec} \left[ D^{-1}(\alpha) \right] ,
 * \f]
 * where
 *     \f$ \alpha \f$  = current value for the population parameter.
 *
 * Note that for the population level Pred model, the individual
 * parameter variance is equivalent to omega, i.e.,
 * \f[
 *     D(\alpha) = \Omega(\mbox{omegaPar}) .
 * \f]
 * This function returns a value of true if this derivative has 
 * at least one nonzero element.
 *
 */
/*************************************************************************/

bool PopPredModel::doIndParVarianceInv_popPar( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nRow = nIndPar * nIndPar;
  int nCol = nPopPar;


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  ret.resize( nRow * nCol );

  if ( isIndParVarianceInv_popParCurrOk )
  {
    ret = indParVarianceInv_popParCurr;
    usedCachedIndParVarianceInv_popPar = true;

    // Return a value of true if this derivative has at least one
    // nonzero element.
    return !allZero( indParVarianceInv_popParCurr );
  }
  else
  {
    usedCachedIndParVarianceInv_popPar = false;
  }

  indParVarianceInv_popParCurr.resize( nRow * nCol );


  //------------------------------------------------------------
  // Prepare to calculate the value.
  //------------------------------------------------------------

  // Save the current value for the derivative of the invese 
  // of omega.
  pOmegaCurr->inv_par( omegaInv_omegaParCurr );


  //------------------------------------------------------------
  // Calculate the value.
  //------------------------------------------------------------

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //
  // Note
  // ----
  //
  // Since the population parameter is
  //
  //                -              -
  //               |   thetaCurr    |
  //               |                |
  //     alpha  =  |  omegaParCurr  |  ,
  //               |                |
  //               |  sigmaParCurr  |
  //                -              -
  //
  // and since the individual parameter variance D( alpha ) does
  // not depend on theta or sigmaPar, the derivative of its inverse
  // is
  //
  //              -1
  //     d       D  ( alpha )
  //      alpha
  //
  //             -                                                               -
  //            |  0, 0, ... , 0,                                  0, 0, ... , 0  |
  //            |                                  -1                             |
  //         =  |  0, 0, ... , 0,  d          omega  ( omega  ) ,  0, 0, ... , 0  |  ,
  //            |                   omegaPar                                      |  
  //            |  0, 0, ... , 0,                                  0, 0, ... , 0  |
  //             -                                                               -
  //
  // where there are nTheta columns of zeroes before the derivatives
  // with respect to omegaPar and there are nSigmaPar columns of
  // zeroes after.
  //
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  int omegaParDepBlockStartRow = 0;
  int omegaParDepBlockStartCol = nTheta;

  // Set all of the elements of the derivative of the inverse of 
  // the individual parameter variance equal to zero.
  indParVarianceInv_popParCurr = 0.0;

  // Set the  partial derivatives of the elements that depend on omegaPar:
  //
  //                  -1
  //     d          D   ( alpha )  .
  //      omegaPar
  //
  replaceSubblock( 
    indParVarianceInv_popParCurr,
    nPopPar,
    omegaInv_omegaParCurr,
    nOmegaPar,
    omegaParDepBlockStartRow,
    omegaParDepBlockStartCol );


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  isIndParVarianceInv_popParCurrOk = true;
  ret = indParVarianceInv_popParCurr;

  // Return a value of true if this derivative has at least one
  // nonzero element.
  return !allZero( indParVarianceInv_popParCurr );
}


/*************************************************************************
 *
 * Function: getPopParLimits
 *
 *//**
 * Gets the lower and upper limits for the elements of the population
 * parameter at the current population parameter value.
 *
 * These limits can be used during the optimization of objective
 * functions that depend on these parameters.
 *
 * This function assumes that the current value for the population
 * parameter is approximately equal to its final or true value.
 */
/*************************************************************************/

void PopPredModel::getPopParLimits(
  SPK_VA::valarray<double>&  popParLow,
  SPK_VA::valarray<double>&  popParUp ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  popParLow.resize( nPopPar );
  popParUp .resize( nPopPar );


  //------------------------------------------------------------
  // Set limits for the parameters that appear in the Pred block.
  //------------------------------------------------------------

  int k;

  // Set the limits for the elements of theta.
  for ( k = 0; k < nTheta; k++ )
  {
    popParLow[k + thetaOffsetInPopPar] = thetaLow[k];
    popParUp [k + thetaOffsetInPopPar] = thetaUp [k];
  }


  //------------------------------------------------------------
  // Set limits for the covariance matrix parameters.
  //------------------------------------------------------------

  // Get the current limits from the omega covariance matrix.
  valarray<double> omegaParLow( nOmegaPar );
  valarray<double> omegaParUp ( nOmegaPar );
  pOmegaCurr->getParLimits( omegaParLow, omegaParUp );

  // Set the limits for the parameters for the omega covariance matrix.
  for ( k = 0; k < nOmegaPar; k++ )
  {
    popParLow[k + omegaParOffsetInPopPar] = omegaParLow[k];
    popParUp [k + omegaParOffsetInPopPar] = omegaParUp [k];
  }

  // Get the current limits from the sigma covariance matrix.
  valarray<double> sigmaParLow( nSigmaPar );
  valarray<double> sigmaParUp ( nSigmaPar );
  pSigmaCurr->getParLimits( sigmaParLow, sigmaParUp );

  // Set the limits for the parameters for the sigma covariance matrix.
  for ( k = 0; k < nSigmaPar; k++ )
  {
    popParLow[k + sigmaParOffsetInPopPar] = sigmaParLow[k];
    popParUp [k + sigmaParOffsetInPopPar] = sigmaParUp [k];
  }

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

void PopPredModel::getIndParLimits(
  SPK_VA::valarray<double>&  indParLow,
  SPK_VA::valarray<double>&  indParUp ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  indParLow.resize( nIndPar );
  indParUp .resize( nIndPar );


  //------------------------------------------------------------
  // Set limits for the individual parameters.
  //------------------------------------------------------------

  // Get the current value for variance of the individual parameter,
  //
  //     D( alpha )  =  omega( omegaPar )  .
  //
  valarray<double> omegaTemp( nEta * nEta );
  pOmegaCurr->cov( omegaTemp );

  double stdDev;
  int k;

  // Set the limits.
  for ( k = 0; k < nEta; k++ )
  {
    // Calculate the standard deviation for this element.
    stdDev = std::sqrt( omegaTemp[k + k * nEta] );

    // Set the limits to be a multiple of the standard deviation.
    indParLow[k] = -3.0 * stdDev;
    indParUp [k] = +3.0 * stdDev;
  }
  assert( nIndPar == nEta );

}


/*************************************************************************
 *
 * Function: getPopParStep
 *
 *//**
 * Gets a vector of step sizes for approximating derivatives with 
 * respect to the elements of the population parameter.
 *
 * These step sizes can be used to approximate the derivatives of
 * objective functions that depend on the population parameters.
 */
/*************************************************************************/

void PopPredModel::getPopParStep( SPK_VA::valarray<double>&  popParStep ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  popParStep.resize( nPopPar );


  //------------------------------------------------------------
  // Set the step sizes.
  //------------------------------------------------------------

  int k;

  // Get the current limits for the population parameter.
  valarray<double> popParLow( nPopPar );
  valarray<double> popParUp ( nPopPar );
  getPopParLimits( popParLow, popParUp );

  // Set the step sizes for the population parameter.
  for ( k = 0; k < nPopPar; k++ )
  {
    popParStep[k] = ( popParUp[k] - popParLow[k] ) / 1000.0;
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

void PopPredModel::getIndParStep( SPK_VA::valarray<double>&  indParStep ) const
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
 * Function: getNPopPar
 *
 *//**
 * Returns the number of elements in the population parameter.
 */
/*************************************************************************/

int PopPredModel::getNPopPar() const
{
  return nPopPar;
}


/*************************************************************************
 *
 * Function: getNIndPar
 *
 *//**
 * Returns the number of elements in the individual parameter.
 */
/*************************************************************************/

int PopPredModel::getNIndPar() const
{
  return nIndPar;
}


/*************************************************************************
 *
 * Function: getPopPar
 *
 *//**
 * Gets the current value for the population parameter.
 */
/*************************************************************************/

void PopPredModel::getPopPar( SPK_VA::valarray<double>& ret ) const 
{
  ret.resize( nPopPar );

  ret = alphaCurr;
}


/*************************************************************************
 *
 * Function: getIndPar
 *
 *//**
 * Gets the current value for the individual parameter.
 */
/*************************************************************************/

void PopPredModel::getIndPar( SPK_VA::valarray<double>& ret ) const 
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

void PopPredModel::getTheta( SPK_VA::valarray<double>& ret ) const 
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

void PopPredModel::getEta( SPK_VA::valarray<double>& ret ) const 
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

void PopPredModel::getOmega( SPK_VA::valarray<double>& ret ) const 
{
  ret.resize( pOmegaCurr->getNPar() );

  // Get the current value for omega.
  valarray<double> omegaTemp( nEta * nEta );
  pOmegaCurr->cov( omegaTemp );

  // Return its minimal representation.
  pOmegaCurr->calcCovMinRep( omegaTemp, ret );
}


/*************************************************************************
 *
 * Function: getSigma
 *
 *//**
 * Gets the minimal representation for the current value for sigma.
 */
/*************************************************************************/

void PopPredModel::getSigma( SPK_VA::valarray<double>& ret ) const 
{
  ret.resize( pSigmaCurr->getNPar() );

  // Get the current value for sigma.
  pSigmaCurr->cov( sigmaCurr );

  // Return its minimal representation.
  pSigmaCurr->calcCovMinRep( sigmaCurr, ret );
}


/*************************************************************************
 *
 * Function: getStandardPar
 *
 *//**
 * Gets the current values for the standard parameters.
 *
 * In particular, this function gets the current values for theta,
 * the minimal representation for omega, and the minimal representation 
 * for sigma combined into a single vector,
 * \f[
 *     \mbox{standardPar} =
 *       \left[ 
 *         \begin{array}{c}
 *           \mbox{thetaCurr} \\
 *           \mbox{omegaMinRepCurr} \\
 *           \mbox{sigmaMinRepCurr}
 *         \end{array}
 *       \right] .
 * \f]
 */
/*************************************************************************/

void PopPredModel::getStandardPar( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  ret.resize( nPopPar );


  //------------------------------------------------------------
  // Prepare the covariance matrices.
  //------------------------------------------------------------

  // Get the current value for omega.
  valarray<double> omegaTemp( nEta * nEta );
  pOmegaCurr->cov( omegaTemp );

  // Get omega's minimal representation.
  valarray<double> omegaMinRepTemp( nOmegaPar );
  pOmegaCurr->calcCovMinRep( omegaTemp, omegaMinRepTemp );

  // Get the current value for sigma.
  pSigmaCurr->cov( sigmaCurr );

  // Get sigma's minimal representation.
  valarray<double> sigmaMinRepTemp( nSigmaPar );
  pSigmaCurr->calcCovMinRep( sigmaCurr, sigmaMinRepTemp );


  //------------------------------------------------------------
  // Set the vector of standard parameters.
  //------------------------------------------------------------

  int k;

  // Set the elements that contain theta.
  for ( k = 0; k < nTheta; k++ )
  {
    ret[k + thetaOffsetInPopPar] = thetaCurr[k];
  }

  // Set the elements that contain the minimal representation
  // for omega.
  for ( k = 0; k < nOmegaPar; k++ )
  {
    ret[k + omegaParOffsetInPopPar] = omegaMinRepTemp[k];
  }

  // Set the elements that contain the minimal representation
  // for sigma.
  for ( k = 0; k < nSigmaPar; k++ )
  {
    ret[k + sigmaParOffsetInPopPar] = sigmaMinRepTemp[k];
  }

}


/*************************************************************************
 *
 * Function: getStandardPar_popPar
 *
 *//**
 * Gets the current values for the derivatives of the standard parameters.
 *
 * In particular, this function gets the current values for the 
 * derivative with respect to the population parameter of theta, 
 * the minimal representation for omega, and the minimal representation 
 * for sigma combined into a single vector,
 * \f[
 *     \partial_{\alpha} \; \mbox{standardPar} = \partial_{\alpha} 
 *       \left[ 
 *         \begin{array}{c}
 *           \mbox{thetaCurr} \\
 *           \mbox{omegaMinRepCurr} \\
 *           \mbox{sigmaMinRepCurr}
 *         \end{array}
 *       \right] .
 * \f]
 */
/*************************************************************************/

void PopPredModel::getStandardPar_popPar( SPK_VA::valarray<double>& ret ) const 
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  ret.resize( nPopPar * nPopPar );


  //------------------------------------------------------------
  // Prepare the derivatives of the covariance matrices.
  //------------------------------------------------------------

  // Get the current value for the derivative of omega.
  valarray<double> omega_omegaParTemp( nEta * nEta * nOmegaPar );
  pOmegaCurr->cov_par( omega_omegaParTemp );

  // Get the derivative of omega's minimal representation.
  valarray<double> omegaMinRep_omegaParTemp( nOmegaPar * nOmegaPar );
  pOmegaCurr->calcCovMinRep_par( 
    omega_omegaParTemp,
    nOmegaPar,
    omegaMinRep_omegaParTemp );

  // Get the current value for the derivative of sigma.
  pSigmaCurr->cov_par( sigma_sigmaParCurr );

  // Get the derivative of sigma's minimal representation.
  valarray<double> sigmaMinRep_sigmaParTemp( nSigmaPar * nSigmaPar );
  pSigmaCurr->calcCovMinRep_par(
    sigma_sigmaParCurr,
    nSigmaPar,
    sigmaMinRep_sigmaParTemp );


  //------------------------------------------------------------
  // Set the vector of standard parameters.
  //------------------------------------------------------------

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //
  // Note
  // ----
  //
  // Since the population parameter is
  //
  //                -              -
  //               |   thetaCurr    |
  //               |                |
  //     alpha  =  |  omegaParCurr  |  ,
  //               |                |
  //               |  sigmaParCurr  |
  //                -              -
  //
  // the derivative of the vector of standard parameters is
  //
  //     d       standardPar
  //      alpha
  //
  //             -                                                                                  -
  //            |  I                      0                                                     0    |
  //            |   nTheta                                                                           |
  //            |                                                                                    |
  //         =  |  0          d        omegaMinRep( omegaPar )                                  0    |  ,
  //            |              omegaPar                                                              |
  //            |                                                                                    |
  //            |  0                      0                       d        sigmaMinRep( sigmaPar )   |
  //            |                                                  sigmaPar                          |
  //             -                                                                                  -
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
    row = k + thetaOffsetInPopPar;
    col = k + thetaOffsetInPopPar;

    ret[row + col * nPopPar] = 1.0;
  }

  // Set the partial derivatives of the elements that depend on 
  // the minimal representation for omega.
  for ( j = 0; j < nOmegaPar; j++ )
  {
    for ( k = 0; k < nOmegaPar; k++ )
    {
      row = j + omegaParOffsetInPopPar;
      col = k + omegaParOffsetInPopPar;
    
      ret[row + col * nPopPar] = omegaMinRep_omegaParTemp[j + k * nOmegaPar];
    }
  }

  // Set the partial derivatives of the elements that depend on 
  // the minimal representation for sigma.
  for ( j = 0; j < nSigmaPar; j++ )
  {
    for ( k = 0; k < nSigmaPar; k++ )
    {
      row = j + sigmaParOffsetInPopPar;
      col = k + sigmaParOffsetInPopPar;
    
      ret[row + col * nPopPar] = sigmaMinRep_sigmaParTemp[j + k * nSigmaPar];
    }
  }

}

