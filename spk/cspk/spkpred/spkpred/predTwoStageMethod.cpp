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
 * @file: predTwoStageMethod.cpp
 *
 * Implements predTwoStageMethod() function.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "IndPredModel.h"
#include "PopPredModel.h"
#include "predTwoStageMethod.h"

// SPK library header files.
#include <spk/add.h>
#include <spk/divByScalar.h>
#include <spk/getCol.h>
#include <spk/getSubblock.h>
#include <spk/Objective.h>
#include <spk/Optimizer.h>
#include <spk/replaceJth.h>
#include <spk/SpkValarray.h>
#include <spk/twoStageMethod.h>

using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: predTwoStageMethod
 *
 *//**
 * This Pred library specific two-stage method wrapper determines
 * population means and covariances for individual parameters.
 *
 * This function performs a two-stage method analysis using the
 * individual model, indModelWithPopData, which should contain the same
 * data and model as the population model, popModel.
 *
 * At the end, the population parameters for the population model are
 * set to the values determined by the two-stage method.
 *
 * To be specific, at the end of these methods,
 * \f[
 *     theta^{\mbox{Pop}}  =  thetaMean^{\mbox{Ind}}  =  \frac{1}{nInd} \sum_{i = 1}^{nInd}  theta_i^{\mbox{Ind}}   ,
 * \f]
 * \f[
 *     Omega^{\mbox{Pop}}  =  thetaCov^{\mbox{Ind}}   =  \frac{1}{nInd} \sum_{i = 1}^{nInd}   ( theta_i^{\mbox{Ind}}  -  thetaMean^{\mbox{Ind}} ) ( theta_i^{\mbox{Ind}}  -  thetaMean^{\mbox{Ind}} )^T   ,
 * \f]
 * \f[
 *     Sigma^{\mbox{Pop}}  =  OmegaMean^{\mbox{Ind}}  =  \frac{1}{nInd} \sum_{i = 1}^{nInd}  Omega_i^{\mbox{Ind}}   .
 * \f]
 * This function, therefore, requires that the following be true:
 * \f[
 *     nTheta^{\mbox{Pop}}       =  nTheta^{\mbox{Ind}}  ,
 * \f]
 * \f[
 *     nEta^{\mbox{Pop}}         =  nTheta^{\mbox{Ind}}  ,
 * \f]
 * \f[
 *     nEps^{\mbox{Pop}}         =  nEta^{\mbox{Ind}}  ,
 * \f]
 * \f[
 *     SigmaStruct^{\mbox{Pop}}  =  OmegaStruct^{\mbox{Ind}}  ,
 * \f]
 * \f[
 *     Omega^{\mbox{Pop}}        =  nEta^{\mbox{Pop}} \times nEta^{\mbox{Pop}} ,
 * \f]
 * \f[
 *     Sigma^{\mbox{Pop}}        =  nEps^{\mbox{Pop}} \times nEps^{\mbox{Pop}} ,
 * \f]
 * \f[
 *     Omega^{\mbox{Ind}}        =  nEta^{\mbox{Ind}} \times nEta^{\mbox{Ind}}  .
 * \f]
 * 
 * This function allows the following Two-Stage methods to be used:
 * 
 *    - STANDARD_TWO_STAGE  = Standard Two-Stage (STS) method,
 *    - ITERATIVE_TWO_STAGE = Iterative Two-Stage (ITS) method,
 *    - GLOBAL_TWO_STAGE    = Global Two-Stage (GTS) method.
 * 
 * For the Standard Two-Stage methods (STS), the population mean of
 * the individuals' parameter estimates is calculated as
 * \f[
 *     bMean       =  \frac{1}{nInd} \sum_{i = 1}^{nInd}  bOut_i   
 * \f]
 * 
 * and the population covariance of the individuals' estimates is
 * calculated as
 * \f[
 *     bCov       =  \frac{1}{nInd} \sum_{i = 1}^{nInd}   ( bOut_i  -  bMean ) ( bOut_i  -  bMean )^T   .
 * \f]
 * 
 * For the Iterative and Global Two-Stage methods (ITS and GTS), the
 * population mean and the population covariance of the individuals' 
 * estimates are calculated using the algorithms described in Schumitzky
 * (1995).
 * 
 * Reference:
 *
 * A. Schumitzky, EM algorithms and two stage methods in phamacokinetic population analysis.
 * in "Advanced Methods of Pharmacokinetic and Pharmacodynamic Systems Analysis", 
 * edited by D. Z. D'Argenio. New York: Plenum, 1995, p. 145-160.
 */
/*************************************************************************/

void predTwoStageMethod( PopPredModel&                    popModel,
                         IndPredModel&                    indModelWithPopData,
                         enum  Objective                  method,
                         const SPK_VA::valarray<int>&     nMeasurementsAll,
                         const SPK_VA::valarray<double>&  measurementsAll,
                         Optimizer&                       popOptInfo,
                         Optimizer&                       indOptInfo,
                         SPK_VA::valarray<double>*        pIndParAllOut )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  const int nInd = nMeasurementsAll.size();
  const int nY   = measurementsAll.size();


  //------------------------------------------------------------
  // Validate the input models.
  //------------------------------------------------------------

  valarray<double> popThetaTemp;
  valarray<double> popEtaTemp;
  valarray<double> popEpsTemp;

  popModel.getTheta( popThetaTemp );
  popModel.getEta  ( popEtaTemp );
  popModel.getEps  ( popEpsTemp );

  int nThetaPop = popThetaTemp.size();
  int nEtaPop   = popEtaTemp  .size();
  int nEpsPop   = popEpsTemp  .size();

  valarray<double> indThetaTemp;
  valarray<double> indEtaTemp;

  indModelWithPopData.getTheta( indThetaTemp );
  indModelWithPopData.getEta  ( indEtaTemp );

  int nThetaInd = indThetaTemp.size();
  int nEtaInd   = indEtaTemp  .size();

  // This function requires that the following be true:
  //
  //     nThetaPop  =  nThetaInd  .
  //
  if( nThetaPop != nThetaInd )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR, 
      "The Two-Stage method could not be performed because the number of population THETA values \ndid not match the number of individual THETA values.",
      __LINE__,
      __FILE__ );
  }

  // This function requires that the following be true:
  //
  //     nEtaPop  =  nThetaInd  .
  //
  if( nEtaPop != nThetaInd )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR, 
      "The Two-Stage method could not be performed because the number of population ETA values \ndid not match the number of individual THETA values.",
      __LINE__,
      __FILE__ );
  }

  // This function requires that the following be true:
  //
  //     nEpsPop  =  nEtaInd  .
  //
  if( nEpsPop != nEtaInd )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR, 
      "The Two-Stage method could not be performed because the number of population EPS values \ndid not match the number of individual ETA values.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Validate the rest of the inputs.
  //------------------------------------------------------------

  // Check to see if the method is one of the two-stage methods.
  if( method != STANDARD_TWO_STAGE            &&
      method != ITERATIVE_TWO_STAGE           &&
      method != GLOBAL_TWO_STAGE              &&
      method != MAP_BAYES_STANDARD_TWO_STAGE  &&
      method != MAP_BAYES_ITERATIVE_TWO_STAGE &&
      method != MAP_BAYES_GLOBAL_TWO_STAGE )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR, 
      "A Two-Stage method was requested with an unknown method or population objective function.",
      __LINE__,
      __FILE__ );
  }

  int i;

  //===============[Begin: Vector lengths validation]===============
  if( nY != nMeasurementsAll.sum() )
  {
    int max = SpkError::maxMessageLen();
    char message[ max ];
    snprintf( message, max, 
      "The sum of the values contained in nMeasurementsAll vector must match the lenth of \nmeasurementsAll vector.  %d does not match %d.",
        nMeasurementsAll.sum(), nY );

    throw SpkException(
            SpkError::SPK_USER_INPUT_ERR, 
            message,
            __LINE__, __FILE__
    );
  }
  //===============[End: Vector lengths validation]===============
  
  //===============[Begin: Data validation]===============

  // Check that values in N are consistent with the size of y, i.e.,
  // 
  //     nY  =  N(1)  +  N(2)  +  ...  +  N(nInd)  .
  //
  for ( i = 0; i < nInd; i++ )
  {
    if( nMeasurementsAll[i] < 0 )
    {
        int max = SpkError::maxMessageLen();
        char message[ max ];
        snprintf( message, max,
      "The number of measurements must be greater than zero.  %d-th element, %d, is invalid.",
                  i, nMeasurementsAll[i] );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR,  
                message,
                __LINE__, __FILE__
        );
    }
  }

  //===============[End: Data validation]===============


  //------------------------------------------------------------
  // Convert the valarray inputs to DoubleMatrix.
  //------------------------------------------------------------

  DoubleMatrix dvecN( nInd, 1 );
  double * pN = dvecN.data();
  for( i=0; i<nInd; i++ )
  {
    pN[i] = static_cast<int>( nMeasurementsAll[i] ) ;
  }

  DoubleMatrix dvecY( measurementsAll );

  DoubleMatrix dvecEpsilon( 2, 1 );
  dvecEpsilon.data()[ 0 ] = indOptInfo.getEpsilon();
  dvecEpsilon.data()[ 1 ] = popOptInfo.getEpsilon();

  DoubleMatrix dvecNMaxIter( 2, 1 );
  dvecNMaxIter.data()[ 0 ] = indOptInfo.getNMaxIter();
  dvecNMaxIter.data()[ 1 ] = popOptInfo.getNMaxIter();

  DoubleMatrix dvecLevel( 2, 1 );
  dvecLevel.data()[ 0 ] = indOptInfo.getLevel();
  dvecLevel.data()[ 1 ] = popOptInfo.getLevel();


  //------------------------------------------------------------
  // Prepare quantities related to individual parameter optimization.
  //------------------------------------------------------------

  // Set the number of the individual parameters.
  //
  // Note that the individual parameters include both the individual
  // model theta and the parameters that make up Omega,
  //
  //             -           -
  //            |   theta     |
  //            |        i    |
  //     b   =  |             |  .
  //      i     |  omegaPar   |
  //            |          i  |
  //             -           -
  //
  int nB = indModelWithPopData.getNIndPar();

  valarray<double> bLow  ( nB );
  valarray<double> bUp   ( nB );
  valarray<double> bCurr ( nB );
  valarray<double> bStep ( nB );

  // Get the current value for the individual parameter.
  indModelWithPopData.getIndPar( bCurr );

  // Get the limits for the individual parameters.
  indModelWithPopData.getIndParLimits( bLow, bUp );

  // Get the step sizes for the individual parameters.
  indModelWithPopData.getIndParStep( bStep );

  DoubleMatrix dvecBLow ( bLow );
  DoubleMatrix dvecBUp  ( bUp );
  DoubleMatrix dvecBCurr( bCurr );
  DoubleMatrix dvecBStep( bStep );

  DoubleMatrix dmatBAllIn( nB, nInd );

  // Set the initial values for all of the individuals' parameters.
  for ( i = 0; i < nInd; i++ )
  {
    // Prepare this individual's initial parameter value.
    replaceJth( dmatBAllIn, i, dvecBCurr );
  }


  //------------------------------------------------------------
  // Validate the output values for this function.
  //------------------------------------------------------------

  // Check the size for the matrix that will contain all of the
  // individuals' parameter values.
  if( pIndParAllOut )
  {
    if( pIndParAllOut->size() != nB * nInd )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The Two-Stage method could not be performed because matrix of individuals' parameter values had the wrong size.",
        __LINE__,
        __FILE__ );
    }
  }


  //------------------------------------------------------------
  // Prepare the objects to hold the two-stage output values.
  //------------------------------------------------------------

  // Instantiate matrices and vectors to hold the individual parameter
  // estimates for each individual, their mean, and their covariance.
  DoubleMatrix dmatBAllOut( nB, nInd );

  DoubleMatrix dvecBMeanOut( nB, 1 );
  DoubleMatrix dmatBCovOut ( nB, nB );


  //------------------------------------------------------------
  // Perform the two-stage method.
  //------------------------------------------------------------

  twoStageMethod( indModelWithPopData,
                  method,
                  dvecN,
                  dvecY,
                  popOptInfo,
                  indOptInfo,
                  dvecBLow,
                  dvecBUp,
                  dmatBAllIn,
                  &dmatBAllOut,
                  dvecBStep,
                  &dvecBMeanOut,
                  &dmatBCovOut );


  //------------------------------------------------------------
  // Get the means and covariance of the individual model parameters.
  //------------------------------------------------------------

  DoubleMatrix dvecBOut_i           ( nB,        1 );
  DoubleMatrix dvecThetaIndMean     ( nThetaInd, 1 );
  DoubleMatrix dmatThetaIndCov      ( nThetaInd, nThetaInd );
  DoubleMatrix dmatOmegaInd_i       ( nEtaInd,   nEtaInd );
  DoubleMatrix dmatOmegaInd_iSum    ( nEtaInd,   nEtaInd );
  DoubleMatrix dmatOmegaIndMean     ( nEtaInd,   nEtaInd );

  valarray<double> omegaInd_i( nEtaInd * nEtaInd );

  // The individual parameters returned by the two-stage methods
  // contain both theta and the parameters that make up Omega, i.e.,
  //
  //             -           -
  //            |   theta     |
  //            |        i    |
  //     b   =  |             |  .
  //      i     |  omegaPar   |
  //            |          i  |
  //             -           -
  //
  // Get the mean value for the individuals' values for theta,
  //
  //                              nInd
  //                              ----
  //                        1     \    
  //     thetaIndMean  =  ------  /     thetaInd   ,
  //                       nInd   ----          i
  //                              i = 1 
  //
  // and the covariance of the individuals' values for theta,
  //
  //                              nInd
  //                              ----
  //                        1     \                                                                 T
  //     thetaIndCov   =  ------  /      ( thetaInd  -  thetaIndMean ) ( thetaInd  -  thetaIndMean )   ,
  //                       nInd   ----             i                             i
  //                              i = 1 
  //
  dvecThetaIndMean = getSubblock( dvecBMeanOut, 0, 0, nThetaInd, 1 );
  dmatThetaIndCov  = getSubblock( dmatBCovOut,  0, 0, nThetaInd, nThetaInd );

  // Initially set this equal to zero.
  dmatOmegaInd_iSum.fill( 0.0 );

  // Calculate the mean value for the individuals' values for Omega,
  //
  //                              nInd
  //                              ----
  //                        1     \    
  //     OmegaIndMean  =  ------  /     OmegaInd   .
  //                       nInd   ----          i
  //                              i = 1 
  //
  for ( i = 0; i < nInd; i++ )
  {
    // Get this individual's parameter value.
    dvecBOut_i = getCol( dmatBAllOut, i );
    
    // Set the current individual and their parameter value.
    indModelWithPopData.selectIndividual( i );
    indModelWithPopData.setIndPar( dvecBOut_i.toValarray() );

    // Get the current individual's Omega value.
    indModelWithPopData.getOmega( omegaInd_i );
    dmatOmegaInd_i.fromValarray( omegaInd_i );

    // Add in this individual's value.
    dmatOmegaInd_iSum = add( dmatOmegaInd_iSum, dmatOmegaInd_i );
  }

  // Divide by the number of individuals to get the mean value.
  divByScalar( dmatOmegaInd_iSum, nInd, dmatOmegaIndMean );


  //------------------------------------------------------------
  // Set the population model parameters related to the two-stage method.
  //------------------------------------------------------------

  // Set
  //
  //     thetaPop  =  thetaIndMean  .
  //
  popModel.setTheta( dvecThetaIndMean.toValarray() );

  // Set
  //
  //     OmegaPop  =  thetaIndCov  .
  //
  popModel.setOmega( dmatThetaIndCov.toValarray() );

  // Set
  //
  //     SigmaPop  =  OmegaIndMean  .
  //
  popModel.setSigma( dmatOmegaIndMean.toValarray() );


  //------------------------------------------------------------
  // Set the values to be returned.
  //------------------------------------------------------------

  // Set the matrix of individual parameter estimates for each
  // individual, if necessary.
  if ( pIndParAllOut )
  {
    *pIndParAllOut = dmatBAllOut.toValarray() ;
  }

}
