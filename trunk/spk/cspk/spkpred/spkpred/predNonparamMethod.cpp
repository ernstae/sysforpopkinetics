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
 * @file: predNonparamMethod.cpp
 *
 * Implements predNonparamMethod() function.
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
#include "predNonparamMethod.h"

// SPK library header files.
#include <spk/add.h>
#include <spk/divByScalar.h>
#include <spk/getCol.h>
#include <spk/getSubblock.h>
#include <spk/mulByScalar.h>
#include <spk/multiply.h>
#include <spk/Objective.h>
#include <spk/Optimizer.h>
#include <spk/replaceJth.h>
#include <spk/SpkValarray.h>
#include <spk/spk_non_par.h>
#include <spk/subtract.h>
#include <spk/transpose.h>

using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: predNonparamMethod
 *
 *//**
 * This Pred library specific nonparametric method wrapper determines
 * the population means and covariances for individual parameters by
 * finding the optimal atomic measure points corresponding to the
 * solution of the nonparametric objective function optimization
 * problem.
 *
 * This function performs a nonparametric method analysis using the
 * individual model, indModelWithPopData, which should contain the same
 * data and model as the population model, popModel.
 *
 * At the end, the population parameters for the population model are
 * set equal to the values determined by the nonparametric method.
 *
 * To be specific, at the end of this nonparametric method analysis,
 * \f[
 *     theta^{\mbox{(Pop)}}  =  \overline{ theta^{\mbox{(Ind)}} }  =  \mbox{E}\left[ theta^{\mbox{(Ind)}} \right]  =  \sum_{j = 1}^{nMeasurePoint}  \lambda_j * theta_j^{\mbox{(Ind)}}  ,
 * \f]
 * \f[
 *     Omega^{\mbox{(Pop)}}  =  \mbox{Cov}\left[ theta^{\mbox{(Ind)}} \right]  =  \sum_{j = 1}^{nMeasurePoint}  \lambda_j * \left[ theta_j^{\mbox{(Ind)}} - \overline{ theta^{\mbox{(Ind)}} } \right] \left[ theta_j^{\mbox{(Ind)}} - \overline{ theta^{\mbox{(Ind)}} } \right]^T  ,
 * \f]
 * \f[
 *     Sigma^{\mbox{(Pop)}}  =  \overline{ Omega^{\mbox{(Ind)}} }  =  \mbox{E}\left[ Omega^{\mbox{(Ind)}} \right]  =  \sum_{j = 1}^{nMeasurePoint}  \lambda_j * Omega_j^{\mbox{(Ind)}}   ,
 * \f]
 * where \f$ nMeasurePoint \f$ is the number of optimal atomic measure
 * points determined by the nonparametric method, and the sum of the
 * \f$ \lambda_j \f$ values is equal to one.  Note that the above sums
 * are over the optimal set of atomic measure points and not over the
 * individuals in the population.  The values for \f$
 * theta_j^{\mbox{(Ind)}} \f$ and \f$ Omega_j^{\mbox{(Ind)}} \f$ are
 * calculated at the j-th atomic measure point \f$ b_j \f$ using the
 * individual level model.
 *
 * This function, therefore, requires that the following be true:
 * \f[
 *     nTheta^{\mbox{(Pop)}}       =  nTheta^{\mbox{(Ind)}}  ,
 * \f]
 * \f[
 *     nEta^{\mbox{(Pop)}}         =  nTheta^{\mbox{(Ind)}}  ,
 * \f]
 * \f[
 *     nEps^{\mbox{(Pop)}}         =  nEta^{\mbox{(Ind)}}  ,
 * \f]
 * \f[
 *     SigmaStruct^{\mbox{(Pop)}}  =  OmegaStruct^{\mbox{(Ind)}}  ,
 * \f]
 * \f[
 *     Omega^{\mbox{(Pop)}}        =  nEta^{\mbox{(Pop)}} \times nEta^{\mbox{(Pop)}} ,
 * \f]
 * \f[
 *     Sigma^{\mbox{(Pop)}}        =  nEps^{\mbox{(Pop)}} \times nEps^{\mbox{(Pop)}} ,
 * \f]
 * \f[
 *     Omega^{\mbox{(Ind)}}        =  nEta^{\mbox{(Ind)}} \times nEta^{\mbox{(Ind)}}  .
 * \f]
 * 
 *
 * @param indMeasurePointAllIn This SPK_VA::valarray<double> object
 * specifies the matrix of initial atomic measure points for the
 * nonparametric method.  Its size must be equal to the initial number
 * of atomic points in the non-parametric measure, \f$ nMeasurePoint
 * \f$, times the number of individual parameters \f$ nB \f$.  The
 * j-th column of the matrix will contain a column vector that is the
 * initial value for the j-th atomic measure point, \f$ b_j \f$.  Its
 * number of columns must be greater than or equal to the number of
 * individuals \f$ nInd \f$.
 * 
 * @param pIndMeasurePointAllOut If pIndMeasurePointAllOut is not
 * NULL, then the SPK_VA::valarray<double> object pointed to by
 * pIndMeasurePointAllOut must be declared in the function that calls
 * this function, but its input size does not matter.  If
 * pIndMeasurePointAllOut is not NULL and this function completed the
 * nonparametric method successfully, then the object pointed to by
 * pIndMeasurePointAllOut will contain the matrix of optimal atomic
 * measure points that were determined by the nonparametric method.
 * Its size will be equal to the final number of atomic points in the
 * non-parametric measure, \f$ nMeasurePoint \f$, times the number of
 * individual parameters \f$ nB \f$.
 * The j-th column of the matrix will contain a column vector that is
 * the atomic measure point for the j-th atomic point, \f$ b_j \f$.
 * Otherwise, this function will not attempt to change the contents of
 * the object pointed to by pIndMeasurePointAllOut.
 *
 * @param pIndWeightOut If pIndWeightOut is not NULL, then the
 * SPK_VA::valarray<double> object pointed to by pIndWeightOut must be
 * declared in the function that calls this function, but its input
 * size does not matter.  If pIndWeightOut is not NULL and this
 * function completed the nonparametric method successfully, then the
 * object pointed to by pIndWeightOut will contain the vector of
 * weights associated with the optimal atomic measure points that were
 * determined by the nonparametric method.  Its size will be equal to
 * the final number of atomic points in the non-parametric measure,
 * \f$ nMeasurePoint \f$.  The j-th element of the matrix will contain
 * the weight for the j-th atomic measure point, \f$ b_j \f$.
 * Otherwise, this function will not attempt to change the contents of
 * the object pointed to by pIndWeightOut.
 *
 * @param pIndProbDensityAllOut If pIndProbDensityAllOut is not NULL,
 * then the SPK_VA::valarray<double> object pointed to by
 * pIndProbDensityAllOut must be declared in the function that calls
 * this function, but its input size does not matter.  If
 * pIndProbDensityAllOut is not NULL and this function completed the
 * nonparametric method successfully, then the object pointed to by
 * pIndProbDensityAllOut will contain the matrix of probability
 * densities that were determined by the nonparametric method.  The
 * (i,j)-th element of the matrix will contain the probability density
 * \f$ p(y_i | b_j ) \f$ for \f$ y_i \f$ given the individual
 * parameter \f$ b \f$ is equal to the j-th atomic point, \f$ b_j \f$.
 * Otherwise, this function will not attempt to change the contents of
 * the object pointed to by pIndProbDensityAllOut.
 *
 * @param pIndParPostMeanAllOut If pIndParPostMeanAllOut is not NULL,
 * then the SPK_VA::valarray<double> object pointed to by
 * pIndParPostMeanAllOut must be declared in the function that calls
 * this function, and its size must be equal to the number of
 * individuals \f$ nInd \f$ times the number of individual parameters
 * \f$ nB \f$.  If pIndParPostMeanAllOut is not NULL and this function
 * completed the nonparametric method successfully, then the object
 * pointed to by pIndParPostMeanAllOut will contain the matrix of
 * posterior means of the individual parameters for each of the
 * individuals that were determined by the nonparametric method.  The
 * i-th column of the matrix will contain a column vector that is the
 * posterior mean for the i-th individual,
 * \f[
 *     \overline{b_i} = \mbox{E}[b | y_i ] =
 *       \frac{ \sum_{j=1}^{nMeasurePoint} \lambda_j * p(y_i | b_j ) *  b_j } {\sum_{j=1}^{nMeasurePoint} \lambda_j * p(y_i | b_j) }  .
 * \f]
 * Otherwise, this function will not attempt to change the contents of
 * the object pointed to by pIndParPostMeanAllOut.
 *
 */
/*************************************************************************/

void predNonparamMethod( PopPredModel&                           popModel,
                         IndPredModel&                           indModelWithPopData,
                         IndPredModelBase< CppAD::AD<double> >&  indModelWithPopDataAD,
                         const SPK_VA::valarray<int>&            nMeasurementsAll,
                         const SPK_VA::valarray<double>&         measurementsAll,
                         Optimizer&                              popOptInfo,
                         Optimizer&                              indOptInfo,
                         const SPK_VA::valarray<double>&         indParLow,
                         const SPK_VA::valarray<double>&         indParUp,
                         const SPK_VA::valarray<double>&         indMeasurePointAllIn,
                         double*                                 pPopObjOut,
                         SPK_VA::valarray<double>*               pIndMeasurePointAllOut,
                         SPK_VA::valarray<double>*               pIndWeightOut,
                         SPK_VA::valarray<double>*               pIndProbDensityAllOut,
                         SPK_VA::valarray<double>*               pIndParPostMeanAllOut )
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
      "The Nonparametric method could not be performed because the number of population THETA values \ndid not match the number of individual THETA values.",
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
      "The Nonparametric method could not be performed because the number of population ETA values \ndid not match the number of individual THETA values.",
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
      "The Nonparametric method could not be performed because the number of population EPS values \ndid not match the number of individual ETA values.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Validate the rest of the inputs.
  //------------------------------------------------------------

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

  // Set the constants for the convergence criteria.
  //
  // Use the population level epsilon for the nonparametric
  // optimization problem convergence criteria.
  DoubleMatrix dvecEpsilon( 5, 1 );
  dvecEpsilon.data()[ 0 ] = popOptInfo.getEpsilon();

  // Use the value suggested in the spk_non_par() specifications for
  // the zero weight criteria.
  dvecEpsilon.data()[ 1 ] = 1.0e-4;

  // Use the individual level epsilon for the joining criteria.
  dvecEpsilon.data()[ 2 ] = indOptInfo.getEpsilon();

  // Use the value suggested in the spk_non_par() specifications for
  // the sub-convergence criteria.
  dvecEpsilon.data()[ 3 ] = 1.0e-13;

  // Use the value suggested in the spk_non_par() specifications for
  // the relaxation factor.
  dvecEpsilon.data()[ 4 ] = 0.25;

  // Set the maximum number of iterations.
  //
  // Use the population level number of iterations to solve the
  // nonparametric optimization problem and the individual level
  // number of iterations for the sub-problems.
  DoubleMatrix dvecNMaxIter( 2, 1 );
  dvecNMaxIter.data()[ 0 ] = popOptInfo.getNMaxIter();
  dvecNMaxIter.data()[ 1 ] = indOptInfo.getNMaxIter();

  // Use the population level of tracing.
  int level = popOptInfo.getLevel();


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

  DoubleMatrix dvecBLow ( bLow );
  DoubleMatrix dvecBUp  ( bUp );

  // Set the initial values for all of the individual parameters'
  // atomic measure points.
  //
  // Note that the number of columns for this matrix specifies the
  // initial number of atomic measure points.
  DoubleMatrix dmatIndMeasurePointAllIn(
    indMeasurePointAllIn,
    indMeasurePointAllIn.size() / nB );


  //------------------------------------------------------------
  // Validate the output values for this function.
  //------------------------------------------------------------

  // Check the size for the matrix that will contain all of the
  // posterior means of the individual parameters.
  if( pIndParPostMeanAllOut )
  {
    if( pIndParPostMeanAllOut->size() != nB * nInd )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The Nonparametric method could not be performed because the matrix of individual posterior means had the wrong size.",
        __LINE__,
        __FILE__ );
    }
  }


  //------------------------------------------------------------
  // Prepare the objects to hold the nonparametric output values.
  //------------------------------------------------------------

  // Instantiate matrices and vectors to hold the output values
  // calculate by the nonparametric method.
  //
  // The actual size of these matrices does not matter on input to
  // spk_non_par().
  DoubleMatrix dmatIndMeasurePointAllOut( nB,   nInd );
  DoubleMatrix dvecIndWeightOut         ( nInd, 1 );
  DoubleMatrix dmatIndProbDensityAllOut ( nB,   nInd );
  DoubleMatrix dmatIndParPostMeanAllOut ( nB,   nInd );


  //------------------------------------------------------------
  // Perform the nonparametric method.
  //------------------------------------------------------------

  try
  {
    spk_non_par( level,
                 indModelWithPopDataAD,
                 indModelWithPopData,
                 dvecN,
                 dvecY,
                 dvecNMaxIter,
                 dvecEpsilon,
                 dvecBLow,
                 dvecBUp,
                 dmatIndMeasurePointAllIn,
                 dmatIndMeasurePointAllOut,
                 dvecIndWeightOut,
                 dmatIndProbDensityAllOut );
  }
  catch( SpkException& e )
  {         
    throw e.push(
      SpkError::SPK_UNKNOWN_ERR, 
      "The nonparametric method failed.",
      __LINE__, 
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    throw SpkException(
      stde,
      "A standard exception was thrown during the nonparametric method.", 
      __LINE__,
      __FILE__ );
  }
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR,
      "An unknown exception was thrown during the nonparametric method.", 
      __LINE__,
      __FILE__ );
  }

  // Set the final number of atomic measure points that were
  // determined by the nonparametric method.
  int nMeasurePoint = dmatIndMeasurePointAllOut.nc();


  //------------------------------------------------------------
  // Calculate the nonparametric population objective function.
  //------------------------------------------------------------

  double* pIndWeightOutData      = dvecIndWeightOut.data();
  double* pIndProbDensityOutData = dmatIndProbDensityAllOut.data();

  int j;

  // Calculate the nonparametric population objective function,
  //
  //              nInd
  //              ----
  //              \    
  //     F  =  -  /     log[ p(y | Lambda) ]  ,
  //              ----          i
  //              i = 1 
  //
  // where the probability for the i-th individual's data
  // given Lambda, which is the discrete measure specified by the
  // lambda_j values, is
  //
  //                        nMeasurePoint
  //                            ----
  //                            \    
  //     p(y | Lambda)  =       /     lambda  * p (y | bOut )
  //        i                   ----        j       i      j
  //                            j = 1 
  //
  //
  double prob_i;
  double popObj = 0.0;
  for ( i = 0; i < nInd; i++ )
  {
    prob_i = 0.0;

    // Calculate the probability of this individual's data.
    for ( j = 0; j < nMeasurePoint; j++ )
    {
      prob_i += pIndWeightOutData[j] * pIndProbDensityOutData[i + j * nInd];
    }

    // Add in this individual's contribution to the nonparametric
    // population objective function.
    popObj -= std::log( prob_i );
  }


  //------------------------------------------------------------
  // Calculate the means of the individual model parameters.
  //------------------------------------------------------------

  DoubleMatrix dvecBOut_j      ( nB,        1 );
  DoubleMatrix dvecBMean       ( nB,        1 );
  DoubleMatrix dvecThetaIndMean( nThetaInd, 1 );
  DoubleMatrix dmatThetaIndCov ( nThetaInd, nThetaInd );
  DoubleMatrix dmatOmegaInd_j  ( nEtaInd,   nEtaInd );
  DoubleMatrix dmatOmegaIndMean( nEtaInd,   nEtaInd );

  valarray<double> thetaInd_j( nThetaInd );
  valarray<double> omegaInd_j( nEtaInd * nEtaInd );

  // Initially set these equal to zero.
  dvecThetaIndMean.fill( 0.0 );
  dmatOmegaIndMean.fill( 0.0 );

  // The individual parameters returned by the nonparametric method
  // contain both theta and the parameters that make up Omega, i.e.,
  //
  //             -           -
  //            |   theta     |
  //            |        j    |
  //     b   =  |             |  .
  //      j     |  omegaPar   |
  //            |          j  |
  //             -           -
  //
  // Calculate the mean values for the atomic measure points and for
  // Omega,
  //
  //               nMeasurePoint
  //                   ----
  //                   \    
  //     bMean  =      /     lambda  * bOut   ,
  //                   ----        j       j
  //                   j = 1 
  //
  //
  //                      nMeasurePoint
  //                          ----
  //                          \    
  //     OmegaIndMean  =      /     lambda  * OmegaInd   ,
  //                          ----        j           j
  //                          j = 1 
  //
  for ( j = 0; j < nMeasurePoint; j++ )
  {
    // Get this atomic measure point.
    dvecBOut_j = getCol( dmatIndMeasurePointAllOut, j );
    
    // Set the model's individual parameter value equal to this atomic
    // measure point.
    indModelWithPopData.setIndPar( dvecBOut_j.toValarray() );

    // Add in this atomic measure point's value multiplied by its
    // weight.
    dvecBMean = add( 
      dvecBMean,
      mulByScalar( dvecBOut_j, pIndWeightOutData[j] ) );

    // Get the current atomic measure point's Omega value.
    indModelWithPopData.getOmega( omegaInd_j );
    dmatOmegaInd_j.fromValarray( omegaInd_j );

    // Add in this atomic measure point's Omega value multiplied by
    // its weight.
    dmatOmegaIndMean = add( 
      dmatOmegaIndMean,
      mulByScalar( dmatOmegaInd_j, pIndWeightOutData[j] ) );
  }

  // Get the mean values for theta,
  //
  //                      nMeasurePoint
  //                          ----
  //                          \    
  //     thetaIndMean  =      /     lambda  * thetaInd   ,
  //                          ----        j           j
  //                          j = 1 
  //
  //
  dvecThetaIndMean = getSubblock( dvecBMean, 0, 0, nThetaInd, 1 );


  //------------------------------------------------------------
  // Calculate the covariance of the individual model parameters.
  //------------------------------------------------------------

  DoubleMatrix dvecBOut_jMinusBMean            ( nB, 1 );
  DoubleMatrix dvecBOut_jMinusBMeanTrans       ( 1,  nB );
  DoubleMatrix dmatBOut_jMinusBMeanCrossProd   ( nB, nB );
  DoubleMatrix dmatBOutCov( nB, nB );

  // Initially set this equal to zero.
  dmatBOutCov.fill( 0.0 );

  // Calculate the covariance of the atomic measure points,
  //
  //     bOutCov  =  cov[ bOut , bOut  ]  .
  //                          j      j
  //
  //                 nMeasurePoint
  //                     ----
  //                     \                                                     T
  //              =      /      lambda  * ( bOut  -  bMean ) ( bOut  -  bMean )   .
  //                     ----         j         j                  j       
  //                     j = 1 
  //
  for ( j = 0; j < nMeasurePoint; j++ )
  {
    // Get this atomic measure point.
    dvecBOut_j = getCol( dmatIndMeasurePointAllOut, j );

    // Calculate
    //
    //     bOut  -  bMean  
    //         j
    //
    // and its transpose.
    subtract( dvecBOut_j, dvecBMean, dvecBOut_jMinusBMean );
    transpose( dvecBOut_jMinusBMean, dvecBOut_jMinusBMeanTrans );

    // Calculate the cross product
    //
    //                                          T
    //     ( bOut  -  bMean ) ( bOut  -  bMean )   .
    //           j                  j
    //
    multiply( 
      dvecBOut_jMinusBMean,
      dvecBOut_jMinusBMeanTrans,
      dmatBOut_jMinusBMeanCrossProd );

    // Add in this atomic measure point's cross product multiplied by
    // its weight.
    dmatBOutCov = add( 
      dmatBOutCov,
      mulByScalar( dmatBOut_jMinusBMeanCrossProd, pIndWeightOutData[j] ) );
  }

  // Get the covariance of the atomic measure points' values for
  // theta,
  //
  //     thetaIndCov   =  cov[ thetaInd , thetaInd  ]  .
  //                                   j          j
  //
  dmatThetaIndCov = getSubblock( dmatBOutCov,  0, 0, nThetaInd, nThetaInd );


  //------------------------------------------------------------
  // Calculate the posterior mean of each individual's parameters.
  //------------------------------------------------------------

  double lambda_jTimesProbDensity_i_j;

  DoubleMatrix dvecIndParPostMean_i                        ( nB, 1 );
  DoubleMatrix dvecLambda_jTimesProbDensityTimesBOut_i_jSum( nB, 1 );

  // Calculate the posterior mean for the i-th individual,
  //
  //                        nMeasurePoint
  //                            ----
  //                            \    
  //                            /     lambda  * p (y | bOut ) * bOut
  //                            ----        j       i      j        j
  //                            j = 1 
  //
  //     bOutPostMean   =  ------------------------------------------  ,
  //                 i
  //                            nMeasurePoint
  //                                ----
  //                                \    
  //                                /     lambda  * p (y | bOut )
  //                                ----        j       i      j
  //                                j = 1 
  //
  for ( i = 0; i < nInd; i++ )
  {
    // Zero these before calculating each individual's value.
    prob_i = 0.0;
    dvecLambda_jTimesProbDensityTimesBOut_i_jSum.fill( 0.0 );

    for ( j = 0; j < nMeasurePoint; j++ )
    {
      // Get this atomic measure point.
      dvecBOut_j = getCol( dmatIndMeasurePointAllOut, j );
    
      // Calculate this atomic measure point's weight multiplied by
      // the probability density p(y_i | b_j) for this individual's
      // data given this atomic measure point.
      lambda_jTimesProbDensity_i_j = 
        pIndWeightOutData[j] * pIndProbDensityOutData[i + nInd * j];

      // Add this atomic measure point's contribution to the
      // probability for this individual's data.
      prob_i += lambda_jTimesProbDensity_i_j;

      // Add in this atomic measure point multiplied by
      // its weight and its probability density.
      dvecLambda_jTimesProbDensityTimesBOut_i_jSum = add( 
        dvecLambda_jTimesProbDensityTimesBOut_i_jSum,
        mulByScalar( dvecBOut_j, lambda_jTimesProbDensity_i_j ) );
    }

    // Normalize this individual's posterior mean by dividing by the
    // probability for this individual's data, i.e., the sum of all of
    // the atomic measure point's weight multiplied by their
    // probability densities.
    divByScalar( dvecLambda_jTimesProbDensityTimesBOut_i_jSum,
      prob_i,
      dvecIndParPostMean_i );

    // Set this individual's posterior mean in the matrix of all of
    // the individuals' values.
    replaceJth( dmatIndParPostMeanAllOut, i, dvecIndParPostMean_i );
  }


  //------------------------------------------------------------
  // Set the population model parameters related to the nonparametric method.
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

  // Set the nonparametric population objective function value, if
  // necessary.
  if ( pPopObjOut )
  {
    *pPopObjOut = popObj;;
  }

  // Set the matrix of optimal atomic measure points that were
  // determined by the nonparametric method, if necessary.
  if ( pIndMeasurePointAllOut )
  {
    // Update the size because the final number of atomic measure
    // points can different than the initial number.
    pIndMeasurePointAllOut->resize( 
      dmatIndMeasurePointAllOut.nr() * dmatIndMeasurePointAllOut.nc() );

    *pIndMeasurePointAllOut = dmatIndMeasurePointAllOut.toValarray() ;
  }

  // Set the vector of weights for each optimal atomic measure point,
  // if necessary.
  if ( pIndWeightOut )
  {
    // Update the size because the final number of atomic measure
    // points can different than the initial number.
    pIndWeightOut->resize( dvecIndWeightOut.nr() );

    *pIndWeightOut = dvecIndWeightOut.toValarray() ;
  }

  // Set the matrix of probability densities, if necessary.
  if ( pIndProbDensityAllOut )
  {
    // Update the size because the final number of atomic measure
    // points can different than the initial number.
    pIndProbDensityAllOut->resize(
      dmatIndProbDensityAllOut.nr() * dmatIndProbDensityAllOut.nc() );

    *pIndProbDensityAllOut = dmatIndProbDensityAllOut.toValarray() ;
  }

  // Set the matrix of posterior means for each individual, if
  // necessary.
  if ( pIndParPostMeanAllOut )
  {
    *pIndParPostMeanAllOut = dmatIndParPostMeanAllOut.toValarray() ;
  }

}
