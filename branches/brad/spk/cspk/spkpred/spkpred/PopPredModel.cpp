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
#include "BlkDiagCov.h"
#include "PopPredModel.h"
#include "isEqual.h"
#include "PredBase.h"

// SPK library header files.
#include <spk/AkronBtimesC.h>
#include <spk/allZero.h>
#include <spk/intToOrdinalString.h>
#include <spk/isNotANumber.h>
#include <spk/isUnnormNumber.h>
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

//Constructor (original)
PopPredModel::PopPredModel(
  PredBase< double >&                          predEvaluatorIn,
  PredBase< CppAD::AD<double> >&               predEvaluatorADIn,
  PredBase< CppAD::AD< CppAD::AD<double> > >&  predEvaluatorADADIn,
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
  PopPredModelBase<double>::PopPredModelBase<double>(
    predEvaluatorIn,
    predEvaluatorADIn,
    predEvaluatorADADIn,
    nThetaIn,
    thetaLowIn,
    thetaUpIn,
    thetaCurrIn,
    nEtaIn,
    etaCurrIn,
    nEpsIn,
    omegaStructIn,
    omegaMinRepIn,
    sigmaStructIn,
    sigmaMinRepIn )
{
}

/*************************************************************************
 *
 * Function: PopPredModelBase
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

//Constructor (with input for FIXed elements)
//[Revisit - remove this (2nd) constructor - Dave]
PopPredModel::PopPredModel(
  PredBase< double >&                          predEvaluatorIn,
  PredBase< CppAD::AD<double> >&               predEvaluatorADIn,
  PredBase< CppAD::AD< CppAD::AD<double> > >&  predEvaluatorADADIn,
  int                              nThetaIn,
  const SPK_VA::valarray<double>&  thetaLowIn,
  const SPK_VA::valarray<double>&  thetaUpIn,
  const SPK_VA::valarray<double>&  thetaCurrIn,
  int                              nEtaIn,
  const SPK_VA::valarray<double>&  etaCurrIn,
  int                              nEpsIn,
  covStruct                        omegaStructIn,
  const SPK_VA::valarray<double>&  omegaMinRepIn,
  const SPK_VA::valarray<bool>&    omegaMinRepFixedIn,
  covStruct                        sigmaStructIn,
  const SPK_VA::valarray<double>&  sigmaMinRepIn,
  const SPK_VA::valarray<bool>&    sigmaMinRepFixedIn )
  :
  PopPredModelBase<double>::PopPredModelBase<double>(
    predEvaluatorIn,
    predEvaluatorADIn,
    predEvaluatorADADIn,
    nThetaIn,
    thetaLowIn,
    thetaUpIn,
    thetaCurrIn,
    nEtaIn,
    etaCurrIn,
    nEpsIn,
    omegaStructIn,
    omegaMinRepIn,
    omegaMinRepFixedIn,
    sigmaStructIn,
    sigmaMinRepIn,
    sigmaMinRepFixedIn )
{
}

/*************************************************************************
 *
 * Function: PopPredModelBase
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

//Constructor (3rd version - with block structure info)
PopPredModel::PopPredModel(
  PredBase< double >&                          predEvaluatorIn,
  PredBase< CppAD::AD<double> >&               predEvaluatorADIn,
  PredBase< CppAD::AD< CppAD::AD<double> > >&  predEvaluatorADADIn,
  int                                 nThetaIn,
  const SPK_VA::valarray<double>&     thetaLowIn,
  const SPK_VA::valarray<double>&     thetaUpIn,
  const SPK_VA::valarray<double>&     thetaCurrIn,
  int                                 nEtaIn,
  const SPK_VA::valarray<double>&     etaCurrIn,
  int                                 nEpsIn,
  covStruct                           omegaStructIn,
  const SPK_VA::valarray<double>&     omegaMinRepIn,
  const SPK_VA::valarray<bool>&       omegaMinRepFixedIn,
  const SPK_VA::valarray<covStruct>&  omegaBlockStruct,
  const SPK_VA::valarray<int>&        omegaBlockDims,
  const SPK_VA::valarray<bool>&       omegaBlockSameAsPrev,
  covStruct                           sigmaStructIn,
  const SPK_VA::valarray<double>&     sigmaMinRepIn,
  const SPK_VA::valarray<bool>&       sigmaMinRepFixedIn,
  const SPK_VA::valarray<covStruct>&  sigmaBlockStruct,
  const SPK_VA::valarray<int>&        sigmaBlockDims,
  const SPK_VA::valarray<bool>&       sigmaBlockSameAsPrev )
  :
  PopPredModelBase<double>::PopPredModelBase<double>(
    predEvaluatorIn,
    predEvaluatorADIn,
    predEvaluatorADADIn,
    nThetaIn,
    thetaLowIn,
    thetaUpIn,
    thetaCurrIn,
    nEtaIn,
    etaCurrIn,
    nEpsIn,
    omegaStructIn,
    omegaMinRepIn,
    omegaMinRepFixedIn,
    omegaBlockStruct,
    omegaBlockDims,
    omegaBlockSameAsPrev,
    sigmaStructIn,
    sigmaMinRepIn,
    sigmaMinRepFixedIn,
    sigmaBlockStruct,
    sigmaBlockDims,
    sigmaBlockSameAsPrev )
{
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

  int nRow = nObsRecordCurr;
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

  // Set this derivative of the data mean values for the current
  // individual,
  //
  //     d       f ( theta, eta )  .
  //      theta   i
  //
  // This function sets the value for f_thetaCurr.
  evalFAndH_theta();


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
  for ( j = 0; j < nObsRecordCurr; j++ )
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

  // Set this derivative of the data mean values for the current
  // individual,
  //
  //     d     f ( theta, eta )  .
  //      eta   i
  //
  // This function sets the value for f_etaCurr.
  evalFAndH_eta();


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
  for ( j = 0; j < nObsRecordCurr; j++ )
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

  int nRow = nObsRecordCurr * nObsRecordCurr;
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

  // Set this derivative of the derivative of the data values for the
  // current individual,
  //
  //                                           -                                        - 
  //                                          |                               |          |
  //     d       h ( theta, eta )  =  d       |  d     y ( theta, eta, eps )  |          |  .
  //      theta   i                    theta  |   eps   i                     | eps = 0  |
  //                                           -                                        - 
  //
  // This function sets the value for h_thetaCurr.
  evalFAndH_theta();

  // Save the current value for sigma and its derivative.
  pSigmaCurr->cov( sigmaCurr );
  pSigmaCurr->cov_par( sigma_sigmaParCurr );

  // These will hold the columns of h that correspond to each
  // observation record.
  valarray<double> hColTrans ( nEps );
  valarray<double> hCol_theta( nEps * nTheta );

  // Set the number of rows in the derivative of h.
  int nH_thetaRow = nObsRecordCurr * nEps;

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
  for ( j = 0; j < nObsRecordCurr; j++ )
  {
    // Set the row for this element in the rvec version of R.
    row = j * nObsRecordCurr + j;

    // Set the transpose and the derivative of the column of h
    // that corresponds to this observation record,
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
      hColTrans[m] = hCurr[j + m * nObsRecordCurr];

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

  // Set this derivative of the derivative of the data values for the
  // current individual,
  //
  //                                       -                                        - 
  //                                      |                               |          |
  //     d     h ( theta, eta )  =  d     |  d     y ( theta, eta, eps )  |          |  .
  //      eta   i                    eta  |   eps   i                     | eps = 0  |
  //                                       -                                        - 
  //
  // This function sets the value for h_etaCurr.
  evalFAndH_eta();

  // Save the current value for sigma and its derivative.
  pSigmaCurr->cov( sigmaCurr );
  pSigmaCurr->cov_par( sigma_sigmaParCurr );

  // These will hold the columns of h that correspond to each
  // observation record.
  valarray<double> hColTrans( nEps );
  valarray<double> hCol_eta ( nEps * nEta );

  // Set the number of rows in the derivative of h.
  int nH_etaRow = nObsRecordCurr * nEps;

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
  for ( j = 0; j < nObsRecordCurr; j++ )
  {
    // Set the row for this element in the rvec version of R.
    row = j * nObsRecordCurr + j;

    // Set the transpose and the derivative of the column of h
    // that corresponds to this observation record,
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
      hColTrans[m] = hCurr[j + m * nObsRecordCurr];

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

  int nRow = nObsRecordCurr * nObsRecordCurr;
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
    valarray<double> dataVarianceTemp( nObsRecordCurr * nObsRecordCurr );
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
  for ( j = 0; j < nObsRecordCurr; j++ )
  {
    // Set the row for this element in the rvec version of the 
    // inverse of R.
    row = j * nObsRecordCurr + j;

    // Set the partial derivatives.
    for ( k = 0; k < nPopPar; k++ )
    {
      assert( dataVarianceCurr[j + j * nObsRecordCurr] != 0.0 );

      col = k;

      dataVarianceInv_popParCurr[row + col * nRow] = 
        - dataVariance_popParCurr[row + col * nRow]
        / ( dataVarianceCurr[j + j * nObsRecordCurr] * 
            dataVarianceCurr[j + j * nObsRecordCurr] );
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
  for ( j = 0; j < nObsRecordCurr; j++ )
  {
    // Set the row for this element in the rvec version of the 
    // inverse of R.
    row = j * nObsRecordCurr + j;

    // Set the partial derivatives.
    for ( k = 0; k < nIndPar; k++ )
    {
      assert( dataVarianceCurr[j + j * nObsRecordCurr] != 0.0 );

      col = k;

      dataVarianceInv_indParCurr[row + col * nRow] = 
        - dataVariance_indParCurr[row + col * nRow]
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


