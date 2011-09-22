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
  PredBase< double >&                          predEvaluatorIn,
  PredBase< CppAD::AD<double> >&               predEvaluatorADIn,
  PredBase< CppAD::AD< CppAD::AD<double> > >&  predEvaluatorADADIn,
  int                              nThetaIn,
  const SPK_VA::valarray<double>&  thetaLowIn,
  const SPK_VA::valarray<double>&  thetaUpIn,
  const SPK_VA::valarray<double>&  thetaCurrIn,
  int                              nEtaIn,
  covStruct                        omegaStructIn,
  const SPK_VA::valarray<double>&  omegaMinRepIn )
  :
  IndPredModelBase<double>::IndPredModelBase<double> (
    predEvaluatorIn,
    predEvaluatorADIn,
    predEvaluatorADADIn,
    nThetaIn,
    thetaLowIn,
    thetaUpIn,
    thetaCurrIn,
    nEtaIn,
    omegaStructIn,
    omegaMinRepIn )
{
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
IndPredModel::IndPredModel(
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
  const SPK_VA::valarray<bool>&    omegaMinRepFixedIn )
  :
  IndPredModelBase<double>::IndPredModelBase<double> (
    predEvaluatorIn,
    predEvaluatorADIn,
    predEvaluatorADADIn,
    nThetaIn,
    thetaLowIn,
    thetaUpIn,
    thetaCurrIn,
    nEtaIn,
    omegaStructIn,
    omegaMinRepIn,
    omegaMinRepFixedIn )
{
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
IndPredModel::IndPredModel(
  PredBase< double >&                          predEvaluatorIn,
  PredBase< CppAD::AD<double> >&               predEvaluatorADIn,
  PredBase< CppAD::AD< CppAD::AD<double> > >&  predEvaluatorADADIn,
  int                                 nThetaIn,
  const SPK_VA::valarray<double>&     thetaLowIn,
  const SPK_VA::valarray<double>&     thetaUpIn,
  const SPK_VA::valarray<double>&     thetaCurrIn,
  int                                 nEtaIn,
  covStruct                           omegaStructIn,
  const SPK_VA::valarray<double>&     omegaMinRepIn,
  const SPK_VA::valarray<bool>&       omegaMinRepFixedIn,
  const SPK_VA::valarray<covStruct>&  omegaBlockStruct,
  const SPK_VA::valarray<int>&        omegaBlockDims,
  const SPK_VA::valarray<bool>&       omegaBlockSameAsPrev )
  :
  IndPredModelBase<double>::IndPredModelBase<double> (
    predEvaluatorIn,
    predEvaluatorADIn,
    predEvaluatorADADIn,
    nThetaIn,
    thetaLowIn,
    thetaUpIn,
    thetaCurrIn,
    nEtaIn,
    omegaStructIn,
    omegaMinRepIn,
    omegaMinRepFixedIn,
    omegaBlockStruct,
    omegaBlockDims,
    omegaBlockSameAsPrev )
{
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

  // Set the data mean values for the current individual,
  //
  //     d       f ( theta )  .
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

  // Set the derivative of the derivative of the data values for the
  // current individual,
  //
  //                                      -                                  - 
  //                                     |                         |          |
  //     d       h ( theta )  =  d       |  d     y ( theta eta )  |          |  .
  //      theta   i               theta  |   eta   i               | eta = 0  |
  //                                           -                             - 
  //
  // This function sets the value for h_thetaCurr.
  evalFAndH_theta();

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

