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
 * @file: IndPredModel.h
 *
 *
 * Declares IndPredModel class.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef INDPREDMODEL_H
#define INDPREDMODEL_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "Cov.h"
#include "PredBase.h"

// CppAD header files.
#include <CppAD/CppAD.h>

// SPK library header files.
#include <spk/SpkModel.h>
#include <spk/SpkValarray.h>


/*************************************************************************
 *
 * Class: IndPredModel
 *
 *//**
 * This SpkModel subclass evaluates individual level models that
 * correspond to the expressions in an NM-TRAN $PRED block.
 *//*
 *************************************************************************/

class IndPredModel : public SpkModel
{
  //------------------------------------------------------------
  // Class scope declarations.
  //------------------------------------------------------------

public:
  enum covStruct {DIAGONAL, FULL};


  //------------------------------------------------------------
  // Constructors and destructors.
  //------------------------------------------------------------

public:
IndPredModel::IndPredModel(
    PredBase< CppAD::AD<double> >&   predEvaluatorIn,
    int                              nThetaIn,
    const SPK_VA::valarray<double>&  thetaLowIn,
    const SPK_VA::valarray<double>&  thetaUpIn,
    const SPK_VA::valarray<double>&  thetaCurrIn,
    int                              nEtaIn,
    covStruct                        omegaStructIn,
    const SPK_VA::valarray<double>&  omegaMinRepIn );

  ~IndPredModel();


  //------------------------------------------------------------
  // Model parameter related members.
  //------------------------------------------------------------

protected:
  int nIndPar;                                 ///< Number of individual parameters.

  const int nTheta;                            ///< Number of theta parameters.
  const int nEta;                              ///< Number of eta parameters.
  int       nOmegaPar;                         ///< Number of omega parameters.

  const int thetaOffsetInIndPar;               ///< Offset for theta in the vector of individual parameters.
  const int omegaParOffsetInIndPar;            ///< Offset for the omega parameters in the vector of individual parameters.


  //------------------------------------------------------------
  // State information.
  //------------------------------------------------------------

protected:
  int iCurr;                                   ///< Current individual's index.

  SPK_VA::valarray<double> bCurr;              ///< Current value for individual parameter.

  SPK_VA::valarray<double> thetaCurr;          ///< Current value for theta.
  SPK_VA::valarray<double> etaCurr;            ///< Current value for eta.
  SPK_VA::valarray<double> omegaParCurr;       ///< Current value for the omega parameters.


  //------------------------------------------------------------
  // State changing functions.
  //------------------------------------------------------------

protected:
  void doSelectIndividual( int iIn );

  void doSetIndPar( const SPK_VA::valarray<double>& indParIn );


  //------------------------------------------------------------
  // Cache management members and functions.
  //------------------------------------------------------------

protected:
  void invalidateCache() const;

protected:
  // This is not const because it is set in the constructor body.
  // This is not mutable because it should not change after it is
  // set in the constructor.
  Cov* pOmegaCurr;                                   ///< Pointer to the covariance of eta.

  // These quantities do not have cache flags associated with them
  // because the Cov object maintains them itself.
  mutable SPK_VA::valarray<double> omegaCurr;           ///< Current value for the covariance of eta.
  mutable SPK_VA::valarray<double> omega_omegaParCurr;  ///< Current value for the derivative of the covariance of eta.

  // This is not const because it is reset each time the
  // expressions in the Pred block are evaluated for a 
  // particular individual. 
  mutable CppAD::ADFun<double>*    pPredADFunCurr;    ///< Current Pred block automatic differentiation function object.

  mutable SPK_VA::valarray<double> dataMeanCurr;
  mutable SPK_VA::valarray<double> dataMean_indParCurr;
  mutable SPK_VA::valarray<double> dataVarianceCurr;
  mutable SPK_VA::valarray<double> dataVariance_indParCurr;
  mutable SPK_VA::valarray<double> dataVarianceInvCurr;
  mutable SPK_VA::valarray<double> dataVarianceInv_indParCurr;

  mutable bool isDataMeanCurrOk;
  mutable bool isDataMean_indParCurrOk;
  mutable bool isDataVarianceCurrOk;
  mutable bool isDataVariance_indParCurrOk;
  mutable bool isDataVarianceInvCurrOk;
  mutable bool isDataVarianceInv_indParCurrOk;
  mutable bool isPredADFunCurrOk;
  mutable bool isPredFirstDerivCurrOk;
  mutable bool isPredSecondDerivCurrOk;

  mutable bool usedCachedDataMean;
  mutable bool usedCachedDataMean_indPar;
  mutable bool usedCachedDataVariance;
  mutable bool usedCachedDataVariance_indPar;
  mutable bool usedCachedDataVarianceInv;
  mutable bool usedCachedDataVarianceInv_indPar;
  mutable bool usedCachedPredADFun;
  mutable bool usedCachedPredFirstDeriv;
  mutable bool usedCachedPredSecondDeriv;

public:
  bool getUsedCachedDataMean()               const;
  bool getUsedCachedDataMean_indPar()        const;
  bool getUsedCachedDataVariance()           const;
  bool getUsedCachedDataVariance_indPar()    const;
  bool getUsedCachedDataVarianceInv()        const;
  bool getUsedCachedDataVarianceInv_indPar() const;
  bool getUsedCachedPredADFun()              const;
  bool getUsedCachedPredFirstDeriv()         const;
  bool getUsedCachedPredSecondDeriv()        const;
  bool getUsedCachedOmega()                  const;
  bool getUsedCachedOmega_omegaPar()         const;


  //------------------------------------------------------------
  // Model evaluation functions.
  //------------------------------------------------------------

protected:
  void doDataMean              ( SPK_VA::valarray<double>& ret ) const;
  bool doDataMean_indPar       ( SPK_VA::valarray<double>& ret ) const;

  void doDataVariance          ( SPK_VA::valarray<double>& ret ) const;
  bool doDataVariance_indPar   ( SPK_VA::valarray<double>& ret ) const;
  void doDataVarianceInv       ( SPK_VA::valarray<double>& ret ) const;
  bool doDataVarianceInv_indPar( SPK_VA::valarray<double>& ret ) const;


  //------------------------------------------------------------
  // Pred block related members and functions.
  //------------------------------------------------------------

protected:
  const int nZ;                                ///< Number of independent variables.
  const int thetaOffsetInZ;                    ///< Offset for theta in the vector of independent variables.
  const int etaOffsetInZ;                      ///< Offset for eta in the vector of independent variables.

  int       nW;                                ///< Number of dependent variables for current individual.
  const int fOffsetInW;                        ///< Offset for f in the vector of dependent variables.
  int       yOffsetInW;                        ///< Offset for y in the vector of dependent variables.

  int       nDataRecordCurr;                   ///< Number of data records for current individual.
  int       nObsRecordCurr;                    ///< Number of data records that are observation records for current individual.

  PredBase< CppAD::AD<double> >&  predEvaluator;   ///< Pred block expression evaluator.

  mutable std::vector< CppAD::AD<double> > zCurr;  ///< Current independent variables.
  mutable std::vector< CppAD::AD<double> > wCurr;  ///< Current dependent variables.

  mutable SPK_VA::valarray<double> f_thetaCurr;    ///< Current value for f_theta.
  mutable SPK_VA::valarray<double> hCurr;          ///< Current value for y_eta.
  mutable SPK_VA::valarray<double> h_thetaCurr;    ///< Current value for y_eta_theta.

protected:
  void evalAllPred() const;

  virtual void evalPredFirstDeriv()  const;
  virtual void evalPredSecondDeriv() const;


  //------------------------------------------------------------
  // SPK parameter estimation related members and functions.
  //------------------------------------------------------------

public:
  void getIndParLimits(
    SPK_VA::valarray<double>& indParLow,
    SPK_VA::valarray<double>& indParUp ) const;

  void getIndParStep( SPK_VA::valarray<double>& indParStep ) const;

protected:
  SPK_VA::valarray<double> thetaLow;           ///< Lower limits for theta.
  SPK_VA::valarray<double> thetaUp;            ///< Upper limits for theta.


  //------------------------------------------------------------
  // Miscellaneous helper functions.
  //------------------------------------------------------------

public:
  int getNIndPar() const;

  void getIndPar( SPK_VA::valarray<double>& ret ) const;

  void getTheta( SPK_VA::valarray<double>& ret ) const;
  void getOmega( SPK_VA::valarray<double>& ret ) const;

  void getStandardPar       ( SPK_VA::valarray<double>& ret ) const;
  void getStandardPar_indPar( SPK_VA::valarray<double>& ret ) const;


  //------------------------------------------------------------
  // Disallowed, implicitly generated member functions.
  //------------------------------------------------------------

private:
  // These functions should not be called, so they are not
  // implemented in IndPredModel.cpp.
  IndPredModel();
  IndPredModel( const IndPredModel& right );
  IndPredModel& operator=( const IndPredModel& right );
};


#endif
