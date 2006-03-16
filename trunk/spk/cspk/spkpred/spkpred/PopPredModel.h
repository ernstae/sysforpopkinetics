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
 * @file: PopPredModel.h
 *
 *
 * Declares PopPredModel class.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef POPPREDMODEL_H
#define POPPREDMODEL_H

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
 * Class: PopPredModel
 *
 *//**
 * This SpkModel subclass evaluates population level models that
 * correspond to the expressions in an NM-TRAN $PRED block.
 *//*
 *************************************************************************/

class PopPredModel : public SpkModel
{
  //------------------------------------------------------------
  // Constructors and destructors.
  //------------------------------------------------------------

public:
PopPredModel::PopPredModel(
    PredBase< CppAD::AD<double> >&   predEvaluatorIn,
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
    const SPK_VA::valarray<double>&  sigmaMinRepIn );

PopPredModel::PopPredModel(
    PredBase< CppAD::AD<double> >&          predEvaluatorIn,
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
    const SPK_VA::valarray<bool>&    sigmaMinRepFixedIn );

PopPredModel::PopPredModel(
    PredBase< CppAD::AD<double> >&          predEvaluatorIn,
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
    const SPK_VA::valarray<covStruct>&  omegaBlockStruct,
    const SPK_VA::valarray<int>&     omegaBlockDims,
    const SPK_VA::valarray<bool>&    omegaBlockSameAsPrev,
    covStruct                        sigmaStructIn,
    const SPK_VA::valarray<double>&  sigmaMinRepIn,
    const SPK_VA::valarray<bool>&    sigmaMinRepFixedIn,
    const SPK_VA::valarray<covStruct>&  sigmaBlockStruct,
    const SPK_VA::valarray<int>&     sigmaBlockDims,
    const SPK_VA::valarray<bool>&    sigmaBlockSameAsPrev );

  ~PopPredModel();


  //------------------------------------------------------------
  // Model parameter related members.
  //------------------------------------------------------------

protected:
  int nPopPar;                                 ///< Number of population parameters.
  int nIndPar;                                 ///< Number of individual parameters.

  const int nTheta;                            ///< Number of theta parameters.
  const int nEta;                              ///< Number of eta parameters.
  const int nEps;                              ///< Number of eps parameters.
  int       nOmegaPar;                         ///< Number of omega parameters.
  int       nSigmaPar;                         ///< Number of sigma parameters.

  int thetaOffsetInPopPar;                     ///< Offset for theta in the vector of population parameters.
  int omegaParOffsetInPopPar;                  ///< Offset for the omega parameters in the vector of population parameters.
  int sigmaParOffsetInPopPar;                  ///< Offset for the sigma parameters in the vector of population parameters.


  //------------------------------------------------------------
  // State information.
  //------------------------------------------------------------

protected:
  int iCurr;                                   ///< Current individual's index.

  SPK_VA::valarray<double> alphaCurr;          ///< Current value for population parameter.
  SPK_VA::valarray<double> bCurr;              ///< Current value for individual parameter.

  SPK_VA::valarray<double> thetaCurr;          ///< Current value for theta.
  SPK_VA::valarray<double> etaCurr;            ///< Current value for eta.
  SPK_VA::valarray<double> epsCurr;            ///< Current value for eps.
  SPK_VA::valarray<double> omegaParCurr;       ///< Current value for the omega parameters.
  SPK_VA::valarray<double> sigmaParCurr;       ///< Current value for the sigma parameters.


  //------------------------------------------------------------
  // State changing functions.
  //------------------------------------------------------------

protected:
  void doSelectIndividual( int iIn );

  void doSetPopPar( const SPK_VA::valarray<double>& popParIn );
  void doSetIndPar( const SPK_VA::valarray<double>& indParIn );

public:
  void setTheta( const SPK_VA::valarray<double>& thetaIn );
  void setOmega( const SPK_VA::valarray<double>& omegaIn );
  void setSigma( const SPK_VA::valarray<double>& sigmaIn );


  //------------------------------------------------------------
  // Cache management members and functions.
  //------------------------------------------------------------

protected:
  void invalidateCache() const;

protected:
  // These are not const because they are set in the constructor body.
  // These are not mutable because they should not change after it is
  // set in the constructor.
  Cov* pOmegaCurr;                                   ///< Pointer to the covariance of eta.
  Cov* pSigmaCurr;                                   ///< Pointer to the covariance of eps.

  // These quantities do not have cache flags associated with them
  // because the Cov objects maintain them themselves.
  mutable SPK_VA::valarray<double> omega_omegaParCurr;    ///< Current value for the derivative of the covariance of eta.
  mutable SPK_VA::valarray<double> omegaInv_omegaParCurr; ///< Current derivative of the inverse of the covariance of eta.
  mutable SPK_VA::valarray<double> sigmaCurr;             ///< Current value for the covariance of eps.
  mutable SPK_VA::valarray<double> sigma_sigmaParCurr;    ///< Current value for the derivative of the covariance of eps.

  // This is not const because it is reset each time the
  // expressions in the Pred block are evaluated for a 
  // particular individual. 
  mutable CppAD::ADFun<double>*    pPredADFunCurr;    ///< Current Pred block automatic differentiation function object.

  mutable SPK_VA::valarray<double> dataMeanCurr;
  mutable SPK_VA::valarray<double> dataMean_popParCurr;
  mutable SPK_VA::valarray<double> dataMean_indParCurr;
  mutable SPK_VA::valarray<double> dataVarianceCurr;
  mutable SPK_VA::valarray<double> dataVariance_popParCurr;
  mutable SPK_VA::valarray<double> dataVariance_indParCurr;
  mutable SPK_VA::valarray<double> dataVarianceInvCurr;
  mutable SPK_VA::valarray<double> dataVarianceInv_popParCurr;
  mutable SPK_VA::valarray<double> dataVarianceInv_indParCurr;
  mutable SPK_VA::valarray<double> indParVariance_popParCurr;
  mutable SPK_VA::valarray<double> indParVarianceInv_popParCurr;

  mutable bool isDataMeanCurrOk;
  mutable bool isDataMean_popParCurrOk;
  mutable bool isDataMean_indParCurrOk;
  mutable bool isDataVarianceCurrOk;
  mutable bool isDataVariance_popParCurrOk;
  mutable bool isDataVariance_indParCurrOk;
  mutable bool isDataVarianceInvCurrOk;
  mutable bool isDataVarianceInv_popParCurrOk;
  mutable bool isDataVarianceInv_indParCurrOk;
  mutable bool isIndParVariance_popParCurrOk;
  mutable bool isIndParVarianceInv_popParCurrOk;
  mutable bool isPredADFunCurrOk;
  mutable bool isPredFirstDerivCurrOk;
  mutable bool isPredSecondDerivCurrOk;

  mutable bool usedCachedDataMean;
  mutable bool usedCachedDataMean_popPar;
  mutable bool usedCachedDataMean_indPar;
  mutable bool usedCachedDataVariance;
  mutable bool usedCachedDataVariance_popPar;
  mutable bool usedCachedDataVariance_indPar;
  mutable bool usedCachedDataVarianceInv;
  mutable bool usedCachedDataVarianceInv_popPar;
  mutable bool usedCachedDataVarianceInv_indPar;
  mutable bool usedCachedIndParVariance;
  mutable bool usedCachedIndParVariance_popPar;
  mutable bool usedCachedIndParVarianceInv;
  mutable bool usedCachedIndParVarianceInv_popPar;
  mutable bool usedCachedPredADFun;
  mutable bool usedCachedPredFirstDeriv;
  mutable bool usedCachedPredSecondDeriv;

public:
  bool getUsedCachedDataMean()                 const;
  bool getUsedCachedDataMean_popPar()          const;
  bool getUsedCachedDataMean_indPar()          const;
  bool getUsedCachedDataVariance()             const;
  bool getUsedCachedDataVariance_popPar()      const;
  bool getUsedCachedDataVariance_indPar()      const;
  bool getUsedCachedDataVarianceInv()          const;
  bool getUsedCachedDataVarianceInv_popPar()   const;
  bool getUsedCachedDataVarianceInv_indPar()   const;
  bool getUsedCachedIndParVariance()           const;
  bool getUsedCachedIndParVariance_popPar()    const;
  bool getUsedCachedIndParVarianceInv()        const;
  bool getUsedCachedIndParVarianceInv_popPar() const;
  bool getUsedCachedPredADFun()                const;
  bool getUsedCachedPredFirstDeriv()           const;
  bool getUsedCachedPredSecondDeriv()          const;
  bool getUsedCachedOmega()                    const;
  bool getUsedCachedOmega_omegaPar()           const;
  bool getUsedCachedOmegaInv()                 const;
  bool getUsedCachedOmegaInv_omegaPar()        const;
  bool getUsedCachedSigma()                    const;
  bool getUsedCachedSigma_sigmaPar()           const;


  //------------------------------------------------------------
  // Model evaluation functions.
  //------------------------------------------------------------

protected:
  void doDataMean                ( SPK_VA::valarray<double>& ret ) const;
  bool doDataMean_popPar         ( SPK_VA::valarray<double>& ret ) const;
  bool doDataMean_indPar         ( SPK_VA::valarray<double>& ret ) const;

  void doDataVariance            ( SPK_VA::valarray<double>& ret ) const;
  bool doDataVariance_popPar     ( SPK_VA::valarray<double>& ret ) const;
  bool doDataVariance_indPar     ( SPK_VA::valarray<double>& ret ) const;

  void doDataVarianceInv         ( SPK_VA::valarray<double>& ret ) const;
  bool doDataVarianceInv_popPar  ( SPK_VA::valarray<double>& ret ) const;
  bool doDataVarianceInv_indPar  ( SPK_VA::valarray<double>& ret ) const;

  void doIndParVariance          ( SPK_VA::valarray<double>& ret ) const;
  bool doIndParVariance_popPar   ( SPK_VA::valarray<double>& ret ) const;

  void doIndParVarianceInv       ( SPK_VA::valarray<double>& ret ) const;
  bool doIndParVarianceInv_popPar( SPK_VA::valarray<double>& ret ) const;


  //------------------------------------------------------------
  // Pred block related members and functions.
  //------------------------------------------------------------

protected:
  const int nZ;                                ///< Number of independent variables.
  const int thetaOffsetInZ;                    ///< Offset for theta in the vector of independent variables.
  const int etaOffsetInZ;                      ///< Offset for eta in the vector of independent variables.
  const int epsOffsetInZ;                      ///< Offset for eps in the vector of independent variables.

  int       nW;                                ///< Number of dependent variables for current individual.
  const int fOffsetInW;                        ///< Offset for f in the vector of dependent variables.
  int       yOffsetInW;                        ///< Offset for y in the vector of dependent variables.

  int       nDataRecordCurr;                   ///< Number of data records for current individual.
  int       nObsRecordCurr;                    ///< Number of data records that are observation records for current individual.

  PredBase< CppAD::AD<double> >&  predEvaluator;   ///< Pred block expression evaluator.

  mutable std::vector< CppAD::AD<double> > zCurr;  ///< Current independent variables.
  mutable std::vector< CppAD::AD<double> > wCurr;  ///< Current dependent variables.

  mutable SPK_VA::valarray<double> f_thetaCurr;    ///< Current value for f_theta.
  mutable SPK_VA::valarray<double> f_etaCurr;      ///< Current value for f_eta.
  mutable SPK_VA::valarray<double> hCurr;          ///< Current value for y_eps.
  mutable SPK_VA::valarray<double> h_thetaCurr;    ///< Current value for y_eps_theta.
  mutable SPK_VA::valarray<double> h_etaCurr;      ///< Current value for y_eps_eta.

protected:
  void evalAllPred() const;

  virtual void evalPredFirstDeriv()  const;
  virtual void evalPredSecondDeriv() const;


  //------------------------------------------------------------
  // SPK parameter estimation related members and functions.
  //------------------------------------------------------------

public:
  void getPopParLimits(
    SPK_VA::valarray<double>& popParLow,
    SPK_VA::valarray<double>& popParUp ) const;

  void getIndParLimits(
    SPK_VA::valarray<double>& indParLow,
    SPK_VA::valarray<double>& indParUp ) const;

  void getPopParStep( SPK_VA::valarray<double>& popParStep ) const;
  void getIndParStep( SPK_VA::valarray<double>& indParStep ) const;

protected:
  SPK_VA::valarray<double> thetaLow;           ///< Lower limits for theta.
  SPK_VA::valarray<double> thetaUp;            ///< Upper limits for theta.


  //------------------------------------------------------------
  // Miscellaneous helper functions.
  //------------------------------------------------------------

public:
  int getNPopPar() const;
  int getNIndPar() const;

  void getPopPar( SPK_VA::valarray<double>& ret ) const;
  void getIndPar( SPK_VA::valarray<double>& ret ) const;

  void getTheta( SPK_VA::valarray<double>& ret ) const;
  void getEta  ( SPK_VA::valarray<double>& ret ) const;
  void getEps  ( SPK_VA::valarray<double>& ret ) const;
  void getOmega( SPK_VA::valarray<double>& ret ) const;
  void getSigma( SPK_VA::valarray<double>& ret ) const;

  void getStandardPar       ( SPK_VA::valarray<double>& ret ) const;
  void getStandardPar_popPar( SPK_VA::valarray<double>& ret ) const;

  void getStandardParMask( 
    const SPK_VA::valarray<bool>& popParMaskIn,
    SPK_VA::valarray<bool>&       standardParMaskOut ) const;


  //------------------------------------------------------------
  // Disallowed, implicitly generated member functions.
  //------------------------------------------------------------

private:
  // These functions should not be called, so they are not
  // implemented in PopPredModel.cpp.
  PopPredModel();
  PopPredModel( const PopPredModel& right );
  PopPredModel& operator=( const PopPredModel& right );
};


#endif
