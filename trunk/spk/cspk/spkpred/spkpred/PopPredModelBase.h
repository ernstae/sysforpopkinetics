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
 * @file: PopPredModelBase.h
 *
 *
 * Declares PopPredModelBase class.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef POPPREDMODELBASE_H
#define POPPREDMODELBASE_H

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
 * Class: PopPredModelBase
 *
 *//**
 * This SpkModel subclass evaluates population level models that
 * correspond to the expressions in an NM-TRAN $PRED block.
 * 
 * The following sections of this description describe the mixed effects
 * models used at the population level by SPK and NONMEM.
 * 
 * <B>SPK Mixed Effects Model</B>
 * 
 * SPK's mixed effects model for the \f$j^{\mbox{th}}\f$ value of the
 * \f$i^{\mbox{th}}\f$ individual's data is 
 *   \f[
 *     y_{i(j)} = f_{i(j)}(\alpha, b_i) + e_{i(j)} ,
 *   \f]
 * where
 *   \f[
 *     R_{i(j,k)}(\alpha, b_i) = \mbox{cov}[e_{i(j)},e_{i(k)}] ,
 *   \f]
 *   \f[
 *     D_{(p,q)}(\alpha) = \mbox{cov}[b_{i(p)},b_{i(q)}] ,
 *   \f]
 * \f$\alpha\f$ is a vector of fixed effects parameters, 
 * and \f$b_i\f$ is the individual's vector of random effects parameters.
 * 
 * The output of SPK's population level estimation is
 *   \f[
 *     \left\{ \alpha^{\mbox{Out}}, 
 *       \{ b_i^{\mbox{Out}} ( \alpha^{\mbox{Out}} ) \} \right\} ,
 *   \f]
 * which are estimates for the optimal values for the fixed 
 * effects parameters along with estimate for the optimal values
 * for each individual's random effects parameters.
 * 
 * 
 * <B>NONMEM Mixed Effects Model</B>
 * 
 * NONMEM's mixed effects model (NONMEM User's Guide - Part V, 
 * NONMEM Project Group, pp. 32-9, 1994)
 * for the \f$j^{\mbox{th}}\f$ value of the \f$i^{\mbox{th}}\f$ individual's data is 
 *   \f[
 *     y_{i(j)} = f(x_{ij}, \theta, \eta_i) + 
 *       \sum_{m=1}^{n_{\Sigma}} h_{(m)}(x_{ij}, \theta, \eta_i) \epsilon_{i(m)} .
 *   \f]
 * where
 *   \f[
 *     \Sigma_{(m,n)} = \mbox{cov}[\epsilon_{i(m)},\epsilon_{i(n)}] ,
 *   \f]
 *   \f[
 *     \Omega_{(p,q)} = \mbox{cov}[\eta_{i(p)},\eta_{i(q)}] ,
 *   \f]
 * \f$\theta\f$, \f$\Sigma\f$, and \f$\Omega\f$ are vectors and matrices of fixed effects parameters,
 * \f$\eta_i\f$ is the individual's vector of random effects parameters,
 * \f$x_{ij}\f$ is a vector of known quantities for the individual
 * such as times and covariates,
 * and \f$n_{\Sigma}\f$ is the number of nonzero elements in the lower 
 * triangle of \f$\Sigma\f$.
 * 
 * The output of NONMEM's population level estimation is
 *   \f[
 *     \left\{ \theta^{\mbox{Out}}, \Omega^{\mbox{Out}},
 *       \Sigma^{\mbox{Out}} \right\} ,
 *   \f]
 * which are estimates for the optimal values for the fixed 
 * effects parameters.
 * 
 * 
 * <B>Comparison of the Mixed Effects Models</B>
 * 
 * The SPK population parameter \f$\alpha\f$ is defined as
 * \f[
 *     \alpha =
 *       \left[ 
 *         \begin{array}{c}
 *           \theta \\
 *           \mbox{omegaPar} \\
 *           \mbox{sigmaPar}
 *         \end{array}
 *       \right] ,
 * \f]
 * where omegaPar and sigmaPar are vectors of covariance matrix
 * parameters that correspond to the minimal representations for
 * \f$\Omega\f$ and \f$\Sigma\f$, respectively.
 *
 * The SPK individual parameter \f$b_i\f$ is defined as
 * \f[
 *     b_i = \eta_i .
 * \f]
 *
 * In these two formulations, \f$b_i\f$ and \f$\eta_i\f$ are
 * equivalent.
 * Thus, the covariance matrices \f$D(\alpha)\f$ and 
 * \f$\Omega\f$ are equal to one another, and
 *   \f[
 *     D_{(p,q)}(\alpha) = \Omega_{(p,q)} .
 *   \f]
 * 
 * Although \f$e_i\f$ and \f$\epsilon_i\f$ are not equivalent, 
 * the covariance matrices \f$R_i(\alpha, b_i)\f$ and 
 * \f$\Sigma\f$ are related as described in the following section,
 *   \f[
 *     R_{i(j,k)}(\alpha, b_i) = \sum_{m=1}^{n_{\Sigma}} \sum_{n=1}^{n_{\Sigma}} \left[
 *       h_{(m)}\left( \rule{0.0in}{0.15in} f(x_{ij}, \theta, \eta_i),
 *         x_{ij}, \theta, \eta_i \right) 
 *       \right]
 *       \Sigma_{(m,n)}
 *       \left[
 *       h_{(n)}\left( \rule{0.0in}{0.15in} f(x_{ik}, \theta, \eta_i),
 *         x_{ik}, \theta, \eta_i \right)
 *      \right] .
 *   \f]
 * Note that \f$\Sigma\f$ is the same for all of the individuals in
 * the population.
 * 
 * 
 * <B>First-order Approximation for the Intraindividual Error Model during Population Level Estimation</B>
 * 
 * Because NM-TRAN makes a first-order approximation for the 
 * intraindividual error model during population level 
 * estimation (NONMEM User's Guide - Part V, NONMEM Project Group, 
 * p. 81, 1994) the corresponding SPK models do the same.
 * 
 * To be specific, let the model for 
 * the \f$j^{\mbox{th}}\f$ value of the \f$i^{\mbox{th}}\f$ individual's data
 * be expressed as the following functional:
 *   \f[
 *     y_{i(j)} = y_{i(j)}\left( \rule{0.0in}{0.15in}
 *       f(x_{ij}, \theta, \eta_i), x_{ij}, \theta, \eta_i, \epsilon_i \right) .
 *   \f]
 * where \f$f(x_{ij}, \theta, \eta_i)\f$ is the mean or expected value 
 * for the data,
 * \f$x_{ij}\f$ is a vector of known quantities for the individual
 * such as times and covariates,
 * \f$\theta\f$ is a vector of fixed effects parameters,
 * \f$\eta_i\f$ is the individual's vector of random effects parameters,
 * and \f$\epsilon_i\f$ is a vector of random variables that appears in
 * NONMEM'S intraindividual error model.
 * 
 * During population level estimation this functional is approximated by
 *   \f{eqnarray}
 *     \lefteqn{ \widetilde{y}_{i(j)} \left( \rule{0.0in}{0.15in} f(x_{ij}, \theta, \eta_i), 
 *         x_{ij}, \theta, \eta_i, \epsilon_i \right) } 
 *         \nonumber \\
 *     & & = y_{i(j)}\left( \rule{0.0in}{0.15in} f(x_{ij}, \theta, \eta_i), 
 *         x_{ij}, \theta, \eta_i, \epsilon_i \right)
 *         \left| \!
 *           \begin{array}{l}
 *             \\
 *             {\scriptstyle \epsilon_i = 0}
 *           \end{array}
 *         \right.
 *       + \sum_{m=1}^{n_{\Sigma}} \left[ \partial_{\epsilon}^{(m)} 
 *         y_{i(j)}\left( \rule{0.0in}{0.15in} f(x_{ij}, \theta, \eta_i), 
 *         x_{ij}, \theta, \eta_i, \epsilon_i \right)
 *         \left| \!
 *           \begin{array}{l}
 *             \\
 *             {\scriptstyle \epsilon_i = 0}
 *           \end{array}
 *         \right.
 *         \right] \epsilon_{i(m)} .
 *   \f}
 * Define the derivative of the functional evaluated at \f$\epsilon_i = 0\f$ as
 *   \f[
 *     h_{(m)}\left( \rule{0.0in}{0.15in} f(x_{ij}, \theta, \eta_i), 
 *         x_{ij}, \theta, \eta_i \right)
 *       = \partial_{\epsilon}^{(m)} 
 *         y_{i(j)}\left( \rule{0.0in}{0.15in} f(x_{ij}, \theta, \eta_i), 
 *         x_{ij}, \theta, \eta_i, \epsilon_i \right)
 *         \left| \!
 *           \begin{array}{l}
 *             \\
 *             {\scriptstyle \epsilon_i = 0}
 *           \end{array}
 *         \right. .
 *   \f]
 * Then, the covariance of the \f$j^{\mbox{th}}\f$ and \f$k^{\mbox{th}}\f$ values of 
 * the \f$i^{\mbox{th}}\f$ individual's data is given by
 *   \f{eqnarray}
 *     \mbox{cov} [ \tilde{y}_{i(j)} , \tilde{y}_{i(k)} ]
 *     & = &  \mbox{cov} \left[
 *       \sum_{m=1}^{n_{\Sigma}} h_{(m)}\left( \rule{0.0in}{0.15in} 
 *         f(x_{ij}, \theta, \eta_i), x_{ij}, \theta, \eta_i \right) \epsilon_{i(m)},
 *       \sum_{n=1}^{n_{\Sigma}} h_{(n)}\left( \rule{0.0in}{0.15in} 
 *         f(x_{ik}, \theta, \eta_i), x_{ik}, \theta, \eta_i \right) \epsilon_{i(n)}
 *       \right] \\
 *     & = & \sum_{m=1}^{n_{\Sigma}} \sum_{n=1}^{n_{\Sigma}} \left[
 *       h_{(m)}\left( \rule{0.0in}{0.15in} f(x_{ij}, \theta, \eta_i),
 *         x_{ij}, \theta, \eta_i \right) 
 *       \right]
 *       \mbox{cov} [ \epsilon_{i(m)} , \epsilon_{i(n)} ]
 *       \left[
 *       h_{(n)}\left( \rule{0.0in}{0.15in} f(x_{ik}, \theta, \eta_i), 
 *         x_{ik}, \theta, \eta_i \right)
 *       \right]
 *     \\
 *     & = & \sum_{m=1}^{n_{\Sigma}} \sum_{n=1}^{n_{\Sigma}} \left[
 *       h_{(m)}\left( \rule{0.0in}{0.15in} f(x_{ij}, \theta, \eta_i),
 *         x_{ij}, \theta, \eta_i \right) 
 *       \right]
 *       \Sigma_{(m,n)}
 *       \left[
 *       h_{(n)}\left( \rule{0.0in}{0.15in} f(x_{ik}, \theta, \eta_i),
 *         x_{ik}, \theta, \eta_i \right)
 *      \right]
 *   \f}
 * This corresponds to \f$R_{i(j,k)}\f$ in SPK notation.
 * 
 * If the intraindividual error model can be expressed in traditional
 * NONMEM form, 
 *   \f[
 *     y_{i(j)} = f(x_{ij}, \theta, \eta_i) 
 *       + \sum_{m=1}^{n_{\Sigma}}
 *       h_{(m)}\left( \rule{0.0in}{0.15in} f(x_{ij}, \theta, \eta_i), 
 *         x_{ik}, \theta, \eta_i \right) \epsilon_{i(m)} ,
 *   \f]
 * then 
 *   \f[
 *     \widetilde{y}_{i(j)} = y_{i(j)} ,
 *   \f]
 * and there is no difference between the approximate and exact
 * intraindividual error models.
 * 
 * If the intraindividual error model is not linear
 * in \f$\epsilon\f$, however, the two forms are not equal.
 * For example, in the case of an exponential error model with a single 
 * \f$\epsilon\f$ component, 
 *   \f[
 *     y_{i(j)} = f(x_{ij}, \theta, \eta_i) \exp[ \epsilon_{i(1)} ] ,
 *   \f]
 * which implies that
 *   \f{eqnarray}
 *     \widetilde{y}_{i(j)} 
 *       & = & f(x_{ij}, \theta, \eta_i) 
 *         + f(x_{ij}, \theta, \eta_i) \epsilon_{i(1)} \\
 *       & \neq & y_{i(j)} ,
 *   \f}
 * and the intraindividual error models are therefore different.
 * 
 * Note that as a result of this first-order approximation
 * an exponential model is equivalent to a constant 
 * coefficient of variation (CCV) model during population level 
 * estimation.
 * 
 *//*
 *************************************************************************/

template<class Scalar>
class PopPredModelBase : public SpkModel<Scalar>
{
  //------------------------------------------------------------
  // Constructors and destructors.
  //------------------------------------------------------------

public:
PopPredModelBase(
    PredBase< Scalar >&                          predEvaluatorIn,
    PredBase< CppAD::AD<Scalar> >&               predEvaluatorADIn,
    PredBase< CppAD::AD< CppAD::AD<Scalar> > >&  predEvaluatorADADIn,
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

PopPredModelBase(
    PredBase< Scalar >&                          predEvaluatorIn,
    PredBase< CppAD::AD<Scalar> >&               predEvaluatorADIn,
    PredBase< CppAD::AD< CppAD::AD<Scalar> > >&  predEvaluatorADADIn,
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

PopPredModelBase(
    PredBase< Scalar >&                          predEvaluatorIn,
    PredBase< CppAD::AD<Scalar> >&               predEvaluatorADIn,
    PredBase< CppAD::AD< CppAD::AD<Scalar> > >&  predEvaluatorADADIn,
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

  ~PopPredModelBase();


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

  SPK_VA::valarray<Scalar> alphaCurr;          ///< Current value for population parameter.
  SPK_VA::valarray<Scalar> bCurr;              ///< Current value for individual parameter.

  SPK_VA::valarray<Scalar> thetaCurr;          ///< Current value for theta.
  SPK_VA::valarray<Scalar> etaCurr;            ///< Current value for eta.
  SPK_VA::valarray<Scalar> epsCurr;            ///< Current value for eps.
  SPK_VA::valarray<Scalar> omegaParCurr;       ///< Current value for the omega parameters.
  SPK_VA::valarray<Scalar> sigmaParCurr;       ///< Current value for the sigma parameters.


  //------------------------------------------------------------
  // State changing functions.
  //------------------------------------------------------------

protected:
  void doSelectIndividual( int iIn );

  void doSetPopPar( const SPK_VA::valarray<Scalar>& popParIn );
  void doSetIndPar( const SPK_VA::valarray<Scalar>& indParIn );

public:
  void setTheta( const SPK_VA::valarray<Scalar>& thetaIn );
  void setOmega( const SPK_VA::valarray<Scalar>& omegaIn );
  void setSigma( const SPK_VA::valarray<Scalar>& sigmaIn );


  //------------------------------------------------------------
  // Cache management members and functions.
  //------------------------------------------------------------

protected:
  void invalidateCache() const;

protected:
  // These are not const because they are set in the constructor body.
  // These are not mutable because they should not change after it is
  // set in the constructor.
  Cov<Scalar>* pOmegaCurr;                                   ///< Pointer to the covariance of eta.
  Cov<Scalar>* pSigmaCurr;                                   ///< Pointer to the covariance of eps.

  // These quantities do not have cache flags associated with them
  // because the Cov objects maintain them themselves.
  mutable SPK_VA::valarray<double> omega_omegaParCurr;    ///< Current value for the derivative of the covariance of eta.
  mutable SPK_VA::valarray<double> omegaInv_omegaParCurr; ///< Current derivative of the inverse of the covariance of eta.
  mutable SPK_VA::valarray<Scalar> sigmaCurr;             ///< Current value for the covariance of eps.
  mutable SPK_VA::valarray<double> sigma_sigmaParCurr;    ///< Current value for the derivative of the covariance of eps.

  mutable SPK_VA::valarray<Scalar> dataMeanCurr;
  mutable SPK_VA::valarray<double> dataMean_popParCurr;
  mutable SPK_VA::valarray<double> dataMean_indParCurr;
  mutable SPK_VA::valarray<Scalar> dataVarianceCurr;
  mutable SPK_VA::valarray<double> dataVariance_popParCurr;
  mutable SPK_VA::valarray<double> dataVariance_indParCurr;
  mutable SPK_VA::valarray<Scalar> dataVarianceInvCurr;
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
  mutable bool isFAndHCurrOk;
  mutable bool isFAndH_thetaCurrOk;
  mutable bool isFAndH_etaCurrOk;

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
  mutable bool usedCachedFAndH;
  mutable bool usedCachedFAndH_theta;
  mutable bool usedCachedFAndH_eta;

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
  bool getUsedCachedFAndH()                    const;
  bool getUsedCachedFAndH_theta()              const;
  bool getUsedCachedFAndH_eta()                const;
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
  void doDataMean                ( SPK_VA::valarray<Scalar>& ret ) const;
  bool doDataMean_popPar         ( SPK_VA::valarray<double>& ret ) const;
  bool doDataMean_indPar         ( SPK_VA::valarray<double>& ret ) const;

  void doDataVariance            ( SPK_VA::valarray<Scalar>& ret ) const;
  bool doDataVariance_popPar     ( SPK_VA::valarray<double>& ret ) const;
  bool doDataVariance_indPar     ( SPK_VA::valarray<double>& ret ) const;

  void doDataVarianceInv         ( SPK_VA::valarray<Scalar>& ret ) const;
  bool doDataVarianceInv_popPar  ( SPK_VA::valarray<double>& ret ) const;
  bool doDataVarianceInv_indPar  ( SPK_VA::valarray<double>& ret ) const;

  void doIndParVariance          ( SPK_VA::valarray<Scalar>& ret ) const;
  bool doIndParVariance_popPar   ( SPK_VA::valarray<double>& ret ) const;

  void doIndParVarianceInv       ( SPK_VA::valarray<Scalar>& ret ) const;
  bool doIndParVarianceInv_popPar( SPK_VA::valarray<double>& ret ) const;


  //------------------------------------------------------------
  // Pred block related members and functions.
  //------------------------------------------------------------

protected:
  const int nZ;                                ///< Number of independent variables.
  const int thetaOffsetInZ;                    ///< Offset for theta in the vector of independent variables.
  const int etaOffsetInZ;                      ///< Offset for eta in the vector of independent variables.
  const int epsOffsetInZ;                      ///< Offset for eps in the vector of independent variables.
  const int omegaParOffsetInZ;                 ///< Offset for the omega parameters in the vector of independent variables.
  int       sigmaParOffsetInZ;                 ///< Offset for the sigma parameters in the vector of independent variables.

  int       nDataRecordCurr;                   ///< Number of data records for current individual.
  int       nObsRecordCurr;                    ///< Number of data records that are observation records for current individual.

  PredBase< Scalar >&                         predEvaluator;     ///< Pred block expression evaluator (Scalar version).
  PredBase< CppAD::AD<Scalar> >&              predEvaluatorAD;   ///< Pred block expression evaluator (AD<Scalar> version).
  PredBase< CppAD::AD< CppAD::AD<Scalar> > >& predEvaluatorADAD; ///< Pred block expression evaluator (AD< AD<Scalar> > version).

  mutable std::vector< Scalar > zCurr;                             ///< Current vector of variables (Scalar version).
  mutable std::vector< CppAD::AD<Scalar> > zCurrAD;                ///< Current vector of variables (AD<Scalar> version).
  mutable std::vector< CppAD::AD< CppAD::AD<Scalar> > > zCurrADAD; ///< Current vector of variables (AD< AD<Scalar> > version).
  mutable std::vector< CppAD::AD<Scalar> > yCurrAD;                ///< Current data values (AD<Scalar> version).
  mutable std::vector< CppAD::AD< CppAD::AD<Scalar> > > yCurrADAD; ///< Current data values (AD< AD<Scalar> > version).
  mutable std::vector< Scalar > fCurr;                             ///< Current data mean values (Scalar version).
  mutable std::vector< CppAD::AD<Scalar> > fCurrAD;                ///< Current data mean values (AD<Scalar> version).
  mutable std::vector< CppAD::AD< CppAD::AD<Scalar> > > fCurrADAD; ///< Current data mean values (AD< AD<Scalar> > version).
  mutable std::vector< CppAD::AD<Scalar> > hCurrAD;                ///< Current data derivative values (AD<Scalar> version).
  mutable std::vector< CppAD::AD<Scalar> > wCurrAD;                ///< Current dependent variables (AD<Scalar> version).

  mutable std::vector< CppAD::AD<Scalar> >              thetaCurrAD;   ///< Current value for theta (AD<Scalar> version).
  mutable std::vector< CppAD::AD< CppAD::AD<Scalar> > > thetaCurrADAD; ///< Current value for theta (AD< AD<Scalar> > version).
  mutable std::vector< CppAD::AD<Scalar> >              etaCurrAD;     ///< Current value for eta (AD<Scalar> version).
  mutable std::vector< CppAD::AD< CppAD::AD<Scalar> > > etaCurrADAD;   ///< Current value for eta (AD< AD<Scalar> > version).
  mutable std::vector< CppAD::AD<Scalar> >              epsCurrAD;     ///< Current value for eps (AD<Scalar> version).
  mutable std::vector< CppAD::AD< CppAD::AD<Scalar> > > epsCurrADAD;   ///< Current value for eps (AD< AD<Scalar> > version).

  mutable SPK_VA::valarray<Scalar> f_thetaCurr;           ///< Current value for f_theta (Scalar version).
  mutable SPK_VA::valarray<Scalar> f_etaCurr;             ///< Current value for f_eta (Scalar version).
  mutable SPK_VA::valarray<Scalar> hCurr;                 ///< Current value for h = y_eps (Scalar version).
  mutable SPK_VA::valarray<Scalar> h_thetaCurr;           ///< Current value for h_theta = y_eps_theta (Scalar version).
  mutable SPK_VA::valarray<Scalar> h_etaCurr;             ///< Current value for h_eta = y_eps_eta (Scalar version).

protected:
  void evalFAndH() const;

  virtual void evalFAndH_theta()  const;
  virtual void evalFAndH_eta()    const;


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

  void getPopPar( SPK_VA::valarray<Scalar>& ret ) const;
  void getIndPar( SPK_VA::valarray<Scalar>& ret ) const;

  void getTheta( SPK_VA::valarray<Scalar>& ret ) const;
  void getEta  ( SPK_VA::valarray<Scalar>& ret ) const;
  void getEps  ( SPK_VA::valarray<Scalar>& ret ) const;
  void getOmega( SPK_VA::valarray<Scalar>& ret ) const;
  void getSigma( SPK_VA::valarray<Scalar>& ret ) const;

  const Cov<Scalar>& getOmega() const;
  const Cov<Scalar>& getSigma() const;

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
  // implemented in PopPredModelBase.cpp.
  PopPredModelBase();
  PopPredModelBase( const PopPredModelBase& right );
  PopPredModelBase& operator=( const PopPredModelBase& right );
};


#endif
