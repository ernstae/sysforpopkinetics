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
 * @file: IndPredModelBase.h
 *
 *
 * Declares IndPredModelBase class.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef INDPREDMODELBASE_H
#define INDPREDMODELBASE_H

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
 * Class: IndPredModelBase
 *
 *//**
 * This SpkModel subclass evaluates individual level models that
 * correspond to the expressions in an NM-TRAN $PRED block.
 * 
 * The following sections of this description describe the
 * models used at the individual level by SPK and NONMEM.
 * 
 * <B> SPK Model for an Individual's Data</B>
 * 
 * SPK's model for the \f$j^{\mbox{th}}\f$ value of an individual's
 * data is 
 *   \f[
 *     y_{(j)} = f_{(j)}(b) + e_{(j)} ,
 *   \f]
 * where
 *   \f[
 *     R_{(j,k)}(b) = \mbox{cov}[e_{(j)},e_{(k)}] ,
 *   \f]
 * and \f$b\f$ is the individual's vector of parameters.
 * If the MAP Bayesian objective function is used, then 
 *   \f[
 *     D_{(p,q)} = \mbox{cov}[b_{(p)},b_{(q)}] ,
 *   \f]
 * is the covariance of the individual's parameters.
 * 
 * The output of SPK's individual level estimation is
 *   \f[
 *     b^{\mbox{Out}} ,
 *   \f]
 * which is an estimate for the optimal value for the individual's
 * parameters.
 * 
 * <B>NONMEM Model for an Individual's Data</B>
 * 
 * NONMEM's model (NONMEM User's Guide - Part V, NONMEM Project Group, 
 * pp. 23-31, 1994)
 * for the \f$j^{\mbox{th}}\f$ value of an individual's data is 
 *   \f[
 *     y_{(j)} = f(x_{j}, \theta) + 
 *       \sum_{m=1}^{n_{\Omega}} h_{(m)}(x_{j}, \theta) \eta_{(m)} .
 *   \f]
 * where
 *   \f[
 *     \Omega_{(m,n)} = \mbox{cov}[\eta_{(m)},\eta_{(n)}] ,
 *   \f]
 * \f$\theta\f$ and \f$\Omega\f$ are a vector and matrix of 
 * the individual's parameters, 
 * \f$x_{j}\f$ is a vector of known quantities for the individual
 * such as times and covariates,
 * and \f$n_{\Omega}\f$ is the number of elements along the diagonal
 * of \f$\Omega\f$.
 * NONMEM does not support the MAP Bayesian objective function
 * at the individual level.
 * 
 * The output of NONMEM's individual level estimation is
 *   \f[
 *     \left\{ \theta^{\mbox{Out}}, \Omega^{\mbox{Out}} \right\} ,
 *   \f]
 * which are estimates for the optimal values for the individual's
 * parameters.
 * 
 * 
 * 
 * <B>Comparison of the Models for an Individual's Data</B>
 * 
 * The SPK individual parameter \f$b\f$ is defined as
 * \f[
 *     b =
 *       \left[ 
 *         \begin{array}{c}
 *           \theta \\
 *           \mbox{omegaPar}
 *         \end{array}
 *       \right] ,
 * \f]
 * where omegaPar is a vector of covariance matrix parameters that 
 * correspond to the minimal representation for \f$\Omega\f$.
 *
 * In these two formulations, \f$b\f$ and \f$\eta\f$ are
 * not equivalent, and there is no relationship between
 * the covariance matrices \f$D\f$ and \f$\Omega\f$.
 * 
 * Although \f$e\f$ and \f$\eta\f$ are also not equivalent, 
 * the covariance matrices \f$R(b)\f$ and 
 * \f$\Omega\f$ are related as described in the following section,
 *   \f[
 *     R_{(j,k)}(b) = \sum_{m=1}^{n_{\Omega}} \sum_{n=1}^{n_{\Omega}} \left[
 *       h_{(m)}\left( \rule{0.0in}{0.15in} f(x_j, \theta),
 *         x_j, \theta \right) 
 *       \right]
 *       \Omega_{(m,n)}
 *       \left[
 *       h_{(n)}\left( \rule{0.0in}{0.15in} f(x_{k}, \theta),
 *         x_{k}, \theta \right)
 *      \right] .
 *   \f]
 * 
 * 
 * <B>First-order Approximation for the Intraindividual Error Model during Individual Level Estimation</B>
 * 
 * Because NM-TRAN makes a first-order approximation for the 
 * intraindividual error model during population level 
 * estimation (NONMEM User's Guide - Part V, NONMEM Project Group, 
 * p. 81, 1994) the corresponding SPK models do the same.
 * 
 * To be specific, let the model for 
 * the \f$j^{\mbox{th}}\f$ value of an individual's data
 * be expressed as the following functional:
 *   \f[
 *     y_{(j)} = y_{(j)}\left( \rule{0.0in}{0.15in}
 *       f(x_j, \theta), x_j, \theta, \eta \right) .
 *   \f]
 * where \f$f(x_j, \theta)\f$ is the mean or expected value 
 * for the data,
 * \f$x_j\f$ is a vector of known quantities for the individual
 * such as times and covariates,
 * \f$\theta\f$ is a vector of fixed effects parameters,
 * and \f$\eta\f$ is a vector of random variables that appears in
 * NONMEM'S intraindividual error model.
 * 
 * During individual level estimation this functional is approximated by
 *   \f{eqnarray}
 *     \lefteqn{ \widetilde{y}_{(j)} \left( \rule{0.0in}{0.15in} f(x_j, \theta), 
 *         x_j, \theta, \eta \right) } 
 *         \nonumber \\
 *     & & = y_{(j)}\left( \rule{0.0in}{0.15in} f(x_j, \theta), 
 *         x_j, \theta, \eta \right)
 *         \left| \!
 *           \begin{array}{l}
 *             \\
 *             {\scriptstyle \eta = 0}
 *           \end{array}
 *         \right.
 *       + \sum_{m=1}^{n_{\Omega}} \left[ \partial_{\eta}^{(m)} 
 *         y_{(j)}\left( \rule{0.0in}{0.15in} f(x_j, \theta), 
 *         x_j, \theta, \eta \right)
 *         \left| \!
 *           \begin{array}{l}
 *             \\
 *             {\scriptstyle \eta = 0}
 *           \end{array}
 *         \right.
 *         \right] \eta_{(m)} .
 *   \f}
 * Define the derivative of the functional evaluated at \f$\eta = 0\f$ as
 *   \f[
 *     h_{(m)}\left( \rule{0.0in}{0.15in} f(x_j, \theta), 
 *         x_j, \theta \right)
 *       = \partial_{\eta}^{(m)} 
 *         y_{(j)}\left( \rule{0.0in}{0.15in} f(x_j, \theta), 
 *         x_j, \theta, \eta \right)
 *         \left| \!
 *           \begin{array}{l}
 *             \\
 *             {\scriptstyle \eta = 0}
 *           \end{array}
 *         \right. .
 *   \f]
 * Then, the covariance of the \f$j^{\mbox{th}}\f$ and \f$k^{\mbox{th}}\f$ values of 
 * an individual's data is given by
 *   \f{eqnarray}
 *     \mbox{cov} [ \tilde{y}_j , \tilde{y}_{(k)} ]
 *     & = &  \mbox{cov} \left[
 *       \sum_{m=1}^{n_{\Omega}} h_{(m)}\left( \rule{0.0in}{0.15in} 
 *         f(x_j, \theta), x_j, \theta \right) \eta_{(m)},
 *       \sum_{n=1}^{n_{\Omega}} h_{(n)}\left( \rule{0.0in}{0.15in} 
 *         f(x_{k}, \theta), x_{k}, \theta \right) \eta_{(n)}
 *       \right] \\
 *     & = & \sum_{m=1}^{n_{\Omega}} \sum_{n=1}^{n_{\Omega}} \left[
 *       h_{(m)}\left( \rule{0.0in}{0.15in} f(x_j, \theta),
 *         x_j, \theta \right) 
 *       \right]
 *       \mbox{cov} [ \eta_{(m)} , \eta_{(n)} ]
 *       \left[
 *       h_{(n)}\left( \rule{0.0in}{0.15in} f(x_{k}, \theta), 
 *         x_{k}, \theta \right)
 *       \right]
 *     \\
 *     & = & \sum_{m=1}^{n_{\Omega}} \sum_{n=1}^{n_{\Omega}} \left[
 *       h_{(m)}\left( \rule{0.0in}{0.15in} f(x_j, \theta),
 *         x_j, \theta \right) 
 *       \right]
 *       \Omega_{(m,n)}
 *       \left[
 *       h_{(n)}\left( \rule{0.0in}{0.15in} f(x_{k}, \theta),
 *         x_{k}, \theta \right)
 *      \right]
 *   \f}
 * This corresponds to \f$R_{(j,k)}\f$ in SPK notation.
 * 
 * If the intraindividual error model can be expressed in traditional
 * NONMEM form, 
 *   \f[
 *     y_{(j)} = f(x_j, \theta) 
 *       + \sum_{m=1}^{n_{\Omega}}
 *       h_{(m)}\left( \rule{0.0in}{0.15in} f(x_j, \theta), 
 *         x_{k}, \theta \right) \eta_{(m)} ,
 *   \f]
 * then 
 *   \f[
 *     \widetilde{y}_{(j)} = y_{(j)} ,
 *   \f]
 * and there is no difference between the approximate and exact
 * intraindividual error models.
 * 
 * If the intraindividual error model is not linear
 * in \f$\eta\f$, however, the two forms are not equal.
 * For example, in the case of an exponential error model with a single 
 * \f$\eta\f$ component, 
 *   \f[
 *     y_{(j)} = f(x_j, \theta) \exp[ \eta_{(1)} ] ,
 *   \f]
 * which implies that
 *   \f{eqnarray}
 *     \widetilde{y}_{(j)} 
 *       & = & f(x_j, \theta) 
 *         + f(x_j, \theta) \eta_{(1)} \\
 *       & \neq & y_{(j)} ,
 *   \f}
 * and the intraindividual error models are therefore different.
 * 
 * Note that as a result of this first-order approximation
 * an exponential model is equivalent to a constant 
 * coefficient of variation (CCV) model during individual level 
 * estimation.
 *
 *//*
 *************************************************************************/

template<class Scalar>
class IndPredModelBase : public SpkModel<Scalar>
{
  //------------------------------------------------------------
  // Constructors and destructors.
  //------------------------------------------------------------

public:
IndPredModelBase(
    PredBase< Scalar >&                          predEvaluatorIn,
    PredBase< CppAD::AD<Scalar> >&               predEvaluatorADIn,
    PredBase< CppAD::AD< CppAD::AD<Scalar> > >&  predEvaluatorADADIn,
    int                              nThetaIn,
    const SPK_VA::valarray<double>&  thetaLowIn,
    const SPK_VA::valarray<double>&  thetaUpIn,
    const SPK_VA::valarray<double>&  thetaCurrIn,
    int                              nEtaIn,
    covStruct                        omegaStructIn,
    const SPK_VA::valarray<double>&  omegaMinRepIn );

IndPredModelBase(
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
    const SPK_VA::valarray<bool>&    omegaMinRepFixedIn );

IndPredModelBase(
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
    const SPK_VA::valarray<bool>&    omegaBlockSameAsPrev );

  ~IndPredModelBase();


  //------------------------------------------------------------
  // Model parameter related members.
  //------------------------------------------------------------

protected:
  int nIndPar;                                 ///< Number of individual parameters.

  const int nTheta;                            ///< Number of theta parameters.
  const int nEta;                              ///< Number of eta parameters.
  const int nEps;                              ///< Number of eps parameters.
  int       nOmegaPar;                         ///< Number of omega parameters.

  const int thetaOffsetInIndPar;               ///< Offset for theta in the vector of individual parameters.
  const int omegaParOffsetInIndPar;            ///< Offset for the omega parameters in the vector of individual parameters.


  //------------------------------------------------------------
  // State information.
  //------------------------------------------------------------

protected:
  int iCurr;                                   ///< Current individual's index.

  SPK_VA::valarray<Scalar> bCurr;              ///< Current value for individual parameter.

  SPK_VA::valarray<Scalar> thetaCurr;          ///< Current value for theta.
  SPK_VA::valarray<Scalar> etaCurr;            ///< Current value for eta.
  SPK_VA::valarray<Scalar> omegaParCurr;       ///< Current value for the omega parameters.


  //------------------------------------------------------------
  // State changing functions.
  //------------------------------------------------------------

protected:
  void doSelectIndividual( int iIn );

  void doSetIndPar( const SPK_VA::valarray<Scalar>& indParIn );

public:
  void setTheta( const SPK_VA::valarray<Scalar>& thetaIn );
  void setOmega( const SPK_VA::valarray<Scalar>& omegaIn );


  //------------------------------------------------------------
  // Cache management members and functions.
  //------------------------------------------------------------

protected:
  void invalidateCache() const;

protected:
  // This is not const because it is set in the constructor body.
  // This is not mutable because it should not change after it is
  // set in the constructor.
  Cov<Scalar>* pOmegaCurr;                                   ///< Pointer to the covariance of eta.

  // These quantities do not have cache flags associated with them
  // because the Cov object maintains them itself.
  mutable SPK_VA::valarray<Scalar> omegaCurr;           ///< Current value for the covariance of eta.
  mutable SPK_VA::valarray<double> omega_omegaParCurr;  ///< Current value for the derivative of the covariance of eta.

  mutable SPK_VA::valarray<Scalar> dataMeanCurr;
  mutable SPK_VA::valarray<double> dataMean_indParCurr;
  mutable SPK_VA::valarray<Scalar> dataVarianceCurr;
  mutable SPK_VA::valarray<double> dataVariance_indParCurr;
  mutable SPK_VA::valarray<Scalar> dataVarianceInvCurr;
  mutable SPK_VA::valarray<double> dataVarianceInv_indParCurr;

  mutable bool isDataMeanCurrOk;
  mutable bool isDataMean_indParCurrOk;
  mutable bool isDataVarianceCurrOk;
  mutable bool isDataVariance_indParCurrOk;
  mutable bool isDataVarianceInvCurrOk;
  mutable bool isDataVarianceInv_indParCurrOk;
  mutable bool isFAndHCurrOk;
  mutable bool isFAndH_thetaCurrOk;

  mutable bool usedCachedDataMean;
  mutable bool usedCachedDataMean_indPar;
  mutable bool usedCachedDataVariance;
  mutable bool usedCachedDataVariance_indPar;
  mutable bool usedCachedDataVarianceInv;
  mutable bool usedCachedDataVarianceInv_indPar;
  mutable bool usedCachedFAndH;
  mutable bool usedCachedFAndH_theta;

public:
  bool getUsedCachedDataMean()               const;
  bool getUsedCachedDataMean_indPar()        const;
  bool getUsedCachedDataVariance()           const;
  bool getUsedCachedDataVariance_indPar()    const;
  bool getUsedCachedDataVarianceInv()        const;
  bool getUsedCachedDataVarianceInv_indPar() const;
  bool getUsedCachedFAndH()                  const;
  bool getUsedCachedFAndH_theta()            const;
  bool getUsedCachedOmega()                  const;
  bool getUsedCachedOmega_omegaPar()         const;


  //------------------------------------------------------------
  // Model evaluation functions.
  //------------------------------------------------------------

protected:
  void doDataMean              ( SPK_VA::valarray<Scalar>& ret ) const;
  bool doDataMean_indPar       ( SPK_VA::valarray<double>& ret ) const;

  void doDataVariance          ( SPK_VA::valarray<Scalar>& ret ) const;
  bool doDataVariance_indPar   ( SPK_VA::valarray<double>& ret ) const;

  void doDataVarianceInv       ( SPK_VA::valarray<Scalar>& ret ) const;
  bool doDataVarianceInv_indPar( SPK_VA::valarray<double>& ret ) const;


  //------------------------------------------------------------
  // Pred block related members and functions.
  //------------------------------------------------------------

protected:
  const int nZ;                                ///< Number of independent variables.
  const int thetaOffsetInZ;                    ///< Offset for theta in the vector of independent variables.
  const int etaOffsetInZ;                      ///< Offset for eta in the vector of independent variables.
  const int epsOffsetInZ;                      ///< Offset for eps in the vector of independent variables.
  const int omegaParOffsetInZ;                 ///< Offset for the omega parameters in the vector of independent variables.

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

  mutable SPK_VA::valarray<Scalar> f_thetaCurr;                        ///< Current value for f_theta.
  mutable SPK_VA::valarray<Scalar> hCurr;                              ///< Current value for y_eta.
  mutable SPK_VA::valarray<Scalar> h_thetaCurr;                        ///< Current value for y_eta_theta.

protected:
  void evalFAndH() const;

  virtual void evalFAndH_theta()  const;


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

  void getIndPar( SPK_VA::valarray<Scalar>& ret ) const;

  void getTheta( SPK_VA::valarray<Scalar>& ret ) const;
  void getEta  ( SPK_VA::valarray<Scalar>& ret ) const;
  void getOmega( SPK_VA::valarray<Scalar>& ret ) const;

  const Cov<Scalar>& getOmega() const;

  void getStandardPar       ( SPK_VA::valarray<double>& ret ) const;
  void getStandardPar_indPar( SPK_VA::valarray<double>& ret ) const;

  void getStandardParMask( 
    const SPK_VA::valarray<bool>& indParMaskIn,
    SPK_VA::valarray<bool>&       standardParMaskOut ) const;


  //------------------------------------------------------------
  // Disallowed, implicitly generated member functions.
  //------------------------------------------------------------

private:
  // These functions should not be called, so they are not
  // implemented in IndPredModelBase.cpp.
  IndPredModelBase();
  IndPredModelBase( const IndPredModelBase& right );
  IndPredModelBase& operator=( const IndPredModelBase& right );
};


#endif
