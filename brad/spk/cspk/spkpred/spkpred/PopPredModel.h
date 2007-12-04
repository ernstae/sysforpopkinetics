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
#include "PopPredModelBase.h"

// CppAD header files.
#include <CppAD/CppAD.h>

// SPK library header files.
#include <spk/SpkValarray.h>


/*************************************************************************
 *
 * Class: PopPredModel
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

class PopPredModel : public PopPredModelBase<double>
{
  //------------------------------------------------------------
  // Constructors and destructors.
  //------------------------------------------------------------

public:
PopPredModel(
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
    const SPK_VA::valarray<double>&  sigmaMinRepIn );

PopPredModel(
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
    const SPK_VA::valarray<bool>&    sigmaMinRepFixedIn );

PopPredModel(
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
    const SPK_VA::valarray<covStruct>&  omegaBlockStruct,
    const SPK_VA::valarray<int>&     omegaBlockDims,
    const SPK_VA::valarray<bool>&    omegaBlockSameAsPrev,
    covStruct                        sigmaStructIn,
    const SPK_VA::valarray<double>&  sigmaMinRepIn,
    const SPK_VA::valarray<bool>&    sigmaMinRepFixedIn,
    const SPK_VA::valarray<covStruct>&  sigmaBlockStruct,
    const SPK_VA::valarray<int>&     sigmaBlockDims,
    const SPK_VA::valarray<bool>&    sigmaBlockSameAsPrev );


  //------------------------------------------------------------
  // Model evaluation functions.
  //------------------------------------------------------------

protected:
  // These functions could not be implemented for arbitrary types,
  // but they are implemented in this class using double values.
  bool doDataMean_popPar         ( SPK_VA::valarray<double>& ret ) const;
  bool doDataMean_indPar         ( SPK_VA::valarray<double>& ret ) const;

  bool doDataVariance_popPar     ( SPK_VA::valarray<double>& ret ) const;
  bool doDataVariance_indPar     ( SPK_VA::valarray<double>& ret ) const;

  bool doDataVarianceInv_popPar  ( SPK_VA::valarray<double>& ret ) const;
  bool doDataVarianceInv_indPar  ( SPK_VA::valarray<double>& ret ) const;

  bool doIndParVariance_popPar   ( SPK_VA::valarray<double>& ret ) const;

  bool doIndParVarianceInv_popPar( SPK_VA::valarray<double>& ret ) const;


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
