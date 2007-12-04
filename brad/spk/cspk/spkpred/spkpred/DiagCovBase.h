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
 * @file: DiagCovBase.h
 *
 *
 * Declares DiagCovBase class.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef DIAGCOVBASE_H
#define DIAGCOVBASE_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "Cov.h"

// SPK library header files.
#include <spk/SpkValarray.h>


/*************************************************************************
 *
 * Class: DiagCovBase
 *
 *//**
 * This class supports diagonal covariance matrices.  It is a concrete 
 * subclass of the abstract covariance base class.
 *
 * This class utilizes the following parameterization for the covariance
 * matrix in order to insure that it is positive definite and symmetric:
 * \f[
 *     \mbox{cov}(\mbox{par}) =
 *       \left[ 
 *         \begin{array}{cccc}
 *           \exp(2 \mbox{par}_1) &                      &        &                                0   \\
 *                                & \exp(2 \mbox{par}_2) &        &                                    \\
 *                                &                      & \ddots &                                    \\
 *           0                    &                      &        & \exp(2 \mbox{par}_{\mbox{nPar}})
 *         \end{array}
 *       \right] ,
 * \f]
 * where par contains the current value for the parameters.
 *//*
 *************************************************************************/

template<class Scalar>
class DiagCovBase : public Cov<Scalar>
{
  //------------------------------------------------------------
  // Constructors and destructors.
  //------------------------------------------------------------

public:
  DiagCovBase( int nRowIn );
  DiagCovBase( int nRowIn,  const SPK_VA::valarray<bool>& minRepFixedIn );

  //------------------------------------------------------------
  // Mathematical functions.
  //------------------------------------------------------------

public:
  void cov    ( SPK_VA::valarray<Scalar>& covOut     ) const;
  void cov_par( SPK_VA::valarray<double>& cov_parOut ) const;

  void inv    ( SPK_VA::valarray<Scalar>& invOut     ) const;
  void inv_par( SPK_VA::valarray<double>& inv_parOut ) const;

  void getParLimits(
    SPK_VA::valarray<double>&  parLow,
    SPK_VA::valarray<double>&  parUp ) const;

  void calcPar( 
    const SPK_VA::valarray<Scalar>& covIn,
    SPK_VA::valarray<Scalar>&       parOut ) const;

  void calcCovMinRep( 
    const SPK_VA::valarray<Scalar>& covIn,
    SPK_VA::valarray<Scalar>&       covMinRepOut ) const;

  void calcCovMinRep_par( 
    const SPK_VA::valarray<double>& cov_parIn,
    int                             nCov_parInCol,
    SPK_VA::valarray<double>&       covMinRep_parOut ) const;

  void calcCovMinRepMask( 
    const SPK_VA::valarray<bool>& parMaskIn,
    SPK_VA::valarray<bool>&       covMinRepMaskOut ) const;

  void expandCovMinRep( 
    const SPK_VA::valarray<double>& covMinRepIn,
    SPK_VA::valarray<Scalar>&       covOut ) const;


  //------------------------------------------------------------
  // Disallowed, implicitly generated member functions.
  //------------------------------------------------------------

private:
  // These functions should not be called, so they are not
  // implemented in DiagCovBase.cpp.
  DiagCovBase( const DiagCovBase& right );
  DiagCovBase& operator=( const DiagCovBase& right );
};


#endif

