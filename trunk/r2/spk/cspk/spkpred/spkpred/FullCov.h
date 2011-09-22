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
 * @file: FullCov.h
 *
 *
 * Declares FullCov class.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef FULLCOV_H
#define FULLCOV_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "FullCovBase.h"

// SPK library header files.
#include <spk/SpkValarray.h>


/*************************************************************************
 *
 * Class: FullCov
 *
 *//**
 * This class supports full covariance matrices.  It is a concrete 
 * subclass of the abstract covariance base class.
 *
 * This class utilizes the following parameterization for the covariance
 * matrix in order to insure that it is positive definite and symmetric:
 * \f[
 *     \mbox{cov}(\mbox{par}) =
 *       L(\mbox{par}) \; L(\mbox{par})^{\mbox{T}} ,
 * \f]
 * where par contains the current value for the parameters and
 *\f$ L(\mbox{par}) \f$ is the following square, lower triangular matrix,
 * \f[
 *     L(\mbox{par}) =
 *       \left[ 
 *         \begin{array}{ccccc}
 *           \exp(\mbox{par}_1) &                    &                    &                            &                             0 \\
 *           \mbox{par}_2       & \exp(\mbox{par}_3) &                    &                            &                               \\
 *           \mbox{par}_4       & \mbox{par}_5       & \exp(\mbox{par}_6) &                            &                               \\
 *           \vdots             & \vdots             & \ddots             & \ddots                     &                               \\
 *                              &                    &                    & \mbox{par}_{\mbox{nPar}-1} & \exp(\mbox{par}_{\mbox{nPar}})
 *         \end{array}
 *       \right] .
 * \f]
 *//*
 *************************************************************************/

class FullCov : public FullCovBase<double>
{
  //------------------------------------------------------------
  // Constructors and destructors.
  //------------------------------------------------------------

public:
  FullCov( int nRowIn );
  FullCov( int nRowIn, const SPK_VA::valarray<bool>& minRepFixedIn );


  //------------------------------------------------------------
  // Disallowed, implicitly generated member functions.
  //------------------------------------------------------------

private:
  // These functions should not be called, so they are not
  // implemented in FullCov.cpp.
  FullCov( const FullCov& right );
  FullCov& operator=( const FullCov& right );
};


#endif

