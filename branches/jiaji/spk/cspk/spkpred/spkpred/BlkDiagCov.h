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
 * @file: BlkDiagCov.h
 *
 *
 * Declares BlkDiagCov class.
 *//*
 * Author: David Salinger/Mitch Watrous
 *
 *************************************************************************/

#ifndef BLKDIAGCOV_H
#define BLKDIAGCOV_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "BlkDiagCovBase.h"

// SPK library header files.
#include <spk/SpkValarray.h>

// Standard library header files.
#include <vector>

/*************************************************************************
 *
 * Class: BlkDiagCov
 *
 *//**
 * This class supports block diagonal covariance matrices.  It is a concrete 
 * subclass of the abstract covariance base class.
 *
 * This class utilizes block diagonal structure where each block is either
 * diagonal or full:
 * \f[
 *     \mbox{cov}(\mbox{par}) =
 *       \left[ 
 *         \begin{array}{cccc}
 *           \mbox{block}_1 &                      &        &                                0   \\
 *                                &  \mbox{block}_2 &        &                                    \\
 *                                &                      & \ddots &                                    \\
 *           0                    &                      &        & \mbox{block}_{\mbox{nPar}}
 *         \end{array}
 *       \right] ,
 * \f]
 * The  parameterization of the covariance matirx is made up of the 
 * parameterizations of the individual blocks (see DiagCov and FullCov) to 
 * ensure that  it is positive definite and symmetric.
 *//*
 *************************************************************************/

class BlkDiagCov : public BlkDiagCovBase<double>
{
  //------------------------------------------------------------
  // Constructors and destructors.
  //------------------------------------------------------------

public:
  BlkDiagCov( int nRowIn, 
              const SPK_VA::valarray<bool>&  minRepFixedIn,
              const SPK_VA::valarray<covStruct>&  blockStruct,
              const SPK_VA::valarray<int>&   blockDims,
              const SPK_VA::valarray<bool>&  blockSameAsPrev );

  //------------------------------------------------------------
  // Disallowed, implicitly generated member functions.
  //------------------------------------------------------------

private:
  // These functions should not be called, so they are not
  // implemented in BlkDiagCov.cpp.
  BlkDiagCov( const BlkDiagCov& right );
  BlkDiagCov& operator=( const BlkDiagCov& right );
};


#endif

