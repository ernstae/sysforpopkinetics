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
 * @file: BlkDiagCovBase.h
 *
 *
 * Declares BlkDiagCovBase class.
 *//*
 * Author: David Salinger/Mitch Watrous
 *
 *************************************************************************/

#ifndef BLKDIAGCOVBASE_H
#define BLKDIAGCOVBASE_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "Cov.h"

// SPK library header files.
#include <spk/SpkValarray.h>

// Standard library header files.
#include <vector>

/*************************************************************************
 *
 * Class: BlkDiagCovBase
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

template<class Scalar>
class BlkDiagCovBase : public Cov<Scalar>
{
  //------------------------------------------------------------
  // Constructors and destructors.
  //------------------------------------------------------------

public:
  BlkDiagCovBase( int nRowIn, 
                  const SPK_VA::valarray<bool>&  minRepFixedIn,
                  const SPK_VA::valarray<covStruct>&  blockStruct,
                  const SPK_VA::valarray<int>&   blockDims,
                  const SPK_VA::valarray<bool>&  blockSameAsPrev );

  ~BlkDiagCovBase( void );
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

  // BlockDiag version of setPar (replace Cov version).
  void setPar(const SPK_VA::valarray<Scalar>& parIn );

  //------------------------------------------------------------
  // Special Block Diagonal matrix member varibles.
  //------------------------------------------------------------
  //public:
  //int nBlocks; 
  //std::vector<Cov *> block;


  //------------------------------------------------------------
  // Disallowed, implicitly generated member functions.
  //------------------------------------------------------------

private:
  // These functions should not be called, so they are not
  // implemented in BlkDiagCovBase.cpp.
  BlkDiagCovBase( const BlkDiagCovBase& right );
  BlkDiagCovBase& operator=( const BlkDiagCovBase& right );
};


#endif

