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
 *
 * File: FullCov.h
 *
 *
 * This class supports full covariance matrices.  It is a concrete 
 * subclass of the abstract covariance base class.
 *
 * This class utilizes the following parameterization for the covariance
 * matrix in order to insure that it is positive definite and symmetric:
 *
 *                                     T
 *    cov( par )  =  L( par )  L( par )  ,
 *
 * where par contains the current value for the parameters and
 * L( par ) is the following square, lower triangular matrix,
 *
 *                  -                                                            -
 *                 |  exp[ par  ]                                           0     |
 *                 |          0                                                   |
 *                 |                                                              |
 *                 |     par       exp[ par  ]                                    |
 *                 |        1              2                                      |
 *                 |                                                              |
 *    L( par )  =  |     par          par       exp[ par  ]                       |  ,
 *                 |        3            4              5                         |
 *                 |                                                              |
 *                 |       .            .    .            .                       |
 *                 |       .            .      .            .                     |
 *                 |       .            .        .            .                   |
 *                 |                                                              |
 *                 |                               par          exp[ par       ]  |
 *                 |                                  nPar-2            nPar-1    |
 *                  -                                                            -
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef FULLCOV_H
#define FULLCOV_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "Cov.h"

// SPK library header files.
#include <spk/SpkValarray.h>


/*------------------------------------------------------------------------
 * Class declaration
 *------------------------------------------------------------------------*/

class FullCov : public Cov
{
  //------------------------------------------------------------
  // Constructors and destructors.
  //------------------------------------------------------------

public:
  FullCov( int nRowIn );


  //------------------------------------------------------------
  // Mathematical functions.
  //------------------------------------------------------------

public:
  void cov    ( SPK_VA::valarray<double>& covOut     ) const;
  void cov_par( SPK_VA::valarray<double>& cov_parOut ) const;

  void inv    ( SPK_VA::valarray<double>& invOut     ) const;
  void inv_par( SPK_VA::valarray<double>& inv_parOut ) const;

  void getParLimits(
    SPK_VA::valarray<double>&  parLow,
    SPK_VA::valarray<double>&  parUp ) const;

  void calcPar( 
    const SPK_VA::valarray<double>& covIn,
    SPK_VA::valarray<double>&       parOut ) const;

  void calcCovMinRep( 
    const SPK_VA::valarray<double>& covIn,
    SPK_VA::valarray<double>&       covMinRepOut ) const;

  void calcCovMinRep_par( 
    const SPK_VA::valarray<double>& cov_parIn,
    int                             nCov_parInCol,
    SPK_VA::valarray<double>&       covMinRep_parOut ) const;

  void expandCovMinRep( 
    const SPK_VA::valarray<double>& covMinRepIn,
    SPK_VA::valarray<double>&       covOut ) const;


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

