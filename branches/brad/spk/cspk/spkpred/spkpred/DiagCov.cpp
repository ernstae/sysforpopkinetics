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
 * @file: DiagCov.cpp
 *
 *
 * Implements DiagCov class.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "DiagCov.h"

// SPK library header files.
#include <spk/SpkValarray.h>

// Standard library header files.
#include <cassert>
#include <cmath>

using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: DiagCov
 *
 *//**
 * Constructs a diagonal covariance matrix with nRowIn rows and columns.
 *//*
 *************************************************************************/

DiagCov::DiagCov( int nRowIn )
  :
  DiagCovBase<double>( nRowIn )
{
}

DiagCov::DiagCov( int nRowIn,  const SPK_VA::valarray<bool>& minRepFixedIn )
  :
  DiagCovBase<double>( nRowIn, minRepFixedIn )
{
}

