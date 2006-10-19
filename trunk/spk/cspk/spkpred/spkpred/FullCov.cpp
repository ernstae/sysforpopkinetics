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
 * @file: FullCov.cpp
 *
 *
 * Implements FullCov class.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "FullCov.h"

// SPK library header files.
#include <spk/AkronBtimesC.h>
#include <spk/inverse.h>
#include <spk/cholesky.h>
#include <spk/SpkValarray.h>

// Standard library header files.
#include <cassert>
#include <cmath>

using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: FullCov
 *
 *//**
 * Constructs a full covariance matrix with nRowIn rows and columns.
 *//*
 *************************************************************************/

FullCov::FullCov( int nRowIn )
  :
  FullCovBase<double>( nRowIn )
{
}

FullCov::FullCov( int nRowIn, const valarray<bool>& minRepFixedIn )
  :
  FullCovBase<double>( nRowIn, minRepFixedIn )
{
}

