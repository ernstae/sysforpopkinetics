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
 * @file: BlkDiagCov.cpp
 *
 *
 * Implements BlkDiagCov class.
 *//*
 * Author: David Salinger / Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "BlkDiagCov.h"
#include "DiagCov.h"
#include "FullCov.h"
#include "isEqual.h"

// SPK library header files.
#include <spk/SpkValarray.h>

// Standard library header files.
#include <cassert>
#include <cmath>

using SPK_VA::valarray;
using SPK_VA::slice;

/*************************************************************************
 *
 * Function: BlkDiagCov
 *
 *//**
 * Constructs a block diagonal covariance matrix.  Each covariance block 
 * is either full or diagonal as specified in FullCov or DiagCov. 
 *//*
 *************************************************************************/

BlkDiagCov::BlkDiagCov( int nRowIn,
                        const SPK_VA::valarray<bool>&  minRepFixedIn,
                        const SPK_VA::valarray<covStruct>&  blockStruct,
                        const SPK_VA::valarray<int>&   blockDims,
                        const SPK_VA::valarray<bool>&  blockSameAsPrev )
  :
  BlkDiagCovBase<double>( nRowIn,
                          minRepFixedIn,
                          blockStruct,
                          blockDims,
                          blockSameAsPrev )
{
}

