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
 * File: derParStatistics.h
 *
 *
 * Calculates statistics for a parameter vector that is derived from
 * another parameter vector with a known covariance matrix.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef DERPARSTATISTICS_H
#define DERPARSTATISTICS_H

#include "SpkValarray.h"

void derParStatistics( const SPK_VA::valarray<double>& xCov,        // Covariance matrix for x.
		       const SPK_VA::valarray<double>& z,           // Derived parameter z(x).
		       const SPK_VA::valarray<double>& z_x,         // Derivative of derived parameter.
		       int                             nDegFreedom, // Number of degrees of freedom.
		       SPK_VA::valarray<double>*       zCovOut,     // Derived parameter covariance matrix.
		       SPK_VA::valarray<double>*       zSEOut,      // Derived parameter standard errors.
		       SPK_VA::valarray<double>*       zCorOut,     // Derived parameter correlation matrix.
		       SPK_VA::valarray<double>*       zCVOut,      // Derived parameter coefficients of variation.
		       SPK_VA::valarray<double>*       zCIOut );    // Derived parameter confidence intervals.

void derParStatistics( const SPK_VA::valarray<bool>   & mask,
		       const SPK_VA::valarray<double> & xCov,
		       const SPK_VA::valarray<double> & z,
		       const SPK_VA::valarray<double> & z_x,
		       int                              nDegOfFreedom,
		       SPK_VA::valarray<double>       * alpCovOut,
		       SPK_VA::valarray<double>       * alpInvCovOut,
		       SPK_VA::valarray<double>       * alpSEOut,
		       SPK_VA::valarray<double>       * alpCorOut,
		       SPK_VA::valarray<double>       * alpCVOut,
		       SPK_VA::valarray<double>       * alpCIOut
		       );
#endif
