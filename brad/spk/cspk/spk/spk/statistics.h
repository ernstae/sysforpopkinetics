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
 * File: statistics.h
 *
 * Declares statistics() function.
 *
 *************************************************************************/

#ifndef STATISTICS_H
#define STATISTICS_H

#include "SpkValarray.h"

void statistics( const SPK_VA::valarray<double>& x,       // vector of which quality is to be analyzed
		 const SPK_VA::valarray<double>& xCov,    // covariance of x
		 int                             degFree, // degree of freedom
		 SPK_VA::valarray<double>*       seOut,   // standard error           
		 SPK_VA::valarray<double>*       corOut,  // correlation matrix
		 SPK_VA::valarray<double>*       cvOut,   // coefficient of variance
		 SPK_VA::valarray<double>*       ciOut    // confidence interval
                  );

void statistics( const SPK_VA::valarray<bool>  & mask,
		 const SPK_VA::valarray<double>& x,       // vector of which quality is to be analyzed
		 const SPK_VA::valarray<double>& xCov,    // covariance of x
		 int                             degFree, // degree of freedom
		 SPK_VA::valarray<double>*       seOut,   // standard error           
		 SPK_VA::valarray<double>*       corOut,  // correlation matrix
		 SPK_VA::valarray<double>*       cvOut,   // coefficient of variance
		 SPK_VA::valarray<double>*       ciOut    // confidence interval
                  );

#endif
