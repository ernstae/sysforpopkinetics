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
 * File: indStatistics.h
 *
 *
 * Compute the covariance matrix, standard errors, correlation matrix and 
 * 95% confidence interval for individual parameter estimates.
 *
 * Author: Jiaji Du
 *
 * Modified later by: Mitch Watrous
 *
 *************************************************************************/

#ifndef INDSTATISTICS_H
#define INDSTATISTICS_H

#include "popStatistics.h"
#include "SpkValarray.h"
#include "SpkModel.h"

typedef PopCovForm IndCovForm;

void indStatistics( SpkModel&                        model,
                    const SPK_VA::valarray<double>&  measurements,
                    const SPK_VA::valarray<double>&  indPar,
                    const SPK_VA::valarray<bool>&    indParMask,
                    const SPK_VA::valarray<double>&  indObj_indPar_indPar,
                    const IndCovForm                 formulation,
                    SPK_VA::valarray<double>*        indParCovOut,
                    SPK_VA::valarray<double>*        indParSEOut,                          
                    SPK_VA::valarray<double>*        indParCorOut,
                    SPK_VA::valarray<double>*        indParCVOut,
                    SPK_VA::valarray<double>*        indParCIOut,
                    bool                             withD );

// Deprecated.
void indStatistics( const SPK_VA::valarray<bool>&    indParMask,
                    const SPK_VA::valarray<double>&  indPar,
                    const SPK_VA::valarray<double>&  dataMean_indPar,
                    const SPK_VA::valarray<double>&  dataVariance_indPar,
                    const SPK_VA::valarray<double>&  dataVarianceInv,
                    SPK_VA::valarray<double>*        indParCovOut,
                    SPK_VA::valarray<double>*        indParSEOut,                          
                    SPK_VA::valarray<double>*        indParCorOut,
                    SPK_VA::valarray<double>*        indParCVOut,
                    SPK_VA::valarray<double>*        indParCIOut );

// Deprecated.
void indStatistics( const SPK_VA::valarray<double>&  indPar,
                    const SPK_VA::valarray<double>&  dataMean_indPar,
                    const SPK_VA::valarray<double>&  dataVariance_indPar,
                    const SPK_VA::valarray<double>&  dataVarianceInv,
                    SPK_VA::valarray<double>*        indParCovOut,
                    SPK_VA::valarray<double>*        indParSEOut,                          
                    SPK_VA::valarray<double>*        indParCorOut,
                    SPK_VA::valarray<double>*        indParCVOut,
                    SPK_VA::valarray<double>*        indParCIOut );

#endif
