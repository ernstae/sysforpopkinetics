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
 * File: popStatistics.h
 *
 *
 * This header file includes other headers and declares components
 * necessary for the user to get statistics of population parameter estimates.
 *
 * Author: Jiaji Du
 *
 * Modified later by Sachiko Honda - isolated core computation portion
 *
 * Modified later by: Mitch Watrous
 *
 *************************************************************************/

#ifndef POPSTATISTICS_H
#define POPSTATISTICS_H

#include "SpkValarray.h"
#include "SpkModel.h"
#include "Objective.h"

enum PopCovForm { RSR=1, R, S, HSH, H };

void popStatistics( SpkModel&                       popModel,
                    enum Objective                  objective,
                    const SPK_VA::valarray<int>&    nMeasurementsAll,
                    const SPK_VA::valarray<double>& measurementsAll,
                    const SPK_VA::valarray<double>& popPar,
                    const SPK_VA::valarray<bool>&   popParMask,
                    const SPK_VA::valarray<double>& popObj_popPar_popPar,
                    const SPK_VA::valarray<double>& indParAll,
                    const SPK_VA::valarray<double>& indParLow,
                    const SPK_VA::valarray<double>& indParUp,
                    const SPK_VA::valarray<double>& indParStep,
                    enum PopCovForm                 formulation,
                    SPK_VA::valarray<double>*       popParCovOut,
                    SPK_VA::valarray<double>*       popParSEOut,                          
                    SPK_VA::valarray<double>*       popParCorOut,
                    SPK_VA::valarray<double>*       popParCVOut,                          
                    SPK_VA::valarray<double>*       popParCIOut );

void popStatistics( const SPK_VA::valarray<bool>   & mask,
                    const SPK_VA::valarray<double> & y,
                    const SPK_VA::valarray<double> & alp,
                    const SPK_VA::valarray<double> & indObj_alp,
                    const SPK_VA::valarray<double> & popObj_alp_alp,
                    enum PopCovForm                 form,
                    SPK_VA::valarray<double>      * alpCovOut,
                    SPK_VA::valarray<double>      * alpSEOut,
                    SPK_VA::valarray<double>      * alpCorOut,
                    SPK_VA::valarray<double>      * alpCVOut,
                    SPK_VA::valarray<double>      * alpCIOut );

void popStatistics( const SPK_VA::valarray<double>& y,
                    const SPK_VA::valarray<double>& alp,
                    const SPK_VA::valarray<double>& indObj_alpAll,
                    const SPK_VA::valarray<double>& popObj_alp_alp,
                    const PopCovForm              & formulation,
                    SPK_VA::valarray<double>      * alpCovOut,
                    SPK_VA::valarray<double>      * alpSEOut,
                    SPK_VA::valarray<double>      * alpCorOut,
                    SPK_VA::valarray<double>      * alpCVOut,
                    SPK_VA::valarray<double>      * alpCIOut );

#endif
