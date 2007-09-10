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
 * File: fitPopulation.h
 *
 *
 * This header file includes other headers and declares components
 * necessary for the user to do the population level analysis.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

#ifndef FITPOPULATION_H
#define FITPOPULATION_H

//
// Necessary component headers for fitPopulation() declaration.
//
#include "SpkValarray.h"
#include "SpkModel.h"
#include "Objective.h"
#include "Optimizer.h"
#include "DirBasedParallelControls.h"

#include <CppAD/CppAD.h>

void fitPopulation( SpkModel<double>&                model,
                    SpkModel< CppAD::AD<double> >&   modelAD,
                    enum Objective                   objective,
                    const SPK_VA::valarray<int>&     nMeasurementsAll,
                    const SPK_VA::valarray<double>&  measurementsAll,
                    Optimizer&                       popOptimizer,
                    const SPK_VA::valarray<double>&  popParLow,
                    const SPK_VA::valarray<double>&  popParUp,
                    const SPK_VA::valarray<double>&  popParIn,
                    const SPK_VA::valarray<double>&  popParStep,
                    SPK_VA::valarray<double>*        popParOut,
                    Optimizer&                       indOptimizer,
                    const SPK_VA::valarray<double>&  indParLow,
                    const SPK_VA::valarray<double>&  indParUp,
                    const SPK_VA::valarray<double>&  indParAllIn,
                    const SPK_VA::valarray<double>&  indParStep,
                    SPK_VA::valarray<double>*        indParAllOut,
                    double*                          popObjOut,
                    SPK_VA::valarray<double>*        popObj_popParOut,
                    SPK_VA::valarray<double>*        popObj_popPar_popParOut,
                    bool                             isUsingPvm,
                    bool                             isPvmParallel,
                    const DirBasedParallelControls&  dirBasedParallelControls = DirBasedParallelControls(false, NULL, NULL) );

//
// Headers that are not necessary for fitPopulation() but
// needed for the user to do the population analysis.
// 
#include "namespace_population_analysis.h"
#include "SpkException.h"

#endif
