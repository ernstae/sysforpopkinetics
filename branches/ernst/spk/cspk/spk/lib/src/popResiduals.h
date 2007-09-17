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
 * File: popResiduals.h
 *
 *
 * Calculates residuals for all of the individuals in the population.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef POPRESIDUALS_H
#define POPRESIDUALS_H

#include "SpkValarray.h"
#include "SpkModel.h"
#include "Objective.h"

void popResiduals( SpkModel&                        model,
                   enum Objective                   objective,
                   const SPK_VA::valarray<int>&     nMeasurementsAll,
                   const SPK_VA::valarray<double>&  measurementsAll,
                   const SPK_VA::valarray<double>&  popPar,
                   const SPK_VA::valarray<double>&  indParAll,
                   SPK_VA::valarray<double>*        pPopPredOut,
                   SPK_VA::valarray<double>*        pPopResOut,
                   SPK_VA::valarray<double>*        pPopResWtdOut,
                   SPK_VA::valarray<double>*        pPopIndParResOut,
                   SPK_VA::valarray<double>*        pPopIndParResWtdOut );

#endif
