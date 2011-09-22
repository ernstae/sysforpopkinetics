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
 * File: indResiduals.h
 *
 *
 * Calculates residuals for an individual.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef INDRESIDUALS_H
#define INDRESIDUALS_H

#include "SpkValarray.h"
#include "SpkModel.h"
#include "Objective.h"

void indResiduals( SpkModel<double>&                model,
                   const SPK_VA::valarray<double>&  measurements,
                   const SPK_VA::valarray<double>&  indPar,
                   SPK_VA::valarray<double>*        pIndPredOut,
                   SPK_VA::valarray<double>*        pIndResOut,
                   SPK_VA::valarray<double>*        pIndResWtdOut,
                   SPK_VA::valarray<double>*        pIndParResOut,
                   SPK_VA::valarray<double>*        pIndParResWtdOut );

#endif
