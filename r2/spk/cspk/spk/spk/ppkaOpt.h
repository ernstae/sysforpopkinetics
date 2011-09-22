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
 * File: ppkaOpt.h
 *
 *
 * Optimizes the parametric population objective functions.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef PPKAOPT_H
#define PPKAOPT_H

#include <string>
#include "SpkModel.h"
#include "DoubleMatrix.h"
#include "Objective.h"
#include "Optimizer.h"

/* Sigle processor mode */
void ppkaOpt( SpkModel<double>&       model,
              enum  Objective         objective,
              const DoubleMatrix&     dvecN,
              const DoubleMatrix&     dvecY,
              Optimizer&              popOptimizer,
              const DoubleMatrix&     dvecAlpLow,
              const DoubleMatrix&     dvecAlpUp,
              const DoubleMatrix&     dvecAlpIn,
              DoubleMatrix*           pdvecAlpOut,
              const DoubleMatrix&     dvecAlpStep,
              Optimizer&              indOptimizer,
              const DoubleMatrix&     dvecBLow,
              const DoubleMatrix&     dvecBUp,
              const DoubleMatrix&     dmatBIn,
              DoubleMatrix*           pdmatBOut,
              const DoubleMatrix&     dvecBStep,
              double*                 pdLTildeOut,
              DoubleMatrix*           pdrowLTilde_alpOut,
              DoubleMatrix*           pdmatLTilde_alp_alpOut );

/* Parallel processor mode */
void ppkaOpt( 
              SpkModel<double>&       model,
              enum  Objective         objective,
              const DoubleMatrix&     dvecN,
              const DoubleMatrix&     dvecY,
              Optimizer&              popOptimizer,
              const DoubleMatrix&     dvecAlpLow,
              const DoubleMatrix&     dvecAlpUp,
              const DoubleMatrix&     dvecAlpIn,
              DoubleMatrix*           pdvecAlpOut,
              const DoubleMatrix&     dvecAlpStep,
              Optimizer&              indOptimizer,
              const DoubleMatrix&     dvecBLow,
              const DoubleMatrix&     dvecBUp,
              const DoubleMatrix&     dmatBIn,
              DoubleMatrix*           pdmatBOut,
              const DoubleMatrix&     dvecBStep,
              double*                 pdLTildeOut,
              DoubleMatrix*           pdrowLTilde_alpOut,
              DoubleMatrix*           pdmatLTilde_alp_alpOut,
              int                     nPvmTasks,
              bool                    isMultiple,
              const char*             sharedDiskSpace = 0,
              const char*             nodeCommand = 0
              );

#endif
