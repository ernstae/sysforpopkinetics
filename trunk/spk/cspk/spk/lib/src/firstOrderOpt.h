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
 * File: firstOrderOpt.h
 *
 *
 * Optimizes the parametric population objective functions with first order
 * approximation.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

#ifndef FIRSTORDEROPT_H
#define FIRSTORDEROPT_H

#include "SpkModel.h"
#include "DoubleMatrix.h"
#include "Optimizer.h"

void firstOrderOpt( SpkModel&               model,
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
                    DoubleMatrix*           pdmatLambdaTilde_alpOut
                  );

#endif
