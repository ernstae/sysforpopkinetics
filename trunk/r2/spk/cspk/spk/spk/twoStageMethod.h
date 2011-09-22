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
 * File: twoStageMethod.h
 *
 *
 * Uses one of the two-stage methods to determine the population mean
 * and covariance of the individual parameters.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef TWOSTAGEMETHOD_H
#define TWOSTAGEMETHOD_H

#include "DoubleMatrix.h"
#include "Objective.h"
#include "Optimizer.h"
#include "SpkModel.h"

void twoStageMethod( SpkModel<double>&    model,
                     enum  Objective      method,
                     const DoubleMatrix&  dvecN,
                     const DoubleMatrix&  dvecY,
                     Optimizer&           popOptInfo,
                     Optimizer&           indOptInfo,
                     const DoubleMatrix&  dvecBLow,
                     const DoubleMatrix&  dvecBUp,
                     const DoubleMatrix&  dmatBIn,
                     DoubleMatrix*        pdmatBOut,
                     const DoubleMatrix&  dvecBStep,
                     DoubleMatrix*        pdvecBMeanOut,
                     DoubleMatrix*        pdmatBCovOut );

#endif
