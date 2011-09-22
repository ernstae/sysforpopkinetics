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
 * File: ppdOpt.h
 *
 *
 * Optimizes the population determinant optimal design criterion.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef PPDOPT_H
#define PPDOPT_H

#include "SpdModel.h"
#include "Optimizer.h"
#include "DoubleMatrix.h"

void ppdOpt( SpdModel&             model,
             Optimizer&            optimizer,
             const DoubleMatrix&   dvecXLow,
             const DoubleMatrix&   dvecXUp,
             const DoubleMatrix&   dvecXIn,
             DoubleMatrix*         pdvecXOut,
             const DoubleMatrix&   dvecXStep,
             const DoubleMatrix&   dvecAlp,
             const DoubleMatrix&   dvecAlpStep,
             double*               pdPhiTildeOut,
             DoubleMatrix*         pdrowPhiTilde_xOut,
             DoubleMatrix*         pdmatPhiTilde_x_xOut );

#endif
