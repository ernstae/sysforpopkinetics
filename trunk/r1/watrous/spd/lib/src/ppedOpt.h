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
 * File: ppedOpt.h
 *
 *
 * Optimizes the modified Laplace approximation for the population
 * expected determinant optimal design criterion.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef PPEDOPT_H
#define PPEDOPT_H

#include "SpdModel.h"
#include "Optimizer.h"
#include "DoubleMatrix.h"

void ppedOpt( SpdModel&             model,
              Optimizer&            xOptimizer,
              const DoubleMatrix&   dvecXLow,
              const DoubleMatrix&   dvecXUp,
              const DoubleMatrix&   dvecXIn,
              DoubleMatrix*         pdvecXOut,
              const DoubleMatrix&   dvecXStep,
              Optimizer&            alpOptimizer,
              const DoubleMatrix&   dvecAlpLow,
              const DoubleMatrix&   dvecAlpUp,
              const DoubleMatrix&   dvecAlpIn,
              DoubleMatrix*         pdvecAlpOut,
              const DoubleMatrix&   dvecAlpStep,
              double*               pdPhiTildeOut,
              DoubleMatrix*         pdrowPhiTilde_xOut,
              DoubleMatrix*         pdmatPhiTilde_x_xOut );

#endif
