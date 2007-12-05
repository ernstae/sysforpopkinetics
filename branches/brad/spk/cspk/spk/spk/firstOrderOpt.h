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
 * Author: Brad Bell
 *
 *************************************************************************/

#ifndef FIRSTORDEROPT_H
#define FIRSTORDEROPT_H

#include "SpkModel.h"
#include "DoubleMatrix.h"
#include "Optimizer.h"
#include <cppad/cppad.hpp>

void firstOrderOpt(
              SpkModel<double>&              model                      ,
              SpkModel< CppAD::AD<double> >& adModel                    ,
              const DoubleMatrix&            dvecN                      ,
              const DoubleMatrix&            dvecY                      ,
              Optimizer&                     alpOptInfo                 ,
              const DoubleMatrix&            dvecAlpLow                 ,
              const DoubleMatrix&            dvecAlpUp                  ,
              const DoubleMatrix&            dvecAlpIn                  ,
              DoubleMatrix*                  pvecAlpOut                 ,
              const DoubleMatrix&            dvecAlpStep                ,
              Optimizer&                     bOptInfo                   ,
              const DoubleMatrix&            dvecBLow                   ,
              const DoubleMatrix&            dvecBUp                    ,
              const DoubleMatrix&            dmatBIn                    ,
              DoubleMatrix*                  pmatBOut                   ,
              const DoubleMatrix&            dvecBStep                  ,
              double*                        pLTildeOut                 ,
              DoubleMatrix*                  prowLTilde_alpOut          ,
              DoubleMatrix*                  pmatLTilde_alp_alpOut      ,
              DoubleMatrix*                  pmatLTilde_alpOut          
);

#endif
