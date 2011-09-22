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
 * File: mapOpt.h
 *
 *
 * Minimizes the map Bayesian objective function.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef MAPOPT_H
#define MAPOPT_H

#include "SpkModel.h"
#include "DoubleMatrix.h"
#include "Optimizer.h"

void mapOpt(  SpkModel<double>& model,
              const DoubleMatrix& dvecY,
              Optimizer& optimizer,
              const DoubleMatrix& dvecBLow,
              const DoubleMatrix& dvecBUp,
              const DoubleMatrix& dvecBIn,
              DoubleMatrix* pdvecBOut,
              const DoubleMatrix& dvecBStep,
              double* pdMapObjOut,
              DoubleMatrix* pdrowMapObj_bOut,
              DoubleMatrix* pdmatMapObj_b_bOut,
              bool withD,
              bool isFO = false,
              const DoubleMatrix* pdvecN = 0,
              const DoubleMatrix* pdvecBMean = 0
           );
#endif
