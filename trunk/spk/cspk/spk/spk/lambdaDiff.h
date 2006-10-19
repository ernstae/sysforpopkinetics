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
/*
 * lambdaDiff.h
 *
 * Declaration of lambdaDiff()
 *
 * Author: Sachiko Honda
 */
#ifndef LAMBDADIFF_H
#define LAMBDADIFF_H

#include <iostream>
#include "SpkModel.h"
#include "DoubleMatrix.h"

void lambdaDiff(
                SpkModel<double> &model,
                const DoubleMatrix &dvecY,
                const DoubleMatrix &dvecAlp,
                const DoubleMatrix &dvecAlpStep,
                const DoubleMatrix &dvecB,
                const DoubleMatrix &dvecBStep,
                int   withRespectToWhichVar,
                DoubleMatrix *lambda_xOut,
                DoubleMatrix *lambda_x_alpOut,
                DoubleMatrix *lambda_x_bOut,
                const bool withD = true
                );
#endif
