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
 * lambda2diff.h
 *
 * Transposed central difference approximations (with respect to alp or b)
 * of derivative of lambda with respect to b
 *
 * Author: Sachiko Honda
 *
 */
#ifndef LAMBDA2DIFF_H
#define LAMBDA2DIFF_H

#include "SpkModel.h"
#include "DoubleMatrix.h"

void lambda2diff(
        SpkModel &model,
        const DoubleMatrix &dvecY,
        const DoubleMatrix &dvecAlp,
        const DoubleMatrix &dvecB,
        const DoubleMatrix &dvecBStep,
        DoubleMatrix *dmatLambda_b_bOut,
        DoubleMatrix *dmatLambda_b_b_alpOut,
        DoubleMatrix *dmatLambda_b_b_bOut,
        bool  withD = true
     );
#endif
