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
#ifndef MAPOBJDIFF_H
#define MAPOBJDIFF_H

#include "SpkModel.h"
#include "DoubleMatrix.h"

void mapObjDiff(
                SpkModel &model,
                const DoubleMatrix &dvecY,
                const DoubleMatrix &dvecBStep,
                const DoubleMatrix &dvecB,
                DoubleMatrix *dRowVecMapObj_bOut,
                DoubleMatrix *dSqrVecMapObj_b_bOut,
                bool withD,
                bool isFO,
                const DoubleMatrix *pdvecN = NULL,
                const DoubleMatrix *pdvecBMean = NULL
                );
#endif
