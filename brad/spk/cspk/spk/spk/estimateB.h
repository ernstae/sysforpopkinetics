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
 * estimateB.h
 *
 *
 * Author: Sachiko Honda
 */
#ifndef ESTIMATEB_H
#define ESTIMATEB_H

#include "DoubleMatrix.h"
#include "SpkModel.h"
#include "add.h"
#include "IkronBtimesC.h"
#include "transposeRowBlocks.h"
#include "identity.h"
#include "transpose.h"
#include "Optimizer.h"

void estimateB(
    SpkModel<double> &model,
    bool   isBlsq,
    Optimizer& optimizer,
    const  DoubleMatrix &dvecY,
    const  DoubleMatrix &dvecAlp,
    const  DoubleMatrix &dvecBin,
    const  DoubleMatrix &dvecBlow,
    const  DoubleMatrix &dvecBup,
    const  DoubleMatrix &dvecBstep,
    DoubleMatrix *dvecBhatOut,
    DoubleMatrix *dvecBtildeOut,
    DoubleMatrix *dmatBtilde_alpOut
    );

#endif
