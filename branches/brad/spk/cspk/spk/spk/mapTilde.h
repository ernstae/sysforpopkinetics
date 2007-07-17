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
 * mapTilde.h
 *
 * Declaration of mapTilde
 *
 * Author: Sachiko Honda
 *
 */
#ifndef MAPTILDE_H
#define MAPTILDE_H

#include "SpkModel.h"
#include "DoubleMatrix.h"
#include "Optimizer.h"

void mapTilde(
    SpkModel<double> &model,
    const DoubleMatrix &dvecY,
    Optimizer& optimizer,
    const DoubleMatrix &dvecBinitial, 
    const DoubleMatrix &dvecBlower, 
    const DoubleMatrix &dvecBupper, 
    DoubleMatrix &dvecBout, 
    const DoubleMatrix &dvecBstep,
    DoubleMatrix &dvecNormOut, 
    DoubleMatrix *drowMapObj_bOut, 
    DoubleMatrix *dmatMapObj_b_bOut,
    bool  withD
    );

#endif
