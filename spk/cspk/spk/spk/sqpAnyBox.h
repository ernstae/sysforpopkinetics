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
 * File: sqpAnyBox.h
 *
 *
 * Minimizes an arbitrary smooth function subject to  simple bounds on
 * the variables using a sequential quadratic programming (SQP) method. 
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef SQPANYBOX_H
#define SQPANYBOX_H

#include "DoubleMatrix.h"
#include "Optimizer.h"

namespace sqpanybox{
    typedef void (* FVAL_PROTOTYPE) ( const DoubleMatrix& dvecX, 
                                      double* pdFOut, 
                                      DoubleMatrix* pdrowGOut,
                                      const void* pFvalInfo);
};

void sqpAnyBox( sqpanybox::FVAL_PROTOTYPE fval,
                const void* pFvalInfo, 
                Optimizer& optimizer,
                const DoubleMatrix& dvecXLow,
                const DoubleMatrix& dvecXUp,
                const DoubleMatrix& dvecXIn, 
                DoubleMatrix* pdvecXOut, 
                double* pdFOut, 
                DoubleMatrix* pF_xOut );
#endif
