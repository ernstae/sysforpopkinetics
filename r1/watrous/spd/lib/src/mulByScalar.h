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
#ifndef MULBYSCALAR_H
#define MULBYSCALAR_H

#include "DoubleMatrix.h"

const DoubleMatrix mulByScalar(const DoubleMatrix &dmatA, const double scalar);
void mulByScalar(const DoubleMatrix &dmatA, const double scalar, DoubleMatrix& dmatC);


const DoubleMatrix mulByScalar(const DoubleMatrix &dmatA, const DoubleMatrix &dmatScalars);
void mulByScalar(const DoubleMatrix &dmatA, const DoubleMatrix &dmatScalars, DoubleMatrix &dmatC);

#endif
