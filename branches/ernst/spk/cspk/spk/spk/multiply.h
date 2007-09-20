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
#ifndef MULTIPLY_H
#define MULTIPLY_H
#include "DoubleMatrix.h"
#include "SpkValarray.h"

const DoubleMatrix multiply(const DoubleMatrix& A, const DoubleMatrix& B);
void multiply(const DoubleMatrix &dmatA, const DoubleMatrix &dmatB, DoubleMatrix& dmatC);

const SPK_VA::valarray<double> multiply( const SPK_VA::valarray<double>& A, int iAnc, 
                                         const SPK_VA::valarray<double>& B, int iBnc );

#endif
