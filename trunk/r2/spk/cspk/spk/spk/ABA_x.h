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
#ifndef ABA_X_H
#define ABA_X_H

#include "DoubleMatrix.h"

const DoubleMatrix ABA_x(const DoubleMatrix &A, const DoubleMatrix& B, 
                         const DoubleMatrix &A_x, const DoubleMatrix& B_x);
void ABA_x(const DoubleMatrix &A, const DoubleMatrix& B, 
                         const DoubleMatrix &A_x, const DoubleMatrix& B_x, 
                         DoubleMatrix &C);

const SPK_VA::valarray<double> ABA_x(
                         const SPK_VA::valarray<double>& A, int nColsA,
                         const SPK_VA::valarray<double>& B, int nColsB,
                         const SPK_VA::valarray<double> &A_x, 
                         const SPK_VA::valarray<double>& B_x,
                         int nX
                         );

#endif
