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
#ifndef UTRANTIMESSYMKRONSYMTIMESU_X_H
#define UTRANTIMESSYMKRONSYMTIMESU_X_H

#include "DoubleMatrix.h"

void UTranTimesSymKronSymTimesU_x(
                   const DoubleMatrix& V,    // m by m
                   const DoubleMatrix& V_x,   // m * m by p
                   const DoubleMatrix& U,     // m * m symmetric by k
                   const DoubleMatrix& U_x,   // m*m*k by p
                   int p,
                   const DoubleMatrix A[],
                   DoubleMatrix &C_x         // k*k by p matrices
                   );
#endif
