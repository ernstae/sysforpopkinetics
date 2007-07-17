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
#ifndef ELSQ_X_H
#define ELSQ_X_H

#include "DoubleMatrix.h"
#include "SpkValarray.h"

const DoubleMatrix elsq_x(
                    const DoubleMatrix &dvecZ, 
					const DoubleMatrix &dvecH, 
					const DoubleMatrix &dmatQ,
					const DoubleMatrix &dmatInvQ,
					const DoubleMatrix &dmatH_x,
					const DoubleMatrix &dmatQ_x
					);
const DoubleMatrix elsq_x(
                    const DoubleMatrix &dvecResidual, 
					const DoubleMatrix &dmatQ,
					const DoubleMatrix &dmatInvQ,
					const DoubleMatrix &dmatH_x,
					const DoubleMatrix &dmatQ_x
					);
const SPK_VA::valarray<double> elsq_x(
                    const SPK_VA::valarray<double> &z,      // m size vector
                    const SPK_VA::valarray<double> &h,      // m size vector
                    const SPK_VA::valarray<double> &Q,      // m by m symmetric, positive definite
                    const SPK_VA::valarray<double> &Qinv,   // m by m symmetric, positive definite
                    const SPK_VA::valarray<double> &h_x,    // m by n matrix
                    const SPK_VA::valarray<double> &Q_x     // m*m by n matrix
                    );

#endif
