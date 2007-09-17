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
 * det.h
 *
 *
 * Computes the determinant of a symmetric, positive-definite,
 * double precision matrix.
 *
 * Author: Mitch Watrous
 */


#ifndef DET_H
#define DET_H

#include "DoubleMatrix.h"
#include "SpkValarray.h"

void det( const DoubleMatrix &dmatA, double* pdB, long int* plC );
void det( const SPK_VA::valarray<double> &A, int n, double* pdB, long int* plC );

#endif
