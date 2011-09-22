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
#ifndef MATMAX_H
#define MATMAX_H

#include "DoubleMatrix.h"

// Returns the largest value in the matrix
double matmax( const DoubleMatrix &dmatA );

/* 
 * Return a matrix C such that
 * for all element c(i,j) in C, 
 * where i and j are the row and column indecies respectively,
 * c(i,j) is |a(i,j)| if |a(i,j)| in A >= |b(i,j)| in B,
 * or c(i,j) is |b(i,j)| if |b(i,j)| > |a(i,j)|.
 */
const DoubleMatrix matmax( const DoubleMatrix &dmatA, const DoubleMatrix &dmatB );

#endif
