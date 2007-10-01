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
/*-----------------------------------------------------------------------
 *
 * File: inverse.h
 *
 *
 * Computes the inverse of a square, symmetric, positive-definite,
 * double precision matrix.
 *
 * Revisit-Mitch: We may want to consider moving this function to 
 * DoubleMatrix class as a member function.
 *
 * Author: Mitch Watrous
 *
 *-----------------------------------------------------------------------*/


#ifndef INVERSE_H
#define INVERSE_H

#include "DoubleMatrix.h"
#include "SpkValarray.h"

const DoubleMatrix inverse(const DoubleMatrix& dmatA);
const SPK_VA::valarray<double> inverse(const SPK_VA::valarray<double>& A, int nCols);

#endif
