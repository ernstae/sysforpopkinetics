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
 * File: hTilde.h
 *
 *
 * Calculates the approximate population information matrix.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef HTILDE_H
#define HTILDE_H

#include "SpdModel.h"
#include "DoubleMatrix.h"

void hTilde( SpdModel& model, 
	     const DoubleMatrix& dvecX,
	     const DoubleMatrix& dvecAlp,
	     const DoubleMatrix& dvecAlpStep,
	     DoubleMatrix* pdmatHTildeOut,
	     DoubleMatrix* pdmatHTilde_xOut,
	     DoubleMatrix* pdmatHTilde_alpOut );

#endif
