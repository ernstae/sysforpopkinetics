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
 * isDmatEpsEqual.h
 *
 * Declare isDmatEpsEqual
 *
 * Author: Sachiko Honda
 */
#ifndef ISDMATEPSEQUAL_H
#define ISDMATEPSEQUAL_H

#include "DoubleMatrix.h"

bool isDmatEpsEqual(
	const DoubleMatrix &dmatA, 
	const DoubleMatrix &dmatB, 
	const DoubleMatrix &dmatS );

#endif
