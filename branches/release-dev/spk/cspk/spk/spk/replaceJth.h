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
 * replaceJth.h
 *
 * Replace the jth column of the original matrix given as the first
 * argument with the column vector given as the 3rd argument.
 *
 * Author: Sachiko Honda
 */
#ifndef REPLACEJTH_H
#define REPLACEJTH_H

#include <iostream>
#include "DoubleMatrix.h"

void replaceJth( DoubleMatrix &dmatOrg, const int jth, const DoubleMatrix &dvecTail );

#endif
