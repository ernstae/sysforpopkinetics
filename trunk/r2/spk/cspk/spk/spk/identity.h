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
/****************************************************************************
 * 
 * File: identity.h
 *
 * Declaration of overloaded function identity() which returns n by n identity matrix.
 *
 * Author: Sachiko Honda
 *
 ****************************************************************************/
#ifndef IDENTITY_H
#define IDENTITY_H

#include "DoubleMatrix.h"
#include "SpkValarray.h"

const DoubleMatrix identity( int n );
void identity( int n, SPK_VA::valarray<double>& I );

#endif
