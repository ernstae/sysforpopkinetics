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
 * File: hasUnnormNumber.h
 *
 *
 * Checks to see if a vector contains a value that is unnormalized,
 * i.e., less than (or greater) than the largest (or smallest)
 * normalized value.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef HASUNNORMNUMBER_H
#define HASUNNORMNUMBER_H

template<class VectorType>
bool hasUnnormNumber( const VectorType& vector );

#endif
