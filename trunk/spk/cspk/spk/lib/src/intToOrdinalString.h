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
 * File: intToOrdinalString.h
 *
 *
 * Converts an integer to an ordinal number string.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef INTTOORDINALSTRING_H
#define INTTOORDINALSTRING_H

#include <string>

enum FirstIntForOrdinals { ZERO_IS_FIRST_INT, ONE_IS_FIRST_INT };

std::string intToOrdinalString( int i, enum FirstIntForOrdinals firstInt );

#endif
