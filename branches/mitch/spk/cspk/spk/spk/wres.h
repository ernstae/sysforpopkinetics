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
 * File: wres.h
 *
 *
 * Calculates residuals and weighted residuals.
 *
 * Author: Sachiko Honda
 *
 * Modified later by: Mitch Watrous
 *
 *************************************************************************/

#ifndef WRES_H
#define WRES_H

#include "SpkValarray.h"

void wres( const SPK_VA::valarray<double>& y, 
           const SPK_VA::valarray<double>& f,
           const SPK_VA::valarray<double>& cov,
           SPK_VA::valarray<double>*       pResOut,
           SPK_VA::valarray<double>*       pWresOut );

void wres( const SPK_VA::valarray<double>& y, 
           const SPK_VA::valarray<double>& f,	      
           const SPK_VA::valarray<double>& cov,	      
           SPK_VA::valarray<double>&       resOut,   
           SPK_VA::valarray<double>&       wresOut );

#endif
