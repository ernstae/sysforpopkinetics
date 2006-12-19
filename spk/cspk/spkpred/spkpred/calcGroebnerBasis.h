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
 *//**
 * @file: calcGroebnerBasis.h
 *
 *
 * Declares calcGroebnerBasis() function.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef CALCGROEBNERBASIS_H
#define CALCGROEBNERBASIS_H

extern "C"
{

int calcGroebnerBasis( int         level,
                       int         nTheta,
                       int         thetaSeed,
                       int         nIdentComp,
                       int         nObservType,
                       int         nDoseType,
                       const char* sysExpModelRegChainIn,
                       const char* naturalOrderingIn,
                       const char* charSetOrderingIn,
                       int**       nGroebnerBasisPolyEachOut,
                       int*        nGroebnerBasisPolyTotalOut,
                       char***     groebnerBasisPolyAllOut );

}

#endif

