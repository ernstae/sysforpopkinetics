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
#ifndef RESIDUALS_H
#define RESIDUALS_H

#include "SpkValarray.h"
#include "SpkModel.h"

SPK_VA::valarray<double> residuals(SpkModel &model,
				   int   nSubjects,
				   const SPK_VA::valarray<double> &alp,
				   const SPK_VA::valarray<double> &y,
				   const SPK_VA::valarray<int>    &N,
				   const SPK_VA::valarray<double> &bAll);
#endif
