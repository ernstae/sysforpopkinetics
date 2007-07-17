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
/***********************************************************
*
*	File:	getdata.h
*		
*			Gets input data for test case models.
*
*	Author:	Viet Nguyen
*
***********************************************************/

#ifndef GET_DATA_H
#define GET_DATA_H

#include <assert.h>
#include <fstream>
#include "../../../../spk/SpkValarray.h"

#include "DiffEqnModelTest.h"

bool getdata(const char *const name, int &M, int &Nsum);
bool getdata(const char *const name, int M, int Nsum, SPK_VA::valarray<int> &N,SPK_VA::valarray<double> &gamma,
			 SPK_VA::valarray<double> &w, SPK_VA::valarray<double> &t, SPK_VA::valarray<double> &y);

#endif
