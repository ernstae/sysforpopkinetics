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
#ifndef CONVERT_STATISTICS_TO_NONMEM_H
#define CONVERT_STATISTICS_TO_NONMEM_H

#include <spk/SpkValarray.h>

void convertStatisticsToNonmem(
			  bool                            isDDiagonal,
			  bool                            isRExponential,
			  const SPK_VA::valarray<double>& popParSpk,
			  const SPK_VA::valarray<double>& popParCovSpk,
              const SPK_VA::valarray<double>& popParSeSpk,
			  const SPK_VA::valarray<double>& popParCorSpk,
			  SPK_VA::valarray<double>&       popParCovNonmem,
			  SPK_VA::valarray<double>&       popParSeNonmem, 
			  SPK_VA::valarray<double>&       popParCorNonmem );
#endif
