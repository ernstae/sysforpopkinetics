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
 * AkronBtimesC.h
 *
 * Author: Sachiko Honda
 */

#ifndef AKRONBTIMESC_H
#define AKRONBTIMESC_H

#include "DoubleMatrix.h"
#include "ABA_x.h"
#include "AkronItimesC.h"
#include "IkronBtimesC.h"
#include "UTranTimesSymKronSymTimesU.h"
#include "UTranTimesSymKronSymTimesU_x.h"

const DoubleMatrix AkronBtimesC(const DoubleMatrix &A, const DoubleMatrix &B, const DoubleMatrix &C);
void AkronBtimesC(const DoubleMatrix &A, const DoubleMatrix &B, const DoubleMatrix &C, DoubleMatrix &Res);

const SPK_VA::valarray<double> AkronBtimesC(const SPK_VA::valarray<double> &A, int nColsA,
                                            const SPK_VA::valarray<double> &B, int nColsB,
                                            const SPK_VA::valarray<double> &C, int nColsC
                                            );

#endif
