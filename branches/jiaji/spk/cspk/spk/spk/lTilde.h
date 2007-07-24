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
 * lTilde.h
 *
 * 
 * Author: Sachiko Honda
 */
#ifndef LTILDE_H
#define LTILDE_H

#include <string>
#include "SpkModel.h"
#include "File.h"
#include "DoubleMatrix.h"
#include "Objective.h"
#include "Optimizer.h"

// Single process mode
void lTilde(
        SpkModel<double>           &model,
        enum Objective objective,
        const         DoubleMatrix &dvecY_forAll,  // all individuals' data
        const         DoubleMatrix &dvecNumsOfDataforEachSubject,
        Optimizer&                 optimizer,
        const         DoubleMatrix &dvecAlp,
        const         DoubleMatrix &dvecBlow,
        const         DoubleMatrix &dvecBup,
        const         DoubleMatrix &dvecBstep,
        const         DoubleMatrix &dmatBin_forAll,
        DoubleMatrix *dmatBout,
        double       *dLTildeOut,
        DoubleMatrix *drowLTilde_alpOut,
		DoubleMatrix *dmatLTilde_alpOut = 0
        );

// Multi-process mode
void lTilde(
        bool          isMultiple,
        const File   &sharedDiskSpace,
        SpkModel<double>           &model,
        enum Objective objective,
        const         DoubleMatrix &dvecY_forAll,  // all individuals' data
        const         DoubleMatrix &dvecNumsOfDataforEachSubject,
        Optimizer&                 optimizer,
        const         DoubleMatrix &dvecAlp,
        const         DoubleMatrix &dvecBlow,
        const         DoubleMatrix &dvecBup,
        const         DoubleMatrix &dvecBstep,
        const         DoubleMatrix &dmatBin_forAll,
        DoubleMatrix *dmatBout,
        double       *dLTildeOut,
        DoubleMatrix *drowLTilde_alpOut
        );

#endif
