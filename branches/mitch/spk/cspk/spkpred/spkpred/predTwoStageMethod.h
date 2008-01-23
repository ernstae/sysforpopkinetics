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
 * @file: predTwoStageMethod.h
 *
 *
 * Declares predTwoStageMethod() function.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef PREDTWOSTAGEMETHOD_H
#define PREDTWOSTAGEMETHOD_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "Cov.h"
#include "IndPredModel.h"
#include "PopPredModel.h"
#include "PredBase.h"

// SPK library header files.
#include <spk/Objective.h>
#include <spk/Optimizer.h>
#include <spk/SpkValarray.h>

void predTwoStageMethod( PopPredModel&                    popModel,
                         IndPredModel&                    indModelWithPopModelData,
                         enum  Objective                  method,
                         const SPK_VA::valarray<int>&     nMeasurementsAll,
                         const SPK_VA::valarray<double>&  measurementsAll,
                         Optimizer&                       popOptInfo,
                         Optimizer&                       indOptInfo,
                         SPK_VA::valarray<double>*        pIndParAllOut );

#endif

