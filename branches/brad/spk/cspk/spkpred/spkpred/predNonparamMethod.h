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
 * @file: predNonparamMethod.h
 *
 *
 * Declares predNonparamMethod() function.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef PREDNONPARAMMETHOD_H
#define PREDNONPARAMMETHOD_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "IndPredModel.h"
#include "PopPredModel.h"

// SPK library header files.
#include <spk/Optimizer.h>
#include <spk/SpkValarray.h>

void predNonparamMethod( PopPredModel&                           popModel,
                         IndPredModel&                           indModelWithPopData,
                         IndPredModelBase< CppAD::AD<double> >&  indModelWithPopDataAD,
                         const SPK_VA::valarray<int>&            nMeasurementsAll,
                         const SPK_VA::valarray<double>&         measurementsAll,
                         Optimizer&                              popOptInfo,
                         Optimizer&                              indOptInfo,
                         const SPK_VA::valarray<double>&         indParLow,
                         const SPK_VA::valarray<double>&         indParUp,
                         const SPK_VA::valarray<double>&         indMeasurePointAllIn,
                         double*                                 pPopObjOut,
                         SPK_VA::valarray<double>*               pIndMeasurePointAllOut,
                         SPK_VA::valarray<double>*               pIndWeightOut,
                         SPK_VA::valarray<double>*               pIndProbDensityAllOut,
                         SPK_VA::valarray<double>*               pIndParPostMeanAllOut );

#endif

