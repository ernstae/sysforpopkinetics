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
 * File: popStatistics.h
 *
 *
 * This header file includes other headers and declares components
 * necessary for the user to get statistics of population parameter estimates.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

#ifndef POPSTATISTICS_H
#define POPSTATISTICS_H

//
// Necessary component headers for statistics() declaration.
//
#include "SpkValarray.h"
#include "SpkModel.h"
#include "Objective.h"

enum PopCovForm { RSR=1, R, S };

void popStatistics( 
         SpkModel&                       popModel,
         enum Objective                  objective,
         const SPK_VA::valarray<int>&    nMeasurementsAll,
         const SPK_VA::valarray<double>& measurementsAll,
         const SPK_VA::valarray<double>& popPar,
         const SPK_VA::valarray<double>& popObj_popPar_popPar,
         const SPK_VA::valarray<double>& indParAll,
         const SPK_VA::valarray<double>& indParLow,
         const SPK_VA::valarray<double>& indParUp,
         const SPK_VA::valarray<double>& indParStep,
         enum PopCovForm                 formulation,
         SPK_VA::valarray<double>*       popParCovOut,
         SPK_VA::valarray<double>*       popParSEOut,                          
         SPK_VA::valarray<double>*       popParCorOut,
         SPK_VA::valarray<double>*       popParCVOut,                          
         SPK_VA::valarray<double>*       popParCIOut
               );

#endif
