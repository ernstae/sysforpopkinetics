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
 * File: fitIndividual.h
 *
 *
 * This header file includes other headers and declares components
 * necessary for the user to do the individual level analysis.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

#ifndef FITINDIVIDUAL_H
#define FITINDIVIDUAL_H

//
// Necessary component headers for fitPopulation() declaration.
//
#include "SpkValarray.h"
#include "Optimizer.h"
#include "SpkModel.h"

void fitIndividual( 
				    SpkModel<double>&                       indModel,
                    const SPK_VA::valarray<double>&     measurements,
                    Optimizer&                          indOptimizer,
                    const SPK_VA::valarray<double>&     indParLow,
                    const SPK_VA::valarray<double>&     indParUp,
					const SPK_VA::valarray<double>&     indParIn,
					const SPK_VA::valarray<double>&     indParStep,
                    SPK_VA::valarray<double>*           indParOut,
					double*                             indObjOut,
					SPK_VA::valarray<double>*           indObj_indParOut,
					SPK_VA::valarray<double>*           indObj_indPar_indParOut,
					bool                                withD = true 
				  );
//
// Headers that are not necessary for fitPopulation() but
// needed for the user to do the population analysis.
// 
#include "SpkException.h"

#endif
