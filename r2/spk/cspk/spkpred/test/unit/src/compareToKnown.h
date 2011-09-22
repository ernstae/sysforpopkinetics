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
 * File: compareToKnown.h
 *
 *
 * Compares an array of calculated values to an array of known values.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef COMPARETOKNOWN_H
#define COMPARETOKNOWN_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include <spk/SpkValarray.h>

// Standard library header files.
#include <string>


/*------------------------------------------------------------------------
 * Function declaration
 *------------------------------------------------------------------------*/

void compareToKnown(
  const SPK_VA::valarray<bool>&  xCalc,
  const SPK_VA::valarray<bool>&  xKnown,
  const std::string&             name );

void compareToKnown(
  const SPK_VA::valarray<double>&  xCalc,
  const SPK_VA::valarray<double>&  xKnown,
  const std::string&               name,
  double                           tol );


#endif
