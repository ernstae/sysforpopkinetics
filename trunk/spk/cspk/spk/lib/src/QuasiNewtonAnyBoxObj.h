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
 * File: QuasiNewtonAnyBoxObj.h
 *
 *
 * Abstract base class for the objective function for quasiNewtonAnyBox.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef QUASINEWTONANYBOXOBJ_H
#define QUASINEWTONANYBOXOBJ_H

#include "DoubleMatrix.h"

class QuasiNewtonAnyBoxObj
{
public:
  // There are no default versions for these functions.
  virtual void function( const DoubleMatrix& dvecXIn, double* pdFOut ) = 0;
  virtual void gradient( DoubleMatrix* pdrowF_xOut ) const = 0;

  // If the concrete objective function derived from this base
  // requires any information for a restart, then reimplement these
  // functions to read/write that information from/to a file.
  virtual void readRestartInfoFromFile() const {}
  virtual void writeRestartInfoToFile() const {}
};


#endif
