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
 * File: quasiNewtonAnyBox.h
 *
 *
 * Minimizes an arbitrary smooth function subject to simple bounds on
 * the variables using a quasi-Newton method. 
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef QUASINEWTONANYBOX_H
#define QUASINEWTONANYBOX_H

#include "DoubleMatrix.h"
#include "Optimizer.h"

class QuasiNewtonAnyBoxObj
{
public:
  virtual void function( const DoubleMatrix& dvecXIn, double* pdFOut ) = 0;
  virtual void gradient( DoubleMatrix* pdrowF_xOut ) const = 0;
};

void quasiNewtonAnyBox( 
  QuasiNewtonAnyBoxObj&  objective,
  Optimizer&             optInfo,
  const DoubleMatrix&    dvecXLow,
  const DoubleMatrix&    dvecXUp,
  const DoubleMatrix&    dvecXIn,
  DoubleMatrix*          pdvecXOut,
  double*                pdFOut,
  DoubleMatrix*          pdrowF_xOut );

#endif
