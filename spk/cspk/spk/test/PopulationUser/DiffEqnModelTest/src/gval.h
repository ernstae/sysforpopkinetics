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
 * File: gval.h
 *
 *
 * Evaluates g(a, b) = (exp(a) - exp(b)) / (a - b) for each element pair
 * in the input matrices A and B.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef GVAL_H
#define GVAL_H

#include <spk/DoubleMatrix.h>
#include <spk/SpkValarray.h>

void gval( const DoubleMatrix&    dmatA, 
           const DoubleMatrix&    dmatB, 
           const DoubleMatrix&    dmatDA, 
           const DoubleMatrix&    dmatDB, 
           DoubleMatrix*          pdmatGOut, 
           DoubleMatrix*          pdmatDGOut ); 

void gval( const SPK_VA::valarray<double>&    A, 
           const SPK_VA::valarray<double>&    B, 
           const SPK_VA::valarray<double>&    A_x, 
           const SPK_VA::valarray<double>&    B_x, 
           SPK_VA::valarray<double>&          GOut, 
           SPK_VA::valarray<double>&          G_xOut ); 
#endif
