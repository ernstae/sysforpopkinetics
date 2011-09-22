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
 * isDblEpsEqual.h
 *
 * Checks for epsilon equality of two double precision numbers.
 *
 * (Note that this is an inline function so that there is no 
 * source code file associated with it, i.e, there is no file
 * isDblEpsEqual.cpp.)
 *
 * Author: Mitch Watrous
 */


/*
-----------------------------------------------------------------
     Function Specification
-----------------------------------------------------------------

#begin##

$begin isDblEpsEqual$$

$spell 
  isDblEpsEqual 
  Dbl 
  Eps 
  bool 
  iostream 
  cout 
  endl
  true true
$$

$section Checking for Epsilon Equality of Two Double Precision Numbers$$

$cindex \checking \for epsilon equality \of \two double precision $$
$index  isDblEpsEqual$$

$table
$bold Prototype$$ $cend 
$syntax/bool isDblEpsEqual(double /a/, double /b/, double /scale/)/$$
$tend

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Returns true if the double precision numbers $italic a$$ and $italic b$$ 
are epsilon equivalent,otherwise returns false. Specifically, 
$code isDblEpsEqual$$ returns true if
$math%

  |a - b| \le scale * ( \DBL_\EPS_\EQUAL_\MULT * \DBL_\EPSILON ) .

%$$

Note that the constant $code DBL_EPSILON$$ is the smallest positive double 
precision number x, such that x + 1.0 is not equal to 1.0. 
It is defined in the standard include file $code float.h$$.  The constant
$code DBL_EPS_EQUAL_MULT$$ is defined in the file $code DBL_EPS_EQUAL_MULT.h$$.

$head Example$$
If you compile and link the C++ program,
$codep

#include <iostream.h>
#include <float.h>
#include "isDblEpsEqual.h"
#include "DBL_EPS_EQUAL_MULT.h"

void main()
{
  double a = 1.0;
  double b = 1.0;

  double scale = a;

  // Addition of this value should not change the epsilon  
  // equality of two double precision numbers.
  double c = 0.5 * scale * DBL_EPS_EQUAL_MULT * DBL_EPSILON; 

  cout << (isDblEpsEqual(a, b,         scale) ? "true" : "false") << endl;
  cout << (isDblEpsEqual(a, b + c,     scale) ? "true" : "false") << endl;
  cout << (isDblEpsEqual(a, b + 3.0*c, scale) ? "true" : "false") << endl;
}

$$
then it should output the following when it is run:
$codep

true
true
false
$$
$end
-----------------------------------------------------------------
     Function Implementation
-----------------------------------------------------------------
*/

#ifndef ISDBLEPSEQUAL_H
#define ISDBLEPSEQUAL_H

#include <cmath>
#include <cfloat>
#include "DBL_EPS_EQUAL_MULT.h"

inline bool isDblEpsEqual(double a, double b, double scale)
{
    if ( fabs(a - b) <= scale * ( DBL_EPS_EQUAL_MULT * DBL_EPSILON ) )
       return true;
    else 
       return false;
}

#endif
