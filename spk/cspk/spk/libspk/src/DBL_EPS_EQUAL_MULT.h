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
 * DBL_EPS_EQUAL_MULT.h
 *
 *
 * Defines the multiple used by the function isDblEpsEqual
 * in order to determine the epsilon equality of two double 
 * precision numbers.
 *
 * Author: Mitch Watrous
 */
/*
$begin DBL_EPS_EQUAL_MULT$$
$spell 
   const
   namespace
   std
   cout
   endl
   bool
   iostream
$$

$section Constant Multiplier for Double Epsilon$$

$index DBL_EPS_EQUAL_MULT$$
$cindex Multiplier \of epsilon$$

$table
$bold Header:$$   $cend 
DBL_EPS_EQUAL_MULT.h $rend
$bold Definition:$$ $cend
$syntax/const double DBL_EPS_EQUAL_MULT = 10.0/$$ $rend
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
DBL_EPS_EQUAL_MULT is a constant double-precision value defined as a standard multiplier for double epsilon.

$head Example$$
If you compile, link, and run the following program:
$codep

   #include <iostream>
   #include "DBL_EPS_EQUAL_MULT.h"


   bool testDBL_EPS_EQUAL_MULT()
   {
      using namespace std;
      cout << "DBL_EPS_EQUAL_MULT = " << DBL_EPS_EQUAL_MULT << endl;
      return true;
   }

$$
then it will display the following when it is run:
$codep

DBL_EPS_EQUAL_MULT = 10.0

$$
$end
*/

#ifndef DBL_EPS_EQUAL_MULT_H
#define DBL_EPS_EQUAL_MULT_H

const double DBL_EPS_EQUAL_MULT = 10.0;

#endif
