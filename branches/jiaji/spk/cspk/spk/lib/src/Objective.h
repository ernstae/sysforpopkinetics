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
$begin Objective$$
$spell 
   laplace
   hessian
   const
   std
   iostream
   cout
   endl
   enum
   namespace
$$

$section Symbols for Different Objectives$$

$index Objective$$
$index objective, population analysis$$

$table
$bold Header:$$  
$cend
Objective.h 
$rend
$bold Defined:$$ 
$cend
$syntax/enum Objective/$$ 
$rend
$tend

$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code Objective$$ enumerates symbols for different objectives:
$syntax/

MODIFIED_LAPLACE
/$$
for the Modified Laplace objective,
$syntax/

EXPECTED_HESSIAN
/$$
for the Modified Expected Hessian (also known as FOCE) objective, and 
$syntax/

FIRST_ORDER
/$$
for the Modified First Order objective. 

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Example$$
If you compile and link the following program,

  $codep
    #include <iostream>
    #include "Objective.h"

    int main()
    {
      using namespace std;

      Objective ob = MODIFIED_LAPLACE;

      switch(ob)
      {
        case MODIFIED_LAPLACE:
          cout << "modified laplace" << endl;
          break;
        case EXPECTED_HESSIAN:
          cout << "expected hessian" << endl;
          break;
        case FIRST_ORDER:
          cout << "first order" << endl;
          break;
        default:
          break;
      }
      return 0;
    }
  $$

it will display;

  $codep
    modified laplace 
  $$
$end
*/

#ifndef OBJECTIVE_H
#define OBJECTIVE_H

enum Objective 
{
    MODIFIED_LAPLACE,
	EXPECTED_HESSIAN,
	FIRST_ORDER,
    NAIVE_FIRST_ORDER
};
	          
#endif
