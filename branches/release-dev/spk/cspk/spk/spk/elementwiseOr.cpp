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
 * File: elementwiseOr.cpp
 *
 *
 * logic OR for DoubleMatrix objects.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: logic OR
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin elementwiseOr$$
$spell dmat bool int namespace std pd iostream const elementwise endl cou 
$$

$section Element-wise logical OR$$

$index or$$
$index elementwiseOr$$
$index matrix, OR$$
$index logical operation, OR$$

$table
$bold Prototype:$$   $cend  
$syntax/DoubleMatrix or(const DoubleMatrix &/A/, const DoubleMatrix &/B/)/$$
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
Given $math%m by n%$$ matrices $italic A$$ and $italic B$$,
Returns a $math%m by n%$$ matrix $italic C$$ such that
c(i,j) = 1.0 if either a(i,j) or b(i,j) is 1.0 exactly; otherwise c(i,j) = 0.0,
where i and j are the row index and the column index, respectively.
$pre

$$
If the dimensions do not match, the program terminates.

$head Arguments$$

$syntax/

&/A/
/$$
is a $math%m by n%$$ matrix.

$syntax/

&/B/
/$$
is another $math%m by n%$$ matrix.

$head Example$$
If you compile, link, and run the following program:
$codep

  #include "DoubleMatrix.h"
  #include "elementwiseAnd.h"

  void main(){
    
    // Two matrices must have the same dimensions  
    DoubleMatrix dmatA(3,3);
    DoubleMatrix dmatB(3,3);

    //
    // Initialize A to a matrix:
    //    [ 0  0  0 ]
    //    [ 0  0  0 ]
    //    [ 0  0  0 ]
    //
    dmatA.fill(0);

    //
    // and B to a matrix:
    //    [ 1  0  0 ]
    //    [ 0  1  0 ]
    //    [ 0  0  1 ]
    //
    dmatB = identity(3);

    // Take || and print the resulting matrix
    // Note that any value greater than 0 is considered TRUE
    or(dmatB, dmatA).print();

$$
then it will display the following when it is run:
$codep

	[ 1 0 0 ]
    [ 0 1 0 ]
    [ 0 0 1 ]

$$
$end
*/

// Updated 2-5-01 Alyssa
// fixed for const correctness 


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * Logic (shortcut) operators || and && should not be overloaded.
 * VC++ compilar has a bug that failes calling destructor properly
 * when the operators were used in assert() statements.
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <cassert>
#include <cfloat>
#include "elementwiseOr.h"
#include "DBL_EPS_EQUAL_MULT.h"
#include "DoubleMatrix.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
DoubleMatrix bbor(const DoubleMatrix &dmatA, const DoubleMatrix &dmatB)
{
    assert(dmatA.nr() == dmatB.nr());
    assert(dmatA.nc() == dmatB.nc());
    int m = dmatA.nr();
    int n = dmatA.nc();

    DoubleMatrix  dmatC(m,n);
    const double *pdA = dmatA.data();
    const double *pdB = dmatB.data();
    double *pdC = dmatC.data();

    for( int i=0; i<m*n; i++ ){

        pdC[i] = ((pdA[i]==1.0 || pdB[i]==1.0)? 1.0 : 0.0);
    }
    return dmatC;
}

