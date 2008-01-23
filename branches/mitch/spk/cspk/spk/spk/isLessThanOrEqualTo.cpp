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
 * File: isLessThanOrEqualTo.cpp
 *
 *
 * Check if a(i,j) in A <= b.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: isLessThanOrEqualTo (operator<=)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin isLessThanOrEqualTo$$
$spell bool int dmat pd iostream namespace std const cout endl

$$

$section Element-wise comparison of a matrix to another matrix or a scalar$$

$index matrix, <=$$
$index isLessThanOrEqualTo$$

$table
$bold Prototype:$$   $cend  
$syntax/DoubleMatrix operator<=(const DoubleMatrix &/A/, const DoubleMatrix &/B/)/$$

$bold Prototype:$$   $cend  
$syntax/DoubleMatrix operator<=(const DoubleMatrix &/A/, const D_TYPE /scalar/)/$$
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
$math%A >= B%$$ returns another $math%m by n%$$ matrix $italic C$$
such that c(i,j) in C = 1.0 if a(i,j)<=b(i,j) or
c(i,j) = 0.0 otherwise.
$pre

$$
If the second argument is a double-precision scalar,
$math%A >= B%$$ returns another $math%m by n%$$ matrix $italic C$$
such that c(i,j) in C = 1.0 if a(i,j)<=scalar or
c(i,j) = 0.0 otherwise.


$head Arguments$$

$syntax/

&/A/
/$$
is an arbitrary matrix whose elements are checked against the specified 
scalar value.

$syntax/

/scalar/
/$$
is a int/float/double precision value to which every element of $italic A$$
is check against.

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include "DoubleMatrix.h"
    #include "isLessThanOrEqualTo.h"
    #include "allTrue.h"

    void main(){
        DoubleMatrix A(2,3);
        double *pdA = A.data();

        //
        // Set A to a matrix:
        //  [ 0.1  0.0  0.0 ]
        //  [ 0.0  0.0  0.0 ]
        //  [ 0.0  0.0  0.0 ]
        //
        A.fill(0.0);
        pdA[0] = 0.1;

        // allTrue(B) returns true if all elements of B are true.
        if( allTrue( A <= 0.0 ) )
            cout << "Yes, A <= 0.0." << endl;
        else
            cout << "No, not all elements in A are less than or equal to 0.0." << endl;
    }
$$
then it will display the following when it is run:
$codep

    No, not all elements in A are less than or equal to 0.0.

$$
$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <float.h>
#include <cassert>
#include "isLessThanOrEqualTo.h"
#include "DoubleMatrix.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
DoubleMatrix operator<=(const DoubleMatrix &dmatA, const DoubleMatrix &dmatB){
    assert( dmatA.nr() == dmatB.nr() );
    assert( dmatA.nc() == dmatB.nc() );

    int m = dmatA.nr();
    int n = dmatA.nc();
    DoubleMatrix dmatC(m,n);
    const double *pdA = dmatA.data();
    const double *pdB = dmatB.data();
    double *pdC = dmatC.data();
    dmatC.fill(0.0);

    for(int i=0; i<m*n; i++){
        if( pdA[i] <= pdB[i] )
            pdC[i] = 1.0;
    }
    return dmatC;
}
DoubleMatrix operator<=(const DoubleMatrix &dmatA, const double scalar)
{
    int m = dmatA.nr();
    int n = dmatA.nc();
    DoubleMatrix dmatC(m,n);
    const double *pdA = dmatA.data();
    double *pdC = dmatC.data();
    dmatC.fill(0.0);

    for(int i=0; i<m*n; i++)
    {
        if( pdA[i] <= scalar )
            pdC[i] = 1.0;
    }
    return dmatC;
}
