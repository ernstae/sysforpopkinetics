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
 * File: inxToMax.cpp
 *
 *
 * Returns the index to the element that has the largest value in a matrix.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: inxToMax
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin inxToMax$$
$spell 
inx cout pd max endl dmat int const iostream namespace std
$$

$section Find index to the maximum value$$

$index inxToMax$$
$index matrix, maximum value$$

$table
$bold Prototype:$$   $cend  
$syntax/int inxToMax(const DoubleMatrix &/A/)/$$
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
Returns the index to the element that has the largest value in a $math%m by n%$$ matrix.
The returned index points to the element whose position is computed by 
$math%i+j*m, for 0 \le i < m and 0 \le j < n%$$. The routine
scans the matrix in the column order and
returns the index to the element it finds first.

$head Arguments$$

$syntax/

&/A/
/$$
$math%m by n%$$ arbitrary matrix .

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include "inxToMax.h"

    void main()
    {
        using namespace std;

        DoubleMatrix dmatA(3,3);
        double *pdA = dmatA.data();

        // set the matrix to:
        // 
        // [0, 3, 2]
        // [1, 0, 3]
        // [2, 1, 0]
        //
        for( int i=0; i<3*3; i++ )
            pdA[i] = i%4;

        cout << inxToMax(dmatA) << endl;

    }

$$
then it will display the following when it is run:
$codep

    Index to max: 3

$$
$end
*/

// Updated 2-5-01 Alyssa
// fixed for const correctness

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include "inxToMax.h"
#include "DoubleMatrix.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

int inxToMax(const DoubleMatrix &dmatA)
{

    const int m = dmatA.nr();
    const int n = dmatA.nc();
    int     inx_max = 0;
    const double *pdA = dmatA.data();
    double  maxSoFar = pdA[0];

    for( int i=1; i<m*n; i++ ){
        if( pdA[i] > maxSoFar ){
            maxSoFar = pdA[i];
            inx_max  = i;
        }
    }
    return inx_max;
}

