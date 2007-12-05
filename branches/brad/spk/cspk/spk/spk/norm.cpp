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
 * File: norm.cpp
 *
 *
 * Returns the norm of a matrix.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: norm
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin norm$$
$spell 
inx cout pd max endl dmat int const iostream namespace std
$$

$section Norm of a matrix$$

$index norm$$
$index matrix, norm$$

$table
$bold Prototype:$$   $cend  
$syntax/double norm(const DoubleMatrix &/A/)/$$
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
Returns the norm of a (double) matrix.  The norm is the square
root of the sum of the squares of its elements.

$head Arguments$$

$syntax/

&/A/
/$$
$math%m by n%$$ arbitrary matrix.

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include "norm.h"

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

        cout << "norm =  " << norm(dmatA) << endl;
    }

$$
then it will display the following when it is run:
$codep

    norm = 5.2915

$$
$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <iostream>
#include <cmath>
#include "norm.h"
#include "DoubleMatrix.h"

// Updated 2-5-01 Alyssa
// fixed for const correctness

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

double norm(const DoubleMatrix &dmatA)
{
    using namespace std;

    const int m = dmatA.nr();
    const int n = dmatA.nc();
    const double *pdA = dmatA.data();
    double ans  = 0;

    for( int i=0; i<m*n; i++ ){
        ans += pdA[i]*pdA[i];
    }
    return sqrt(ans);
}

