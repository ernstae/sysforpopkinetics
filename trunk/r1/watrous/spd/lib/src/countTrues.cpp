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
 * File: countTrues.cpp
 *
 *
 * count the number of values that are true (> 0) in a matrix.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/


/*************************************************************************
 *
 * Function: countTrues
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin countTrues$$
$spell 
$$

$section Count Trues$$

$index countTrues$$
$cindex matrix, count trues$$

$table
$bold Prototype:$$   $cend  
$syntax/int countTrues(const DoubleMatrix& A)/$$
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
Returns the number of trues in a matrix.  A value greater than 0.0 is
considered true.


$head Arguments$$

$syntax/

/A/
/$$
is a m by n matrix containing true (>0) and/or false (=0) values,
where m and n are greater than or equal to zero.

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include "DoubleMatrix.h"

    void main()
    {
        using namespace std;

        int m = 3;
        int n = 2;

        //
        // Set a matrix that contains 2 trues.
        //
        DoubleMatrix A(m,n);
        double* pA = A.data();
        pA[0] = 0;
        pA[1] = 1;
        pA[2] = 0;
        pA[3] = 0;
        pA[4] = 0;
        pA[5] = 1;

        cout << "#of trues in A: " << countTrues(A) << endl;
    }
$$
then it will display the following when it is run:
$codep

    #of trues in A: 2

$$
$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include "DoubleMatrix.h"
#include "countTrues.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

int countTrues(const DoubleMatrix& A)
{
    int m = A.nr();
    int n = A.nc();
    int sum = 0;
    const double* pA = A.data();
    for(int i=0; i<m*n; i++)
        if( pA[i] > 0.0 ) sum++;

    return sum;

}

