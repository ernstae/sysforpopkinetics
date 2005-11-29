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
 * File: getMulRows.cpp
 *
 *
 * Returns the specified rows of a matrix.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: getMulRows
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin getMulRows$$
$spell int pd iostream namespace std cout endl Mul const th
$$

$section Get multiple rows as a matrix$$

$index getMulRows$$
$index matrix, multiple rows copy$$

$table
$bold Prototype:$$   $cend  
$syntax/DoubleMatrix getMulRows(
    const DoubleMatrix &/A/,
    const DoubleMatrix &/S/)
    /$$
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
Returns a separate matrix that contains only these rows of a $math%m by n%$$ matrix 
$italic A$$ that are selected by a selection column vector $italic S$$.
A given row in $italic A$$ is selected if the corresponding value in
$italic S$$ is $math%true%$$ (positive).
The number of rows in the result is the number of $math%true%$$ values
in $italic S$$.
If $italic A$$ is empty (i.e. if $math%m=0%$$ or $math%n=0%$$),
or if all values in $italic S$$ are false (zero or negative), then
the result is an empty matrix ($math%m=0%$$ and $math%n=0%$$).
$pre

$$
The order of resulting matrix preserves the original matrix's order.
$pre

$$
The number of rows in $italic A$$ and $italic S$$ must match, 
and $italic S$$ must have one column.
Otherwise, the program terminates.

$head Arguments$$
$syntax/

&/A/
/$$
is a $math%m by n%$$ matrix whose rows are to be selected.  
The collection of these selected rows is returned as 
a matrix.

$syntax/

&/S/
/$$
is a m by 1 column vector that contains true
(> 0.0) and false (<= 0.0) values.  If s(i) = true,
then the i-th row of $italic A$$ is said to be chosen and the row
will be found in the resulting matrix.
If s(i) = false, then the i-th row of $italic A$$ will be ignored.

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include "getMulRows.h"
    #include "DoubleMatrix.h"

    void main(){
        using namespace std;

        int m = 3;
        int n = 3; 
        int i,j;
        DoubleMatrix A(m,n);
        DoubleMatrix S(m,1);
        double *pdA = A.data();
        double *pdS = S.data();

        // Set A to a matrix:
        //  [ 0  3  6 ]
        //  [ 1  4  7 ]
        //  [ 2  5  8 ]
        //
        // Set S to a vector:
        //  [ true  ] = [ 1 ]
        //  [ false ]   [ 0 ]
        //  [ true  ]   [ 1 ]
        //
        for( i=0; i<m; i++ ){
            for( j=0; j<n; j++ ){
                pdA[i+j*m] = i+j*m;
            }
            pdS[i] = (double)(!(i % 2));
        }

        cout << "A = " << endl;
        A.print();

        cout << "\nS = " << endl;
        S.print();

        cout << "\nResult = " << endl;
        (getMulRows( A, S )).print();
    }

$$
then it will display the following when it is run:
$codep
    A =
    [0, 3, 6]
    [1, 4, 7]
    [2, 5, 8]

    S =
    [1]
    [0]
    [1]

    Result =
    [0, 3, 6]
    [2, 5, 8]

$$
$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <cassert>
#include "DBL_EPS_EQUAL_MULT.h"
#include "getMulRows.h"
#include "getMulCols.h"
#include "transpose.h"
#include "DoubleMatrix.h"

static DoubleMatrix dmatAtrans(__FILE__);
static DoubleMatrix dvecStrans(__FILE__);
/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
DoubleMatrix getMulRows(
    const DoubleMatrix &dmatA,
    const DoubleMatrix &dvecS
    )
{
    assert( dmatA.nr() == dvecS.nr() );
    assert( dvecS.nc() == 1 );
    transpose(dmatA, dmatAtrans);
    transpose(dvecS, dvecStrans);

    return transpose( getMulCols(dmatAtrans, dvecStrans) );
}

