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
 * File: getMulCols.cpp
 *
 *
 * Returns the specified rows of a matrix.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: getMulCols
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin getMulCols$$
$spell int pd iostream namespace std cout endl Mul const th Cols
$$

$section Get multiple cols as a matrix$$

$index getMulCols$$
$index matrix, multiple columns copy$$

$table
$bold Prototype:$$   $cend  
$syntax/DoubleMatrix getMulCols(
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
Returns a separate matrix that contains only these columns of a $math%m by n%$$ matrix 
$italic A$$ that are selected by a selection row vector $italic S$$.
A given column in $italic A$$ is selected if the corresponding value in
$italic S$$ is $math%true%$$ (positive).
The number of columns in the result is the number of $math%true%$$ values
in $italic S$$.
If $italic A$$ is empty (i.e. if $math%m=0%$$ or $math%n=0%$$),
or if all values in $italic S$$ are false (zero or negative), then
the result is an empty matrix ($math%m=0%$$ and $math%n=0%$$).
$pre

$$
The order of resulting matrix preserves the original matrix's order.
$pre

$$
The number of columns in $italic A$$ and $italic S$$ must match, 
and $italic S$$ must have one row.
Otherwise, the program terminates.

$head Arguments$$

$syntax/

&/A/
/$$
is a m by n matrix whose rows are to be selected.  
The collection of these selected columns is returned as 
a matrix.

$syntax/

&/S/
/$$
is a 1 by n row vector that contains true
(> 0.0) and false (<= 0.0) values.  If s(j) = true,
then the j-th column of $italic A$$ is said to be chosen and the column
will be found in the resulting matrix.
If s(j) = false, then the j-th row of $italic A$$ will be ignored.

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include "getMulCols.h"
    #include "DoubleMatrix.h"

    void main(){
        using namespace std;

        int m = 3;
        int n = 3; 
        int i,j;
        DoubleMatrix A(m,n);
        DoubleMatrix S(1,n);
        DoubleMatrix C;
        double *pdA = A.data();
        double *pdS = S.data();

        // Set A to a matrix:
        //  [ 0  3  6 ]
        //  [ 1  4  7 ]
        //  [ 2  5  8 ]
        //
        // Set S to a vector:
        //  [ true, false, true] = [ 1, 0, 1 ]
        //
        for( j=0; j<n; j++ ){
            for( i=0; i<m; i++ ){
                pdA[i+j*m] = i+j*m;
            }
            pdS[j] = (double)(!(j % 2));
        }

        C = getMulCols(A,S);

        cout << "A = " << endl;
        A.print();

        cout << "\nS = " << endl;
        S.print();

        // Result =
        //  [0, 6]
        //  [1, 7]
        //  [2, 8]
        cout << "\nResult = " << endl;
        C.print();
    }

$$
then it will display the following when it is run:
$codep
    A =
    [0, 3, 6]
    [1, 4, 7]
    [2, 5, 8]

    S =
    [1, 0, 1]

    Result =
    [0, 6]
    [1, 7]
    [2, 8]

$$
$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <cassert>
#include <cfloat>
#include "DBL_EPS_EQUAL_MULT.h"
#include "getMulCols.h"
#include "DoubleMatrix.h"
#include "countTrues.h"


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
DoubleMatrix getMulCols(
    const DoubleMatrix &dmatA,
    const DoubleMatrix &drowS
    )
{
    assert( dmatA.nc() == drowS.nc() );
    assert( drowS.nr() == 1 );

    DoubleMatrix dmatC;
    const int numCols = countTrues(drowS);
    if( numCols < 1 )
        return dmatC;
    dmatC.resize( dmatA.nr(), numCols );
    
    // If #cols requested is 0 then return an empty matrix.
    if( numCols == 0 ){
        return dmatC;
    }

    int j;
    const int nRows = dmatA.nr();
    const int nCols = dmatA.nc();
    double     *pdC = dmatC.data();
    const double     *pdA = dmatA.data();
    const double     *pdS = drowS.data();
    
    int k = 0;

    for( j=0; j<nCols; j++ )
    {
        if( pdS[j] > 0.0 + DBL_EPSILON*DBL_EPS_EQUAL_MULT )
        {
            std::copy(pdA+(j*nRows), pdA+(j*nRows+nRows), pdC+(k*nRows));
            k++;
        }
    }
    return dmatC;
}
