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
 * File: getSubblock.cpp
 *
 *
 * Returns a sub-block of a given matrix
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: getSubblock
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin getSubblock$$
$spell 
inx cout pd max endl dmat int const iostream namespace std sub 
subblock cols
$$

$section Get a sub-block as a matrix$$

$index getSubblock$$
$index matrix, sub-block$$

$table
$bold Prototype:$$   $cend  
$syntax/DoubleMatrix getSubblock(
    const DoubleMatrix &/A/, 
    const int startRow, 
    const int startCol,
    const int rows,
    const int cols
    )/$$
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
Returns a sub-block of a matrix as a separate copy.
If any of the arguments exceed the matrix size,
the program terminates.

$head Arguments$$

$syntax/

& /A/
/$$
$math%m by n%$$ arbitrary matrix.

$syntax/

/startRow/
/$$
specifies the starting row index ($math%0 \le startRow < m%$$).

$syntax/

/startCol/
/$$
specifies the starting column index 
($math%0 \le startCol < n%$$).

$syntax/

/rows/
/$$
specifies the number of rows, counting from $italic startRow$$.
$math%0 \le (startRow + rows) \le m%$$.

$syntax/

/cols/
/$$
specifies the number of columns, counting from $italic startCol$$.
$math%0 \le (startCol + cols) \le n%$$.


$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include "getSubblock.h"

    void main()
    {
        using namespace std;

        DoubleMatrix dmatA(3,3);
        DoubleMatrix dmatSub;
        double *pdA = dmatA.data();

        // set the matrix to:
        // 
        // [0, 3, 6]
        // [1, 4, 7]
        // [2, 5, 8]
        //
        for( int i=0; i<3*3; i++ )
            pdA[i] = i;

        dmatSub = getSubblock(dmatA, 0, 0, 2, 2);
        dmatSub.print();
    }

$$
then it will display the following when it is run:
$codep

    [0, 3]
    [1, 4]

$$
$end
*/

// Updated 2-5-01 Alyssa
// fixed for const correctness

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <iostream>
#include "getSubblock.h"
#include "DoubleMatrix.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

const DoubleMatrix getSubblock(
    const DoubleMatrix &dmatA,
    const int startRow,
    const int startCol,
    const int rows,
    const int cols
    )
{
    DoubleMatrix dmatB(rows, cols);
    getSubblock(dmatA, startRow, startCol, rows, cols, dmatB);
    return dmatB;
}

void getSubblock(
    const DoubleMatrix &dmatA,
    const int startRow,
    const int startCol,
    const int rows,
    const int cols,
    DoubleMatrix &dmatB
    )
{
    using namespace std;

    const int m = dmatA.nr();
    const int n = dmatA.nc();
    const double *pdA = dmatA.data();

    assert( startRow >= 0 );
    assert( startCol >= 0 );
    assert( startRow < m );
    assert( startCol < n );
    assert( rows >= 0 );
    assert( cols >= 0 );
    assert( startRow + rows <= m );
    assert( startCol + cols <= n );

    dmatB.resize(rows, cols);
    double *pdB = dmatB.data();
    int i, j, tmp, colindex;

    for( j=startCol; j<startCol+cols; j++ ){
		tmp = (j-startCol)*rows;
		colindex = j*m;
        for( i=startRow; i<startRow+rows; i++ ){
            pdB[(i-startRow)+tmp]
                = pdA[i+colindex];
        }
    }
}

