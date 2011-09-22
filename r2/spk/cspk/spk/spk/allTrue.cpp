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
 * File: allTrue.cpp
 *
 *
 * Returns true if every single element in the matrix is non zero.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: allTrue
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin allTrue$$
$spell 
inx cout pd max endl dmat int const iostream namespace std bool
$$

$section Test if all elements are true$$

$index allTrue$$
$index matrix, all true?$$
$index logical operation, all true?$$

$table
$bold Prototype:$$   $cend  
$syntax/bool allTrue(const DoubleMatrix &/A/)/$$
$tend

See also: $xref/allZero//allZero/$$

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Returns true if all elements in the given matrix are exactly 1.0.

$head Arguments$$

$syntax/

&/A/
/$$
is a $math%m by n%$$ arbitrary matrix.

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include "allTrue.h"

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

        cout << (allTrue(dmatA)? "true" : "false") << endl;
    }

$$
then it will display the following when it is run:
$codep

    false

$$
$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include "DoubleMatrix.h"
#include "allTrue.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

bool allTrue(const DoubleMatrix &dmatA)
{

    const int m = dmatA.nr();
    const int n = dmatA.nc();
    const double *pdA = dmatA.data();

    for( int i=0; i<m*n; i++ ){
        if( pdA[i] != 1.0 )
        {
            return false;
        }
    }
    return true;
}

