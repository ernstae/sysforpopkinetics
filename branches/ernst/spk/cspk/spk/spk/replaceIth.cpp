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
 * File: replaceIth.cpp
 *
 *
 * Replace the i-th row of a given matrix with a row vector.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Function: replaceIth
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin replaceIth$$
$spell
    int const rowvec Jth iostream cout endl ith Ith
$$

$section Replace a Row of Matrix with a Row Vector$$

$index replaceIth$$
$index replace, row$$

$table
$bold Prototype:$$   $cend  
$syntax/void replaceIth( DoubleMatrix &/target/, int /ith/, const DoubleMatrix &/rowvec/ )
/$$
$tend

See also: $xref/replaceJth//replaceJth()/$$

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Replaces the $italic ith$$ row of $italic target$$ with the data in $italic rowvec$$.


$head Arguments$$

$syntax/

/target/
/$$
is a m by n matrix, whose $italic ith$$ row will be replaced.

$syntax/

/ith/
/$$
indicates the row that is to be replaced.  $italic ith$$ must be greater than
or equal to 0 and less than m.

$syntax/

/rowvec/
/$$
is a n dimensional row vector of values that will replace the $italic ith$$ row of $italic target$$.

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include "DoubleMatrix.h"
    #include "replaceIth.h"

    void main()
    {
        const int m = 3;
        const int n = 2;

        //
        // initialize target to a m by n matrix filled with 0.0.
        DoubleMatrix target(m,n);
        target.fill(0.0);

        DoubleMatrix rowvec(1,n);
        target.fill(1.0);

        cout << "Before = " << endl;
        target.print();

        replaceIth(target, 1, rowvec);

        cout << "After = " << endl;
        target.print();
    }
$$
then it will display the following when it is run:
$codep

    Before = 
    [ 0 0 ]
    [ 0 0 ]
    [ 0 0 ]

    After =
    [ 0 0 ]
    [ 1 1 ]
    [ 0 0 ]
$$
$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include "replaceIth.h"
#include "DoubleMatrix.h"
/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void replaceIth( DoubleMatrix &target, int ith, const DoubleMatrix &data )
{
    assert(target.nc() == data.nc());
    assert( ith >= 0 && ith < target.nr());

    double *pTarget = target.data();
    const double *pData   = data.data();
    int j;

    for( j=0; j<target.nc(); j++ )
    {
        pTarget[j*target.nr()+ith] = pData[j];
    }
    return;
}


