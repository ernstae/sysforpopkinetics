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
/*
 * getRow.cpp
 * 
 * 
 */
/*
-----------------------------------------------------------
   Function specification
-----------------------------------------------------------
$begin getRow$$

$spell const dmat cout endl int pd th namespace iostream jth const mat
std nr nc dvec nd druglab spk getRow cpp sub ith$$
 
$section Get a single row as a row vector$$
$index matrix, single row copy$$
$index getRow$$

$table
$bold Prototype:$$ $cend
$syntax/DoubleMatrix getRow(
    const DoubleMatrix &/A/, const int /i/)/$$
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
Return a specified row of a given $math%m by n%$$ matrix as a
separate n dimensional row vector.  The row index must be greater than or equal
to 0 and less than m.  If the index is out of range, 
the program terminates.

$head Arguments$$
$syntax/

&/A/
/$$
is a $math%m by n%$$ arbitrary matrix.

$syntax/

/i/
/$$
is the row index that must be greater than or equal to 0 and
less than m.


$head Example$$
If you compile, link and run the following program, 
$codep
    #include <iostream>
    #include "DoubleMatrix.h"
    #include "getRow.h"

    main(){
        using namespace std;

        int m = 2, n = 3;
        DoubleMatrix dmatA(m,n);
        DoubleMatrix dmatSub;
        double *pdA = dmatA.data();
        int ith;

        // Set a matrix to:
        //
        // [0 2 4]
        // [1 3 5]
        //
        for( int i=0; i<m*n; i++ )
            pdA[i] = i;

        cout << "Original matrix: " << endl;
        dmatA.print();

        ith = 0;
        cout << endl;
        cout << ith << "-th row is: " << endl;
        dmatSub = getRow(dmatA, ith);
        dmatSub.print();
    }

$$
the following results are displayed.

$codep
    Original matrix:
    [0, 2, 4]
    [1, 3, 5]

    1-th row is:
    [0, 2, 4]
$$
$end

-----------------------------------------------------------
   Function implementation
-----------------------------------------------------------
*/
#pragma warning( disable : 4786 )

#include "getRow.h"
#include "getCol.h"
#include "transpose.h"
#include "DoubleMatrix.h"

static DoubleMatrix dmatAtrans(__FILE__);

DoubleMatrix getRow( const DoubleMatrix &dmatA, const int ith )
{
    transpose(dmatA, dmatAtrans);
    return transpose(getCol(dmatAtrans, ith));

}
