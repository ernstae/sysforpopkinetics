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
 * File: replaceSubblock.cpp
 *
 *
 * Replaces a sub-block of a matrix by another matrix.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: replaceSubblock <DoubleMatrix version>
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin replaceSubblockDoubleMatrix$$
$spell 
    const
	dmat
	pdmat
	subblock
	valarray
$$

$section Replace a sub-block of a matrix by another matrix [DoubleMatrix]$$

$index replaceSubblockDoubleMatrix$$
$index matrix, sub-block --- DoubleMatrix version$$

$table
$bold Prototype:$$   $cend  
$syntax/void replaceSubblock(
        DoubleMatrix &/dmatA/, 
        const DoubleMatrix &/dmatB/, 
        const int startRow, 
        const int startCol
        )/$$
$tend

See also: $xref/replaceSubblockValarray//valarray version of replaceSubblock()/$$

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$

Replaces a sub-block of a matrix A by another matrix B.

$head Arguments$$
$syntax/

& /A/
/$$
$math%mA by nA%$$ arbitrary matrix.

$syntax/

& /B/
/$$
$math%mB by nB%$$ matrix ($math%mB \le mA%$$ and $math%nB \le nA%$$).

$syntax/

/startRow/
/$$
specifies the starting row index ($math%0 \le startRow \le (mA - mB)%$$).

$syntax/

/startCol/
/$$
specifies the starting column index ($math%0 \le startCol \le (nA - nB)%$$).

$head Example$$
If you compile, link, and run the following program:
$codep

    #include "replaceSubblock.h"

    void main()
    {
        DoubleMatrix dmatA(3,3);
        DoubleMatrix dmatB(2,2);
        double *pdmatA = dmatA.data();
        double *pdmatB = dmatB.data();

        // set the matrix A to:
        // 
        // [0, 3, 6]
        // [1, 4, 7]
        // [2, 5, 8]
        //
        for( int i=0; i<3*3; i++ )
            pdmatA[i] = i;

        // set the matrix B to:
        // 
        // [4, 2]
        // [3, 1]
        //
        for( i=0; i<2*2; i++ )
            pdmatB[i] = 4 - i;

        replaceSubblock(dmatA, 0, 0, dmatB);
        dmatA.print();
    }

$$
then it will display the following when it is run:
$codep

      [4, 2, 6]
      [3, 1, 7]
      [2, 5, 8]

$$
$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include "replaceSubblock.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void replaceSubblock(
                     DoubleMatrix &dmatA,
					 const DoubleMatrix &dmatB,
                     const int startRow,
                     const int startCol                     
                    )
{
    const int mA = dmatA.nr();
    const int nA = dmatA.nc();
	const int mB = dmatB.nr();
    const int nB = dmatB.nc();

    assert( startRow >= 0 );
    assert( startCol >= 0 );
    assert( startRow + mB <= mA );
    assert( startCol + nB <= nA );

    double *pdA = dmatA.data();
    const double *pdB = dmatB.data();

	int i, j;
    for( i = startRow; i < startRow + mB; i++ )
	    for( j = startCol; j < startCol + nB; j++ )
			pdA[ i + mA * j ] = pdB[ i - startRow + mB * ( j - startCol ) ];
}
/*************************************************************************
 *
 * Function: replaceSubblock <valarray version>
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin replaceSubblockValarray$$
$spell 
    const
	subblock
	valarray
	Cols
	str
	Spk
$$

$section Replace a sub-block of a matrix by another matrix [valarray]$$

$index replaceSubblockValarray$$
$index matrix, sub-block -- valarray version$$

$table
$bold Prototype:$$   $cend  
$syntax/void replaceSubblock( 
					 valarray<double> &/a/, 
					 int nACols, 
					 valarray<double> &/b/, 
					 int nBCols, 
					 int strRow, 
					 int strCol 
					 )
/$$
$tend

See also: $xref/replaceSubblockDoubleMatrix//DoubleMatrix version of replaceSubblock()/$$
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$

Replaces a sub-block of a matrix A stored as a vector by another matrix B stored as a vector.

$head Arguments$$
$syntax/

& /A/
/$$
$math%mA by nA%$$ arbitrary matrix.

$syntax/

& /B/
/$$
$math%mB by nB%$$ matrix ($math%mB \le mA%$$ and $math%nB \le nA%$$).

$syntax/

/startRow/
/$$
specifies the starting row index ($math%0 \le startRow \le (mA - mB)%$$).

$syntax/

/startCol/
/$$
specifies the starting column index ($math%0 \le startCol \le (nA - nB)%$$).

$head Example$$
If you compile, link, and run the following program:
$codep

    #include "SpkValarray.h"
    #include "replaceSubblock.h"

    void main()
    {
        valarray<double> a(3 * 3);
        valarray<double> b(2 * 2);

        // matrix A is:
        // 
        // [0, 3, 6]
        // [1, 4, 7]
        // [2, 5, 8]
        //
		//
		// store A in a vector:
		//
		// a = { 0, 1, 2, 3, 4, 5, 6, 7, 8 }
		//
        for( int i=0; i<3*3; i++ )
            a[i] = i;

        // matrix B is:
        // 
        // [4, 2]
        // [3, 1]
        //
		// store B in a vector:
		//
		// b = { 4, 3, 2, 1 }
		//
        for( i=0; i<2*2; i++ )
            b[i] = 4 - i;

        replaceSubblock(a, 3, b, 2, 0, 0);
        DoubleMatrix( a, 3 ).print();
    }

$$

then it will display the following when it is run:
$codep

      [4, 2, 6]
      [3, 1, 7]
      [2, 5, 8]

$$
$end
*/
using SPK_VA::valarray;
using SPK_VA::gslice;
using SPK_VA::slice;

void replaceSubblock( 
					 valarray<double> &a, 
					 int nACols, 
					 const valarray<double> &b, 
					 int nBCols, 
					 int strRow, 
					 int strCol 
					 )
{
	const int nARows = a.size() / nACols;
	const int nBRows = b.size() / nBCols;

    assert( strRow >= 0 );
    assert( strCol >= 0 );
    assert( strRow + nBRows <= nARows );
    assert( strCol + nBCols <= nACols );

    size_t start = strRow + strCol * nARows;

    // Note that C++ assumes row-major order
    size_t len[] = { nBCols, nBRows };
    size_t str[] = { nARows, 1 };
    valarray<size_t> lengths( len, 2 );
    valarray<size_t> strides( str, 2 );
    a[ gslice( start, lengths, strides ) ] = b[ slice( 0, b.size(), 1 ) ];
}
