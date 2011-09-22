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
 * File: blockDiagonalDerivative.cpp
 *
 *
 * Maps derivatives of square matrices that form blocks on the diagonal of  
 * a block-diagonal matrix to the derivative of the block-diagonal matrix.
 *
 * Author: Jiaji Du
 * 
 * Documentation extended by Sachiko Honda, 09/27/2002
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: blockDiagonalDerivative
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin blockDiagonalDerivative$$
$spell 
    const 
    dmat
    pdmat
    std
    ed
    rvec
$$

$section Map derivative of block on diagonal to derivative of block-diagonal matrix$$

$index blockDiagonalDerivative$$
$index matrix, sub-block, derivative, diagonal$$

$table
$bold Prototype:$$   $cend  
$syntax/const DoubleMatrix blockDiagonalDerivative(
        std::vector<const DoubleMatrix> /dmatB/
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

Maps derivatives of square matrices that form blocks on the diagonal of  
a block-diagonal matrix to the derivative of the block-diagonal matrix.  
Returns the derivative of the block-diagonal matrix.

$head Arguments$$
$syntax/

/dmatB/
/$$
An array of DoubleMatrix objects that are the derivatives of 
the square matrices that form the blocks of the block-diagonal matrix
as demonstrated blow:
$math%

Given:
  f_x(x) = [ f_x1(x), f_x2(x) ]
       
         = [ a1_11, a2_11 ]
           [ a1_12, a2_12 ]
           [ a1_21, a2_21 ]
           [ a1_22, a2_22 ]

  where  f_x1(x) = rvec( [ a1_11  a1_12 ]
                         [ a1_21  a1_22 ] )

         f_x2(x) = rvec( [ a2_11  a2_12 ]
                         [ a2_21  a2_22 ] ).
and
 
  g_x(x) = [ g_x1(x), g_x2(x) ]
       
         = [ b1_11, b2_11 ]
           [ b1_12, b2_12 ]
           [ b1_21, b2_21 ]
           [ b1_22, b2_22 ],

  where  g_x1(x) = rvec( [ b1_11  b1_12 ]
                         [ b1_21  b1_22 ] )

         g_x2(x) = rvec( [ b2_11  b2_12 ]
                         [ b2_21  b2_22 ] ),


and
  h_x(x) = [ h_x1(x), h_x2(x) ]

         = [ c1_11, c2_11 ],

  where h_x1(x) = rvec( [ c1_11 ] )
  
        h_x2(x) = rvec( [ c2_11 ] ).
%$$

It will return a matrix in the following representation:
$math%

  F_x(x) = [ F_x1(x),  F_x2(x) ]

           /                                                                                               \
         = | rvec( [ a1_11  a1_12    0      0      0   ]     rvec( [ a2_11  a2_12    0      0      0   ]   |
           |       [ a1_12  a1_22    0      0      0   ]           [ a2_12  a2_22    0      0      0   ]   |
           |       [   0      0    b1_11  b1_12    0   ]           [   0      0    b2_11  b2_12    0   ]   |
           |       [   0      0    b1_21  b1_22    0   ]           [   0      0    b2_21  b2_22    0   ]   |
           |       [   0      0      0      0    c1_11 ] ),        [   0      0      0      0    c1_11 ] ) |
           \                                                                                               /
           
         = [ a1_11, a2_11 ]
           [ a1_12, a2_12 ]
           [   0  ,   0   ]
           [   0  ,   0   ]
           [   0  ,   0   ]
           [ a1_12, a2_12 ]
           [ a1_22, a2_22 ]
           [   0  ,   0   ]
           [   0  ,   0   ]
           [   0  ,   0   ]
           [   0  ,   0   ]
           [   0  ,   0   ]
           [ b1_11, b2_11 ]
           [ b1_12, b2_22 ]
           [   0  ,   0   ]
           [   0  ,   0   ]
           [   0  ,   0   ]
           [ b1_21, b2_21 ]
           [ b1_22, b2_22 ]
           [   0  ,   0   ]
           [   0  ,   0   ]
           [   0  ,   0   ]
           [   0  ,   0   ]
           [   0  ,   0   ]
           [ c1_11, c2_11 ]

%$$


$head Example$$
If you compile, link, and run the following program:
$codep

	#include "blockDiagonalDerivative.h"

    void main()
    {
        DoubleMatrix dmatA(4,2);
        DoubleMatrix dmatB(4,2);
        DoubleMatrix dmatC(1,2);

        double *pdmatA = dmatA.data();
        double *pdmatB = dmatB.data();
        double *pdmatC = dmatC.data();

        // set the matrix A to:
        // 
        // [0, 4]
        // [1, 5]
        // [2, 6]
        // [3, 7]
        //
        // that is an actual value of [ f_x1(x), f_x2(x) ]
        // where each partial has been rvec-ed.
        //
        //   f_x1(x) = rvec( [ 0  1 ]
        //                   [ 2  3 ] )
        //
        //   f_x2(x) = rvec( [ 4  5 ]
        //                   [ 6  7 ] )
        //
        for( int i=0; i<4*2; i++ )
            pdmatA[i] = i;

        // set the matrix B to:
        // 
        // [10, 14]
        // [11, 15]
        // [12  16]
        // [13, 17]
        //
        // that is an actual value of [ g_x1(x), g_x2(x) ].
        //
        // where each partial has been rvec-ed.
        //
        //   g_x1(x) = rvec( [ 10  11 ]
        //                   [ 12  13 ] )
        //
        //   g_x2(x) = rvec( [ 14  15 ]
        //                   [ 16  17 ] )
        //
        for( i=0; i<4*2; i++ )
            pdmatB[i] = 10 + i;

        // set the matrix C to:
        // 
        // [20, 21]
        //
        // that is an actual value of [ h_x1(x), h_x2(x) ]
        // where each partial has been rvec-ed.
        //
        //   h_x1(x) = rvec( [ 20 ] )
        //
        //   h_x2(x) = rvec( [ 21 ] )
        //
        for( i=0; i<1*2; i++ )
            pdmatC[i] = 20 + i;

        vector<DoubleMatrix> dmatD( 3 );
        dmatD[ 0 ]  = dmatA;
        dmatD[ 1 ]  = dmatB;
        dmatD[ 2 ]  = dmatC;

        blockDiagonalDerivative( dmatD ).print();
    }

$$
then it will display the following when it is run:
$codep

[ 0 4 ]
[ 1 5 ]
[ 0 0 ]
[ 0 0 ]
[ 0 0 ]
[ 2 6 ]
[ 3 7 ]
[ 0 0 ]
[ 0 0 ]
[ 0 0 ]
[ 0 0 ]
[ 0 0 ]
[ 10 14 ]
[ 11 15 ]
[ 0 0 ]
[ 0 0 ]
[ 0 0 ]
[ 12 16 ]
[ 13 17 ]
[ 0 0 ]
[ 0 0 ]
[ 0 0 ]
[ 0 0 ]
[ 0 0 ]
[ 20 21 ]

$$
$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include "blockDiagonalDerivative.h"
#include "replaceSubblock.h"
#include "getSubblock.h"
#include <cmath>

#pragma warning( disable : 4786 )
#include <vector>
/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

const DoubleMatrix blockDiagonalDerivative( const std::vector<DoubleMatrix>& dmatB )
{
  using namespace std;

	int i, j, k = 0, l = 0, m = 0, n, cols = dmatB[ 0 ].nc(), rows,
        nBlock = dmatB.size();

	for( i = 0; i < nBlock; i++ )
	{
		n = static_cast<int>(sqrt(static_cast<double>(dmatB[ i ].nr())));
		assert( n * n == dmatB[ i ].nr() );
		assert( dmatB[ i ].nc() == cols );
	    m += n ;
	}

    DoubleMatrix dmatA( m * m, cols );
    dmatA.fill( 0. );

	for( i = 0; i < nBlock; i++ )
	{
		rows = static_cast<int>(sqrt(static_cast<double>( dmatB[ i ].nr())));
		for( j = 0; j < rows; j++ )
		    replaceSubblock( dmatA, getSubblock( dmatB[ i ], rows * j, 0, 
			                 rows, cols ), k + m * j, 0 );
		l += rows * m + rows;
        k = l;
	}
	return dmatA;
}
