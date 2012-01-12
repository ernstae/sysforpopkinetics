/*************************************************************************
 * 
 * From:   Resource Facility for Population Kinetics
 *         Department of Bioengineering Box 352255
 *         University of Washington
 *         Seattle, WA 98195-2255
 *
 * Copyright (C) 2002, University of Washington,
 * Resource Facility for Population Kinetics. All Rights Reserved.
 *
 * This software was developed with support from NIH grant RR-12609.
 * Please cite this grant in any publication for which this software
 * is used and send a notification to the address given above.
 *
 * Check for updates and notices at:
 * http://www.rfpk.washington.edu
 *
 *************************************************************************/

/*************************************************************************
 *
 * File: cholesky.cpp
 *
 * Decompose a symmetric positive definite matrix, A, to C * C = A
 *
 * Author: Sachiko Honda, based on Mitch's inverse()
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: cholesky
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin cholesky$$
$spell 
  cout
  endl
  Cholesky
  chol
  Spk
  valarray
  const
  iostream
  namespace
  std
  cols
$$

$section Cholesky Factoring Of A Matrix (valarray version)$$

$index cholesky$$
$index decomposition, cholesky$$

$table
$bold Prototype:$$   $cend  
$syntax/cholesky( int /nCols/, const SPK_VA::valarray<double>& /A/, SPK_VA::valarray<double>& /CholL/ )/$$
$tend

See also: 
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$

$head Description$$
Returns a $bold lower triangular$$ matrix $math%C%$$ that is a Cholesky factor for the matrix $math%A%$$; i.e., 
$math%

     C  C = A
%$$
where $math%A%$$ is a double-precision positive definite symmetric matrix. 
The return value has the same type as $math%A%$$. 

The matrix $math%A%$$ is positive definite if 
$math%
     x  B x > 0
%$$
whenever $math%x%$$ is a nonzero column vector with row dimension equal to the column dimension of $math%A%$$. 
If the matrix $math%A%$$ is not positive definite, an $xref/SpkException//exception/$$ 
with an $xref/SpkError//SpkError/$$ object whose error code is set to $code SpkError::SPK_NOT_POS_DEF_ERR$$
will be thrown. 

$head Arguments$$
$syntax/
/nCols/
/$$
specifies the number of columns (or rows) in $math%A%$$.  $italic nCols$$ must be greater
than or equal to zero.

$syntax/

/A/
/$$
is a valarray object holding an $math%n * n%$$ length array of type double.  The array contains
the elements of $math%A%$$ in column major order.

$syntax/

/CholA/
/$$
is a valarray object holding an $math%n * n%$$ length array of type double.  
The lower triangle of $italic CholA$$ will be set to the cholesky factor for the matrix $math%A%$$.
If $math%A%$$ is not positive definite, this argument will not be altered.

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include "SpkValarray.h"
    #include "cholesky.h"

    void main()
    {
        using SPK_VA::valarray;
        using namespace std;

        const int n = 3;
        double a[] = { 2, 0, 1, 0, 3, 0, 1, 0, 1 };
        double e[] = { 1.41421, 0, 0.707107, 0, 1.73205, 0, 0, 0, 0.707107 };
        valarray<double> A( a, n*n );
        valarray<double> AChol = cholesky(A, n);
        valarray<double> expected( e, n*n );
        
        cout << "A:" << endl;
        printInMatrix(A,n);
        cout << endl;
        cout << "AChol (lower triangle): " << endl;
        printInMatrix(AChol,n);
        
    }
$$
then it will display the following when it is run:
$codep

    A:
    [ 2 0 1 ]
    [ 0 3 0 ]
    [ 1 0 1 ]

    AChol (lower triangle):
    [ 1.41421 0 0 ]
    [ 0 1.73205 0 ]
    [ 0.707107 0 0.707107 ]
$$
$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * <INFORMATION_REQUIRED_TO_UNDERSTAND_THE_FUNCTION_IMPLEMENTATION>.
 *
 *------------------------------------------------------------------------*/

/*************************************************************************
 *
 *     Function Implementation 
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 *     Standard Header Files
 *------------------------------------------------------------------------*/

#include <cassert>
#include <cmath>
#include <stdio.h>

extern "C"{
#include <clapack.h>
#include <cblas.h>
};
/*------------------------------------------------------------------------
 *     RFPK Header Files
 *------------------------------------------------------------------------*/

#include "SpkValarray.h"
#include "isSymmetric.h"
#include "intToOrdinalString.h"

using SPK_VA::valarray;

/*------------------------------------------------------------------------
 *     DoubleMatrix version of inverse()
 *------------------------------------------------------------------------*/
const SPK_VA::valarray<double> cholesky( const SPK_VA::valarray<double>& A, int n ) 
{
  assert( A.size() == n * n );
  assert( isSymmetric( A, n ) );

  //
  // Create a new matrix whose upper triangle (excluding the diagonal) are 
  // filled with zeros and the lower triangle (including the diagonal)
  // are copies of A.  This is because, the routine's specification
  // promise to return only the L s.t. A = L * L^t, not contaminated
  // with A's values in the upper triangle.
  //
  //         /                 \
  // AChol = |  a11   0    0   |
  //         |  a21  a22   0   |
  //         |  a31  a32  a33  |
  //         \                 /
  //
  // Put all of the elements of A into the Cholesky factor and then
  // zero the elements in the upper triangle.
  valarray<double> AChol( A );
  int i;
  int j;
  for ( j = 1; j < n; j++ )
  {
    for ( i = 0; i < j; i++ )
    {
      AChol[i + j*n] = 0.0;
    }
  }

  //
  // Use an Atlas provided CLAPACK routine to compute L such that A = L*L^t
  //
  // 1) CblasColMajor: telling the routine that the matrix stored in AChol is in column major order.
  // 2) CblasLower:    telling the routine to reference only the lower triangle of AChol to factorize
  // 3) n:             the order of A
  // 4) *A:            on entry, the lower triangle values in A are used to compute L.
  //                   on exit, the lower triangle values will be replaced by L.
  // 5) lda:           the stride from a column to the next (ie. = n in our case).
  //
  int lda = n;
  int cholStatus = clapack_dpotrf( CblasColMajor, CblasLower, n, &AChol[0], lda );
  if( cholStatus < 0 )
  {
     char mess[ SpkError::maxMessageLen() ];
     snprintf( mess, SpkError::maxMessageLen(), "Cholesky factorization failed: the %s argument had an illegal value.\n", 
              intToOrdinalString( -cholStatus, ONE_IS_FIRST_INT ).c_str() );
     throw SpkException( SpkError::SPK_NOT_POS_DEF_ERR, mess, __LINE__, __FILE__ );
  }
  if( cholStatus > 0 )
  {
     char mess[ SpkError::maxMessageLen() ];
     snprintf( mess, SpkError::maxMessageLen(), "Cholesky factorization failed: the leading minor of order %d is not positive definite.\n", cholStatus );
     throw SpkException( SpkError::SPK_NOT_POS_DEF_ERR, mess, __LINE__, __FILE__ );
  }

  return AChol;
}
