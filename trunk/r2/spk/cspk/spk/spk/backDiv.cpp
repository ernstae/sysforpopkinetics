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
 * File: backDiv.cpp
 *
 *
 * Solve A x = B for x, using LU decomposition.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: backDiv
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin backDiv$$
$spell 
    Exception exception
    div 
    const 
    lu 
    afc 
    Spk
    mult 
    ajc 
    rhs 
    namespace 
    std 
    int 
    iostream 
    pd 
    cout 
    endl
    div
    mul

$$

$section Solve a system of equations: A x = B$$

$index backDiv$$
$index matrix, division$$
$index matrix, matrix back division$$

$table
$bold Prototype:$$   $cend  
$syntax/DoubleMatrix backDiv( const DoubleMatrix &/A/, const DoubleMatrix &/B/ )/$$
$tend

See also: $xref/addition//addition/$$, $xref/subtraction//subtraction/$$, $xref/multiply//matrix multiply/$$,
$xref/mulByScalar//element-wise multiply/$$, $xref/divByScalar//element-wise division/$$.

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Solve $math%A x = B%$$ for $italic x$$, which is often expressed as $math%A \ B = x%$$,
using LU decomposition for A being a positive definite matrix.
Given A as a $math%m by n%$$ matrix and B as $math%m by l%$$ where $math%m = n%$$,
the solution $math%x%$$ has $math%m by l%$$ dimensions.
$pre

$$
If either A or B is empty, an empty matrix will be returned.
$pre

$$
This routine assumes the input $italic A$$ matrix is positive definite.  If not
it will throw a SpkException exception.

$head Arguments$$

$syntax/

&/A/
/$$
is a $math%m by n%$$ positive definite matrix (i.e. $math%m = n%$$) 
that represents the system of
$math%m%$$ equations with $math%n%$$ variables.

$syntax/

&/B/
/$$
is the right hand side quantities represented in the form of $math%m by l%$$ matrix.

$head Example$$
If you compile, link, and run the following program:
$codep
    #include <iostream>
    #include "DoubleMatrix.h"
    #include "backDiv.h"

    static void main(){

        using namespace std;

        const int m = 1;
        const int n = 3;
    
        DoubleMatrix A(n,n);    // A must be positive definite
        DoubleMatrix B(n,m);    // B could be anything
        DoubleMatrix x(n,m);    // unknown parameter we want to solve

        double *pdA = A.data();
        double *pdB = B.data();

        int i;

        // Set A to a matrix:
        //  [ 1  4  2 ]
        //  [ 2  5  3 ]
        //  [ 3  1  4 ]
        for( i=0; i<n*n; i++ )
            pdA[i] = i % 5 + 1;

        // Set B to a vector:
        //  [ 1 ]
        //  [ 1 ]
        //  [ 1 ]
        for( i=0; i<m*n; i++ )
            pdB[i] = 1;

        x = backDiv(A,B);

        cout << "A \\ B = x = " << endl;
        x.print();
        cout << endl;
        cout << "A x (should be equal to B) = " << endl;
        (A*x).print();
    }
$$
then it will display the following when it is run:
$codep

    A \ B = x =
    [-1]
    [0]
    [1]

    A x (should be equal to B) =
    [1]
    [1]
    [1]

$$
$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * Given A x = B and 
 * A is positive definite, but not necessary symmetric.
 * 
 * -> A is invertible
 * -> A has a LU decomposition
 * -> A x = B has a unique solution
 * 
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <cassert>
#include <stdio.h>
#include "backDiv.h"
#include "DoubleMatrix.h"
#include "intToOrdinalString.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
extern "C"{
#include <clapack.h>
#include <cblas.h>
}
#include <algorithm>
using namespace std;

const DoubleMatrix backDiv(const DoubleMatrix &dmatA, const DoubleMatrix &dmatB)
{
  // A is assumed to be square.
  int m = dmatA.nr();
  int n = dmatA.nc();
  assert( m == n );

  // B is m by l matrix, where l is the number of right hand sides.
  int l = dmatB.nc();
  assert( dmatB.nr() == m );

  if( dmatA.isEmpty() || dmatB.isEmpty() )
    return DoubleMatrix( 0, 0 );

  //==============================================================
  // First decompose A into LU such that A = P * L * U, 
  // where P is the permutation matrix,
  // L is the lower triangle and the U the upper triangle.
  //
  // We use CLAPACK's DGETRF() which does LU decomposition
  // with partial (ie. row interchanges only) pivoting.
  //==============================================================
  
  // enum CBLAS_ORDER order =: (CblasColMajor | CblasRowMajor)
  //
  // If order = CblasColMajor, the array, a, is assumed to 
  // hold each matrix A's column in the contiguous manner
  // in memory (ie. A is said to be in the column major order).
  // If order = CblasRowMajor, the array, a, is assumed to
  // hold each matrix A's row in the contiguous manner
  // in memory (ie. A is said to be in the row major order).
  enum CBLAS_ORDER order = CblasColMajor;

  // double *a
  //
  // (on entry) a points to the elements of matrix A(m,n) 
  // in the column major order if "order" = CblasColMajor, 
  // or in the row major order if "order" = CblasRowMajor.
  //
  // (on exit) The lower triangle (j<=i) is replaced by L
  // and the upper triangle (j>i) is replaced by U.
  double a[m*n];
  copy( dmatA.data(), dmatA.data()+m*n, a );

  // int lda
  // 
  // The leading dimension of A.  
  // If A is in the column major order, lda = m.
  // If A is in the row major order, lda = n.
  int lda = m;

  // int ipiv(m)
  //
  // (on exit) The i-th row in A was interchanged with the row
  // indicated by the value in ipiv[i].
  int ipiv[m];

  int info = clapack_dgetrf( order, m, n, a, lda, ipiv );
  if( info < 0 )
    {
      char mess[ SpkError::maxMessageLen() ];
      snprintf( mess, SpkError::maxMessageLen(), "Solution of a system of linear equations using the LU decomposition failed: \n the %s argument to the function that performs the LU decomposition  had an illegal value.", 
               intToOrdinalString( -info, ONE_IS_FIRST_INT ).c_str() );
      throw SpkException( SpkError::SPK_UNKNOWN_ERR, mess, __LINE__, __FILE__ );
    }
  else if( info > 0 )
    {
      char mess[ SpkError::maxMessageLen() ];
      snprintf( mess, SpkError::maxMessageLen(), "Solution of a system of linear equations using the LU decomposition failed: \nthe %s diagonal element of U is exactly zero.", 
               intToOrdinalString( info, ONE_IS_FIRST_INT ).c_str() );
      throw SpkException( SpkError::SPK_NOT_POS_DEF_ERR, mess, __LINE__, __FILE__ );
    }

  //==============================================================
  // Solve A x = B for x using the LU computed in the previous
  // step.
  // Note that A is now assumed to be square: m = n.
  //==============================================================
  
  // int rhs
  //
  // The number of right hand sides (ie. the number of columns of B).
  int nrhs = l;

  // int ldb
  // The leading dimension of B.
  // If B is in the column major order, ldb = m.
  // If B is in the row major order, ldb = l.
  int ldb = m;

  // double *x
  //
  // (on entry) x points to the elements of B in the column major
  // order if "order" = CblasColMajor or in the row major otherwise.
  // (on exit) x points to the solution matrix, x (m=n by l).
  DoubleMatrix X( dmatB );
  double * x = X.data();// This points to b on entry and contains the solution x upon exit

  info = clapack_dgetrs( order, CblasNoTrans, n, nrhs, a, lda, ipiv, x, ldb );
  if( info < 0 )
    {
      char mess[ SpkError::maxMessageLen() ];
      snprintf( mess, SpkError::maxMessageLen(), "Solution of a system of linear equations using the LU decomposition failed: \nthe %s argument to the function that solves the equations had an illegal value.", 
               intToOrdinalString( -info, ONE_IS_FIRST_INT ).c_str() );
      throw SpkException( SpkError::SPK_UNKNOWN_ERR, mess, __LINE__, __FILE__ );
    }

  return X; 
}
