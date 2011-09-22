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


/*------------------------------------------------------------------------
 *     RFPK Header Files
 *------------------------------------------------------------------------*/

#include "transpose.h"
#include "SpkValarray.h"
#include "isSymmetric.h"

using SPK_VA::valarray;
using SPK_VA::slice;

/*------------------------------------------------------------------------
 *     Local Function Declarations
 *------------------------------------------------------------------------*/
static void INagRealCholesky( int nCols, 
                              const valarray<double>& A, 
                              valarray<double>& AChol);

/*------------------------------------------------------------------------
 *     DoubleMatrix version of inverse()
 *------------------------------------------------------------------------*/
const valarray<double> cholesky( const valarray<double>& A, int nCols ) 
{
  int nRows = A.size() / nCols;
  assert( A.size() == nRows * nCols );
  assert( nRows == nCols);   // A must be square.
  assert( isSymmetric( A, nCols ) );

  valarray<double> AChol( 0.0, nRows * nCols );

  // Call the interface to the NAG routine that solves a set
  // of real linear equations with multiple right-hand sides
  // in order to determine the cholesky factor.
  INagRealCholesky( nCols, A, AChol );

  return AChol;
}
/*------------------------------------------------------------------------
 *     NAG Header Files
 *------------------------------------------------------------------------*/

#include "nag.h"
#include "nag_types.h"
#include "nag_stdlib.h"
#include "nagf03.h"
#include "nagf04.h"

#include "identity.h"
/*------------------------------------------------------------------------
 *     Function Definition
 *------------------------------------------------------------------------*/
static void INagRealCholesky( int nCols,
                              const valarray<double>& A, 
                              valarray<double>& AChol)
{
  //***********************************************************
  // Preliminaries.
  //***********************************************************

  int nRows = A.size() / nCols;
  assert( nRows == nCols );   // A must be square.
  assert( nRows > 0 );        // A must have more than one row.

  // Because the data elements of valarray are stored in
  // column-major order and NAG routines expect arrays to be
  // stored in row-major order, instantiate a new valarray
  // that is the transpose of A.
  valarray<double> ATrans = transpose( A, nCols );

  // Instantiate a valarray that will hold the transpose of 
  // the inverse of A.
  valarray<double> AInvTrans( nRows * nCols );

  // This column vector will hold the reciprocals of the 
  // diagonal elements of L, the Cholesky factor of A. 
  valarray<double> LDiag( nRows );

  // Set B equal to an identity matrix with the same dimensions 
  // as A.
  valarray<double> B(nRows*nCols);
  identity(nRows, B);

  //***********************************************************
  // Define the parameters for nag_real_cholesky.
  //
  // Note that the input and output specifications below apply 
  // to the parameters only as they are used with nag_real_cholesky, 
  // i.e. they are not the specifications for the parameters  
  // to this function, INagInvRealSymmPosDefMatrix.
  //***********************************************************

  // Parameter: n.
  // Input: n, the order of the matrix A. 
  // Output: unspecified.
  // Constraint: n >= 1. 
  Integer n = nRows;
  assert(n >= 1);

  // Parameter: a[n][tda].
  // Input: the upper triangle of the n by n positive-definite
  // symmetric matrix A.  The elements of the array below the
  // diagonal need not be set.
  // Output: the sub-diagonal elements of the lower triangular
  // matrix L. The upper triangle of A is unchanged.
  double* a = &ATrans[0];

  // Parameter: tda.
  // Input: the last dimension of the array a as declared in the
  // function from which nag_real_cholesky is called.
  // Output: unspecified.
  // Constraint: tda >= n.
  Integer tda = n;

  // Parameter: p[n].
  // Input: unspecified.
  // Output: the reciprocals of the diagonal elements of L.
  double* p = &LDiag[0];

  // Parameter: detf.
  // Parameter: dete.
  // Input: unspecified.
  // Output: the determinant of A is given by detf * 2.0^dete.
  // It is given in this form to avoid overflow or underflow.
  double detf;
  Integer dete;

  //***********************************************************
  // Perform the Cholesky factorization.
  //***********************************************************
 
  static NagError fail;
  INIT_FAIL(fail);
  nag_real_cholesky(n, a, tda, p, &detf, &dete, &fail);
  if( fail.code != NE_NOERROR )
  {
    switch( fail.code )
    {
    case NE_NOT_POS_DEF:
      throw SpkException( SpkError::SPK_NOT_POS_DEF_ERR, 
        "The matrix is not positive-definite,  possibly due to round-ing errors.",
      __LINE__, __FILE__ );
      break;
    default:
      throw SpkException( SpkError::SPK_UNKNOWN_ERR, 
        "Failed to compute a Cholesky factorization.",
      __LINE__, __FILE__ );
      break;
    }
  }
    
  //***********************************************************
  // Finish up.
  //***********************************************************

  // set diagonal elements
  AChol[ slice( 0, nRows, nCols + 1 ) ] = 1.0 / LDiag;
  for( int j=0; j<nCols; j++ )
  {
      for( int i=0; i<nRows; i++ )
      {
          // lower triangle elements
          if( i > j )
          {
              AChol[i + j*nRows] = ATrans[ j + i*nCols ];
          }
      }
  }
  return;
}
