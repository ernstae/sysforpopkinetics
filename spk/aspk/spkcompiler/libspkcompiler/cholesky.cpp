
/*************************************************************************
 *
 * File: cholesky.cpp
 *
 * Decompose a symmetric positive definite matrix, A, to C * C = A
 *
 * Author: Sachiko Honda, based on Mitch's inverse()
 *
 *************************************************************************/
#include <cassert>
#include <cmath>

extern "C"{
#include <atlas/cblas.h>
#include <atlas/clapack.h>
};
/*------------------------------------------------------------------------
 *     RFPK Header Files
 *------------------------------------------------------------------------*/

#include "cholesky.h"

using namespace std;

/*------------------------------------------------------------------------
 *     DoubleMatrix version of inverse()
 *------------------------------------------------------------------------*/
const valarray<double> cholesky( const valarray<double>& A, int n ) 
{
  assert( A.size() == n * n );

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
  valarray<double> AChol( n*n );
  for( int j=0; j<n; j++ )
    {
      for( int i=0; i<n; i++ )
	{
	  // Zeros in the upper
	  if( i<j )
	    {
	      AChol[i+j*n] = 0.0;
	    }
	  // Copy the lower + diagonal elements in A
	  else
	    {
	      AChol[i+j*n] = A[i+j*n];
	    }
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
  clapack_dpotrf( CblasColMajor, CblasLower, n, &AChol[0], lda );

  return AChol;
}
