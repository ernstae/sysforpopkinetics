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
 * File: inverse.cpp
 *
 *
 * Computes the inverse of a square, symmetric, positive-definite,
 * double precision matrix.
 *
 * Revisit-Mitch: We may want to consider moving this function to 
 * DoubleMatrix class as a member function.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 *     DoubleMatrix version of inverse() 
 *
 *************************************************************************/

/*
$begin inverse$$
$spell dmat const iostream iomanip namespace std int pdbl cout 
        setiosflags ios setprecision endl spk
$$

$section Inverse of a matrix stored as a DoubleMatrix object$$

$index inverseDoubleMatrix$$
$index matrix, inverse --- DoubleMatrix version$$

$table
$bold Prototype$$   $cend  
$syntax/const DoubleMatrix inverse(const DoubleMatrix& /dmatA/)/$$   $rend 
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
Returns the matrix inverse of the square, symmetric, positive-definite 
$code DoubleMatrix$$ $italic dmatA$$. The return value is of type 
$code DoubleMatrix$$, and it has the same dimensions as $italic dmatA$$.

$head Arguments$$
$syntax/
/dmatA/
/$$
The $code DoubleMatrix$$ $italic dmatA$$ contains the square, symmetric, 
positive-definite matrix $math%A%$$.  If the matrix did not
have these properties, it throws an $xref/SpkException//SpkException/$$.

$head Example$$
If you compile and link the C++ program,
$codep

#include <iostream>
#include <iomanip>
#include "DoubleMatrix.h"
#include "inverse.h"

void main()
{
  using namespace std;

  int nARow = 2;
  int nACol= nARow;
  DoubleMatrix dmatA(nARow, nACol);
  double* pdblAData = dmatA.data();
  pdblAData[0 + 0 * nARow] = 1000000.0;
  pdblAData[0 + 1 * nARow] =      -1.0;
  pdblAData[1 + 0 * nARow] =      -1.0;
  pdblAData[1 + 1 * nARow] =       2.0;
  
  cout << setiosflags(ios::scientific) << setprecision(15);

  cout << "A = " << endl;
  dmatA.print();
  cout << endl;

  cout << "inverse(A) = " << endl;
  inverse(dmatA).print();
  cout << endl;
}

$$
then it should output the following when it is run:
$codep

A =
[1.000000000000000e+006, -1.000000000000000e+000]
[-1.000000000000000e+000, 2.000000000000000e+000]

inverse(A) =
[1.000000500000250e-006, 5.000002500001250e-007]
[5.000002500001249e-007, 5.000002500001249e-001]

$$
$end
*/

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

extern "C"{
  #include <atlas/clapack.h>
  #include <atlas/cblas.h>
};
/*------------------------------------------------------------------------
 *     RFPK Header Files
 *------------------------------------------------------------------------*/

#include "inverse.h"
#include "DoubleMatrix.h"
#include "isSymmetric.h"
#include "symmetrize.h"
#include "SpkException.h"
#include "isDblEpsEqual.h"
#include "transpose.h"
#include "intToOrdinalString.h"

using SPK_VA::valarray;

/*------------------------------------------------------------------------
 *     DoubleMatrix version of inverse()
 *------------------------------------------------------------------------*/

const DoubleMatrix inverse(const DoubleMatrix& dmatA) 
{
  int n = dmatA.nr();
  assert( dmatA.nc() == n );
  const double* A = dmatA.data();
  if( !isSymmetric( dmatA ) )
  {
     char mess[ SpkError::maxMessageLen() ];
     DoubleMatrix dmatAt = transpose( dmatA );
     double * At = dmatAt.data();

     for( int j=0; j<n; j++ )
     {
        for( int i=j; i<n; i++ )
        {
           double a  = A[i+j*n];
           double at = At[i+j*n];
           if( !isDblEpsEqual( a, at, (fabs(a)>fabs(at)? fabs(a) : fabs(at) ) ) )
           {
              snprintf( mess, SpkError::maxMessageLen(),
              "Inversion using the Cholesky factorization failed because the difference between two reflecting \nelements is too large: a(%d, %d)=%f - a(%d, %d)=%f = %f.",
              i+1, j+1, A[i], j+1, i+1, At[i], fabs(a-at) );
                                                                                                                         
              throw SpkException( SpkError::SPK_NOT_SYMMETRIC_ERR, mess, __LINE__, __FILE__ );
           }
        }
     }
  }
  int lda = n;
  DoubleMatrix AInv(dmatA);
  double *a = AInv.data();

  //
  // Cholesky factorization: A = L * L^t
  // 
  // clapack_dpotrf() is an ATLAS implementation of LAPACK
  // routine, DPOTRF.
  // This implementation is great in that it takes an
  // argument, the 1st argument, indicating the order of
  // matrix so that we don't have to flip around the matrix.
  //
  // CblasColMajor : Telling the routine that the matrix 
  //                 stored in the array, a is in the 
  //                 column major order.
  // 
  // CblasLower    : Telling the routine to reference
  //                 only the lower triangle elements.
  //
  // n             : The order of the matrix stored in a.
  //
  // *a            : (in/out) On entry, a(i,j) for i<=j
  //                 are referenced (because we chose CblasLower).
  //
  //                 On exit, a(i,j) for i<=j are replaced
  //                 with L such that A = L * L^t.
  //                 
  // lda           : The stride from one column to the other.
  //                 which is same as the number of rows
  //                 of A since the values are stored
  //                 contigurously in the memory.
  //
  int cholStatus = clapack_dpotrf( CblasColMajor, CblasLower, n, a, lda );
  if( cholStatus < 0 )
  {
     char mess[ SpkError::maxMessageLen() ];
     snprintf( mess, SpkError::maxMessageLen(),
               "Inversion using the Cholesky factorization failed: \nthe %s argument had an illegal value.", 
              intToOrdinalString( -cholStatus, ONE_IS_FIRST_INT ).c_str() );
     throw SpkException( SpkError::SPK_NOT_POS_DEF_ERR, mess, __LINE__, __FILE__ );
  }
  if( cholStatus > 0 )
  {
     char mess[ SpkError::maxMessageLen() ];
     snprintf( mess, SpkError::maxMessageLen(),
               "Inversion using the Cholesky factorization failed: \nthe leading minor of order %d is not positive definite.", cholStatus );
     throw SpkException( SpkError::SPK_NOT_POS_DEF_ERR, mess, __LINE__, __FILE__ );
  }
  // 
  // Inverse based on Cholesky : A <= A^-1
  // 
    // clapack_dpotrf() is an ATLAS implementation of LAPACK
  // routine, DPOTRF.
  // This implementation is great in that it takes an
  // argument, the 1st argument, indicating the order of
  // matrix so that we don't have to flip around the matrix.
  //
  // CblasColMajor : Telling the routine that the matrix 
  //                 stored in the array, a is in the 
  //                 column major order.
  // 
  // CblasLower    : Telling the routine to reference
  //                 only the lower triangle elements.
  //
  // n             : The order of the matrix stored in a.
  //
  // *a            : (in/out) On entry, a(i,j) for i<=j
  //                 are referenced (because we chose CblasLower).
  //
  //                 On exit, a(i,j) for i<=j are replaced
  //                 with L such that A = L * L^t.
  //                 
  // lda           : The stride from one column to the other.
  //                 which is same as the number of rows
  //                 of A since the values are stored
  //                 contigurously in the memory.
  //
  int invStatus = clapack_dpotri( CblasColMajor, CblasLower, n, a, lda );
  if( invStatus < 0 )
  {
     char mess[ SpkError::maxMessageLen() ];
     snprintf( mess, SpkError::maxMessageLen(), 
               "Inversion using the Choleksy factorization failed: \nthe %s argument had an illegal value.", 
               intToOrdinalString( -invStatus, ONE_IS_FIRST_INT ).c_str() );
     throw SpkException( SpkError::SPK_NOT_INVERTABLE_ERR, mess, __LINE__, __FILE__ );
  }
  if( invStatus > 0 )
  {
     char mess[ SpkError::maxMessageLen() ];
     snprintf( mess, SpkError::maxMessageLen(),
               "Inversion using the Cholesky factorization failed: \nthe %s diagonal element of the Cholesky factor is zero.",
               intToOrdinalString( invStatus, ONE_IS_FIRST_INT ).c_str() );
     throw SpkException( SpkError::SPK_NOT_INVERTABLE_ERR, mess, __LINE__, __FILE__ );
  }

  symmetrize( AInv, AInv );
  return AInv;
  
}
/*------------------------------------------------------------------------
 *     valarray version of inverse()
 *------------------------------------------------------------------------*/
const valarray<double> inverse( const valarray<double> &A, int n )
{
  assert( A.size() == n*n );
  if( !isSymmetric( A, n ) )
  {
     char mess[ SpkError::maxMessageLen() ];
     snprintf( mess, SpkError::maxMessageLen(), 
              "The matrix is not symmetric." );
     throw SpkException( SpkError::SPK_NOT_SYMMETRIC_ERR, mess, __LINE__, __FILE__ );
  }
  int lda = n;
  valarray<double> AInv(A);

  // Cholesky-factorize
  int cholStatus = clapack_dpotrf( CblasColMajor, CblasLower, n, &AInv[0], lda );
  if( cholStatus < 0 )
  {
     char mess[ SpkError::maxMessageLen() ];
     snprintf( mess, SpkError::maxMessageLen(),
               "Inversion using the Cholesky factorization failed: \nthe %s argument had an illegal value.", 
               intToOrdinalString( -cholStatus, ONE_IS_FIRST_INT ).c_str() );
     throw SpkException( SpkError::SPK_NOT_POS_DEF_ERR, mess, __LINE__, __FILE__ );
  }
  if( cholStatus > 0 )
  {
     char mess[ SpkError::maxMessageLen() ];
     snprintf( mess, SpkError::maxMessageLen(),
               "Inversion using the Cholesky factorization failed: \nthe leading minor of order %d is not positive definite.", cholStatus );
     throw SpkException( SpkError::SPK_NOT_POS_DEF_ERR, mess, __LINE__, __FILE__ );
  }

  // Compute the inverse using the cholesky factor.
  int invStatus = clapack_dpotri( CblasColMajor, CblasLower, n, &AInv[0], lda );
  if( invStatus < 0 )
  {
     char mess[ SpkError::maxMessageLen() ];
     snprintf( mess, SpkError::maxMessageLen(),
               "Inversion using the Choleksy factorization failed: \nthe %s argument had an illegal value.", 
               intToOrdinalString( -invStatus, ONE_IS_FIRST_INT ).c_str() );
     throw SpkException( SpkError::SPK_NOT_INVERTABLE_ERR, mess, __LINE__, __FILE__ );
  }
  if( invStatus > 0 )
  {
     char mess[ SpkError::maxMessageLen() ];
     snprintf( mess, SpkError::maxMessageLen(),
               "Inversion using the Cholesky factorization failed: \nthe %s diagonal element of the Cholesky factor is zero.",
               intToOrdinalString( invStatus, ONE_IS_FIRST_INT ).c_str() );
     throw SpkException( SpkError::SPK_NOT_INVERTABLE_ERR, mess, __LINE__, __FILE__ );
  }
  // 
  symmetrize( AInv, n, AInv );
  return AInv;
}

/*************************************************************************
 *
 *     valarray version of inverse() 
 *
 *************************************************************************/

/*
$begin inverseVA$$
$spell dmat const iostream iomanip namespace std int pdbl cout spk
        setiosflags ios setprecision endl
        valarray
        cols
        spk
$$

$section Inverse of a matrix stored in a vector$$

$index inverseValarray$$
$index matrix, inverse --- valarray version$$

$table
$bold Prototype$$   $cend  
$syntax/const valarray<double> inverse(const valarray<double>& /a/, int nCols)/$$   $rend 
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
Returns the matrix inverse of the square, symmetric and positive-definite 
matrix, $italic a$$, stored in a valarray object in the column order. 

$head Arguments$$
$syntax/
/dmatA/
/$$
The $code valarray$$ $italic a$$ contains the square, symmetric, 
positive-definite matrix $math%A%$$ in the column major order.
If the matrix did not
have these properties, it throws an $xref/SpkException//SpkException/$$.

$head Example$$
If you compile and link the C++ program,
$codep

#include <iostream>
#include <iomanip>
#include "SpkValarray.h"
#include "inverse.h"

void main()
{
  using namespace std;

  int nRows = 2;
  int nCols= nARow;
  valarray<double> a (nRows * nCols);

  a[0 + 0 * nARow] = 1000000.0;
  a[0 + 1 * nARow] =      -1.0;
  a[1 + 0 * nARow] =      -1.0;
  a[1 + 1 * nARow] =       2.0;
  
  cout << setiosflags(ios::scientific) << setprecision(15);

  cout << "a = " << endl;
  DoubleMatrix( a, nRows ).print();
  cout << endl;

  cout << "inverse(a, nCols) = " << endl;
  DoubleMatrix( inverse(a, nCols), nCols ).print();
  cout << endl;
}

$$
then it should output the following when it is run:
$codep

A =
[1.000000000000000e+006, -1.000000000000000e+000]
[-1.000000000000000e+000, 2.000000000000000e+000]

inverse(A) =
[1.000000500000250e-006, 5.000002500001250e-007]
[5.000002500001249e-007, 5.000002500001249e-001]

$$
$end
*/
