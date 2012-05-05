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
#include "multiply.h"

/*
-------------------------------------------------------------
   Valarray version of multiply()
-------------------------------------------------------------
$begin multiply$$

$spell
 cout
 Spk
 valarray
 const
 namespace
 iostream
 endl
 iAnc
 iBnc
 iAnr
 iBnr
 std
 cols
$$

$section Matrix-Matrix Multiplication$$

$index multiply$$
$index matrix, matrix multiplication$$

$table
$bold Prototype: $$ $cend
$cend
$syntax/
const SPK_VA::valarray<class T> multiply( const SPK_VA::valarray<class T>& /a/, int /nColsA/, 
                                          const SPK_VA::valarray<class T>& /b/, int /nColsB/ )
/$$
$rend
$tend

See also: $xref/multiply//DoubleMatrix version/$$.
$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Performs matrix-matrix multiply.

$head Return Value$$
Given a $math%nRowsA%$$ by $math%nColsA%$$ matrix, $math%A%$$ and 
a $math%nRowsB%$$ by $math%nColsB%$$ matrix, $math%B%$$,
where $math%nColsA == nRowsB%$$, the function returns an array containing the values of the
resulting $math%nRowsA%$$ by $math%nColsB%$$ matrix in column-major order. 


$head Arguments$$

$syntax/
/a/
/$$
is an array containing the values of a $math%nRowsA%$$ by $math%nColsA%$$ matrix, $math%A%$$, 
in column-major order, where $math%nRowsA%$$ and $math%nColsA%$$ are greater than or equal to zero.
$syntax/

/nColsA/
/$$
specifies the number of columns in the matrix $math%A%$$.
The value must be greater than or equal to zero.


$syntax/

/b/
/$$
is an array containing the values of a $math%nRowsB%$$ by $math%nColsB%$$ matrix, $math%B%$$,
in column major order, where $math%nRowsB%$$ and $math%nColsB%$$ are greater than or equal to zero.

$syntax/

/nColsB/
/$$
specifies the number of columns in the matrix $math%B%$$.
The value must be greater than or equal to zero.  The length of $italic b$$ divided by
$italic nColsB%$$ must be equal to $italic nColsA$$; otherwise, the program terminates.

$head Example$$
If you compile, link and run the following program
$codep

#include <iostream>
#include "SpkValarray.h"
#include "multiply.h"

void main()
{
    using SPK_VA::valarray;
    using std::cout;
    using std::endl;

    // Set A to a matrix:
    //
    //     /         \
    // A = | 1  3  5 |
    //     | 2  4  6 |
    //     \         /
    //
    double a[] = { 1, 2, 3, 4, 5, 6 };
    valarray<double> A(a, 6);

    // set B to a matrix:
    //
    // b = /   \
    //     | 1 |
    //     | 2 |
    //     | 3 |
    //     \   /
    //
    double b[] = { 1, 2, 3 };
    valarray<double> B(b, 3);
    valarray<double> C;

    int i;
    // Compute C:
    //      /           \     /     \     /      \
    // C =  |  1  3  5  |  *  |  1  |  =  |  22  |
    //      |  2  4  5  |     |  2  |     |  28  |
    //      \           /     |  3  |     \      /
    //                        \     /
    C = multiply(A, 3, B, 1);
    cout << "C = " << C << endl;
}

$$
the matrix 
$codep
    { 22, 28 }
$$
will be printed.

$end
*/
#include <cassert>
#include "SpkValarray.h"
extern "C"{
#include <cblas.h>
};

namespace{
  const double ALPHA = 1.0;
  const double BETA  = 0.0;
};

using SPK_VA::valarray;

const valarray<double> multiply( const valarray<double>& X, int nColsX, 
                                 const valarray<double>& Y, int nColsY )
{
  if( X.size() == 0 || Y.size() == 0 )
    return valarray<double>(0);

  //
  // Row Major Matrices
  //
  //    C = alpha * A * B + beta * C  --- (1)
  //
  // A : m by k         lda (stride) = k
  // B : k by n         ldb (stride) = n
  // C : m by n         ldc (stride) = n
  //
  //
  // Column Major Matrices
  //
  //    Z = alpha * X * Y + beta * C  --- (2)
  //    Z = C^t 
  //      = alpha * B^t * A^t + beta * C^t  --- (3)
  //
  // X = B^t : n by k   ldx (stride) = n
  // Y = A^t : k by m   ldy (stride) = k
  // Z = C^t : n by m   ldz (stride) = n
  //
   const int m = nColsY;
   const int k = nColsX;
   const int n = X.size() / k; assert( n*k == X.size() );
 
   valarray<double> Z( n * m );
   const double *pX = &X[0];
   const double *pY = &Y[0];
   double *pZ = &(Z[0]);

   int lda = n;
   int ldb = k;
   int ldc = n;
  
   cblas_dgemm( CblasColMajor, 
		CblasNoTrans,
		CblasNoTrans,
		n,   // #rows of the first matrix
		m,   // #cols of the second matrix
		k,   // #cols of the first which is equal to the second matrix
		ALPHA,
		pX,  // B^t
		lda,   // stride of B^t = #rows in B^t = #cols of B
		pY,  // A^t
		ldb,   // stride of A^t = #rows of A^t = #cols of A
		BETA,
		pZ,  // C^t
		ldc    // stride of C^t = #rows of C^t = #cols of C
              );
   return Z;
}

#include "DoubleMatrix.h"

const DoubleMatrix multiply(const DoubleMatrix &X, const DoubleMatrix &Y)
{
    DoubleMatrix Z(X.nr(), Y.nc());
    multiply(X,Y,Z);
    return Z;
}

void multiply(const DoubleMatrix &X, const DoubleMatrix &Y, DoubleMatrix& Z)
{
  assert(&X!=&Z);
  assert(&Y!=&Z);
  if( X.isEmpty() || Y.isEmpty() )
    return;

  //
  // Row Major Matrices
  //
  //    C = alpha * A * B + beta * C  --- (1)
  //
  // A : m by k         lda (stride) = k
  // B : k by n         ldb (stride) = n
  // C : m by n         ldc (stride) = n
  //
  // Column Major Matrices
  //
  //    Z = alpha * X * Y + beta * C  --- (2)
  //    Z = C^t 
  //      = alpha * B^t * A^t + beta * C^t  --- (3)
  //
  // X = B^t : n by k   ldx (stride) = n
  // Y = A^t : k by m   ldy (stride) = k
  // Z = C^t : n by m   ldz (stride) = n
  //
  int m = Y.nc();
  int k = X.nc();
  int n = X.nr();

  Z.resize( n, m );
  
  assert( X.nr() == n );
  assert( X.nc() == k );
  assert( Y.nr() == k );
  assert( Y.nc() == m );
  assert( Z.nr() == n );
  assert( Z.nc() == m );
  
  const double * pX = X.data();
  const double * pY = Y.data();
  double       * pZ = Z.data(); 
  
  int lda = n;
  int ldb = k;
  int ldc = n;

  cblas_dgemm( CblasColMajor, CblasNoTrans, CblasNoTrans, n, m, k, ALPHA, pX, lda, pY, ldb , BETA, pZ, ldc );

}

