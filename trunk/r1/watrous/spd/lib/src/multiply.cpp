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

static void NagDgemm(
    char transa,            // Is matrix A transpose (At) or non (An)?
    char transb,            // Is matrix B transpose (Bt) or non (Bn)? 
    int m,                  // (An) A->nr, (At) A->nc
    int n,                  // (Bn) B->nc, (Bt) B->nr
    int k,                  // (An) A->nc, (Bn) B->nr, (At) A->nr, (Bt) B->nc
    double alpha,           // scalar
    const double A[],       // column major matrix
    const double B[],       // column major matrix
    double beta,            // scaler
    double C[]              // column major matrix
);
/*
-------------------------------------------------------------
   Valarray version of multiply()
-------------------------------------------------------------
$begin multiplyVA$$

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

using SPK_VA::valarray;
const valarray<double> multiply( const valarray<double>& A, int nColsA, 
                                 const valarray<double>& B, int nColsB )
{
  using namespace std;

  if( A.size() == 0 || B.size() == 0 )
          return valarray<double>(0);

  int nRowsA = A.size() / nColsA;
  assert( nRowsA * nColsA == A.size() );

  int nRowsB = B.size() / nColsB;
  assert( nRowsB * nColsB == B.size() );

  assert( nColsA == nRowsB );

  //
  // A represents a matrix:
  //     [ a11   a12   a13 ... a1n ]
  //     [ a21   a22   a23 ... a2n ]
  //     [ a31   a32   a33 ... a3n ]
  //     [ ...             ...     ]
  //     [ am1   am2   am3 ... amn ]
  //
  // Spk stores it in a vector in the colomn-major order.
  //     [ a11, a21, a31, ... am1, a12, a22, a32, ... am2, ..., a1n, a2n, a3n, ..., amn ]
  //      
  // NAG expects in the row major order:
  //     [ a11, a12, a13, ... a1n, a21, a22, a23, ... a2n, ..., am1, am2, am3, ..., amn ]
  //
  // In effect, for NAG, SPK's matrix looks like a transpose of dimensions swapped around.
  // To trick NAG, without SPK shuffling the elements order, 
  // switch around the dimensions of matrices, A and B, which in turn requires
  // switching A and B's roles to essentially B and A.  Yet, don't tell NAG they are transposes.
  // 
  valarray<double> AA = A;
  valarray<double> BB = B;
  valarray<double> C( nRowsA * nColsB );
  
  const double *pA = &AA[0];
  const double *pB = &BB[0];
  double* pC = &C[0];

  NagDgemm('n', 'n', nColsB, nRowsA, nRowsB, 1.0, pB, pA, 0.0, pC);
  return C;


  //
  // [ Note by Sachiko, 10/04/02 ]
  // You cannot beat a commertial library like NAG for matrix
  // multiply operation.  You can do it from scratch if you'd like
  // but it'll be much & much slower.
  //
  //
  //  |   [ a(1,1),      a(1,2),    ... a(1,nColsA)       ]
  // i|   [ a(2,1),      a(2,2),    ... a(2,nColsA)       ]
  //  |   [ ...                                           ]
  // \|/  [ a(nRowsA,1), a(nRowsA,2)... a(nRowsA, nColsA) ]
  //
  //
  //                      j                    \
  //  ------------------------------------------\
  //                                           /
  // [ b(1,1),       b(1,2),        ... b(1, nColsB)      ]
  // [ b(2,1),       b(2,2),        ... b(2, nColsB)      ]
  // [ ...                                                ]
  // [ b(nRowsB, 1), b(nRowsB, 2),  ... b(nRowsB, nColsB) ]
  //
  //
  /*
  for( int j=0; j<nColsB; j++ )
  {
    for( int i=0; i<nRowsA; i++ )
    {
      C[ i + j * nRowsA ] = ( A[ slice( i, nColsA, nRowsA) ] * B[ slice( j * nRowsB, nRowsB, 1) ]).sum();
    }
  }
  */
};

/******************************************************************************************
 *
 *
 *                NagDgemm Implementation   
 *
 * Cpp interface to Nag's dgemm (f06yac)
 *
 * dgemm (f06yac) performs real matrix-matrix multiplication: 
 *    C = (alpha * A B) + (beta * C)
 *
 * This function terminates the program when receives an invalid argument value.
 *
 ******************************************************************************************/
# include <stdio.h>
# include <stdlib.h>
# include "nag.h"
# include "nag_types.h"
# include "nag_names.h"
# include "nagf06.h"

void NagDgemm(
    char transa,            // Is matrix A transpose (At) or non (An)?
    char transb,            // Is matrix B transpose (Bt) or non (Bn)? 
    int m,                  // (An) A->nr, (At) A->nc
    int n,                  // (Bn) B->nc, (Bt) B->nr
    int k,                  // (An) A->nc, (Bn) B->nr, (At) A->nr, (Bt) B->nc
    double alpha,           // scalar
    const double A[],       // column major matrix
    const double B[],       // column major matrix
    double beta,            // scaler
    double C[]              // column major matrix
){
    const char* errmsg = 
    "\nNagDgemm received an invalid value in the %s argument (%c)...terminating program.\n";


    MatrixTranspose transA;
    MatrixTranspose transB;

    if( transa == 'n' || transa == 'N' ){
        transA = NoTranspose;
    }
    else if( transa == 't' || transa == 'T' ){
        transA = Transpose;
    }
    else{
        fprintf(stderr, errmsg, "1st", transa);
        exit(-1);
    }

    // for matrix B
    if( transb == 'n' || transb == 'N'){
        transB = NoTranspose;
    }
    else if( transb == 't' || transb == 'T' ){
        transB = Transpose;
    }
    else{
        fprintf(stderr, errmsg, "2nd", transb);
        exit(-1);
    }
    
    dgemm(
        transA, 
        transB, 
        (Integer) m, 
        (Integer) n, 
        (Integer) k, 
        alpha, 
        A, 
        (Integer) k, 
        B, 
        (Integer) n, 
        beta, 
        C, 
        (Integer) n
    );
}

/*
-------------------------------------------------------------
   multiply()
-------------------------------------------------------------
$begin multiply$$

$spell
    dmat 
    const 
    Goddard 
    Sachiko 
    nc 
    nr 
    int 
    non 
    spk
    mul
    div
    valarray
    SPK_VA
    Anc
    Bnc
    Anr
    Bnr
    DMclass
$$

$section Matrix-Matrix Multiplication (DoubleMatrix version)$$

$index multiply (DoubleMatrix version)$$
$index matrix, matrix multiplication (DoubleMatrix version)$$

$table
$bold Prototype$$ $cend
$syntax/const DoubleMatrix multiply(const DoubleMatrix &/A/, const DoubleMatrix &/B/) /$$ $rend
$cend
$syntax/void multiply(const DoubleMatrix &/A/, const DoubleMatrix &/B/, DoubleMatrix &/C/)/$$ $rend
$rend
$tend
See also: $xref/addition//addition/$$, $xref/subtraction//subtraction/$$, $xref/backDiv//matrix back division/$$,
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
Performs matrix-matrix multiply.

$head Return Value$$
Given a $math%iAnr by iAnc%$$ matrix, $italic A$$, and a $math%iBnr by iBnc%$$ matrix, $italic B$$,
where $math%iAnc == iBnr%$$,
the resulting matrix has $math%iAnr by iBnc%$$ dimensions.  The result is returned by value or
is set to the memory location referred by $italic C$$ when $italic C$$ is given.
If matrices had wrong dimensions, the program terminates.


$head Arguments$$
$syntax/
/A/
/$$
is a representation of a $math%iAnr by iAnc%$$ matrix, where iAnr and iAnc are greater than or equal to zero.
When the type of $italic A$$ is $xref/DMclass//DMclass/$$, the dimensions of the matrix object
corresponds to $math%iAnr%$$ and $math%iAnc%$$.  When the type is of $code valarray$$,
it is an array of elements containing the elements of a $math%iAnr by iAnc%$$ matrix in the
column major order.
$syntax/

iAnc
/$$
is the number of columns in $italic A$$.  This argument is required when the type of $italic A$$ object
is $code valarray$$.

$syntax/

/B/
/$$
is a representation of a $math%iBnr by iBnc%$$ matrix, where iBnr and iBnc are greater than or equal to zero.
When the type of $italic B$$ is $xref/DMclass//DMclass/$$, the dimensions of the matrix object
corresponds to $math%iBnr%$$ and $math%iBnc%$$.  When the type is of $code SPK_VA::valarray$$,
it is an array of elements containing the elements of a $math%iBnr by iBnc%$$ matrix in the
column major order.

$syntax/

iBnc
/$$
is the number of columns in $italic B$$.  This argument is required when the type of $italic B$$ object
is $code valarray$$.

$syntax/

/C/
/$$
refers to the memory location in which the result is placed if given.  $italic C$$ must have
$math%m by n%$$ dimensions at the entry.  If not, the program terminates.



$head Example$$
If you compile, link and run the following program
$codep

#include "DoubleMatrix.h"

void main(){

    DoubleMatrix A(2, 2);
    DoubleMatrix B(2, 2);
    DoubleMatrix C;

    double *pA = A.data();
    double *pB = B.data();
    int i;

    // Set A to a matrix:
    // [ 1 3 5 ]
    // [ 2 4 6 ]
    //
    // set B to a matrix:
    // [ 1 ]
    // [ 2 ]
    // [ 3 ]
    //
    for(i=0; i<6; i++)
        pA[i] = i+1;
    for(i=0; i<3; i++)
        pB[i] = i+1;

    // Compute
    // [ 22 ]     [ 1 3 5 ]   [ 1 ]
    //         =            * [ 2 ]
    // [ 28 ]     [ 2 4 6 ]   [ 3 ]
    C = multiply(A, B);
    C.print();

}

$$
the matrix 
$math%
    [ 22 ]
    [ 28 ]
%$$
will be printed.

$end
*/
#include "DoubleMatrix.h"

const DoubleMatrix multiply(const DoubleMatrix &dmatA, const DoubleMatrix &dmatB)
{
    DoubleMatrix dmatC(dmatA.nr(), dmatB.nc());
    multiply(dmatA,dmatB,dmatC);
    return dmatC;
}

void multiply(const DoubleMatrix &dmatA, const DoubleMatrix &dmatB, DoubleMatrix& dmatC)
{
    assert(&dmatA!=&dmatC);
    assert(&dmatB!=&dmatC);
    if( dmatA.isEmpty() || dmatB.isEmpty() )
            return;

    assert( dmatA.nc() == dmatB.nr() );

    int iAnr = dmatB.nc();
    int iAnc = dmatB.nr();
    int iBnr = dmatA.nc();
    int iBnc = dmatA.nr();
    const double *pA = dmatB.data();
    const double *pB = dmatA.data();

    dmatC.resize(dmatA.nr(), dmatB.nc());

    double *pC = dmatC.data();

    /*
     * Since Nag takes matrices stored in an array in the ROW-major
     * order while SPK matrices are stored in column major-order,
     * shuffle arrays a bit.
     */
    NagDgemm('n', 'n', iAnr, iBnc, iAnc, 1.0, pA, pB, 0.0, pC);
}
