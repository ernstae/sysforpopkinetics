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
/************************************************************************
 * transpose.cpp
 *
 * Matrix transpose
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*
$begin transpose$$

$spell 
  const 
  dmat 
  int
  valarray
$$

$section Transpose of a matrix (DoubleMatrix version)$$

$index matrix, transpose (DoubleMatrix version)$$
$index transpose (DoubleMatrix version)$$

$table
$bold Prototype:$$ $cend
$syntax/const DoubleMatrix transpose( const DoubleMatrix &/A/ )/$$ $rend
$tend
See also: $xref/transposeVA//Valarray version/$$, $xref/transposeRef//Return-by-ref version/$$.
$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Return the transpose of a matrix $italic A$$.
$pre

$$
$head Return Value$$
Given a $math%m%$$ by $math%n%$$ matrix $italic A$$, 
$syntax/transpose(/A/)/$$ returns a $math%n%$$ by 
$math%m%$$ matrix $italic B$$, such that 
$math%(b(i,j) in B) = (a(j,i) in A)%$$, where 
$math%m > i \ge 0%$$ and $math%n > j \ge 0%$$. 

$head Arguments$$
$syntax/
/A/
/$$
contains the value of a $math%m%$$ by $math%n%$$ matrix, where $math%m%$$ and $math%n%$$ are greater
than zero.

$head Example$$
If you compile, link, and run the following program,
$codep

#include "DoubleMatrix.h"
#include "transpose.h"

void main(){
  
  DoubleMatrix A(3, 2);
  double *pA = A.data();
  int i;

  // Set A to the matrix
  // [ 1 4 ]
  // [ 2 5 ]
  // [ 3 6 ]
  for(i=0; i<6; i++)
    pA[i] = i+1;

  // Compute
  // [1 2 3] = [1 4]^T
  // [4 5 6]   [2 5]
  //           [3 6]
  DoubleMatrix B = transpose(A));
  B.print();
}
$$
the matrix
$math%
   [1 2 3]
   [4 5 6]
%$$
will be printed.
$end
---------------------------------------------------------------------------
*/
#include <iostream>
#include <cassert>
#include "transpose.h"
using std::cout;

const DoubleMatrix transpose(const DoubleMatrix &dmatA)
{
  DoubleMatrix dmatB(dmatA.nc(), dmatA.nr());
  transpose(dmatA, dmatB);
  return dmatB;
}
/*---------------------------------------------------------------------------
$begin transposeRef$$

$spell 
   const 
   dmat 
   int
   valarray
$$

$section Transpose of a matrix (return result through reference)$$

$index matrix, transpose (return result through reference)$$
$index transpose$$

$table
$bold Prototype:$$ $cend
$syntax/void transpose( const DoubleMatrix &/A/, DoubleMatrix &/B/)/$$ $rend
$tend
See also: $xref/transposeVA//Valarray version/$$, $xref/transpose//Return-by-val version/$$.

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Returns the transpose of a matrix $italic A$$ through $italic B$$.

$head Return Value$$
Given a $math%m%$$ by $math%n%$$ matrix $italic A$$, 
$syntax/transpose(/A/)/$$ returns a $math%n%$$ by 
$math%m%$$ matrix $italic B$$, such that 
$math%(b(i,j) in B) = (a(j,i) in A)%$$, where 
$math%m > i \ge 0%$$ and $math%n > j \ge 0%$$. 

$head Arguments$$
$syntax/
/A/
/$$
contains the value of a $math%m%$$ by $math%n%$$ matrix, where $math%m%$$ and $math%n%$$ are greater
than zero.

$syntax/

/B/
/$$
will contain the values of the transpose of $italic A$$.

$head Example$$
If you compile, link, and run the following program,
$codep

#include "DoubleMatrix.h"
#include "transpose.h"

void main(){
  
  DoubleMatrix A(3, 2);
  double *pA = A.data();
  int i;

  // Set A to the matrix
  // [ 1 4 ]
  // [ 2 5 ]
  // [ 3 6 ]
  for(i=0; i<6; i++)
    pA[i] = i+1;

  // Compute
  // [1 2 3] = [1 4]^T
  // [4 5 6]   [2 5]
  //           [3 6]
  DoubleMatrix B(2,3);
  transpose(A,B);
  b.print();
}
$$
the matrix
$math%
   [1 2 3]
   [4 5 6]
%$$
will be printed.
$end
---------------------------------------------------------------------------
*/
void transpose(const DoubleMatrix& A, DoubleMatrix& B)
{
    int nAr = A.nr();
    int nAc = A.nc();
    B.resize(nAc,nAr);
    const double *pA = A.data();
    double *pB       = B.data();

    for( int j=0; j<nAc; j++ )
    {
        for( int i=0; i<nAr; i++ )
        {
            pB[i*nAc + j] = pA[j*nAr + i];
        }
    }
}
/*
$begin transposeVA$$

$spell 
  const 
  dmat 
  int 
  valarray 
  SPK_VA 
  Spk
  cout
  namespace
  std
  nd
  endl
$$

$section Transpose of Matrix$$

$index matrix, Transpose of Matrix$$
$index transpose$$

$table
$bold Prototype:$$ $cend
$syntax/const SPK_VA::valarray<double> transpose( const SPK_VA::valarray<double> &/a/, int &/n/ )/$$ $rend
$tend
See also: $xref/transpose//DoubleMatrix version/$$.

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Returns the transpose of a matrix.

$head Return Value$$
Given a $math%m%$$ by $math%n%$$ matrix $math%A%$$,
this function returns an array containing the values of
the transpose of $math%A%$$, a $math%n%$$ by $math%m%$$ matrix $math%B%$$ in column-major order.

$head Arguments$$
$syntax/
/a/
/$$
is an array containing the value of a $math%m%$$ by $math%n%$$ matrix, $math%A%$$, 
in column-major order, where $math%m%$$ and $math%n%$$ are greater than or equal to zero.

$syntax/

/n/
/$$
is the number of columns in the matrix contained in the array $math%A%$$.
The value must be greater than zero if $math%A%$$ is not empty.

$head Example$$
If you compile, link, and run the following program,
$codep

#include "SpkValarray.h"
#include "transpose.h"

void main()
{
  using SPK_VA::valarray;
  using namespace std;
  
  int i;

  // Set A to a 3 by 2 matrix:
  //     /        \
  // A = |  1  4  |
  //     |  2  5  |
  //     |  3  6  |
  //     \        /
  //
  double a[] = { 1, 2, 3, 4, 5, 6 };
  valarray<double> A(a, 3 * 2);

  // Compute:
  //      /           \
  // A' = |  1  2  3  |
  //      |  4  5  6  |
  //      \           /
  //
  valarray<double> aT = transpose(a, 2);   // 2 is the number of column in A.
  cout << "A^T = " << aT << endl;
}
$$
a vector:
$codep
   A^T = {1, 4, 2, 5, 3, 6 }
$$
will be printed.
$end
---------------------------------------------------------------------------
*/
using SPK_VA::valarray;
using SPK_VA::gslice;

const valarray<double> transpose( const valarray<double> A, int iAnc )
{
    if( A.size() == 0 )
        return A;

    assert( iAnc > 0 );
    int iAnr = A.size() / iAnc;
    assert( iAnr * iAnc == A.size() );

    size_t len[] =  { iAnr, iAnc };
    size_t str[] =  { 1,    iAnr };
    valarray<size_t> lengths(len, 2);
    valarray<size_t> strides(str, 2);
    return A[ gslice(0, lengths, strides) ];
}

