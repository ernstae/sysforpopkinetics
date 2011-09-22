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
/*
 * AkronBtimesC.cpp
 *
 * Kronker product of A with B matrix multiplied by C
 *
 * Author: Sachiko Honda
 */
/*
-----------------------------------------------------------------
     Doublematrix Version of AkronBtimesC
-----------------------------------------------------------------
$begin AkronBtimesC$$

$spell
    Kronker
    Akron
    Btimes
    kron
    int
    dmat
    const
    Ikron Itimes
$$

$section Kronker Product Of A With B Matrix Multiplied by C (DoubleMatrix version)$$

$index AkronBtimesC (DoubleMatrix version)$$
$index kronker product, fast and memory efficient computation of (DoubleMatrix version)$$ 
$index matrix, kronker product (DoubleMatrix version)$$
$index kronker product, saving space and time (DoubleMatrix version)$$

$table
$bold Prototype:$$ $cend 
$syntax/DoubleMatrix AkronBtimesC(
        const DoubleMatrix &/A/, 
        const DoubleMatrix &/B/, 
        const DoubleMatrix &/C/)/$$
$tend

See also: $xref/AkronItimesC//AkronItimesC()/$$, $xref/IkronBtimesC//IkronBtimesC()/$$

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Given $italic A$$ has $math%m%$$ by $math%n%$$ dimensions and 
$italic B$$ has  $math%x%$$ by $math%y%$$ dimensions,  
the kronker product of $italic A$$ and $italic B$$ 
is a $math%m*x%$$ by $math%n*y%$$ matrix $italic S$$ 
such that for each element $math%s(i,j)%$$ in $italic S$$, 
$math%s(i,j) = a(i,j)*B%$$, 
where $math%i%$$ is the row index s.t. $math%m > i >= 0%$$ and 
$math%j%$$ is the column index s.t. $math%n > j >= 0%$$.
$pre

$$
Often the kronker product of two matrices is matrix multiplied by a third matrix.
In this case, when the matrix $italic B$$ is large,
you can save space and time by forming the final product using the statement
$codep
    dmatResult = AkronBtimesC(A, B, C)
$$
instead of the statement
$codep
    dmatResult = kron(A, B) * C
$$
which would both calculate the same result.

$codep
AkronBtimesC = [ a11*B     a12*B  ... ]
               [ a21*B     a22*B  ... ] * C
               [ ...                  ]
             =
               [ a11*B*c1 + a12*B*c2  ...]
               [ a21*B*c1 + a22*B*c2 ... ]
               [ ...                     ]
$$


$head Example$$
If you write, link and run the following program,

$codep

    #include "DoubleMatrix.h"
    #include "AkronBtimesC.h"

    void main(){
      int i;
      DoubleMatrix A( 3, 2 );
      DoubleMatrix B( 2, 3 );
      DoubleMatrix C( 2*3, 2 );
      DoubleMatrix D;

      double *pA = A.data();
      double *pB = B.data();
      double *pC = C.data();

      // Set A to matrix:
      // [ 1 4 ]
      // [ 2 5 ]
      // [ 3 6 ]
      for( i=0; i<3*2; i++ )
         pA[i] = i+1;

      // Set B to matrix:
      // [ 1 3 5 ]
      // [ 2 4 6 ]
      for( i=0; i<2*3; i++ )
         pB[i] = i+1;

      // Set C to matrix:
      // [ 1  7 ]
      // [ 2  8 ]
      // [ 3  9 ]
      // [ 4 10 ]
      // [ 5 11 ]
      // [ 6 12 ]
      for( i=0; i<2*3*2; i++ )
         pC[i] = i+1;

      D = AkronBtimesC( A, B, C );
      D.print(); 
    }
$$

a matrix
$codep
  [ 218  488 ]
  [ 284  644 ]
  [ 289  667 ]
  [ 376  880 ]
  [ 360  846 ]
  [ 468 1116 ]
$$
will be displayed.

$end
-----------------------------------------------------------------
     Function Implementation
-----------------------------------------------------------------
*/

// Updated 2-5-01 Alyssa
// fixed for const correctness

// Updated 3/6/01 rpg
// rearranged to eliminate redim() and reduce allocations and copies

#include <iostream>
#include <string>
#include <cassert>
#include <algorithm>    // for std::copy
#include "AkronBtimesC.h"
#include "transpose.h"
#include "multiply.h"
#include "DoubleMatrix.h"

static DoubleMatrix dmatAt(__FILE__);
static DoubleMatrix dmatCj(__FILE__);
static DoubleMatrix dmatBCj(__FILE__);
static DoubleMatrix dmatBCjAt(__FILE__);

const DoubleMatrix AkronBtimesC(
    const DoubleMatrix &dmatA, 
    const DoubleMatrix &dmatB, 
    const DoubleMatrix &dmatC)
{
    assert( dmatC.nr() == dmatA.nc() * dmatB.nc() );
    const int Cnr = dmatC.nr();
    const int Cnc = dmatC.nc();
    const int Anr = dmatA.nr();
    const int Anc = dmatA.nc();
    const int Bnr = dmatB.nr();
    const int Bnc = dmatB.nc();

    dmatAt.resize(Anc, Anr);
    transpose( dmatA, dmatAt );

    // pCj will point to the start of each column of C in turn
    const double *pCj = dmatC.data();

    // dmatResult will hold the answer.
    DoubleMatrix dmatD(Anr*Bnr, Cnc);
    const int Dnr = dmatD.nr();
    const int Dnc = dmatD.nc();

    // pCj will point to the start of each column of result in turn
    double *pDj = dmatD.data();

    // Construction of temporary matrices is done outside the loop to eliminate
    // redundant memory allocation.
    // COMMENT: To eliminate the copy operations, dmatCj and dmatBCjAt should be
    // wrappers that refer to data held in one column of C and dmatResult
    // respectively. -- rpg 4/6/01
    dmatCj.resize( Bnc, Anc );
    dmatBCj.resize( Bnr, Anc );
    dmatBCjAt.resize( Bnr, Anr );

    for ( int j=0; j<dmatC.nc(); ++j, pCj += Cnr, pDj += Dnr ) {
        // Copy the jth column of C into a B.nc by A.nc matrix Cj
        std::copy( pCj, pCj+Cnr, dmatCj.data() );

        // Each column is the product B*Cj*At, in column major order.
        //
        // 5-15-2001 Sachiko:
        // Switched multiply() routine to one that modifies the 3rd argument for result write
        //
        multiply(dmatB, dmatCj, dmatBCj);
        multiply(dmatBCj, dmatAt, dmatBCjAt);

        // Now copy the B.nr by A.nr result to the jth column of dmatResult
        std::copy( dmatBCjAt.data(), dmatBCjAt.data() + Dnr, pDj );
    }
    return dmatD;
}
/*
-----------------------------------------------------------------
     Function Specification
-----------------------------------------------------------------
$begin AkronBtimesCRef$$

$spell
    Kronker
    Akron
    Btimes
    kron
    int
    dmat
    const
$$

$section Kronker Product Of A With B Matrix Multiplied by C (return result through reference)$$

$index AkronBtimesC (return result through reference)$$
$index kronker product (return result through reference), fast and memory efficient computation of$$ 
$index matrix, kronker product (return result through reference)$$

$table
$bold Prototype:$$ $cend 
$syntax/void AkronBtimesC(
        const DoubleMatrix &/A/, 
        const DoubleMatrix &/B/, 
        const DoubleMatrix &/C/,
        DoubleMatrix &/D/)/$$
$tend

$index kronker product, saving space and time$$

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Computes ($italic A$$ kron $italic B$$) * $italic C$$ and returns the result through
the last argument $italic D$$.  
$pre

$$
Given $italic A$$ has $math%m%$$ by $math%n%$$ dimensions and 
$italic B$$ has  $math%x%$$ by $math%y%$$ dimensions,  
the kronker product of $italic A$$ and $italic B$$ 
is a $math%m*x%$$ by $math%n*y%$$ matrix $italic S$$ 
such that for each element $math%s(i,j)%$$ in $italic S$$, 
$math%s(i,j) = a(i,j)*B%$$, 
where $math%i%$$ is the row index s.t. $math%m > i >= 0%$$ and 
$math%j%$$ is the column index s.t. $math%n > j >= 0%$$.
$pre

$$
Often the kronker product of two matrices is matrix multiplied by a third matrix.
In this case, when the matrix $italic B$$ is large,
you can save space and time by forming the final product using the statement
$codep
    dmatResult = AkronBtimesC(A, B, C)
$$
instead of the statement
$codep
    dmatResult = kron(A, B) * C
$$
which would both calculate the same result.

$codep
AkronBtimesC = [ a11*B     a12*B  ... ]
               [ a21*B     a22*B  ... ] * C
               [ ...                  ]
             =
               [ a11*B*c1 + a12*B*c2  ...]
               [ a21*B*c1 + a22*B*c2 ... ]
               [ ...                     ]
$$


$head Example$$
If you write, link and run the following program,

$codep

    #include "DoubleMatrix.h"
    #include "AkronBtimesC.h"

    void main(){
      int i;
      DoubleMatrix A( 3, 2 );
      DoubleMatrix B( 2, 3 );
      DoubleMatrix C( 2*3, 2 );
      DoubleMatrix D;

      double *pA = A.data();
      double *pB = B.data();
      double *pC = C.data();

      // Set A to matrix:
      // [ 1 4 ]
      // [ 2 5 ]
      // [ 3 6 ]
      for( i=0; i<3*2; i++ )
         pA[i] = i+1;

      // Set B to matrix:
      // [ 1 3 5 ]
      // [ 2 4 6 ]
      for( i=0; i<2*3; i++ )
         pB[i] = i+1;

      // Set C to matrix:
      // [ 1  7 ]
      // [ 2  8 ]
      // [ 3  9 ]
      // [ 4 10 ]
      // [ 5 11 ]
      // [ 6 12 ]
      for( i=0; i<2*3*2; i++ )
         pC[i] = i+1;

      AkronBtimesC( A, B, C, D );
      D.print(); 
    }
$$

a matrix
$codep
  [ 218  488 ]
  [ 284  644 ]
  [ 289  667 ]
  [ 376  880 ]
  [ 360  846 ]
  [ 468 1116 ]
$$
will be displayed.

$end
-----------------------------------------------------------------
     Function Implementation
-----------------------------------------------------------------
*/
void AkronBtimesC(const DoubleMatrix &dmatA, const DoubleMatrix &dmatB, const DoubleMatrix &dmatC, DoubleMatrix &dmatD)
{
    assert( dmatC.nr() == dmatA.nc() * dmatB.nc() );
    const int Cnr = dmatC.nr();
    const int Cnc = dmatC.nc();
    const int Anr = dmatA.nr();
    const int Anc = dmatA.nc();
    const int Bnr = dmatB.nr();
    const int Bnc = dmatB.nc();

    dmatAt.resize(Anc, Anr);
    transpose( dmatA, dmatAt );

    // pCj will point to the start of each column of C in turn
    const double *pCj = dmatC.data();

    // dmatD will hold the answer.
    dmatD.resize(Anr*Bnr, Cnc);
    const int Dnr = dmatD.nr();
    const int Dnc = dmatD.nc();

    // pCj will point to the start of each column of result in turn
    double *pDj = dmatD.data();

    // Construction of temporary matrices is done outside the loop to eliminate
    // redundant memory allocation.
    // COMMENT: To eliminate the copy operations, dmatCj and dmatBCjAt should be
    // wrappers that refer to data held in one column of C and dmatResult
    // respectively. -- rpg 4/6/01
    dmatCj.resize( Bnc, Anc );
    dmatBCj.resize( Bnr, Anc );
    dmatBCjAt.resize( Bnr, Anr );

    for ( int j=0; j<dmatC.nc(); ++j, pCj += Cnr, pDj += Dnr ) {
        // Copy the jth column of C into a B.nc by A.nc matrix Cj
        std::copy( pCj, pCj+Cnr, dmatCj.data() );

        // Each column is the product B*Cj*At, in column major order.
        //
        // 5-15-2001 Sachiko:
        // Switched multiply() routine to one that modifies the 3rd argument for result write
        //
        multiply(dmatB, dmatCj, dmatBCj);
        multiply(dmatBCj, dmatAt, dmatBCjAt);

        // Now copy the B.nr by A.nr result to the jth column of dmatResult
        std::copy( dmatBCjAt.data(), dmatBCjAt.data() + Dnr, pDj );
    }
}

/*
-----------------------------------------------------------------
     Function Specification
-----------------------------------------------------------------
$begin AkronBtimesCVA$$

$spell
    Kronker
    Akron
    Btimes
    kron
    int
    dmat
    const
    Ikron Itimes
    Spk
    valarray
    Cols
    cout
    cvec
    endl
    std
$$

$section Kronker Product Of A With B Matrix Multiplied by C$$

$index AkronBtimesC$$
$index kronker product, fast and memory efficient computation of$$ 
$index matrix, kronker product$$
$index kronker product, saving space and time$$

$table
$bold Prototype:$$ $cend 
$syntax/const SPK_VA::valarray<double AkronBtimesC(
        const SPK_VA::valarray<double> &/a/, int /nColsA/,
        const SPK_VA::valarray<double> &/b/, int /nColsB/,
        const SPK_VA::valarray<double> &/c/, int /nColsC/ 
        )/$$
$tend

See also: $xref/AkronBtimesC//DoubleMatrix version/$$.

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Given a $math%nRowsA%$$ by $math%nColsA%$$ matrix, $math%A%$$, and 
a $math%nRowsB%$$ by $math%nColsB%$$ matrix, $math%B%$$,
the kronker product of $math%A%$$ and $math%B%$$ 
is a $math%nRowsA * nRowsB%$$ by $math%nColsA * nColsB%$$ matrix $math%S%$$ 
such that for each element $math%s(i,j)%$$ in $math%S%$$, 
$math%s(i,j) = a(i,j)*B%$$, 
where $math%i%$$ is the row index s.t. $math%nRowsA > i >= 0%$$ and 
$math%j%$$ is the column index s.t. $math%nColsA > j >= 0%$$.
$pre

$$
Often the kronker product of two matrices is matrix multiplied by a third matrix.
In this case, when the matrix $math%B%$$ is large,
you can save space and time by forming the final product using the statement
$codep
    result = AkronBtimesC( A, nColsA, B, nColsB, C, nColsC )
$$
instead of the statement
$codep
    result = multiply( kron(A, nColsA, B, nColsB), C )
$$
which would both calculate the same result.

$codep
AkronBtimesC = cvec ( [ a11*B     a12*B  ... ]
                      [ a21*B     a22*B  ... ] * C
                      [ ...                  ]
                    )
             =
               [ a11*B*c1 + a12*B*c2  ...]
               [ a21*B*c1 + a22*B*c2 ... ]
               [ ...                     ]
$$

$head Arguments$$
$syntax/

/a/
/$$
is an array containing the values of $math%nRowsA%$$ by $math%nColsA%$$ matrix, $math%A%$$, in column-major order.

$syntax/

/nColsA/
/$$
specifies the number of columns in the matrix $math%A%$$.

$syntax/

/b/
/$$
is an array containing the values of $math%nRowsB%$$ by $math%nColsB%$$ matrix, $math%B%$$, in column-major order.

$syntax/

/nColsB/
/$$
specifies the number of columns in the matrix $math%B%$$.

$syntax/

/c/
/$$
is an array containing the values of $math%nRowsC%$$ by $math%nColsC%$$ matrix, $math%C%$$, in column-major order.

$syntax/

/nColsC/
/$$
specifies the number of columns in the matrix $math%C%$$.


$head Example$$
If you write, link and run the following program,

$codep

    #include "SpkValarray.h"
    #include "AkronBtimesC.h"

    using SPK_VA::valarray;
    void main()
    {
      int i;

      using SPK_VA::valarray;
      using std::cout;
      using std::endl;

      // Set A to a matrix:
      //
      //     /        \
      // A = |  1  4  |
      //     |  2  5  |
      //     |  3  6  |
      //     \        /
      //
      double a[] = { 1, 2, 3, 4, 5, 6 };
      valarray<double> A( a, 6 );

      // Set B to a matrix:
      //
      //     /           \
      // B = |  1  3  5  |
      //     |  2  4  6  |
      //     \           /
      //
      double b[] = { 1, 2, 3, 4, 5, 6 };
      valarray<double> B( b, 6 );

      // Set C to a matrix:
      //     /         \
      // C = |  1   7  |
      //     |  2   8  |
      //     |  3   9  |
      //     |  4  10  |
      //     |  5  11  |
      //     |  6  12  |
      //     \         /
      //
      double c[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 };
      valarray<double> C( c, 12 );

      //
      // This should yield in a (3 * 2) by 2 matrix:
      //
      //     /            \
      // D = |  218  488  |
      //     |  284  644  |
      //     |  289  667  |
      //     |  376  880  |
      //     |  360  846  |
      //     |  468  1116 |
      //     \            /
      //
      valarray<double> D = AkronBtimesC( A, 2, B, 3, C, 2 );
      cout << "D = " << D << endl;
    }
$$

then it will display the following when it is run:
$codep
  D = { 218, 284, 289, 376, 360, 468, 488, 644, 667, 880, 846, 1116 }
$$

$end
-----------------------------------------------------------------
     Function Implementation
-----------------------------------------------------------------
*/
#include "SpkValarray.h"

using SPK_VA::valarray;
using SPK_VA::slice;

const valarray<double> AkronBtimesC(const valarray<double> &A, int nColsA,
                                    const valarray<double> &B, int nColsB,
                                    const valarray<double> &C, int nColsC
                                    )
{
  using namespace std;

  if( nColsA == 0 || nColsB == 0 || nColsC == 0 )
  {
    return valarray<double>(0);
  }

  const int nRowsA = A.size() / nColsA ;
  assert( nRowsA * nColsA == A.size() );

  const int nRowsB = B.size() / nColsB;
  assert( nRowsB * nColsB == B.size() );
  
  const int nRowsC = C.size() / nColsC;
  assert( nRowsC * nColsC == C.size() );

  // This holds the result.
  const int nRowsD = nRowsA * nRowsB;
  const int nColsD = nColsC;
  valarray<double> D( nRowsD * nColsD );

  // Transpose of A.
  valarray<double> At( nColsA * nRowsA ); 
  At = transpose( A, nColsA );

  valarray<double> Cj( nRowsC );
  valarray<double> BCjAt( nRowsD ); 
  valarray<double> BCj( nRowsB * nColsA );

  for( int j = 0; j < nColsC; j++ )
  {
    // REVISIT - Sachiko 06/25/03
    //
    // With g++, the following code does not work:
    //    valarray<double> a;
    //    valarray<double> b( 1.0, 3 );// vector of size 3, filled with 1.0.
    //    a = b;
    // The reference of "b" does not get transfered to "a" properly.
    // So, it's gatta be instead:
    //    valarray<double> a = b;
    // Same as true for valarray = slice assignment.
    //
    Cj = C[ slice( j*nRowsC, nRowsC, 1) ]; // At this point this is a vector.
    assert( Cj.size() == nRowsC );
    // From this pointon, treat Cj as a nColsB by nColsA matrix.
    //cout << "C[" << j << "] = " << DoubleMatrix( Cj, nColsA ) << endl;

    BCj   = multiply( B, nColsB, Cj, nColsA );
    assert( BCj.size() == nRowsB * nColsA );
    // From this pointon, treat Bj as a nRowsB by nColsA matrix.
    //cout << "BCj[" << j << "] = " << DoubleMatrix( BCj, nColsA ) << endl;

    BCjAt = multiply( BCj, nColsA, At, nRowsA );
    assert( BCjAt.size() == nRowsD );
    // From this pointon, treat BCjAt as a nRowsB by nRowsA matrix.
    //cout << "BCjAt[" << j << "] = " << DoubleMatrix( BCjAt, nRowsA ) << endl;

    D[ slice( j * nRowsD, nRowsD, 1 ) ] = BCjAt;
    
  }

  return D;
}
