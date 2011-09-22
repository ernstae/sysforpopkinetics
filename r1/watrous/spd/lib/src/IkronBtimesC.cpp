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
 * File: IkronBtimesC.cpp
 *
 *
 * Kronker product of an IDENTITY matrix with matrix B times C
 *
 * Author: sachiko honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Function: IkronBtimesC (DoubleMatrix version)
 *
 *************************************************************************/
/*
-----------------------------------------------------------------
     Function Specification
-----------------------------------------------------------------
$begin IkronBtimesC$$

$spell
    Kronker
    Ikron
    Btimes
    kron
    int
    dmat
    const
    std
    namespace
    itimes
    ifdef
    endif
    iostream
    ex
    seq
    cout endl
    akron
    Im
$$

$section Kronker Product Of Identity Matrix With B Multiplied by C (DoubleMatrix version)$$

$index IkronBtimesC (DoubleMatrix version)$$
$index kronker product, fast and memory efficient computation of (DoubleMatrix version)$$ 
$index matrix, kronker product (DoubleMatrix version)$$
$index kronker product, saving space and time (DoubleMatrix version)$$
$table
$bold Prototype:$$ $cend 
$syntax/const DoubleMatrix IkronBtimesC(
        const DoubleMatrix &/Im/, 
        const DoubleMatrix &/B/, 
        const DoubleMatrix &/C/)/$$ $rend
$syntax/void IkronBtimesC(
        const DoubleMatrix &/Im/, 
        const DoubleMatrix &/B/, 
        const DoubleMatrix &/C/,
        DoubleMatrix &/D/)/$$

$tend

See also: $xref/AkronBtimesC//AkronBtimesC()/$$, $xref/AkronItimesC//AkronItimesC()/$$

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This is a modified version of $xref/AkronBtimesC//AkronBtimesC(A,B,C)/$$ such that
it takes an identity matrix in place of $italic A$$.
$pre

$$
Often the kronker product of two matrices is matrix multiplied by a third matrix.
In this case, when the first matrix is known to be an identity matrix,
you can save space and time by forming the final product using the statement
$codep
    dmatResult = IkronBtimesC(I, B, C)
$$
instead of the statement
$codep
    dmatResult = kron(I,B) * C
$$
which would both calculate the same result.

$codep
IkronBtimesC = [ a11*I     a12*I  ... ]
               [ a21*I     a22*I  ... ] * C
               [ ...                  ]
             =
               [ a11*I*c1 + a12*I*c2  ...]
               [ a21*I*c1 + a22*I*c2 ... ]
               [ ...                     ]
$$

$head Arguments$$
$syntax/

/Im/
/$$
is a $math%m by m%$$, where m and n are greater than or equal to zero.
$syntax/

/B/
/$$
is a $math%x by y%$$ identity matrix.
$syntax/

/C/
/$$
is a $math%m*x by k%$$ matrix.
$syntax/

/D/
/$$
is a $math%m*x%$$ by $math%n*y%$$ matrix that will contain the resulting matrix value.

$head Example$$
If you write, link and run the following program,

$codep


    #include <iostream>
    #include "DoubleMatrix.h"
    #include "IkronBtimesC.h"
    #include "AkronBtimesC.h"

    void IkronBtimesCTest::IkronBtimesCEx()
    {
        using namespace std;

        const int m = 1;
        const int n = m;
        const int x = 2;
        const int y = 3;
        const int k = 4;
        double seq[] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50};
        DoubleMatrix I = identity(m);
        DoubleMatrix B(x,y);
        DoubleMatrix C(n*y,k);

        std::copy(seq, seq+x*y, B.data());
        std::copy(seq, seq+n*y*k, C.data());

        DoubleMatrix IBC = IkronBtimesC(I,B,C);

        DoubleMatrix ABC = AkronBtimesC(I,B,C);

        cout << "ABC=" << ABC << endl;
        cout << "IBC=" << IBC << endl;
    }

$$

the following matrices will be displayed.

$codep
ABC=2 by 4
[ 1.0000000000000000e+001 2.8000000000000000e+001 4.6000000000000000e+001 6.4000000000000000e+001 ]
[ 1.3000000000000000e+001 4.0000000000000000e+001 6.7000000000000000e+001 9.4000000000000000e+001 ]

IBC=2 by 4
[ 1.0000000000000000e+001 2.8000000000000000e+001 4.6000000000000000e+001 6.4000000000000000e+001 ]
[ 1.3000000000000000e+001 4.0000000000000000e+001 6.7000000000000000e+001 9.4000000000000000e+001 ]
$$
will be displayed.

$end
-----------------------------------------------------------------
     Function Implementation
-----------------------------------------------------------------
*/
#include "IkronBtimesC.h"
#include "DoubleMatrix.h"
#include "multiply.h"
#include "replaceJth.h"
#include "getCol.h"
#include "transpose.h"

const DoubleMatrix IkronBtimesC(
                   const DoubleMatrix& I,
                   const DoubleMatrix& B,
                   const DoubleMatrix& C)
{

    DoubleMatrix D(I.nr()*B.nr(), C.nc());
    IkronBtimesC(I,B,C,D);
    return D;

}

/*
-----------------------------------------------------------------
     Function Specification
-----------------------------------------------------------------
$begin IkronBtimesCVA$$

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
    cvec
    cout
    endl
$$

$section Kronker Product Of Identity Matrix With B  Multiplied by C$$

$index IkronBtimesC$$
$index kronker product, fast and memory efficient computation of$$ 
$index matrix, kronker product$$
$index kronker product, saving space and time$$

$table
$bold Prototype:$$ $cend 
$syntax/const SPK_VA::valarray<double IkronBtimesC(
        const SPK_VA::valarray<double> &/arrayI/, int /nColsI/,
        const SPK_VA::valarray<double> &/arrayB/, int /nColsB/,
        const SPK_VA::valarray<double> &/arrayC/, int /nColsC/ 
        )/$$
$tend

See also: $xref/IkronBtimesC//DoubleMatrix version/$$.

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Given $italic I$$ is a $math%nColsI%$$ identity matrix and 
$italic B$$ is a $math%nRowsB by nColsB%$$ matrix,  
the kronker product of $italic I$$ and $italic B$$ 
is a $math%nRowsI * nRowsB by nColsI * nColsB%$$ matrix $italic S$$ 
such that for each element $math%s(i,j)%$$ in $italic S$$, 
$math%s(i,j) = I(i,j) * B%$$, 
where $math%i%$$ is the row index s.t. $math%nRowsI > i >= 0%$$ and 
$math%j%$$ is the column index s.t. $math%nColsI > j >= 0%$$.
Often the kronker product of two matrices is matrix multiplied by a third matrix.
In this case, when the matrix $italic I$$ is large,
you can save space and time by forming the final product using the statement
$codep
    result = IkronBtimesC(I, nCols B, C)
$$
instead of the statement
$codep
    result = kron(I, B) * C
$$
which would both calculate the same result.

$codep
AkronBtimesC = cvec ( [ 1.0 * B     0.0 * B  ... ]
                      [ 0.0 * B     1.0 * B  ... ] * C
                      [ ...                      ]
                    )
             =
               [ a11*B*c1 ]
               [ a22*B*c2 ]
               [ ...      ]
$$
$head Arguments$$
$syntax/

/I/
/$$
is an array containing the values of the $math%nColsI%$$ identity matrix, 
$math%I%$$, above in column-major order.

$syntax/

/nColsA/
/$$
is the number of columns in the matrix $math%A%$$.

$syntax/

/B/
/$$
is an array containing the values of the $math%nRowsB by nColsB%$$ matrix, 
$math%B%$$, above in column-major order.

$syntax/

/nColsI/
/$$
is the number of columns in the matrix $math%I%$$.

$syntax/

/C/
/$$
is an array containing the values of the $math%nRows * nColsC%$$ matrix, 
$math%C%$$, above in column-major order.

$syntax/

/nColsC/
/$$
is the number of columns in the matrix $math%C%$$.



$head Example$$
If you write, link and run the following program,

$codep

    #include "SpkValarray.h"
    #include "IkronBtimesC.h"

    using SPK_VA::valarray;
    using SPK_VA::slice;

    void main()
    {
      int i;

      // Set A to an identity matrix:
      //
      //     /           \
      // A = |  1  0  0  |
      //     |  0  1  0  |
      //     |  0  0  1  |
      //     \           /
      //
      // 
      valarray<double> I( 0.0, 9 );
      I[ slice( 0, 3, 3+1 ) ] = 1.0;

      // Set B to a matrix:
      // 
      //     /        \
      // B = |  1  4  |
      //     |  2  5  |
      //     |  3  6  |
      //     \        /
      //
      double b[] = { 1, 2, 3, 4, 5, 6 };
      valarray<double> B( b, 6 );

      // Set C to a matrix:
      //
      //     /         \
      // C = |  1  10  |
      //     |  2  11  |
      //     |  3  12  |
      //     |  4  13  |
      //     |  5  14  |
      //     |  6  15  |
      //     |  7  16  |
      //     |  8  17  |
      //     |  9  18  |
      //     \         /
      //
      double c[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18 };
      valarray<double> C( c, 18 );

      //
      // This should yield in a (3 * 3) by 2 matrix:
      // 
      valarray<double> D = IkronBtimesC( I, 3, B, 2, C, 2 );

      cout << "D = " << D << endl;
    }
$$

then it will display the following when it is run:
$codep
  D = { 9, 12, 15, 19, 26, 33, 29, 40, 51, 39, 54, 69, 49, 68, 87, 59, 82, 105 }
$$

$end
-----------------------------------------------------------------
     Function Implementation
-----------------------------------------------------------------
*/
void IkronBtimesC(
                   const DoubleMatrix& I,
                   const DoubleMatrix& B,
                   const DoubleMatrix& C,
                   DoubleMatrix& D)
{

    assert( C.nr() == I.nc() * B.nc() );

    const int Cnr = C.nr();
    const int Cnc = C.nc();
    const int Inr = I.nr();
    const int Inc = I.nc();
    const int Bnr = B.nr();
    const int Bnc = B.nc();

    // pCj will point to the start of each column of C in turn
    const double *pCj = C.data();

    // dmatResult will hold the answer.
    D.resize(Inr*Bnr, Cnc);
    const int Dnr = D.nr();
    const int Dnc = D.nc();

    // pCj will point to the start of each column of result in turn
    double *pDj = D.data();

    // Construction of temporary matrices is done outside the loop to eliminate
    // redundant memory allocation.
    // COMMENT: To eliminate the copy operations, dmatCj and dmatBCjAt should be
    // wrappers that refer to data held in one column of C and dmatResult
    // respectively. -- rpg 4/6/01
    DoubleMatrix Cj( Bnc, Inc );
    DoubleMatrix BCj( Bnr, Inc );
    DoubleMatrix BCjIt( Bnr, Inr );

    for ( int j=0; j<C.nc(); ++j, pCj += Cnr, pDj += Dnr ) {
        // Copy the jth column of C into a B.nc by I.nc matrix Cj
        std::copy( pCj, pCj+C.nr(), Cj.data() );

        // Each column is the product B*Cj*I, in column major order.
        //
        // 5-15-2001 Sachiko:
        // Switched multiply() routine to one that modifies the 3rd argument for result write
        //
        //multiply(B, Cj, BCj);
        //multiply(BCj, It, BCjIt);
        multiply(B, Cj, BCjIt);

        // Now copy the B.nr by I.nr result to the jth column of dmatResult
        std::copy( BCjIt.data(), BCjIt.data() + Dnr, pDj );
    }
}
#include "SpkValarray.h"
using SPK_VA::valarray;
using SPK_VA::slice;

const valarray<double> IkronBtimesC(
                   const valarray<double>& I, int nColsI,
                   const valarray<double>& B, int nColsB,
                   const valarray<double>& C, int nColsC
                   )
{
  using namespace std;

  if( nColsI == 0 || nColsB == 0 || nColsC == 0 )
  {
    return valarray<double>(0);
  }

  const int nRowsI = I.size() / nColsI ;
  assert( nRowsI * nColsI == I.size() );

  const int nRowsB = B.size() / nColsB;
  assert( nRowsB * nColsB == B.size() );
  
  const int nRowsC = C.size() / nColsC;
  assert( nRowsC * nColsC == C.size() );

  // This holds the result.
  const int nRowsD = nRowsI * nRowsB;
  const int nColsD = nColsC;
  valarray<double> D( nRowsD * nColsD );

  // Transpose of I is I

  valarray<double> Cj( nRowsC );
  valarray<double> BCj( nRowsB * nColsI );

  for( int j = 0; j < nColsC; j++ )
  {
    Cj = C[ slice( j*nRowsC, nRowsC, 1) ]; // It this point this is a vector.
    assert( Cj.size() == nRowsC );
    // From this pointon, treat Cj as a nColsB by nColsI matrix.
    //cout << "C[" << j << "] = " << DoubleMatrix( Cj, nColsI ) << endl;

    BCj   = multiply( B, nColsB, Cj, nColsI );
    assert( BCj.size() == nRowsB * nColsI );
    // From this pointon, treat Bj as a nRowsB by nColsI matrix.
    //cout << "BCj[" << j << "] = " << DoubleMatrix( BCj, nColsI ) << endl;

    // BCj * I is BCj

    D[ slice( j * nRowsD, nRowsD, 1 ) ] = BCj;
    
  }

  return D;
}
