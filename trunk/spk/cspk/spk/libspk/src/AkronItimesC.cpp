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
 * File: AkronItimesC.cpp
 *
 *
 * Kronker product of a matrix A with an IDENTITY matrix times C
 *
 * Author: sachiko honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Function: AkronItimesC (DoubleMatrix version)
 *
 *************************************************************************/
/*
-----------------------------------------------------------------
     Function Specification
-----------------------------------------------------------------
$begin AkronItimesC$$

$spell
    Kronker
    Akron
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
    cout endl Ikron
$$

$section Kronker Product Of A With Identity Matrix Multiplied by C (DoubleMatrix version)$$

$index AkronItimesC (DoubleMatrix version)$$
$index kronker product, fast and memory efficient computation of (DoubleMatrix version)$$ 
$index matrix, kronker product (DoubleMatrix version)$$
$index kronker product, saving space and time (DoubleMatrix version)$$

$table
$bold Prototype:$$ $cend 
$syntax/const DoubleMatrix AkronItimesC(
        const DoubleMatrix &/A/, 
        const DoubleMatrix &/I/, 
        const DoubleMatrix &/C/)/$$ $rend
$syntax/void AkronItimesC(
        const DoubleMatrix &/A/, 
        const DoubleMatrix &/I/, 
        const DoubleMatrix &/C/,
        DoubleMatrix &/D/)/$$ $rend
$tend


See also: $xref/AkronBtimesC//AkronBtimesC()/$$, $xref/IkronBtimesC//IkronBtimesC()/$$

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
it takes an identity matrix in place of $italic B$$.
$pre

$$
Often the kronker product of two matrices is matrix multiplied by a third matrix.
In this case, when the second matrix is known to be an identity matrix,
you can save space and time by forming the final product using the statement
$codep
    dmatResult = AkronItimesC(A, I, C)
$$
instead of the statement
$codep
    dmatResult = kron(A, I) * C
$$
which would both calculate the same result.

$codep
AkronItimesC = [ a11*I     a12*I  ... ]
               [ a21*I     a22*I  ... ] * C
               [ ...                  ]
             =
               [ a11*I*c1 + a12*I*c2  ...]
               [ a21*I*c1 + a22*I*c2 ... ]
               [ ...                     ]
$$

$head Arguments$$
$syntax/

/A/
/$$
is a $math%m by n%$$, where m and n are greater than or equal to zero.
$syntax/

/I/
/$$
is a $math%x by y%$$ identity matrix, where x is equal to y.
$syntax/

/C/
/$$
is a $math%m*x by k%$$ matrix, where k is greater than or equal to zero.
$syntax/

/D/
/$$
is a $math%m*x%$$ by $math%n*y%$$ matrix that will contain the resulting matrix value.

$head Example$$
If you write, link and run the following program,

$codep

*/
#ifdef DOCEXAMPLE

    #include <iostream>
    #include "DoubleMatrix.h"
    #include "AkronItimesC.h"
    #include "AkronBtimesC.h"

    void AkronItimesCEx()
    {
        using namespace std;

        const int m = 1;
        const int n = 2;
        const int x = 3;
        const int y = x;
        const int k = 4;
        int i;
        double seq[] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
        31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50};
        DoubleMatrix A(m,n);
        DoubleMatrix I = identity(x);
        DoubleMatrix C(n*y,k);

        std::copy(seq, seq+m*n, A.data());
        std::copy(seq, seq+n*y*k, C.data());

        DoubleMatrix AIC = AkronItimesC(A,I,C);

        DoubleMatrix ABC = AkronBtimesC(A,I,C);
        cout << "ABC=" << ABC << endl;
        cout << "AIC=" << AIC << endl;
    }

#endif 
/*
$$

the following matrices will be displayed.

$codep
    ABC=3 by 4
    [ 3.0000000000000000e+000 9.0000000000000000e+000 1.5000000000000000e+001 2.1000    000000000000e+001 ]
    [ 4.0000000000000000e+000 1.0000000000000000e+001 1.6000000000000000e+001 2.2000    000000000000e+001 ]
    [ 5.0000000000000000e+000 1.1000000000000000e+001 1.7000000000000000e+001 2.3000    000000000000e+001 ]

    AIC=3 by 4
    [ 3.0000000000000000e+000 9.0000000000000000e+000 1.5000000000000000e+001 2.1000    000000000000e+001 ]
    [ 4.0000000000000000e+000 1.0000000000000000e+001 1.6000000000000000e+001 2.2000    000000000000e+001 ]
    [ 5.0000000000000000e+000 1.1000000000000000e+001 1.7000000000000000e+001 2.3000    000000000000e+001 ]$$
will be displayed.

$end
-----------------------------------------------------------------
     Function Implementation
-----------------------------------------------------------------
*/

#include "DoubleMatrix.h"
#include "transpose.h"
#include "multiply.h"
#include "getCol.h"
#include "replaceJth.h"
#include "AkronItimesC.h"

const DoubleMatrix AkronItimesC(
    const DoubleMatrix &A, 
    const DoubleMatrix &I, 
    const DoubleMatrix &C)
{

    DoubleMatrix D(A.nr()*I.nr(), C.nc());
    AkronItimesC(A,I,C,D);
    return D;
}

void AkronItimesC(
    const DoubleMatrix &A, 
    const DoubleMatrix &I, 
    const DoubleMatrix &C,
    DoubleMatrix &D)
{
    const int Anc = A.nc();
    const int Anr = A.nr();
    const int Inc = I.nc();
    const int Inr = I.nr();
    assert( C.nr() == Anc * Inc );
    const int Cnr = C.nr();
    const int Cnc = C.nc();

    DoubleMatrix dmatAt(Anc, Anr);
    transpose( A, dmatAt );

    // pCj will point to the start of each column of C in turn
    const double *pCj = C.data();

    // D will hold the answer.
    D.resize(Anr*Inr, Cnc);
    const int Dnr = D.nr();
    const int Dnc = D.nc();

    // pCj will point to the start of each column of result in turn
    double *pDj = D.data();

    // Construction of temporary matrices is done outside the loop to eliminate
    // redundant memory allocation.
    // COMMENT: To eliminate the copy operations, Cj and BCjAt should be
    // wrappers that refer to data held in one column of C and dmatResult
    // respectively. -- rpg 4/6/01
    DoubleMatrix Cj( Inc, Anc );
    DoubleMatrix ICj( Inr, Anc );
    DoubleMatrix BCjAt( Inr, Anr );

    for ( int j=0; j<Cnc; ++j, pCj += Cnr, pDj += Dnr ) {
        // Copy the jth column of C into a I.nc by A.nc matrix Cj
        std::copy( pCj, pCj+Cnr, Cj.data() );

        // Each column is the product I*Cj*At, in column major order.
        //
        // 5-15-2001 Sachiko:
        // Switched multiply() routine to one that modifies the 3rd argument for result write
        //
        //multiply(I, Cj, ICj);
        //multiply(ICj, dmatAt, BCjAt);
        multiply(Cj, dmatAt, BCjAt);

        // Now copy the I.nr by A.nr result to the jth column of dmatResult
        std::copy( BCjAt.data(), BCjAt.data() + Dnr, pDj );
    }
}

/*
-----------------------------------------------------------------
     Function Specification
-----------------------------------------------------------------
$begin AkronItimesCVA$$

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

$section Kronker Product Of A With Identity Matrix Multiplied by C$$

$index AkronItimesC$$
$index kronker product, fast and memory efficient computation of$$ 
$index matrix, kronker product$$
$index kronker product, saving space and time$$

$table
$bold Prototype:$$ $cend 
$syntax/const SPK_VA::valarray<double AkronItimesC(
        const SPK_VA::valarray<double> &/arrayA/, int /nColsA/,
        const SPK_VA::valarray<double> &/arrayI/, int /nColsI/,
        const SPK_VA::valarray<double> &/arrayC/, int /nColsC/ 
        )/$$
$tend

See also: $xref/AkronItimesC//DoubleMatrix version/$$.

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Given $italic A$$ has $math%nRowsA by nColsA%$$ dimensions and 
$italic I$$ is a $math%nColsI%$$ identity matrix,  
the kronker product of $italic A$$ and $italic I$$ 
is a $math%nRowsA * nRowsI by nColsA * nColsI%$$ matrix $italic S$$ 
such that for each element $math%s(i,j)%$$ in $italic S$$, 
$math%s(i,j) = a(i,j)*B%$$, 
where $math%i%$$ is the row index s.t. $math%nRowsA > i >= 0%$$ and 
$math%j%$$ is the column index s.t. $math%nColsA > j >= 0%$$.
$pre

$$
Often the kronker product of two matrices is matrix multiplied by a third matrix.
In this case, when the matrix $italic I$$ is large,
you can save space and time by forming the final product using the statement
$codep
    result = AkronItimesC( A, nColsA, I, nColsI, C, nColsC )
$$
instead of the statement
$codep
    result = multiply( kron( A, nColsA, I, nColsI ), C, nColsC )
$$
which would both calculate the same result.

$codep
AkronBtimesC = cvec ( [ a11*I     a12*I  ... ]
                      [ a21*I     a22*I  ... ] * C
                      [ ...                  ]
                    )
             =
               [ a11*I*c1 + a12*I*c2  ...]
               [ a21*I*c1 + a22*I*c2 ... ]
               [ ...                     ]
$$

$head Arguments$$
$syntax/

/A/
/$$
is an array containing the values of the $math%nRowsA by nColsA%$$ matrix, 
$math%A%$$, above in column-major order.

$syntax/

/nColsA/
/$$
is the number of columns in the matrix $math%A%$$.

$syntax/

/I/
/$$
is an array containing the values of the $math%nColsI%$$ identity matrix, 
$math%I%$$, above in column-major order.

$syntax/

/nColsI/
/$$
is the number of columns in the matrix $math%I%$$.

$syntax/

/C/
/$$
is an array containing the values of the $math%nRowsC by nColsC%$$ matrix, 
$math%C%$$, above in column-major order.

$syntax/

/nColsC/
/$$
is the number of columns in the matrix $math%C%$$.


$head Example$$
If you write, link and run the following program,

$codep

    #include "SpkValarray.h"
    #include "AkronItimesC.h"

    void main()
    {
      using SPK_VA::valarray;
      using std::cout;
      using std::endl;

      int i;
      valarray<double> D;

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

      // Set I be a identity matrix:
      //     /           \
      // I = |  1  0  0  |
      //     |  0  1  0  |
      //     |  0  0  1  |
      //     \           /
      //
      valarray<double> I( 0.0, 9 );
      I[ slice( 0, 3, 3+1 ) ] = 1.0;

      // Let C be a matrix:
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
      // This should yield in a (3 * 3) by 2 matrix:
      //
      //     /                          \
      // D = |  17  22  27  47  64  81  |
      //     |  22  29  36  52  71  90  |
      //     |  27  36  45  57  78  99  |
      //     \                          /
      //
      D = AkronItimesC( A, 2, I, 3, C, 2 );
      cout << "D = " << D << endl;
    }
$$

then it will display the following when it is run:
$codep
  D = { 17, 22, 27, 22, 29, 36, 27, 36, 45, 47, 52, 57, 64, 71, 78, 81, 90, 99 }
$$

$end
-----------------------------------------------------------------
     Function Implementation
-----------------------------------------------------------------
*/
#include "SpkValarray.h"

using SPK_VA::valarray;
using SPK_VA::slice;

const valarray<double> AkronItimesC(const valarray<double> &A, int nColsA,
                                    const valarray<double> &I, int nICols,
                                    const valarray<double> &C, int nColsC
                                    )
{
  using namespace std;

  if( nColsA == 0 || nICols == 0 || nColsC == 0 )
  {
    return valarray<double>(0);
  }

  const int nRowsA = A.size() / nColsA ;
  assert( nRowsA * nColsA == A.size() );

  const int nRowsI = I.size() / nICols;
  assert( nRowsI * nICols == I.size() );
  
  const int nRowsC = C.size() / nColsC;
  assert( nRowsC * nColsC == C.size() );

  // This holds the result.
  const int nRowsD = nRowsA * nRowsI;
  const int nColsD = nColsC;
  valarray<double> D( nRowsD * nColsD );

  // Transpose of A.
  valarray<double> At( nColsA * nRowsA ); 
  At = transpose( A, nColsA );

  valarray<double> Cj( nRowsC );
  valarray<double> ICjAt( nRowsD );

  for( int j = 0; j < nColsC; j++ )
  {
    Cj = C[ slice( j*nRowsC, nRowsC, 1) ]; // At this point this is a vector.
    assert( Cj.size() == nRowsC );
    // From this point on, treat Cj as a nICols by nColsA matrix.
    //cout << "C[" << j << "] = " << DoubleMatrix( Cj, nColsA ) << endl;

    // I * Cj is Cj
    ICjAt = multiply( Cj, nColsA, At, nRowsA );
    assert( ICjAt.size() == nRowsD );
    // From this point on, treat ICjAt as a nRowsI by nRowsA matrix.
    //cout << "ICjAt[" << j << "] = " << DoubleMatrix( ICjAt, nRowsA ) << endl;

    D[ slice( j * nRowsD, nRowsD, 1 ) ] = ICjAt;
    
  }

  return D;
}
