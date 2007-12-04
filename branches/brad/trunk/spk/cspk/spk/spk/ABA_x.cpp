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
 * File: ABA_x.cpp
 *
 *
 * Derivative of A' * B * A
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: ABA_x
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin ABA_x$$
$spell 
hessian const iostream namespace std cout endl
$$

$section Derivative of A' * B * A$$

$index ABA_x$$
$index expected hessian, $$

$table
$bold Prototype:$$   $cend  
$syntax/const DoubleMatrix ABA_x(const DoubleMatrix &/A/, const DoubleMatrix& /B/, 
                         const DoubleMatrix &/A_x/, const DoubleMatrix& /B_x/)/$$ $rend
$syntax/void ABA_x(const DoubleMatrix &/A/, const DoubleMatrix& /B/, 
                         const DoubleMatrix &/A_x/, const DoubleMatrix& /B_x/,
                         DoubleMatrix &/C/)/$$ $rend

$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Returns the derivative of 
$math%
     A(x)' * B(x) * A(x)
%$$
where * denotes matrix multiplication and the prime ' denotes transpose. 
The values of A(x) and B(x) for the particular value of x are specified by A and B respectively. 
The derivatives of A(x) and B(x) for the particular value of x are specified by A_x and B_x respectively.
$pre

$$
The resulting matrix has n*n by p dimensions.
$head Arguments$$
$syntax/
/A/
/$$
is a m by n matrix.

$syntax/

/B/
/$$
is a m by m matrix.

$syntax/

/A_x/
/$$
is a m*n by p matrix that represents a value of a derivative of A(x) with respect x.

$syntax/

/B_x/
/$$
is a m*m by p matrix that represents a value of a derivative of B(x) with respect x.

$syntax/

/C/
/$$
is a n*n by p matrix that will hold the resulting matrix value.

$head Example$$
If you compile, link, and run the following program:
$codep
    #include <iostream>
    #include "DoubleMatrix.h"
    #include "ABA_x.h"

    void main()
    {
        using namespace std;

        const double x[] = {1.0, 2.0};

        //
        // Set A to:
        //   [ 0     x[1] ]
        //   [ x[0]  1    ]
        //
        DoubleMatrix A(2,2);
        A.data()[0] = 0;
        A.data()[1] = x[0];
        A.data()[2] = x[1];
        A.data()[3] = 1.0;

        //
        // Set B to:
        //
        //   [ 1     x[0]      ]
        //   [ x[1]  x[1]*x[1] ]
        //
        DoubleMatrix B(2,2);
        B.data()[0] = 1.0;
        B.data()[1] = x[1];
        B.data()[2] = x[0];
        B.data()[3] = x[1]*x[1];

        //
        // Therefore A_x is:
        //
        //    [ 0  0 ]
        //    [ 0  1 ]
        //    [ 1  0 ]
        //    [ 0  0 ]
        //
        DoubleMatrix A_x(4,2);
        A_x.fill(0);
        A_x.data()[2] = 1.0;
        A_x.data()[5] = 1.0;

        //
        // Therefore B_x is:
        //
        //    [ 0      0  ]
        //    [ 1      0  ]
        //    [ 0      1  ]
        //    [ 0  2*x[1] ]
        //
        DoubleMatrix B_x(4,2);
        B_x.fill(0);
        B_x.data()[1] = 1.0;
        B_x.data()[6] = 1.0;
        B_x.data()[7] = 2.0*x[1];

        //
        // C=ABA_x should be:
        //
        //   [ 2 * x[0] * x[1]^2              2 * x[0]^2 * x[1]         ]
        //   [ 2 * x[1]^2                     4 * x[0] * x[1]           ]
        //   [ 2 * x[0] * x[1] + x[1]^2       x[0]^2 + 2 * x[0] * x[1]  ]
        //   [ x[1]                           6 * x[1] + x[0]           ]
        //
        DoubleMatrix expectedC(4,2);
        expectedC.data()[0] = 2.0 * x[0] * x[1]*x[1];
        expectedC.data()[1] = 2.0 * x[1]*x[1];
        expectedC.data()[2] = 2.0 * x[0] * x[1] + x[1]*x[1];
        expectedC.data()[3] = x[1];
        expectedC.data()[4] = 2.0 * x[0]*x[0] * x[1];
        expectedC.data()[5] = 4.0 * x[0] * x[1];
        expectedC.data()[6] = x[0]*x[0] + 2.0 * x[0] * x[1];
        expectedC.data()[7] = 6.0 * x[1] + x[0];

        DoubleMatrix C = ABA_x(A, B, A_x, B_x);

        cout << "C=" << endl;
        C.print();
    }
$$
then it will display the following when it is run:
$codep
    
    C=
    [ 8  4 ]
    [ 8  8 ]
    [ 8  5 ]
    [ 2  13 ]

$$
$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include "DoubleMatrix.h"
#include "identity.h"
#include "transpose.h"
#include "AkronBtimesC.h"
#include "AkronItimesC.h"
#include "IkronBtimesC.h"
#include "add.h"
#include "multiply.h"
#include "transposeDerivative.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

const DoubleMatrix ABA_x(const DoubleMatrix &A, const DoubleMatrix& B, 
                         const DoubleMatrix &A_x, const DoubleMatrix& B_x)
{
    DoubleMatrix Ia   = identity(A.nc());
    DoubleMatrix At   = transpose(A);
    DoubleMatrix At_x = transposeDerivative(A,A_x);

    return add( 
                add( AkronItimesC(multiply(At, B), Ia, A_x), AkronBtimesC(At, At, B_x) ), 
            IkronBtimesC(Ia, multiply(At, transpose(B)), At_x));

}

void ABA_x(const DoubleMatrix &A, const DoubleMatrix& B, 
                         const DoubleMatrix &A_x, const DoubleMatrix& B_x, 
                         DoubleMatrix &C)
{
    DoubleMatrix Ia   = identity(A.nc());
    DoubleMatrix At   = transpose(A);
    DoubleMatrix At_x = transposeDerivative(A,A_x);

    add( 
                add( AkronItimesC(multiply(At, B), Ia, A_x), AkronBtimesC(At, At, B_x) ), 
            IkronBtimesC(Ia, multiply(At, transpose(B)), At_x), C);

}

/*************************************************************************
 *
 * Function: ABA_x (valarray version)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin ABA_xVA$$
$spell 
  hessian 
  const 
  iostream 
  namespace 
  std 
  cout 
  endl
  Spk
  valarray
  Cols
$$

$section Derivative of A' * B * A (valarray version)$$

$index ABA_xVA$$
$index ABA_x, derivative of A' * B * A (valarray version)$$

$table
$bold Prototype:$$   $cend  
$syntax/const SPK_VA::valarray<double> ABA_x(
                         const SPK_VA::valarray<double> &/A/,   int /nColsA/,
                         const SPK_VA::valarray<double>& /B/,   int /nColsB/,
                         const SPK_VA::valarray<double> &/A_x/, 
                         const SPK_VA::valarray<double>& /B_x/,
                         int   /nX/
               )/$$ $rend

$tend
See also: $xref/ABA_x//DoubleMatrix version/$$.

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Returns a vector containing the elements of the derivative of 
$math%
     A(x)' * B(x) * A(x)
%$$
in the column-major order, 
where * denotes matrix multiplication and the prime ' denotes transpose. 
The values of A(x) and B(x) for the particular value of x are specified by A and B respectively. 
The derivatives of A(x) and B(x) for the particular value of x are specified by A_x and B_x respectively.
$pre

$$
The resulting matrix has n*n by p dimensions.
$head Arguments$$
$syntax/
/A/
/$$
is a vector containing a $math%m by n%$$ matrix in the column-major order.

$syntax/

/B/
/$$
is a vector containing a $math%m by m%$$ matrix in the column-major order.

$syntax/

/A_x/
/$$
is a vector containing a $math%m*n by p%$$ matrix, in the column-major order,
that represents a value of a derivative of A(x) with respect x.

$syntax/

/B_x/
/$$
is a vector containing a $math%m*m by p%$$ matrix, in the column-major order,
that represents a value of a derivative of B(x) with respect x.

$syntax/

/C/
/$$
is a vector containing a $math%n*n by p%$$ matrix, in the column-major order,
that will hold the resulting matrix value.

$head Example$$
If you compile, link, and run the following program:
$codep
    #include <iostream>
    #include "SpkValarray.h"
    #include "ABA_x.h"

    void main()
    {
      const int nX = 2;
      const int nRowsA = 2;
      const int nColsA = 2;
      const int nRowsB = 2;
      const int nColsB = 2;

      valarray<double> x(nX);
      x[0] = 1.0;
      x[1] = 2.0;

      //
      // Set A to:
      //   [ 0     x[1] ]
      //   [ x[0]  1    ]
      //
      valarray<double> A( nRowsA * nColsA );
      A[0] = 0;
      A[1] = x[0];
      A[2] = x[1];
      A[3] = 1.0;

      //
      // Set B to:
      //
      //   [ 1     x[0]      ]
      //   [ x[1]  x[1]*x[1] ]
      //
      valarray<double> B( nRowsB * nColsB );
      B[0] = 1.0;
      B[1] = x[1];
      B[2] = x[0];
      B[3] = x[1] * x[1];

      //
      // Therefore A_x is:
      //
      //    [ 0  0 ]
      //    [ 0  1 ]
      //    [ 1  0 ]
      //    [ 0  0 ]
      //
      valarray<double> A_x( (nRowsA * nColsA) * nX );
      A_x = 0.0;
      A_x[2] = 1.0;
      A_x[5] = 1.0;

      //
      // Therefore B_x is:
      //
      //    [ 0      0  ]
      //    [ 1      0  ]
      //    [ 0      1  ]
      //    [ 0  2*x[1] ]
      //
      valarray<double> B_x( (nRowsB * nColsB) * nX );
      B_x = 0.0;
      B_x[1] = 1.0;
      B_x[6] = 1.0;
      B_x[7] = 2.0 * x[1];

      //
      // C=ABA_x should be:
      //
      //   [ 2 * x[0] * x[1]^2              2 * x[0]^2 * x[1]         ]
      //   [ 2 * x[1]^2                     4 * x[0] * x[1]           ]
      //   [ 2 * x[0] * x[1] + x[1]^2       x[0]^2 + 2 * x[0] * x[1]  ]
      //   [ x[1]                           6 * x[1] + x[0]           ]
      //
      valarray<double> expectedC( nRowsA * nRowsB * nX );
      expectedC[0] = 2.0 * x[0] * x[1] * x[1];
      expectedC[1] = 2.0 * x[1] * x[1];
      expectedC[2] = 2.0 * x[0] * x[1] + x[1] * x[1];
      expectedC[3] = x[1];
      expectedC[4] = 2.0 * x[0] * x[0] * x[1];
      expectedC[5] = 4.0 * x[0] * x[1];
      expectedC[6] = x[0] * x[0] + 2.0 * x[0] * x[1];
      expectedC[7] = 6.0 * x[1] + x[0];

      valarray<double> C = ABA_x(A, nColsA, B, nColsB, A_x, B_x, nX);

      cout << "C=" << DoubleMatrix(C,nX) << endl;
    }
$$
then it will display the following when it is run:
$codep
    
    C=
    [ 8  4 ]
    [ 8  8 ]
    [ 8  5 ]
    [ 2  13 ]

$$
$end
*/

#include "SpkValarray.h"

using SPK_VA::valarray;

const valarray<double> ABA_x(
                         const valarray<double>& A, int nColsA,
                         const valarray<double>& B, int nColsB,
                         const valarray<double> &A_x, 
                         const valarray<double>& B_x,
                         int nX
                         )
{
  int nRowsA = A.size() / nColsA;
  assert( nRowsA * nColsA == A.size() );

  int nRowsB = B.size() / nColsB;
  assert( nRowsB * nColsB == B.size() );
  
  valarray<double> Ia( nColsA * nColsA );
  identity( nColsA, Ia );

  valarray<double> At   = transpose( A, nColsA );
  valarray<double> Bt   = transpose( B, nColsB );
  valarray<double> AtB  = multiply( At, nRowsA, B, nColsB );
  valarray<double> AtBt = multiply( At, nRowsA, Bt, nRowsB );
  valarray<double> At_x = transposeDerivative( A_x, nRowsA, nColsA, nX );

  valarray<double> C = AkronItimesC( 
                          AtB,  nColsB,
                          Ia,   nColsA, 
                          A_x,  nX )
                     + AkronBtimesC( 
                          At,   nRowsA, 
                          At,   nRowsA, 
                          B_x,  nX )
                     + IkronBtimesC( 
                          Ia,   nColsA, 
                          AtBt, nRowsB, 
                          At_x, nX );
  return C;
}
