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
 * File: transposeDerivative.cpp
 *
 *
 * Transform f_a_b to f_b_a
 *
 * Author: Sachiko Honda (developed this routine as local)
 * Published by Jiaji Du
 *
 *************************************************************************/
/*************************************************************************
 *
 * Function: transposeDerivative
 *
 * 
 * Description
 * -----------
 *
 * Given:
 * x = [ x1, x2 ]^T
 *
 * A(x)   = [ a11  a12 ]    
 *          [ a21  a22 ]
 *          [ a31  a32 ]
 *
 * A(x)_x = [ a11_x1  a11_x2 ]
 *          [ a12_x1  a12_x2 ]
 *          [ a21_x1  a21_x2 ]
 *          [ a22_x1  a22_x2 ]
 *          [ a31_x1  a31_x2 ]
 *          [ a32_x1  a32_x2 ]
 *            
 * A(x)^T_x =
 *          [ a11_x1  a11_x2 ]
 *          [ a21_x1  a21_x2 ]
 *          [ a31_x1  a31_x2 ]
 *          [ a12_x1  a12_x2 ]
 *          [ a22_x1  a22_x2 ]
 *          [ a32_x1  a32_x2 ]
 * 
 *
 *
 * Arguments
 * ---------
 *
 * A      the value of A(x), a matrix to be transposed
 *
 * A_x    The derivative of A(x) with respect to x
 *
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification (DoubleMatrix version)
 *------------------------------------------------------------------------*/

/*
$begin transposeDerivative$$
$spell 
hessian const iostream namespace std cout endl
rvec
ed
Spk
valarray
Sachiko
$$

$section Re-organize Matrix Containing A_x to form A^T_x (DoubleMatrix version)$$

$index transposeDerivative (DoubleMatrix version)$$
$index transpose, derivative  (DoubleMatrix version)$$

  //
  // [ Comment by Sachiko, 10/01/02 ]
  // Taking A as an argument just for the sake of obtaining the dimensions seems
  // incorrect.  I might have done so when this routine was local, which
  // does not require the same level of standard imposed on public interfaces.
  // But, as long as it is published as a public, the interface design
  // should have been thoroughly thought.
  // 

$table
$bold Prototype:$$   $cend  
$syntax/const DoubleMatrix transposeDerivative(const DoubleMatrix &/A/, 
                                               const DoubleMatrix& /A_x/,)/$$ $rend
$tend

See also: $xref/transposeDerivativeVA//valarray version/$$.
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
     A(x)' 
%$$
where the prime ' denotes transpose of the matrix. 

$head Arguments$$

  //
  // [ Comment by Sachiko, 10/01/02 ]
  // This argument should not be part of the interface
  // as long as the soul purpose of this value is to
  // obtain the dimensions of the matrix.
  // It may force the caller to evaluate the function 
  // to get the value when it is not really needed.
  // The function should rather ask for the dimensions directly.
  //
$syntax/
/A/
/$$
is a m by n matrix.

$syntax/
/A_x/
/$$
is a m*n by p matrix that represents a value of a derivative of A(x) with respect to
vector x, where p is the dimension of x.  
   
//
// [ Added by Sachiko, 10/01/02 ]
//
The elements of each column of $italic A_x$$, j-th column,
are assumed to be arranged in the rvec-ed fashion.
$math%
   i.e. A_x = [ rvec(A_x1), rvec(A_x2), ... rvec(A_xp) ]
%$$

$head Example$$
If you compile, link, and run the following program:
$codep
    #include <iostream>
    #include "DoubleMatrix.h"
    #include "transposeDerivative.h"

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
        //
        // C = (A')_x should be:
        //
        //   [ 0  0 ]
        //   [ 1  0 ]
        //   [ 0  1 ]
        //   [ 0  0 ]
        //
 
        DoubleMatrix C = transposeDerivative(A, A_x);

        cout << "C =" << endl;
        C.print();
    }
$$
then it will display the following when it is run:
$codep
    
    C =
    [ 0  0 ]
    [ 1  0 ]
    [ 0  1 ]
    [ 0  0 ]

$$
$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include "transposeDerivative.h"
#include "getCol.h"
#include "transpose.h"
#include "replaceJth.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

const DoubleMatrix transposeDerivative(const DoubleMatrix& A, const DoubleMatrix& A_x)
{
  //
  // [ Comment by Sachiko, 10/01/02 ]
  // Taking A as an argument just for the sake of obtaining the dimensions seems
  // incorrect.  I might have done so when this routine was local, which
  // does not require the same level of standard imposed on public interfaces.
  // But, as long as it is published as a public, the interface design
  // should have been throughly thought.
  // 
    const int nr   = A.nr();
    const int nc   = A.nc();
    const int nx   = A_x.nc();

    DoubleMatrix At_x(nr * nc, nx);
    DoubleMatrix At_j(nr, nc);
    DoubleMatrix A_j;

    //At_x.fill(0);

    for(int j = 0; j < nx; j++)
    {
        A_j  = getCol(A_x, j);
        A_j.resize(nc, nr);
        At_j  = transpose(A_j);
        At_j.resize(nr * nc, 1);
        replaceJth(At_x, j, At_j);
    }
    return At_x;

}
/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin transposeDerivativeVA$$
$spell 
  hessian 
  const 
  iostream 
  namespace 
  std 
  cout 
  endl
  valarray
  Spk
  rvec
  ed
$$

$section Re-organize Matrix Containing A_x to form A^T_x $$

$index transposeDerivative$$
$index transpose, derivative$$

$table
$bold Prototype:$$   $cend  
$syntax/
const SPK_VA::valarray<double> transposeDerivative( const SPK_VA::valarray<double>& /arrayA_x/, int /m/, int /n/, int /p/ )
/$$ $rend
$tend

See also: $xref/transposeDerivative//DoubleMatrix version/$$.
$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Given a (column major) vector-representation of a $math%m * n by p%$$ matrix, $italic A_x$$, 
this function returns another (column major) vector-representation of $math%A'_x%$$
(derivative of the transpose of A (=f(x)) with respect to x).
$pre

$$
Let $math%f(x)_x%$$ denote a derivative of a matrix valued function, $math%f(x)%$$, 
with respect to a parameter vector, $math%x%$$.
Further, let $italic A_x$$ be the matrix value resulted in evaluating $math%f(x)_x%$$.
The elements of $italic A_x$$ are assumed to be organized in the way 
that each column holds the $xref/rvecVA//rvec/$$ of a matrix.
$math%
i.e.
  
   A_x  = [ rvec(A_x(1)),  rvec(A_x(2)),  ... rvec(A_x(p))  ]
  
   rvec(A_x(k)) = rvec( /                                     \ )
                        | a[k](1,1), a[k](1,2), ... a[k](1,n) |
                        | a[k](2,1), a[k](2,2), ... a[k](2,n) |
                        | ...                                 |
                        | a[k](m,1), a[k](m,2), ... a[k](m,n) |
                        \                                     / 
  
                = /           \
                  | a[k](1,1) |
                  | a[k](1,2) |
                  | ...       |
                  | a[k](1,n) |
                  -------------
                  | a[k](2,1) |
                  | a[k](2,2) |
                  | ...       |
                  | a[k](2,n) |
                  -------------
                  | ...       |
                  -------------
                  | a[k](m,1) |
                  | a[k](m,2) |
                  | ...       |
                  | a[k](m,n) |
                  \           /
  
   where 1 < k <= p.
%$$

$math%
   A'_x = [ rvec(A'_x(1)), rvec(A'_x(2)), ... rvec(A'_x(nX)) ]
  
   rvec(A'_x(k)) = rvec( /                                     \ ')
                         | a[k](1,1), a[k](1,2), ... a[k](1,n) |
                         | a[k](2,1), a[k](2,2), ... a[k](2,n) |
                         | ...                                 |
                         | a[k](m,1), a[k](m,2), ... a[k](m,n) |
                         \                                     / 
  
                 = rvec( /                                     \ )
                         | a[k](1,1), a[k](2,1), ... a[k](m,1) |
                         | a[k](1,2), a[k](2,2), ... a[k](m,2) |
                         | ...                                 |
                         | a[k](1,n), a[k](2,n), ... a[k](m,n) |
                         \                                     / 
  
                 = /           \
                   | a[k](1,1) |
                   | a[k](2,2) |
                   | ...       |
                   | a[k](m,1) |
                   -------------
                   | a[k](1,2) |
                   | a[k](2,2) |
                   | ...       |
                   | a[k](m,2) |
                   -------------
                   | ...       |
                   -------------
                   | a[k](1,n) |
                   | a[k](2,n) |
                   | ...       |
                   | a[k](m,n) |
                   \           /
  
%$$

$head Arguments$$
$syntax/
/arrayA_x/
/$$
is an array containing the values of a $math%m * n by p%$$ matrix, $italic A_x$$
in column-major order.
Each column of $italic A_x$$ is assumed to be a $xref/rvecVA//rvec/$$-ed matrix.
   
$syntax/

/m/
/$$
specifies the row dimension of each matrix contained in $math%A_x%$$.

$syntax/

/n/
/$$
specifies the column dimension of each matrix contained in $math%A_x%$$.

$syntax/

/p/
/$$
specifies the number of rvec-ed matrices in $math%A_x%$$.
  
$head Example$$
If you compile, link, and run the following program:
$codep
    #include <iostream>
    #include "SpkValarray.h"
    #include "transposeDerivative.h"

    using SPK_VA::valarray;

    void main()
    {
        using namespace std;

        valarray<double> x(2);
        x[0] = 1.0;
        x[1] = 2.0;

        //
        // Set A_x to a matrix:
        //
        //       /        \
        // A_x = |  0  0  |
        //       |  0  1  |
        //       |  1  0  |
        //       |  0  0  |
        //       \        /
        //
        double a_x[] = { 0, 0, 1, 0, 0, 1, 0, 0 };
        valarray<double> A_x(a_x, 8);

        //
        //
        // A(x)'_x will be:
        //
        //   /        \
        //   |  0  0  |
        //   |  1  0  |
        //   |  0  1  |
        //   |  0  0  |
        //   \        /
        // 
        valarray<double> C = transposeDerivative(A_x, 4, 2, 2);

        cout << "C = " << C << endl;
    }
$$
then it will display the following when it is run:
$codep
    
    C = { 0, 1, 0, 0, 0, 0, 1, 0 }

$$
$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include "SpkValarray.h"

using SPK_VA::valarray;

const valarray<double> transposeDerivative(const valarray<double>& A_x, int m, int n, int p)
{
  //
  // A_x  = [ rvec(A_x(1)),  rvec(A_x(2)),  ... rvec(A_x(p))  ]
  //
  // rvec(A_x(k)) = rvec( /                                     \ )
  //                      | a[k](1,1), a[k](1,2), ... a[k](1,n) |
  //                      | a[k](2,1), a[k](2,2), ... a[k](2,n) |
  //                      | ...                                 |
  //                      | a[k](m,1), a[k](m,2), ... a[k](m,n) |
  //                      \                                     / 
  //
  //              = /           \
  //                | a[k](1,1) |
  //                | a[k](1,2) |
  //                | ...       |
  //                | a[k](1,n) |
  //                -------------
  //                | a[k](2,1) |
  //                | a[k](2,2) |
  //                | ...       |
  //                | a[k](2,n) |
  //                -------------
  //                | ...       |
  //                -------------
  //                | a[k](m,1) |
  //                | a[k](m,2) |
  //                | ...       |
  //                | a[k](m,n) |
  //                \           /
  //
  // where 1 < k <= p.
  //

  // A'_x = [ rvec(A'_x(1)), rvec(A'_x(2)), ... rvec(A'_x(nX)) ]
  //
  // rvec(A'_x(k)) = rvec( /                                     \ ')
  //                       | a[k](1,1), a[k](1,2), ... a[k](1,n) |
  //                       | a[k](2,1), a[k](2,2), ... a[k](2,n) |
  //                       | ...                                 |
  //                       | a[k](m,1), a[k](m,2), ... a[k](m,n) |
  //                       \                                     / 
  //
  //               = rvec( /                                     \ )
  //                       | a[k](1,1), a[k](2,1), ... a[k](m,1) |
  //                       | a[k](1,2), a[k](2,2), ... a[k](m,2) |
  //                       | ...                                 |
  //                       | a[k](1,n), a[k](2,n), ... a[k](m,n) |
  //                       \                                     / 
  //
  //               = /           \
  //                 | a[k](1,1) |
  //                 | a[k](2,2) |
  //                 | ...       |
  //                 | a[k](m,1) |
  //                 -------------
  //                 | a[k](1,2) |
  //                 | a[k](2,2) |
  //                 | ...       |
  //                 | a[k](m,2) |
  //                 -------------
  //                 | ...       |
  //                 -------------
  //                 | a[k](1,n) |
  //                 | a[k](2,n) |
  //                 | ...       |
  //                 | a[k](m,n) |
  //                 \           /
  //
  valarray<double> At_x(m * n * p);
  for( int k_i=0; k_i<p; k_i++ )
  {
    for( int n_i=0; n_i<n; n_i++ )
    {
      for( int m_i=0; m_i<m; m_i++ )
      {
        At_x[k_i * (m * n) + (m_i + m * n_i) ] = A_x[ k_i * (m * n) + (n_i + n * m_i) ];
      }
    }
  }
  return At_x;
}

