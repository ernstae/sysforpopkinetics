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
 * matmax.cpp
 *
 * Definitions of double matmax(DoubleMatrix&) and
 * DoubleMatrix matmax(DoubleMatrix&, DoubleMatrix).
 *
 * Author: Sachiko Honda
 */
/*
-----------------------------------------------------------
   Common
-----------------------------------------------------------
*/
#include <cassert>
#include <cmath>
#include "DoubleMatrix.h"

/*
-----------------------------------------------------------
   double matmax( DoubleMatrix & ) Function specification
-----------------------------------------------------------
$begin matmaxScalar$$

$spell dmat cout endl int pd const th namespace iostream matmax max mat matmin matabs
std nr nc$$
$section Find the maximum value$$

$index matrix, maximum value$$
$index max$$
$index matmax$$

$table
$bold Prototype:$$ $cend
$syntax/double matmax(const DoubleMatrix &/A/)/$$
$tend

See also: $xref/matmaxMatrices//const DoubleMatrix matmax(const DoubleMatrix&, const DoubleMatrix&)/$$.
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Given a $math%m%$$ by $math%n%$$ matrix $italic A$$, 
$code matmax()$$ returns a double
precision number $math%x%$$ such that $math%x > (a(i,j) in A)%$$
if $math%x != a(i,j)%$$ or else $math%x \ge a(i,j)%$$ if 
$math%x = a(i,j)%$$, for $math%m > i > 0%$$ and $math%n > j > 0%$$.
$pre

$$
Empty matrix is forbidden since there is no way returning empty/null double-precision scalar.

$head Example$$
If you compile, link and run the following program, 
$codep
    #include <iostream>
    #include "matmax.h"

    void main(){

      using namespace std;

      DoubleMatrix dmatA(3,2);
      double       *pdA    = dmatA.data(),
                   largest = 0.0;

      // Setting A to a matrix:
      //    [  1  -4 ]
      //    [ -2   5 ]
      //    [  3  -6 ]
      for( int i=0; i<dmatA.nr()*dmatA.nc(); i++ ){
          pdA[i] = (i+1)*(i%2 == 0.0? 1.0: -1.0);
      }

      largest = matmax(dmatA);

      cout << "Original matrix A" << endl;
      dmatA.print();

      cout << "The largest value found in A is: " << largest << endl;
    }

$$
the following results are displayed.

$codep
    Original matrix A
    [1, -4]
    [-2, 5]
    [3, -6]
    The largest value found in A is: 5

  
$$
$end

// Updated 2-5-01 Alyssa
// fixed for const correctness

-----------------------------------------------------------
   Function implementation
-----------------------------------------------------------
*/

double matmax( const DoubleMatrix &A )
{
    assert(!A.isEmpty());

    int iRows = A.nr();
    int iCols = A.nc();
    int n = iRows*iCols;
    const double *pdA = A.data();
    
    double maxsofar = pdA[0];

    for( int i=1; i<n; i++ )
    {
        if( pdA[i] > maxsofar )
            maxsofar = pdA[i];
    }
    return maxsofar;
}
/*
-----------------------------------------------------------
   DoubleMatrix matmax( DoubleMatrix &, DoubleMatrix &) Function specification
-----------------------------------------------------------
$begin matmaxMatrices$$

$spell dmat cout endl std nc nr int const pd iostream largers matmax max matmin matabs
th namespace $$
$section Get a matrix that contains the larger of each pair of elements from two matrices$$
$index larger of each pair of elements$$
$index max$$
$index matmax$$
$index maximum values$$
$index larger values$$

$table
$bold Prototype:$$ $cend
$syntax/const DoubleMatrix matmax(
    const DoubleMatrix &/A/, 
    const DoubleMatrix &/B/)/$$
$tend

See also: $xref/matmaxScalar//double matmax(const DoubleMatrix&)/$$
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Given $math%m%$$ by $math%n%$$ matrices $italic A$$ 
and $italic B$$, $syntax/matmax(/A/, /B/)/$$ returns 
another $math%m%$$ by $math%n%$$ matrix $italic C$$ 
such that $math%c(i,j) = a(i,j) if a(i,j) >= b(i,j)%$$,
or else $math%c(i,j) = b(i,j) if b(i,j) > a(i,j)%$$, 
for all $math%i%$$ and $math%j%$$ such that 
$math%m > i \ge 0%$$ and $math%n > j \ge 0%$$.

$head Example$$
If you compile, link and run the following program, 
$codep

    #include <iostream>
    #include "matmax.h"

    void main(){

      using namespace std;

      DoubleMatrix dmatA(3,2),
                   dmatB(3,2),
                   dmatC(3,2);
      double       *pdA    = dmatA.data(),
                   *pdB    = dmatB.data();

      // Setting A to a matrix:
      //    [  1  -4 ]
      //    [ -2   5 ]
      //    [  3  -6 ]
      // and B to a matrix:
      //    [  0  -3 ]
      //    [ -1   4 ]
      //    [  2  -5 ]
      for( int i=0; i<dmatA.nr()*dmatA.nc(); i++ ){
          pdA[i] = (i+1)*(i%2 == 0? 1: -1);
          pdB[i] = ( i )*(i%2 == 0? 1: -1);
      }

      dmatC = matmax(dmatA, dmatB);

      cout << "Original matrix A" << endl;
      dmatA.print();

      cout << "Original matrix B" << endl;
      dmatB.print();

      cout << "Matrix that contains largers of A and B" << endl;
      dmatC.print();
    }

$$
following matrices are displayed.

$codep
    Original matrix A
    [1, -4]
    [-2, 5]
    [3, -6]
    Original matrix B
    [0, -3]
    [-1, 4]
    [2, -5]
    Matrix that contains largers of A and B
    [1, -3]
    [-1, 5]
    [3, -5]
$$
$end

// Updated 2-5-01 Alyssa
// fixed for const correctness

-----------------------------------------------------------
   Function implementation
-----------------------------------------------------------
*/

const DoubleMatrix matmax(const DoubleMatrix& A, const DoubleMatrix& B)
{
    assert(A.nr() == B.nr());
    assert(A.nc() == B.nc());

    int iRows = A.nr();
    int iCols = A.nc();
    int n = iRows*iCols;
    DoubleMatrix C(iRows, iCols);

    const double* pdA = A.data();
    const double* pdB = B.data();
    double*       pdC = C.data();

    for(int i=0; i<n; i++)
    {
        if( pdA[i] >= pdB[i] )
            pdC[i] = pdA[i];
        else
            pdC[i] = pdB[i];
    }
    return C;
}
