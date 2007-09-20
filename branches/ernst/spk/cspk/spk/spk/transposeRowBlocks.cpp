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
 * File: transposeRowBlocks.cpp
 *
 *
 * The definition for transposeRowBlocks() which
 * transpose a specified row-based subblock of a matrix.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/


/*************************************************************************
 *
 * Function: transposeRowBlocks
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin transposeRowBlocks$$
$spell 
    cout endl iostream namespace std nr nc nb int subblock const subblocks
    valarray
    Spk
$$

$section Transposing Row-based Blocks$$

$index transposeRowBlocks$$
$index transpose, of row-based blocks$$

$table
$bold Prototype:$$   $cend  
$syntax/const DoubleMatrix transposeRowBlocks(const DoubleMatrix& /A/, int /nr/)/$$ $rend
$bold See Also$$ $cend $xref/transposeRowBlocksVA//valarray version/$$  $rend
$tend

$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Returns a copy of the $math%m by n%$$ matrix $italic A$$ where 
successive subblocks
consisting of $italic nr$$ rows are transposed
where $italic nr$$ divides the number of rows in $italic A$$.
$pre

$$
Let $math%B_k%$$ be the subblock
consisting of rows numbers
$math%(k-1)*nr+1%$$ through $math%k*nr%$$ of $italic A$$; i.e.,
$math%	 
	B_k = A.row((k-1)*nr + 1 , nr)
%$$
The return value is the matrix
$math%
	                         T
	[B_1 , B_2 , ... , B_nb ]
%$$
where $math%nb%$$ is the number of rows in $italic A%$$ 
divided by $italic nr%$$.

$head Arguments$$
$syntax/
/A/
/$$
is a m by n matrix.
$syntax/

/nr/
/$$
specifies the number of rows by which row-based transpose is performed.
$italic nr$$ must be greater than or equal to zero and less than or
equal to $math%m%$$ and $math%m = nr * x%$$ for some integer value $italic x$$.
If any of these requirements is violated, it terminates the system.


$head Example$$
$codep
    #include <iostream>
    #include "transposeRowBlocks.h"
    #include "DoubleMatrix.h"

    using namespace std;

    int main()
    {
        // Given a 4 by 3 matrix, A:
        A = [ 1 , 2 , 3 ], ...
		    [ 4 , 5 , 6 ], ...
		    [ 7 , 8 , 9 ], ...
		    [ 10, 11, 12] ...
		    
        // Set #of rows to 2
        int nr = 2;
    
        // Call the function and puts the result to B
	    DoubleMatrix B = transposeRowBlocks(A, nr);
        std::cout << "B=" << endl;
        B.print();
        return 0;
    }
$$
displays the following matrix:
$codep
	B=
	[ 1  4 ]
	[ 2  5 ]
	[ 3  6 ]
	[ 7  10 ]
	[ 8  11 ]
	[ 9  12 ]
	
$$
$end


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * The motivation for developing this routine was to swap the order of:
 * a central difference approximation of a partial derivative
 * to a partial of central difference.
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <iostream>
#include <cassert>
#include "transposeRowBlocks.h"
#include "transpose.h"
#include "DoubleMatrix.h"

static DoubleMatrix transposeColBlocks(const DoubleMatrix& A, int nc);

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

const DoubleMatrix transposeRowBlocks(const DoubleMatrix& A, int nr)
{
    int aNr = A.nr();
    int aNc = A.nc();

    assert( nr >= 0 );
    assert( nr <= aNr );
    assert( aNr % nr == 0 );
    return transpose(transposeColBlocks(transpose(A),nr));
}
void transposeRowBlocks(const DoubleMatrix& A, int nr, DoubleMatrix &B)
{
    int aNr = A.nr();
    int aNc = A.nc();

    assert( nr >= 0 );
    assert( nr <= aNr );
    assert( aNr % nr == 0 );
    transpose(transposeColBlocks(transpose(A),nr),B);
}


/*========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 *========================================================================*/

DoubleMatrix transposeColBlocks(const DoubleMatrix& A, int nc)
{
    using namespace std;
    /*
     * Given A = [ 1 3 5 7  9 11 ]
     *     [ 2 4 6 8 10 12 ]
     *
     * Given nc = 2;
     *
     *
     * A1= [ 1 3 ]  A2= [ 5 7 ]  A3= [  9 11 ]
     *     [ 2 4 ]      [ 6 8 ]      [ 10 12 ]
     *
     * A1'=[ 1 2 ]  A2'=[ 5 6 ]  A3'=[  9 10 ]
     *     [ 3 4 ]      [ 7 8 ]      [ 11 12 ]
     *
     * A' = [ 1 2 5 6  9 10 ]
     *      [ 3 4 7 8 11 12 ]
     *
     */
    int aNr = A.nr();
    int aNc = A.nc();

    assert( nc >= 0 );
    assert( nc <= aNc );
    assert( aNc % nc == 0 );

    int nBlocks = aNc / nc;

    // conquor
    if( nBlocks == 1 )
        return transpose(A);
    else    // divide
    {
        DoubleMatrix B(nc, aNr*nBlocks);
        B.fill(0);
        double* b = B.data();
        int nHalfBlocks = nBlocks / 2;
        bool odd = (nBlocks%2 == 1? true : false);
        const double* a = A.data();

        // upper half
        unsigned int begin = 0;
        unsigned int end = begin+aNr*nc*nHalfBlocks;
        DoubleMatrix upper(aNr, nHalfBlocks*nc);
        std::copy(a+begin, a+end, upper.data());
        upper = transposeColBlocks(upper, nc);
        std::copy(upper.data(), upper.data()+aNr*nc*nHalfBlocks, b+begin);

        begin = end;
        // middle block if nr is odd
        if(odd)
        {
            end = begin+aNr*nc;
            DoubleMatrix middle(aNr, nc);
            std::copy(a+begin, a+end, middle.data());
            middle = transposeColBlocks(middle, nc);
            std::copy(middle.data(), middle.data()+aNr*nc, b+begin);
            begin = end;
        }

        end = begin+aNr*nc*nHalfBlocks;
        // lower half block
        DoubleMatrix lower(aNr, nc*nHalfBlocks);
        std::copy(a+begin, a+end, lower.data());
        lower = transposeColBlocks(lower, nc);
        std::copy(lower.data(), lower.data()+aNr*nc*nHalfBlocks, b+begin);
        
        return B;
    }
}
/*************************************************************************
 *
 * Function: valarray version of transposeRowBlocks()
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin transposeRowBlocksVA$$
$spell 
      valarray
      cout 
      endl 
      iostream 
      namespace 
      std 
      nr 
      nc 
      nb 
      int 
      subblock 
      const 
      subblocks
      submatrix
      Spk
$$

$section Transposing Row-based Blocks (Valarray Version)$$

$index transposeRowBlocksVA$$
$index transpose, of row-based blocks (valarray version)$$

$table
$bold Prototype:$$   $cend  
$syntax/const DoubleMatrix transposeRowBlocks(const SPK_VA::valarray<double>& /A/, int /n/, int /m/)/$$ $rend
$bold See Also$$ $cend $xref/transposeRowBlocks//DoubleMatrix version/$$  $rend
$tend

$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Given a matrix, $italic A$$, composed of $math%k%$$ number of 
$math%m by n%$$ sub matrices, $math%A[i]%$$, such that:
$math%
        /        \
   A  = |  A[1]  |
        |  A[2]  |
        |  .     |
        |  .     |
        |  .     |
        |  A[k]  |
        \        /
 
        /                                       \
      = |  A[1](1,1), A[1](1,2), ... A[1](1,n)  |
        |  A[1](2,1), A[1](2,2), ... A[1](2,n)  |
        |  ...                                  |
        |  A[1](m,1), A[1](m,2), ... A[1](m,n)  |
        -----------------------------------------
        |  A[2](1,1), A[2](1,2), ... A[2](1,n)  |
        |  A[2](2,1), A[2](2,2), ... A[2](2,n)  |
        |  ...                                  |
        |  A[2](m,1), A[2](m,2), ... A[2](m,n)  |
        -----------------------------------------
        |  ...                                  |
        |  ...                                  |
        |  ...                                  |
        -----------------------------------------
        |  A[k](1,1), A[k](1,2), ... A[k](1,n)  |
        |  A[k](2,1), A[k](2,2), ... A[k](2,n)  |
        |  ...                                  |
        |  A[k](m,1), A[k](m,2), ... A[k](m,n)  |
        \                                       /
%$$
the row-based transpose of $math%A%$$ is a $math%k * n by m%$$ matrix, $math%B%$$,
composed of transpose of $math%A[i]%$$
in the following fashion:
$math%
        /         \
   B  = |  A[1]'  |
        |  A[2]'  |
        |  .      |
        |  .      |
        |  .      |
        |  A[k]'  |
        \         /
      
        /                                     \
      = | A[1](1,1), A[1](2,1), ... A[1](m,1) |
        | A[1](1,2), A[1](2,2), ... A[1](m,2) |
        | ...                                 |
        | A[1](1,n), A[1](2,n), ... A[1](m,n) |
        ---------------------------------------
        | A[2](1,1), A[2](2,1), ... A[2](m,1) |
        | A[2](1,2), A[2](2,2), ... A[2](m,2) |
        | ...                                 |
        | A[2](1,n), A[2](2,n), ... A[2](m,n) |
        ---------------------------------------
        |  ...                                |
        |  ...                                |
        |  ...                                |
        ---------------------------------------
        | A[k](1,1), A[k](2,1), ... A[k](m,1) |
        | A[k](1,2), A[k](2,2), ... A[k](m,2) |
        | ...                                 |
        | A[k](1,n), A[k](2,n), ... A[k](m,n) |
        \                                     /
%$$
This valarray version of $code transposeRowBlocks$$ orders $math%B%$$ in the column order
and, thus, returns a $math%k * n * m%$$ size vector.
$pre

$$
When dimensions do not match, the program terminates.

$head Arguments$$
$syntax/
/A/
/$$
is an array storing, in the column major order,
the $math%k * m by n%$$ matrix, $math%A%$$, described above.
$syntax/

/k/
/$$
is the number of columns in the $math%A%$$ matrix.
$syntax/

/m/
/$$
is the number of rows in a $math%A[i]%$$ submatrix.
The value must be positive.

$head Example$$
$codep
    #include <iostream>
    #include "transposeRowBlocks.h"
    #include "SpkValarray.h"

    using SPK_VA::valarray;
    using namespace std;

    int main()
    {
        // Given a 4 by 3 matrix, A:
        // [ 1.11e+002 1.12e+002 1.13e+002 ]
        // [ 1.21e+002 1.22e+002 1.23e+002 ]
        // [ 2.11e+002 2.12e+002 2.13e+002 ]
        // [ 2.21e+002 2.22e+002 2.23e+002 ]
		//
        // The corresponding valarray object, a, will contain the values
        // in the following order:
        // a = [ 111, 121, 211, 221, 112, 122, 212, 222, 113, 123, 213, 223 ]
        //
        valarray<double> a( 4 * 3 );
        a[0] = 111;
        a[1] = 121;
        a[2] = 211;
        a[3] = 221;
        a[4] = 112;
        a[5] = 122;
        a[6] = 212;
        a[7] = 222;
        a[8] = 113;
        a[9] = 123;
        a[10]= 213;
        a[11]= 223;

        //
        // Assume the subblock contained in A has 2 rows in each.
        //
        int m = 2;
    
        // Call the function and puts the result to C
	    valarray<double> c = transposeRowBlocks(a, 3, m);
        
        cout << "[ ";
        for( int i=0; i< c.size(); i++ )
        {
          cout << c[i] << ", ";
        }
        cout << " ]" << endl;

        return 0;
    }
$$
displays the following matrix:
$codep
	[ 111, 112, 113, 211, 212, 213, 121, 122, 123, 221, 222, 223, ]
	
$$
$end

*/
#include "SpkValarray.h"

using SPK_VA::valarray;
using SPK_VA::gslice;

const valarray<double> transposeRowBlocks( const valarray<double>& A, int n, int m )
{
  if( A.size() == 0 || n == 0 )
    return A;

  assert( m >= 0 );

  int k = A.size() / n / m;
  assert( k * n * m == A.size() );

  //
  // [ Note from Sachiko, 10/11/2002 ]
  // Remember that C++ reads matrix elements (2D vectors) in row-major order.
  // So, think like a matrix whose elements we'll manuplate is the transpose of A
  // or further:
  //
  //
  // A = 
  //  [ 1.1100000000000000e+002 1.1200000000000000e+002 1.1300000000000000e+002 ]
  //  [ 1.2100000000000000e+002 1.2200000000000000e+002 1.2300000000000000e+002 ]
  //  [ 2.1100000000000000e+002 2.1200000000000000e+002 2.1300000000000000e+002 ]
  //  [ 2.2100000000000000e+002 2.2200000000000000e+002 2.2300000000000000e+002 ]
  //
  //
  // B =
  //  [ 1.1100000000000000e+002 1.2100000000000000e+002 ]
  //  [ 1.1200000000000000e+002 1.2200000000000000e+002 ]
  //  [ 1.1300000000000000e+002 1.2300000000000000e+002 ]
  //  [ 2.1100000000000000e+002 2.2100000000000000e+002 ]
  //  [ 2.1200000000000000e+002 2.2200000000000000e+002 ]
  //  [ 2.1300000000000000e+002 2.2300000000000000e+002 ]
  //
  using namespace std;
  size_t A_sub_len[] = { m, n };
  size_t A_sub_str[] = { 1, m * k };
  valarray<size_t> A_sub_lengths( A_sub_len, 2 );
  valarray<size_t> A_sub_strides( A_sub_str, 2 );

  size_t B_sub_len[] = { m,     n };
  size_t B_sub_str[] = { n * k, 1 };
  valarray<size_t> B_sub_lengths( B_sub_len, 2 );
  valarray<size_t> B_sub_strides( B_sub_str, 2 );
  valarray<double> B( k * m * n );
  for( int i=0; i<k; i++ )
  {
    B[ gslice( i * n, B_sub_lengths, B_sub_strides  ) ] = A[ gslice( i * m, A_sub_lengths, A_sub_strides) ];
  }
  return B;
}

