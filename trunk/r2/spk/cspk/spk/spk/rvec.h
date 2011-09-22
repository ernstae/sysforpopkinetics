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
/****************************************************************
 * 
 * rvec.h
 * 
 * Author: Sachiko Honda
 *
 ****************************************************************/
/*
-----------------------------------------------------------------
     Function Specification
-----------------------------------------------------------------
$begin rvec$$

$spell rvec dmat redim nc nr cout iostream endl namespace const std
  valarray
$$

$section Row major conversion of a matrix to a column vector (DoubleMatrix version)$$

$cindex row major conversion \of \a matrix \to \a column vector (DoubleMatrix version)$$
$index rvec (DoubleMatrix version)$$

$table
$bold Prototype$$ $cend 
$syntax/
const DoubleMatrix rvec(const DoubleMatrix &/X/)
/$$
$tend

See also: $xref/rvecVA//valarray version of rvec()/$$

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Reads the original matrix in the row-major order and returns as a column vector
$math%
                                                      T
    [ x   , x    , ... , x  ,  x   ,  x   , ... , x  ]
       1,1   1,2          1,n   2,1    2,2         m,n
%$$
where $math%m%$$ is the number of rows
and $math%n%$$ is the
number of columns 
in the matrix $italic X$$.

$head Example$$
If you compile, link and run the following program,
$codep

  #include <iostream>
  #include "DoubleMatrix.h"
  #include "rvec.h"

  void main(){
    
    using namespace std;

    DoubleMatrix dmatA( 2, 2 );
    double * pDmatA = dmatA.data();

    DoubleMatrix dmatB;

    pDmatA[0] = 1.0;
    pDmatA[1] = 3.0;
    pDmatA[2] = 2.0;
    pDmatA[3] = 4.0;

    dmatB = rvec( dmatA );
    
    cout << "dmatA =" << endl;
    dmatA.print();
    cout << "rvec(dmatA) =" << endl;
    dmatB.print();
  }
$$

the following results will be displayed.
$codep

  dmatA = 
  [ 1  2 ]
  [ 3  4 ]

  rvec(dmatA) =
  [ 1 ]
  [ 2 ]
  [ 3 ]
  [ 4 ]

$$
$end
-----------------------------------------------------------------
     Function Implementation
-----------------------------------------------------------------
*/

#ifndef RVEC_H
#define RVEC_H

#include "DoubleMatrix.h"

inline const DoubleMatrix rvec(const DoubleMatrix &dmatA)
{
	int nc = dmatA.nc();
	int nr = dmatA.nr();

	DoubleMatrix dmatRet(nr*nc, 1);
	const double *pX = dmatA.data();
	double *pRet = dmatRet.data();

	for(int i = 0; i < nr; i++)
	{
		for(int j = 0; j < nc; j++)
		{
			pRet[i*nc + j] = pX[j*nr + i];
		}	
	}
	
	return dmatRet;
}
/*
-----------------------------------------------------------------
     Function Specification
-----------------------------------------------------------------
$begin rvecVA$$

$spell 
   valarray
   SpkValarray
   const
   cols
   cout
   endl
   iostream
   namespace
   std
   rvec
$$

$section Row major conversion of a matrix to a column vector$$

$cindex row major conversion \of \a matrix \to \a column vector$$
$index rvec$$

$table
$bold Prototype$$ $cend 
$syntax/
const SPK_VA::valarray<double> rvec(const SPK_VA::valarray<double> &/a/, int /nCols/)
/$$
$tend

See also: $xref/rvec//DoubleMatrix version/$$

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The function converts a $math%m%$$ by $math%n%$$ matrix, $math%A%$$, 
to a column vector containing the values of $math%A%$$ in
the following order:

$math%
                                                      T
    [ a   , a    , ... , a  ,  a   ,  a   , ... , a  ]
       1,1   1,2          1,n   2,1    2,2         m,n
%$$

$head Arguments$$
$syntax/
/a/
/$$
is an array containing the values of $math%m%$$ by $math%n%$$ matrix, $math%A%$$,
where $math%m%$$ and $math%n%$$ are greater than or equal to zero.

$syntax/

/nCols/
/$$
specifies the column dimension $math%n%$$ of the matrix $math%A%$$.
The value must be greater than zero unless $math%A%$$ is empty.

$head Example$$
If you compile, link and run the following program,
$codep

  #include <iostream>
  #include "SpkValarray.h"
  #include "rvec.h"

  void main()
  {
    using SPK_VA::valarray;
    using namespace std;

    //
    // Set A to a matrix:
    //
    //      /            \
    //  A = |  1.0  2.0  |
    //      |  3.0  4.0  |
    //      \            /
    //
    double a[] = { 1.0, 3.0, 2.0, 4.0 };
    SPK_VA::valarray<double> A ( a, 4 );

    //
    // rvec(A) expects a matrix B in the following form:
    //
    //            /       \
    //  rvec(A) = |  1.0  |
    //            |  2.0  |
    //            |  3.0  |
    //            |  4.0  |
    //            \       /
    //
    SPK_VA::valarray<double> B = rvec( A, 2 );
    
    cout << "A =" << A << endl;
    cout << "rvec(A) =" << B << endl;
  }
$$

the following results will be displayed.
$codep

  a = { 1.0, 3.0, 2.0, 4.0 }

  rvec(A) = { 1.0, 2,0, 3.0, 4.0 }

$$
$end
-----------------------------------------------------------------
     Function Implementation
-----------------------------------------------------------------
*/
#include <cassert>
#include "SpkValarray.h"

inline const SPK_VA::valarray<double> rvec(const SPK_VA::valarray<double>& a, int nCols)
{
  using namespace std;

  const int nRows = a.size() / nCols;
  assert( nRows * nCols == a.size() );

  size_t len[] =  { nRows, nCols };
  size_t str[] =  { 1,     nRows };
  valarray<size_t> lengths(len, 2);
  valarray<size_t> strides(str, 2);

  return a[ SPK_VA::gslice(0, lengths, strides) ];
}
#endif
