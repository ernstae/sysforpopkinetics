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
 * mulByScalar.cpp
 *
 * Element-wise multiplication of a matrix by a matrix of scalars and a scalar  
 *
 * Author: Sachiko Honda
 */
# include <cassert>
# include "mulByScalar.h"

/*
-----------------------------------------------------------
   Function specification
-----------------------------------------------------------
$begin mulByScalar$$

$spell 
    mul 
    dmat 
    int 
    const
    Spk
    div
$$

$section Element-wise Multiplication$$
$index mulByScalar$$
$index matrix, element-wise multiplication$$

$table
$bold Prototype:$$ $cend
$syntax/DoubleMatrix mulByScalar(const DoubleMatrix &/A/, const double &/b/)/$$ $rend
$cend
$syntax/void mulByScalar(const DoubleMatrix &/A/, const double &/b/, DoubleMatrix &/C/)/$$ $rend
$cend
$syntax/DoubleMatrix mulByScalar(const DoubleMatrix &/A/, const DoubleMatrix &/B/)/$$ $rend
$cend
$syntax/void mulByScalar(const DoubleMatrix &/A/, const DoubleMatrix &/B/, DoubleMatrix &/C/)/$$ $rend
$tend

See also: $xref/addition//addition/$$, $xref/subtraction//subtraction/$$, $xref/multiply//matrix multiply/$$, 
$xref/backDiv//matrix back division/$$,
$xref/divByScalar//element-wise division/$$.

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Performs $bold element-wise$$ matrix multiplication.  

$head Returned Value$$
The $code void$$ versions return the results through the third argument.
Otherwise, the function returns the resulting $math%m by n%$$ matrix by value.

$head Arguments$$
$syntax/
/A/
/$$
is a $math%m by n%$$ matrix.

$syntax/

/b/
/$$
is a double-precision scalar multiplier.

$syntax/

/B/
/$$
is a $math%m by n%$$ matrix multiplier.

$syntax/

/C/
/$$
is a reference to a $math%m by n%$$ matrix.  When this argument is given,
the result is placed at the memory location pointed by the reference.

$head Example$$
If you compile, link and run the following program, 
$codep
  #include "mulByScalar.h"

  void main(){
	DoubleMatrix A( 3, 4 );
	double scalar = 2.0;
	DoubleMatrix C;

    double *a = A.data();

    // Set both A to a matrix
	// 	  [ 1  4  7 ]
	//    [ 2  5  8 ]
	//    [ 3  6  9 ]

    for( int i=0; i<12; i++ ){
		a[i] = i+1;
	}
	
	// Computer C = A * scalar;
	C = mulByScalar( A, scalar );

    // Display C
	C.print();

  }
$$
a following matrix is displayed.

$codep

  [ 2   8  14 ]
  [ 4  10  16 ]
  [ 9  12  18 ]
  
$$
$end

-----------------------------------------------------------
   Function implementation
-----------------------------------------------------------
*/
void mulByScalar(const DoubleMatrix &dmatA, const DoubleMatrix &dmatScalars, DoubleMatrix &dmatC)
{

	assert(dmatA.nr() >= 0);
	assert(dmatA.nc() >= 0);
	assert(dmatA.nr() == dmatScalars.nr());
	assert(dmatA.nc() == dmatScalars.nc());

	int nr = dmatA.nr();
	int nc = dmatA.nc();
	int i;

	dmatC.resize(nr, nc);
	const double *pA = dmatA.data();
	const double *pB = dmatScalars.data();
	double *pC = dmatC.data();

	for( i=0; i<nr*nc; i++ ){
		pC[i] = pA[i] * pB[i];
	}
}

const DoubleMatrix mulByScalar(const DoubleMatrix &dmatA, const DoubleMatrix &dmatScalars)
{
	DoubleMatrix dmatC(dmatA.nr(), dmatA.nc());
    mulByScalar(dmatA, dmatScalars, dmatC);
    return dmatC;
}

const DoubleMatrix mulByScalar(const DoubleMatrix &dmatA, const double scalar)
{
    int nr = dmatA.nr();
    int nc = dmatA.nc();

    DoubleMatrix dmatC(nr, nc);
    mulByScalar(dmatA, scalar, dmatC);
    return dmatC;
}


void mulByScalar(const DoubleMatrix &dmatA, const double scalar, DoubleMatrix& dmatC)
{

	assert(dmatA.nr() >= 0);
	assert(dmatA.nc() >= 0);
	assert(dmatA.nr() == dmatC.nr());
	assert(dmatA.nc() == dmatC.nc());

	int nr = dmatA.nr();
	int nc = dmatA.nc();
	int i;

	const double *pA = dmatA.data();
	double *pC = dmatC.data();

	for( i=0; i<nr*nc; i++ )
    {
		pC[i] = pA[i] * scalar;
	}
}
