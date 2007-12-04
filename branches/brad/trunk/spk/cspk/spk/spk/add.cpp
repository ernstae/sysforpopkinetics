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
 * add.cpp
 *
 * DoubleMatrix - DoubleMatrix addition
 *
 * Author: Sachiko Honda
 *
 */
/*
--------------------------------------------------------
   Function specification
--------------------------------------------------------
$begin addition$$

$spell 
    Goddard 
    Sachiko 
    int 
    dmat 
    const 
    pre 
    sutter
    Spk
    div
    mul
$$

$section Element-wise addition$$

$index add$$
$index matrix, addition$$

$table
$bold Prototype:$$ $cend
$syntax/const DoubleMatrix add( const DoubleMatrix &/A/,  const DoubleMatrix &/B/)/$$ $rend
$cend
$syntax/void add( const DoubleMatrix &/A/, const DoubleMatrix &/B/, DoubleMatrix& /C/)/$$ $rend
$tend

See also: $xref/subtraction//subtraction/$$, $xref/multiply//matrix multiply/$$, $xref/backDiv//matrix back division/$$,
$xref/mulByScalar//element-wise multiply/$$, $xref/divByScalar//element-wise division/$$.

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This function performs matrix-matrix addition.

$head Return Value$$
Given a $math%m by n%$$ matrix, $italic A$$, and $math%m by n%$$ matrix, $italic B$$, it returns
another $math%m by n%$$ matrix that is the result of the addition of $italic A$$ and $italic B$$.
For the void version, the result is placed at a memory location referenced by $code &$$ $italic C$$.

$head Arguments$$
$syntax/
/A/
/$$
is a $math%m by n%$$ matrix, where m and n are greater than or equal to zero.
$syntax/

/B/
/$$
is a $math%m by n%$$ matrix, where m and n are greater than or equal to zero.

$syntax/

/C/
/$$
is a $math%m by n%$$ matrix, if given, which will contain the result.

$head Example$$
If you compile, link and run the following program,

$codep

   #include "add.h"

   void main(){
   
       DoubleMatrix A(3, 2);
	   DoubleMatrix B(3, 2);
	   DoubleMatrix C;

	   double *a = A.data();
	   double *b = B.data();

       // Set A to a matrix
	   //    [ 0  3 ]
	   //    [ 1  4 ]
	   //    [ 2  5 ]
	   // and B to a matrix
	   //    [ 1  1 ]
       //    [ 1  1 ]
	   //    [ 1  1 ]
       for( int i=0; i<6; i++ ){
	       a[i] = i;
		   b[i] = 1;
	   }

       // Computes C = A + B
       C = add(A, B);

       // display C
	   C.print();
   }
$$
the following matrix will be displayed.
$codep
   [ 1, 4 ]
   [ 2, 5 ]
   [ 3, 6 ]
$$
will be displayed.

$head Review$$

Goddard 3/15/00: Documentation should state the requirements on the sizes, and
what happens if they are not met.

  Respond Sachiko 4/3/00: Apply from now on: The sizes are specified in the documentation.
  Add "pre-condition" section in the specification area and state what
  happens when assertion fails.

Goddard 3/15/00: User-defined types returned by value should
almost always be marked "const". (The exceptions are special-purpose
expressions designed for use on the left side of an assignment, which
is NOT the case here.)
REF: Sutter, Exceptional C++, Item 43.

  Respond Sachiko 4/3/00: Fix and apply from now on .

$end
--------------------------------------------------------
   Function implementation
--------------------------------------------------------
*/
#include <iostream>
#include <cassert>
#include "add.h"

const DoubleMatrix add(const DoubleMatrix &dmatA, const DoubleMatrix &dmatB){

	DoubleMatrix dmatC( dmatA.nr(), dmatA.nc() );
    add(dmatA,dmatB,dmatC);
	return dmatC;
}

void add(const DoubleMatrix &dmatA, const DoubleMatrix &dmatB, DoubleMatrix &dmatC)
{
	assert( dmatA.nr() == dmatB.nr() );
	assert( dmatB.nc() == dmatB.nc() );

    using namespace std;

	dmatC.resize( dmatA.nr(), dmatA.nc() );
	const double *pA = dmatA.data();
	const double *pB = dmatB.data();
	double *pC = dmatC.data();

	int i=dmatA.nr()*dmatA.nc();
	while(i)
    {
		i--;
		pC[i] = pA[i] + pB[i];
	}
}
