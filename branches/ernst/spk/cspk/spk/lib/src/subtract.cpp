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
 * subtract.cpp
 *
 * DoubleMatrix - DoubleMatrix subtraction
 *
 * Author: Sachiko Honda
 *
 */
/*
--------------------------------------------------------
   Function specification
--------------------------------------------------------
$begin subtraction$$

$spell 
    dmat 
    int 
    const
    spk
    div
    mul
$$

$section Element-wise subtraction$$

$index subtract$$
$index matrix, subtraction$$

$table
$bold Prototype:$$ $cend
$syntax/const DoubleMatrix subtract(const DoubleMatrix &/A/, const DoubleMatrix &/B/)/$$ $rend
$cend
$syntax/void subtract(const DoubleMatrix &/A/, const DoubleMatrix &/B/, DoubleMatrix &/C/)/$$ $rend
$tend

See also: $xref/addition//addition/$$, $xref/multiply//matrix multiply/$$, $xref/backDiv//matrix back division/$$,
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
This function performs matrix subtraction.  

$head Returned Value$$
Given a $math%m by n%$$ matrix, $italic A$$, and another matrix with the same dimensions, $italic B$$,
the resulting matrix is also a $math%m by n%$$.  The result is returned by value unless
the third argument $italic C$$
is given.  When $italic C$$ is given, the result is set to the memory location referred by $italic C$$.

$pre

$$
This function checks to see whether a floating-point error has occurred or not at the
end of computation.  If detected, it will throw an $xref/SpkException//SpkException/$$ whose last
($xref/SpkError//error/$$) element is set to indicate the nature of the error.

$head Arguments$$
$syntax/
/A/
/$$
is the $math%m by n%$$left hand side  matrix, where m and n are greater than or equal to zero.
$syntax/

/B/
/$$
is the $math%m by n%$$ right hand side matrix, where m and n are greater than or equal to zero.
$syntax/

/C/
/$$
is the $math%m by n%$$ right hand side matrix, which will contain the result, if given.

$head Example$$
If you compile, link and run the following program,

$codep

   #include "subtract.h"

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

       // Computes C = A - B
       C = subtract(A, B);

       // display C
	   C.print();
   }
$$
a matrix
$codep
   [ -1  2 ]
   [  0  3 ]
   [  1  4 ]

$$
will be displayed.
$end
--------------------------------------------------------
   Function implementation
--------------------------------------------------------
*/
#include <iostream>
#include <cassert>
#include "subtract.h"

using std::cout;

DoubleMatrix subtract(const DoubleMatrix &dmatA, const DoubleMatrix &dmatB){

	DoubleMatrix dmatC( dmatA.nr(), dmatA.nc() );
    subtract(dmatA,dmatB,dmatC);
	
	return dmatC;
}

void subtract(const DoubleMatrix &dmatA, const DoubleMatrix &dmatB, DoubleMatrix &dmatC){

	assert( dmatA.nr() == dmatB.nr() );
	assert( dmatB.nc() == dmatB.nc() );

	dmatC.resize( dmatA.nr(), dmatA.nc() );
	const double *pA = dmatA.data();
	const double *pB = dmatB.data();
	double *pC = dmatC.data();

	int i=dmatA.nr()*dmatA.nc();
	while(i){
		i--;
		pC[i] = pA[i] - pB[i];
	}
}
