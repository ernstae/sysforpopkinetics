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
 * divByScalar.cpp
 *
 * Element-wise division of a matrix by a matrix of scalars and a scalar  
 *
 *
 * Revisit-Sachiko
 *
 * The version of this routine that takes a matrix on the right hand
 * side did not contribute to speed-up when I tried out 
 * making a full matrix with its scalar value when given a 1 by 1 matrix.
 * 
 * Author: Sachiko Honda
 */
# include <cassert>
# include "divByScalar.h"

const DoubleMatrix divByScalar(const DoubleMatrix &dmatA, const DoubleMatrix &dmatScalars){

    DoubleMatrix dmatC(dmatA.nr(), dmatA.nc());
    divByScalar(dmatA,dmatScalars,dmatC);
    return dmatC;
}
void divByScalar(const DoubleMatrix &dmatA, const DoubleMatrix &dmatScalars, DoubleMatrix &dmatC){

	const double *pB = dmatScalars.data();
    if( dmatScalars.nr() == 1 && dmatScalars.nc() == 1 ){
        divByScalar( dmatA, pB[0], dmatC );
    }
    else{
	    assert(dmatA.nr() >= 0);
	    assert(dmatA.nc() >= 0);
	    assert(dmatA.nr() == dmatScalars.nr());
	    assert(dmatA.nc() == dmatScalars.nc());
        assert(dmatC.nr() == dmatA.nr());
        assert(dmatC.nc() == dmatA.nc());

	    int nr = dmatA.nr();
	    int nc = dmatA.nc();
	    //DoubleMatrix dmatC(nr, nc);
	    const double *pA = dmatA.data();
	    double *pC = dmatC.data();
	    int i;
	    for( i=0; i<nr*nc; i++ ){
            assert( pB[i] != 0.0 );
		    pC[i] = pA[i] / pB[i];
	    }
    }

}
/*
-----------------------------------------------------------
   Function specification
-----------------------------------------------------------
$begin divByScalar$$

$spell 
    dmat 
    div 
    non-zeros 
    int 
    th
    const
    Spk
    mul
$$
$section Element-wise Division$$
$index divByScalar$$
$index matrix, element-wise division$$

$table
$bold Prototype:$$ $cend
$syntax/const DoubleMatrix divByScalar(const DoubleMatrix &/A/, double /b/)/$$ $rend
$cend
$syntax/void divByScalar(const DoubleMatrix &/A/, double /b/, DoubleMatrix &/C/)/$$ $rend
$cend
$syntax/const DoubleMatrix divByScalar(const DoubleMatrix &/A/, const DoubleMatrix /B/)/$$ $rend
$cend
$syntax/void divByScalar(const DoubleMatrix &/A/, const DoubleMatrix /B/, DoubleMatrix& /C/)/$$ $rend
$cend
$tend

See also: $xref/addition//addition/$$, $xref/subtraction//subtraction/$$, $xref/multiply//matrix multiply/$$, 
$xref/backDiv//matrix back division/$$,
$xref/mulByScalar//element-wise multiply/$$.

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This function performs element-wise matrix division.  If the second argument is a scalar,
all the elements of $italic A$$ is divided by $italic b$$.  If it is a matrix with the same 
dimensions as of $italic A$$, each element of $italic A$$ is divided by the corresponding
element of $italic B$$.  The result is returned by value or is set to the memory location
referred by $italic C$$ if given.

$head Arguments$$
$syntax/
/A/
/$$
is a $math%m by n%$$ matrix numerator.
$syntax/

/b/
/$$
is a scalar denominator.  $italic b$$ must be non-zero.
$syntax/

/B/
/$$
is a $math%m by n%$$ matrix denominator.

$syntax/

/C/
/$$
is a $math%m by n%$$ matrix that will contain the result if given.  If the dimensions does not match,
the program terminates.

$head Revisit$$
Division by zero exception should be handled properly.

$head Example$$
If you compile, link and run the following program, 
$codep
  #include "divByScalar.h"

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
	
	// Computer C = A / scalar;
	C = divByScalar( A, scalar );

    // Display C
	C.print();

  }
$$
a following matrix is displayed.

$codep

  [ 0.5  2.0  3.5 ]
  [ 1.0  2.5  4.0 ]
  [ 1.5  3.0  4.5 ]
  
$$
$end
-----------------------------------------------------------
   Function implementation
-----------------------------------------------------------
*/


# include <iostream>
# include <cassert>
# include "divByScalar.h"


const DoubleMatrix divByScalar(const DoubleMatrix &dmatA, double scalar)
{
	int nr = dmatA.nr();
	int nc = dmatA.nc();
	DoubleMatrix dmatC(nr, nc);
    divByScalar(dmatA,scalar,dmatC);
    return dmatC;
}
void divByScalar(const DoubleMatrix &dmatA, double scalar, DoubleMatrix & dmatC)
{
    using namespace std;

	assert(dmatA.nr() >= 0);
	assert(dmatA.nc() >= 0);
	assert(scalar != 0.0);
    assert(dmatC.nr() == dmatA.nr());
    assert(dmatC.nc() == dmatA.nc());

	int nr = dmatA.nr();
	int nc = dmatA.nc();
	int i;

	const double *pA = dmatA.data();
	double *pC = dmatC.data();

	for( i=0; i<nr*nc; i++ ){
		pC[i] = pA[i] / scalar;
	}
}
