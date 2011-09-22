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
 * isMatEpsEqual
 *
 * Compare two DoubleMatrix matrices
 *
 * Sachiko Honda
 *
 */
/*
---------------------------------------------------------------
     Function specification
---------------------------------------------------------------
$begin isDmatEpsEqual$$

$spell const eps dmat cout endl bool int iostream float ANSI dbl cfloat$$

$section Element-wise comparison of two matrices$$

$index isDmatEpsEqual$$
$index matrix, comparison$$

$table
$bold Prototype:$$ $cend
$syntax/bool isDmatEpsEqual(const DoubleMatrix &/A/, const DoubleMatrix &/B/, const DoubleMatrix &/S/)/$$
$tend
See also $xref/isDblEpsEqual//isDblEpsEqual()/$$
$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Returns true if the double precision numbers a(i,j) in A and b(i,j) in B
are epsilon equivalent,otherwise returns false. Specifically, isDmatEpsEqual returns true if 
$math%
  for all elements, isDblEpsEqual(a(i,j), b(i,j), s(i,j) is true.
%$$

$head Arguments$$
$syntax/
/A/
/$$
is a m by n matrix.

$syntax/

/B/
/$$
is a m by n matrix.

$syntax/

/S/
/$$
is a m by n matrix that contains scaling values.  See also $xref/isDblEpsEqual//isDblEpsEqual()/$$

$head Example$$
If you link and run the following program,

$codep

  #include <iostream.h>
  #include <cfloat>
  #include "isDmatEpsEqual.h"

  int main()
  {
	DoubleMatrix A(3,2);
	DoubleMatrix B(3,2);
	DoubleMatrix S1(3,2);
    DoubleMatrix S2(3,2);

	double *a = A.data();
	double *b = B.data();
	double *s1 = S1.data();
    double *s2 = S2.data();

    // Set A to a matrix:
	//    [ 0.0  3.0 ] 
	//    [ 1.0  4.0 ]
	//    [ 2.0  5.0 ]
    //
    // Set B to a matrix that contains:
    //    B(i,j) = A(i,j)+DBL_EPSILON
	for( int i=0; i<6; i++ )
    {
		a[i] = i;   
		b[i] = i+DBL_EPSILON;
    }
    // Set S1 to a matrix that specifies the scale of values equal to A.
    S1.fill(1.0);

    // Set S2 to a matrix that specifies the scale smaller than the scale of values in A.
    S2.fill(0.01);

    // Compare A to B
    if( isDmatEpsEqual(A,B,S1) )
		cout << "A == B" << endl;
	else
	    cout << "A != B" << endl;

    // Compare A to C
    if( isDmatEpsEqual(A,B,S2) )
		cout << "A == B" << endl;
	else
	    cout << "A != B" << endl;

    return 0;
 }
$$

The program will display the following;

$codep
  A == B
  A != C
$$

$end

// Updated 2-5-01 Alyssa
// fixed for const correctness

---------------------------------------------------------------
     Function implementation
---------------------------------------------------------------
*/
#include <cassert>
#include "isDblEpsEqual.h"
#include "isDmatEpsEqual.h"

bool isDmatEpsEqual(
	const DoubleMatrix &dmatA, 
	const DoubleMatrix &dmatB, 
	const DoubleMatrix &dmatS )
{

    assert( dmatA.nr() == dmatB.nr() );
    assert( dmatA.nc() == dmatB.nc() );
    assert( dmatA.nr() == dmatS.nr() );
    assert( dmatA.nc() == dmatS.nc() );

    const double *pA = dmatA.data();
    const double *pB = dmatB.data();
	const double *pS = dmatS.data();
    

    for( int i=0; i<dmatA.nr()*dmatA.nc(); i++ ){
        if( !isDblEpsEqual(pA[i], pB[i], pS[i]) )
            return false;
    }
    return true;
}
