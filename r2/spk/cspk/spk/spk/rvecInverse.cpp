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
 * File: rvecInverse.cpp
 *
 * Given m and a such that:
 *
 * m > 0,
 *
 * a: (k*m) dimensional column vector
 *    transpose([ a(1,1), a(1,2)..., a(1,m), a(2,1), a(2,2)..., a(2,m).......a(k,m) ])
 *
 * rvecInverse(a,m) returns C such that:
 * 
 * C: k by m matrix
 *    [ a(1,1) a(1,2)... a(1,m) ]
 *    [ a(2,1) a(2,2)... a(2,m) ]
 *    [ ...                     ]
 *    [ a(k,1) a(k,2)... a(k,m) ]
 *
 *
 * Author: Sachiko Honda
 *
 *************************************************************************
 */
/*************************************************************************
 *
 * Function: rvecInverse
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin rvecInverse$$
$spell
    namespace std cout endl int rvec const ifdef endif Ex iostream ed cols
$$

$section Retrieve Matrix from Rvec-ed Column Vector$$

$index rvecInverse$$
$index rvec, inverse$$

$table
$bold Prototype:$$   $cend  
$syntax/const DoubleMatrix rvecInverse(const DoubleMatrix &/a/, int /n/)/$$ $rend
$syntax/void rvecInverse(const DoubleMatrix &/a/, int /n/, DoubleMatrix &A)/$$ $rend

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
Given a $math%m times n%$$ dimensional column vector, $code rvecInverse()$$
retrieves a $math%m by n%$$ matrix $italic A$$, such that A(i,j) = a(i*m+j).

$head Arguments$$

$syntax/

/a/
/$$
is a $math%m times n%$$ dimensional column vector.

$syntax/

/m/
/$$
specifies the number of cols in the resulting matrix.  It must be greater than zero.

$syntax/

/A/
/$$
is a reference to a $math%m times n%$$ matrix that will contain the resulting matrix.

$head Example$$
If you compile, link, and run the following program:
$codep
    #include <iostream>
    #include "DoubleMatrix.h"
    #include "rvecInverse.h"

    void rvecInverseEx()
    {
        using namespace std;
        const int m = 2;
        const int n = 3;

        // initialize a matrix A
        DoubleMatrix A(m,n);
        for(int i=0; i<m*n; i++)
        {
            A.data()[i] = i+1;
        }

        // 
        DoubleMatrix a = rvec(A);
        DoubleMatrix C = rvecInverse(a,n);
        cout << "A       = " << A << endl;
        cout << "rvec(A) = " << a << endl;
        cout << "rvec^-1(a," << m << ") = " << C << endl;
    }

$$
then it will display the following when it is run:
$codep
    A       = 3 by 2
    [ 1.0000000000000000e+000 4.0000000000000000e+000 ]
    [ 2.0000000000000000e+000 5.0000000000000000e+000 ]
    [ 3.0000000000000000e+000 6.0000000000000000e+000 ]

    rvec(A) = 6 by 1
    [ 1.0000000000000000e+000 ]
    [ 4.0000000000000000e+000 ]
    [ 2.0000000000000000e+000 ]
    [ 5.0000000000000000e+000 ]
    [ 3.0000000000000000e+000 ]
    [ 6.0000000000000000e+000 ]

    rvec^-1(a,2) = 3 by 2
    [ 1.0000000000000000e+000 4.0000000000000000e+000 ]
    [ 2.0000000000000000e+000 5.0000000000000000e+000 ]
    [ 3.0000000000000000e+000 6.0000000000000000e+000 ]

$$
$end
*/
