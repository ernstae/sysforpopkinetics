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
 * File: backDiv.cpp
 *
 *
 * Solve A x = B for x, using LU decomposition.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: backDiv
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin backDiv$$
$spell 
    div 
    const 
    lu 
    afc 
    Spk
    mult 
    ajc 
    rhs 
    namespace 
    std 
    int 
    iostream 
    pd 
    cout 
    endl
    div
    mul

$$

$section Solve a system of equations: A x = B$$

$index backDiv$$
$index matrix, division$$
$index matrix, matrix back division$$

$table
$bold Prototype:$$   $cend  
$syntax/DoubleMatrix backDiv( const DoubleMatrix &/A/, const DoubleMatrix &/B/ )/$$
$tend

See also: $xref/addition//addition/$$, $xref/subtraction//subtraction/$$, $xref/multiply//matrix multiply/$$,
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
Solve $math%A x = B%$$ for $italic x$$, which is often expressed as $math%A \ B = x%$$, and
return $italic x$$.
$italic A$$ is first decomposed into lower, $math%L%$$, and upper, $math%U%$$, 
triangular matrices by Nag's nag_real_lu (f03afc) and 
fed into nag_real_lu_solve_mult_rhs (f04ajc) to solve for 
$italic x$$.
$pre

$$
If either A or B is empty, an empty matrix will be returned.
$pre

$$
This routine assumes the input $italic A$$ matrix is positive definite.  If not
it will terminate the program.

$head Arguments$$

$syntax/

&/A/
/$$
is a $math%n by n%$$ representing the system of equations.

$syntax/

&/B/
/$$
is a $math%m by n%$$ matrix representing the values of $math%A x%$$.

$head Example$$
If you compile, link, and run the following program:
$codep
    #include <iostream>
    #include "DoubleMatrix.h"
    #include "backDiv.h"

    static void main(){

        using namespace std;

        const int m = 1;
        const int n = 3;
    
        DoubleMatrix A(n,n);    // A must be positive definite
        DoubleMatrix B(n,m);    // B could be anything
        DoubleMatrix x(n,m);    // unknown parameter we want to solve

        double *pdA = A.data();
        double *pdB = B.data();

        int i;

        // Set A to a matrix:
        //  [ 1  4  2 ]
        //  [ 2  5  3 ]
        //  [ 3  1  4 ]
        for( i=0; i<n*n; i++ )
            pdA[i] = i % 5 + 1;

        // Set B to a vector:
        //  [ 1 ]
        //  [ 1 ]
        //  [ 1 ]
        for( i=0; i<m*n; i++ )
            pdB[i] = 1;

        x = backDiv(A,B);

        cout << "A \\ B = x = " << endl;
        x.print();
        cout << endl;
        cout << "A x (should be equal to B) = " << endl;
        (A*x).print();
    }
$$
then it will display the following when it is run:
$codep

    A \ B = x =
    [-1]
    [0]
    [1]

    A x (should be equal to B) =
    [1]
    [1]
    [1]

$$
$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * Nag assumes matrices are stored in the row-major order.  Since
 * what we have in DoubleMatrix is in the column-major order,
 * we have to transpose inputs to and output from the last nag
 * routine.
 *      
 *
 * Given A x = B and 
 * A is positive definite, but not necessary symmetric.
 * 
 * -> A is invertible
 * -> A has a LU decomposition
 * -> A x = B has a unique solution
 * 
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <cassert>
#include "backDiv.h"
#include "transpose.h"
#include "DoubleMatrix.h"

/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

static DoubleMatrix INagRealPosDefLUbasedSolvingEqs(
    DoubleMatrix &dmatA,
    DoubleMatrix &dmatB);

static DoubleMatrix toDM( const double *pdblData, const int nr, const int nc );

static DoubleMatrix dmatTransA(__FILE__);
static DoubleMatrix dmatTransB(__FILE__);
static DoubleMatrix dmatTransX(__FILE__);
static DoubleMatrix dmatX(__FILE__);
/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

DoubleMatrix backDiv(const DoubleMatrix &dmatA, const DoubleMatrix &dmatB)
{
    // assume A is square
    assert( dmatA.nr() == dmatA.nc() );
    // assume #cols in A == #rows in B
    assert( dmatA.nc() == dmatB.nr() );
    if( dmatA.isEmpty() || dmatB.isEmpty() )
    {
        return DoubleMatrix(0,0/*dmatB.nc()*/);
    }

    const int iAnr = dmatA.nr();
    const int iAnc = dmatA.nc();
    const int iBnr = dmatB.nr();
    const int iBnc = dmatB.nc();

    transpose(dmatA, dmatTransA);
    transpose(dmatB, dmatTransB); 

    dmatTransX = INagRealPosDefLUbasedSolvingEqs( dmatTransA, dmatTransB );
    transpose(dmatTransX, dmatX);

    return dmatX;
}


/*========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 *========================================================================*/

/*************************************************************************
 *
 * Function: INagRealPosDefLUbasedSolvingEqs
 *
 * 
 * Description
 * -----------
 *
 * For LU decomposition, this routine uses nag_real_lu (f03afc).
 * For solving a system of equations using the LU-decomposed matrix,
 * this routine uses nag_real_lu_solve_mult_rhs (f04ajc).
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
#include <iostream>

#include "nag.h"
#include "nag_types.h"
#include "nag_stdlib.h"
#include "nagf03.h"
#include "nagf04.h"

static DoubleMatrix INagRealPosDefLUbasedSolvingEqs(
    DoubleMatrix &dmatTransA, 
    DoubleMatrix &dmatTransB)
{
 
	double *pdTransAdata = dmatTransA.data();
    double *pdTransBdata = dmatTransB.data();
    const  int iAdim     = dmatTransA.nc(); // transA is n by n
    const  int iTransBnr = dmatTransB.nr(); // transB is n by m 
    const  int iTransBnc = dmatTransB.nc(); // transB is n by m

    // Integer n
    // Input: n, the order of the matrix A
    const Integer n = iAdim;
    assert( n >= 1 );

    // double a[n][tda]
    // Input: n by n matrix
    // Output: A is overwritten by the lower trinagular matrix L and 
    // the off-diagonal elements of the upper triangular matrix U.
    // The unit diagonal elements of U are not stored.
    double *a = pdTransAdata;

    // Integer tda
    // Input: the last dimension of the matrix A as declared in
    // the function from which nag_real_lu is called.
    // constraint: tda >= n
    const Integer tda = n;
    assert( tda >= n );

    // Integer pivot[n]
    // Output: pivot[i-1] gives the row index of the ith pivot.
    Integer *pivot = new Integer[n+1];

    // double detf
    // Interger dete
    // Output: the determinant of A is given by detf * 2.0^dete.
    double  detf = 0.0;
    Integer dete = 0;

    nag_real_lu( n, a, tda, pivot, &detf, &dete, NAGERR_DEFAULT);

    // Integer nrhs
    // Input: the number of right-hand sides
    Integer nrhs = iTransBnr;
    assert( nrhs >= 1 );

    // double b[n][tdb]
    // Input: the n by tdb right-hand side matrix B
    double *b = pdTransBdata;

    // Integer tdb
    // Input: the last dimension of the array b as declared 
    // in the function from which nag_real_lu_solve_mult_rhs is called.
    Integer tdb =iTransBnr;
    assert( tdb >= nrhs );

    nag_real_lu_solve_mult_rhs(n,nrhs,a,tda,pivot,b,tdb,NAGERR_DEFAULT);

    DoubleMatrix dmatTransX = toDM(b, dmatTransB.nr(), dmatTransB.nc());
    delete [] pivot;
    return dmatTransX;
}

static DoubleMatrix toDM( const double *data, const int nr, const int nc )
{
    DoubleMatrix dmatA(nr,nc);
    double *pdA = dmatA.data();

    std::copy(data, data+nr*nc, pdA);
    
    return dmatA;
}
