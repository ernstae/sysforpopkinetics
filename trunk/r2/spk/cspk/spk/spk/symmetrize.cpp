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
 * File: symmetrize.cpp
 *
 *
 * Create a symmetric matrix based on a lower triangle.
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Function: symmetrize (DoubleMatrix version)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin symmetrize$$
$spell 
   const ifdef Sym Ex Org std cout endl namespace iostream int endif symmetrize
   valarray
$$

$section Symmetrize Matrix$$

$index symmetrize$$
$index matrix, symmetric$$

$table
$bold Prototype:$$   $cend  
$syntax/void symmetrize(const DoubleMatrix& /A/, DoubleMatrix& /B/)/$$
$tend

$bold See also: $$ $xref/symmetrizeVA//valarray version/$$.

$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Fill the upper triangle with the lower triangle values in $math%A%$$. i.e.
$math%

    A(j,i) = A(i,j), where 1 <= i <= n and 1 <= j <= n.
    
%$$
Self-assignment is permitted.

$head Arguments$$

$syntax/

/A/
/$$
is a $math%n by n%$$ matrix that contains values in the lower triangle and the
diagonal to be copied into the upper half.

$syntax/

/B/
/$$
is a $math%n by n%$$ matrix that will contain the resulting symmetric matrix.
If the size fails to match the dimensions, the program terminates.

$head Example$$
If you compile, link, and run the following program:
$codep
        
    #include <iostream>
    #include "symmetrize.h"
    #include "DoubleMatrix.h"

    void symmetrizeEx()
    {
        using namespace std;
        int n = 3;
        DoubleMatrix A(n, n);
        DoubleMatrix B(n, n);
        double* a = A.data();

        A.fill(0);
        for( int j=0; j<n; j++ )
        {
            // fill lower half + diagonal with arbitrary values
            for( int i=j; i<n; i++ )
            {
                a[i+j*n] = i+j+1;
            }        
        }
        // Keep the original
        symmetrize(A, B);
        cout << "original) A=" << A << endl;
        cout << "target)   B=" << B << endl;

        // Self modify
        symmetrize(A, A);
        cout << "modified) A=" << A << endl;            
    }        

$$
then it will display the following when it is run:
$codep
    original) A=3 by 3
    [ 1.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    [ 2.0000000000000000e+000 3.0000000000000000e+000 0.0000000000000000e+000 ]
    [ 3.0000000000000000e+000 4.0000000000000000e+000 5.0000000000000000e+000 ]

    target)   B=3 by 3
    [ 1.0000000000000000e+000 2.0000000000000000e+000 3.0000000000000000e+000 ]
    [ 2.0000000000000000e+000 3.0000000000000000e+000 4.0000000000000000e+000 ]
    [ 3.0000000000000000e+000 4.0000000000000000e+000 5.0000000000000000e+000 ]

    modified) A=3 by 3
    [ 1.0000000000000000e+000 2.0000000000000000e+000 3.0000000000000000e+000 ]
    [ 2.0000000000000000e+000 3.0000000000000000e+000 4.0000000000000000e+000 ]
    [ 3.0000000000000000e+000 4.0000000000000000e+000 5.0000000000000000e+000 ]

$$
$end
*/
#include "DoubleMatrix.h"
#include "symmetrize.h"

void symmetrize(const DoubleMatrix& L, DoubleMatrix& Sym)
{
    int m = L.nr();
    int n = L.nc();
    assert(m==n);
    assert(Sym.nr() == m);
    assert(Sym.nc() == n);

    const double* pL = L.data();
    double*       pS = Sym.data();

    if( &L != &Sym )
    {
        std::copy(pL, pL+m*n, pS);
    }
    int j,i;
    for( j=0; j<n; j++ )
    {
        // Copy the lower triangle data into the resulting matrix's upper triangle
        for( i=0; i<j; i++ )
        {
            pS[i+j*m] = pL[j+i*m];
        }        
    }
}
/*************************************************************************
 *
 * Function: symmetrize (valarray version)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin symmetrizeVA$$
$spell 
   const ifdef Sym Ex Org std cout endl namespace iostream int endif symmetrize
   valarray
   spk
$$

$section Symmetrize Matrix (valarray version)$$

$index symmetrize (valarray)$$
$index matrix, symmetric (valarray)$$

$table
$bold Prototype:$$   $cend  
$syntax/void symmetrize(const SPK_VA::valarray<double>& /A/, int /n/, SPK_VA::valarray<double>& /B/)/$$
$tend

$bold See also: $$ $xref/symmetrize//DoubleMatrix version/$$.
$fend 15$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Fill the upper triangle with the lower triangle values in $math%A%$$. i.e.
$math%

    A(j,i) = A(i,j), where 1 <= i <= n and 1 <= j <= n.
    
%$$
Self-assignment is permitted.

$head Arguments$$

$syntax/
/A/
/$$
is a $code valarray<double>$$ array containing the elements of $math%n by n%$$ square matrix, $math%A%$$
in column-major order.  Only the lower triangle values will be used.
$syntax/

/n/
/$$
specifies $math%n%$$, the number of columns (or rows for this matter) in $math%A%$$.
$syntax/

/B/
/$$
is a $code valarray<double>$$ array of length $math%n * n%$$.
This array will contain the elements of the resulting $math%n by n%$$ symmetric matrix
in column-major order.

$head Example$$
If you compile, link, and run the following program:
$codep
        
    #include <iostream>
    #include "symmetrize.h"
    #include "SpkValarray.h"

    void main()
    {
        using SPK_VA::valarray;
        using namespace std;

        int n = 3;
        valarray<double> A(0.0, n * n);
        valarray<double> B(n * n);

        //
        //     /             \
        // A = |  1   0   0  |
        //     |  2   3   0  |
        //     |  3   4   5  |
        //     \             /
        //
        for( int j=0; j<n; j++ )
        {
            // fill lower half + diagonal with arbitrary values
            for( int i=j; i<n; i++ )
            {
                A[i+j*n] = i+j+1;
            }        
        }

        // Keep the original
        symmetrize(A, n, B);

        cout << "A: " << endl;
        printInMatrix( A, n );
        cout << endl;

        cout << "B: " << endl;
        printInMatrix( B, n );
        cout << endl;

        // Self modify
        symmetrize(A, n, A);
        cout << "A: " << endl;
        printInMatrix( A, n );
        cout << endl;
    }        

$$
then it will display the following when it is run:
$codep
    A:
    [ 1 0 0 ]
    [ 2 3 0 ]
    [ 3 4 5 ]

    B:
    [ 1 2 3 ]
    [ 2 3 4 ]
    [ 3 4 5 ]
    
    A:
    [ 1 2 3 ]
    [ 2 3 4 ]
    [ 3 4 5 ]
$$
$end
*/
#include "SpkValarray.h"

using SPK_VA::valarray;

void symmetrize(const SPK_VA::valarray<double>& L, int n, SPK_VA::valarray<double>& Sym)
{
    assert( L.size() / n * n == L.size() );
    assert( Sym.size() == L.size() );

    if( &L != &Sym )
    {
        Sym = L;
    }
    int j,i;
    for( j=0; j<n; j++ )
    {
        // Copy the lower triangle data into the resulting matrix's upper triangle
        for( i=0; i<j; i++ )
        {
            Sym[i+j*n] = L[j+i*n];
        }        
    }
}
