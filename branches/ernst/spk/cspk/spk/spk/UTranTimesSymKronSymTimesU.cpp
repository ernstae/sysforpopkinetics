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
 * File: UTranTimesSymKronSymTimesU.cpp
 *
 *
 * Computes U^T (V kron V) U for the case where V is symmetric. 
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Function: UTranTimesSymKronSymTimesU
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin UTranTimesSymKronSymTimesU$$
$spell 
    const int namespace std kron tran sym hessian rvec Ui Uj Cij iostream 
    resize cout endl Jth
$$

$section U^T (V kron V) U$$

$index UTranTimesSymKronSymTimesU$$
$index expected hessian, $$

$table
$bold Prototype:$$   $cend  
$syntax/void UTranTimesSymKronSymTimesU(
                   const DoubleMatrix& /V/,     
                   const DoubleMatrix /U/,
                   int k,
                   DoubleMatrix &/C/,
                   DoubleMatrix /A/[]
                   )/$$
$tend
See also $xref/UTranTimesSymKronSymTimesU_x//Derivative of UTranTimesSymKronSymTimesU/$$
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Computes U^T (V kron V) U, 
where $math%V%$$ denotes a $bold symmetric$$ m by m matrix and $math%U%$$ denotes
a $math%m^2 by k%$$ matrix such that each of the columns $math%ui%$$ of $math%U%$$,
for $math%i=1...k%$$, is the $xref/rvec//rvec/$$ of a $bold symmetric$$ matrix which
we denote Ui. Then,
$codep
                   T
  Cij = (rvec(Ui V)) rvec(Uj V)

$$
Upon the successful completion
$head Arguments$$
$syntax/
/V/
/$$
is a m by m symmetric matrix.

$syntax/

/U/
/$$
is a m^2 by k matrix, where each column represents a rvec of a m by m symmetric.

$syntax/

/k/
/$$
indicates the size of $italic U[]$$ array.

$syntax/

/C/
/$$
is a k by k symmetric matrix that will hold the resulting matrix upon the successful completion
of calculation.

$syntax/

/A/[]
/$$
is an array of matrices that will contain matrices $math%Uj*V%$$, for $math%j=1...k%$$, formed during the calculation.

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include "DoubleMatrix.h"
    #include "UTranTimesSymKronSymTimesU.h"
    #include "transpose.h"


    void main()
    {
        using namespace std;

        // Set matrix dimensions
        const int m = 3;
        const int k = 2;

        // Create V such that V is a m by m symmetric matrix
        //
        //   V = [ 1 2 0 ]
        //       [ 2 1 2 ]
        //       [ 0 2 1 ]
        //
        DoubleMatrix V(m,m);
        double* v = V.data();
        v[0] = 1;
        v[1] = 2;
        v[2] = 0;
        v[3] = 2;
        v[4] = 1;
        v[5] = 2;
        v[6] = 0;
        v[7] = 2;
        v[8] = 1;

        // Create an array U such that each element of the array represents a 
        // m by m symmetric matrix.
        //
        //   U[0] = [ 1 0 0 ]
        //          [ 0 1 0 ]
        //          [ 0 0 1 ]
        //   U[1] = [ 0 1 0 ]
        //          [ 1 0 1 ]
        //          [ 0 1 0 ]
        //
        DoubleMatrix U(m*m,k);
        
        DoubleMatrix U0 = identity(m);

        DoubleMatrix U1(m,m);
        U1.fill(0);
        U1[1] = 1;
        U1[3] = 1;
        U1[5] = 1;
        U1[7] = 1;
        replaceJth(U, 0, rvec(U0));
        replaceJth(U, 1, rvec(U1));

        // Create C for holding the answer
        //
        DoubleMatrix C(k,k);

        // Create A array
        std::vector<DoubleMatrix> A;
        A.resize(k);
        DoubleMatrix* a = A.begin();

        UTranTimesSymKronSymTimesU(transpose(U), V, U, k, C, a);

        for( int i=0; i<k; i++ )
            cout << "U(" << i+1 << ")V = " << A[i] << endl;
        cout << endl;

        cout << "U^T (V kron V) U = " << C << endl;
}
$$
then it will display the following when it is run:
$codep

    U(1)V = 3 by 3
    [ 1.0000000000000000e+000 2.0000000000000000e+000 0.0000000000000000e+000 ]
    [ 2.0000000000000000e+000 1.0000000000000000e+000 2.0000000000000000e+000 ]
    [ 0.0000000000000000e+000 2.0000000000000000e+000 1.0000000000000000e+000 ]

    U(2)V = 3 by 3
    [ 2.0000000000000000e+000 1.0000000000000000e+000 2.0000000000000000e+000 ]
    [ 1.0000000000000000e+000 4.0000000000000000e+000 1.0000000000000000e+000 ]
    [ 2.0000000000000000e+000 1.0000000000000000e+000 2.0000000000000000e+000 ]


    U^T (V kron V) U = 2 by 2
    [ 1.9000000000000000e+001 1.6000000000000000e+001 ]
    [ 1.6000000000000000e+001 3.6000000000000000e+001 ]
$$
$end
*/
//#include <vector>
#include "UTranTimesSymKronSymTimesU.h"
#include "DoubleMatrix.h"
#include "getCol.h"
#include "multiply.h"
#include "transpose.h"
#include "rvec.h"
#include "symmetrize.h"
#include "rvecInverse.h"
#include "isDmatEpsEqual.h"

void UTranTimesSymKronSymTimesU(
                   const DoubleMatrix& Sym,  
                   const DoubleMatrix& U,
                   int k,
                   DoubleMatrix &C,
                   DoubleMatrix A[]
                   )       
{
    // Computes C = U'(V kron V) U for a special case:
    //
    // Given,
    // V=Sym: m by m symmetric
    // U:     m*m (rvec of m by m symmetric matrices) by k matrix
    //
    // 
    // C(i,j) = (rvec(U(i)V))' rvec(U(j)V), where i=j for symmetric V
    //
    // When neither Ui nor V is symmetric,
    // C(i,j) = (rvec(U(i)'V))' rvec(V'U(j)')
    //
    // The result is stored in C, and intermediate matrices Aj is returned through *A
    // A must have k elements space already allocated.
    //
    //
    const int m  = Sym.nr();
    const int mm = m*m;
    if( k == 0 )
        return;
    assert(m > 0);

    assert(Sym.nc() == m);

    int i,j;

    C.resize(k,k);
    double* pC = C.data();

    DoubleMatrix Uj(m,m);
    DoubleMatrix Cj(k,1);
    DoubleMatrix Aj(m,m);

    DoubleMatrix Ui(m,m);
    DoubleMatrix Ai(m,m);
    DoubleMatrix Cij(1,1);
    double* pCij = Cij.data();

    DoubleMatrix rvecAiTran;
    DoubleMatrix AjTran;
    
    for( j=0; j<k; j++ )
    {
        rvecInverse(getCol(U,j), m, Uj);
        multiply(Uj, Sym, Aj);
        assert(Aj.nr()==m);
        assert(Aj.nc()==m);

        A[j] = Aj;
    }
    
    for( j=0; j<k; j++ )
    {
        // Do the computation only for the lower triangle.
        for( i=j; i<k; i++ )
        {
            transpose(rvec(A[i]), rvecAiTran);
            transpose(A[j], AjTran);

            multiply(rvecAiTran, rvec(AjTran), Cij);
            assert(Cij.nr()==1);
            assert(Cij.nc()==1);

            // Cij is a 1 by 1 matrix. So, just access the first element.
            pC[i+j*k] = *pCij;
        }
    }
    // Copy the values in the lower triangle to the upper half.
    symmetrize(C, C);
}
