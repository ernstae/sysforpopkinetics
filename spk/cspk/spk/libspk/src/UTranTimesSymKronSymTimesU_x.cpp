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
 * File: UTranTimesSymKronSymTimesU_x.cpp
 *
 *
 * Computes the derivative of U^T (V kron V) U with respect to x,
 * for the case where V is symmetric. 
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Function: UTranTimesSymKronSymTimesU_x
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin UTranTimesSymKronSymTimesU_x$$
$spell
    const int namespace std kron tran sym hessian of rvec symmetrize
    Cij Uj Ui th cmath Jth Ex cout endl resize xj Ith
    xi
$$

$section Derivative of U^T (V kron V) U With Respect to x$$

$index UTranTimesSymKronSymTimesU_x$$
$index expected hessian, derivative$$

$table
$bold Prototype:$$   $cend  
$syntax/void UTranTimesSymKronSymTimesU_x(
                   const DoubleMatrix& /V/,
                   const DoubleMatrix& /V_x/,
                   const DoubleMatrix& /U/,
                   const DoubleMatrix& /U_x/,
                   int /p/,
                   const DoubleMatrix /A/[],
                   DoubleMatrix& /C_x/
                   )/$$
$tend
See also $xref/UTranTimesSymKronSymTimesU//Original Function UTranTimesSymKronSymTimesU/$$

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Computes the derivative of U^T (V kron V) U with respect to x, 
where V denotes a square m by m matrix and U denotes
a m^2 by k matrix, for a special case where $bold V is symmetric$$.  m must be greater
than zero and k must be greater than or equal to zero. x contains p number of variables.
$pre

$$
Upon a successful completion, the resulting array of matrix, $italic C_x[]$$ will
contain values based upon the following computation:
$codep
                          T
    Cij_x = (rvec(Ui * V))  {rvec(V * Uj_x}
                             T                   T
          + {(rvec(Ui * V_x))  + (rvec(Ui_x * V))  } * rvec(V Uj)

$$
where Ui is a m by m matrix such that U[i] = rvec(Ui), where i points to the i-th column of U.  
Similarly, Uj is a matrix retrieved from the j-th column of U. 

$head Arguments$$

$syntax/
/V/
/$$
is a m by m symmetric matrix.

$syntax/

/V_x/
/$$
is a m^2 times p matrix, where each column represents a rvec of a derivative of V with respect the i-th component of x.

$syntax/

/U/
/$$
is m * m by k matrix, where each column represents a rvec of a symmetric matrix.

$syntax/

/U_xTran/
/$$
is a m^2*p by k matrix such that if dUi is the i-th column of U_xTran, rvecInverse(U_xTran, p) = Ui_x, where Ui_x is
a m*m by p matrix that denotes a rvec of the derivative of rvecInverse(U[i],m) with respect to x.
In the case that U = f(x,y)_y, for example, the argument U_xTran is f(x,y)_$bold x_y$$, not f(x,y)_y_x.

$syntax/

/p/
/$$
indicates the number of components in x.

$syntax/

/A/[]
/$$
is a k sized array of matrices $math%Uj * V%$$, for 0 < j <= k, formed during the calculation of the original function.

$syntax/

/C_x/
/$$
is a k*k by p matrix that will hold the resulting derivatives upon the successful completion of the calculation.
The ith column of C_x will contain a rvec of the derivative of C=U^T(V kron V)U with respect to x(1).

$head Example$$
If you compile, link, and run the following program:

$codep
    #include <vector>
    #include <cmath>
    #include "transpose.h"
    #include "multiply.h"
    #include "replaceJth.h"
    #include "DBL_EPS_EQUAL_MULT.h"
    #include "symmetrize.h"
    #include "add.h"
    #include "rvecInverse.h"
    #include "getCol.h"
    #include "subtract.h"


    void UTranTimesSymKronSymTimesU_xTest::testUTranTimesSymKronSymTimesU_xEx()
    {
        using namespace std;
        int i,j,l,o;

        const int m = 3;
        const int k = 2;
        const int p = 2;

        // Initialize V:symmetric m by m 
        DoubleMatrix V(m,m);
        V.fill(0);
        for( j=0; j<m; j++ )
        {
            for( i=j; i<m; i++ )
            {
                V.data()[i+j*m] = i+j*m;
            }
        }
        symmetrize(V,V);
    
        cout << "V:" << endl;
        V.print();
        cout << endl;
    

        // Initialize U: m*m (rvec of symmetric) by k matrix
        DoubleMatrix U(m*m, k);
        DoubleMatrix Ui(m,m);
        for( l=0; l<k; l++ )
        {
            for( j=0; j<m; j++ )
            {
                for( i=j; i<m; i++ )
                {
                    Ui.resize(m,m);
                    Ui.data()[i+j*m] = i+j*m+l;
                }
            }
            symmetrize(Ui, Ui);
            replaceJth(U, l, rvec(Ui));
        }
    
        cout << "U:" << endl;
        U.print();
        cout << endl;
    

        // Create C: k by k
        DoubleMatrix C(k,k);
        C.fill(0);

        // Create A[]: k number of m by m
        vector<DoubleMatrix> A(k);

        UTranTimesSymKronSymTimesU(V, U, k, C, A.begin());
    
        cout << "C:" << endl;
        C.print();
        cout << endl;
        cout << "A:" << endl;

        for( i=0; i<A.size(); i++ )
        {
            cout << "A[" << i << "]=" << endl;
            A[i].print();
        }
        cout << endl;

        // Initialize V_x: m*m (rvec of symmetric) by p matrix
        DoubleMatrix V_x(m*m,p);
        DoubleMatrix V_xi(m,m);
        for(l=0; l<p; l++)
        {
            for( j=0; j<m; j++ )
            {
                for( i=j; i<m; i++ )
                {
                    V_xi.resize(m,m);
                    V_xi.data()[i+j*m] = i+j*m-l;
                }
            }
            symmetrize(V_xi, V_xi);
            replaceJth(V_x, l, rvec(V_xi));
        }

        cout << "V_x: " << endl;
        V_x.print();
        cout << endl;
    
        // Initialize U_x: m*m*k by p matrix
        DoubleMatrix U_x(m*m*k,p);
        DoubleMatrix U_xi(m*m,k);
        DoubleMatrix Ui_x(m*m,k);
        DoubleMatrix Ui_xj(m,m);
        for(o=0; o<p; o++)
        {
            for(l=0; l<k; l++)
            {
                for( j=0; j<m; j++ )
                {
                    for( i=j; i<m; i++ )
                    {
                        Ui_xj.resize(m,m);
                        Ui_xj.data()[i+j*m] = i+j*m-2*l;
                    }
                }
                symmetrize(Ui_xj, Ui_xj);
                replaceJth(Ui_x, l, rvec(Ui_xj));
            }
            replaceJth(U_x, o, rvec(Ui_x));
        }
    
        cout << "U_x:" << endl;
        U_x.print();
        cout << endl;
    

        // Create C_x: k*k by p
        DoubleMatrix C_x(k*k, p);

        // Finally, call the derivative function.
        UTranTimesSymKronSymTimesU_x(V,V_x, U, U_x, p, A.begin(), C_x);

        cout << "C_x: " << endl;
        C_x.print();
        cout << endl;
    }

$$
then it will display the following when it is run:
$codep

    V:
    [ 0 1 2 ]
    [ 1 4 5 ]
    [ 2 5 8 ]

    U:
    [ 0 1 ]
    [ 1 2 ]
    [ 2 3 ]
    [ 1 2 ]
    [ 4 5 ]
    [ 5 6 ]
    [ 2 3 ]
    [ 5 6 ]
    [ 8 9 ]

    C:
    [ 19400 23340 ]
    [ 23340 28064 ]

    A:
    A[0]=
    [  5 14 21 ]
    [ 14 42 62 ]
    [ 21 62 93 ]
    A[1]=
    [  8 24  36 ]
    [ 17 52  77 ]
    [ 24 72 108 ]

    V_x:
    [ 0 -1 ]
    [ 1  0 ]
    [ 2  1 ]
    [ 1  0 ]
    [ 4  3 ]
    [ 5  4 ]
    [ 2  1 ]
    [ 5  4 ]
    [ 8  7 ]

    U_x:
    [  0  0 ]
    [ -2 -2 ]
    [  1  1 ]
    [ -1 -1 ]
    [  2  2 ]
    [  0  0 ]
    [  1  1 ]
    [ -1 -1 ]
    [  4  4 ]
    [  2  2 ]
    [  5  5 ]
    [  3  3 ]
    [  2  2 ]
    [  0  0 ]
    [  5  5 ]
    [  3  3 ]
    [  8  8 ]
    [  6  6 ]

    C_x:
    [  77600 53960 ]
    [  89420 62208 ]
    [  89420 62208 ]
    [ 103026 71520 ]


$$
$end
*/
//
// These pragma are to disable "too long literals" warnings generated
// as a result of using STL component, std::vector.
//
#pragma warning( disable : 4786 )  
#include <vector>

#include "UTranTimesSymKronSymTimesU_x.h"
#include "DoubleMatrix.h"
#include "getCol.h"
#include "multiply.h"
#include "transpose.h"
#include "rvec.h"
#include "symmetrize.h"
#include "identity.h"
#include "AkronItimesC.h"
#include "IkronBtimesC.h"
#include "replaceJth.h"
#include "add.h"
#include "rvecInverse.h"
#include "replaceIth.h"

static DoubleMatrix Im(__FILE__);
static DoubleMatrix Cij_x(__FILE__);
static DoubleMatrix term01(__FILE__);
static DoubleMatrix term02(__FILE__);
static DoubleMatrix term1(__FILE__);
static DoubleMatrix term2(__FILE__);
static DoubleMatrix term3(__FILE__);
static DoubleMatrix term4(__FILE__);
static DoubleMatrix term12(__FILE__);
static DoubleMatrix term34(__FILE__);
static DoubleMatrix rvecAiTran(__FILE__);
static DoubleMatrix rvecAjTran(__FILE__);

void UTranTimesSymKronSymTimesU_x(
                   const DoubleMatrix& V,     // m by m symmetric
                   const DoubleMatrix& V_x,   // m * m by p where each column represents a rvec of symmetric matrix
                   const DoubleMatrix& U,     // m * m symmetric by k
                   const DoubleMatrix& Ux_y,  // m*m*p by k where each column represents a rvec of U[i]_x
                   int p,
                   const DoubleMatrix A[],
                   DoubleMatrix &C_x          // k*k(rvec of symmetric) by p matrix
                   )        

{
    using namespace std;
    if( p < 1 )
        return;
    const int m = V.nr();
    assert(V.nc() == m);
    const int mm = m*m;
    const int k = U.nc();
    if( k < 1 )
        return;

    assert(U.nr() == mm);
    assert(Ux_y.nr() == mm*p);
    assert(Ux_y.nc() == k);

    C_x.fill(0);
    int i,j;

    Im = identity(m);

    std::vector<DoubleMatrix> Uarray;
    std::vector<DoubleMatrix> Ux_yArray;
    Uarray.resize(k);
    Ux_yArray.resize(k);

    std::vector<DoubleMatrix>::iterator itrUarray    = Uarray.begin();
    std::vector<DoubleMatrix>::iterator itrUx_yArray = Ux_yArray.begin();

    for(i=0; i<k; i++)
    {
        rvecInverse(getCol(U,i), m, *(itrUarray+i));
        rvecInverse(getCol(Ux_y,i), p, *(itrUx_yArray+i));
        assert((*(itrUx_yArray+i)).nr() == mm);
        assert((*(itrUx_yArray+i)).nc() == p);
    }

    for(j=0; j<k; j++)
    {
        //cout << "j=" << j << endl;

        assert(A[j].nr() == m);
        assert(A[j].nc() == m);
        transpose(rvec(A[j]), rvecAjTran);
        assert(rvecAjTran.nr() == 1);
        assert(rvecAjTran.nc() == mm);

        AkronItimesC(V, Im, *(itrUx_yArray+j), term1);
        assert(term1.nr() == mm);
        assert(term1.nc() == p);

        IkronBtimesC(Im, *(itrUarray+j), V_x, term2);
        assert(term2.nr() == mm);
        assert(term2.nc() == p);

        add(term1,term2, term12);

        for(i=j; i<k; i++)
        {
            assert(A[i].nr() == m);
            assert(A[i].nc() == m);
            transpose(rvec(A[i]), rvecAiTran);
            assert(rvecAiTran.nr() == 1);
            assert(rvecAiTran.nc() == mm);
            
            AkronItimesC(*(itrUarray+i), Im, V_x, term3);
            assert(term3.nr() == mm);
            assert(term3.nc() == p);

            IkronBtimesC(Im, V, *(itrUx_yArray+i), term4);
            assert(term4.nr() == mm);
            assert(term4.nc() == p);
            
            add(term3,term4, term34);

            multiply(rvecAiTran, term12, term01);
            multiply(rvecAjTran, term34, term02);

            add(term01, term02, Cij_x);
            assert(Cij_x.nr() == 1);
            assert(Cij_x.nc() == p);

            replaceIth(C_x, i+j*k, Cij_x);
            replaceIth(C_x, j+i*k, Cij_x);

            /*
            cout << "term1=" << term1 << endl;
            cout << "term2=" << term2 << endl;
            cout << "term01=" << term01 << endl;
            cout << "term3=" << term3 << endl;
            cout << "term4=" << term4 << endl;
            cout << "term02=" << term02 << endl;
            cout << "Cij_x=" << Cij_x << endl;
            */
        }        
    }
}
