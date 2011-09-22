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
/***************************************************************
 *
 * elsq.cpp
 *
 * Implementation for Extended Least Square function
 *
 * Author: Sachiko Honda
 *
 ***************************************************************
 
//**************************************************************
// Function: elsq (DoubleMatrix version)
//**************************************************************
/*
---------------------------------------------------------------
   Function Specification
---------------------------------------------------------------
*/
/*
$begin elsq$$
$escape &$$
$spell 
    elsq
    Qinv
    Qi
    kron
    inv
    det
    ans
    endl
    cout
    nr
    nc
    int
    iostream
    dmat
    cassert
    std
    namespace
    yi
    th
    pz
    ph
    Spk
    bool
    covariance
    cov
$$

$section Extended Least Squares Function$$

$index elsq, $$

$table
$bold prototype$$ $cend 
$syntax/double elsq(
    DoubleMatrix &/z/, 
    DoubleMatrix &/h/, 
    DoubleMatrix &/Q/, 
    DoubleMatrix &/Qinv/)/$$ $rend
$bold prototype$$ $cend 
$syntax/double elsq(
    DoubleMatrix &/r/, 
    DoubleMatrix &/Q/, 
    DoubleMatrix &/Qinv/)/$$ $rend

$tend
See also $xref/elsq_x//elsq_x()/$$
$pre
$$
See also $tref elsqFuncOb$$.

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The routine $code elsq$$ computes the
extended least squares function 
$math%g(x)%$$
which is defined by
$math%
            1  %          %             1            T  -1
    g(x) = --- &logdet[ 2 &pi Q(x) ] + --- [z - h(x)]  Q (x) [z - h(x)]
            2  %          %             2
%$$
$math%x%$$ and $math%z%$$ are column vectors,
$math%Q(x)%$$ is a symmetric, positive definite matrix valued function,
and $math%h(x)%$$ is a column vector valued function.
$pre

$$

$head Reference$$
See Lemma 11 of 
$italic Approximating the maximum likelihood estimate 
for models with random parameters$$.

$head Return Value$$
Upon a successful completion, this function returns 
the value $math%g(x)%$$ as a double-precision scalar.

$head Arguments$$

$syntax//z/
/$$
is a $math%m%$$ dimensional column vector.
In the context of population analysis, this corresponds to the data vector (yi).

$syntax/

/h/
/$$
is a $math%m%$$ dimensional column vector 
containing the value of $math%h(x)%$$.
It has the same number of rows as $italic z$$.
In the context of population analysis, this corresponds to the vector
value returned by evaluating the model for the variance of i-th data at 
$math%x%$$.

$syntax/

/r/
/$$
is a $math%m%$$ dimensional column vector 
containing the value of $math%z - h(x)%$$.

$syntax/

/Q/
/$$
is a $math%m%$$ by $math%m%$$ 
square matrix containing the value of $math%Q(x)%$$
corresponding to the current value of $math%x%$$.
It has the same row dimension as $italic z$$.
In the context of population analysis, this corresponds to the
symmetric matrix returned by evaluating the model for the
variance of the i-th individual's data at $math%x%$$.

$syntax/

/Qinv/
/$$
is a 
square matrix containing the matrix inverse of $italic Q$$.
It has the same dimensions a $italic Q$$.



$head Example$$

If you compile, link and run the following program;
$codep
    #include <iostream>
    #include <cassert>
    #include "DoubleMatrix.h"
    #include "elsq.h"
    #include "inv2by2.h"
    #include "det.h"

    void main(){

        using namespace std;

        int i;
        DoubleMatrix z(2,1);  // column vector
        DoubleMatrix h(2,1);  // column vector
        DoubleMatrix Q(2,2);  // symmetric and invertible
        DoubleMatrix Qinv(2,2);

        double *pz = z.data();
        double *ph = h.data();
        double *pQ = Q.data();
        double ans;

        // Set Q to a matrix
        //   [ 2.0  1.0 ]
        //   [ 1.0  3.0 ]
        pQ[0] = 2.0;
        pQ[1] = 1.0;
        pQ[2] = 1.0;
        pQ[3] = 3.0;

        // Compute inverse of Q
        Qinv = inverse(Q);

        // Set h and z to a matrix
        //   [ 1.0 ]
        //   [ 1.0 ]
        for( i=0; i<2; i++ ){
            pz[i] = 1.0;
            ph[i] = 1.0;
        }

        cout << "Q = " << endl;
        Q.print();
        
        cout << "Qinv = " << endl;
        Qinv.print();

        // Compute elsq and display the result
        ans = elsq(z, h, Q, Qinv);
        cout << "elsq = " << ans << endl;
$$
the program will display;
$codep

    Q =
    [ 2  1 ]
    [ 1  3 ]

    Qinv =
    [  0.6 -0.2 ]
    [ -0.2  0.4 ]

    elsq = 2.6426

$$
$end
*/
/*
---------------------------------------------------------------
   Function Implementation
---------------------------------------------------------------
*/
#include <cassert>
#include <cmath>
#include "elsq.h"
#include "det.h"
#include "pi.h"
#include "DoubleMatrix.h"
#include "subtract.h"

static const double log2pi = log(PI*2.0);
static const double log2s   = log( 2.0 );
static DoubleMatrix dvecR(__FILE__);
double elsq(
            const DoubleMatrix &dvecZ,     // column vector : yi
            const DoubleMatrix &dvecH,     // column vector : fi()
            const DoubleMatrix &dmatQ,     // symmetric matrix Q(x) : Ri()
            const DoubleMatrix &dmatInvQ   // inverse of Q : doDataVarianceInv()
            )
{
    assert( dvecZ.nc() == 1 );
    assert( dvecH.nc() == 1 );
    assert( dvecZ.nr() == dvecH.nr() );
    assert( dmatQ.nr() == dvecZ.nr() );
    assert( dmatQ.nr() == dmatQ.nc() );

    int   m = dmatInvQ.nr();    // == DmatInvQ.nc()
    int   i,j;

    // Compute b and c such that det(Q) = b * 2^c.
    double b = 0.0;
    long int c   = 0;
    det( dmatQ , &b, &c );
    assert( b > 0.0 );

    double  logdetQ = log( b ) + c * log2s;
    double  sum  = m * log2pi + logdetQ;
    double  val;
    subtract( dvecZ, dvecH, dvecR );
    
    //DoubleMatrix term2 = multiply(multiply(transpose(dvecR), dmatInvQ), dvecR);
    //dTerm2 = oneByOneToScalar(term2);
    /*
     * The loop below untangles the following two statements
     * for eliminating matrix object constructions and taking advantage of
     * Q being symmetric.
     *
     *    DoubleMatrix term2 = multiply(multiply(transpose(dvecR), dmatInvQ), dvecR);
     *    dTerm2 = oneByOneToScalar(term2);
     *
     * Given Q being symmetric, for x^T Q x, the following is true:
     *    x(j)Q(i,j)x(i) = x(j)Q(j,i)x(i)
     *
     * Thus, we can eliminate #of arithmatics by computing only a half + diagonal elements.
     *
     */
    const double* pInvQ = dmatInvQ.data();
    const double* pR    = dvecR.data();
    for( j=0; j<m; j++ )
    {
        for( i=0; i<m; i++ )
        {
            if( i<=j )
            {
                val = pInvQ[j*m+i]*pR[i]*pR[j];
                sum += val;
                if(i<j)
                    sum += val;
            }
        }
    }

    return sum / 2.0;
}

double elsq(
            const DoubleMatrix &dvecR,     // column vector : residual (yi - fi)
            const DoubleMatrix &dmatQ,     // symmetric matrix Q(x) : Ri()
            const DoubleMatrix &dmatInvQ   // inverse of Q : doDataVarianceInv()
            )
{
    assert( dvecR.nc() == 1 );
    assert( dmatQ.nr() == dvecR.nr() );
    assert( dmatQ.nr() == dmatQ.nc() );

    int   m = dmatInvQ.nr();    // == DmatInvQ.nc()
    int   i,j;

    // Compute b and c such that det(Q) = b * 2^c.
    double b = 0.0;
    long int c   = 0;
    det( dmatQ , &b, &c );
    assert( b > 0.0 );

    double  logdetQ = log( b ) + c * log2s;
    double  sum  = m * log2pi + logdetQ;
    double  val;
    
    //DoubleMatrix term2 = multiply(multiply(transpose(dvecR), dmatInvQ), dvecR);
    //dTerm2 = oneByOneToScalar(term2);
    /*
     * The loop below untangles the following two statements
     * for eliminating matrix object constructions and taking advantage of
     * Q being symmetric.
     *
     *    DoubleMatrix term2 = multiply(multiply(transpose(dvecR), dmatInvQ), dvecR);
     *    dTerm2 = oneByOneToScalar(term2);
     *
     * Given Q being symmetric, for x^T Q x, the following is true:
     *    x(j)Q(i,j)x(i) = x(j)Q(j,i)x(i)
     *
     * Thus, we can eliminate #of arithmatics by computing only a half + diagonal elements.
     *
     */
    const double* pInvQ = dmatInvQ.data();
    const double* pR    = dvecR.data();
    for( j=0; j<m; j++ )
    {
        for( i=0; i<m; i++ )
        {
            if( i<=j )
            {
                val = pInvQ[j*m+i]*pR[i]*pR[j];
                sum += val;
                if(i<j)
                    sum += val;
            }
        }
    }

    return sum / 2.0;
}
//**************************************************************
// Function: elsq (valarray version)
//**************************************************************
/*
---------------------------------------------------------------
   Function Specification
---------------------------------------------------------------
*/
/*
$begin elsqVA$$
$escape &$$
$spell 
    elsq
    Qinv
    Qi
    kron
    inv
    det
    ans
    endl
    cout
    nr
    nc
    int
    iostream
    dmat
    cassert
    std
    namespace
    yi
    th
    pz
    ph
    Spk
    bool
    covariance
    cov
    Spk
    valarray
$$

$section Extended Least Squares Function$$

$index elsq, $$

$table
$bold prototype$$ $cend 
$syntax/double elsq(
    SPK_VA::valarray &/z/, 
    SPK_VA::valarray &/h/, 
    SPK_VA::valarray &/Q/, 
    SPK_VA::valarray &/Qinv/)/$$ $rend

$tend
See also $xref/elsq_xVA//elsq_x()/$$
$pre
$$
See also $tref elsqVA$$.

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The routine $code elsq$$ computes the
extended least squares function 
$math%g(x)%$$
which is defined by
$math%
            1  %          %             1            T  -1
    g(x) = --- &logdet[ 2 &pi Q(x) ] + --- [z - h(x)]  Q (x) [z - h(x)]
            2  %          %             2
%$$
$math%x%$$ and $math%z%$$ are column vectors,
$math%Q(x)%$$ is a symmetric, positive definite matrix valued function,
and $math%h(x)%$$ is a column vector valued function.
$pre

$$

$head Reference$$
See Lemma 11 of 
$italic Approximating the maximum likelihood estimate 
for models with random parameters$$.

$head Return Value$$
Upon a successful completion, this function returns 
the value $math%g(x)%$$ as a double-precision scalar.

$head Arguments$$
$syntax/
/z/
/$$
is a $code valarray<double>$$ object containing the value of $math%n%$$ dimensional column vector, $math%z%$$.
In the context of population analysis, this corresponds to the data vector (yi).

$syntax/

/h/
/$$
is a $code valarray<double>$$ object containing the value of $math%n%$$ dimensional column vector, $math%h%$$.
In the context of population analysis, this corresponds to the vector
value returned by evaluating the model for the variance of i-th data at 
$math%x%$$.

$syntax/

/Q/
/$$
is a $code valarray<double>$$ object containing the value of $math%n by n%$$ 
square matrix, $math%Q(x)%$$,
corresponding to the current value of $math%x%$$.
In the context of population analysis, this corresponds to the
symmetric matrix returned by evaluating the model for the
variance of the i-th individual's data at $math%x%$$.

$syntax/

/Qinv/
/$$
is a $code valarray<double>$$ object containing the value of the inverse of $math%Q%$$, 
which is a $math%n by n%$$ 
square matrix.


$head Example$$

If you compile, link and run the following program;
$codep
    #include <iostream>
    #include <cassert>
    #include "SpkValarray.h"
    #include "elsq.h"
    #include "inv2by2.h"
    #include "det.h"

    using SPK_VA::valarray;

    void main(){

        using namespace std;

        int i;
        valarray<double> z( 2 );
        valarray<double> h( 2 );
        valarray<double> Q( 2 * 2 );
        valarray<double> Qinv( 2 * 2 );

        double ans;

        // Set Q to a matrix
        //   [ 2.0  1.0 ]
        //   [ 1.0  3.0 ]
        Q[0] = 2.0;
        Q[1] = 1.0;
        Q[2] = 1.0;
        Q[3] = 3.0;

        // Compute inverse of Q
        Qinv = inverse(Q, n);

        // Set h and z to a matrix
        //   [ 1.0 ]
        //   [ 1.0 ]
        for( i=0; i<2; i++ ){
            z[i] = 1.0;
            z[i] = 1.0;
        }

        cout << "Q = " << endl;
        DoubleMatrix( Q, 2 ).print();
        
        cout << "Qinv = " << endl;
        DoubleMatrix( Qinv, 2 ).print();

        // Compute elsq and display the result
        ans = elsq(z, h, Q, Qinv);
        cout << "elsq = " << ans << endl;
$$
the program will display;
$codep

    Q =
    [ 2  1 ]
    [ 1  3 ]

    Qinv =
    [  0.6 -0.2 ]
    [ -0.2  0.4 ]

    elsq = 2.6426

$$
$end
*/
/*
---------------------------------------------------------------
   Function Implementation
---------------------------------------------------------------
*/
#include "SpkValarray.h"

using SPK_VA::valarray;
using SPK_VA::slice;


double elsq(
            const valarray<double> &z,     // column vector : yi
            const valarray<double> &h,     // column vector : fi()
            const valarray<double> &Q,     // symmetric matrix Q(x) : Ri()
            const valarray<double> &invQ   // inverse of Q : doDataVarianceInv()
            )
{
    const int n = z.size();
    assert( h.size() == n );
    assert( Q.size() == n * n );

    int   i,j;

    // Compute b and c such that det(Q) = b * 2^c.
    double b = 0.0;
    long int c   = 0;
    det( Q, n, &b, &c );
    assert( b > 0.0 );

    double  logdetQ = log( b ) + c * log2s;
    double  sum  = n * log2pi + logdetQ;
    double  val;
    valarray<double> residual = z - h;
    
    /*
     * The loop below untangles the following two statements
     * for eliminating matrix object constructions and taking advantage of
     * Q being symmetric.
     *
     *    DoubleMatrix term2 = multiply(multiply(transpose(dvecR), dmatInvQ), dvecR);
     *    dTerm2 = oneByOneToScalar(term2);
     *
     * Given Q being symmetric, for x^T Q x, the following is true:
     *    x(j)Q(i,j)x(i) = x(j)Q(j,i)x(i)
     *
     * Thus, we can eliminate #of arithmatics by computing only a half + diagonal elements.
     *
     */
    for( j=0; j<n; j++ )
    {
        for( i=0; i<n; i++ )
        {
            if( i<=j )
            {
                val = invQ[ j * n + i ] * residual[ i ] * residual[ j ];
                sum += val;
                if( i < j )
                    sum += val;
            }
        }
    }

    return sum / 2.0;
}


//**************************************************************
// Class: Elsq
//**************************************************************

/*
$begin elsqFuncOb$$
$spell  
    elsq
    iostream
    cfloat 
    centdiff 
    cerr cout 
    approx
    const 
    arg 
    res 
    Qx 
    ob 
    Rinvval 
    nr
    nc 
    xval 
    ind 
    int
    endl 
    inv 
    Rval 
    ios 
    std 
    namespace 
    pdb 
    fval 
    typedef 
    setprecision
    setiosflags
    Ri 
    pd 
    pb 
    wraps
    fi 
    pf 
    bval 
    det 
    covariances
    Spk
    bool
    cov
    covariance
    valarray
    resize
    Nv
    Model model
$$
$section Function Object for Extended Least Squares Function$$

$index function object, elsq$$
$index elsq, function object$$

$table
$bold Declaration:$$ $cend 
$syntax/template <class /Model/> class Elsq : public std::unary_function<DoubleMatrix, DoubleMatrix>/$$
$tend
$table
$bold Constructor:$$ $cend
$syntax/Elsq</Model/>::Elsq(Model* /model/, const DoubleMatrix& /y/)
/$$
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
This class wraps $tref elsq$$ function and allows user to call/evaluate the function through $code operator()$$
in the form of a unary function.

$head Arguments$$
$syntax/
/Model/
/$$
is the name of a user-provided model derived from SpkModel base class.


$head Public Members$$
$syntax/const DoubleMatrix operator(const DoubleMatrix /arg/) const
/$$
This member function evaluates $tref elsq$$ at an evaluation point specified by the $italic arg$$.

$head Example$$

The following piece of code demonstrates an example use of this function object:

$codep
    #include <iostream>
    #include <cfloat>

    #include "elsq.h"
    #include "centdiff.h"
    #include "DoubleMatrix.h"
    #include "SpkModel.h"
    #include "add.h"

    using namespace std;

    class UserModel : public SpkModel
    {
        valarray<double> _b;
    public:
        UserModel(){};
        ~UserModel(){};
        UserModel(const UserModel& right)
        {
            _b = right._b;
        }

    protected:
        // b = [1
        //      1
        //      1]
        void doSetIndPar(const valarray<double>& bval)
        {
            _b = bval;
        }

        // fi(b) = [b[2]
        //          b[2]]
        void doDataMean( valarray<double>& f ) const
        {
            f.resize( 2 * 1 );
            f = _b[2];
        }

        // fi_b(b) = [0  0  1]
        //           [0  0  1]
        bool doDataMean_indPar( valarray<double>& f_b ) const
        {
            f_b.resize( 2 * 3 );

            f_b = 0.0;
            f_b[4] = 1.0;
            f_b[5] =  1.0;
            return true;
        }

        // Ri(b) = [ 2*b[0]   0    ]
        //         [   0    1*b[1] ]
        void doDataVariance( valarray<double>& R ) const
        {
            R.resize( 2 * 2 );

            R[0] = 2.0 * _b[0];
            R[1] = 0.0;
            R[2] = 0.0;
            R[3] = 1.0 * _b[1];
        }

        //
        // RiInv(b) = [ 2/b[0]  0     ]
        //            [ 0      2/b[1] ]
        //
        void doDataVarianceInv( valarray<double>& RInv ) const
        {
            RInv.resize( 2 * 2 );
            RInv = 0.0;
            RInv[0] = 2.0 / _b[0];
            RINv[3] = 2.0 / _b[1];
        }


        // Ri_b(b) = [2 0 0
        //            0 0 0
        //            0 0 0
        //            0 1 0]
        bool doDataVariance_indPar( valarray<double>& R_b ) const
        {
            R_b.resize( 2 * 2 * 3 );
            R_b = 0.0;

            R_b[0] = 2.0;
            R_b[7] = 1.0;
            return true;
        } 

        //
        // RiInv_b(b) = [ -2.0 / b[0]^2     0.0               0.0 ]
        //              [  0.0              0.0               0.0 ]
        //              [  0.0              0.0               0.0 ]
        //              [  0.0             -2.0 / b[1]^2      0.0 ]
        //
        bool doIndParVarianceInv_indPar( valarray<double> & RInv_b ) const
        {
            RInv_b.resize( 2 * 2 * 3 );
            RInv_b = 0.0;
            RInv_b[0] = -2.0 / _b[0]^2;
            RInv_b[7] = -2.0 / _b[0]^2;
            
            return !allZero(RInv_b);
        }
    };

    void main(){

        using namespace std;

        cout << setiosflags(ios::scientific) << setprecision(15);

        int nB = 3;
        int nY = 2;

        // initialize the parameter vector
        // b = [1
        //      1
        //      1]
        //
        DoubleMatrix b(nB,1);
        b.fill(1.0);

        // initialize the data (measurement) vector
        // y = [0
        //      0]
        //
        DoubleMatrix y(nY,1);
        y.fill(0.0);

  
        // Initialize vector h which specifies the step size
        DoubleMatrix h(nB, 1);
        double *pdb = b.data();
        double *pdS = h.data();

        valarray<double> fval, f_xval, Rval, R_xval, Rinvval;

        typedef Elsq<UserModel> PROTO;

        UserModel model;
        PROTO elsqOb(&model, y);

        // ----- Simply evaluating the elsq function object -----
        cout << "elsq(x) = " << elsqOb(b) << endl;

        DoubleMatrix approx = centdiff<PROTO, DoubleMatrix, DoubleMatrix>(elsqOb, b, h);

        cerr << "approximation based on central difference  = " << approx << endl;
    }

$$
the program will display;
$codep

    elsq = 
    1 by 1
    [ 2.9344506566893180e+000 ]

    approximation based on central difference = 
    1 by 3
    [ 2.499999993669290e-001 -2.542453773616521e-009 1.500000000000000e+000 ]
$$
$end
*/
