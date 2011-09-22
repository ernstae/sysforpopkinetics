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
/****************************************************************
 *
 * elsq_x.cpp
 *
 * Definition of the 1st derivative of elsq function
 *
 * Author: Sachiko Honda
 *
 ****************************************************************/
/*
------------------------------------------------------------
   Function Specification (DoubleMatrix version)
------------------------------------------------------------
*/
/*
$begin elsq_x$$
$escape &$$
$spell
	Model model 
    elsq Qinv kron inv det dmat dvec const iostream cout
    endl namespace std deriv dg/dx st comp ind nc nr
    Qx Rb Fb Hx cassert fi Rinv Fx covariances
    Spk
    bool
    covariance
    cov
    Ri
    valarray
    resize
$$

$section Derivative of elsq With Full Covariance$$

$index elsq, elsq_x$$

$table
$bold syntax$$ $cend 
$syntax/const DoubleMatrix elsq_x(
    const DoubleMatrix &/z/, 
    const DoubleMatrix &/h/, 
    const DoubleMatrix &/Q/, 
    const DoubleMatrix &/Qinv/
    const DoubleMatrix &/h_x/
    const DoubleMatrix &/Q_x/)/$$ $rend

$bold syntax$$ $cend 
$syntax/const DoubleMatrix elsq_x(
    const DoubleMatrix &/r/, 
    const DoubleMatrix &/Q/, 
    const DoubleMatrix &/Qinv/
    const DoubleMatrix &/h_x/
    const DoubleMatrix &/Q_x/)/$$ $rend

$tend

See also $xref/elsq//elsq/$$.

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The routine $code elsq_x$$ computes the 1st derivative of 
extended least squares function (elsq) with respect to $math%x%$$.  \
If $math%g(x)%$$ is the extended least squares function, then 
$math%(dg/dx)%$$ is defined by
$math%
              1  %             T      -1
    g_x(x) = --- { rvec[Q(x)] }  Q_x(x)
              2  %
                 %      T  -1
              -[z - h(x)]  Q  h_x(x)
                 %
               1 %          T             T        -1
              +--- { [z-h(x)] kron [z-h(x)] } Q_x(x)
               2 %
%$$
$math%x%$$ and $math%z%$$ are column vectors,
$math%Q(x)%$$ is a symmetric, positive definite matrix valued function,
and $math%h(x)%$$ is a column vector valued function.
The routine $code elsq_x$$ computes the
$xref/glossary/Derivative/derivative/$$ of $math%g(x)%$$; i.e.,
$math%g_x(x)%$$.


$head Reference$$
See Lemma 11 of 
$italic Approximating the maximum likelihood estimate 
for models with random parameters$$.

$head Return Value$$
Upon a successful completion, this function returns
the $math%g_x(x)%$$ value as a column vector, where
$math%i%$$-th element of the vector is the derivative of
g(x) with respect to x[i].

$head Arguments$$
Given a $math%n%$$ dimensional column vector, $math%x%$$, 

$syntax/
/z/
/$$
is a $math%m by 1%$$ double-precision matrix.

$syntax/
/h/
/$$
is a $math%m by 1%$$ double-precision matrix 
containing the value of $math%h(x)%$$.

$syntax/
/r/
/$$
is a $math%m by 1%$$ double-precision matrix 
containing the value of $math%z - h(x)%$$.

$syntax/
/Q/
/$$
is a $math%m by m%$$ double-precision
containing the value of $math%Q(x)%$$.

$syntax/
/Qinv/
/$$
is a $math%m by m%$$ double-precision
matrix containing the matrix inverse of $italic Q$$.


$syntax/
/h_x/
/$$
is a $math%m by n%$$ double-precision
matrix containing the 
$xref/glossary/Derivative/derivative/$$ of $math%h(x)%$$;
i.e., $math%h_x(x)%$$.


$syntax/
/Q_x/
/$$
is a $math%m*m by n%$$ double-precision
matrix containing the 
$xref/glossary/Derivative/derivative/$$ of $math%Q(x)%$$; 
i.e., $math%Q_x(x)%$$.

$head Example$$
Suppose that
$math%
              / 2 x(1)     0  \            / x(3) \        / 0 \
       Q(x) = |               |    h(x) =  |      |    z = |   |
              \  0      x(2)  /            \ x(3) /        \ 0 /
%$$
It follows that
$math%                             
    g(x) = 1/2 &log[ (2 &pi)^2 2 x(1) x(2)] + 1/4 x(3)^2 [1 / x(1) + 2 / x(2)]
%$$
The gradient of $math%g(x)%$$ is equal to
$math%
     1  /  2 / x(1) - x(3)^2 / x(1)^2    \
    --- |  2 / x(2) - 2 x(3)^2 / x(2)^2  |
     4  \ 2 x(3) [ 1 / x(1) + 2 / x(2)]  /
%$$
If all the components of $math%x%$$ are one, 
the derivative of $math%g(x)%$$ is equal to
$math%
    [ 1 , 0 , 6 ] / 4 = [ .025 , 0 , 1.5 ]
%$$
If you compile and link the following program:
$codep

    #include <iostream>
    #include <cassert>
    #include "DoubleMatrix.h"
    #include "SpkModel.h"
    #include "SpkValarray.h"

    using namespace std;

    class UserModel : public SpkModel
    {
        valarray<double> _b;
    public:
        UserModelElsq_xTest(){};
        ~UserModelElsq_xTest(){};
    protected:
        void doSetIndPar(const valarray<double>& b)
        { 
            _b = b;
        }
        void doDataMean_popPar( valarray<double>& f ) const
        {
            // return { b(3), b(3) }
            f.resize( 2 );

            f[0] = 1.0 * _b[2];
            f[1] = 1.0 * _b[2];
        }
        bool doDataMean_indPar( valarray<double>& f_b ) const
        {
            //  return { ...
            //  [ 0 , 0 , 1 ], ...
            //  [ 0 , 0 , 1 ] ...
            //  }
            f_b.resize( 2 * 3 );

            f_b = 0.0;
            pFb_b[4] = 1.0;
            pFb_b[5] = 1.0;
            
            return true;
        }
        void doDataVariance( valarray<double>& R ) const
        {
            // return { ...
            //        [ 2 * x(1) ,   0   ], ...
            //        [     0    ,  x(2) ] ...
            //        }
            R.resize( 2 * 2 );

            R[0] = 2.0 * _b[0];
            R[1] = 0.0;
            R[2] = 0.0;
            R[3] = 1.0 * _b[1];
        }
        bool doDataVariance_indPar( valarray<double>& R_b ) const
        {
            // return { ...
            //  [ 2 , 0 , 0 ], ...
            //  [ 0 , 0 , 0 ], ...
            //  [ 0 , 0 , 0 ], ...
            //  [ 0 , 1 , 0 ] ...
            //  }
            R_b.resize( 2 * 2 * 3 );

            R_b = 0.0;
            R_b[0]   = 2.0;
            R_b[7] = 1.0;
            
            return true;
        }   
    };
    void main(){

        UserModel model;
        valarray<double> h, h_x, Q, Q_x, Qinv;

        valarray<double> b(3);
        b = 1.0;
        DoubleMatrix dvecB( b, 1 );

        valarray<double> y(2);
        y = 0.0;
        DoubleMatrix dvecY( y, 1 );

        model setIndPar(b);

        model.dataMean(h);              DoubleMatrix dvecH  ( h,   1 );
        model.dataMean_indPar(h_x);     DoubleMatrix dmatH_x( h_x, 3 );
        model.dataVariance(Q);          DoubleMatrix dmatQ  ( Q,   2 );
        model.dataVariance_indPar(Q_x); DoubleMatrix dmatQ_x( Q_x, 3 );

        cout << "b" << endl;
        dvecB.print();

        cout << "y" << endl;
        dvecY.print();

        cout << "h" << endl;
        dvecH.print();

        cout << "h_x" << endl;
        dmatH_x.print();

        cout << "Q" << endl;
        dmatQ.print();

        cout << "Q_x" << endl;
        dmatQ_x.print();

        assert( det( DoubleMatrix( Q, 2 ) ) > 0.0 );
        model.dataVarianceInv(Qinv);

        // Compute dg/dx
        DoubleMatrix dmatDerivExact = pElsq_x(dvecY, dvecH, dmatQ, dmatQinv, dmatH_x, dmatQ_x);

        cout << "elsq_x = ";
        dmatDerivExact.print();
    }

$$
it displays the following:
$codep
    b
    [ 1 ]
    [ 1 ]
    [ 1 ]

    y
    [ 0 ]
    [ 0 ]
    
    h
    [ 1 ]
    [ 1 ]
    
    h_x
    [ 0 0 1 ]
    [ 0 0 1 ]
    
    Q
    [ 2 0 ]
    [ 0 1 ]
    
    Q_x
    [ 2 0 0 ]
    [ 0 0 0 ]
    [ 0 0 0 ]
    [ 0 1 0 ]

    elsq_x = [ 0.25 0 1.5 ]

$$
$end
*/
/*
------------------------------------------------------------
   Function Implementation
------------------------------------------------------------
*/
#include <iostream>
#include <assert.h>
#include "elsq_x.h"
#include "subtract.h"
#include "divByScalar.h"
#include "mulByScalar.h"
#include "transpose.h"
#include "rvec.h"
#include "AkronBtimesC.h"
#include "multiply.h"
#include "DoubleMatrix.h"
#include "isDmatEpsEqual.h"

static DoubleMatrix dvecRTrans(__FILE__);
static DoubleMatrix drowW(__FILE__);
static DoubleMatrix rvecQinvTrans(__FILE__);
static DoubleMatrix drowTerm1(__FILE__);
static DoubleMatrix drowTerm2(__FILE__);
static DoubleMatrix drowTerm3(__FILE__);
static DoubleMatrix drowAns(__FILE__);   

// David: this function does not get passed the derivative of Qinv and uses a formula
// where those derivatives are expressed in terms of Qinv and derivatives of
// Q.  This should be changed in some way so that we could take advantage of 
// the inverse derivatives if they are provided.

const DoubleMatrix elsq_x(const DoubleMatrix &dvecZ,      // m size vector
                    const DoubleMatrix &dvecH,      // m size vector
                    const DoubleMatrix &dmatQ,      // m by m symmetric, positive definite
                    const DoubleMatrix &dmatQinv,   // m by m symmetric, positive definite
                    const DoubleMatrix &dmatH_x,    // m by n matrix
                    const DoubleMatrix &dmatQ_x     // m*m by n matrix
                    )
{
    DoubleMatrix dvecR;
    subtract( dvecZ, dvecH, dvecR );
    return elsq_x(dvecR, dmatQ, dmatQinv, dmatH_x, dmatQ_x);
}
const DoubleMatrix elsq_x(const DoubleMatrix &dvecR,      // m size vector, z - h
                    const DoubleMatrix &dmatQ,      // m by m symmetric, positive definite
                    const DoubleMatrix &dmatQinv,   // m by m symmetric, positive definite
                    const DoubleMatrix &dmatH_x,    // m by n matrix
                    const DoubleMatrix &dmatQ_x     // m*m by n matrix
                    )
{
    using namespace std;
    // x:   m by 1
    // z:   m by 1
    // h:   m by 1
    // Q:   m by m
    // invQ:m by m
    // h_x: m by n
    // Q_x: m*m by n
    const int m = dvecR.nr();
    const int n = dmatQ_x.nc();
    double  val;

    assert( dmatQ.nr() == m );
    assert( dmatQ.nc() == m );
    assert( dmatQinv.nr() == m );
    assert( dmatQinv.nc() == m );
    assert( dmatH_x.nr() == m );
    assert( dmatH_x.nc() == n );
    assert( dmatQ_x.nr() == m*m );
    assert( dmatQ_x.nc() == n );

    transpose(dvecR, dvecRTrans);
    multiply(dvecRTrans, dmatQinv, drowW);
    transpose( rvec( dmatQinv ), rvecQinvTrans );

    multiply(rvecQinvTrans, dmatQ_x, drowTerm1);
    assert(drowTerm1.nr()== 1);
    assert(drowTerm1.nc()== n);

    multiply(drowW, dmatH_x, drowTerm2);
    assert(drowTerm2.nr()== 1);
    assert(drowTerm2.nc()== n);

    //
    // The loop below untangles the following statement
    // for eliminating matrix object constructions and taking advantage of
    // Q being symmetric.
    //
    //    DoubleMatrix dmatTerm3  = AkronBtimesC( drowW, rvecQinvTrans, dmatQ_x );
    // 
    // Let w be (z-h).
    // 
    // The orignal formula
    //    0.5 [w^T kron w^T] partial_x(Qinv) --- (eq. 1)
    //
    // is equivalent to:
    //    0.5 partial_x(k) [ w^T Qinv w ] --- (eq. 2)
    //   =0.5 SUM [ w(i) w(j) partial_x(k)(Q(i,j)) ], over 1<=i<=m and 1<=j<=m.
    //
    // For Q being symmetric, the following is true:
    //    w(i) w(j) partial_x(k)(Q(i,j)) == w(j) w(i) partial_x(k)(Q(j,i))
    //
    //
    const double* pW   = drowW.data();
    const double* pQ_x = dmatQ_x.data();
    drowTerm3.resize(1,n);
    drowTerm3.fill(0.0);
    double* pTerm3 = drowTerm3.data();

    for( int k=0; k<n; k++ )
    {
        for( int j=0; j<m; j++ )
        {
            for( int i=0; i<m; i++ )
            {
                if( i<=j )
                {
                    val = pW[i] * pW[j] * pQ_x[ j*m+i+k*m*m ] / 2.0;
                    drowTerm3.data()[k] += val;
                    if( i<j )
                        pTerm3[k] += val;
                }
            }
        }
    }
    
    subtract( subtract(mulByScalar(drowTerm1, 0.5), drowTerm2 )
                                        ,drowTerm3, drowAns );


    // elsq_x returns a 1 by n matrix
    assert(drowAns.nr()==1);
    assert(drowAns.nc()==n);
    return drowAns;

}
/*
------------------------------------------------------------
   Function Specification (valarray version)
------------------------------------------------------------
*/
/*
$begin elsq_xVA$$
$escape &$$
$spell
	Model model 
    elsq Qinv kron inv det dmat dvec const iostream cout
    endl namespace std deriv dg/dx st comp ind nc nr
    Qx Rb Fb Hx cassert fi Rinv Fx covariances
    Spk
    bool
    covariance
    cov
    Ri
    valarray
    resize
$$

$section Derivative of elsq With Full Covariance (valarray version)$$

$index elsq, elsq_x$$

$table
$bold syntax$$ $cend 
$syntax/const valarray<double> elsq_x(
    const valarray<double> &/z/, 
    const valarray<double> &/h/, 
    const valarray<double> &/Q/, 
    const valarray<double> &/Qinv/
    const valarray<double> &/h_x/
    const valarray<double> &/Q_x/)/$$ $rend


$tend

See also $xref/elsqVA//elsq/$$.

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The routine $code elsq_x$$ computes the 1st derivative of 
extended least squares function (elsq) with respect to $math%x%$$.  \
If $math%g(x)%$$ is the extended least squares function, then 
$math%(dg/dx)%$$ is defined by
$math%
              1  %             T      -1
    g_x(x) = --- { rvec[Q(x)] }  Q_x(x)
              2  %
                 %      T  -1
              -[z - h(x)]  Q  h_x(x)
                 %
               1 %          T             T        -1
              +--- { [z-h(x)] kron [z-h(x)] } Q_x(x)
               2 %
%$$
$math%x%$$ and $math%z%$$ are $math%n%$$ dimensional column vectors,
$math%Q(x)%$$ is a symmetric, positive definite matrix valued function,
and $math%h(x)%$$ is a column vector valued function.
The routine $code elsq_x$$ computes the
$xref/glossary/Derivative/derivative/$$ of $math%g(x)%$$; i.e.,
$math%g_x(x)%$$.


$head Reference$$
See Lemma 11 of 
$italic Approximating the maximum likelihood estimate 
for models with random parameters$$.

$head Return Value$$
Upon a successful completion, this function returns
the $math%g_x(x)%$$ value as a column vector, where
$math%i%$$-th element of the vector is the derivative of
g(x) with respect to x[i].

$head Arguments$$
Given a $math%n%$$ dimensional column vector, $math%x%$$, 

$syntax/
/z/
/$$
This is a $code valarray<double>$$ object containing the value of
$math%z%$$ matrix in the column order.  $math%z%$$ is a $math%n%$$
dimensional column vector.

$syntax/
/h/
/$$
This is a $code valarray<double>$$ object containing the value of
$math%h(x)%$$.  $math%h(x)%$$ is a vector valued function,
yielding in $math%n%$$ dimensions.

$syntax/
/Q/
/$$
This is a $code valarray<double>$$ object containing the value of
$math%Q(x)%$$.  $math%Q(x)%$$ is a matrix valued function, yielding
$math%n by n%$$ dimensions.  The value of $math%Q()%$$ is expected
to have symmetric, positive definite properties.

$syntax/
/Qinv/
/$$
This is a $code valarray<double>$$ object containing the value of
$math%Q(x)^(-1)%$$.


$syntax/
/h_x/
/$$
This is a $code valarray<double>$$ object containing the value of
$xref/glossary/Derivative/derivative/$$ of $math%h(x)%$$;
i.e., $math%h_x(x)%$$.  The derivative yields in 
$math%n by nX%$$ dimensions, where $math%nX%$$ is the length
of $math%x%$$ parameter vector.


$syntax/
/Q_x/
/$$
This is a $code valarray<double>$$ object containing the value of
$xref/glossary/Derivative/derivative/$$ of $math%Q(x)%$$; 
i.e., $math%Q_x(x)%$$.
The derivative yields in 
$math%n^2 by nX%$$ dimensions, where $math%nX%$$ is the length
of $math%x%$$ parameter vector.

$head Example$$
Suppose that
$math%
              / 2 x(1)     0  \            / x(3) \        / 0 \
       Q(x) = |               |    h(x) =  |      |    z = |   |
              \  0      x(2)  /            \ x(3) /        \ 0 /
%$$
It follows that
$math%                             
    g(x) = 1/2 &log[ (2 &pi)^2 2 x(1) x(2)] + 1/4 x(3)^2 [1 / x(1) + 2 / x(2)]
%$$
The gradient of $math%g(x)%$$ is equal to
$math%
     1  /  2 / x(1) - x(3)^2 / x(1)^2    \
    --- |  2 / x(2) - 2 x(3)^2 / x(2)^2  |
     4  \ 2 x(3) [ 1 / x(1) + 2 / x(2)]  /
%$$
If all the components of $math%x%$$ are one, 
the derivative of $math%g(x)%$$ is equal to
$math%
    [ 1 , 0 , 6 ] / 4 = [ .025 , 0 , 1.5 ]
%$$
If you compile and link the following program:
$codep

    #include <iostream>
    #include <cassert>
    #include "SpkModel.h"
    #include "SpkValarray.h"
    #include "DoubleMatrix.h"

    using VA_SPK::valarray;
    using namespace std;

    class UserModel : public SpkModel
    {
        valarray<double> _b;
    public:
        UserModelElsq_xTest(){};
        ~UserModelElsq_xTest(){};
    protected:
        void doSetIndPar(const valarray<double>& b)
        { 
            _b = b;
        }
        void doDataMean_popPar( valarray<double>& f ) const
        {
            // return { b(3), b(3) }
            f.resize( 2 );

            f[0] = 1.0 * _b[2];
            f[1] = 1.0 * _b[2];
        }
        bool doDataMean_indPar( valarray<double>& f_b ) const
        {
            //  return { ...
            //  [ 0 , 0 , 1 ], ...
            //  [ 0 , 0 , 1 ] ...
            //  }
            f_b.resize( 2 * 3 );

            f_b = 0.0;
            pFb_b[4] = 1.0;
            pFb_b[5] = 1.0;
            
            return true;
        }
        void doDataVariance( valarray<double>& R ) const
        {
            // return { ...
            //        [ 2 * x(1) ,   0   ], ...
            //        [     0    ,  x(2) ] ...
            //        }
            R.resize( 2 * 2 );

            R[0] = 2.0 * _b[0];
            R[1] = 0.0;
            R[2] = 0.0;
            R[3] = 1.0 * _b[1];
        }
        bool doDataVariance_indPar( valarray<double>& R_b ) const
        {
            // return { ...
            //  [ 2 , 0 , 0 ], ...
            //  [ 0 , 0 , 0 ], ...
            //  [ 0 , 0 , 0 ], ...
            //  [ 0 , 1 , 0 ] ...
            //  }
            R_b.resize( 2 * 2 * 3 );

            R_b = 0.0;
            R_b[0]   = 2.0;
            R_b[7] = 1.0;
            
            return true;
        }   
    };
    void main(){

        const int nX = 3;
        const int n  = 2;

        valarray<double> h, h_x, Q, Q_x, Qinv;

        valarray<double> x(1.0, nX);

        valarray<double> z(0.0, n);

        UserModel model;
        model setIndPar(x);

        model.dataMean(h); 
        model.dataMean_indPar(h_x);
        model.dataVariance(Q);
        model.dataVariance_indPar(Q_x);

        cout << "x" << endl;
        DoubleMatrix( x, 1 ).print();

        cout << "z" << endl;
        DoubleMatrix( z, 1 ).print();

        cout << "h" << endl;
        DoubleMatrix( h, 1 ).print();

        cout << "h_x" << endl;
        DoubleMatrix( h_x, nX ).print();

        cout << "Q" << endl;
        DoubleMatrix( Q, n ).print();

        cout << "Q_x" << endl;
        DoubleMatrix( Q_x, nX ).print();

        assert( det( Q, n, 2 ) > 0.0 );
        model.dataVarianceInv(Qinv);

        // Compute dg/dx
        valarray<double> answer = pElsq_x(z, h, Q, Qinv, h_x, Q_x);

        cout << "elsq_x = ";
        DoubleMatrix( answer, nX ).print();
    }

$$
it displays the following:
$codep
    b
    [ 1 ]
    [ 1 ]
    [ 1 ]

    y
    [ 0 ]
    [ 0 ]
    
    h
    [ 1 ]
    [ 1 ]
    
    h_x
    [ 0 0 1 ]
    [ 0 0 1 ]
    
    Q
    [ 2 0 ]
    [ 0 1 ]
    
    Q_x
    [ 2 0 0 ]
    [ 0 0 0 ]
    [ 0 0 0 ]
    [ 0 1 0 ]

    elsq_x = [ 0.25 0 1.5 ]

$$
$end
*/
#include "SpkValarray.h"
using SPK_VA::valarray;

const valarray<double> elsq_x(
                    const valarray<double> &z,      // n size vector
                    const valarray<double> &h,      // n size vector
                    const valarray<double> &Q,      // n by n symmetric, positive definite
                    const valarray<double> &Qinv,   // n by n symmetric, positive definite
                    const valarray<double> &h_x,    // n by nX matrix
                    const valarray<double> &Q_x     // n^2 by nX matrix
                    )
{
  const int n = z.size();
  assert( h.size() == n );
  assert( Q.size() == n * n );
  const int nX = h_x.size() / n;
  assert( n * nX == h_x.size() );
  assert( Q_x.size() == n * n * nX );

  valarray<double> residual = z - h;
  valarray<double> residualTran = residual;   // 1 by n
  double  val;

  valarray<double> W = multiply(residualTran, n, Qinv, n);  // 1 by n
  valarray<double> rvecQinvTrans = rvec( Qinv, n );   // 1 by n^2: don't really need to transpose because it's just an array

  valarray<double> term1 = multiply(rvecQinvTrans, n*n, Q_x, nX );  // 1 by nX 

  valarray<double> term2 = multiply(W, n, h_x, nX);
  assert(term2.size()== nX);

  //
  // The loop below untangles the following statement
  // for eliminating matrix object constructions and taking advantage of
  // Q being symmetric.
  //
  //    valarray<double> term3  = AkronBtimesC( W, n, rvecQinvTrans, n, Q_x, n);
  // 
  // Let w be (z-h).
  // 
  // The orignal formula
  //    0.5 [w^T kron w^T] partial_x(Qinv) --- (eq. 1)
  //
  // is equivalent to:
  //    0.5 partial_x(k) [ w^T Qinv w ] --- (eq. 2)
  //   =0.5 SUM [ w(i) w(j) partial_x(k)(Q(i,j)) ], over 1<=i<=m and 1<=j<=m.
  //
  // For Q being symmetric, the following is true:
  //    w(i) w(j) partial_x(k)(Q(i,j)) == w(j) w(i) partial_x(k)(Q(j,i))
  //
  //
  valarray<double> term3( 0.0, nX );  // 1 by nX

  for( int k=0; k<nX; k++ )
  {
      for( int j=0; j<n; j++ )
      {
          for( int i=0; i<n; i++ )
          {
              if( i<=j )
              {
                  val = W[i] * W[j] * Q_x[ j*n+i+k*n*n ] / 2.0;
                  term3[k] += val;
                  if( i<j )
                      term3[k] += val;
              }
          }
      }
  }
  
  // Returns a 1 by nX matrix.
  return 0.5 * term1 - term2 - term3;
}
