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
 * elsq_xDiag.cpp
 *
 * Definition of the 1st derivative of elsq function with 
 * diagonal Q
 *
 * Author: Jiaji Du
 */
/*
------------------------------------------------------------
   Function Specification
------------------------------------------------------------
*/
/*
$begin elsq_xDiag$$
$escape &$$
$spell
	Model model 
    elsqDiag 
    Qinv 
    kron
    inv
    det
    dmat
    dvec 
    const 
    iostream
    cout
    endl 
    namespace
    std 
    deriv 
    dg/dx 
    st
    comp 
    ind 
    nc 
    nr
    Qx
    Rb 
    Fb 
    Hx 
    cassert
    fi 
    Ri 
    Rinv 
    Fx 
    covariance
    covariances
    Spk
    bool
    cov
    valarray
    resize
$$

$section Derivative of elsq With Diagonal Covariance$$

$index elsq, elsq_xDiag$$

$table
$bold syntax$$ $cend 
$syntax/DoubleMatrix elsq_xDiag(
    const DoubleMatrix &/z/, 
    const DoubleMatrix &/h/, 
    const DoubleMatrix &/Q/, 
    const DoubleMatrix &/Qinv/
    const DoubleMatrix &/h_x/
    const DoubleMatrix &/Q_x/)/$$ $rend

$bold syntax$$ $cend 
$syntax/DoubleMatrix elsq_xDiag(
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
The routine $code elsq_xDiag$$ computes the 1st derivative of 
extended least squares function (elsq) with respect to $math%x%$$ 
for the case of the covariance matrix being diagonal.  
If $math%g(x)%$$ is the extended least squares function, then 
$math%(dg/dx)%$$ is defined by
$math%
              1  %          -1 T      
    g_x(x) = --- { rvec[Q(x)  ] } Q_x(x)
              2  %
                 %        T -1
              - [z - h(x)] Q  h_x(x)
                 %
                 1 %          T             T        -1
              + --- { [z-h(x)] kron [z-h(x)] } Q_x(x)
                 2 %
%$$
$math%x%$$ and $math%z%$$ are column vectors,
$math%Q(x)%$$ is a diagonal, positive definite matrix valued function,
and $math%h(x)%$$ is a column vector valued function.
The routine $code elsq_xDiag$$ computes the
$xref/glossary/Derivative/derivative/$$ of $math%g(x)%$$; i.e.,
$math%g_x(x)%$$.


$head Reference$$
See Lemma 11 of 
$italic Approximating the maximum likelihood estimate 
for models with random parameters$$.

$head Return Value$$
Upon a successful completion, this function returns
the $math%g_x(x)%$$ value as a row vector, where
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
is a $math%m by m%$$ double-precision diagonal matrix
containing the value of $math%Q(x)%$$.

$syntax/
/Qinv/
/$$
is a $math%m by m%$$ double-precision diagonal
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
     4  \  2 x(3)[ 1 / x(1) + 2 / x(2)]  /
%$$
If all the components of $math%x%$$ are one, 
the derivative of $math%g(x)%$$ is equal to
$math%
    [ 1 , 0 , 6 ] / 4 = [ .025 , 0 , 1.5 ]
%$$
If you compile and link the following program:
$codep

    #include <iostream>
    #include "DoubleMatrix.h"
    #include "SpkModel.h"
    #include "SpkValarray.h"

    class UserModel : public SpkModel
    {
        valarray<double> _b;
    public:
        UserModel(){};
        ~UserModel(){};
    protected:
        void doSetIndPar(const valarray<double>& b)
        { 
            _b = b;
        }
        void doDataMean( valarray<double>& ret ) const
        {
            // return { b(3), b(3) }
            valarray<double> f( 2 * 1 );

            f[0] = 1.0 * _b[2];
            f[1] = 1.0 * _b[2];
        }
        bool doDataMean_indPar( valarray<double>& ret ) const
        {
            //  return { ...
            //  [ 0 , 0 , 1 ], ...
            //  [ 0 , 0 , 1 ] ...
            //  }
            ret.resize(2 * 3);

            ret = 0.0;
            ret[4] = 1.0;
            ret[5] = 1.0;

            return true;
        }
        void Ri( valarray<double>& ret ) const
        {
            // return { ...
            //        [ 2 * b(1) ,   0   ], ...
            //        [     0    ,  b(2) ] ...
            //        }
            ret.resize( 2 * 2 );

            ret[0] = 2.0 * _b[0];
            ret[1] = 0.0;
            ret[2] = 0.0;
            ret[3] = 1.0 * _b[1];

            ret = dmatRb;
        }
        bool doDataVariance_indPar( valarray<double>& ret ) const
        {
            // return { ...
            //  [ 2 , 0 , 0 ], ...
            //  [ 0 , 0 , 0 ], ...
            //  [ 0 , 0 , 0 ], ...
            //  [ 0 , 1 , 0 ] ...
            //  }
            ret.resize( 2 * 2 * 3 );
            ret    = 0.0;
            ret[0] = 2.0;
            ret[7] = 1.0;
        }   
    };
    void main()
    {
        using namespace std;

        const int nB = 3;
        const int nY = 2;
        valarray<double> h, h_x, Q, Q_x, QInv;

        UserModel model;

        valarray<double> b(1.0, nB);
        valarray<double> y(0.0, nY);

        model setIndPar(b);

        model.dataMean(h);
        model.dataMean_indPar(h_x);
        model.dataVariance(Q);
        model.dataVariance_indPar(Q_x);

        DoubleMatrix dvecH   ( h, 1 );
        DoubleMatrix dmatH_x ( h_x, nB );
        DoubleMatrix dmatQ   ( Q, nY );
        DoubleMatrix dmatQ_x ( Q_x, nB );
        DoubleMatrix dmatQInv( QInv, nY );

        cout << "dvecB" << endl;
        dvecB.print();
        cout << "dvecY" << endl;
        dvecY.print();
        cout << "dvecH" << endl;
        dvecH.print();
        cout << "dmatH_x" << endl;
        dmatH_x.print();
        cout << "dmatQ" << endl;
        dmatQ.print();
        cout << "dmatQ_x" << endl;
        dmatQ_x.print();
        cout << endl;

        assert( det(dmatQ) > 0.0 );
        model.dataVarianceInv(dmatQInv);

        // Compute dg/dx
        DoubleMatrix dmatDerivExact = pElsq_xDiag(dvecY, dvecH, dmatQ, dmatQInv, dmatH_x, dmatQ_x);

        cout << "elsq_xDiag = ";
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

    elsq_xDiag = [ 0.25 0 1.5 ]

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
#include "elsq_xDiag.h"
#include "DoubleMatrix.h"
#include "subtract.h"

static DoubleMatrix drowTemp(__FILE__);
static DoubleMatrix drowAns(__FILE__);   

DoubleMatrix elsq_xDiag(const DoubleMatrix &dvecZ,      // m size vector
                    const DoubleMatrix &dvecH,      // m size vector
                    const DoubleMatrix &dmatQ,      // m by m symmetric, positive definite
                    const DoubleMatrix &dmatQinv,   // m by m symmetric, positive definite
                    const DoubleMatrix &dmatH_x,    // m by n matrix
                    const DoubleMatrix &dmatQ_x     // m*m by n matrix
                    )
{
    DoubleMatrix dvecR;
    subtract( dvecZ, dvecH, dvecR );
    return elsq_xDiag(dvecR, dmatQ, dmatQinv, dmatH_x, dmatQ_x);
}
DoubleMatrix elsq_xDiag(const DoubleMatrix &dvecR,  // m size vector, z - h
                    const DoubleMatrix &dmatQ,      // m by m symmetric, positive definite
                    const DoubleMatrix &dmatQinv,   // m by m symmetric, positive definite
                    const DoubleMatrix &dmatH_x,    // m by n matrix
                    const DoubleMatrix &dmatQ_x     // m*m by n matrix
                    )
{
	// Check input
	const int m = dvecR.nr();
    const int n = dmatQ_x.nc();
    assert( dmatQinv.nr() == m );
    assert( dmatQinv.nc() == m );
    assert( dmatH_x.nr() == m );
    assert( dmatH_x.nc() == n );
    assert( dmatQ_x.nr() == m*m );
    assert( dmatQ_x.nc() == n );

	// Prepare variables
	double term1, term2, term3;
	int i, j, k, l;
    const double* pQ_x  = dmatQ_x.data();
    const double* pQinv = dmatQinv.data();
    const double* pR    = dvecR.data();
	const double* pH_x  = dmatH_x.data();
	drowTemp.resize( 1, m );
	drowAns.resize( 1, n );
    double* pTemp       = drowTemp.data();
    double* pAns        = drowAns.data();

	// Calculate R'Qinv
	for( i = 0; i < m; i++ )
	{
	    pTemp[ i ]  = pR[ i ] * pQinv[ i * ( m + 1 ) ]; 
	}

	// Calculate the answer
    for( k = 0; k < n; k++ )
    {
		term1 = 0;
		term2 = 0;
		term3 = 0;

        for( j = 0; j < m; j++ )
        {
            l = j + ( j + k * m ) * m;
            term1 += pQinv[ j * ( m + 1 ) ] * pQ_x[ l ];
		    term2 += pTemp[ j ] * pH_x[ j + m * k ];
			term3 += pTemp[ j ] * pTemp[ j ] * pQ_x[ l ];
        }

		pAns[ k ] = .5 * term1 - term2 - .5 * term3;
    }
    
    return drowAns;
}
