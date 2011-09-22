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
 * elsq_xBlockDiag.cpp
 *
 * Definition of the 1st derivative of elsq function with block
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
$begin elsq_xBlockDiag$$
$escape &$$
$spell
	Model model 
    elsqBlockDiag 
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

$section Derivative of elsq With Block Diagonal Covariance$$

$index elsq, elsq_xBlockDiag$$

$table
$bold syntax$$ $cend 
$syntax/DoubleMatrix elsq_xBlockDiag(
    const DoubleMatrix &/r/, 
    const DoubleMatrix &/Qinv/,
    const DoubleMatrix &/h_x/,
    const DoubleMatrix &/Q_x/)/,
    const DoubleMatrix &/N/$$ $rend 
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
The routine $code elsq_xBlockDiag$$ computes the 1st derivative of 
extended least squares function (elsq) with respect to $math%x%$$ 
for the case of the covariance matrix being block diagonal.  
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
$math%Q(x)%$$ is a block diagonal, positive definite matrix valued function,
and $math%h(x)%$$ is a column vector valued function.
The routine $code elsq_xBlockDiag$$ computes the
$xref/glossary/Derivative/derivative/$$ of $math%g(x)%$$; i.e.,
$math%g_x(x)%$$.


$head Reference$$
See Lemma 12 of 
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
/r/
/$$
is a $math%m by 1%$$ double-precision matrix 
containing the value of $math%z-h(x)%$$.

$syntax/
/r/
/$$
is a $math%m by 1%$$ double-precision matrix 
containing the value of $math%z - h(x)%$$.

$syntax/
/Q/
/$$
is a $math%m by m%$$ double-precision block diagonal matrix
containing the value of $math%Q(x)%$$.

$syntax/
/Qinv/
/$$
is a $math%m by m%$$ double-precision block diagonal
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

$syntax/
/N/
/$$
is a $math%nInd by 1%$$ double-precision matrix 
containing the block sizes of Q matrix.  nInd is the number of
blocks (number of individuals).
 
$head Example$$
Suppose that
$math%
              / 2*x(1)   0     0   \            / x(3) \        / 0 \        / 1 \
       Q(x) = |   0     x(2)  0.5  |    h(x) =  | x(3) |    z = | 0 |    N = |   |
              \   0     0.5   x(2) /            \ x(3) /        \ 0 /        \ 2 /
%$$
It follows that
$math%                             
    g(x) = 1/2*&log[(2 &pi)^2*2*x(1)(x(2)^2-0.25)]+x(3)^2*[0.25/x(1)+1/(x(2)+0.5)]
%$$
The gradient of $math%g(x)%$$ is equal to
$math%
       /  0.5*x(1)-0.25*x(3)^2/x(1)^2            \
       |  x(2)/(x(2)^-0.25)-x(3)^2/(x(2)+0.5)^2  |
       \  x(3)[0.5/x(1)+2/(x(2)+0.5)]            /
%$$
If all the components of $math%x%$$ are one, 
the derivative of $math%g(x)%$$ is equal to
$math%
       [ 0.25 , 8/9 , 5.5/3 ]
%$$
If you compile and link the following program:
$codep

#include <iostream>
#include "DoubleMatrix.h"
#include "SpkModel.h"
#include "SpkValarray.h"
#include "elsq_xBlockDiag.h"

using namespace std;

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
        // return { b(3), b(3), b(3) }
        ret.resize( 3, 0.0);

        ret[0] = _b[2];
        ret[1] = _b[2];
	    ret[2] = _b[2];
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        //  return { ...
        //  [ 0 , 0 , 1 ], ...
		//  [ 0 , 0 , 1 ], ...
        //  [ 0 , 0 , 1 ] ...
        //  }
        ret.resize( 9, 0.0);

        ret[6] = 1.0;
        ret[7] = 1.0;
        ret[8] = 1.0;

        return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        // return { ...
        //        [ 2 * b(1) ,   0       0   ], ...
        //        [     0    ,  b(2),   0.5  ], ...
		//        [     0    ,  0.5,    b(2) ] ...
        //        }
        ret.resize( 9, 0.0 );

        ret[0] = 2.0 * _b[0];
        ret[4] = _b[1];
        ret[5] = 0.5;
		ret[7] = 0.5;
        ret[8] = _b[1];
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        // return { ...
        //  [ 2 , 0 , 0 ], ...
        //  [ 0 , 0 , 0 ], ...
        //  [ 0 , 0 , 0 ], ...
		//  [ 0 , 0 , 0 ], ...
		//  [ 0 , 1 , 0 ], ...
        //  [ 0 , 0 , 0 ], ...
		//  [ 0 , 0 , 0 ], ...
        //  [ 0 , 0 , 0 ], ...
        //  [ 0 , 1 , 0 ] ...
        //  }
        ret.resize( 27, 0.0 );
        ret[0] = 2.0;
        ret[13] = 1.0;
        ret[17] = 1.0;

		return true;
    }   
};
void main()
{
    const int nB = 3;
    const int nY = 3;
    valarray<double> h, h_x, Q, Q_x, QInv;

    UserModel model;

    valarray<double> b(1.0, nB);
    valarray<double> y(0.0, nY);

    model.setIndPar(b);

    model.dataMean(h);
    model.dataMean_indPar(h_x);
    model.dataVariance(Q);
	model.dataVarianceInv(QInv);
    model.dataVariance_indPar(Q_x);

	DoubleMatrix dvecR   ( y-h,  1  );
	DoubleMatrix dmatQ   ( Q,    nY );
    DoubleMatrix dmatH_x ( h_x,  nB );
    DoubleMatrix dmatQ_x ( Q_x,  nB );
    DoubleMatrix dmatQInv( QInv, nY );
	DoubleMatrix dvecN   ( 2,    1  );

	dvecN.data()[ 0 ] = 1;
	dvecN.data()[ 1 ] = 2;

    // Compute dg/dx
    DoubleMatrix dmatDerivExact = elsq_xBlockDiag( dvecR, dmatQInv, dmatH_x, dmatQ_x, dvecN );

    cout << "elsq_xBlockDiag = ";
    dmatDerivExact.print();
}

$$
it displays the following:
$codep

elsq_xBlockDiag = [ 0.25 0.888889 1.83333 ]

$$
$end
*/
/*
------------------------------------------------------------
   Function Implementation
------------------------------------------------------------
*/
#include <assert.h>
#include "elsq_xBlockDiag.h"
#include "DoubleMatrix.h"
#include "subtract.h"
#include "mulByScalar.h"

DoubleMatrix elsq_xBlockDiag(const DoubleMatrix &dvecR,    // m size vector, z - h
                             const DoubleMatrix &dmatQInv, // m by m symmetric, positive definite
                             const DoubleMatrix &dmatH_x,  // m by n matrix
                             const DoubleMatrix &dmatQ_x,  // m*m by n matrix
					         const DoubleMatrix &dvecN     // array of block sizes of dmatQ
                             )
{
	// Check input
	const int nY = dvecR.nr();
    const int nB = dmatQ_x.nc();
	const int nInd = dvecN.nr();
    assert( dmatQInv.nr() == nY );
    assert( dmatQInv.nc() == nY );
    assert( dmatH_x.nr() == nY );
    assert( dmatH_x.nc() == nB );
    assert( dmatQ_x.nr() == nY * nY );

	// Prepare variables
	int i, j, k, ind;
    const double* pQ_x  = dmatQ_x.data();
    const double* pQInv = dmatQInv.data();
    const double* pR    = dvecR.data();
	const double* pH_x  = dmatH_x.data();
	const double* pN    = dvecN.data();

	DoubleMatrix drowAns( 1, nB ); 
    DoubleMatrix drowTerm1( 1, nB );
    DoubleMatrix drowTerm2( 1, nB );
    DoubleMatrix drowTerm3( 1, nB );
	DoubleMatrix drowW( 1, nY );

    drowTerm1.fill( 0.0);
    drowTerm2.fill( 0.0);
    drowTerm3.fill( 0.0);
    drowW.fill( 0.0 );

	double* pW = drowW.data();

	// Compute term1
    double* pTerm1 = drowTerm1.data();

    for( k = 0; k < nB; k++ )
	{
		int start = 0;
		for( ind = 0; ind < nInd; ind++ )
		{
			int Ni = static_cast<int>(pN[ ind ]);
			int kk = k * nY * nY;
			for( j = 0; j < Ni; j++ )
			{
				for( i = start; i < start + Ni; i++ )
				{
				    pTerm1[ k ] += pQInv[ i ] * pQ_x[ i + kk ];					
				}
				start += nY;
			}
			start += Ni;
		}
	}

	// Compute term2
    double* pTerm2 = drowTerm2.data();

	int start = 0;
    int begin = 0;
    for( ind = 0; ind < nInd; ind++ )
	{
		int Ni = static_cast<int>(pN[ ind ]);
        for( j = start; j < start + Ni; j++ )
		{
			int jj = ( j - start ) * nY + begin;
            for( i = start; i < start + Ni; i++ )
			{
                pW[ j ] += pR[ i ] * pQInv[ i + jj ];
			}
			for( k = 0; k < nB; k++ )
			{
				pTerm2[ k ] += pW[ j ] * pH_x[ j + k * nY ];
			}
		}
		start += Ni;
		begin += Ni * nY;
	}

    // Compute term3
    double* pTerm3 = drowTerm3.data();

    for( k = 0; k < nB; k++ )
    {
		int start = 0;
		for( ind = 0; ind < nInd; ind++ )
		{
			int Ni = static_cast<int>(pN[ ind ]);
			int kk = k * nY * nY;
			for( j = start; j < start + Ni; j++ )
			{
				int jj = j * nY + kk;
				for( i = start; i < start + Ni; i++ )
				{	
					double val = pW[ i ] * pW[ j ] * pQ_x[ jj + i ];
					if( i < j )
					{
						pTerm3[ k ] += val;
					}				
					if( i == j )
					{
						pTerm3[ k ] += val * 0.5;
					}
				}
			}
			start += Ni;
		}
    }
    
    subtract( subtract( mulByScalar( drowTerm1, 0.5 ), drowTerm2 ), drowTerm3, drowAns );

    return drowAns;
}
