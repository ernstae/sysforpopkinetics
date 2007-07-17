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
/*********************************************************************************************
 *
 * File: centdiff.h
 *
 * Generic version of centdiff
 *
 * (f(xi+hi)-f(xi-hi))/(hi*2)
 *
 * Let x be a n dimensional parameter vector, and h be a step size vector.
 * Given that f(x) evaluates to a nR by nC matrix, where nR>0 and nC>0,
 * (f(xi+hi)-f(xi-hi))/(hi*2) evalutes to a matrix with dimensions
 * of nR*nC by n.
 *
 * Note that f(x) must return a value as a matrix! (ie. a scalar = 1 by 1 matrix)
 *
 * Author: Sachiko Honda
 *
 *********************************************************************************************/

#ifndef CENTDIFF_H
#define CENTDIFF_H

#pragma warning( disable : 4786 )

#include "DoubleMatrix.h"
#include "SpkValarray.h"

/*************************************************************************
 *
 * Function: centdiff
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin centdiff$$
$spell 
    jacobian 
    namespace 
    cout 
    std 
    centdiff 
    una 
    res 
    fct 
    th 
    const 
    trancation 
    fordiff 
    iostream 
    cc 
    mul 
    int 
    dh 
    endl 
    typedef
    roundoff
    Yi
    Spk
    ed
    rvec
    valarray
    fval
  $$

$section <DoubleMatrix> Central difference approximation for the Jacobian of f(x)$$

$cindex central difference approximation$$
$index Jacobian$$


$table
$bold Prototype:$$   $cend
template <class /UnaFun/>
const DoubleMatrix centdiff( const /UnaFun/ &/f/, const DoubleMatrix &/x/, const DoubleMatrix &/h/)
$cend
$tend

See also: $xref/centdiffVA//valarray version of generic centdiff/$$.

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Returns the central difference approximation for the Jacobian of a matrix valued
function $math%f(x)%$$, where $math%x%$$ is a vector.
$pre

$$
The vector $math%x%$$ specifies the points at which to approximate 
the Jacobian.  $math%h%%$$ specifies the step size for approximating 
the partials of $math%f%$$.  The i-th element of $math%h%$$ is used as a step size for the i-th element of
$math%x%$$. 

If $math%h(j)%$$ is 0.0, partial with respect to $math%x(j)%$$ are not approximated and 0.0 is 
returned in the corresponding column of the Jacobian.
$pre

$$
If the evaluation of $math%f(x)%$$ results in a  
$math%m%$$ by $math%n%$$ matrix, and $math%x%$$ and $math%h%$$ are vectors of length
$math%l%$$, then the return value of $code centdiff$$
is a $math%m * n%$$ by $math%l%$$ matrix in which the k-th column contains a derivative of f(x) with respect to
the k-th element of $math%x%$$ and each derivative is $xref/rvec//rvec-ed/$$.
$pre

$$
The functions 
$code centdiff$$ and $code fordiff$$ can be used to 
approximate derivatives for an optimization or zero-finding algorithm. 
The $code centdiff$$ function is more accurate, 
but it requires more function evaluations.
$pre

$$
$math%h%$$ should be chosen carefully.  $math%(EPSILON*x/2.0)^(1/3)%$$ 
gives an optimal $math%h%$$ where the difference between the roundoff and 
trancation errors reaches minimal.  Every element of $italic h%$$ must be
greater than 0.0; otherwise, the program terminates.

$head Arguments$$
$syntax/
/UnaFct/
/$$
specifies the function class in which a $bold unary$$ function (i.e. f(x)), to be evaluated is defined 
as an overloaded operator() that takes a single parameter.  The function is executed
exactly in the form of a call f(x), if the $italic Fct$$ object is $math%f%$$ and the
parameter is $math%x%$$.

$syntax/

/f/
/$$
is a reference to a unary function object of type $italic UnaFct$$.

$syntax/

/x/
/$$
is a $math%l%$$ dimensional parameter vector of type $italic DoubleMatrix$$
 specifying the function evaluation points.
$syntax/

/h/
/$$
is a $math%l%$$ dimensional step value vector of type $italic DoubleMatrix$$.
All values must be positive, or the system will terminate.

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include <functional>
    #include "centdiff.h"
    #include "DoubleMatrix.h"
    
    using namespace std;
    class Function : public std::unary_function<DoubleMatrix, DoubleMatrix>
    {
        const DoubleMatrix c1, c2;
    public: 
        Function(const DoubleMatrix& cc1, const DoubleMatrix cc2)
            : c1(cc1), c2(cc2) {}
        ~Function(){}
        Function(const Function& right) : c1(right.c1), c2(right.c2) {}

        //
        // Defines f(x) = x * c1 + c2
        //
        const DoubleMatrix operator()(const DoubleMatrix& x) const
        {
            return add(mulByScalar(x,c1),c2);
        }
    };
    void main()
    {
        int n=3;
        DoubleMatrix c1(n,1), c2(n,1);
        c1.fill(0.5);
        c2.fill(3.0);
        
        DoubleMatrix x(n,1);
        x.fill(1.0);
        DoubleMatrix h(n,1);
        double *dh = h.data();
        for( int i=0; i<n; i++ )
            dh[i] = 0.1*i;
          
        Function f(c1, c2);
        cout << "f_x(x) = " << endl;
        cout << centdiff<Function, DoubleMatrix, DoubleMatrix>(f, x, h);
    }
$$
then it will display the following when it is run:
$codep
    f_x(x) = 
    3 by 3
    [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    [ 0.0000000000000000e+000 4.9999999999999822e-001 0.0000000000000000e+000 ]
    [ 0.0000000000000000e+000 0.0000000000000000e+000 5.0000000000000044e-001 ]
$$
$end
*/

#include <cassert>
#include "replaceJth.h"
#include "rvec.h"
#include "subtract.h"
#include "divByScalar.h"
#include "matmin.h"

template <class UnaFun>
const DoubleMatrix centdiff( const UnaFun &f, const DoubleMatrix &x, const DoubleMatrix &h)
{
    assert( x.nc() == 1 );
    assert( h.nc() == 1 );
    assert( x.nr() == h.nr() );
    assert( matmin(h) >= 0.0 );

    // Parameter size
    int n = x.nr();

    // Compute the center values to figure out the output matrix's dimensions (n by m)
    // The operation immediately below will fail at compilation if f(x) does not evaluate to a matrix.
    DoubleMatrix ctr = f(x);

    int nR = ctr.nr();
    int nC = ctr.nc();

    DoubleMatrix xU;
    DoubleMatrix xL;

    DoubleMatrix jacob(nR*nC, n);
    DoubleMatrix delta;

    double *dxU, *dxL;
    const double * dx = x.data();
    const double * dh = h.data();

    // fill jacob with 0.0 so that if h[i] is 0 and operation is skipped
    // the i-th column of jacob still has clean values.
    jacob.fill(0);

    for(int i=0; i<n; ++i)
    {
        if(dh[i] != 0)
        {
            // vary only the i-th component of the parameter vector
            xU = x;
            xL = x;
            dxU = xU.data();
            dxL = xL.data();
            dxU[i] = dx[i]+dh[i];
            dxL[i] = dx[i]-dh[i];

            delta = divByScalar( subtract(f(xU), f(xL)), 2.0*dh[i] );

            // If f(x) were a matrix valued function, we need to rvec it.
            // If not, rvec() doesn't do anything.
            // Then, place that resulting column vector in the i-th column of
            // the returning matrix.
            replaceJth(jacob, i, rvec(delta));
        }
    }

    // Compute the center value again to restore any state variables
    // for f to their original values.
    ctr = f(x);

    return jacob;
}
/*************************************************************************
 *
 * Function: centdiff
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin centdiffVA$$
$spell 
    jacobian 
    namespace 
    cout 
    std 
    centdiff 
    una 
    res 
    fct 
    th 
    const 
    trancation 
    fordiff 
    iostream 
    cc 
    mul 
    int 
    dh 
    endl 
    typedef
    roundoff
    valarray
    Yi
    resize
    Spk
    ed
    rvec
    valarray
    cols
    SpkValarray
    fval
  $$

$section <valarray> Central difference approximation for the Jacobian of f(x)$$

$cindex central difference approximation$$
$index Jacobian$$


$table
$bold Prototype:$$   $cend
$syntax/template <class /UnaFun/, class /DataType/>
const SPK_VA::valarray</DataType/> centdiff( 
  const /UnaFun/ &/f/, int /fvalCols/, 
  const SPK_VA::valarray</DataType/> &/x/, 
  const SPK_VA::valarray</DataType/> &/h/)
/$$ $rend
$cend
$tend

See also: $xref/centdiff//DoubleMatrix version of generic centdiff/$$.

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Returns the central difference approximation for the Jacobian of a matrix valued
function $math%f(x)%$$, where $math%x%$$ is a vector.
$pre

$$
The vector $math%x%$$ specifies the points at which to approximate 
the Jacobian.  $math%h%%$$ specifies the step size for approximating 
the partials of $math%f%$$.  The i-th element of $math%h%$$ is used as a step size for the i-th element of
$math%x%$$. 

If $math%h(j)%$$ is 0.0, partial with respect to $math%x(j)%$$ are not approximated and 0.0 is 
returned in the corresponding column of the Jacobian.
$pre

$$
If the evaluation of $math%f(x)%$$ results in a  
$math%m%$$ times $math%n%$$ vector, and $math%x%$$ and $math%h%$$ are vectors of length
$math%l%$$, then the return value of $code centdiff$$
is a $math%m * n%$$ times $math%l%$$ vector containing 
a sequence of $xref/rvec//rvec-ed/$$ derivatives 
of f(x) with respect to
the k-th element of $math%x%$$.
$pre

$$
The functions 
$code centdiff$$ and $code fordiff$$ can be used to 
approximate derivatives for an optimization or zero-finding algorithm. 
The $code centdiff$$ function is more accurate, 
but it requires more function evaluations.
$pre

$$
$math%h%$$ should be chosen carefully.  $math%(EPSILON*x/2.0)^(1/3)%$$ 
gives an optimal $math%h%$$ where the difference between the roundoff and 
trancation errors reaches minimal.  Every element of $italic h%$$ must be
greater than 0.0; otherwise, the program terminates.

$head Arguments$$
$syntax/
/UnaFct/
/$$
specifies the function class in which a $bold unary$$ function (i.e. f(x)), to be evaluated is defined 
as an overloaded operator() that takes a single parameter.  The function is executed
exactly in the form of a call f(x), if the $italic Fct$$ object is $math%f%$$ and the
parameter is $math%x%$$.

$syntax/

/DataType/
/$$
specifies the data type of the elements of vectors, $italic x$$ and $italic h$$.

$syntax/

/f/
/$$
is a reference to a unary function object of type $italic UnaFct$$.

$syntax/

/nCols/
/$$
is the number of columns 

$syntax/

/x/
/$$
is a $math%l%$$ dimensional parameter vector of type $italic DoubleMatrix$$ or $italic SPK_VA::valarray$$
 specifying the function evaluation points.
$syntax/

/h/
/$$
is a $math%l%$$ dimensional step value vector of type $italic DoubleMatrix$$ or $italic SPK_VA::valarray$$.
All values must be positive, or the system will terminate.

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include <functional>
    #include "centdiff.h"
    #include "SpkValarray.h"
    #include "DoubleMatrix.h"
    
    using namespace std;
    class Function : public std::unary_function< SPK_VA::valarray<double>, SPK_VA::valarray<double> >
    {
        const SPK_VA::valarray<double> c1, c2;
    public: 
        Function(const SPK_VA::valarray<double>& cc1, const SPK_VA::valarray<double> cc2)
            : c1(cc1), c2(cc2) {}
        ~Function(){}
        Function(const Function& right) : c1(right.c1), c2(right.c2) {}

        //
        // Defines f(x) = x * c1 + c2
        //
        const SPK_VA::valarray<double> operator()(const SPK_VA::valarray<double>& x) const
        {
            // returns a n times n dimension vector.
            return (x * c1) + c2;
        }
    };
    void main()
    {
        int n=3;
        SPK_VA::valarray<double> c1(0.5, n);
        SPK_VA::valarray<double> c2(3.0, n);
        
        SPK_VA::valarray<double> x(1.0, n);
        SPK_VA::valarray<double> h(0.1, n);
          
        Function f(c1, c2);
        cout << "f_x(x) = " << endl;
        cout << DoubleMatrix( centdiff<Function, double>(f, n, x, h), n );
    }
$$
then it will display the following when it is run:
$codep
    f_x(x) = 
    3 by 3
    [ 0.0000000000000000e+000 0.0000000000000000e+000 0.0000000000000000e+000 ]
    [ 0.0000000000000000e+000 4.9999999999999822e-001 0.0000000000000000e+000 ]
    [ 0.0000000000000000e+000 0.0000000000000000e+000 5.0000000000000044e-001 ]
$$
$end
*/
template <class UnaFun, class DataType>
const SPK_VA::valarray<DataType> centdiff( const UnaFun &f, int fvalCols, const SPK_VA::valarray<DataType> &x, const SPK_VA::valarray<DataType> &h)
{
    assert( x.size() == h.size() );

    // Parameter size
    int nX = x.size();
    int k;

#ifndef NDEBUG
    for( k=0; k<nX; k++ )
    {
      if( h[k] < 0.0 )
      {
        throw SpkException( SpkError::SPK_DIFF_ERR, "Step sizes must be all positive.", __LINE__, __FILE__ );
      }
    }
#endif
    // Compute the center values to figure out the output matrix's dimensions (n by m)
    // The operation immediately below will fail at compilation if f(x) does not evaluate to a matrix.
    SPK_VA::valarray<DataType> ctr = f(x);
    const int fvalRows = ctr.size() / fvalCols;
    assert( ctr.size() == fvalRows * fvalCols );
    const int nY = fvalRows * fvalCols;

    SPK_VA::valarray<DataType> xU(nX);
    SPK_VA::valarray<DataType> xL(nX);

    SPK_VA::valarray<DataType> delta(nY);

    // fill jacob with 0.0 so that if h[i] is 0 and operation is skipped
    // the i-th column of jacob still has clean values.
    SPK_VA::valarray<double> jacob(nY * nX);

    for( k=0; k<nX; k++ )
    {
        if(h[k] != 0)
        {
            // vary only the i-th component of the parameter vector
            xU = x;
            xL = x;
            xU[k] = x[k]+h[k];
            xL[k] = x[k]-h[k];

            delta = ( f(xU) - f(xL) ) / ( 2.0 * h[k] );

            // If f(x) were a matrix valued function, we need to rvec it.
            // If not, rvec() doesn't do anything.
            // Then, place that resulting column vector in the i-th column of
            // the returning matrix.
            for( int i=0; i<fvalRows; i++ )
            {
              for( int j=0; j<fvalCols; j++ )
              {
                jacob[ k*(fvalCols*fvalRows) + i*fvalCols + j ]
                  = delta[i + j * fvalRows];
              }
            }
        }
    }

    // Compute the center value again to restore any state variables
    // for f to their original values.
    ctr = f(x);

    return jacob;
   
}

#endif
