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
/*******************************************************
 *
 * File: mapObj.cpp
 *
 * Map Bayesian Objective Function
 *
 * Author: Sachiko Honda
 *
 *******************************************************/
/*******************************************************
 *
 * Function: mapObj
 *
 *******************************************************/
/*------------------------------------------------------
 *   Function specification
 *------------------------------------------------------*/
/*
$begin mapObj$$
$escape #$$

$spell
    throwExcepIfMaxIter
    const
  Model model
    bool
    const
    Bayesian
    dmat
    dvec
    Rinv
    Qinv
    elsq
    elsq_x
    std
    endl
    cout
    pow
    pd
    nr
    nc
    obj
    namespace
    cmath
    iostream
    isf
    ind
    Spk
    pf 
    pb 
    covariances
    covariance
    inv
    valarray
    Ri
  Fo
  resize
$$

$section Map Bayesian Objective Function$$

$index mapObj$$
$cindex map Bayesian objective function$$
$index individual, objective function$$

$table
$bold Syntax:$$ $cend
$syntax/void mapObj(
    SpkModel           & /model/,
    const DoubleMatrix & /y/,
    const DoubleMatrix & /b/,
    double             * /pMapObjOut/,
    DoubleMatrix       * /pMapObj_bOut/,
    bool                 /withD/,
    bool                 /isFO/,
    const DoubleMatrix * /pN/ = NULL
    /$$

$tend

$table
$bold See also: $$ $cend
$xref/mapObjFuncOb//(function class) The Map Bayesian Objective Function/$$ $rend
$cend
$xref/mapObj_bFuncOb//(function class) The Derivative of Map Bayesian Objective Function/$$ $rend
$tend

$fend 30$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Evaluates the map Bayesian objective function. To be specific,
$math%
            1 %          %            1          T   -1
MapObj(b) = - #logdet[ 2 #pi R(b) ] + - [y - f(b)] R(b) [y - f(b)]
            2 %          %            2

            1 %          %            1  T  -1
          + - #logdet[ 2 #pi D ]    + - b  D  b
            2 %          %            2
%$$
(The equation above uses
$xref/glossary/Individual Notation/individual notation/$$.)

$head Reference$$
$italic
Approximating The Maximum Likelihood Estimate For Models With Random Parameter
$$

$head Return Value$$
Upon a successful completion, the function sets
the user-given output value place holders to point to the result values (ones that are requested).
If a failure occurs during the evaluation, a SpkException object is
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$  
$syntax/
/model/
/$$
is a reference to a SpkModel object that is a function of $math%b%$$ if $italic withD$$ is $math%false%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on only b/$$)
or a function of $math%alp%$$ and $math%b%$$ if $italic withD$$ is $math%true%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on alp and b/$$).
for details.

$syntax/

/y/
/$$
is a $math%n%$$ dimensional column vector containing the measurement data vector.
$syntax/

/b/
/$$
The $math%m%$$ dimensional column vector $italic b$$
specifies a value for the parameter vector.
$syntax/

/pMapObjOut/
/$$
is a pointer to a double-precision object or $code NULL$$.
User must allocate memory to $italic pMapObjOut$$ when requesting the value of the 
map Bayesian objective function.  If $code NULL$$ is given, 
the objective function will not be evaluated.
If not $code NULL$$, $italic pMapObjOut$$ will point to a double-precision number 
containing the value of the objective function if the evaluation completes successfully.

$syntax/

/pMapObj_bOut/
/$$
is a pointer to a $code DoubleMatrix$$ or $code NULL$$.
User must allocate memory to $italic pMapObj_bOut$$, $math%1 by nB%$$ matrix object,
when requesting the value of the 
derivative of the map Bayesian objective function; i.e., $math%mapObj_b(b)%$$. 
If $code NULL$$ is specified for $italic pMapObj_bOut$$, 
the objective function will not be evaluated.
If not $code NULL$$, $italic pMapObj_bOut$$ will point to a $math%1 by m%$$ matrix (row vector)  
containing the value of the derivative of the objective function if the evaluation 
completes successfully.

$syntax/

/withD/
/$$
When $italic withD$$ is specified $math%false%$$,
the system assumes that the prior distribution is provided by the user.
In such a case, $italic withD$$ is a function of $math%alp%$$ and $math%b%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on alp and b/$$).
Otherwise, $italic model$$ is a function of only $math%b%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on only b/$$)

$syntax/

/isFO/
/$$
If $italic isFO$$ is $math%false%$$, then the Modified First Order will be chosen for approximation.
If $math%true%$$ is given, other approximations are assumed.

$syntax/

/pN/(null by default)
/$$ 
If $italic isFO$$ is specified as $math%true%$$, $italic pN$$ points to a DoubleMatrix 
object that contains the column vector $math%N%$$.  The $th i$$ element of $math%N%$$
specifies the number of elements of $math%y%$$ that correspond to the $th i$$ individual.
If $italic isFO$$ is specified as $math%false%$$, set $italic N$$ to null.


$head Example$$
Suppose that
$math%
       /  b(1)     0  \         / 1  0 \
R(b) = |              |    D  = |      |
       \  0      b(1) /         \ 0  1 /

       / b(2) \        / 1 \
f(b) = |      |   y =  |   |
       \ b(2) /        \ 1 /
%$$
It follows that
$math%                         
MapObj(b) = (1/2) #log{[2 #pi b(1)]^2} + [1 - b(2)]^2 / b(1)
          + (1/2) #log{[2 #pi]^2}      + (1/2) [b(1)^2 + b(2)^2]
%$$
The gradient of $math%MapObj(b)%$$ is equal to
$math%
    / 1 / b(1) - [1 - b(2)]^2 / b(1)^2 + b(1) \
    |                                         |
    \     - 2 [1 - b(2)] / b(1) + b(2)        /
%$$
If all the components of $math%b%$$ are two,
$math%
MapObj(b)   = #log(4 #pi) + #log(2 #pi) + 1 / 2 + 4
            = #log(8 #pi^2) + 4.5

MapObj_b(b) = [ 1 / 2 - 1 / 4 + 2 , 1 + 2  ]
            = [ 2.25 , 3 ]
%$$
If you enter
$codep

    #include <iostream>
    #include <cmath>
    #include "DoubleMatrix.h"
    #include "mapObj.h"
    #include "SpkModel.h"
    #include "PI.h"
    #include "SpkValarray.h"

    using SPK_VA::valarray;
    class UserModel : public SpkModel
    {
        valarray<double> _b;

        const int _nY;
    public:
        UserModel( int nY ): _nY(nY) {};
        ~UserModel(){};

    protected:
        void doSetIndPar(const valarray<double>& b)
        {
            _b = b;
        }
        void doDataMean( valarray<double>& fOut ) const
        {
          //
          // fOut = [ b(2) ]
          //        [ b(2) ]
          //
          fOut.resize( _nY, _b[1] );
        }
        bool doDataMean_indPar( valarray<double>& f_bOut ) const
        {
          //
          // f_bOut = [ 0  1 ]
          //          [ 0  1 ]
          //
          f_bOut.resize( _nY * _b.size(), 0.0 );
          f_bOut[2] = 1.0;
          f_bOut[3] = 1.0;
          return true;
        }
        void doDataVariance( valarray<double>& ROut ) const
        {
          // 
          // ROut = [ b(1)  0   ]
          //        [  0   b(1) ]
          //
          ROut.resize( _nY * _nY, 0.0 );
          ROut[0] = _b[0];
          ROut[3] = _b[0];
        }
        bool doDataVariance_indPar( valarray<double>& R_bOut ) const
        {
          //
          // R_bOut = [ 1  0 ]
          //          [ 0  0 ]
          //          [ 0  0 ]
          //          [ 1  0 ]
          //
          R_bOut.resize( _nY * _nY * _b.size(), 0.0 );
          R_bOut[0] = 1.0;
          R_bOut[3] = 1.0;
          return true;
        }
        void doIndParVariance( valarray<double>& DOut ) const
        {
            //
            // Return a matrix;
            //
            // [ 1  0 ]
            // [ 0  1 ]
            //
            DOut.resize( _b.size() * _b.size(), 0.0 );
            DOut[0] = 1.0;
            DOut[3] = 1.0;
        }
    };
    void main(){

        using namespace std;

        // 
        // b = [ 2.0 ]
        //     [ 2.0 ]
        //
        const int nB = 2;
        DoubleMatrix b(nB, 1);
        b.fill(2.0);

        //
        // y = [ 1.0 ]
        //     [ 1.0 ]
        //
        const int nY = 2;
        DoubleMatrix y(nY, 1);
        y.fill(1.0);

        double       mapObjOut;
        DoubleMatrix mapObj_bOut( 1, nB );
        bool withD = true;
        bool isFO  = false;
        DoubleMatrix * pN = NULL;

        UserModel model(nY);

        try{
          mapObj( model, y, b, &mapObjOut, &mapObj_bOut, withD, isFO, pN );
        }
        catch( ... )
        {
            abort();

        }
        
        cout << "mapObjOut - log(8.0 * PI^2.0)) = " << mapObjOut - log(8.0*pow(PI,2.0)) << endl;
        cout << endl;
        cout << "mapObj_bOut                    = ";
        mapObj_bOut.print();
        cout << endl;
    }

$$
The following results will be displayed:
$codep

mapObjOut - log(8.0 * PI^2.0)) = 4.5

mapObj_bOut                    = [ 2.25 3 ]

$$

$end
*/
/*------------------------------------------------------
 *   Include files
 *------------------------------------------------------*/
#include <iostream>
#include <cstdlib>
#include <cmath>
#include "SpkModel.h"
#include "mapObj.h"
#include "elsq_x.h"
#include "elsq_xBlockDiag.h"
#include "add.h"
#include "transpose.h"
#include "multiply.h"
#include "DoubleMatrix.h"
#include "subtract.h"
#include "pi.h"
#include "SpkException.h"
#include "det.h"
#include "getSubblock.h"

/*------------------------------------------------------
 *   Local definitions
 *------------------------------------------------------*/

static const double logTwoPi = log( 2.0 * PI );

/*------------------------------------------------------
 *   Function definition
 *------------------------------------------------------*/
/*
 * Identifiers correspoindance:
 *
 *    mapObj               elsq/elsq_x
 *       y                     z
 *       f                     h
 *       f_b                   h_x
 *       R                     Q
 *       Rinv                  Qinv
 *       R_b                   Q_x
 */
static DoubleMatrix dvecF(__FILE__);
static DoubleMatrix dmatF_b(__FILE__);
static DoubleMatrix dmatR(__FILE__);
static DoubleMatrix dmatR_b(__FILE__);
static DoubleMatrix dmatRInv(__FILE__);
static DoubleMatrix dmatDInv(__FILE__);
static DoubleMatrix dvecResidual(__FILE__);
static DoubleMatrix dvecBTrans(__FILE__);
static DoubleMatrix drowBTransInvD(__FILE__);
static DoubleMatrix mapObj_bOutTemp(__FILE__);

// David: notice that we first calculate the objective, by using the elsq function,
// and then we calculate the derivative, by using the elsq_x function. Y-F is an expression
// that is common to both the objective and its derivative.  If the two were calculated in tandem
// I think some time could be saved overall.  This appears to be generally true in other cases
// where the objective and its derivative are calculated one after the other.
//
// Response Watrous 7/19/01: This should be addressed by the Covariance 
// objects that have been added to the user-model class.

void mapObj(  SpkModel &model, 
              const   DoubleMatrix &dvecY,
              const   DoubleMatrix &dvecB,
              double *pMapObjOut,
              DoubleMatrix *pMapObj_bOut,
              bool withD,
              bool isFO,
              const   DoubleMatrix* pdvecN 
           )
{
    //------------------------------------------------------------
    // Preliminaries.
    //------------------------------------------------------------

    using namespace std;

    if( pMapObjOut == 0 && pMapObj_bOut == 0 )
        return;

    if( isFO )
      assert( pdvecN != NULL );

    double mapObjOutTemp;

    int nB = dvecB.nr();
    int nY = dvecY.nr();

    const valarray<double> y = dvecY.toValarray();
    const valarray<double> b = dvecB.toValarray();

    valarray<double> f(nY), f_b(nY*nB);
    valarray<double> R(nY*nY), R_b(nY*nY*nB);
    valarray<double> residual(nY);

    //
    // Revisit - Exception - Sachiko
    //
    // This function call may throw SpkException.
    //
    model.setIndPar(b);

    // Calculate f_b(b) before f(b) to allow caching.
    if( pMapObj_bOut != 0 )
    {
        //
        // Revisit - Exception - Sachiko
        //
        // This function call may throw SpkException.
        //
        model.dataMean_indPar(f_b);
        dmatF_b.fromValarray( f_b, nB );
        assert( dmatF_b.nr() == nY );
        assert( dmatF_b.nc() == nB );
    }

    //
    // Revisit - Exception - Sachiko
    //
    // This function call may throw SpkException.
    //
    model.dataMean(f);
    dvecF.fromValarray( f, 1 );
    assert( dvecF.nr() == nY );
//cout << __FILE__ << ", " << __LINE__ << "f(b) = " << "f(" << b << ") = " << f << endl;

    //subtract(dvecY, dvecF, dvecResidual);
    residual = y - f;
    dvecResidual.fromValarray( residual, 1 );

    // Calculate R_b(b) before R(b) to allow caching.
    if( pMapObj_bOut != 0 )
    {
        //
        // Revisit - Exception - Sachiko
        //
        // This function call may throw SpkException.
        //
        model.dataVariance_indPar( R_b );
        dmatR_b.fromValarray( R_b, nB );
        assert( dmatR_b.nr() == nY*nY );
        assert( dmatR_b.nc() == nB );
    }


    //------------------------------------------------------------
    // Calculate the objective function, if requested.
    //------------------------------------------------------------

    if( pMapObjOut != 0 )
    {
        //
        // Calculate the first two terms in MapObj(b),
        //
        //     1                        1           T  -1
        //     -  logdet[ 2 pi R(b) ] + - [y - f(b)]  R  (b) [y - f(b)]  .
        //     2                        2
        //

        //
        // Revisit - Exception - Sachiko
        //
        // This function call may throw SpkException.
        //
        if( isFO )
		{
            const int nInd = pdvecN->nr();
            valarray<double> R;
            model.dataVariance( R );
            dmatR.fromValarray( R, nY );
            valarray<double> RInv;
            model.dataVarianceInv( RInv );
            const double* pN = pdvecN->data();
            int start = 0;
            double db;
            long int lc;
            double term1 = 0.0;
            double term2 = 0.0;
            for( int ind =0; ind < nInd; ind++ )
			{
                int Ni = static_cast<int>( pN[ ind ] );
                det( getSubblock( dmatR, start, start, Ni, Ni ), &db, &lc );
                term1 += log( db ) + lc * log(2.0);
                for( int j = start; j < start + Ni; j++ )
				{
                    int jj = j * nY;
                    for( int i = start; i < start + Ni; i++ )
					{
                        double val = residual[ i ] * residual[ j ] * RInv[ jj + i ];
                        if( i < j ) term2 += val;   
                        if( i == j )term2 += val * 0.5;
					}
				}
                start += Ni;
			}
            mapObjOutTemp = 0.5 * ( nY * logTwoPi + term1 ) + term2;
		}
        else
            mapObjOutTemp = 0.5 * ( nY * logTwoPi + 
                            model.getDataCovariance().logdet() + 
                            model.getDataCovariance().weightedSumOfSquares( residual ) );
        if( withD )
		{
            // Add in the last two terms in MapObj(b), 
            //
            //     1                       1  T  -1
            //     - logdet[ 2 pi D ]    + - b  D    b  .
            //     2                       2
            //
            //
            // Revisit - Exception - Sachiko
            //
            // This function call may throw SpkException.
            //
            mapObjOutTemp += 0.5 * ( nB * logTwoPi + 
                model.getIndParCovariance().logdet() + 
                model.getIndParCovariance().weightedSumOfSquares( b ) );
        }
    }


    //------------------------------------------------------------
    // Ccalculate the derivative of the objective function, if requested.
    //------------------------------------------------------------

    if( pMapObj_bOut != 0 )
    {
        assert(pMapObj_bOut->nr() == 1);
        assert(pMapObj_bOut->nc() == nB);

        mapObj_bOutTemp.resize(1,nB);
        
        // Calculate the first three terms in MapObj_b(b),
        //
        //     1            T       -1              T  -1
        //     -  rvec[R(b)]  R_b(b)    - [y - f(b)]  R  (b)  f_b(b)
        //     2  
        //
        //                 1           T              T         -1
        //               + - { [y-f(b)]  kron [y-f(b)]  } R_b(b)    .
        //                 2 
        //

        //
        // Revisit - Exception - Sachiko
        //
        // This function call may throw SpkException.
        //
        valarray<double> R;
        model.dataVariance( R );
        dmatR.fromValarray( R, nY );
        assert( dmatR.nr() == nY );
        assert( dmatR.nc() == nY );

        //
        // Revisit - Exception - Sachiko
        //
        // This function call may throw SpkException.
        //
        valarray<double> RInv;
        model.dataVarianceInv( RInv );
        dmatRInv.fromValarray( RInv, nY );
        assert( dmatRInv.nr() == nY );
        assert( dmatRInv.nc() == nY );

        if( isFO )
            mapObj_bOutTemp = elsq_xBlockDiag(dvecResidual, dmatRInv, dmatF_b, dmatR_b, *pdvecN );
        else
            mapObj_bOutTemp = elsq_x(dvecResidual, dmatR, dmatRInv, dmatF_b, dmatR_b);

        assert(mapObj_bOutTemp.nr() == 1 );
        assert(mapObj_bOutTemp.nc() == nB );

        if( withD )
        {
            // Add in the last term in MapObj_b(b), 
            //
            //      T  -1
            //     b  D   .
            //

            //
            // Revisit - Exception - Sachiko
            //
            // This function call may throw SpkException.
            //
            valarray<double> DInv;
            model.indParVarianceInv( DInv );
            dmatDInv.fromValarray( DInv, nB );
            assert( dmatDInv.nr() == nB );
            assert( dmatDInv.nc() == nB );

            transpose(dvecB, dvecBTrans);
            assert( dvecBTrans.nr() == 1 );
            assert( dvecBTrans.nc() == nB );

            multiply(dvecBTrans, dmatDInv, drowBTransInvD);

            mapObj_bOutTemp = add( mapObj_bOutTemp, drowBTransInvD );
        }
    }


    //------------------------------------------------------------
    // Finish it up.
    //------------------------------------------------------------
    if( pMapObjOut )
        *pMapObjOut = mapObjOutTemp;
    if( pMapObj_bOut )
        *pMapObj_bOut = mapObj_bOutTemp;

    return;
}
/*************************************************************************
 *
 * Function Class: MapObj
 *
 *************************************************************************/
/*
$begin mapObjFuncOb$$
$spell
  Bayesian
  ElemType
  Spk
  resize
  obj
  const
  bool
  ind
  valarray
  trancation
  Model model
  centdiff
  cout
  endl
  iostream
  namespace
  std
  instanciate
  mapobj
  resize
  cmath
$$
$section Function Object for Map Bayesian Objective Function$$

$index function object, mapObj$$
$index mapObj, function object$$

$table
$bold Header:$$ 
$cend mapObj.h 
$rend
$bold Constructor: $$ 
$cend 
$syntax/MapObj<class /ElemType/>::MapObj( 
  SpkModel         * /model/, 
  const /ElemType/   & /y/, 
  bool             & /withD/, 
  bool             & /isFO/, 
  const /ElemType/   & /pN/ = NULL )
/$$
$tend

$table
$bold See also: $$ $cend
$xref/mapObj_bFuncOb//(function class) The Derivative of Map Bayesian Objective Function/$$ $rend
$cend
$xref/mapObj//(function) The Map Bayesian Objective Function/$$ $rend
$tend

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This class encapsulate $tref mapObj$$ function and allows user to call/evaluate the function 
as a unary function through $code operator()$$.

$head Arguments$$
$syntax/
/model/
/$$
is a pointer to a SpkModel object that is a function of $math%b%$$ if $italic withD$$ is $math%false%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on only b/$$)
or a function of $math%alp%$$ and $math%b%$$ if $italic withD$$ is $math%true%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on alp and b/$$).
for details.
$syntax/

/y/
/$$
is a column vector containing measurement data for all individuals.
$syntax/

/withD/
/$$
When $italic withD$$ is specified $math%false%$$,
the system assumes that the prior distribution is provided by the user.
In such a case, $italic withD$$ is a function of $math%alp%$$ and $math%b%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on alp and b/$$).
Otherwise, $italic model$$ is a function of only $math%b%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on only b/$$)

$syntax/

/isFO/
/$$
If $italic isFO$$ is $math%false%$$, then the Modified First Order will be chosen for approximation.
If $math%true%$$ is given, other approximations are assumed.


$syntax/

/pN/(null by default)
/$$ 
If $italic isFO$$ is specified as $math%true%$$, $italic pN$$ points to a DoubleMatrix 
object that contains the column vector $math%N%$$.  The $th i$$ element of $math%N%$$
specifies the number of elements of $math%y%$$ that correspond to the $th i$$ individual.
If $italic isFO$$ is specified as $math%false%$$, set $italic N$$ to null.

$head Public Members$$
$syntax/const ElemType operator(const ElemType &/x/) const
/$$
This member function evaluates $tref mapObj$$ at the evaluation point specified by the $italic x$$.
The return value is a $math%n%$$ dimensional row vector, where n is the size of $italic x$$,
containing the Map Bayesian objective for each corresponding individual at given $math%b%$$.

$head Example$$

The following piece of code demonstrates how to pass a function object to centdiff() algorithm as an example:

$codep

    #include <iostream>
    #include <cmath>
    #include "DoubleMatrix.h"
    #include "mapObj.h"
    #include "SpkModel.h"
    #include "PI.h"
    #include "SpkValarray.h"
    #include "centdiff.h"

    using SPK_VA::valarray;
    class UserModel : public SpkModel
    {
        valarray<double> _b;

        const int _nY;
    public:
        UserModel( int nY ): _nY(nY) {};
        ~UserModel(){};

    protected:
        void doSetIndPar(const valarray<double>& b)
        {
            _b = b;
        }
        void doDataMean( valarray<double>& fOut ) const
        {
          //
          // fOut = [ b(2) ]
          //        [ b(2) ]
          //
          fOut.resize( _nY, _b[1] );
        }
        bool doDataMean_indPar( valarray<double>& f_bOut ) const
        {
          //
          // f_bOut = [ 0  1 ]
          //          [ 0  1 ]
          //
          f_bOut.resize( _nY * _b.size(), 0.0 );
          f_bOut[2] = 1.0;
          f_bOut[3] = 1.0;
          return true;
        }
        void doDataVariance( valarray<double>& ROut ) const
        {
          // 
          // ROut = [ b(1)  0   ]
          //        [  0   b(1) ]
          //
          ROut.resize( _nY * _nY, 0.0 );
          ROut[0] = _b[0];
          ROut[3] = _b[0];
        }
        bool doDataVariance_indPar( valarray<double>& R_bOut ) const
        {
          //
          // R_bOut = [ 1  0 ]
          //          [ 0  0 ]
          //          [ 0  0 ]
          //          [ 1  0 ]
          //
          R_bOut.resize( _nY * _nY * _b.size(), 0.0 );
          R_bOut[0] = 1.0;
          R_bOut[3] = 1.0;
          return true;
        }
        void doIndParVariance( valarray<double>& DOut ) const
        {
            //
            // Return a matrix;
            //
            // [ 1  0 ]
            // [ 0  1 ]
            //
            DOut.resize( _b.size() * _b.size(), 0.0 );
            DOut[0] = 1.0;
            DOut[3] = 1.0;
        }
    };
    void main(){

        using namespace std;

        // 
        // b = [ 2.0 ]
        //     [ 2.0 ]
        //
        const int nB = 2;
        DoubleMatrix b(nB, 1);
        b.fill(2.0);

        // 
        // b = [ 2.0 ]
        //     [ 2.0 ]
        //
        DoubleMatrix bStep(nB, 1);
        bStep.fill(0.1);

        //
        // y = [ 1.0 ]
        //     [ 1.0 ]
        //
        const int nY = 2;
        DoubleMatrix y(nY, 1);
        y.fill(1.0);

        double       mapObjOut;
        DoubleMatrix approxMapObj_bOut ( 1, nB );
        bool withD = true;
        bool isFO  = false;
        DoubleMatrix * pN = NULL;

        UserModel model(nY);

        MapObj<DoubleMatrix> mapObjOb(&model, y, withD, isFO, pN);

        try{
          mapObjOut = mapObjOb(b).data()[0];
        }
        catch( ... )
        {
            abort();

        }
        try{
          approxMapObj_bOut = centdiff< MapObj<DoubleMatrix> >(mapObjOb, b, bStep);
        }
        catch(... )
        {
          abort();
        }
        
        cout << "mapObjOut - log(8.0 * PI^2.0))                = " << mapObjOut - log(8.0*pow(PI,2.0)) << endl;
        cout << endl;
        cout << "Central difference approximation for mapObj_b = ";
        approxMapObj_bOut.print();
        cout << endl;
    }


$$
the program will display;
$codep

mapObjOut - log(8.0 * PI^2.0))                = 4.5

Central difference approximation for mapObj_b = [ 2.24979 3 ]

$$
$end
*/
/*************************************************************************
 *
 * Function Class: MapObj_b
 *
 *************************************************************************/
/*
$begin mapObj_bFuncOb$$
$spell
  Bayesian
  ElemType
  Spk
  resize
  obj
  const
  bool
  ind
  valarray
  trancation
  Model model
  centdiff
  cout
  endl
  iostream
  namespace
  std
  instanciate
  mapobj
  FO
  resize
  cmath
$$
$section Function Object for The Derivative of Map Bayesian Objective Function$$

$index function object, mapObj_b$$
$index mapObj_b, function object$$

$table
$bold Header:$$ 
$cend  mapObj.h 
$rend
$bold Constructor: $$ 
$cend 
$syntax/MapObj_b<class /ElemType/>::MapObj_b( 
      SpkModel       * /model/, 
      const /ElemType/ & /y/, 
      bool             /withD/, 
      bool             /isFO/, 
      const /ElemType/ * /pN/ = NULL )
/$$
$tend

$table
$bold See also: $$ $cend
$xref/mapObjFuncOb//(function class) The Map Bayesian Objective Function/$$ $rend
$cend
$xref/mapObj//(function) The Map Bayesian Objective Function/$$ $rend
$tend

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This class encapsulate $tref mapObj$$ function and allows user to call/evaluate the function 
as a unary function through $code operator()$$.

$head Arguments$$
$syntax/
/model/
/$$
is a reference to a SpkModel object that is a function of $math%b%$$ if $italic withD$$ is $math%false%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on only b/$$)
or a function of $math%alp%$$ and $math%b%$$ if $italic withD$$ is $math%true%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on alp and b/$$).
for details.
$syntax/

/y/
/$$
is a column vector containing measurement data for all individuals.
$syntax/

/withD/
/$$
When $italic withD$$ is specified $math%false%$$,
the system assumes that the prior distribution is provided by the user.
In such a case, $italic withD$$ is a function of $math%alp%$$ and $math%b%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on alp and b/$$).
Otherwise, $italic model$$ is a function of only $math%b%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on only b/$$)

$syntax/

/isFO/
/$$
If $italic isFO$$ is $math%false%$$, then the Modified First Order will be chosen for approximation.
If $math%true%$$ is given, other approximations are assumed.

$syntax/

/pN/(null by default)
/$$ 
If $italic isFO$$ is specified as $math%true%$$, $italic pN$$ points to a DoubleMatrix 
object that contains the column vector $math%N%$$.  The $th i$$ element of $math%N%$$
specifies the number of elements of $math%y%$$ that correspond to the $th i$$ individual.
If $italic isFO$$ is specified as $math%false%$$, set $italic N$$ to null.

$head Public Members$$
$syntax/const ElemType operator(const ElemType & /x/) const
/$$
This member function evaluates $tref mapObj$$ at the evaluation point specified by the $italic x$$.
The return value is a $math%n%$$ dimensional row vector, where n is the size of $italic x$$,
containing the Map Bayesian objective for each corresponding individual at given $math%b%$$.

$head Example$$

The following piece of code demonstrates how to pass a function object to centdiff() algorithm as an example:

$codep

    #include <iostream>
    #include <cmath>
    #include "DoubleMatrix.h"
    #include "mapObj.h"
    #include "SpkModel.h"
    #include "PI.h"
    #include "SpkValarray.h"
    #include "centdiff.h"

    using SPK_VA::valarray;
    class UserModel : public SpkModel
    {
        valarray<double> _b;

        const int _nY;
    public:
        UserModel( int nY ): _nY(nY) {};
        ~UserModel(){};

    protected:
        void doSetIndPar(const valarray<double>& b)
        {
            _b = b;
        }
        void doDataMean( valarray<double>& fOut ) const
        {
          //
          // fOut = [ b(2) ]
          //        [ b(2) ]
          //
          fOut.resize( _nY, _b[1] );
        }
        bool doDataMean_indPar( valarray<double>& f_bOut ) const
        {
          //
          // f_bOut = [ 0  1 ]
          //          [ 0  1 ]
          //
          f_bOut.resize( _nY * _b.size(), 0.0 );
          f_bOut[2] = 1.0;
          f_bOut[3] = 1.0;
          return true;
        }
        void doDataVariance( valarray<double>& ROut ) const
        {
          // 
          // ROut = [ b(1)  0   ]
          //        [  0   b(1) ]
          //
          ROut.resize( _nY * _nY, 0.0 );
          ROut[0] = _b[0];
          ROut[3] = _b[0];
        }
        bool doDataVariance_indPar( valarray<double>& R_bOut ) const
        {
          //
          // R_bOut = [ 1  0 ]
          //          [ 0  0 ]
          //          [ 0  0 ]
          //          [ 1  0 ]
          //
          R_bOut.resize( _nY * _nY * _b.size(), 0.0 );
          R_bOut[0] = 1.0;
          R_bOut[3] = 1.0;
          return true;
        }
        void doIndParVariance( valarray<double>& DOut ) const
        {
            //
            // Return a matrix;
            //
            // [ 1  0 ]
            // [ 0  1 ]
            //
            DOut.resize( _b.size() * _b.size(), 0.0 );
            DOut[0] = 1.0;
            DOut[3] = 1.0;
        }
    };
    void main(){

        using namespace std;

        // 
        // b = [ 2.0 ]
        //     [ 2.0 ]
        //
        const int nB = 2;
        DoubleMatrix b(nB, 1);
        b.fill(2.0);

        // 
        // b = [ 2.0 ]
        //     [ 2.0 ]
        //
        DoubleMatrix bStep(nB, 1);
        bStep.fill(0.1);

        //
        // y = [ 1.0 ]
        //     [ 1.0 ]
        //
        const int nY = 2;
        DoubleMatrix y(nY, 1);
        y.fill(1.0);

        DoubleMatrix mapObj_bOut( 1, nB );
        DoubleMatrix approxMapObj_bOut ( 1, nB );
        bool withD = true;
        bool isFO  = false;
        DoubleMatrix * pN = NULL;

        UserModel model(nY);

        MapObj  <DoubleMatrix> mapObjOb  (&model, y, withD, isFO, pN);
        MapObj_b<DoubleMatrix> mapObj_bOb(&model, y, withD, isFO, pN);

        try{
          mapObj_bOut = mapObj_bOb(b);
        }
        catch( ... )
        {
            abort();

        }
        try{
          approxMapObj_bOut = centdiff< MapObj<DoubleMatrix> >(mapObjOb, b, bStep);
        }
        catch(... )
        {
          abort();
        }
        
        cout << "True mapObj_b                                 = ";
        mapObj_bOut.print();
        cout << endl;
        cout << "Central difference approximation for mapObj_b = ";
        approxMapObj_bOut.print();
        cout << endl;
    }

$$
the program will display;
$codep

True mapObj_b                                 = [ 2.25 3 ]

Central difference approximation for mapObj_b = [ 2.24979 3 ]
$$
$end
*/


