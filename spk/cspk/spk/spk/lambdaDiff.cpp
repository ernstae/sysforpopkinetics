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
 * File: lambdaDiff.cpp
 *
 *
 * Central differences of Lambda (individual joint negative log-likelihood)
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: lambdaDiff
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin lambdaDiff$$
$escape #$$

$spell
	Model model
    const diff str var namespace dmat std int DBL_EPSILON dtemp
    bool nc nr pd dvec dmat endl cout cerr lambda iostream cfloat cmath
    vec temp pow st ind distrib inv Da Fab Rab
    int pf pb palp covariances Spk
    covariance
    Ri
    valarray
    Yi
    resize
$$

$section Central Differences of Individual Joint Negative Log-likelihood$$

$index lambdaDiff$$
$index index, lambda$$
$cindex central difference #of lambda$$

$table
$bold Prototype:$$   $cend  
$syntax/void lambdaDiff(
                SpkModel &/model/,
                const DoubleMatrix &/y/,
                const DoubleMatrix &/alp/,
                const DoubleMatrix &/alpStep/,
                const DoubleMatrix &/b/,
                const DoubleMatrix &/bStep/,
                int   /withRespectToWhichVar/,
                DoubleMatrix * /lambda_xOut/,
                DoubleMatrix * /lambda_x_alpOut/,
                DoubleMatrix * /lambda_x_bOut/,
                const bool /withD/
                )/$$
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
Evaluates the central differences for the derivatives of
$math%Lambda(alp, b)%$$ where
$math%
                 1 %          %                 1               T  -1
Lambda(alp, b) = - #logdet[ 2 #pi R(alp, b) ] + - [y - f(alp, b)] R (alp, b) [y - f(alp, b)]
                 2 %          %                 2

                 1 %          %                 1  T  -1
               + - #logdet[ 2 #pi D(alp) ]    + - b  D (alp)  b
                 2 %          %                 2
%$$
In addition, the derivatives of the central difference approximations
are also evaluated.
(The equation above uses
$xref/glossary/Individual Notation/individual notation/$$.)

$head Reference$$
$italic
Approximating The Maximum Likelihood Estimate For Models With Random Parameter
$$

$head Return Value$$
Upon a successful completion, the function 
sets the given output value place holders to 
point to the result values (ones that are requested).
If a failure occurs during the evaluation, a SpkException object is
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
The return value of $code lambdaDiff$$ is true, if it succeeds,
and false otherwise.  This function requires $code population_analysis$$
namespace.

$syntax/

/model/
/$$
This function expects $italic model$$ to be a function of
all three parameters: $math%alp%$$, $math%b%$$ and $math%i%$$.
Refer $xref/glossary/Model Functions Depend on i - alp - b/Model Functions Depend on i - alp - b/$$
for details.



$syntax/

/y/
/$$
is a $math%nY%$$ dimensional column vector containing the individual's data.
$syntax/

/alp/
/$$
is a $math%nAlp%$$ dimensional column vector 
specifying a value for the fixed population parameter.
$syntax/

/alpStep/
/$$
is a $math%nAlp%$$ dimensional column vector 
specifying the step size for the central difference approximations
with respect to $math%alp%$$.  Every element in $italic alpStep$$ must be
greater than or equal to zero when $italic withRespectToWhichVar$$ is
specified $code WITH_RESPECT_TO_ALP$$.
$syntax/

/b/
/$$
is a $math%nB%$$ dimensional column vector 
specifies a value for the random population parameter.
$syntax/

/bStep/
/$$
is a $math%nB%$$ dimensional column vector
specifying the step size for the central difference approximations
with respect to $math%b%$$.  Every element in $italic bStep$$ must be
greater than or equal to zero when $italic withRespectToWhichVar$$ is
specified $code WITH_RESPECT_TO_B$$.
$syntax/

/withRespectToWhichVar/
/$$
is a constant integer that indicates a variable to the derivative you are trying to compute:
$table
$bold Value$$ $cend $bold Meaning$$ $rend
WITH_RESPECT_TO_ALP        $cend central difference with respect to $math%alp%$$ $rend
WITH_RESPECT_TO_B          $cend central difference with respect to $math%b%$$ $rend
$tend
$syntax/

/Lambda_xOut/
/$$
is a $math%nAlp%$$ or $math%nB%$$ dimensional row vector containing the 
central difference approximation for $math%Lambda(alp, b)%$$ with respect to x (alp/b).
Specify $code '\0'$$ for no request.
$syntax/

/Lambda_x_alpOut/
/$$
is a $math%nAlp or nB by nAlp%$$ matrix containing the true derivative with respect to $math%alp%$$ of the 
central difference approximation for
$math%Lambda_x(alp, b)%$$.
Specify $code '\0'$$ for no request.
$syntax/

/Lambda_x_bOut/
/$$
is a $math%nAlp or nB by nB%$$ matrix containing the true derivative with respect to $math%b%$$ of the 
central difference approximation for
$math%Lambda_x(alp, b)%$$.
Specify $code '\0'$$ for no request.


$head Example$$
Suppose that
$math%
                /  b(1)     0  \
    R(alp, b) = |              |
                \  0      b(1) /

                / alp(1)  0 \
    D(alp)    = |           |
                \ 0  alp(1) /

                / alp(2) + b(2) \
    f(alp, b) = |               |
                \ alp(2) + b(2) /

                / 1 \
    y         = |   |
                \ 1 /
%$$
It follows that
$math%                         
Lambda(alp, b) = (1/2) #log{[2 #pi b(1)]^2}   + [1 - alp(2) - b(2)]^2 / b(1)
               + (1/2) #log{[2 #pi alp(1)]^2} + (1/2) [b(1)^2 + b(2)^2] / alp(1)
%$$
$pre

$$
The transpose of $math%Lambda_b(alp, b)%$$ is equal to
$math%                                          
/                                                                                                            \
| 1 / b(1) - [1 - alp(2) - b(2)]^2 / b(1)^2 + b(1) / alp(1) , - 2 [1 - alp(2) - b(2)] / b(1) + b(2) / alp(1) |
\                                                                                                            /
%$$
The value of $math%Lambda_b_alp(alp, b)%$$ is the $math%2 x 2%$$ matrix
$math%
/ - b(1) / alp(1)^2    2 [1 - alp(2) - b(2)] / b(1)^2 \
|                                                     |
\ - b(2) / alp(1)^2               2 / b(1)            /
%$$
The value of $math%Lambda_b_b(alp, b)%$$ is the $math%2 x 2%$$ matrix
$math%
/-1/b(1)^2 + 2[1-alp(2)-b(2)]^2 / b(1)^3 + 1/alp(1)   2[1-alp(2)-b(2)]/b(1)^2 \
|                                                                             |
\            2[1-alp(2)-b(2)]/b(1)^2                      2/b(1)+1/alp(1)     /
%$$
$pre

$$
The transpose of $math%Lambda_alp(alp, b)%$$ is equal to
$math%                                          
/ 1 / alp(1) - (1/2) [b(1)^2 + b(2)^2] / alp(1)^2 \
|                                                 |
\        - 2 [1 - alp(2) - b(2)] / b(1)           /
%$$
The value of $math%Lambda_alp_b(alp, b)%$$ is the $math%2 x 2%$$ matrix
$math%
/     - b(1) / alp(1)^2           - b(2) / alp(1)^2 \
|                                                   |
\ 2 [1 - alp(2) - b(2)] / b(1)^2      2 / b(1)      /
%$$
The value of $math%Lambda_alp_alp(alp, b)%$$ is the $math%2 x 2%$$ matrix
$math%
/-1/alp(1)^2 + 2[1-alp(2)-b(2)]^2/alp(1)^3    0         \
|                                                       |
\  0                                         2 / b(1)   /
%$$
$pre

$$
If you compile and link the following code,
$codep
    #include <iostream>
    #include <cmath>
    #include <cfloat>
    #include "DoubleMatrix.h"
    #include "lambdaDiff.h"
    #include "SpkModel.h"
    #include "namespace_population_analysis.h"


    class UserModel : public SpkModel
    {
        valarray<double> _a, _b;
        int _i;
        const int _nA, _nB, _nYi;
    public:
        UserModel(int nA, int nB, int nYi)
        : _nA(nA), _nB(nB), _nYi(nYi)
        {};  
        ~UserModel(){};
    protected:
        void doSelectIndividual(int i)
        {
            _i = i;
        }
        void doSetPopPar(const valarray<double>& alp)
        {
            _a = alp;
        }
        void doSetIndPar(const valarray<double>& b)
        {
            _b = b;
        }
        void doIndParVariance( valarray<double>& D ) const
        {
          //
          // D(alp) = [  alp(1)  0.0   ]
          //          [   0.0   alp(1) ]
          //
          D.resize(_nB * _nB);
          D[0] = _alp[0];
          D[1] =  0.0;
          D[2] =  0.0;
          D[3] = _alp[0];
        }
        bool doIndParVariance_popPar( valarray<double>& D_alp ) const
        {
          //
          // D(alp)_alp = [ 1.0   0.0 ]
          //              [ 0.0   0.0 ]
          //              [ 0.0   0.0 ]
          //              [ 1.0   0.0 ]
          //
          D_alp.resize(_nB * _nB * _nA);
          D_alp = 0.0;
          D_alp[0] = 1.0;
          D_alp[3] = 1.0;
          return true;
        }
        void doDataMean( valarray<double>& f ) const
        {
          //
          // f(alp, b) = [  alp(2) + b(2)  ]
          //             [  alp(2) + b(2)  ]
          //
          f.resize( _nYi );
          f[0] = _alp[1] + _b[1];
          f[1] = _alp[1] + _b[1];
        }
        bool doDataMean_popPar( valarray<double>& f_alp ) const
        {
          //
          // f(alp, b)_alp = [ 0  1 ]
          //                 [ 0  1 ]
          //
          f_alp.resize( _nYi * _nYi );
          f_alp[0] = 0.0;
          f_alp[1] = 0.0;
          f_alp[2] = 1.0;
          f_alp[3] = 1.0;
          return true;
        }
        bool doDataMean_indPar( valarray<double>& f_b ) const
        {
          //
          // f(alp, b)_b = [ 0  1 ]
          //               [ 0  1 ]
          //
          f_b.resize( _nYi * _nYi );
          f_b[0] = 0.0;
          f_b[1] = 0.0;
          f_b[2] = 1.0;
          f_b[3] = 1.0;
          return true;
        }
        void doDataVariance( valarray<double>& R ) const
        {
          //
          // R(alp, b) = [ b(1)  0.0  ]
          //             [ 0.0   b(1) ]
          //
          R.resize( _nYi * _nYi );
          dmatR.fill(0);
          R[0] = _b[0];
          R[1] =  0.0;
          R[2] =  0.0;
          R[3] = _b[0];
        }
        bool doDataVariance_popPar( valarray<double>& R_alp ) const
        {
          //
          // R(alp, b)_alp = [ 0  0 ]
          //                 [ 0  0 ]
          //                 [ 0  0 ]
          //                 [ 0  0 ]
          //
          R_alp.resize( _nYi * _nYi * _nA );
          R_alp = 0.0;
          return false;
        }
        bool doDataVariance_indPar( valarray<double>& R_b ) const
        {
          //
          // R(alp, b)_alp = [ 1  0 ]
          //                 [ 0  0 ]
          //                 [ 0  0 ]
          //                 [ 1  0 ]
          //
          R_b.resize( _nYi * _nYi * _nB );
          R_b  = 0.0;
          R[0] = 1.0;
          R[3] = 1.0;
          return true;
        }   
    };    

    void main()
    {
        using namespace std;
        using namespace population_analysis;

        int n = 2;
        int m = n;
        UserModel model;

        //
        // y =   [ 1 ]
        //       [ 1 ]
        //
        DoubleMatrix y( nYi, 1 );
        y.data()[0] = 1.0;
        y.data()[1] = 1.0;

        //
        // alp = [ 1 ]
        //       [ 1 ]
        //
        DoubleMatrix alp( nA, 1 );
        alp.data()[0] = 1.0;
        alp.data()[1] = 1.0;

        //
        // b =   [ 1 ]
        //       [ 1 ]
        //
        DoubleMatrix b( nB, 1 );
        b.data()[0] = 1.0;
        b.data()[1] = 1.0;

        DoubleMatrix alpStep(n,1);
        DoubleMatrix bStep(n,1);
        DoubleMatrix temp;
        DoubleMatrix dRowVecLambda_xOut;
        DoubleMatrix dmatLambda_x_alpOut;
        DoubleMatrix dmatLambda_x_bOut;

        double dtemp = -1.0;
        double *pdAlpStep = alpStep.data();
        double *pdBStep   = bStep.data();
        double *palp      = alp.data();
        double *pb        = b.data();

        int i;

        // Setting the proper step values for alpha
        for( i=n-1; i>=0; i-- ){
            dtemp = pow( DBL_EPSILON / (2.0/6.0), 1/3.0) + palp[i];
            pdAlpStep[i] = 10.0 * ( dtemp - palp[i] );
        }

        // Setting the proper step values for b
        for( i=n-1; i>=0; i-- ){
            dtemp = pow( DBL_EPSILON / (2.0/6.0), 1/3.0) + pb[i];
            pdBStep[i] = 10.0 * ( dtemp - pb[i] );
        }

        //-------------------------- 1st derivative with respect to alpha -----------------------//
        // Testing with respect to alpha with D term
        cout << "<< Testing lambdaDiff with D term >>" << endl;

        try{
            lambdaDiff(model, y, alp, alpStep, b, bStep, WITH_RESPECT_TO_ALP, 
                &dRowVecLambda_xOut, &dmatLambda_x_alpOut, &dmatLambda_x_bOut );
        }
        catch(...)
        {
            cerr << "lambdaDiff failed" << endl;
            abort();
        }
        cout << "(1) The central difference of lambda with respect to alpha =" << endl;
        dRowVecLambda_xOut.print();
        cout << endl;

        cout << "(2) The derivative of the result from (1) with respect to alpha = " << endl;
        dmatLambda_x_alpOut.print();
        cout << endl;

        cout << "(3) The derivative of the result from (1) with respect to b = " << endl;
        dmatLambda_x_bOut.print();
        cout << endl;
    }

    //
    // Return a m by n matrix filled with zeros
    //
    static DoubleMatrix getZeroMatrix( const int m, const int n ){
        DoubleMatrix zero(m, n);
        zero.fill(0.0);
        return zero;
    }

    //
    // y = [1]
    //     [1]
    //
    static DoubleMatrix getY(){
        DoubleMatrix dvecY(2,1);
        dvecY.fill(1.0);
        return dvecY;
    }

    //
    // alpha = [1]
    //         [1]
    ///
    static DoubleMatrix getAlp(){
        DoubleMatrix alp(2,1);
        alp.fill(1.0);
        return alp;
    }

    //
    // b = [1]
    //     [1]
    //
    static DoubleMatrix getB(){
        DoubleMatrix b(2,1);
        b.fill(1.0);
        return b;
    }

    //
    // D = [alp(0)   0   ]
    //     [   0   alp(0)]
    //
    static DoubleMatrix funD( const DoubleMatrix &alp ){
        DoubleMatrix dmatD(2,2);
        double *pD = dmatD.data(),
               *palp = alp.data();

        pD[0] = palp[0];
        pD[1] = 0.0;
        pD[2] = 0.0;
        pD[3] = palp[0];
        return dmatD;
    }

    //
    // D(alp)_alp = [ 1  0 ]
    //              [ 0  0 ]
    //              [ 0  0 ]
    //              [ 1  0 ]
    //
    static DoubleMatrix funD_alp( const DoubleMatrix &dmatD, const DoubleMatrix &alp ){
        DoubleMatrix dmatD_alp( dmatD.nr()*dmatD.nc(), alp.nr() );
        double *pD_alp = dmatD_alp.data();
        dmatD_alp.fill(0.0);

        pD_alp[0] = 1.0;
        pD_alp[3] = 1.0;
        return dmatD_alp;
    }

    //
    // f(alp, b) = [ alp(1) + b(1) ]
    //             [ alp(1) + b(1) ]
    //
    static DoubleMatrix funF( const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatF(2,1);
        double *palp = alp.data(),
               *pb     = b.data();

        dmatF.fill( palp[1] + pb[1] );
        return dmatF;
    }

    //
    // f(alp, b)_alp = [ 0  1 ]
    //                 [ 0  1 ]
    //
    static DoubleMatrix funF_alp( const DoubleMatrix &dvecF, 
                    const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatF_alp(dvecF.nr(), alp.nr());
        double *pf_alp = dmatF_alp.data();

        pf_alp[0] = 0.0;
        pf_alp[1] = 0.0;
        pf_alp[2] = 1.0;
        pf_alp[3] = 1.0;
        return dmatF_alp;
    }

    //
    // f(alp, b)_b = [ 0  1 ]
    //               [ 0  1 ]
    //
    static DoubleMatrix funF_b( const DoubleMatrix &dvecF, 
                    const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatF_b(dvecF.nr(), b.nr());
        double *pf_b = dmatF_b.data();

        pf_b[0] = 0.0;
        pf_b[1] = 0.0;
        pf_b[2] = 1.0;
        pf_b[3] = 1.0;
        return dmatF_b;
    }

    //
    // R(alpha, b) = [ b(1)  0   ]
    //               [  0   b(1) ]
    //
    static DoubleMatrix funR(  const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatR(2,2);
        double *pR = dmatR.data(),
               *pb = b.data();

        pR[0] = pb[0];
        pR[1] = 0.0;
        pR[2] = 0.0;
        pR[3] = pb[0];
        return dmatR;
    }

    //
    // R_alp = [ 0  0 ]
    //         [ 0  0 ]
    //         [ 0  0 ]
    //         [ 0  0 ]
    //
    static DoubleMatrix funR_alp( const DoubleMatrix &dmatR, 
                    const DoubleMatrix &alp, const DoubleMatrix &b ){
        return getZeroMatrix(4,2);
    }

    //
    // R_b   = [ 1  0 ]
    //         [ 0  0 ]
    //         [ 0  0 ]
    //         [ 1  0 ]
    //
    static DoubleMatrix funR_b( const DoubleMatrix &dmatR, 
                    const DoubleMatrix &alp, const DoubleMatrix &b ){
        DoubleMatrix dmatR_b(dmatR.nr()*dmatR.nc(), b.nr());
        double *pR_b = dmatR_b.data();
        dmatR_b.fill(0.0);
        pR_b[0] = 1.0;
        pR_b[3] = 1.0;
        return dmatR_b;
    }
$$

the following results will be displayed.
$codep
    << Testing lambdaDiff with D term >>
    (1) The central difference of lambda with respect to alpha =
    [-5.07982e-009, 2]

    (2) The derivative of the result from (1) with respect to alpha =
    [1, 0]
    [0, 2]

    (3) The derivative of the result from (1) with respect to b =
    [-1, -1]
    [-2, 2]

$$
$end
*/


/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * Be aware that the output values, 2nd derivatives, are true derivatives of 
 * central difference approximations of Lambda function, as opposed to central 
 * differences approximations of true derivatives of Lambda function.
 * The approximation and true derivative are related to each other by tranposing
 * each other.
 *    approx{ partial(g/x)/x } = transpose{ partial[approx(g/x)/x] }
 *
 *------------------------------------------------------------------------*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <cmath>
#include "SpkModel.h"
#include "lambda.h"
#include "lambdaDiff.h"
#include "matmax.h"
#include "matmin.h"
#include "namespace_population_analysis.h"
#include "centdiff.h"
#include "transposeRowBlocks.h"
#include "DoubleMatrix.h"
#include "SpkException.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
void lambdaDiff(
                SpkModel &model,
                const DoubleMatrix &dvecY,
                const DoubleMatrix &dvecAlp,
                const DoubleMatrix &dvecAlpStep,
                const DoubleMatrix &dvecB,
                const DoubleMatrix &dvecBStep,
                int   withRespectToWhichVar,
                DoubleMatrix *lambda_xOut,
                DoubleMatrix *lambda_x_alpOut,
                DoubleMatrix *lambda_x_bOut,
                const bool withD
                )
{
    using namespace std;
    using namespace population_analysis;

    int nA = dvecAlp.nr();
    int nB = dvecB.nr();
    int nY = dvecY.nr();
    int nX;

    assert(dvecAlpStep.nr() == nA);
    assert(dvecBStep.nr()   == nB);

    if( withRespectToWhichVar == WITH_RESPECT_TO_ALP )
    {
        nX = nA;
    }
    else if( withRespectToWhichVar == WITH_RESPECT_TO_B )
    {
        nX = nB;
    }
    else
    {
        assert(false);
    }
    if( lambda_xOut )
    {
        assert(lambda_xOut->nr() == 1);
        assert(lambda_xOut->nc() == nX);
    }
    if( lambda_x_alpOut )
    {
        assert(lambda_x_alpOut->nr() == nX);
        assert(lambda_x_alpOut->nc() == nA);
    }
    if( lambda_x_bOut )
    {
        assert(lambda_x_bOut->nr() == nX);
        assert(lambda_x_bOut->nc() == nB);
    }
    DoubleMatrix dmatLambda_alp_xOut, dmatLambda_b_xOut;

    Lambda lambdaOb(&model, dvecY, withD);          // this evaluates to lambda(alp,b) at the given points
    
    Lambda_alp lambda_alpOb(&model, dvecY, withD);  // this evaluates to the true derivative of lambda wrt alp

    Lambda_b lambda_bOb(&model, dvecY, withD);      // this evalutes to the true derivative of lambda wrt b

    DoubleMatrix empty;      // empty matrix to fool centdiff
 
    if( withRespectToWhichVar == WITH_RESPECT_TO_ALP )
    {
        // approximation for lambda_alp
        if( lambda_xOut )
        {
            *lambda_xOut
            = centdiff< binder2nd<Lambda> >(bind2nd(lambdaOb, dvecB), dvecAlp, dvecAlpStep);
        }
        if( lambda_x_alpOut )
        {
            //central difference approximation of (the true derivative lambda_alp) with respect to x=alp
            dmatLambda_alp_xOut 
            = centdiff< binder2nd<Lambda_alp> >(bind2nd(lambda_alpOb, dvecB), dvecAlp, dvecAlpStep);

            //true drivative of (the approximation for lambda_x=alp) with respect to alp
            // Note that this is different from what trancendiff() does. 
            *lambda_x_alpOut = transposeRowBlocks(dmatLambda_alp_xOut,nA);
        }
        if( lambda_x_bOut )
        {
            //central difference approximation of (the true derivative lambda_b) with respect to x=alp
            dmatLambda_b_xOut 
            = centdiff< binder2nd<Lambda_b> >(bind2nd(lambda_bOb,dvecB), dvecAlp, dvecAlpStep);

            //true drivative of (the approximation for lambda_x=alp) with respect to b
            // Note that this is different from what trancendiff() does. 
            *lambda_x_bOut   = transposeRowBlocks(dmatLambda_b_xOut,nB);
        }
    }
    // approximation for lambda_b
    if( withRespectToWhichVar == WITH_RESPECT_TO_B )
    {
        // approximation for lambda_b
        if( lambda_xOut )
        {
            *lambda_xOut 
            = centdiff< binder1st<Lambda> >(bind1st(lambdaOb,dvecAlp), dvecB, dvecBStep);
        }
        if( lambda_x_alpOut )
        {
            //central difference approximation of (the true derivative lambda_alp) with respect to x=b
            dmatLambda_alp_xOut 
            = centdiff< binder1st<Lambda_alp> >(bind1st(lambda_alpOb, dvecAlp), dvecB, dvecBStep);

            //true drivative of (the approximation for lambda_x=b) with respect to alp
            // Note that this is different from what trancendiff() does. 
            *lambda_x_alpOut = transposeRowBlocks(dmatLambda_alp_xOut, nA);
        }
        if( lambda_x_bOut )
        {
            //central difference approximation of (the true derivative lambda_b) with respect to x=b
            dmatLambda_b_xOut 
            = centdiff< binder1st<Lambda_b> >(bind1st(lambda_bOb, dvecAlp), dvecB, dvecBStep);

            //true drivative of (the approximation for lambda_x=b) with respect to b
            // Note that this is different from what trancendiff() does. 
            *lambda_x_bOut   = transposeRowBlocks(dmatLambda_b_xOut, nB);
        }

    }
    return;
}

