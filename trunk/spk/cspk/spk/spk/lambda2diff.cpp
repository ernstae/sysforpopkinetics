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
 * File: lambda2diff.cpp
 *
 *
 * Tranposed central difference approximations (with respect to alp or b)
 * of the 1st and 2nd derivatives of lambda with respect to b
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: lambda2diff
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin lambda2diff$$
$escape #$$

$spell
	Model model
    diff const bool bstep pd dmat fab rab da dvec ra rb fb cout endl
    namespace std iostream int ind inv Fab Rab Da distrib
    int pf pb palp covariances Spk cerr
    covariance
    Ri
    valarray
    Yi
    resize
$$

$section Second Order Central Difference of Lambda$$

$index  lambda2diff$$
$index individual, lambda$$
$cindex second #order central difference #of Lambda$$

$table
$bold Prototype:$$   $cend
$syntax/void lambda2diff(
        SpkModel<double>   & /model/,
        const DoubleMatrix & /y/,
        const DoubleMatrix & /alp/,
        const DoubleMatrix & /b/,
        const DoubleMatrix & /step/,
        DoubleMatrix * /Lambda_b_bOut/,
        DoubleMatrix * /Lambda_b_b_alpOut/,
        DoubleMatrix * /Lambda_b_b_bOut/,
        bool  /withD/ = true
     )/$$
$tend

$fend 35$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Evaluates the second order central difference for the derivatives of
$math%Lambda(alp, b)%$$ with respect to $math%b%$$ where
$math%
                 1 %          %                 1               T  -1
Lambda(alp, b) = - #logdet[ 2 #pi R(alp, b) ] + - [y - f(alp, b)] R (alp, b) [y - f(alp, b)]
                 2 %          %                 2

                 1 %          %                 1  T  -1
               + - #logdet[ 2 #pi D(alp) ]    + - b  D (alp)  b
                 2 %          %                 2
%$$
In addition, the derivatives of the central difference approximation
are also evaluated.
(The equation above uses
$xref/glossary/Individual Notation/individual notation/$$.)

$head Reference$$
$italic 
Approximating The maximum Likelihood Estimate For Models With Random Parameter
$$

$head Return Value$$
Upon a successful completion, the function returns normally and
set the given output value place holders to the result values (ones that are requested).
If a failure occurs during the evaluation, a SpkException object is
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
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
A m dimensional column vector that holds an individual's data.

$syntax/

/alp/
/$$
A n dimensional column vector that holds the fixed population parameter, alpha.

$syntax/

/b/
/$$
A n dimensional column vector that holds the random population parameter, b.

$syntax/

/step/
/$$
A n dimensional column vector that holds the step size for b in central difference approximation.
$syntax/

* /Lambda_b_bOut/
/$$
The pointer to a matrix that will contain the central difference approximation for lambda_b_b(alp, b) if
memory is allocated upon a request.  If 0 were specified, no evaluation will be performed 
and the value remains as null.

$syntax/

* /Lambda_b_b_alpOut/
/$$
The pointer to a matrix that will contain the true derivative with respect to $italic alpha$$ of
the double central difference approximation for lambda_b_b(alp,b) if
memory is allocated at request.  If 0 were specified, no evaluation will be performed 
and the value remains as null.

$syntax/

* /Lambda_b_b_bOut/
/$$
The pointer to a matrix that will contain the true derivative with respect to $italic b$$ of
the double central difference approximation for lambda_b_b(alp,b) if
memory is allocated at request.  If 0 were specified, no evaluation will be performed and
the value remains as null.

$syntax/

/withD/ (optional)
/$$
The flag that shall indicate as to whether include the whole D term in the evaluation.
The default is true.

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include "DoubleMatrix.h"
    #include "lambda2diff.h"
    #include "SpkModel.h"
    #include "SpkValarray.h"

    class UserModel : public SpkModel<double>
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
    
    //-------------------------------------------------------------------------
    //  Main
    //-------------------------------------------------------------------------

    // Set parameters dimensions to 2
    static const int n   = 2;
    static const int nA  = n;
    static const int nB  = n;
    static const int nYi = n;


    void main()
    {    
        using namespace std;

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
        
        //
        // bstep = [ 0.1 ]
        //         [ 0.1 ]
        //
        DoubleMatrix bStep( nB, 1 );
        bStep.data()[0] = 0.1;
        bStep.data()[1] = 0.1;


        UserModel model;

        // 
        // allocate memory for evaluation request
        //
        DoubleMatrix dmatLambda_b_bOut(nB, nB);
        DoubleMatrix dmatLambda_b_b_alpOut(nB*nB, nA);
        DoubleMatrix dmatLambda_b_b_bOut(nB*nB, nB);

        try{
            lambda2diff(model, y, alp, b, bStep, 
                &dmatLambda_b_bOut, &dmatLambda_b_b_alpOut, &dmatLambda_b_b_bOut, true );
        }
        catch( ... )
        {
            cerr << "lambda2diff failed" << endl;
            abort();
        }
        cout << "lambda_b_b = " << endl;
        dmatLambda_b_bOut.print();
        cout << endl;

        cout << "lambda_b_b_alp = " << endl;
        dmatLambda_b_b_alpOut.print();
        cout << endl;

        cout << "lambda_b_b_b = " << endl;
        dmatLambda_b_b_bOut.print();
        cout << endl;

        return;
    }


$$
then it will display the following when it is run:
$codep

    lambda_b_b =
    [ 2.06278 -2.0202 ]
    [ -2.0202 3 ]

    lambda_b_b_alp =
    [ -1 4.16667 ]
    [ 5.55112e-015 -2.0202 ]
    [ 5.55112e-015 -2.0202 ]
    [ -1 1.11022e-014 ]

    lambda_b_b_b =
    [ -4.34028 4.16667 ]
    [ 4.08122 -2.0202 ]
    [ 4.08122 -2.0202 ]
    [ -2 2.22045e-014 ]

$$
$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include <cassert>
#include "SpkModel.h"
#include "lambda.h"
#include "lambda2diff.h"
#include "matmin.h"
#include "DoubleMatrix.h"
#include "SpkException.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void lambda2diff(
        SpkModel<double>   &model,
        const DoubleMatrix &dvecY,
        const DoubleMatrix &dvecAlp,
        const DoubleMatrix &dvecB,
        const DoubleMatrix &dvecStep,
        DoubleMatrix *dmatLambda_b_bOut,
        DoubleMatrix *dmatLambda_b_b_alpOut,
        DoubleMatrix *dmatLambda_b_b_bOut,
        bool  withD
     )
{
    //------------------------------------------------------------
    // Preliminaries.
    //------------------------------------------------------------

    if( !( dmatLambda_b_bOut || dmatLambda_b_b_alpOut || dmatLambda_b_b_bOut ) )
    {
        return;
    }

    int nAlp = dvecAlp.nr();
    int nB   = dvecB  .nr();

   
    //------------------------------------------------------------
    // Validate the inputs (debug version only).
    //------------------------------------------------------------

    // y, alp, b, b's step must all be row vectors.
    assert( dvecY.nc()     == 1 );
    assert( dvecAlp.nc()   == 1 );
    assert( dvecB.nc()     == 1 );
    assert( dvecStep.nc()  == 1 );

    // These must have the same dimension.
    assert( dvecStep.nr() == nB );

    // These must all be positive.
    assert( matmin( dvecStep) > 0.0 );


    //------------------------------------------------------------
    // Prepare to compute the derivatives.
    //------------------------------------------------------------

    // These are values where lamba, lambda_alp, and lambda_b will be evaluated.
    DoubleMatrix dvecBUU = dvecB;
    DoubleMatrix dvecBUL = dvecB;
    DoubleMatrix dvecBLU = dvecB;
    DoubleMatrix dvecBLL = dvecB;

    double* bUU = dvecBUU.data();
    double* bUL = dvecBUL.data();
    double* bLU = dvecBLU.data();
    double* bLL = dvecBLL.data();

    const double* b    = dvecB  . data();
    const double* step = dvecStep.data();
    int nRows = nB * nB;
    int j;
    int k;
    int m;
	double* lambda_b_bOut;
	double* lambda_b_b_alpOut;
	double* lambda_b_b_bOut;

    //------------------------------------------------------------
    // Compute the double central difference approximation for lambda_b_b.
    //------------------------------------------------------------

    // [Revisit - General Version of Double Central Difference - Mitch]
    // If a general version of the double central difference approximation 
    // function is ever written, then this block of code should be replaced 
    // by it.

    // Create a function object to evaluate lambda(alp, b).
    Lambda lambda( &model, dvecY, withD );

	double lambdaUU;
	double lambdaLL;
	double lambdaUL;
	double lambdaLU;
    double lambdaAtB;

    if( dmatLambda_b_bOut != 0 )
	{
        assert( dmatLambda_b_bOut->nr() == nB && dmatLambda_b_bOut->nc() == nB );
        lambda_b_bOut = dmatLambda_b_bOut->data();

		lambdaAtB = lambda( dvecAlp, dvecB );
	}

    //------------------------------------------------------------
    // Compute the true derivative with respect to alp of the 
    // double central difference approximation for lambda_b_b.
    //------------------------------------------------------------

    // [Revisit - General Version of Double Central Difference - Mitch]
    // The block of code that calculates lambda_b_b_b is just a copy
    // of the the code that calculates lambda_b_b_a.  If a general version
    // of the double central difference approximation function is ever
    // written, then these two blocks of code should be replaced by it.

    //******************************************************************
    // From equation (11) in AMC6583 (Approximating the marginal 
    // likelihood estimate for models with random parameters, Bell, 
    // 2001), the true derivative with respect to alp of the 
    // double central difference approximation for lambda_b_b is
    //
    //           ~  ~                     ~ T ~ T
    //      d    d  d  lambda(alp, b)  =  d   d   d    lambda(alp, b)  .
    //       alp  b  b                     b   b   alp
    //
    // Because lambda(alp, b) is a scalar function,
    //
    //                                         -                               -   
    //                                        |   -                        -    |
    //           ~  ~                     ~ T |  |  ~                       |T  |
    //      d    d  d  lambda(alp, b)  =  d   |  |  d  d    lambda(alp, b)  |   |  .
    //       alp  b  b                     b  |  |   b  alp                 |   |
    //                                        |   -                        -    |
    //                                         -                               -   
    //
    //******************************************************************

    DoubleMatrix drowLambda_alpUU;
    DoubleMatrix drowLambda_alpUL;
    DoubleMatrix drowLambda_alpLU;
    DoubleMatrix drowLambda_alpLL;
	DoubleMatrix drowLambda_alpAtB;

    double* lambda_alpUU;
    double* lambda_alpUL;
    double* lambda_alpLU;
    double* lambda_alpLL;
	double* lambda_alpAtB;

    // Create a function object to evaluate lambda_alp(alp, b).
    Lambda_alp lambda_alp( &model, dvecY, withD );

    if( dmatLambda_b_b_alpOut != 0 )
	{
        assert(dmatLambda_b_b_alpOut->nr() == nRows && dmatLambda_b_b_alpOut->nc() == nAlp);
        lambda_b_b_alpOut = dmatLambda_b_b_alpOut->data();
	
        drowLambda_alpUU.resize( 1, nAlp );
        drowLambda_alpUL.resize( 1, nAlp );
        drowLambda_alpLU.resize( 1, nAlp );
        drowLambda_alpLL.resize( 1, nAlp );

		drowLambda_alpAtB = lambda_alp( dvecAlp, dvecB );
        lambda_alpAtB = drowLambda_alpAtB.data();
	}

    //------------------------------------------------------------
    // Compute the true derivative with respect to b of the 
    // double central difference approximation for lambda_b_b.
    //------------------------------------------------------------

    // [Revisit - General Version of Double Central Difference - Mitch]
    // The block of code that calculates lambda_b_b_b is just a copy
    // of the the code that calculates lambda_b_b_a.  If a general version
    // of the double central difference approximation function is ever
    // written, then these two blocks of code should be replaced by it.

    //******************************************************************
    // From equation (11) in AMC6583 (Approximating the marginal 
    // likelihood estimate for models with random parameters, Bell, 
    // 2001), the true derivative with respect to b of the 
    // double central difference approximation for lambda_b_b is
    //
    //           ~  ~                     ~ T ~ T
    //      d    d  d  lambda(alp, b)  =  d   d   d    lambda(alp, b)  .
    //       b    b  b                     b   b   b
    //
    // Because lambda(alp, b) is a scalar function,
    //
    //                                         -                               -   
    //                                        |   -                        -    |
    //           ~  ~                     ~ T |  |  ~                       |T  |
    //      d    d  d  lambda(alp, b)  =  d   |  |  d  d    lambda(alp, b)  |   |  .
    //       b    b  b                     b  |  |   b  b                   |   |
    //                                        |   -                        -    |
    //                                         -                               -   
    //
    //******************************************************************

    DoubleMatrix drowLambda_bUU;
    DoubleMatrix drowLambda_bUL;
    DoubleMatrix drowLambda_bLU;
    DoubleMatrix drowLambda_bLL;
	DoubleMatrix drowLambda_bAtB;

    double* lambda_bUU;
    double* lambda_bUL;
    double* lambda_bLU;
    double* lambda_bLL;
	double* lambda_bAtB;

    // Create a function object to evaluate lambda_b(alp, b).
    Lambda_b lambda_b( &model, dvecY, withD );

    if( dmatLambda_b_b_bOut != 0 )
	{
        assert(dmatLambda_b_b_bOut->nr() == nRows && dmatLambda_b_b_bOut->nc() == nB);
        lambda_b_b_bOut = dmatLambda_b_b_bOut->data();
	
        drowLambda_bUU.resize( 1, nB );
        drowLambda_bUL.resize( 1, nB );
        drowLambda_bLU.resize( 1, nB );
        drowLambda_bLL.resize( 1, nB );

		drowLambda_bAtB = lambda_b( dvecAlp, dvecB );
        lambda_bAtB = drowLambda_bAtB.data();
	}

    // Set the diagonal elements.
    for ( j = 0; j < nB; j++ )
    {
        bUU[j] = b[j] + 2.0 * step[j];

        if( dmatLambda_b_b_alpOut != 0 )
        {
            drowLambda_alpUU = lambda_alp( dvecAlp, dvecBUU );
            lambda_alpUU = drowLambda_alpUU.data();
        }
		if( dmatLambda_b_b_bOut != 0 )      
        {
            drowLambda_bUU = lambda_b( dvecAlp, dvecBUU );
            lambda_bUU = drowLambda_bUU.data();
        }
		if( dmatLambda_b_bOut != 0 )
		{
            lambdaUU = lambda( dvecAlp, dvecBUU );
		}

        bLL[j] = b[j] - 2.0 * step[j];

        if( dmatLambda_b_b_alpOut != 0 )
        {
            drowLambda_alpLL = lambda_alp( dvecAlp, dvecBLL );
            lambda_alpLL = drowLambda_alpLL.data();
        }		
		if( dmatLambda_b_b_bOut != 0 )      
        {
            drowLambda_bLL = lambda_b( dvecAlp, dvecBLL );
            lambda_bLL = drowLambda_bLL.data();
        }
        if( dmatLambda_b_bOut != 0 )
		{
			lambdaLL = lambda( dvecAlp, dvecBLL );
		}

        if( dmatLambda_b_bOut != 0 )
		{
            lambda_b_bOut[ j + j * nB ] = 
                ( lambdaUU 
                - lambdaAtB 
                - lambdaAtB 
                + lambdaLL ) / ( 4.0 * step[j] * step[j] );
		}

		//--------------------------------------------------------
        // Set the elements that correspond to the derivatives 
        // with respect to the same component of b, i.e., the
        // elements that correspond to
        //
        //      ~ (q)  ~ (q)
        //      d      d      d    lambda(alp, b)  ,
        //       b      b      alp
        //
        // where q denotes one of the components of b.
        //--------------------------------------------------------

        if( dmatLambda_b_b_alpOut != 0 )
		{
            for ( m = 0; m < nAlp; m++ )
            {
                lambda_b_b_alpOut[ ( j + j * nB ) + m * nRows ] = 
                    ( lambda_alpUU [m] 
                    - lambda_alpAtB[m]
                    - lambda_alpAtB[m]
                    + lambda_alpLL [m] ) / ( 4.0 * step[j] * step[j] );
            }
        }

		//--------------------------------------------------------
        // Set the elements that correspond to the derivatives 
        // with respect to the same component of b, i.e., the
        // elements that correspond to
        //
        //      ~ (q)  ~ (q)
        //      d      d      d    lambda(alp, b)  ,
        //       b      b      b
        //
        // where q denotes one of the components of b.
        //--------------------------------------------------------

		if( dmatLambda_b_b_bOut != 0 )      
		{
            for ( m = 0; m < nB; m++ )
            {
                lambda_b_b_bOut[ ( j + j * nB ) + m * nRows ] = 
                    ( lambda_bUU [m] 
                    - lambda_bAtB[m]
                    - lambda_bAtB[m]
                    + lambda_bLL [m] ) / ( 4.0 * step[j] * step[j] );
            }
        }

		bUU[j] = b[j];
        bLL[j] = b[j];
    }

    // Set the off-diagonal elements.
    for ( j = 1; j < nB; j++ )
    {
		bUU[j] = b[j] + step[j];
        bUL[j] = b[j] + step[j];
        bLU[j] = b[j] - step[j];
        bLL[j] = b[j] - step[j];

        for ( k = 0; k < j; k++ )
        {
            bUU[k] = b[k] + step[k];
        
            if( dmatLambda_b_b_alpOut != 0 )
            {
                drowLambda_alpUU = lambda_alp( dvecAlp, dvecBUU );
                lambda_alpUU = drowLambda_alpUU.data();
            }
			if( dmatLambda_b_b_bOut != 0 )
			{
                drowLambda_bUU = lambda_b( dvecAlp, dvecBUU );
                lambda_bUU = drowLambda_bUU.data();
            }
			if( dmatLambda_b_bOut != 0 )
			{
				lambdaUU = lambda( dvecAlp, dvecBUU );				
            }

            bUL[k] = b[k] - step[k];

            if( dmatLambda_b_b_alpOut != 0 )
            {
                drowLambda_alpUL = lambda_alp( dvecAlp, dvecBUL );
                lambda_alpUL = drowLambda_alpUL.data();
            }
			if( dmatLambda_b_b_bOut != 0 )
			{
                drowLambda_bUL = lambda_b( dvecAlp, dvecBUL );
                lambda_bUL = drowLambda_bUL.data();
			}
			if( dmatLambda_b_bOut != 0 )
			{
				lambdaUL = lambda( dvecAlp, dvecBUL );
            }

            bLU[k] = b[k] + step[k];
        
            if( dmatLambda_b_b_alpOut != 0 )
            {
                drowLambda_alpLU = lambda_alp( dvecAlp, dvecBLU );
                lambda_alpLU = drowLambda_alpLU.data();
            }
			if( dmatLambda_b_b_bOut != 0 )
			{
                drowLambda_bLU = lambda_b( dvecAlp, dvecBLU );
                lambda_bLU = drowLambda_bLU.data();
            }
			if( dmatLambda_b_bOut != 0 )
			{
				lambdaLU = lambda( dvecAlp, dvecBLU );
            }

            bLL[k] = b[k] - step[k];
        
            if( dmatLambda_b_b_alpOut != 0 )
            {
                drowLambda_alpLL = lambda_alp( dvecAlp, dvecBLL );
                lambda_alpLL = drowLambda_alpLL.data();
            }        
			if( dmatLambda_b_b_bOut != 0 )
			{
                drowLambda_bLL = lambda_b( dvecAlp, dvecBLL );
                lambda_bLL = drowLambda_bLL.data();
            }
			if( dmatLambda_b_bOut != 0 )
			{
				lambdaLL = lambda( dvecAlp, dvecBLL );
            }

			if( dmatLambda_b_bOut != 0 )
			{
                lambda_b_bOut[ j + k * nB ] = 
                    ( lambdaUU 
                    - lambdaUL  
                    - lambdaLU  
                    + lambdaLL ) / ( 4.0 * step[j] * step[k] );

                lambda_b_bOut[ k + j * nB ] = lambda_b_bOut[ j + k * nB ];
            }

			//--------------------------------------------------------
			// Set the elements that correspond to the derivatives 
			// with respect to the different components of b, i.e., 
			// the elements that correspond to
			//
			//      ~ (q)  ~ (s)
			//      d      d      d    lambda(alp, b)  ,
			//       b      b      alp
			//
			// where q and s denote different components of b.
			//--------------------------------------------------------

            if( dmatLambda_b_b_alpOut != 0 )
            {
                for ( m = 0; m < nAlp; m++ )
                {
                    lambda_b_b_alpOut[ ( j + k * nB ) + m * nRows ] = 
                        ( lambda_alpUU[m] 
                        - lambda_alpUL[m] 
                        - lambda_alpLU[m] 
                        + lambda_alpLL[m] ) / ( 4.0 * step[j] * step[k] );

                    lambda_b_b_alpOut[ ( k + j * nB ) + m * nRows ] = 
                        lambda_b_b_alpOut[ ( j + k * nB ) + m * nRows ];
                }
            }

			//--------------------------------------------------------
			// Set the elements that correspond to the derivatives 
			// with respect to the different components of b, i.e., 
			// the elements that correspond to
			//
			//      ~ (q)  ~ (s)
			//      d      d      d    lambda(alp, b)  ,
			//       b      b      b
			//
			// where q and s denote different components of b.
			//--------------------------------------------------------

			if( dmatLambda_b_b_bOut != 0 )
			{
                for ( m = 0; m < nB; m++ )
                {
                    lambda_b_b_bOut[ ( j + k * nB ) + m * nRows ] = 
                        ( lambda_bUU[m] 
                        - lambda_bUL[m] 
                        - lambda_bLU[m] 
                        + lambda_bLL[m] ) / ( 4.0 * step[j] * step[k] );

                    lambda_b_b_bOut[ ( k + j * nB ) + m * nRows ] = 
                        lambda_b_b_bOut[ ( j + k * nB ) + m * nRows ];
                }
            }

			bUU[k] = b[k];
            bUL[k] = b[k];
            bLU[k] = b[k];
            bLL[k] = b[k];
        }

		bUU[j] = b[j];
        bUL[j] = b[j];
        bLU[j] = b[j];
        bLL[j] = b[j];
    }

    return;
}   
 


 

                
            


