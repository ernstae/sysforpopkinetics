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
 * File: estimateB.cpp
 *
 *
 * Computes indHat, a prediction of ind such that the derivative of lambda(pop,indHat)
 * with respect to ind is minimal (nearly 0), and indTilde, another prediction of ind 
 * such that the central difference approximation of lambda(pop,indTilde) 
 * with respect to ind is minimal (nearly 0) and its derivative with respect to pop.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: estimateB
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin estimateB$$
$escape #$$

$spell
	Model model
    bool int iostream endl cout namespace std dmat dvec abs const
    mitr obj cstdlib pre ind Ny Nb Na
    inv pop fab rab da distrib blsq cerr
    int i pf pb ind
    stdout
    Gz
    Varbl
    rd
    covariances
    Spk
    covariance
    inv
    optimizer
    valarray
    resize
    Model model
    Fo
	Optimizer optimizer
$$

$section Solving Derivative and Difference Condition For Individual Parameters$$

$cindex solving derivative #and difference #condition #for random #population #parameters$$
$index EstimateB$$
$index individual, estimate bHat$$

$table
$bold Prototype:$$ $cend
$syntax/void estimateB(
    SpkModel<double> & /model/,
    bool   /isBlsq/,
    Optimizer& /optimizer/,
    const  DoubleMatrix & /y/,
    const  DoubleMatrix & /alp/,
    const  DoubleMatrix & /BIn/,
    const  DoubleMatrix & /bLow/,
    const  DoubleMatrix & /bUp/,
    const  DoubleMatrix & /bStep/,
    DoubleMatrix * /bHatOut/,
    DoubleMatrix * /bTildeOut/,
    DoubleMatrix * /bTilde_alpOut/,
    bool           /includeD/ = true )/$$

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
Computes $italic bHat$$, a prediction of $math%b%$$
such that the derivative of $math%lambda(alp, bHat)%$$
with respect to $math%b%$$ is minimal (nearly 0), and $math%bTilde%$$, 
another prediction of $math%b%$$ 
such that the central difference approximation of $math%lambda(alp,bTilde)%$$
with respect to $math%b%$$ is minimal (nearly 0) and 
its derivative with respect to $math%alp%$$.
$pre

$$
Solves the first order necessary conditions for an optimal value
for the problem
$math%
#minimizes Lambda(alp, b) #with #respect #to b
#subject #to indLow #le ind #le bUp
#where
                 1 %          %                 1                        T  -1
Lambda(alp, b) = - #logdet[ 2 #pi R(alp, b) ] + - [dataForAll - f(alp, b)] R (alp, b) [dataForAll - f(alp, b)]
                 2 %          %                 2

                   1 %          %                 1    T  -1
                 + - #logdet[ 2 #pi D(alp) ]    + - ind  D (alp) b
                   2 %          %                 2
%$$
The equation above uses
$xref/glossary/Individual Notation/individual notation/$$.

$head Reference$$
$italic
Approximating The Maximum Likelihood Estimate For Models With Random Parameter
$$

$head Pre-conditions$$
Given that $math%Na%$$ = |alp|, $math%Nb%$$ = |ind| and $math%n%$$ = (the size of a population), 
$italic dataForAll$$ is a $math%Nb * n%$$ dimensional vector, 
$italic alp$$ is a $math%Na%$$ dimensional vector, and $italic bIn$$, 
$italic bLow$$, $italic bUp$$ and $italic bStep$$ are all
be $math%Nb%$$ dimensional column vectors.
$pre

$$
$italic epsilon$$ > 0.0.
$pre

$$
$italic mitr$$ >= 0.
$pre

$$
All elements in $italic alp$$ and $italic bIn$$ must be greater than or equal to 0.0.
$pre

$$
All elements in $italic bUp$$ must be greater than equal to corresponding elements
in $italic bLow$$.

$head Return Value$$
Upon a successful completion, the function returns normally and
sets the user-given output value place holders to point to the result values (ones that are requested).
If a failure occurs during the evaluation, an $xref/SpkException//exception/$$ may be
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/
/model/
/$$
This function expects $italic model$$ to be a function of
all three parameters: $math%alp%$$, $math%b%$$ and $math%i%$$.
Refer $xref/glossary/Model Functions Depend on alp and b/Model Functions Depend on alp and b/$$
for details. It expects that the individual has been selected by the caller
so that the model will address a proper subset of the entire data array.
$pre

$$
When $italic whichObjective$$ is specified $code NAIVE_FIRST_ORDER$$, $italic model$$ must
be an object of $xref/NaiveFoModel//NaiveFoModel/$$ which is a subclass of $xref/SpkModel//SpkModel/$$.
$syntax/

/isBlsq/
/$$
If this logical scalar is true it is assumed that
$math%
    f(alp, b) = f(alp, 0) + f_b(alp, 0) * b
    R(alp, b) = R(alp, 0)
%$$
and $italic model$$ is of type of $xref/NaiveFoModel//NaiveFoModel/$$.
For this case, the problem of optimizing in $math%b%$$  
is a linear least squares problem.
$syntax/

/optimizer/
/$$
This $xref/Optimizer//Optimizer/$$ class object has three attributes.  
These attributes are parameters of the optimizer used in the individual 
level optimization.
$syntax/

/dataForAll/
/$$
is a $math%Nb * n%$$ vector aggregating data vectors for all individuals.
$syntax/

/alp/
/$$
is a $math%Na%$$ dimensional column vector that
specifies a value for the fixed population parameter vector.
$syntax/

/bIn/
/$$
The $math%Nb%$$ dimensional column vector $italic bIn$$
specifies the initial value for the parameter vector 
during the optimization procedure.
$syntax/

/bLow/
/$$
The $math%Nb%$$ dimensional column vector $italic bLow$$
specifies the lower limit for the parameter vector 
$math%b%$$ during the optimization procedure.
$syntax/

/bUp/
/$$
The $math%Nb%$$ dimensional column vector $italic bUp$$
specifies the upper limit for the parameter vector 
$math%b%$$ during the optimization procedure.
$syntax/

/bStep/
/$$
The $math%Nb%$$ dimensional column vector $italic bStep$$
specifies the step size used for approximating
the derivative of $math%MapObj_b(b)%$$.
This is not used unless 
the input value of $italic MapObj_b_bOut%$$ is true.
$syntax/

/bHatOut/
/$$
The output value $italic bHatOut$$
is a $math%Nb%$$ dimensional column vector 
such that the 
$xref/glossary/Projected Gradient/projected gradient/$$ of
$math%Lambda(alp, bHatOut)%$$ with respect to $math%b%$$
is nearly zero.
$syntax/

/bTildeOut/
/$$
The output value $italic bTildeOut$$
is a $math%Nb%$$ dimensional column vector 
such that the central difference approximation for the
$xref/glossary/Projected Gradient/projected gradient/$$ of
$math%Lambda(alp, bTildeOut)%$$ with respect to $math%b%$$
is nearly zero.
$syntax/

/bTilde_popOut/
/$$
the output value $italic bTilde_popOut$$
is a $math%Nb by Na%$$ matrix equal to the derivative
of $math%bTilde(alp)_alp%$$ where $math%bTilde(alp)%$$
is the value of $math%b%$$ that solves the approximate
first order necessary conditions for a minimum.
Note that this derivative is only value on open sets
in $math%alp%$$ space where the set of active constraints
is constant.

$syntax/

/includeD/ (optional)
/$$
This flag indicates whether to include the whole D term into the evaluation.
The default is set to $math%true%$$ which forces to include the D term.

$head Example$$
Suppose that
$math%
            / alp(1)   0  \              / alp(2)  0   \
R(alp, b) = |             |    D(alp)  = |             |
            \ 0    alp(1) /              \ 0    alp(2) /

            / b(1) \        / 1 \
f(alp, b) = |      |   y =  |   |
            \ b(2) /        \ 2 /
%$$
It follows that
$math%                         
Lambda(alp, ind) = #log{2 #pi alp(1)} + (1/2) {[1 - b(1)]^2 + [2 - b(2)]^2} / alp(1)
                 + #log{2 #pi alp(2)} + (1/2) [b(1)^2 + b(2)^2] / alp(2)
%$$
The transpose of $math%lambda_b(alp, b)%$$ is equal to
$math%
    / [b(1) - 1] / alp(1) + b(1) / alp(2) \
    |                                      |
    \ [b(2) - 2] / alp(1) + b(2) / alp(2) /
%$$
The first order necessary condition for a minimum is 
that the gradient is zero. Thus, if $math%alp > 0%$$,
$math%bLow = 0%$$ and $math%bUp = 10%$$,
$math%
                      alp(2)      / 1 \
    bHat(alp) =  ---------------- |   | 
                 alp(1) + alp(2)  \ 2 /
%$$
Taking the derivative of the equation above,
$math%
                     -alp(2)        / 1   1 \             1        / 0   1 \
bHat_alp(alp) = ------------------- |       |  +   --------------- |       |
                [alp(1) + alp(2)]^2 \ 2   2 /      alp(1) + alp(2) \ 0   2 /
%$$
substituting in 
$math%alp(1) = 1%$$ and $math%alp(2) = 1%$$, we obtain
$math%
                       / .5 \
    bHat(alp)     =    |    |
                       \ 1. /

                    / -.25  .25 \
    bHat_alp(alp) = |           |
                    \ -.5   .5  /
%$$
$pre

$$
If you compile, link, and run the following program:
$codep
    #include <iostream>
    #include <cstdlib>
    #include "DoubleMatrix.h"
    #include "SpkModel.h"
    #include "estimateB.h"
    #include "transpose.h"
    #include "Optimizer.h"

    //========================================================================
    //   class UserModel declaration
    //========================================================================

    class UserModel : public SpkModel<double>
    {
        valarray<double> _a, _b;
        int _i, _nA, _nB, _nY;

    public:
        UserModel(int nY)
        : _nY ( nY );
        {};  
        ~UserModel(){};
    protected:
        void doSelectIndividual(int i)
        {
            _i = i;
        }
        void doSetPopPar(const valarray<double>& alp)
        {
            _a  = alp;
            _nA = _a.size();
        }
        void doSetIndPar(const valarray<double>& b)
        {
            _b  = b;
            _nB = _b.size();
        }
        void doIndParVariance( valarray<double>& D ) const
        {
            //
            // D(alp) = [ alp(2)   0     ]
            //          [  0      alp(2) ]
            //
            D.resize( _nB * _nB );
            D[0] = _a[1];
            D[1] = 0.0;
            D[2] = 0.0;
            D[3] = _a[1];
        }
        bool doIndParVariance_popPar( valarray<double>& D_a) const
        {
            //
            // D_alp(alp) = [ 0.0   1.0 ]
            //              [ 0.0   0.0 ]
            //              [ 0.0   0.0 ]
            //              [ 0.0   1.0 ]
            //
            D_a.resize( _nB * _nB * _nA );
            D_a = 0.0;
            D_a[4] = 1.0;
            D_a[7] = 1.0;
            return true;

        }
        void doDataMean( valarray<double>& f) const
        {
            //
            // f(alp, b) = [ b(1) ]
            //             [ b(2) ]
            //
            f.resize( _nB );
            f[0] = _b[0];
            f[1] = _b[1];
        }
        bool doDataMean_popPar( valarray<double>& f_a) const
        {
            //
            // f(alp, b)_alp = [ 0.0  0.0 ]
            //                 [ 0.0  0.0 ]
            //
            f_a.resize( _nB * _nA );
            f_a = 0.0;
            return false;
        }
        bool doDataMean_indPar( valarray<double>& f_b) const
        {
            //
            // f(alp, b)_b = [ 1.0  0.0 ]
            //               [ 0.0  1.0 ]
            //
            f_b.resize( _nB * _nB );
            f_b[0] = 1.0;
            f_b[1] = 0.0;
            f_b[2] = 0.0;
            f_b[3] = 1.0;
            return true;
        }
        void doDataVariance( valarray<double>& R) const
        {
            //
            // R(alp, b) = [ alp(1)   0     ]
            //             [  0      alp(1) ]
            //
            R.resize( _nY * _nY );
            R[0] = _a[0];
            R[1] = 0.0;
            R[2] = 0.0;
            R[3] = _a[0];
        }
        bool doDataVariance_popPar( valarray<double>& R_a) const
        {
            //
            // R(alp, b)_alp = [ 1.0   0.0 ]
            //                 [ 0.0   0.0 ]
            //                 [ 0.0   0.0 ]
            //                 [ 1.0   0.0 ]
            //
            R_a.resize( _nY * _nY * _nA );
            R_a = 0.0;
            R_a[0] = 1.0;
            R_a[3] = 1.0;

            return true;
        }
        bool doDataVariance_indPar( valarray<double>& R_b) const
        {
            //
            // R(alp, b)_b   = [ 0.0   0.0 ]
            //                 [ 0.0   0.0 ]
            //                 [ 0.0   0.0 ]
            //                 [ 0.0   0.0 ]
            //
            R_b.resize( _nY * _nY * _nB );
            R_b = 0.0;
            return false;
        }   
    };

    void estimateBTest::testExample()
    {
        using namespace std;
        int    Ny=2;
        int    Nb=2;
        int    Na=2;
        bool   blsq      = false;
        Optimizer optimizer(1.0e-3, 40, 0);
        DoubleMatrix dataForAll(Ny,1); 
        DoubleMatrix alp(Na,1); 
        DoubleMatrix bIn(Nb,1); 
        DoubleMatrix bUp(Nb,1); 
        DoubleMatrix bLow(Nb,1);
        DoubleMatrix bStep(Nb,1);
        DoubleMatrix bHatOut(Nb,1);
        DoubleMatrix bTildeOut(Nb,1);
        DoubleMatrix bTilde_alpOut(Nb,Na);

        double *dY = dataForAll.data();
        dY[0] = 1;
        dY[1] = 2;

        alp.fill(1);
        bIn.fill(.75);
        bUp.fill(10);
        bLow.fill(0);
        bStep.fill(0.1);

        UserModel model( Ny );

        try{
            estimateB(model, blsq, optimizer, dataForAll, 
                alp, bIn, bLow, bUp, bStep, 
                &bHatOut, &bTildeOut, &bTilde_alpOut);
        }
        catch( ... )
        {
            cerr << "estimateB failed" << endl;
            abort();
        }
        cout << "b^' = " << endl;
        (transpose(bHatOut)).print();
        cout << endl;

        cout << "b~' = " << endl;
        (transpose(bTildeOut)).print();
        cout << endl;

        cout << "b~_alp = " << endl;
        bTilde_alpOut.print();    
    }
$$
    then it will display the following when it is run:
$codep
        b^' =
        [0.5, 1]

        b~' =
        [0.5, 1]

        b~_alp =
        [-0.25, 0.25]
        [-0.5, 0.5]
$$
$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <iostream>
#include <cassert>
#include <cfloat>
#include <functional>
#include "estimateB.h"
#include "SpkModel.h"
#include "mapOpt.h"
#include "mapTilde.h"
#include "lambda.h"
#include "inverse.h"
#include "isGreaterThanOrEqualTo.h"
#include "allTrue.h"
#include "getMulRows.h"
#include "getMulCols.h"
#include "getSubblock.h"
#include "transpose.h"
#include "backDiv.h"
#include "elementwiseAnd.h"
#include "elementwiseOr.h"
#include "DBL_EPS_EQUAL_MULT.h"
#include "isLessThanOrEqualTo.h"
#include "isGreaterThanOrEqualTo.h"
#include "centdiff.h"
#include "DoubleMatrix.h"
#include "SpkException.h"
#include "placeRows.h"
#include "add.h"
#include "lambdaDiff.h"

#include "namespace_population_analysis.h"

namespace dbl_true_false{
    // Define constants
    const double dTRUE  = (double)true;
    const double dFALSE = (double)false;
}

using namespace std;
/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/
static DoubleMatrix isActive( 
    const DoubleMatrix &dvecCodomainToBeTested,
    const DoubleMatrix &dvecLowerbound,
    const DoubleMatrix &dvecUpperbound,
    const DoubleMatrix &dvecDomainToBeTested
    );

static DoubleMatrix operator!( const DoubleMatrix &A );
static DoubleMatrix operator-( const DoubleMatrix &A );
static DoubleMatrix operator==(const DoubleMatrix &A, const DoubleMatrix &B );
static DoubleMatrix operator!=(const DoubleMatrix &A, const DoubleMatrix &B );

static DoubleMatrix dmatD(__FILE__);
static DoubleMatrix dvecBhatTemp(__FILE__);
static DoubleMatrix dvecBtildeTemp(__FILE__);
static DoubleMatrix dvecNorm(__FILE__);
static DoubleMatrix drowMapObj_b(__FILE__);
static DoubleMatrix dmatMapObj_b_b(__FILE__);
static DoubleMatrix dvecActive(__FILE__);
static DoubleMatrix dvecFree(__FILE__);
static DoubleMatrix dvecFreeTrans(__FILE__);
static DoubleMatrix dmatLambda_b(__FILE__);
static DoubleMatrix dmatLambda_alp_bTemp(__FILE__);
static DoubleMatrix dmatLambda_b_alpTemp(__FILE__);
static DoubleMatrix dmatLambda_b_alp(__FILE__);
static DoubleMatrix dmatLambda_b_b(__FILE__);
static DoubleMatrix drowMapObj_bTrans(__FILE__);
static DoubleMatrix dmatf_b(__FILE__);
static DoubleMatrix dmatRInv(__FILE__);
static DoubleMatrix dmatDInv(__FILE__);

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
void estimateB(
    SpkModel<double> &model,
    bool   isBlsq,
    Optimizer& optimizer,
    const  DoubleMatrix &dvecY,
    const  DoubleMatrix &dvecAlp,
    const  DoubleMatrix &dvecBin,
    const  DoubleMatrix &dvecBlow,
    const  DoubleMatrix &dvecBup,
    const  DoubleMatrix &dvecBstep,
    DoubleMatrix *dvecBhatOut,
    DoubleMatrix *dvecBtildeOut,
    DoubleMatrix *dmatBtilde_alpOut    )
{
  using namespace population_analysis;

    int n = dvecBin.nr();

    assert( dvecBlow.nr() == n );
    assert( dvecBup.nr() == n );
    assert( dvecBstep.nr() == n );
    
    assert( optimizer.getEpsilon() > 0.0 );
    assert( optimizer.getNMaxIter() >= 0 );
    assert( optimizer.getLevel() >= 0 );
    assert( allTrue(dvecBup >= dvecBlow) );

    if( dvecBhatOut == 0 && dvecBtildeOut == 0 && dmatBtilde_alpOut == 0 ){
        return;
	}

    const valarray<double> alp = dvecAlp.toValarray();
    const valarray<double> bStep = dvecBstep.toValarray();
    valarray<double> f_b, RInv, DInv;
    const int nY = dvecY.nr();

    // 
    // doIndParVariance() is now requested in mapObj() but must set alpha at this level since
    // mapObj() does not take alpha as an argument.
    //
    model.setPopPar(alp);

    // Compute b^
	if( dvecBhatOut != 0 )
    {
		dvecBhatTemp.resize(n,1);

        //
        // Assumption:
        // When isBlsq is specified true, the concrete class type of SpkModel must be
        // of NaiveFoModel.
        //
        //
        // Reference: Lemma 15, pg71, Approximating the Marginal Likehood Estimate for Model
        // with random parameters".
        // 
        //   For FO, bHat could be predicted from:
        //          bHat(alp,y) = bTilde(alp,y) = -[ lambda_b_b(alp,0,y)]^-1 * lambda_b(alp0,y)^T
        //
        if( isBlsq )		
		{
            //------------------------------------------------------------
            // Handle zero iterations for the objective function.
            //------------------------------------------------------------

            if ( optimizer.getNMaxIter() == 0 )
            {
              // If the number of iterations is zero, then bIn is the
              // desired value for bHat.
              dvecBhatTemp = dvecBin;
    
            }
            else
            {
              valarray<double> bZeros( n );
              bZeros = 0.0;

              model.setPopPar(alp);
              model.setIndPar(bZeros);

              model.dataMean_indPar(f_b);
              dmatf_b.fromValarray( f_b, n );

              model.dataVarianceInv(RInv);
              dmatRInv.fromValarray( RInv, nY );


              model.indParVarianceInv(DInv);
              dmatDInv.fromValarray( DInv, n );

              dmatLambda_b_b = add( transpose( dmatf_b ) 
			                          * dmatRInv
			                          * dmatf_b, 
							        dmatDInv ); 
              Lambda lambdaOb(&model, dvecY, true);
              dmatLambda_b = centdiff<std::binder1st<Lambda> >
				                     (std::bind1st(lambdaOb, dvecAlp), DoubleMatrix( bZeros ), dvecBstep);

              
              //
              // For FO, bHat is approximated by
              //
              //   ^
              //   b = lambda_b_b^(-1) * lambda_b'
              //
              // NOTE that the above computation does not require optimziation.
              //
              dvecBhatTemp = - ( inverse( dmatLambda_b_b ) ) * ( transpose( dmatLambda_b ) );
            }
		}
		else
		{
            mapOpt( model, dvecY, optimizer, dvecBlow, dvecBup, dvecBin, &dvecBhatTemp,
                    dvecBstep, 0, 0, 0, true );
		}
        *dvecBhatOut = dvecBhatTemp;
    }

	// Compute b~
	if( dvecBtildeOut != 0 )
    {
        dvecBtildeTemp.resize(n,1);
        dvecNorm.resize(n,1);
        drowMapObj_b.resize(1,n);
        dmatMapObj_b_b.resize(n,n);

        try
        {
            mapTilde( model, dvecY, optimizer, *dvecBhatOut, dvecBlow, dvecBup, 
                dvecBtildeTemp, dvecBstep, dvecNorm, &drowMapObj_b, &dmatMapObj_b_b, true );
        }
        catch( SpkException& e )
        {
            throw e.push(
                SpkError::SPK_OPT_ERR,
               "Solution of the individual level first order necessary conditions failed.", 
                __LINE__,
                __FILE__);
        }
        catch( const std::exception& e )
        {
            throw SpkException(e,
                "A standard exception was thrown during the solution of the individual level \nfirst order necessary conditions.", 
                __LINE__,
                __FILE__);
        }
        catch( ... )
        {
            throw SpkException(SpkError::SPK_UNKNOWN_OPT_ERR,
                "An exception of unknown type was thrown during the solution of the individual level \nfirst order necessary conditions.", 
                __LINE__,
                __FILE__);
        }

        *dvecBtildeOut = dvecBtildeTemp;
    }    

    // Compute b~_alp
    if( dmatBtilde_alpOut != 0 )
	{
        assert( dmatBtilde_alpOut->nr() == dvecBin.nr() );
        assert( dmatBtilde_alpOut->nc() == dvecAlp.nr() );
        dmatBtilde_alpOut->fill(0);
        transpose(drowMapObj_b, drowMapObj_bTrans);

        dvecActive = isActive(drowMapObj_bTrans, dvecBlow, dvecBup, dvecBtildeTemp);

        // Compute 2nd derivatives only if constraints are not all active.
        if( !allTrue(dvecActive) )
        {
            dvecFree = !dvecActive;
            transpose(dvecFree, dvecFreeTrans);
            //
            // The derivative of approximate lambda_b is now computed here, instead of lambdaDiff() subroutine
            // to reduce if-then-else switching at run-time.
            //

            Lambda_alp lambda_alpOb(&model, dvecY, true);
            try{
                dmatLambda_alp_bTemp = centdiff< std::binder1st<Lambda_alp> >
					(std::bind1st(lambda_alpOb, dvecAlp), dvecBtildeTemp, dvecBstep);
			}
			catch( SpkException& e )
			{
				//
				// Revisit - Sachiko:
				//
				// This should dump all the parameter values to a file and 
				// give the filename as an error message.
				//
				throw e.push(SpkError::SPK_DIFF_ERR, 
					"An attempt to approximate the first derivative of Lambda with respect to the \npopulation parameter failed.",
					__LINE__, __FILE__);
			}
			catch( const std::exception& e )
			{
				//
				// Revisit - Sachiko:
				//
				// This should dump all the parameter values to a file and 
				// give the filename as an error message.
				//
 			        const int max = SpkError::maxMessageLen();
                                char buf[max];
				snprintf( buf, SpkError::maxMessageLen(),
                                               "A standard error was thrown during an approximation \
                                               of the first derivative of Lambda with respect to the population parameter." );
				throw SpkException( e, buf, __LINE__, __FILE__ );
			}
			catch( ... )
			{ 
				//
				// Revisit - Sachiko:
				//
				// This should dump all the parameter values to a file and 
				// give the filename as an error message.
				//
				throw SpkException(
					SpkError::SPK_DIFF_ERR, 
					"Unknown exception was thrown during an attempt to approximate \
					the first derivative of Lambda with respect to the population parameter.",
					__LINE__, __FILE__);
			}

            transpose(dmatLambda_alp_bTemp, dmatLambda_b_alpTemp);
            dmatLambda_b_alp     = getMulRows( dmatLambda_b_alpTemp, dvecFree );
            dmatLambda_b_b       = getMulCols( getMulRows( dmatMapObj_b_b, dvecFree ), dvecFreeTrans );
            placeRows(backDiv(-dmatLambda_b_b, dmatLambda_b_alp), *dmatBtilde_alpOut, dvecFree);
        }
    }
    
    return;
}

/*========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 *========================================================================*/

/*************************************************************************
 *
 * Function: isActive
 *
 * 
 * Description
 * -----------
 *
 * Check if the cuurent b value is on the upper bound AND its value is increasing
 * or on the lower bound AND decreasing.
 *
 * When either case is true, it implies the value is locally minimum.
 *
 *************************************************************************/
static DoubleMatrix isActive( 
    const DoubleMatrix &dvecCodomainToBeTested,
    const DoubleMatrix &dvecLowerbound,
    const DoubleMatrix &dvecUpperbound,
    const DoubleMatrix &dvecDomainToBeTested
    )
{
    assert( dvecLowerbound.nr()       == dvecUpperbound.nr() );
    assert( dvecUpperbound.nr()       == dvecDomainToBeTested.nr() );
    assert( dvecDomainToBeTested.nr() == dvecCodomainToBeTested.nr() );
    assert( dvecLowerbound.nc()       == dvecUpperbound.nc() );
    assert( dvecUpperbound.nc()       == dvecDomainToBeTested.nc() );
    assert( dvecDomainToBeTested.nc() == dvecCodomainToBeTested.nc() );

    const int n = dvecLowerbound.nr();

    DoubleMatrix onUpper(n,1);
    DoubleMatrix onLower(n,1);

    onUpper = 
        bband( (dvecLowerbound == dvecDomainToBeTested), (dvecCodomainToBeTested >= 0.0) );
    onLower =
        bband( (dvecUpperbound == dvecDomainToBeTested), (dvecCodomainToBeTested <= 0.0) );
 
    return bbor(onUpper, onLower);
}

// Updated 2-5-01 Alyssa
// fixed for const correctness 

static DoubleMatrix operator!(const DoubleMatrix &A){
    using namespace dbl_true_false;

    DoubleMatrix B(A.nr(), A.nc());
    const double *pdA = A.data();
    double *pdB = B.data();

    for( int i=0; i<A.nr()*A.nc(); i++ ){
        if( pdA[i] == dFALSE )
            pdB[i] = dTRUE;
        else
            pdB[i] = dFALSE;
    }
    return B;
}
// Updated 2-5-01 Alyssa
// fixed for const correctness 

static DoubleMatrix operator-(const DoubleMatrix &A){
    DoubleMatrix B(A.nr(), A.nc());
    const double *pdA = A.data();
    double *pdB = B.data();

    for( int i=0; i<A.nr()*A.nc(); i++ ){
            pdB[i] = -pdA[i];
    }
    return B;
}

// Updated 2-5-01 Alyssa
// fixed for const correctness 
static DoubleMatrix operator==(const DoubleMatrix &A, const DoubleMatrix &B){
    assert( A.nr() == B.nr() );
    assert( A.nc() == B.nc() );
    using namespace dbl_true_false;

    int m = A.nr();
    int n = A.nc();
    const double *pdA = A.data();
    const double *pdB = B.data();

    DoubleMatrix C(m,n);
    double *pdC = C.data();

    for( int i=0; i<A.nr()*A.nc(); i++ ){
        if( pdA[i] == pdB[i] ){
            pdC[i] = dTRUE;
        }
        else
            pdC[i] = dFALSE;
    }
    return C;
}
static DoubleMatrix operator!=(const DoubleMatrix &A, const DoubleMatrix &B)
{
    return !(A == B);
} 
