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
 * File: expectedHessian.cpp
 *
 *
 * Define expectedHessian() routine that computes the function
 * and its derivatives.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/


/*************************************************************************
 *
 * Function: expectedHessian
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin expectedHessian$$
$spell
    namespace std cout endl hessian const
    Dinv Rinv D_a Dinv_a Rinv_a Rinv_b Spk pa pb int doSetIndPar doSetPopPar
    nr doDataMean Ri inv Htilde covariances
    covariance
    ind
    bool
    valarray
    resize
    Model model
$$

$section Expected Hessian and Its Derivatives$$

$index expectedHessian$$
$index population, expected hessian$$
$cindex expected hessian approximation$$

$table
$bold Prototype:$$   $cend  
$syntax/
void expectedHessian(
         SpkModel& /model/, 
         const DoubleMatrix & /alp/,
         const DoubleMatrix & /b/,
         const DoubleMatrix & /bStep/,
         DoubleMatrix * /expectedHessianTilde/,
         DoubleMatrix * /expectedHessianTilde_alp/,
         DoubleMatrix * /expectedHessianTilde_b/
         )
/$$
$tend

See also $xref/ExpectedHessianFuncOb//function object for the DoubleMatrix version of expectedHessian()/$$
$pre
$$
See also $xref/ExpectedHessianValarrayFuncOb//function object for the valarray version of expectedHessian()/$$

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
If $math%alp%$$ and $math%b%$$ are equal to the
true fixed and random population parameter values respectively,
the expected value of the Hessian of $mref/Lambda/$$ with
respect to $math%b%$$ is
$math%
             -1                    T  -1
H(alp, b) = D  (alp) + f_b (alp, b)  R  (alp, b) f_b (alp, b)

            1            T        -1           -1
          + - R_b(alp, b)  kron[ R  (alp, b), R  (alp, b) ] R_b(alp, b)
            2
%$$
(The equation above uses
$xref/glossary/Individual Notation/individual notation/$$.)
The approximate expected Hessian, $math%Htilde(alp, b)%$$,
has the same definition as above
except that the derivatives are replaced by central difference approximations.

$head Reference$$
$italic
Approximating The Maximum Likelihood Estimate For Models With Random Parameter
$$


$head Arguments$$
$syntax/
/model/
/$$
This function expects $italic model$$ to be a function of
all three parameters: $math%alp%$$, $math%b%$$ and $math%i%$$.
Refer $xref/glossary/Model Functions Depend on i - alp - b/Model Functions Depend on i - alp - b/$$
for details.

$syntax/

/alp/
/$$
is a m dimensional population parameter vector.
$syntax/

/b/
/$$
is a n dimensional individual parameter vector.

$syntax/

/bStep/
/$$
is a n dimensional step size vector for taking central difference approximation for derivatives 
with respect to $italic b$$.
$syntax/

/*expectedHessianTilde/
/$$
is a n by n matrix that will hold the computational result from evaluation of the function.
If null is specified, no computation will be performed unless 
either or both $italic expectedHessianTilde_alp$$ or/and $italic expectedHessianTilde_b$$
is specified.

$syntax/

/*expectedHessianTilde_alp/
/$$
is a n^2 by m matrix that will hold the computational result from evaluation of the derivative of the function
with respect to $italic alp$$.
If null is specified, no computation will be performed. 
$syntax/

/*expectedHessianTilde_b/
/$$
is a n^2 by n matrix that will hold the computational result from evaluation of the derivative of the function
with respect to $italic b$$.
If null is specified, no computation will be performed. 

$head Example$$
If you compile, link, and run the following program:
$codep

    class expectedHessianTest::exampleModel : public SpkPopModelWithCovariances
    {
        valarray<double> _a, _b;
        int _i;
        int _nA, _nB;
    public:
        exampleModel() : _a(0,0), _b(0,0), _i(0), _nA(0), _nB(0) {};    
        ~exampleModel(){};
    protected:
        void doSelectIndividual(int i)
        {
            _i = i;
        }
        void doSetPopPar(const valarray<double>& a)
        {
            _a = a;
            _nA = _a.size();
        }
        void doSetIndPar(const valarray<double>& b)
        {
            _b = b;
            _nB = _b.size();
        }
        void doDataMean( valarray<double>& fOut ) const
        {
            //fOut = {alp(2) + b(2), alp(2) + b(2)}

            fOut.resize(2 * 1);
            fOut[0] = _a[1]+_b[1];
            fOut[1] = _a[1]+_b[1];
        }
        bool doDataMean_popPar( valarray<double>& f_alpOut ) const
        {
		    //f_alpOut = 
		    ///[ 0 , 1 ]
		    // [ 0 , 1 ]
            f_alpOut.resize(2 * 2);
            f_alpOut[0] = 0;
            f_alpOut[1] = 0;
            f_alpOut[2] = 1;
            f_alpOut[3] = 1;
            return !allZero(f_alpOut);
        }
        bool doDataMean_popPar( valarray<double>& f_bOut ) const
        {
		    //f_bOut =
		    //[ 0 , 1 ]
		    //[ 0 , 1 ]
            f_bOut.resize(2 * 2);
            f_bOut[0] = 0;
            f_bOut[1] = 0;
            f_bOut[2] = 1;
            f_bOut[3] = 1;
            return !allZero(f_bOut);

        }
        void Ri( valarray<double>& ROut ) const
        {
		    //ROut = 
		    //[ b(1) ,   0   ]
		    //[ 0    ,  a(1) ]
            ROut.resize(2 * 2);
            ROut[0] = _b[0];
            ROut[1] =  0.0;
            ROut[2] =  0.0;
            ROut[3] = _b[0];
        }
        bool doDataVariance_popPar( valarray<double>& R_alpOut ) const
        {
		    //R_alpOut =
		    //[ 0 , 0 ]
		    //[ 0 , 0 ]
		    //[ 0 , 0 ]
		    //[ 0 , 0 ]
            R_alpOut.resize(4 * 2);
            R_alpOut = 0.0;
            return false;
        }
        bool doDataVariance_indPar( valarray<double>& R_bOut ) const
        {
		    //R_bOut = 
		    //[ 1 , 0 ]
		    //[ 0 , 0 ]
		    //[ 0 , 0 ]
		    //[ 1 , 0 ]
            R_bOut.resize(4 * 2);
            R_bOut = 0.0;
            R_bOut[0] = 1.0;
            R_bOut[3] = 1.0;
            return true;
        }
        void doDataVarianceInv( valarray<double>& RinvOut ) const
        {
            //RinvOut = 
            // [ 1/ b(1)    0   ]
            // [   0     1/b(1) ]
            RinvOut.resize(2 * 2);
            RinvOut = 0.0;
            RinvOut[0] = 1.0/_b[0];
            RinvOut[3] = 1.0/_b[0];
       }
        bool doDataVarianceInv_popPar( valarray<double>& Rinv_aOut ) const
        {
            //Rinv_aOut =
            //
            // [ 0    0 ]
            // [ 0    0 ]
            // [ 0    0 ]
            // [ 0    0 ]
            Rinv_aOut.resize(4,2);
            Rinv_aOut = 0.0;
            return false;
        }
        bool doIndParVarianceInv_indPar( valarray<double>& Rinv_bOut ) const
        {
            // Rinv_bOut =
            //
            // [ -1/b(1)^2    0 ]
            // [    0         0 ]
            // [    0         0 ]
            // [ -1/b(1)^2    0 ]
            Rinv_bOut.resize(4 * 2);
            Rinv_bOut = 0.0;
            Rinv_bOut[0] = -1.0/(_b[0]*_b[0]);
            Rinv_bOut[3] = -1.0/(_b[0]*_b[0]);

            ret = Rinv_bOut;
            return !allZero(ret);
        }
        void doIndParVariance( valarray<double>& DOut ) const
        {
		    //DOut = 
		    //[ alp(1)^(-1) ,   0   ]
		    //[ 0    ,  alp(1)^(-1) ]
            DOut.resize(2 * 2);
            DOut = 0.0;
            DOut[0] = 1.0/_a[0];
            DOut[3] = 1.0/_a[0];
        }
        bool doIndParVariance_popPar( valarray<double>& D_alpOut ) const
        {
		    //D_alpOut = 
		    //[ -alp(1)^(-2) , 0 ]
		    //[ 0            , 0 ]
		    //[ 0            , 0 ]
		    //[ -alp(1)^(-2) , 0 ]
            D_alpOut.resize( 2 * 2 * 2 );
            D_alpOut = 0.0;
            D_alpOut[0] = -1.0/(_a[0]*_a[0]);
            D_alpOut[3] = -1.0/(_a[0]*_a[0]);
            return true;
        }
        void doIndParVarianceInv( valarray<double>& DinvOut ) const
        {
		    //DinvOut = 
		    //[ alp(1) ,            0   ]
		    //[ 0    ,           alp(1) ]
            DinvOut.resize(2 * 2);
            DinvOut[0] = _a[0];
            DinvOut[1] =  0.0;
            DinvOut[2] =  0.0;
            DinvOut[3] = _a[0];
        }
        bool doIndParVarianceInv_popPar( valarray<double>& Dinv_aOut ) const
        {
            // Dinv_aOut =
            // [ 1     0 ]
            // [ 0     0 ]
            // [ 0     0 ]
            // [ 1     0 ]
            Dinv_aOut.resize(2 * 2 * 2);
            Dinv_aOut = 0.0;
            Dinv_aOut[0] = 1;
            Dinv_aOut[3] = 1;
            return true;
        }
    };
    void expectedHessianTest::testExample()
    {
        using namespace std;
        DoubleMatrix alp(2,1);
        alp.fill(1.0);
    
        DoubleMatrix b(2,1);
        b.fill(1.0);

        DoubleMatrix bStep(2,1);
        bStep.fill(0.1);

        exampleModel model;

        DoubleMatrix expectedHessianTilde(2, 2);
        DoubleMatrix expectedHessianTilde_b(2, 2*2);
        DoubleMatrix expectedHessianTilde_a(2, 2*2);

        expectedHessian(model, alp, b, bStep, 
            &expectedHessianTilde, &expectedHessianTilde_a, &expectedHessianTilde_b);
    
        cout << "HtildeOut = " << endl;
        expectedHessianTilde.print();
        cout << "Htilde_alpOut = " << endl;
        expectedHessianTilde_a.print();
        cout << "Htilde_bOut = " << endl;
        expectedHessianTilde_b.print();
    
    }
$$
then it will display the following when it is run:
$codep

    HtildeOut =
    [ 2 0 ]
    [ 0 3 ]
    Htilde_alpOut =
    [ 1 0 ]
    [ 0 0 ]
    [ 0 0 ]
    [ 1 0 ]
    Htilde_bOut =
    [ -2 0 ]
    [ 0 0 ]
    [ 0 0 ]
    [ -2 0 ]
$$
$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#pragma warning ( disable : 4786 )
#include <vector>

#include <cmath>

#include "expectedHessian.h"
#include "DoubleMatrix.h"
#include "UTranTimesSymKronSymTimesU.h"
#include "add.h"
#include "mulByScalar.h"
#include "rvecInverse.h"
#include "transpose.h"
#include "getCol.h"
#include "multiply.h"
#include "AkronBtimesC.h"
#include "centdiff.h"
#include "Function.h"
#include "replaceJth.h"
#include "transposeRowBlocks.h"
#include "getRow.h"
#include "IkronBtimesC.h"
#include "AkronItimesC.h"
#include "replaceIth.h"
#include "symmetrize.h"
#include "isSymmetric.h"

/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

static const DoubleMatrix expectedHessian_b(
         SpkModel& model, 
         const DoubleMatrix & alp,
         const DoubleMatrix & b,
         const DoubleMatrix & bStep,
         const DoubleMatrix & Rinv,
         const DoubleMatrix & R_bApprox,
         const DoubleMatrix & f_bApprox,
         const DoubleMatrix A[]);

static const DoubleMatrix expectedHessian_alp(
         SpkModel& model, 
         const DoubleMatrix & alp,
         const DoubleMatrix & b,
         const DoubleMatrix & bStep,
         const DoubleMatrix & Rinv,
         const DoubleMatrix & R_bApprox,
         const DoubleMatrix & f_bApprox,
         const DoubleMatrix A[]);


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
void expectedHessian(
         SpkModel& model, 
         const DoubleMatrix & dvecAlp,
         const DoubleMatrix & dvecB,
         const DoubleMatrix & dvecBStep,
         DoubleMatrix * expectedHessianTilde,
         DoubleMatrix * expectedHessianTilde_alp,
         DoubleMatrix * expectedHessianTilde_b
         )
{
    if( !(expectedHessianTilde || expectedHessianTilde_b || expectedHessianTilde_alp) )
        return;

    using namespace std;
    int j;

    const int nA = dvecAlp.nr();
    const int nB = dvecB.nr();
    assert(dvecBStep.nr()   == nB);

    const valarray<double> alp   = dvecAlp.toValarray();
    const valarray<double> b     = dvecB.toValarray();
    const valarray<double> bStep = dvecBStep.toValarray();

    model.setPopPar(alp);
    model.setIndPar(b);

    valarray<double> Dinv( nB * nB );
    model.indParVarianceInv(Dinv);
    DoubleMatrix dmatDinv( Dinv, nB );
    assert(dmatDinv.nr() == nB);
    assert(dmatDinv.nc() == nB);

    valarray<double> RInv;
    model.dataVarianceInv(RInv);
    const int nY = static_cast<int>(sqrt(static_cast<double>(RInv.size())));
    assert( RInv.size() == nY * nY );
    DoubleMatrix dmatRInv( RInv, nY );

    assert(isSymmetric(dmatRInv));

    // [Revisit - Model Based Caching and the Expected Hessian Functions - Mitch]
    // If R_bApprox and f_bApprox were calculated in a single
    // loop in this function and if f_bApprox were calculated
    // first, it would allow model-based R matrices to use the
    // cached values for f calculated for each value of dvecB.

    ModelFunctionValarray ROb(&SpkModel::dataVariance, &model);
    DoubleMatrix R_bApprox = DoubleMatrix( centdiff<binder1st<ModelFunctionValarray> >(bind1st(ROb, alp), nY, b, bStep), nB );
    assert(R_bApprox.nr() == nY*nY);
    assert(R_bApprox.nc() == nB);

    ModelFunctionValarray fOb(&SpkModel::dataMean, &model);
    DoubleMatrix f_bApprox = DoubleMatrix( centdiff<binder1st<ModelFunctionValarray> >(bind1st(fOb,alp), 1, b, bStep), nB );
    assert(f_bApprox.nr() == nY);
    assert(f_bApprox.nc() == nB);
    
    DoubleMatrix C(nB,nB);
    DoubleMatrix A[nB];

    //    std::vector<DoubleMatrix> A(nB);
    //    A.resize(nB);
    //    for(j=0; j<nB; j++)
    //    {
    //        A[j].resize(nB,nB);
    //    }

    //DoubleMatrix H1 = dmatDinv;
    DoubleMatrix H2 = multiply(multiply(transpose(f_bApprox), dmatRInv), f_bApprox);

    //    UTranTimesSymKronSymTimesU(dmatRInv, R_bApprox, nB, C, A.begin());
    UTranTimesSymKronSymTimesU(dmatRInv, R_bApprox, nB, C, A);

    DoubleMatrix H3 = mulByScalar(C, 0.5);

    if( expectedHessianTilde )
    {
        symmetrize(add(add(dmatDinv, H2), H3), *expectedHessianTilde);
        //*expectedHessianTilde = symmetrize(add(add(dmatDinv, H2), H3));
    }
    if( expectedHessianTilde_b )
        *expectedHessianTilde_b = expectedHessian_b(model,dvecAlp,dvecB,dvecBStep,dmatRInv,R_bApprox,f_bApprox, A);

    if( expectedHessianTilde_alp )
        *expectedHessianTilde_alp = expectedHessian_alp(model,dvecAlp,dvecB,dvecBStep,dmatRInv,R_bApprox,f_bApprox, A);
}

/*========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 *========================================================================*/

#include "UTranTimesSymKronSymTimesU.h"
#include "UTranTimesSymKronSymTimesU_x.h"
#include "ABA_x.h"
#include "add.h"
#include "mulByScalar.h"
#include "rvecInverse.h"
#include "transpose.h"
#include "getCol.h"
#include "multiply.h"
#include "AkronBtimesC.h"


#include "centdiff.h"
#include "Function.h"
#include "replaceJth.h"
#include "transposeRowBlocks.h"
#include "getRow.h"
#include "IkronBtimesC.h"
#include "AkronItimesC.h"
#include "replaceIth.h"

/*************************************************************************
 *
 * Function: expectedHessian_b
 *
 * 
 * Description
 * -----------
 *
 * Compute the derivative of Expected Hessian with respect to b.
 *
 *
 * Reference
 * ---------
 *
 * Lemma 14, "Approximateing the marginal likelihood estimate for models with random paramters"
 * (Applied Mathematics and Computation 119(2001) 57-75)
 *
 *
 * Arguments
 * ---------
 *
 * Rinv       the inverse value of R(alp,b)
 *
 * R_bApprox  the central difference approximation for the derivative of R(alp,b) with respect to b.
 *
 * f_bApprox  the central difference approximation for the derivative of f(alp,b) with respect to b.
 *
 *
 *************************************************************************/

const DoubleMatrix expectedHessian_b(
         SpkModel& model, 
         const DoubleMatrix & dvecAlp,
         const DoubleMatrix & dvecB,
         const DoubleMatrix & dvecBStep,
         const DoubleMatrix & Rinv,
         const DoubleMatrix & R_bApprox,
         const DoubleMatrix & f_bApprox,
         const DoubleMatrix A[])
{
    /*
     **********************************************
        Compute expectedHessian_b 
     ***********************************************
     */
    using namespace std;

    const int nA = dvecAlp.nr();
    const int nB = dvecB.nr();
    const int nY = Rinv.nr();
    assert(dvecBStep.nr()   == nB);
    assert(Rinv.nc() == nY);

    const valarray<double> alp   = dvecAlp.toValarray();
    const valarray<double> b     = dvecB.toValarray();
    const valarray<double> bStep = dvecBStep.toValarray();

    model.setPopPar(alp);
    model.setIndPar(b);

    //
    // [Comment - Sachiko]
    // Eliminated a block declaring and filling with zero a DInv_b matrix.
    // The matrix was a mere place holder to better follow the paper describing this method.
    //
    // DoubleMatrix Dinv_b( nB, nB );
    // Dinv_b.fill(0.0);

    valarray<double> RInv_b( nY * nY * nB );
    model.dataVarianceInv_indPar( RInv_b );
    DoubleMatrix dmatRInv_b( RInv_b, nB );

    assert(dmatRInv_b.nr() == nY*nY);
    assert(dmatRInv_b.nc() == nB);

    // [Revisit - Model Based Caching and the Expected Hessian Functions - Mitch]
    // If R_b_bApprox and f_b_bApprox were calculated in a single
    // loop in this function and if f_b_bApprox were calculated
    // first, it would allow model-based R_b matrices to use the
    // cached values for f_b calculated for each value of dvecB.

    ModelDerivativeValarray R_bOb(&SpkModel::dataVariance_indPar, &model);
    DoubleMatrix R_b_bApprox = DoubleMatrix( centdiff<binder1st<ModelDerivativeValarray> >(bind1st(R_bOb, alp), nB, b, bStep), nB );
    assert(R_b_bApprox.nr() == nY*nY*nB);
    assert(R_b_bApprox.nc() == nB);

    ModelDerivativeValarray f_bOb(&SpkModel::dataMean_indPar, &model);
    DoubleMatrix f_b_bApprox = DoubleMatrix( centdiff<binder1st<ModelDerivativeValarray> >(bind1st(f_bOb, alp), nB, b, bStep), nB );
    assert(f_b_bApprox.nr() == nY*nB);
    assert(f_b_bApprox.nc() == nB);
    DoubleMatrix f_bApprox_b = transposeRowBlocks(f_b_bApprox, nB);

    DoubleMatrix C_x(nB*nB, nB);

    UTranTimesSymKronSymTimesU_x(Rinv, dmatRInv_b, R_bApprox, R_b_bApprox, nB, A, C_x);

    //
    // [ Comment - Sachiko ]
    // Took off Dinv_b component that was added to the resulting matrix of (C_x*0.5).
    // Dinv_b was filled with zero in this routine.
    //
    return add(mulByScalar(C_x, 0.5), ABA_x(f_bApprox, Rinv, f_bApprox_b, dmatRInv_b));
    
}
/*************************************************************************
 *
 * Function: expectedHessian_alp
 *
 * 
 * Description
 * -----------
 *
 * Compute the derivative of Expected Hessian with respect to alp.
 *
 *
 * Reference
 * ---------
 *
 * Lemma 14, "Approximateing the marginal likelihood estimate for models with random paramters"
 * (Applied Mathematics and Computation 119(2001) 57-75)
 *
 *
 * Arguments
 * ---------
 *
 * Rinv       the inverse value of R(alp,b)
 *
 * R_bApprox  the central difference approximation for the derivative of R(alp,b) with respect to alp.
 *
 * f_bApprox  the central difference approximation for the derivative of f(alp,b) with respect to alp.
 *
 *
 *************************************************************************/
const DoubleMatrix expectedHessian_alp(
         SpkModel& model, 
         const DoubleMatrix & dvecAlp,
         const DoubleMatrix & dvecB,
         const DoubleMatrix & dvecBStep,
         const DoubleMatrix & Rinv,
         const DoubleMatrix & R_bApprox,
         const DoubleMatrix & f_bApprox,
         const DoubleMatrix A[])
{
    /*
     **********************************************
        Compute expectedHessian_alp
     ***********************************************
     */
    using namespace std;

    const int nA = dvecAlp.nr();
    const int nB = dvecB.nr();
    const int nY = Rinv.nr();
    assert(dvecBStep.nr()   == nB);
    assert(Rinv.nc() == nY);

    const valarray<double> alp   = dvecAlp.toValarray();
    const valarray<double> b     = dvecB.toValarray();
    const valarray<double> bStep = dvecBStep.toValarray();

    model.setPopPar(alp);
    model.setIndPar(b);

    valarray<double> DInv_a( nB * nB * nA );
    model.indParVarianceInv_popPar( DInv_a );
    DoubleMatrix dmatDInv_a( DInv_a, nA );
    assert(dmatDInv_a.nr()==nB*nB);
    assert(dmatDInv_a.nc()==nA );

    valarray<double> RInv_a( nY * nY * nA );
    model.dataVarianceInv_popPar( RInv_a );
    DoubleMatrix dmatRInv_a( RInv_a, nA );
    assert(dmatRInv_a.nr() == nY*nY);
    assert(dmatRInv_a.nc() == nA);

    // [Revisit - Model Based Caching and the Expected Hessian Functions - Mitch]
    // If R_a_bApprox and f_a_bApprox were calculated in a single
    // loop in this function and if f_a_bApprox were calculated
    // first, it would allow model-based R_b matrices to use the
    // cached values for f_b calculated for each value of dvecB.

    ModelDerivativeValarray R_aOb(&SpkModel::dataVariance_popPar, &model);
    DoubleMatrix R_a_bApprox = DoubleMatrix( centdiff<binder1st<ModelDerivativeValarray> >(bind1st(R_aOb, alp), nA, b, bStep), nB );
    assert(R_a_bApprox.nr() == nY*nY*nA);
    assert(R_a_bApprox.nc() == nB);

    ModelDerivativeValarray f_aOb(&SpkModel::dataMean_popPar, &model);
    DoubleMatrix f_a_bApprox = DoubleMatrix( centdiff<binder1st<ModelDerivativeValarray> >(bind1st(f_aOb, alp), nA, b, bStep), nB );
    assert(f_a_bApprox.nr() == nY*nA);
    assert(f_a_bApprox.nc() == nB);
    DoubleMatrix f_bApprox_a = transposeRowBlocks(f_a_bApprox, nA);

    DoubleMatrix C_x(nB*nB, nA);

    UTranTimesSymKronSymTimesU_x(Rinv, dmatRInv_a, R_bApprox, R_a_bApprox, nA, A, C_x);

    DoubleMatrix H_b1 = dmatDInv_a;
    assert(H_b1.nr() == nB*nB);
    assert(H_b1.nc() == nA);

    DoubleMatrix H_b234 = mulByScalar(C_x, 0.5);
    assert(H_b234.nr() == nB*nB);
    assert(H_b234.nc() == nA);

    DoubleMatrix H_b567 = ABA_x(f_bApprox, Rinv, f_bApprox_a, dmatRInv_a);
    assert(H_b567.nr() == nB*nB);
    assert(H_b567.nc() == nA);

    return add(add(H_b1,H_b234),H_b567);
        //add(add(dmatDInv_a, mulByScalar(C_x, 0.5)), ABA_x(f_bApprox, Rinv, f_a_bApprox, Rinv_a));
}
/*************************************************************************
 *
 * Class: ExpectedHessian
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin ExpectedHessianFuncOb$$
$spell
    namespace std cout endl hessian const
    Dinv Rinv D_a Dinv_a Rinv_a Rinv_b Spk pa pb int doSetIndPar doSetPopPar
    nr doDataMean Ri inv Htilde covariances
    covariance
    ind
    bool
    valarray
    resize
    Model model
$$

$section Function Object for DoubleMatrix Version of expectedHessian()$$

$index expectedHessian$$
$index function, expectedHessian$$

$table
$bold Constructor:$$   $cend  
$syntax/
ExpectedHessian(SpkModel *m, const DoubleMatrix& bStep)/$$
$tend

See also $xref/expectedHessian//function definition/$$
$pre
$$
See also $xref/ExpectedHessianValarrayFuncOb//function object for the valarray version of expectedHessian()/$$

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Encapsulate $xref/expectedHessian//expectedHessian()/$$ and provide a way for the user to
evaluate the function as if it is simply a function of two variables (ie. binary function);
$math%alp%$$ and $math%b%$$.

$head Constructor Arguments$$
$syntax/
/model/
/$$
The constructor expects $italic model$$ to be a function of
all three parameters: $math%alp%$$, $math%b%$$ and $math%i%$$.
Refer $xref/glossary/Model Functions Depend on i - alp - b/Model Functions Depend on i - alp - b/$$
for details.

$syntax/

/
bStep
/$$
The constructor expects a step size vector for particularly one of the variables, $math%b%$$.
The length of $italic bStep$$ must match the length of $math%b%$$.

$head Public Interfaces$$
$syntax/
/const DoubleMatrix operator( const DoubleMatrix & /alp/, const DoubleMatrix & /b/ )/
/$$
evaluates $xref/expectedHessian//expectedHessian()/$$ at the points $italic alp$$ and $italic b$$
and returns the objective.  $italic alp$$ is a $math%m%$$ dimensional column vector.
$italic b$$ is a $math%n%$$ dimensional column vector.  
The resulting value is a $math%n x n%$$ dimensional matrix.


$head Example$$
If you compile, link, and run the following program:
$codep

    class expectedHessianTest::exampleModel : public SpkPopModelWithCovariances
    {
        valarray<double> _a, _b;
        int _i;
        int _nA, _nB;
    public:
        exampleModel() : _a(0,0), _b(0,0), _i(0), _nA(0), _nB(0) {};    
        ~exampleModel(){};
    protected:
        void doSelectIndividual(int i)
        {
            _i = i;
        }
        void doSetPopPar(const valarray<double>& a)
        {
            _a = a;
            _nA = _a.size();
        }
        void doSetIndPar(const valarray<double>& b)
        {
            _b = b;
            _nB = _b.size();
        }
        void doDataMean( valarray<double>& fOut ) const
        {
            //fOut = {alp(2) + b(2), alp(2) + b(2)}

            fOut.resize(2 * 1);
            fOut[0] = _a[1]+_b[1];
            fOut[1] = _a[1]+_b[1];
        }
        bool doDataMean_popPar( valarray<double>& f_alpOut ) const
        {
		    //f_alpOut = 
		    ///[ 0 , 1 ]
		    // [ 0 , 1 ]
            f_alpOut.resize(2 * 2);
            f_alpOut[0] = 0;
            f_alpOut[1] = 0;
            f_alpOut[2] = 1;
            f_alpOut[3] = 1;
            return !allZero(f_alpOut);
        }
        bool doDataMean_popPar( valarray<double>& f_bOut ) const
        {
		    //f_bOut =
		    //[ 0 , 1 ]
		    //[ 0 , 1 ]
            f_bOut.resize(2 * 2);
            f_bOut[0] = 0;
            f_bOut[1] = 0;
            f_bOut[2] = 1;
            f_bOut[3] = 1;
            return !allZero(f_bOut);

        }
        void Ri( valarray<double>& ROut ) const
        {
		    //ROut = 
		    //[ b(1) ,   0   ]
		    //[ 0    ,  a(1) ]
            ROut.resize(2 * 2);
            ROut[0] = _b[0];
            ROut[1] =  0.0;
            ROut[2] =  0.0;
            ROut[3] = _b[0];
        }
        bool doDataVariance_popPar( valarray<double>& R_alpOut ) const
        {
		    //R_alpOut =
		    //[ 0 , 0 ]
		    //[ 0 , 0 ]
		    //[ 0 , 0 ]
		    //[ 0 , 0 ]
            R_alpOut.resize(4 * 2);
            R_alpOut = 0.0;
            return false;
        }
        bool doDataVariance_indPar( valarray<double>& R_bOut ) const
        {
		    //R_bOut = 
		    //[ 1 , 0 ]
		    //[ 0 , 0 ]
		    //[ 0 , 0 ]
		    //[ 1 , 0 ]
            R_bOut.resize(4 * 2);
            R_bOut = 0.0;
            R_bOut[0] = 1.0;
            R_bOut[3] = 1.0;
            return true;
        }
        void doDataVarianceInv( valarray<double>& RinvOut ) const
        {
            //RinvOut = 
            // [ 1/ b(1)    0   ]
            // [   0     1/b(1) ]
            RinvOut.resize(2 * 2);
            RinvOut = 0.0;
            RinvOut[0] = 1.0/_b[0];
            RinvOut[3] = 1.0/_b[0];
       }
        bool doDataVarianceInv_popPar( valarray<double>& Rinv_aOut ) const
        {
            //Rinv_aOut =
            //
            // [ 0    0 ]
            // [ 0    0 ]
            // [ 0    0 ]
            // [ 0    0 ]
            Rinv_aOut.resize(4,2);
            Rinv_aOut = 0.0;
            return false;
        }
        bool doIndParVarianceInv_indPar( valarray<double>& Rinv_bOut ) const
        {
            // Rinv_bOut =
            //
            // [ -1/b(1)^2    0 ]
            // [    0         0 ]
            // [    0         0 ]
            // [ -1/b(1)^2    0 ]
            Rinv_bOut.resize(4 * 2);
            Rinv_bOut = 0.0;
            Rinv_bOut[0] = -1.0/(_b[0]*_b[0]);
            Rinv_bOut[3] = -1.0/(_b[0]*_b[0]);

            ret = Rinv_bOut;
            return !allZero(ret);
        }
        void doIndParVariance( valarray<double>& DOut ) const
        {
		    //DOut = 
		    //[ alp(1)^(-1) ,   0   ]
		    //[ 0    ,  alp(1)^(-1) ]
            DOut.resize(2 * 2);
            DOut = 0.0;
            DOut[0] = 1.0/_a[0];
            DOut[3] = 1.0/_a[0];
        }
        bool doIndParVariance_popPar( valarray<double>& D_alpOut ) const
        {
		    //D_alpOut = 
		    //[ -alp(1)^(-2) , 0 ]
		    //[ 0            , 0 ]
		    //[ 0            , 0 ]
		    //[ -alp(1)^(-2) , 0 ]
            D_alpOut.resize( 2 * 2 * 2 );
            D_alpOut = 0.0;
            D_alpOut[0] = -1.0/(_a[0]*_a[0]);
            D_alpOut[3] = -1.0/(_a[0]*_a[0]);
            return true;
        }
        void doIndParVarianceInv( valarray<double>& DinvOut ) const
        {
		    //DinvOut = 
		    //[ alp(1) ,            0   ]
		    //[ 0    ,           alp(1) ]
            DinvOut.resize(2 * 2);
            DinvOut[0] = _a[0];
            DinvOut[1] =  0.0;
            DinvOut[2] =  0.0;
            DinvOut[3] = _a[0];
        }
        bool doIndParVarianceInv_popPar( valarray<double>& Dinv_aOut ) const
        {
            // Dinv_aOut =
            // [ 1     0 ]
            // [ 0     0 ]
            // [ 0     0 ]
            // [ 1     0 ]
            Dinv_aOut.resize(2 * 2 * 2);
            Dinv_aOut = 0.0;
            Dinv_aOut[0] = 1;
            Dinv_aOut[3] = 1;
            return true;
        }
    };
    void expectedHessianTest::testExample()
    {
        using namespace std;
        DoubleMatrix alp(2,1);
        alp.fill(1.0);
    
        DoubleMatrix b(2,1);
        b.fill(1.0);

        DoubleMatrix bStep(2,1);
        bStep.fill(0.1);

        exampleModel model;

        DoubleMatrix expectedHessianTilde(2, 2);

        ExpectedHessian ehOb( &model, bStep );
        expectedHessianTilde = ehOb( alp, b );

        cout << "HtildeOut = " << endl;
        expectedHessianTilde.print();  
    }
$$
then it will display the following when it is run:
$codep

    HtildeOut =
    [ 2 0 ]
    [ 0 3 ]
$$
$end
*/

/*************************************************************************
 *
 * Class: ExpectedHessianValarray
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin ExpectedHessianValarrayFuncOb$$
$spell
    namespace 
    std
    cout 
    endl
    hessian 
    const
    Dinv 
    Rinv
    D_a
    Dinv_a 
    Rinv_a
    Rinv_b
    Spk 
    pa 
    pb 
    int 
    nr 
    Ri
    inv 
    Htilde 
    covariances
    covariance
    ind
    bool
    valarray
    resize
    resize
    Model model
$$

$section Function Object for valarray Version of expectedHessian()$$

$index expectedHessian$$
$index function, expectedHessian$$

$table
$bold Constructor:$$   $cend  
$syntax/
ExpectedHessian(SpkModel *m, const valarray<double>& bStep)/$$
$tend

See also $xref/expectedHessian//function definition/$$
$pre
$$
See also $xref/ExpectedHessianFuncOb//function object for the DoubleMatrix version of expectedHessian()/$$

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Encapsulate $xref/expectedHessian//expectedHessian()/$$ and provide a way for the user to
evaluate the function as if it is simply a function of two variables (ie. binary function);
$math%alp%$$ and $math%b%$$.

$head Constructor Arguments$$
$syntax/
/model/
/$$
The constructor expects $italic model$$ to be a function of
all three parameters: $math%alp%$$, $math%b%$$ and $math%i%$$.
Refer $xref/glossary/Model Functions Depend on i - alp - b/Model Functions Depend on i - alp - b/$$
for details.

$syntax/

/
bStep
/$$
The constructor expects a step size vector for particularly one of the variables, $math%b%$$.
The length of $italic bStep$$ must match the length of $math%b%$$.

$head Public Interfaces$$
$syntax/
/const valarray<double> operator( const valarray<double> & /alp/, const valarray<double> & /b/ )/
/$$
evaluates $xref/expectedHessian//expectedHessian()/$$ at the points $italic alp$$ and $italic b$$
and returns the objective.  $italic alp$$ is a $math%m%$$ dimensional vector.
$italic b$$ is a $math%n%$$ dimensional vector.  
The resulting value is a $math%n * n%$$ dimensional vector.


$head Example$$
If you compile, link, and run the following program:
$codep

    #include "SpkValarray.h"

    class expectedHessianTest::exampleModel : public SpkPopModelWithCovariances
    {
        valarray<double> _a, _b;
        int _i;
        int _nA, _nB;
    public:
        exampleModel() : _a(0,0), _b(0,0), _i(0), _nA(0), _nB(0) {};    
        ~exampleModel(){};
    protected:
        void doSelectIndividual(int i)
        {
            _i = i;
        }
        void doSetPopPar(const valarray<double>& a)
        {
            _a = a;
            _nA = _a.size();
        }
        void doSetIndPar(const valarray<double>& b)
        {
            _b = b;
            _nB = _b.size();
        }
        void doDataMean( valarray<double>& fOut ) const
        {
            //fOut = {alp(2) + b(2), alp(2) + b(2)}

            fOut.resize(2 * 1);
            fOut[0] = _a[1]+_b[1];
            fOut[1] = _a[1]+_b[1];
        }
        bool doDataMean_popPar( valarray<double>& f_alpOut ) const
        {
		    //f_alpOut = 
		    ///[ 0 , 1 ]
		    // [ 0 , 1 ]
            f_alpOut.resize(2 * 2);
            f_alpOut[0] = 0;
            f_alpOut[1] = 0;
            f_alpOut[2] = 1;
            f_alpOut[3] = 1;
            return !allZero(f_alpOut);
        }
        bool doDataMean_popPar( valarray<double>& f_bOut ) const
        {
		    //f_bOut =
		    //[ 0 , 1 ]
		    //[ 0 , 1 ]
            f_bOut.resize(2 * 2);
            f_bOut[0] = 0;
            f_bOut[1] = 0;
            f_bOut[2] = 1;
            f_bOut[3] = 1;
            return !allZero(f_bOut);

        }
        void Ri( valarray<double>& ROut ) const
        {
		    //ROut = 
		    //[ b(1) ,   0   ]
		    //[ 0    ,  a(1) ]
            ROut.resize(2 * 2);
            ROut[0] = _b[0];
            ROut[1] =  0.0;
            ROut[2] =  0.0;
            ROut[3] = _b[0];
        }
        bool doDataVariance_popPar( valarray<double>& R_alpOut ) const
        {
		    //R_alpOut =
		    //[ 0 , 0 ]
		    //[ 0 , 0 ]
		    //[ 0 , 0 ]
		    //[ 0 , 0 ]
            R_alpOut.resize(4 * 2);
            R_alpOut = 0.0;
            return false;
        }
        bool doDataVariance_indPar( valarray<double>& R_bOut ) const
        {
		    //R_bOut = 
		    //[ 1 , 0 ]
		    //[ 0 , 0 ]
		    //[ 0 , 0 ]
		    //[ 1 , 0 ]
            R_bOut.resize(4 * 2);
            R_bOut = 0.0;
            R_bOut[0] = 1.0;
            R_bOut[3] = 1.0;
            return true;
        }
        void doDataVarianceInv( valarray<double>& RinvOut ) const
        {
            //RinvOut = 
            // [ 1/ b(1)    0   ]
            // [   0     1/b(1) ]
            RinvOut.resize(2 * 2);
            RinvOut = 0.0;
            RinvOut[0] = 1.0/_b[0];
            RinvOut[3] = 1.0/_b[0];
       }
        bool doDataVarianceInv_popPar( valarray<double>& Rinv_aOut ) const
        {
            //Rinv_aOut =
            //
            // [ 0    0 ]
            // [ 0    0 ]
            // [ 0    0 ]
            // [ 0    0 ]
            Rinv_aOut.resize(4,2);
            Rinv_aOut = 0.0;
            return false;
        }
        bool doIndParVarianceInv_indPar( valarray<double>& Rinv_bOut ) const
        {
            // Rinv_bOut =
            //
            // [ -1/b(1)^2    0 ]
            // [    0         0 ]
            // [    0         0 ]
            // [ -1/b(1)^2    0 ]
            Rinv_bOut.resize(4 * 2);
            Rinv_bOut = 0.0;
            Rinv_bOut[0] = -1.0/(_b[0]*_b[0]);
            Rinv_bOut[3] = -1.0/(_b[0]*_b[0]);

            ret = Rinv_bOut;
            return !allZero(ret);
        }
        void doIndParVariance( valarray<double>& DOut ) const
        {
		    //DOut = 
		    //[ alp(1)^(-1) ,   0   ]
		    //[ 0    ,  alp(1)^(-1) ]
            DOut.resize(2 * 2);
            DOut = 0.0;
            DOut[0] = 1.0/_a[0];
            DOut[3] = 1.0/_a[0];
        }
        bool doIndParVariance_popPar( valarray<double>& D_alpOut ) const
        {
		    //D_alpOut = 
		    //[ -alp(1)^(-2) , 0 ]
		    //[ 0            , 0 ]
		    //[ 0            , 0 ]
		    //[ -alp(1)^(-2) , 0 ]
            D_alpOut.resize( 2 * 2 * 2 );
            D_alpOut = 0.0;
            D_alpOut[0] = -1.0/(_a[0]*_a[0]);
            D_alpOut[3] = -1.0/(_a[0]*_a[0]);
            return true;
        }
        void doIndParVarianceInv( valarray<double>& DinvOut ) const
        {
		    //DinvOut = 
		    //[ alp(1) ,            0   ]
		    //[ 0    ,           alp(1) ]
            DinvOut.resize(2 * 2);
            DinvOut[0] = _a[0];
            DinvOut[1] =  0.0;
            DinvOut[2] =  0.0;
            DinvOut[3] = _a[0];
        }
        bool doIndParVarianceInv_popPar( valarray<double>& Dinv_aOut ) const
        {
            // Dinv_aOut =
            // [ 1     0 ]
            // [ 0     0 ]
            // [ 0     0 ]
            // [ 1     0 ]
            Dinv_aOut.resize(2 * 2 * 2);
            Dinv_aOut = 0.0;
            Dinv_aOut[0] = 1;
            Dinv_aOut[3] = 1;
            return true;
        }
    };
    void expectedHessianTest::testExample()
    {
        using namespace std;
        valarray<double> alp(1.0, 2);
    
        valarray<double> b(1.0, 2);

        valarray<double> bStep(0.1, 2);

        exampleModel model;

        valarray<double> expectedHessianTilde(2 * 2);

        ExpectedHessian ehOb( &model, bStep );
        expectedHessianTilde = ehOb( alp, b );

        cout << "HtildeOut = [   " << endl;
        for( int i=0; i<expectedHessianTilde.size(); i++ )
           cout << expectedHessianTilde[i] << "   ";  
        cout << "]" << endl;
    }
$$
then it will display the following when it is run:
$codep

    HtildeOut = [   2   0   0   3   ]
$$
$end
*/
