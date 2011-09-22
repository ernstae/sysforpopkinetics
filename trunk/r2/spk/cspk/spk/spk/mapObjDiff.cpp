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
 * File: mapObjDiff.cpp
 *
 *
 * Evaluates central differences of the map Bayesian objective function and 
 * its derivative.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: mapObjDiff
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin mapObjDiff$$

$spell
  Model model const int bool Diff Bayesian centdiff Obj th
    endl cout pb fb dmat Rb pd std iostream namespace pfb ind dvec
    doSetIndPar int pf pb covariances Spk 
    covariance
    inv
    Ri
    valarray
    resize
  Fo
  pdvec
$$

$section Central Differences of Map Bayesian Objective Function and its Derivatives$$

$index mapObjDiff$$
$index individual, mapObj$$
$cindex central difference \of \the derivative \of map Bayesian objective function$$

$table
$bold Prototype:$$   $cend  
$syntax/void mapObjDiff(
          SpkModel<double>   & /model/,
          const DoubleMatrix & /dvecY/,
          const DoubleMatrix & /dvecBStep/,
          const DoubleMatrix & /dvecB/,
          DoubleMatrix       * /pMapObj_bOut/,
          DoubleMatrix       * /pMapObj_b_bOut/,
          bool                 /withD/,
          bool                 /isFO/,
          const DoubleMatrix * /pdvecN/ = NULL,
          const DoubleMatrix * /pdvecBMean/ = NULL
          )
          /$$
$tend

$table
$bold See also:$$ $cend
$tref mapObj$$ $rend
$cend
$tref centdiff$$, $rend
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
Compute central difference approximation of $xref/mapObj//the map Bayesian objective function/$$ 
and the derivatives of the approximation.  

$head Reference$$
$italic
Approximating The Maximum Likelihood Estimate For Models With Random Parameter
$$

$head Return Value$$
Upon a successful completion, the function returns normally and
set the given output value place holders to the result values (ones that are requested).
If a failure occurs during differentiations, a $xref/SpkError//SpkError/$$ object
is generated with SpkError::SPK_DIFF_ERR as its error code,
is appended to a $xref/SpkException//SpkException/$$ object and thrown.
The state at which an exception is thrown is defined in
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

/dvecY/
/$$
The $math%m%$$ dimensional column vector contains the individual's data.

$syntax/

/dvecBStep/
/$$
The $math%n%$$ dimensional column vector specifies the step sizes for 
approximating derivatives.  $math%i-th%$$ element of $italic dvecBStep$$
is the step size for $math%i-th%$$ element of 
the parameter vector, $italic b$$.  Every step size must be
greater than 0.0; otherwise the program terminates.

$syntax/

/dvecB/
/$$
The $math%n%$$ dimensional column vector is the parameter vector.

$syntax/

/pMapObj_bOut/
/$$
If this parameter is passed as NULL, no attempt is made to evaluate the 
central difference approximation of the map
Bayesian objective function.  If a pre-allocated $math%1 by n%$$ matrix
is given, upon a successful completion of the evaluation,
the pointer points to the resulting value.  If the evaluation
fails, an exception is thrown and the state of this pointer is
undefined.

$syntax/

/pMapObj_b_bOut/
/$$
If this parameter is passed as NULL, no attempt is made to evaluate the 
central difference approximation of the derivative of the map
Bayesian objective function with respect to the individual parameter.
If a pre-allocated $math%1 by n%$$ matrix is given, upon a successful
completion of the evaluation, the pointer points to the resulting value.
If the evaluation fails, an exception is thrown and the state of this pointer
is undefined.

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

/pdvecN/(null by default)
/$$ 
If $italic isFO$$ is specified as $math%true%$$, $italic pdvecN$$ points to a DoubleMatrix 
object that contains the column vector $math%N%$$.  The $th i$$ element of $math%N%$$
specifies the number of elements of $math%y%$$ that correspond to the $th i$$ individual.
If $italic isFO$$ is specified as $math%false%$$, set $italic N$$ to null.

$syntax/

/pdvecBMean/(null by default)
/$$ 
If the pointer $italic pdvecBMean$$ is not equal to null, then it points to a DoubleMatrix 
object that contains the column vector $math%bMean%$$.  The $th j$$ element of $math%bMean%$$
specifies the mean value for the $th j$$ element of $math%b%$$.
If the mean values for all of the elements of $math%b%$$ are equal to zero, 
set $italic pdvecBMean$$ to null.



$head Example$$
  
If you compile, link, and run the following program:
$codep
    #include <iostream>
    #include "DoubleMatrix.h"
    #include "SpkModel.h"
    #include "mapObj.h"
    #include "mapObjDiff.h"
    #include "identity.h"

    using namespace std;

    class UserModel : public SpkModel<double>
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
        void doDataMean( valarray<double>& f ) const
        {
            //
            // f(b) = [ b(2) ]
            //        [ b(2) ]
            //
           f.resize( 2 );
            double *pb = _b.data();
            dvecF.fill(pb[1]);
            ret = dvecF;
        }
        bool doDataMean_indPar( valarray<double>& f_b ) const
        {
            //
            // f(b)_b = [ 0  1 ]
            //          [ 0  1 ]
            //
            f_b.resize(2 * 2);
            f_b[0] = 0.0;
            f_b[1] = 0.0;
            f_b[2] = 1.0;
            f_b[3] = 1.0;
            return true;
        }
        void doDataVariance( valarray<double>& R ) const
        {
            //
            // R(b) = [ b(1)  0   ]
            //        [  0   b(1) ]
            //
            R.resize(2 * 2);
            R[0] = _b[0];
            R[1] =  0.0;
            R[2] =  0.0;
            R[3] = _b[0];
        }
        bool doDataVariance_indPar( valarray<double>& R_b ) const
        {
            //
            // R(b)_b = [ 1  0 ]
            //          [ 0  0 ]
            //          [ 0  0 ]
            //          [ 1  0 ]
            R_b.resize(4 * 2);
            R_b = 0.0;
            R_b[0] = 1.0;
            R_b[3] = 1.0;
            return true;
        }   
        void D( valarray<double>& D ) const
        {
            //
            // D = [ 1  0 ]
            //     [ 0  1 ]
            D.resize( 2 * 2 );
            D[0] = 1.0;
            D[1] = 0.0;
            D[2] = 0.0;
            D[3] = 1.0;
        }
    };

    void main()
    {

        SpkModel<double> model;
        int n = 2;
        int m = n;
        DoubleMatrix b(n,1);
        DoubleMatrix y(m,1);
        DoubleMatrix bStep(n,1);
        DoubleMatrix mapObj_bOut(1,n);
        DoubleMatrix mapObj_b_bOut(n,n);

        double *pdY = y.data();

        // Set b to a column vector:
        //  b = [ 1.0 ]
        //      [ 1.0 ]
        b.fill(1.0);

        // Set y to a column vector:
        //  y = [ 0.5 ]
        //      [ 0.3 ]
        pdY[0] = 0.5;
        pdY[1] = 0.3;


        // Set bStep to a matrix:
        //  bStep = [ 0.05 ]
        //          [ 0.05 ]
        bStep.fill(0.05);

        bool withD = true;  // indicating we provide D(alp).
        bool isFO  = false; 
        try{
            mapObjDiff( model, y, bStep, b, &mapObj_bOut, &mapObj_b_bOut, withD, isFO );
        }
        catch( ... )
        {
            cout << "The evaluation failed." << endl;
            abort();
        }
        cout << "mapObj_bOut = " << endl;
        mapObj_bOut.print();
        cout << endl;
        cout << "mapObj_b_Out = " << endl;
        mapObj_b_bOut.print();
    }

$$
then it will display the following when it is run:
$codep

    mapObj_bOut =
    [1.62991, 2.2]

    mapObj_b_Out =
    [0.741208, -1.2]
    [-1.20301, 3]

$$
$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#pragma warning( disable : 4786 )

#include <iostream>
#include "mapObj.h"
#include "mapObjDiff.h"
#include "centdiff.h"
#include "matmin.h"
#include "SpkModel.h"
#include "DoubleMatrix.h"
#include "SpkException.h"

namespace MapObjNamespace{
    typedef MapObj<DoubleMatrix>   MAPOBJ_PROTO;
    typedef MapObj_b<DoubleMatrix> MAPOBJ_B_PROTO;

};
static DoubleMatrix mapObj_bOutTemp(__FILE__);
static DoubleMatrix mapObj_b_bOutTemp(__FILE__);

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
void mapObjDiff(
                SpkModel<double>   &model,
                const DoubleMatrix &dvecY,
                const DoubleMatrix &dvecBStep,
                const DoubleMatrix &dvecB,
                DoubleMatrix *pMapObj_bOut,
                DoubleMatrix *pMapObj_b_bOut,
                bool withD,
                bool isFO,
                const DoubleMatrix *pdvecN,
                const DoubleMatrix *pdvecBMean
                )
{
    using namespace std;
    using namespace MapObjNamespace;

    int nB = dvecB.nr();
    int nY = dvecY.nr();

    // Check argument matrices dimensions
    assert( dvecY.nc() == 1 );
    assert( dvecB.nc() == 1 );
    assert( dvecBStep.nc() == 1 );

    assert( dvecBStep.nr() == nB );
  
    // Check output placeholders' dimensions
    if(pMapObj_bOut)
    {
        assert( pMapObj_bOut->nr()   == 1 );
        assert( pMapObj_bOut->nc()   == nB );
        mapObj_bOutTemp.resize( 1, nB );
    }
    if(pMapObj_b_bOut)
    {
        assert( pMapObj_b_bOut->nr() == nB );
        assert( pMapObj_b_bOut->nc() == nB );
        mapObj_b_bOutTemp.resize( nB, nB );
    }

    assert( matmin(dvecBStep) >= 0.0 );

    MAPOBJ_PROTO mapObjFuncOb(&model, dvecY, withD, isFO );
    if(pMapObj_bOut!=0)
    {
        try{
            mapObj_bOutTemp 
                = centdiff<MAPOBJ_PROTO>(mapObjFuncOb, dvecB, dvecBStep);
        }
        catch( SpkException& e )
        {
            throw e.push(SpkError::SPK_DIFF_ERR,
                         "An attempt to approximate the gradient.",
                          __LINE__,
                         __FILE__);
        }
        catch( const std::exception& e )
        {
            const int max = SpkError::maxMessageLen();
            char buf[max];
            snprintf( buf, max,
                      "A standard error was thrown during an attempt to approximate the gradient." );
            throw SpkException( e, buf, __LINE__, __FILE__ );
        }
        catch( ... )
        {
            throw SpkException(SpkError::SPK_DIFF_ERR,
                         "Unknown exception was thrown during an attempt to approximate the gradient.",
                         __LINE__,
                         __FILE__);
        }
    }
    MAPOBJ_B_PROTO mapObj_bDerivOb(&model, dvecY, withD, isFO, pdvecN, pdvecBMean);
    if(pMapObj_b_bOut!=0)
    {
        try{
            mapObj_b_bOutTemp 
                = centdiff<MAPOBJ_B_PROTO>(mapObj_bDerivOb, dvecB, dvecBStep);
        }
        catch( SpkException& e )
        {
            throw e.push(SpkError::SPK_DIFF_ERR,
                         "An attempt to approximate the hessian failed.",
                         __LINE__,
                         __FILE__);
        }
        catch( const std::exception& e )
        {
            const int max = SpkError::maxMessageLen();
            char buf[max];
            snprintf( buf, max, 
                      "A standard error was thrown during an attempt to approximate the hessian." );
            throw SpkException( e, buf, __LINE__, __FILE__ );
        }
        catch( ... )
        {
            throw SpkException(SpkError::SPK_DIFF_ERR,
                         "Unknown exception was thrown during an attempt to approximate the hessian.",
                         __LINE__,
                         __FILE__);
        }
    }

    // Set the results in parment places
    if( pMapObj_bOut )
        *pMapObj_bOut   = mapObj_bOutTemp;
    if( pMapObj_b_bOut )
        *pMapObj_b_bOut = mapObj_b_bOutTemp;
}
