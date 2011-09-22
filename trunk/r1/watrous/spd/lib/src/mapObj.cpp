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
 *
 * Evaluates the negative logarithm of the integrand that appears 
 * in the population expected determinant optimal design criterion.  
 *
 * Note that the implementation of mapObj has been modified for
 * use in the population optimal design system.
 *
 * Author: Mitch Watrous
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
  cg
  covariance
  df
  dx
  eps
  epsline
  mapobjVal
  Hessian
  Info
  itr
  Laplace
  mod
  mitr
  ndir
  ok
  opt
  pped
  ppka
  Ri
  Rval
  Std
  subvector
  const
  sqrt
  snormal
  seq
$$

$section Negative Logarithm of the Population Expected Determinant Criterion Integrand$$

$index mapObj$$
$cindex negative logarithm #of #the population expected determinant criterion integrand$$

$table
$bold Syntax:$$ $cend
$syntax/void mapObj( SpkModel&             /model/,
                const DoubleMatrix&   /dvecY/, 
                const DoubleMatrix&   /dvecAlp/,
                double*               /pdMapObjOut/,
                DoubleMatrix*         /pdrowMapObj_alpOut/,
                bool                  /withD/,
                bool                  /isFO/,
                const DoubleMatrix*   /pdvecN/ = NULL )
/$$
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

Evaluates the negative logarithm of the integrand that appears 
in the population expected determinant optimal design criterion.  
In particular, this function evaluates
$math%

                       %      -         %                      -
                       %     |          %                       |
    MapObj(x, alp) = - #log  |  p(alp)  #det[ HTilde(x, alp) ]  |  ,
                       %     |          %                       |
                       %      -         %                      -

%$$
where $math%p(alp)%$$ is the prior distribution of the
value of $math%alp%$$, $xref/HTilde//HTilde(x, alp)/$$ is an approximation for
the information matrix corresponding to the negative log-likelihood
of all of the data, 
$math%x%$$ is a vector of design parameters, 
and $math%alp%$$ is a vector of fixed population parameters.

The full design parameter vector $math%x%$$ contains the design vectors 
$math%x_i%$$ for all $math%M%$$ of the individuals in the population 
together with $math%x_common%$$, which is a vector of design 
parameters common to all of the individuals.
That is,
$math%
           -          -
          |  x_1       |
          |  x_2       |
          |   .        |
    x  =  |   .        |  .
          |   .        |
          |  x_M       |
          |  x_common  |
           -          -

%$$
The combination of parameters that each individuals' model functions will
depend on is denoted by
$math%
               -          -
    chi_i  =  |  x_i       |  .
              |  x_common  |
               -          -

%$$

The approximate information matrix $math%HTilde(x, alp)%$$ is defined as
$math%

                        M
                       ----   
    HTilde(x, alp)  =  >      HTilde_i(chi_i, alp)  ,
                       ----
                       i = 1

%$$
where the contribution from each individual is given by
$math%

    HTilde_i(chi_i, alp)

                                      T         -1            
        =  fTilde_i_alp(chi_i, alp, 0)  VTilde_i  (chi_i, alp)  fTilde_i_alp(chi_i, alp, 0)

           1                         T               -1                      -1             
        +  - VTilde_i_alp(chi_i, alp)  kron[ VTilde_i  (chi_i, alp), VTilde_i  (xTilde_i, alp) ] VTilde_i_alp(chi_i, alp)  ,
           2
%$$
and 
$math%

    VTilde_i(chi_i, alp)  =  RTilde_i(chi_i, alp)

                                                                       T
        +  fTilde_i_b(chi_i, alp, 0)  D(alp)  fTilde_i_b(chi_i, alp, 0)  .
%$$

$head Notes$$
(1.) This function makes the following first order approximation
for the model for the mean of the $th i$$ individual's data,
$math%

    fTilde_i(chi_i, alp, b_i)  =  f_i(chi_i, alp, 0)  +  f_i_b(chi_i, alp, 0) * b_i  ,

%$$
where $math%f_i_b(chi_i, alp, b_i)%$$ is the derivative of 
$math%f_i(chi_i, alp, b_i)%$$ with respect to the random effect 
vector $math%b_i%$$.
$pre

$$
(2.) This function makes the following approximation for the
model for the covariance of the $th i$$ individual's data,
$math%

    RTilde_i(chi_i, alp)  =  R_i(chi_i, alp, 0)  .
%$$
$pre

$$
(3.) This function requires the model function to provide the
following second derivatives of $math%f_i(chi_i, alp, b_i)%$$
$math%

  f_i_b_chi(chi_i, alp, b_i)  ,

  f_i_b_alp(chi_i, alp, b_i)  .
%$$
$pre

$$
(4.) The number of individuals in the population, $math%M%$$,
must be available from the model.
$pre

$$
(5.) The function assumes that the current value for the
vector of design parameters $math%x%$$ has been set 
for $italic model$$ before this function is called.


$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation,
119 (2001), pp. 57-73. 

$head Model Assumptions$$
The following model assumptions are stated using 
$xref/glossary/Population Notation/population notation/$$.
The bar above $math%chi_i%$$, $math%alp%$$, and $math%b_i%$$ denote 
the true, but unknown, values for the design parameters, 
the fixed population parameters, and the random population 
parameters for the $th i$$ individual, respectively.
$math%
             _____  ___   ___
  y_i = f_i( chi_i, alp , b_i ) + e_i
                  _____  ___  ___
  e_i ~ N[0, R_i( chi_i, alp, b_i)]
  ___          ___
  b_i ~ N[0, D(alp)]
%$$

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

/dvecY/
/$$
The double-precision column vector $italic y$$
contains the data vector.
$syntax/

/dvecAlp/
/$$
The double-precision column vector $italic alp$$
specifies the value for the fixed population parameter vector.

$syntax/

/pdMapObjOut/
/$$
is a pointer to a double-precision object or $code NULL$$.
User must allocate memory to $italic pdMapObjOut$$ when requesting
the negative logarithm of the integrand that appears 
in the population expected determinant optimal design criterion.  
If $code NULL$$ is given, 
the value will not be evaluated.
If not $code NULL$$, and if the evaluation completes successfully,
$italic pdMapObjOut$$ will point to a double-precision number 
containing the value.

$syntax/

/pdrowMapObj_alpOut/
/$$
is a pointer to a $code DoubleMatrix$$ or $code NULL$$.
User must allocate memory to $italic pdrowMapObj_alpOut$$, $math%1 by nB%$$ matrix object,
when requesting the value of the derivative with respect to the fixed 
population parameter $math%alp%$$ of 
the negative logarithm of the integrand that appears 
in the population expected determinant optimal design criterion.  
If $code NULL$$ is given, 
the value will not be evaluated.
If not $code NULL$$, and if the evaluation completes successfully,
$italic pdrowMapObj_alpOut$$ will point to a $math%1 by m%$$ matrix (row vector)  
containing the value of the derivative.

$syntax/

/withD/
/$$
is a boolean flag indicating as to whether the terms involving D (the variance of individuals parameters)
are to be included in $math%MapObj%$$.
If false is given, the D terms will be completely eliminated from the calculation.

$syntax/

/isFO/
/$$
This parameter is not used.

$syntax/

/pN/(null by default)
/$$ 
This parameter is not used.

$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPD include files.
#include "mapObj.h"
#include "hTilde.h"
#include "SpdModel.h"
#include "FoMapsParSpkModel.h"

// SPK include files.
#include "DoubleMatrix.h"
#include "det.h"
#include "inverse.h"
#include "rvec.h"
#include "transpose.h"
#include "multiply.h"
#include "mulByScalar.h"
#include "subtract.h"

// Standard include files.
#include <string>
#include <cassert>
#include <exception>


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

void mapObj( SpkModel&             model,
             const DoubleMatrix&   dvecY, 
             const DoubleMatrix&   dvecAlp,
             double*               pdMapObjOut,
             DoubleMatrix*         pdrowMapObj_alpOut,
             bool                  withD,
             bool                  isFO,
             const DoubleMatrix*   pdvecN )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // If no evaluation is requested, return immediately.
  if ( !pdMapObjOut && !pdrowMapObj_alpOut )
  {
    return;
  }

  // Get the number of fixed population parameters.
  int nAlp = dvecAlp.nr();


  //------------------------------------------------------------
  // Prepare the model.
  //------------------------------------------------------------

  // Use the cast operator to construct an SpkModel subclass 
  // that evaluates first order approximations for the mean
  // and covariance of the data, maps individual to population
  // parameters, and maps population to design parameters.
  FoMapsParSpkModel foMapsParSpkModel =
    dynamic_cast<FoMapsParSpkModel&>( model );

  // Get the SpdModel.
  SpdModel* pSpdModel = foMapsParSpkModel.getSpdModel();

  // Set the current fixed population parameter.
  pSpdModel->setPopPar( dvecAlp.toValarray() );


  //------------------------------------------------------------
  // Prepare the objects to hold the output values.
  //------------------------------------------------------------

  // This value is required to calculate the output values, 
  // initialize it so that it wil be calculated.
  DoubleMatrix dmatHTilde( nAlp, nAlp );
  DoubleMatrix* pdmatHTilde = &dmatHTilde;

  // If this value is required to calculate the output values, 
  // initialize it so that it wil be calculated.  Otherwise, 
  // set its pointer to zero so that it won't be calculated.
  DoubleMatrix dmatHTilde_alp;
  DoubleMatrix* pdmatHTilde_alp = &dmatHTilde_alp;
  if ( pdrowMapObj_alpOut )
  {
    dmatHTilde_alp.resize( nAlp * nAlp, nAlp );
  }
  else
  {
    pdmatHTilde_alp = 0;
  }

  DoubleMatrix* pdmatNull = 0;


  //------------------------------------------------------------
  // Prepare the remaining inputs to hTilde.
  //------------------------------------------------------------

  // [Revisit - Current Design Parameter in FoMapsParSpkModel - Mitch]
  // Because mapObj does not take the current design parameter value
  // as an argument, it must be made a member of the FoMapsParSpkModel
  // class.  In the future, when SPK is reorganized to accomodate
  // the optimal design system, then this parameter should be
  // taken out of FoMapsParSpkModel and made an explicit argument.
  //
  valarray<double> x;
  foMapsParSpkModel.getDesPar( x );
  DoubleMatrix dvecX( x, 1 );

  // Set the current design parameter.
  pSpdModel->setDesPar( x );

  // [Revisit - Population Parameter Step Size in FoMapsParSpkModel - Mitch]
  // Because mapObj does not take the population parameter step
  // size as an argument, it must be made a member of the FoMapsParSpkModel
  // class.  In the future, when SPK is reorganized to accomodate
  // the optimal design system, then this step size should be
  // taken out of FoMapsParSpkModel and made an explicit argument.
  //
  valarray<double> alpStep;
  foMapsParSpkModel.getPopParStep( alpStep );
  DoubleMatrix dvecAlpStep( alpStep, 1 );

  
  //------------------------------------------------------------
  // Compute the expected Hessian.
  //------------------------------------------------------------

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - Duplicate Code in mapObj and lambda - Mitch]
  // Reorganize lambda and mapObj so that lambda calls mapObj
  // (or vice versa) and the code here is not duplicated in
  // the other function.  The issues to consider are that mapObj
  // does not take the current design parameter as an input and
  // that currently lambda actually contains three functions
  // (lambda, lambda_alp, lambda_b) each of which calls hTilde
  // and if mapObj were to call lambda as it is implemented now,
  // then hTilde would be called twice as many times.
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  try
  {
    hTilde( 
      *pSpdModel,
      dvecX,
      dvecAlp,
      dvecAlpStep,
      pdmatHTilde,
      pdmatNull,
      pdmatHTilde_alp );
  }
  catch( SpkException& e )
  {
    throw e.push(
      SpkError::SPK_UNKNOWN_ERR,
      "The evaluation of the expected Hessian failed.",
      __LINE__,
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    throw SpkException(
      stde,
      "The evaluation of the expected Hessian failed.",
      __LINE__,
      __FILE__ );
  }
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR,
      "The evaluation of the expected Hessian failed.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Compute the negative logarithm of the integrand.
  //------------------------------------------------------------

  // Get the prior distribution of the fixed population 
  // parameters, p(alp).
  double prior;
  pSpdModel->popParPrior( prior );

  // Determine z and q such that
  //                                    q
  //     det[ HTilde(x, alp) ]  =  z * 2   .
  //
  // Note that the information matrix may not be positive
  // definite.  The function det() will throw an exception if
  // this is the case, but that exception is not handled here.
  double z;
  long int q;
  det( dmatHTilde, &z, &q );

  // If necessary, calculate and set the negative logarithm of the integrand that appears 
  // in the population expected determinant optimal design criterion,
  //
  //                            -                               -
  //                           |                                 |
  //    MapObj(x, alp) = - log |  p(alp)  det[ HTilde(x, alp) ]  |  .
  //                           |                                 |
  //                            -                               -
  //
  double mapObjTemp = - ( log( prior * z ) + q * log( 2.0 ) );


  //------------------------------------------------------------
  // Compute the derivative of the negative logarithm of the integrand.
  //------------------------------------------------------------

  DoubleMatrix drowMapObj_alpTemp;

  // Skip this if the derivative is not required.
  if( pdrowMapObj_alpOut )
  {
    // Get the prior distribution of the fixed population 
    // parameters, p(alp), and its derivative.
    double prior;
    pSpdModel->popParPrior( prior );
    valarray<double> prior_alp;
    pSpdModel->popParPrior_popPar( prior_alp );
    DoubleMatrix drowPrior_alp( prior_alp, nAlp );

    // Construct the derivative of the (positive) logarithm of
    // the determinant of the expected Hessian,
    //
    //                -                       -
    //               |                         |
    //     d     log |  det[ HTilde(x, alp) ]  |  
    //      alp      |                         |
    //                -                       -
    //
    //             -                          -  T
    //            |                      -1    |
    //         =  |  rvec[ HTilde(x, alp)   ]  |   * HTilde_alp(x, alp)  .
    //            |                            |
    //             -                          -
    //
    // This expression is based on Lemma 9, of B. M. Bell, 
    // "Approximating the marginal likelihood estimate for
    // models with random parameters", Applied Mathematics 
    // and Computation, 119 (2001), pp. 57-73, and the
    // fact that HTilde is symmetric.
    DoubleMatrix dmatHTildeInv;
    DoubleMatrix dmatHTildeInvRvec;
    DoubleMatrix dmatHTildeInvRvecTrans;
    DoubleMatrix drowPosLogDetHTilde_alp;
    dmatHTildeInv = inverse( dmatHTilde );
    dmatHTildeInvRvec = rvec( dmatHTildeInv );
    transpose( dmatHTildeInvRvec, dmatHTildeInvRvecTrans );
    multiply( dmatHTildeInvRvecTrans, dmatHTilde_alp, drowPosLogDetHTilde_alp );

    // Calculate the derivative of the negative logarithm of
    // the integrand that appears in the population expected
    // determinant optimal design criterion,
    //
    //     MapObj_alp(x, alp)  =  - p_alp(alp) / p(alp)
    //
    //                                         -                       -
    //                                        |                         |
    //                            - d     log |  det[ HTilde(x, alp) ]  |  
    //                               alp      |                         |
    //                                         -                       -
    //
    drowMapObj_alpTemp.resize( 1, nAlp );
    DoubleMatrix drowNegLogPrior_alp( 1, nAlp );
    mulByScalar( drowPrior_alp, -1.0 / prior, drowNegLogPrior_alp );
    subtract( drowNegLogPrior_alp, drowPosLogDetHTilde_alp, drowMapObj_alpTemp );
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Set the negative logarithm of the integrand value, if necessary.
  if ( pdMapObjOut )
  {
    *pdMapObjOut = mapObjTemp;
  }

  // Set the first derivative of the negative logarithm of the 
  // integrand value, if necessary.
  if ( pdrowMapObj_alpOut )
  {
    *pdrowMapObj_alpOut = drowMapObj_alpTemp;
  }

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


