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
 * File: lambda.cpp
 *
 *
 * Evaluates the negative logarithm of the integrand that appears 
 * in the population expected determinant optimal design criterion.  
 *
 * Note that the implementation of lambda has been modified for
 * use in the population optimal design system.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: lambda
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin lambda$$
$escape #$$
$spell
  cg
  covariance
  df
  dx
  eps
  epsline
  lambdaVal
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

$index Lambda$$
$cindex negative logarithm #of #the population expected determinant criterion integrand$$

$table
$bold Prototype:$$   $cend  
$syntax/double lambda( SpkModel&             /model/, 
               const DoubleMatrix&   /dvecY/, 
               const DoubleMatrix&   /dvecX/,
               const DoubleMatrix&   /dvecAlp/,
               bool                  /withD/ )
/$$
$tend

See also: $xref/lambda_alp//lambda_alp/$$, $xref/lambda_b//lambda_b/$$.
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
    Lambda(x, alp) = - #log  |  p(alp)  #det[ HTilde(x, alp) ]  |  ,
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

Upon a successful completion, the function returns the
$math%Lambda%$$ value as a double precision scalar.

If a failure occurs during the evaluation, an SpkException object is
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/
/model/
/$$
This function expects $italic model$$ to be 
$xref/glossary/Model Functions Depend on only b/a function of b/$$.
$syntax/

/dvecY/
/$$
The double-precision column vector $italic y$$
contains the data vector.
$syntax/

/dvecX/
/$$
The double-precision column vector $italic x$$
specifies the value for the design parameter vector.

$syntax/

/dvecAlp/
/$$
The double-precision column vector $italic alp$$
specifies the value for the fixed population parameter vector.
$syntax/

/includeD/
/$$
is a boolean flag indicating as to whether the terms involving D (the variance of individuals parameters)
are to be included in $math%Lambda%$$.

If false is given, the D terms will be completely eliminated from the calculation.

$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPD include files.
#include "lambda.h"
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

double lambda( SpkModel&             model, 
               const DoubleMatrix&   dvecY, 
               const DoubleMatrix&   dvecX,
               const DoubleMatrix&   dvecAlp,
               bool                  withD )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // Get the number of design and fixed population parameters.
  int nX   = dvecX.nr();
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

  // Set the state variables.
  pSpdModel->setDesPar( dvecX.toValarray() );
  pSpdModel->setPopPar( dvecAlp.toValarray() );


  //------------------------------------------------------------
  // Prepare the objects to hold the output values.
  //------------------------------------------------------------

  DoubleMatrix dmatHTilde( nAlp, nAlp );

  DoubleMatrix* pdmatNull = 0;


  //------------------------------------------------------------
  // Prepare the remaining inputs to hTilde.
  //------------------------------------------------------------

  // [Revisit - Population Parameter Step Size in FoMapsParSpkModel - Mitch]
  // Because lambda does not take the population parameter step
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
      &dmatHTilde,
      pdmatNull,
      pdmatNull );
  }
  catch( SpkException& e )
  {
    throw e.push(
      SpkError::SPK_UNKNOWN_ERR,
      "Evaluation of the population expected Hessian failed.",
      __LINE__,
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    throw SpkException(
      stde,
      "A standard exception occurred during the evaluation of the population expected Hessian.",
      __LINE__,
      __FILE__ );
  }
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR,
      "An unknown exception occurred during the evaluation of the population expected Hessian.",
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

  // Calculate the negative logarithm of the integrand that appears 
  // in the population expected determinant optimal design criterion,
  //
  //                            -                               -
  //                           |                                 |
  //    Lambda(x, alp) = - log |  p(alp)  det[ HTilde(x, alp) ]  |  .
  //                           |                                 |
  //                            -                               -
  //
  return - ( log( prior * z ) + q * log( 2.0 ) );
}

/*************************************************************************
 *
 * Function: lambda_alp (analytical derivative of lambda wrt alp)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin lambda_alp$$
$escape #$$

$spell
  cg
  covariance
  df
  dx
  eps
  epsline
  lambdaVal
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

$section Derivative of the Negative Logarithm of the Population Expected Determinant Criterion Integrand with Respect to the Design Parameter$$

$index Lambda_alp$$
$cindex derivative #of #the negative logarithm #of #the population expected determinant criterion integrand #with #respect #to #the design parameter$$

$table
$bold Prototype:$$   $cend  
$syntax/double lambda_alp( SpkModel&             /model/, 
                   const DoubleMatrix&   /dvecY/, 
                   const DoubleMatrix&   /dvecX/,
                   const DoubleMatrix&   /dvecAlp/,
                   bool                  /withD/ )
/$$
$tend

See also: $xref/lambda//lambda/$$, $xref/lambda_b//lambda_b/$$.
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Evaluates the derivative with respect to the design parameter
vector $math%x%$$ of the negative logarithm of the integrand that appears 
in the population expected determinant optimal design criterion.  
In particular, this function evaluates
$math%

    d   Lambda(x, alp)  ,
     x

%$$
where
$math%

                       %      -         %                      -
                       %     |          %                       |
    Lambda(x, alp) = - #log  |  p(alp)  #det[ HTilde(x, alp) ]  |  ,
                       %     |          %                       |
                       %      -         %                      -

%$$
$math%p(alp)%$$ is the prior distribution of the
value of $math%alp%$$, $xref/HTilde//HTilde(x, alp)/$$ is an approximation for
the information matrix corresponding to the negative log-likelihood
of all of the data, 
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
(4.) The number of individuals in the population, $math%M%$$, is 
should be available from the model.

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

Upon a successful completion, the function returns the
$math%Lambda%$$ value as a double precision scalar.

If a failure occurs during the evaluation, an SpkException object is
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/
/model/
/$$
This function expects $italic model$$ to be 
$xref/glossary/Model Functions Depend on only b/a function of b/$$.
$syntax/

/dvecY/
/$$
The double-precision column vector $italic y$$
contains the data vector.
$syntax/

/dvecX/
/$$
The double-precision column vector $italic x$$
specifies the value for the design parameter vector.

$syntax/

/dvecAlp/
/$$
The double-precision column vector $italic alp$$
specifies the value for the fixed population parameter vector.
$syntax/

/includeD/
/$$
is a boolean flag indicating as to whether the terms involving D (the variance of individuals parameters)
are to be included in $math%Lambda%$$.

If false is given, the D terms will be completely eliminated from the calculation.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

const DoubleMatrix lambda_alp( SpkModel&             model, 
                               const DoubleMatrix&   dvecY, 
                               const DoubleMatrix&   dvecX,
                               const DoubleMatrix&   dvecAlp,
                               bool                  withD )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // Get the number of design and fixed population parameters.
  int nX   = dvecX.nr();
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

  // Set the design and fixed population parameters.
  pSpdModel->setDesPar( dvecX.toValarray() );
  pSpdModel->setPopPar( dvecAlp.toValarray() );


  //------------------------------------------------------------
  // Prepare the objects to hold the output values.
  //------------------------------------------------------------

  DoubleMatrix dmatHTilde( nAlp, nAlp );
  DoubleMatrix dmatHTilde_x( nAlp * nAlp, nX );

  DoubleMatrix* pdmatNull = 0;


  //------------------------------------------------------------
  // Prepare the remaining inputs to hTilde.
  //------------------------------------------------------------

  // [Revisit - Population Parameter Step Size in FoMapsParSpkModel - Mitch]
  // Because lambda does not take the population parameter step
  // size as an argument, it must be made a member of the FoMapsParSpkModel
  // class.  In the future, when SPK is reorganized to accomodate
  // the optimal design system, then this step size should be
  // taken out of FoMapsParSpkModel and made an explicit argument.
  //
  valarray<double> alpStep;
  foMapsParSpkModel.getPopParStep( alpStep );
  DoubleMatrix dvecAlpStep( alpStep, 1 );

  
  //------------------------------------------------------------
  // Compute the derivative of the expected Hessian.
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
      &dmatHTilde,
      &dmatHTilde_x,
      pdmatNull );
  }
  catch( SpkException& e )
  {
    throw e.push(
      SpkError::SPK_UNKNOWN_ERR,
      "Evaluation of the population expected Hessian derivative failed.",
      __LINE__,
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    throw SpkException(
      stde,
      "A standard exception occurred during the evaluation of the population expected Hessian derivative.",
      __LINE__,
      __FILE__ );
  }
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR,
      "An unknown exception occurred during the evaluation of the population expected Hessian derivative.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Compute the derivative of the negative logarithm of the integrand.
  //------------------------------------------------------------

  // Construct the derivative of the (positive) logarithm of
  // the determinant of the expected Hessian,
  //
  //              -                       -
  //             |                         |
  //     d   log |  det[ HTilde(x, alp) ]  |  
  //      x      |                         |
  //              -                       -
  //
  //             -                          -  T
  //            |                      -1    |
  //         =  |  rvec[ HTilde(x, alp)   ]  |   * HTilde_x(x, alp)  .
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
  DoubleMatrix drowPosLogDetHTilde_x;
  dmatHTildeInv = inverse( dmatHTilde );
  dmatHTildeInvRvec = rvec( dmatHTildeInv );
  transpose( dmatHTildeInvRvec, dmatHTildeInvRvecTrans );
  multiply( dmatHTildeInvRvecTrans, dmatHTilde_x, drowPosLogDetHTilde_x );

  // Calculate the derivative of the negative logarithm of
  // the integrand that appears in the population expected
  // determinant optimal design criterion,
  //
  //                                     -                       -
  //                                    |                         |
  //     Lambda_x(x, alp)  =  - d   log |  det[ HTilde(x, alp) ]  |  
  //                             x      |                         |
  //                                     -                       -
  //
  DoubleMatrix drowLambda_x( 1, nX );
  mulByScalar( drowPosLogDetHTilde_x, -1.0, drowLambda_x );

  return drowLambda_x;
}
/*************************************************************************
 *
 * Function: lambda_b (analytical derivative of lambda wrt b)
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin lambda_b$$
$escape #$$

$spell
  cg
  covariance
  df
  dx
  eps
  epsline
  lambdaVal
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

$section Derivative of the Negative Logarithm of the Population Expected Determinant Criterion Integrand with Respect to the Fixed Population Parameter$$

$index Lambda_b$$
$cindex derivative #of #the negative logarithm #of #the population expected determinant criterion integrand #with #respect #to #the fixed population parameter$$

$table
$bold Prototype:$$   $cend  
$syntax/double lambda_b( SpkModel&             /model/, 
                 const DoubleMatrix&   /dvecY/, 
                 const DoubleMatrix&   /dvecX/,
                 const DoubleMatrix&   /dvecAlp/,
                 bool                  /withD/ )
/$$
$tend

See also: $xref/lambda//lambda/$$, $xref/lambda_alp//lambda_alp/$$.
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Evaluates the derivative with respect to the fixed population parameter
$math%alp%$$ of the negative logarithm of the integrand that appears 
in the population expected determinant optimal design criterion.  
In particular, this function evaluates
$math%

    d     Lambda(x, alp)  ,
     alp

%$$
where
$math%

                       %      -         %                      -
                       %     |          %                       |
    Lambda(x, alp) = - #log  |  p(alp)  #det[ HTilde(x, alp) ]  |  ,
                       %     |          %                       |
                       %      -         %                      -

%$$
$math%p(alp)%$$ is the prior distribution of the
value of $math%alp%$$, $xref/HTilde//HTilde(x, alp)/$$ is an approximation for
the information matrix corresponding to the negative log-likelihood
of all of the data, 
and $math%x%$$ is a vector of design parameters.

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
(4.) The number of individuals in the population, $math%M%$$, is 
should be available from the model.

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

Upon a successful completion, the function returns the
$math%Lambda%$$ value as a double precision scalar.

If a failure occurs during the evaluation, an SpkException object is
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/
/model/
/$$
This function expects $italic model$$ to be 
$xref/glossary/Model Functions Depend on only b/a function of b/$$.
$syntax/

/dvecY/
/$$
The double-precision column vector $italic y$$
contains the data vector.
$syntax/

/dvecX/
/$$
The double-precision column vector $italic x$$
specifies the value for the design parameter vector.

$syntax/

/dvecAlp/
/$$
The double-precision column vector $italic alp$$
specifies the value for the fixed population parameter vector.
$syntax/

/includeD/
/$$
is a boolean flag indicating as to whether the terms involving D (the variance of individuals parameters)
are to be included in $math%Lambda%$$.
If false is given, the D terms will be completely eliminated from the calculation.

$end
*/


/*------------------------------------------------------------------------
 * Function Definition
 *------------------------------------------------------------------------*/

const DoubleMatrix lambda_b( SpkModel&             model, 
                             const DoubleMatrix&   dvecY, 
                             const DoubleMatrix&   dvecX,
                             const DoubleMatrix&   dvecAlp,
                             bool                  withD )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // Get the number of design and fixed population parameters.
  int nX   = dvecX.nr();
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

  // Set the design and fixed population parameters.
  pSpdModel->setDesPar( dvecX.toValarray() );
  pSpdModel->setPopPar( dvecAlp.toValarray() );


  //------------------------------------------------------------
  // Prepare the objects to hold the output values.
  //------------------------------------------------------------

  DoubleMatrix dmatHTilde( nAlp, nAlp );
  DoubleMatrix dmatHTilde_alp( nAlp * nAlp, nAlp );

  DoubleMatrix* pdmatNull = 0;


  //------------------------------------------------------------
  // Prepare the remaining inputs to hTilde.
  //------------------------------------------------------------

  // [Revisit - Population Parameter Step Size in FoMapsParSpkModel - Mitch]
  // Because lambda does not take the population parameter step
  // size as an argument, it must be made a member of the FoMapsParSpkModel
  // class.  In the future, when SPK is reorganized to accomodate
  // the optimal design system, then this step size should be
  // taken out of FoMapsParSpkModel and made an explicit argument.
  //
  valarray<double> alpStep;
  foMapsParSpkModel.getPopParStep( alpStep );
  DoubleMatrix dvecAlpStep( alpStep, 1 );

  
  //------------------------------------------------------------
  // Compute the derivative of the expected Hessian.
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
      &dmatHTilde,
      pdmatNull,
      &dmatHTilde_alp );
  }
  catch( SpkException& e )
  {
    throw e.push(
      SpkError::SPK_UNKNOWN_ERR,
      "Evaluation of the population expected Hessian derivative failed.",
      __LINE__,
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    throw SpkException(
      stde,
      "A standard exception occurred during the evaluation of the population expected Hessian derivative.",
      __LINE__,
      __FILE__ );
  }
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR,
      "An unknown exception occurred during the evaluation of the population expected Hessian derivative.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Compute the derivative of the negative logarithm of the integrand.
  //------------------------------------------------------------

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
  //     Lambda_alp(x, alp)  =  - p_alp(alp) / p(alp)
  //
  //                                         -                       -
  //                                        |                         |
  //                            - d     log |  det[ HTilde(x, alp) ]  |  
  //                               alp      |                         |
  //                                         -                       -
  //
  DoubleMatrix drowLambda_alp( 1, nAlp );
  DoubleMatrix drowNegLogPrior_alp( 1, nAlp );
  mulByScalar( drowPrior_alp, -1.0 / prior, drowNegLogPrior_alp );
  subtract( drowNegLogPrior_alp, drowPosLogDetHTilde_alp, drowLambda_alp );

  return drowLambda_alp;
}

/*************************************************************************
 *
 * Class: Lambda
 *
 *************************************************************************/
/*
$begin lambdaFuncOb$$
$spell
        Model model  
    dmat dtemp iostream std namespace pow approx ind int pop cout endl
    nc nr const bool pd centdiff ob typedef inv arg res
    dvec D_a pdb pda Dinv
    cfloat cmath spk fs aval bval inx covariances
    valarray
    
$$
$section Function Objects for lambda(alp,b) and Its True Derivatives$$

$index function objects, lambda$$
$index lambda, function object$$

$table
$bold Lambda:$$     $cend 
$syntax/template <class /Model/> class Lambda 
: public std::binary_function<DoubleMatrix, DoubleMatrix, DoubleMatrix>/$$ $rend
Constructor:        $cend
$syntax/Lambda</Model/>::Lambda(Model* /model/, const DoubleMatrix& /y/, bool /includeD/)/$$ $rend

$bold Lambda_alp:$$ $cend
$syntax/template <class /Model/> class Lambda_alp 
: public std::binary_function<DoubleMatrix, DoubleMatrix, DoubleMatrix>/$$ $rend
Constructor:        $cend
$syntax/Lambda</Model/>::Lambda_alp(Model* /model/, const DoubleMatrix& /y/, bool /includeD/)/$$ $rend

$bold Lambda_b:$$   $cend
$syntax/template <class /Model/> class Lambda_b 
: public std::binary_function<DoubleMatrix, DoubleMatrix, DoubleMatrix>/$$ $rend
Constructor:        $cend
$syntax/Lambda</Model/>::Lambda_b(Model* /model/, const DoubleMatrix& /y/, bool /includeD/)/$$
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
$code Lambda$$, $code Lambda_alp$$ and $code Lambda_b$$ classes wrap $xref/lambda//lambda()/$$, 
$xref/lambda_alp//lambda_alp()/$$ and $xref/lambda_b//lambda_b()/$$ functions, respectively, 
and allows user to call/evaluate the functions through $code operator()$$
in the form of a binary function.

$head Arguments$$
$syntax/
/Model/
/$$
is the name of a user-provided model derived from SpkModel base class.
$syntax/

/model/
/$$
is a pointer to a population level user-provided model.
$syntax/

/y/
/$$
is a column vector containing measurements.
$syntax/

/includeD/
/$$
should be true if you have implemented $code doIndParVariance()$$ in your user-provided model.

$head Public Members$$
$syntax/const DoubleMatrix operator(const DoubleMatrix /x1/, const DoubleMatrix /x2/) const
/$$
This member function evaluates the function associate with the object; $xref/lambda//lambda()/$$,
$xref/lambda_alp//lambda_alp/$$ or $xref/lambda_b//lambda_b()/$$
at the evaluation points specified by the $italic x1$$ and $italic x2$$.
It returns a n dimensional row vector, where n is the size of $italic x1$$ or $italic x2$$.

$head Example$$

The following piece of code demonstrates how to pass a function object to centdiff() algorithm as an example:

$codep
    #include <iostream>
    #include <cfloat>
    #include <cmath>

    #include "SpkValarray.h"
    #include "DoubleMatrix.h"
    #include "centdiff.h"
    #include "lambda.h"
    #include "inverse.h"
    #include "allZero.h"
    #include "SpkValarray.h"

    class LambdaCentdiffTestModel : public SpkModel
    {
        DoubleMatrix _a, _b;
        int _i;
    public:
        LambdaCentdiffTestModel(){};    
        ~LambdaCentdiffTestModel(){};
    protected:
        void doSelectIndividual(int inx)
        {
            _i = inx;
        }
        void doSetPopPar(const valarray<double>& a)
        {
            _a = DoubleMatrix( a, 1 );
        }
        void doSetIndPar(const valarray<double>& b)
        {
            _b = DoubleMatrix( b, 1 );
        }

        void doIndParVariance( valarray<double>& ret ) const
        {
            //
            // D = [a(0)   0   ]
            //     [   0   a(0)]
            //
            DoubleMatrix dmatD(2,2);
            double *pdD = dmatD.data();
            const double *pda = _a.data();

            pdD[0] = pda[0];
            pdD[1] = 0.0;
            pdD[2] = 0.0;
            pdD[3] = pda[0];
            ret = dmatD.toValarray();
        }
        bool doIndParVariance_popPar( valarray<double>& ret ) const
        {
            //
            // D(a)a = [ 1  0 ]
            //         [ 0  0 ]
            //         [ 0  0 ]
            //         [ 1  0 ]
            //
            DoubleMatrix dmatD = doIndParVariance(ret);
            DoubleMatrix dmatD_a( dmatD.nr()*dmatD.nc(), _a.nr() );
            double *pdD_a = dmatD_a.data();
            dmatD_a.fill(0.0);

            pdD_a[0] = 1.0;
            pdD_a[3] = 1.0;
            ret = dmatD_a.toValarray();
        }
        void doIndParVarianceInv( valarray<double>& ret ) const
        {
            doIndParVariance(ret);
            ret = inverse( ret );
        }
        bool doIndParVarianceInv_a( valarray<double>& ret ) const
        {
            DoubleMatrix dmatD;
            Dinv(dmatD);
            DoubleMatrix dmatD_a( dmatD.nr()*dmatD.nc(), _a.nr() );
            double *pdD_a = dmatD_a.data();
            dmatD_a.fill(0.0);

            pdD_a[0] = 1.0;
            pdD_a[3] = 1.0;
            ret = dmatD_a.toValarray();

            return !allZero(ret);
        }
        void doDataMean_popPar( valarray<double>& ret ) const
        {
            //
            // f(a, b) = [ a(1) + b(1) ]
            //           [ a(1) + b(1) ]
            //

            DoubleMatrix dmatF(2,1);
            const double *pda = _a.data();
            const double *pdb = _b.data();

            dmatF.fill( pda[1] + pdb[1] );
            ret = dmatF.toValarray();
        }
        bool doDataMean_popPar( valarray<double>& ret ) const
        {
            //
            // f(a, b)_a = [ 0  1 ]
            //             [ 0  1 ]
            //
            DoubleMatrix dvecF = doDataMean_popPar();
            DoubleMatrix dmatF_a(dvecF.nr(), _a.nr());
            double *pdF_a = dmatF_a.data();

            pdF_a[0] = 0.0;
            pdF_a[1] = 0.0;
            pdF_a[2] = 1.0;
            pdF_a[3] = 1.0;
            ret = dmatF_a.toValarray();

            return !allZero(ret);

        }
        bool doDataMean_indPar( valarray<double>& ret ) const
        {
            //
            // f(a, b)_b = [ 0  1 ]
            //             [ 0  1 ]
            //
            DoubleMatrix dvecF;
            doDataMean_popPar(dvecF);
            DoubleMatrix dmatF_b(dvecF.nr(), _b.nr());
            double *pdF_b = dmatF_b.data();

            pdF_b[0] = 0.0;
            pdF_b[1] = 0.0;
            pdF_b[2] = 1.0;
            pdF_b[3] = 1.0;
            ret = dmatF_b.toValarray();

            return !allZero(ret);

        }
        void doDataVariance( valarray<double>& ret ) const
        {
            //
            // R(a, b) = [ b(0)  0   ]
            //           [  0   b(0) ]
            //
            DoubleMatrix dmatR(2,2);
            double *pdR = dmatR.data();
                const double *pdb = _b.data();

            pdR[0] = pdb[0];
            pdR[1] = 0.0;
            pdR[2] = 0.0;
            pdR[3] = pdb[0];
            ret = dmatR.toValarray();
        }
        bool doDataVariance_popPar( valarray<double>& ret ) const
        {
            //
            // R_a = [ 0  0 ]
            //       [ 0  0 ]
            //       [ 0  0 ]
            //       [ 0  0 ]
            //
            valarray<double> zeros(0.0, 4 * 2);
            ret = zeros;
            return !allZero(ret);
        }
        bool doDataVariance_indPar( valarray<double>& ret ) const
        {
            //
            // R_b   = [ 1  0 ]
            //         [ 0  0 ]
            //         [ 0  0 ]
            //         [ 1  0 ]
            //
            DoubleMatrix dmatR;
            doDataVariance(ret);
            DoubleMatrix dmatR_b(dmatR.nr()*dmatR.nc(), _b.nr());
            double *pdR_b = dmatR_b.data();
            dmatR_b.fill(0.0);
            pdR_b[0] = 1.0;
            pdR_b[3] = 1.0;
            ret = dmatR_b.toValarray();

            return !allZero(ret);
        } 
        void doDataVarianceInv( valarray<double>& ret ) const
        {
            doDataVariance(ret);
            ret = inverse(ret).toValarray();
        }
        bool doDataVarianceInv_indPar( valarray<double>& ret ) const
        {
            DoubleMatrix dmatR;
            doDataVarianceInv(dmatR);
            DoubleMatrix dmatR_b(dmatR.nr()*dmatR.nc(), _b.nr());
            double *pdR_b = dmatR_b.data();
            dmatR_b.fill(0.0);
            pdR_b[0] = 1.0;
            pdR_b[3] = 1.0;
            ret = dmatR_b.toValarray();
            return !allZero(ret);
        }
        bool doDataVarianceInv_popPar( valarray<double>& ret ) const
        {
            DoubleMatrix dmatR;
            doDataVarianceInv(dmatR);
            DoubleMatrix dmatR_b(dmatR.nr()*dmatR.nc(), _b.nr());
            double *pdR_b = dmatR_b.data();
            dmatR_b.fill(0.0);
            pdR_b[0] = 1.0;
            pdR_b[3] = 1.0;
            ret = dmatR_b.toValarray();
            return !allZero(ret);
        }

    };

    void lambdaCentdiffTest()
    {
        using namespace std;

        // Use n=2 for all alp, b and y, though they could have all different sizes
        int n = 2;

        // measurement vector
        DoubleMatrix y(n,1);
        y.fill(1);

        // population parameter
        DoubleMatrix pop(n,1);
        pop.fill(1);
        double *pdPop     = pop.data();

        // individual parameter
        DoubleMatrix ind(n,1);
        ind.fill(1);
        double *pdInd     = ind.data();

        // place holder for approximation for lambda(pop)_pop
        DoubleMatrix approxLambda_popOut;
        DoubleMatrix exactLambda_popOut;


        // step size vectors
        DoubleMatrix popStep(n,1);
        DoubleMatrix indStep(n,1);
        double *pdPopStep = popStep.data();
        double *pdIndStep = indStep.data();
        double dtemp;

        for( int i=0; i<n; i++){

            // set the step values for the population parameter
            dtemp = pow( DBL_EPSILON / (2.0/6.0), 1/3.0) + pdAlp[i];
            pdPopStep[i] = 10.0 * ( dtemp - pdPop[i] );

            // set zeros to the individual parameters so that
            // it is fixed.
            pdIndStep[i] = 0;
        }

        typedef Lambda<LambdaCentdiffTestModel> LAMBDA_PROTO;
        LambdaCentdiffTestModel model;
        
        lambda(model,y,pop,ind,0,&exactLambda_popOut,0,true);


        // Creating a Lambda function object to pass it to centdiff() algorithm
        LAMBDA_PROTO lambdaOb(&model, y, true);
        DoubleMatrix zeros(ind.nr(), ind.nc());       zeros.fill(0);
        approxLambda_popOut = centdiff_alp<LAMBDA_PROTO>(lambdaOb, pop, ind, popStep);
        cout << "approx = " << approxLambda_popOut << endl;
        cout << "exact  = " << exactLambda_popOut  << endl;
    }

$$
the program will display;
$codep

    approx = 1 by 2
    [ -5.0849075472330428e-009 2.0000000000000000e+000 ]

    exact  = 1 by 2
    [ 0.0000000000000000e+000 2.0000000000000000e+000 ]


$$
$end
*/

