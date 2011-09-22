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
 * File: ppedOpt.cpp
 *
 *
 * Optimizes the modified Laplace approximation for the population
 * expected determinant optimal design criterion.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: ppedOpt
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*

$begin ppedOpt$$

$escape #$$

$spell
  cg
  covariance
  df
  dx
  eps
  epsline
  fval
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

$section Optimization of the Population Expected Determinant Criterion$$

$index ppedOpt$$
$cindex optimization #of #the population expected determinant criterion$$

$table
$bold Syntax:$$ $cend
$syntax/void ppedOpt( SpdModel&             /model/,
              Optimizer&            /xOptInfo/,
              const DoubleMatrix&   /dvecXLow/,
              const DoubleMatrix&   /dvecXUp/,
              const DoubleMatrix&   /dvecXIn/,
              DoubleMatrix*         /pdvecXOut/,
              const DoubleMatrix&   /dvecXStep/,
              Optimizer&            /alpOptInfo/,
              const DoubleMatrix&   /dvecAlpLow/,
              const DoubleMatrix&   /dvecAlpUp/,
              const DoubleMatrix&   /dvecAlpIn/,
              DoubleMatrix*         /pdvecAlpOut/,
              const DoubleMatrix&   /dvecAlpStep/,
              double*               /pdPhiTildeOut/,
              DoubleMatrix*         /pdrowPhiTilde_xOut/,
              DoubleMatrix*         /pdmatPhiTilde_x_xOut/ )
/$$

$tend

$fend 25$$

$head Description$$
Determines the design vector $math%x%$$ that
maximizes the modified Laplace approximation for
the parametric population expected determinant criterion
$math%

                     +infinity
                    /\
                    \                      
    phiTilde(x)  =   \        p(alp) #det[ HTilde(x, alp) ] dalp  .
                    \/
                     -infinity

%$$
In this expression, $math%p(alp)%$$ is the prior distribution of the
value of $math%alp%$$, $xref/HTilde//HTilde(x, alp)/$$ is an approximation for
the information matrix corresponding to the negative log-likelihood
of all of the data, 
and $math%alp%$$ is the value for the fixed population parameter vector.
$pre

$$
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
replacing the derivatives with respect to the fixed population 
parameter $math%alp%$$ by central difference approximations
in the definition of
$math%

    HTilde_i(chi_i, alp)

                                      T         -1            
        =  fTilde_i_alp(chi_i, alp, 0)  VTilde_i  (chi_i, alp)  fTilde_i_alp(chi_i, alp, 0)

           1                         T               -1                      -1             
        +  - VTilde_i_alp(chi_i, alp)  kron[ VTilde_i  (chi_i, alp), VTilde_i  (xTilde_i, alp) ] VTilde_i_alp(chi_i, alp)  ,
           2
%$$
and where
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

$head Return Values$$
This function returns normally and sets the given output
value place holders if it is able to obtain an acceptable
estimate for $math%xHat%$$, the true maximizer of 
the Laplace approximation for the population optimal design criterion.
Note that the situation where the maximum number of
iterations is reached is not considered a successful completion. 
$pre

$$
If an error is detected or failure occurs during the evaluation, 
an SpkException object is thrown.  
The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax//Model/(/i/, /chi/, /alp/, /b/, /fOut/, /f_chiOut/, /f_alpOut/, /f_bOut/, /f_b_chiOut/, /f_b_alpOut/, 
    /ROut/, /R_chiOut/, /R_alpOut/, /R_bOut/, /DOut/, /D_alpOut/)
/$$
The return value of $italic Model$$ is true if it succeeds and false otherwise.
All the arguments to this routine have real or double-precision values. In addition,
the arguments that end in $italic Out$$ are $xref/glossary/Output Value/output values/$$.
$pre

$$
$center
$table
$bold Argument$$
  $cend $bold Value$$ 
  $cend $bold Comments$$ $rend
$italic i$$
  $cend $math%i%$$
  $cend index for this individual $rend
$italic chi$$
  $cend $math%chi_i%$$
  $cend design parameter vector for this individual combined with the
          design parameter vector common to all individuals $rend
$italic alp$$
  $cend $math%alp%$$
  $cend fixed population parameter vector $rend
$italic b$$
  $cend $math%b_i%$$
  $cend random population parameter vector for this individual $rend
$italic fOut$$
  $cend $math%f_i(chi_i, alp, b_i)%$$ 
  $cend model for the mean of $math%y_i%$$ given $math%b_i%$$ $rend
$italic f_chiOut$$ 
  $cend $math%f_i_chi(chi_i, alp, b_i)%$$
  $cend derivative of $math%f_i(chi_i, alp, b_i)%$$ with respect to $math%chi_i%$$ $rend
$italic f_alpOut$$ 
  $cend $math%f_i_alp(chi_i, alp, b_i)%$$ 
  $cend derivative of $math%f_i(chi_i, alp, b_i)%$$ with respect to $math%alp%$$ $rend
$italic f_bOut$$ 
  $cend $math%f_i_b(chi_i, alp, b_i)%$$ 
  $cend derivative of $math%f_i(chi_i, alp, b_i)%$$ with respect to $math%b_i%$$ $rend
$italic f_b_chiOut$$ 
  $cend $math%f_i_b_chi(chi_i, alp, b_i)%$$ 
  $cend derivative of $math%f_i_b(chi_i, alp, b_i)%$$ with respect to $math%chi_i%$$ $rend
$italic f_b_alpOut$$ 
  $cend $math%f_i_b_alp(chi_i, alp, b_i)%$$ 
  $cend derivative of $math%f_i_b(chi_i, alp, b_i)%$$ with respect to $math%alp%$$ $rend
$italic ROut$$ 
  $cend $math%R_i(chi_i, alp, b_i)%$$ 
  $cend model for the variance of $math%y_i%$$ given $math%b_i%$$ $rend
$italic R_chiOut$$ 
  $cend $math%R_i_chi(chi_i, alp, b_i)%$$
  $cend derivative of $math%R_i(chi_i, alp, b_i)%$$ with respect to $math%chi_i%$$ $rend
$italic R_alpOut$$
  $cend $math%R_i_alp(chi_i, alp, b_i)%$$ 
  $cend derivative of $math%R_i(chi_i, alp, b_i)%$$ with respect to $math%alp%$$ $rend
$italic R_bOut$$
  $cend $math%R_i_b(chi_i, alp, b_i)%$$ 
  $cend derivative of $math%R_i(chi_i, alp, b_i)%$$ with respect to $math%b_i%$$ $rend
$italic DOut$$ 
  $cend $math%D(alp)%$$ 
  $cend model for the variance of $math%b_i%$$ $rend
$italic D_alpOut$$
  $cend $math%D_alp(alp)%$$ 
  $cend derivative of $math%D(alp)%$$ with respect to $math%alp%$$
$tend
$$

$syntax/

/xOptInfo/
/$$
This $xref/Optimizer//Optimizer/$$ class object has three attributes.  
These attributes are parameters of the optimizer during in the design 
parameter optimization.  It has other attributes for handling running out of 
maximum iterations and for holding the optimization state information 
that is required by later restart(warm start) run.  
For the current implementation, the restart capabilities have been
disabled.

A design parameter value $math%xOut%$$ is 
accepted as close enough to optimal if 
$math%
        abs(xOut - xHat) #le eps (xUp - xLow)
%$$
where $math%abs%$$ is the element-by-element absolute value function,
$math%eps%$$ is the value returned by the function
$italic xOptInfo$$ $code .getEpsilon()$$, 
and $math%xHat%$$ is the true maximizer of the population expected determinant optimal 
design criterion.
This is a rough approximation that is quick to
calculate during the optimization procedure.

$syntax/

/dvecXLow/
/$$
The $code DoubleMatrix$$ $italic dvecXLow$$ contains the column vector 
$math%xLow%$$, which specifies the lower limit for $math%x%$$ during 
the design parameter optimization procedure.
The length of $italic dvecXLow$$ is equal to the length of 
dvecXIn$$.
$syntax/

/dvecXUp/
/$$
The $code DoubleMatrix$$ $italic dvecXUp$$ contains the column vector 
$math%xUp%$$, which specifies the upper limit for $math%x%$$ during 
the design parameter optimization procedure.
The length of $italic dvecXUp$$ is equal to the length of 
dvecXIn$$.
$syntax/

/dvecXIn/
/$$
The $code DoubleMatrix$$ $italic dvecXIn$$ contains the 
column vector $math%xIn%$$, which specifies the initial value
for the design parameters
The $xref/glossary/Ordering Of Vectors/order condition/$$
$math%xLow \le xIn \le xUp%$$ is assumed to hold.
Note that the length of $italic dvecXIn$$ specifies the length of 
the design parameter vector $math%x%$$.
$syntax/

/pdvecXOut/
/$$
If $italic pdvecXOut$$ is not $code NULL$$, then the 
$code DoubleMatrix$$ pointed to by $italic pdvecXOut$$ must 
be declared in the function that calls this function, and it 
must have the same dimensions as $italic dvecXIn$$.
If $italic pdvecXOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully,  
then the $code DoubleMatrix$$ pointed to by $italic pdvecXOut$$ 
will contain the column vector $math%xOut%$$, which is the 
approximate maximizer of the population expected determinant optimal design criterion.
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdvecXOut$$.
$syntax/

/dvecXStep/
/$$
The $code DoubleMatrix$$ $italic dvecXStep$$ contains the 
column vector $math%xStep%$$, which specifies the step size 
used for approximating the derivatives with respect to the
design parameters.
The value of this parameter does not matter if
$italic pdmatPhiTilde_x_xOut$$ is $code NULL$$.
The length of $italic dvecXStep$$ is equal to the length of 
dvecXIn$$.
$syntax/

/alpOptInfo/
/$$
This $xref/Optimizer//Optimizer/$$ class object has three attributes.  
These attributes are parameters of the optimizer used during the fixed population 
parameter optimization.  It has other attributes for handling running out of 
maximum iterations and for holding the optimization state information 
that is required by later restart(warm start) run.  
For the current implementation, the restart capabilities have been
disabled.

A fixed population parameter value $math%alpOut%$$ is 
accepted as close enough to optimal if 
$math%
        abs(alpOut - alpHat) #le eps (alpUp - alpLow)
%$$
where $math%abs%$$ is the element-by-element absolute value function,
$math%eps%$$ is the value returned by the function
$italic alpOptInfo$$ $code .getEpsilon()$$, 
and $math%alpHat%$$ is the true minimizer of 
$xref/Lambda//Lambda(x, alp)/$$, 
the negative logarithm of the integrand that appears in the 
population expected determinant optimal design criterion.
This is a rough approximation that is quick to
calculate during the optimization procedure.

$syntax/

/dvecAlpLow/
/$$
The $code DoubleMatrix$$ $italic dvecAlpLow$$ contains the column vector 
$math%alpLow%$$, which specifies the lower limit for $math%alp%$$ during 
the fixed population parameter optimization procedure.
The length of $italic dvecAlpLow$$ is equal to the length of 
dvecAlpIn$$.
$syntax/

/dvecAlpUp/
/$$
The $code DoubleMatrix$$ $italic dvecAlpUp$$ contains the column vector 
$math%alpUp%$$, which specifies the upper limit for $math%alp%$$ during 
the fixed population parameter optimization procedure.
The length of $italic dvecAlpUp$$ is equal to the length of 
dvecAlpIn$$.
$syntax/

/dvecAlpIn/
/$$
The $code DoubleMatrix$$ $italic dvecAlpIn$$ contains the 
column vector $math%alpIn%$$, which specifies the initial value
for the fixed population parameters.
The $xref/glossary/Ordering Of Vectors/order condition/$$
$math%alpLow \le alpIn \le alpUp%$$ is assumed to hold.
Note that the length of $italic dvecAlpIn$$ specifies the length of 
the fixed population parameter vector $math%alp%$$.
$syntax/

/pdvecAlpOut/
/$$
If $italic pdvecAlpOut$$ is not $code NULL$$, then the 
$code DoubleMatrix$$ pointed to by $italic pdvecAlpOut$$ must 
be declared in the function that calls this function, and it 
must have the same dimensions as $italic dvecAlpIn$$.
If $italic pdvecAlpOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully,  
then the $code DoubleMatrix$$ pointed to by $italic pdvecAlpOut$$ 
will contain the column vector $math%alpOut%$$, which is the 
approximate minimizer of $xref/Lambda//Lambda(x, alp)/$$, 
the negative logarithm of the integrand that appears in the 
population expected determinant optimal design criterion.
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdvecAlpOut$$.
$syntax/

/dvecAlpStep/
/$$
The $code DoubleMatrix$$ $italic dvecAlpStep$$ contains the 
column vector $math%alpStep%$$, which specifies the step size 
used for approximating the derivatives with respect to the
fixed population parameters.
The length of $italic dvecAlpStep$$ is equal to the length of 
dvecAlp$$.
$syntax/

/pdPhiTildeOut/
/$$
If $italic pdPhiTildeOut is not $code NULL$$, then the 
$code double$$ pointed to by $italic pdPhiTildeOut$$ must 
be declared in the function that calls this function.
If $italic pdPhiTildeOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully,  
then the $code double$$ pointed to by $italic pdPhiTildeOut$$ 
will be equal to $math%phiTilde(xOut)%$$, which is the 
value of the population expected determinant optimal design criterion evaluated 
at $math%xOut%$$.
Otherwise, this function will not attempt to change the contents of the 
$code double$$ pointed to by $italic pdPhiTildeOut$$.
$syntax/

/pdrowPhiTilde_xOut/
/$$
If $italic pdPhiTilde_xOut is not $code NULL$$, then the 
$code DoubleMatrix$$ pointed to by $italic pdPhiTilde_xOut$$ must 
be declared in the function that calls this function, and it 
must be a row vector that is the same length as $italic dvecXIn$$.
If $italic pdPhiTilde_xOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully,  
then the $code DoubleMatrix$$ pointed to by $italic pdPhiTilde_xOut$$ 
will contain the row vector $math%phiTilde_x(xOut)%$$, which is the 
derivative of the population expected determinant optimal design criterion with 
respect to $math%x%$$ evaluated at $math%xOut%$$.
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdPhiTilde_xOut$$.
$syntax/

/dmatPhiTilde_x_xOut/ 
/$$
If $italic pdPhiTilde_x_xOut is not $code NULL$$, then the 
$code DoubleMatrix$$ pointed to by $italic pdPhiTilde_x_xOut$$ must 
be declared in the function that calls this function, and it 
must be a square matrix with the same number of rows and 
columns as the length as $italic dvecXIn$$.
If $italic pdPhiTilde_x_xOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully,  
then the $code DoubleMatrix$$ pointed to by $italic pdPhiTilde_x_xOut$$ 
will contain the matrix $math%phiTilde_x_x(xOut)%$$, which is 
an approximation for the Hessian of the population expected determinant optimal 
design criterion with respect to $math%x%$$ evaluated at $math%xOut%$$.
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdPhiTilde_x_xOut$$.

$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPD include files.
#include "ppedOpt.h"
#include "SpdModel.h"
#include "FoMapsParSpkModel.h"

// SPK include files.
#include "DoubleMatrix.h"
#include "Optimizer.h"
#include "ppkaOpt.h"
#include "multiply.h"
#include "mulByScalar.h"
#include "subtract.h"
#include "transpose.h"

// Standard include files.
#include <string>
#include <cassert>
#include <exception>
#include <cmath>


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// [Revisit - Number of Individuals Specified by Model - Mitch]
//
// The specifications for this function say that the number 
// of individuals should be available from the model.  This
// should be made explicit in the model specification.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

void ppedOpt( SpdModel&             model,
              Optimizer&            xOptInfo,
              const DoubleMatrix&   dvecXLow,
              const DoubleMatrix&   dvecXUp,
              const DoubleMatrix&   dvecXIn,
              DoubleMatrix*         pdvecXOut,
              const DoubleMatrix&   dvecXStep,
              Optimizer&            alpOptInfo,
              const DoubleMatrix&   dvecAlpLow,
              const DoubleMatrix&   dvecAlpUp,
              const DoubleMatrix&   dvecAlpIn,
              DoubleMatrix*         pdvecAlpOut,
              const DoubleMatrix&   dvecAlpStep,
              double*               pdPhiTildeOut,
              DoubleMatrix*         pdrowPhiTilde_xOut,
              DoubleMatrix*         pdmatPhiTilde_x_xOut )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // If no evaluation is requested, return immediately.
  if ( !pdvecXOut && 
       !pdPhiTildeOut &&
       !pdrowPhiTilde_xOut &&
       !pdmatPhiTilde_x_xOut )
  {
    return;
  }

  // Get the number of design and fixed population parameters.
  int nX   = dvecXIn.nr();
  int nAlp = dvecAlpIn.nr();


  //------------------------------------------------------------
  // Validate the inputs (Debug mode).
  //------------------------------------------------------------

  // Check the length of the design parameter vector.
  assert( nX == model.getNDesPar() );


  //------------------------------------------------------------
  // Validate the inputs (All modes).
  //------------------------------------------------------------

  // [Revisit - Warm Start Disabled - Mitch]
  // In order to simplify the testing of this function,
  // warm starts have been disabled for now.
  if ( xOptInfo.getIsWarmStart() || alpOptInfo.getIsWarmStart() )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "One of the input Optimizer objects requested a warm start, which have been disabled for now.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Prepare the values required to calculate the output values.
  //------------------------------------------------------------

  // If this value is required to calculate the output values, 
  // initialize it so that it wil be calculated.  Otherwise, 
  // set its pointer to zero so that it won't be calculated.
  DoubleMatrix dvecXOutTemp;
  DoubleMatrix* pdvecXOutTemp = &dvecXOutTemp;
  if ( pdvecXOut )
  {
    dvecXOutTemp.resize( nX, 1 );
  }
  else
  {
    pdvecXOutTemp = 0;
  }

  // If this value is required to calculate the output values, 
  // initialize it so that it wil be calculated.  Otherwise, 
  // set its pointer to zero so that it won't be calculated.
  DoubleMatrix dvecAlpOutTemp;
  DoubleMatrix* pdvecAlpOutTemp = &dvecAlpOutTemp;
  if ( pdvecAlpOut )
  {
    dvecAlpOutTemp.resize( nAlp, 1 );
  }
  else
  {
    pdvecAlpOutTemp = 0;
  }

  // If this value is required to calculate the output values, 
  // initialize it so that it wil be calculated.  Otherwise, 
  // set its pointer to zero so that it won't be calculated.
  double dLTildeOut;
  double* pdLTildeOut = &dLTildeOut;
  if ( pdPhiTildeOut || pdrowPhiTilde_xOut || pdmatPhiTilde_x_xOut )
  {
    dLTildeOut = 0.0;
  }
  else
  {
    pdLTildeOut = 0;
  }

  // If this value is required to calculate the output values, 
  // initialize it so that it wil be calculated.  Otherwise, 
  // set its pointer to zero so that it won't be calculated.
  DoubleMatrix drowLTilde_xOut;
  DoubleMatrix* pdrowLTilde_xOut = &drowLTilde_xOut;
  if ( pdrowPhiTilde_xOut || pdmatPhiTilde_x_xOut )
  {
    drowLTilde_xOut.resize( 1, nX );
  }
  else
  {
    pdrowLTilde_xOut = 0;
  }
  
  // If this value is required to calculate the output values, 
  // initialize it so that it wil be calculated.  Otherwise, 
  // set its pointer to zero so that it won't be calculated.
  DoubleMatrix dmatLTilde_x_xOut;
  DoubleMatrix* pdmatLTilde_x_xOut = &dmatLTilde_x_xOut;
  if ( pdmatPhiTilde_x_xOut )
  {
    dmatLTilde_x_xOut.resize( nX, nX );
  }
  else
  {
    pdmatLTilde_x_xOut = 0;
  }
  

  //------------------------------------------------------------
  // Set arguments related to using ppkaOpt for optimal design.
  //------------------------------------------------------------

  // ppkaOpt solves a two-level optimization problem, where the second
  // level is made up of multiple optimization problems that are all 
  // solved for each iteration of the first level.  The length of the 
  // vector N specifies the number of optimation problems that must be 
  // solved at the second level.  In this use of ppkaOpt, the first 
  // level optimization is over the design parameter vector x, while the 
  // second level is over the fixed population vector alp.  Since there 
  // is only a single fixed population vector alp, N has length one.  Its
  // only value is one because lTilde requires that there be at least
  // one data value, but this criterion itself does not depend on the data.
  int nIndPped = 1;
  int nY_iPped = 1;
  DoubleMatrix dvecN( nIndPped, 1 );
  DoubleMatrix dvecY( nY_iPped, 1 );
  dvecN.fill( (double) nY_iPped );
  dvecY.fill( 0.0 );

  // Choose the modified Laplace approximation.
  enum Objective objective = MODIFIED_LAPLACE;

  // Construct a model that evaluates first order approximations for
  // the mean and covariance of the data, maps individual to population
  // parameters, and maps population to design parameters.
  FoMapsParSpkModel ppedOptModel( &model, dvecAlpStep.toValarray() );


  //------------------------------------------------------------
  // Optimize the modified Laplace approximation for the negative log of phi(x).
  //------------------------------------------------------------

  // The criterion phi(x) is optimized by replacing the definition of 
  // lambda(alp, b) and mapObj(b) by the negative natural logarithm 
  // of the integral with respect to alp and then calls the function 
  // ppkaOpt to optimize the Laplace approximation for the integral.
  try
  {
    ppkaOpt( ppedOptModel,
             objective,
             dvecN,
             dvecY,
             xOptInfo,
             dvecXLow,
             dvecXUp,
             dvecXIn,
             &dvecXOutTemp,
             dvecXStep,
             alpOptInfo,
             dvecAlpLow,
             dvecAlpUp,
             dvecAlpIn,
             &dvecAlpOutTemp,
             dvecAlpStep,
             &dLTildeOut,
             &drowLTilde_xOut,
             &dmatLTilde_x_xOut );
  }
  catch( SpkException& e )
  {
    throw e.push(
      SpkError::SPK_UNKNOWN_ERR,
      "Optimization of the population expected determinant criterion failed.",
      __LINE__,
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    throw SpkException(
      stde,
      "A standard exception occurred during the optimization of the population expected determinant criterion.",
      __LINE__,
      __FILE__ );
  }
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR,
      "An unknown exception occurred during the optimization of the population expected determinant criterion.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Don't set any of the values if the maximum number of
  // iterations was reached for either optimization level.
  if ( xOptInfo.getIsTooManyIter() || alpOptInfo.getIsTooManyIter() )
  {
    return;
  }

  // Set the final design parameter value, if necessary.
  if ( pdvecXOut )
  {
    *pdvecXOut = dvecXOutTemp;
  }

  // Set the final fixed population parameter value, if necessary.
  if ( pdvecAlpOut )
  {
    *pdvecAlpOut = dvecAlpOutTemp;
  }

  // Calculate and set the final population expected determinant
  // criterion value, if necessary.
  double dPhiTildeOutTemp;
  if ( pdPhiTildeOut || pdrowPhiTilde_xOut || pdmatPhiTilde_x_xOut )
  {
    // Calculate
    //
    //     phiTilde(x)  =  exp[ - LTilde(x) ] 
    //
    // at x = xOut.
    dPhiTildeOutTemp = exp( -dLTildeOut );

   if ( pdPhiTildeOut )
   {
     *pdPhiTildeOut = dPhiTildeOutTemp;
   }
  }

  // Calculate and set the first derivative of the population
  // expected determinant criterion at the final parameter value,
  // if necessary.
  DoubleMatrix drowPhiTilde_xOutTemp;
  if ( pdrowPhiTilde_xOut || pdmatPhiTilde_x_xOut )
  {
    // Calculate
    //
    //     phiTilde_x(x)  =  - exp[ - LTilde(x) ] * LTilde_x(x)
    //
    //                    =  - phiTilde(x) * LTilde_x(x)
    //
    // at x = xOut.
    drowPhiTilde_xOutTemp.resize( 1, nX );
    mulByScalar( drowLTilde_xOut, -dPhiTildeOutTemp, drowPhiTilde_xOutTemp );

    if ( pdrowPhiTilde_xOut )
    {
      *pdrowPhiTilde_xOut = drowPhiTilde_xOutTemp;
    }
  }

  // Calculate and set the second derivative of the population
  // expected determinant criterion at the final parameter value, if necessary.
  if ( pdmatPhiTilde_x_xOut )
  {
    // [Revisit - Hessian Mixes Analytic and Approximate Derivatives - Mitch]
    // Would it be better to approximate the Hessian, phiTilde_x_x(x),
    // by taking finite differences of the full derivative, phiTilde_x(x),
    // rather than approximating one term in the Hessian, LTilde_x_xOut(x),
    // by taking finite differences of LTilde_x(x)?
    //
    // Calculate
    //
    //     phiTilde_x_x(x)  =  exp[ - LTilde(x) ]  *
    //
    //                            -                                                -
    //                           |                                 T                |
    //                           |  - LTilde_x_x(x)  +  LTilde_x(x)  * LTilde_x(x)  |
    //                           |                                                  |
    //                            -                                                -
    //
    //                      =  phiTilde(x)  *
    //
    //                            -                                                -
    //                           |                                 T                |
    //                           |  - LTilde_x_x(x)  +  LTilde_x(x)  * LTilde_x(x)  |
    //                           |                                                  |
    //                            -                                                -
    //
    // at x = xOut.
    //
    DoubleMatrix dvecLTilde_xTrans;
    DoubleMatrix temp1;
    DoubleMatrix temp2;
    transpose( drowLTilde_xOut, dvecLTilde_xTrans );
    multiply( dvecLTilde_xTrans, drowLTilde_xOut, temp1 );
    subtract( temp1, dmatLTilde_x_xOut, temp2 );
    mulByScalar( temp2, dPhiTildeOutTemp, *pdmatPhiTilde_x_xOut );
  }

}

