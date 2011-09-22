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
 * File: ppdOpt.cpp
 *
 *
 * Optimizes the population determinant optimal design criterion.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: ppdOpt
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*

$begin ppdOpt$$

$escape #$$

$spell
  cg
  covariance
  df
  dx
  eps
  epsline
  negLogVal
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

$section Optimization of the Population Determinant Criterion$$

$index ppdOpt$$
$cindex optimization #of #the population determinant criterion$$

$table
$bold Syntax:$$ $cend
$syntax/void ppdOpt( SpdModel&             /model/,
             Optimizer&            /optInfo/,
             const DoubleMatrix&   /dvecXLow/,
             const DoubleMatrix&   /dvecXUp/,
             const DoubleMatrix&   /dvecXIn/,
             DoubleMatrix*         /pdvecXOut/,
             const DoubleMatrix&   /dvecXStep/,
             const DoubleMatrix&   /dvecAlp/,
             const DoubleMatrix&   /dvecAlpStep/,
             double*               /pdPhiTildeOut/,
             DoubleMatrix*         /pdrowPhiTilde_xOut/,
             DoubleMatrix*         /pdmatPhiTilde_x_xOut/ )
/$$

$tend

$fend 25$$

$head Description$$
Determines the design vector $math%x%$$ that
maximizes the parametric population determinant criterion
$math%

    phiTilde(x)  =  #det[ HTilde(x, alp) ]  .

%$$
In this expression, $xref/HTilde//HTilde(x, alp)/$$ is an approximation for
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
estimate for $math%xHat%$$, the true minimizer of 
$math%phiTilde(x)%$$, within a specified number of iterations. 
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

/optInfo/
/$$
This $xref/Optimizer//Optimizer/$$ class object has three attributes.  
These attributes are parameters of the optimizer used during the design 
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
$italic optInfo$$ $code .getEpsilon()$$, 
and $math%xHat%$$ is the true maximizer of the population determinant optimal 
design criterion.
This is a rough approximation that is quick to
calculate during the optimization procedure.

$syntax/

/dvecXLow/
/$$
The $code DoubleMatrix$$ $italic dvecXLow$$ contains the column vector 
$math%xLow%$$, which specifies the lower limit for $math%x%$$ during 
the optimization procedure.
The length of $italic dvecXLow$$ is equal to the length of 
dvecXIn$$.
$syntax/

/dvecXUp/
/$$
The $code DoubleMatrix$$ $italic dvecXUp$$ contains the column vector 
$math%xUp%$$, which specifies the upper limit for $math%x%$$ during 
the optimization procedure.
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
approximate maximizer of the population determinant optimal design criterion.
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

/dvecAlp/
/$$
The $code DoubleMatrix$$ $italic dvecAlp$$ contains the column 
vector $math%alp%$$, which specifies the value for the fixed 
population parameters.
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
value of the population determinant optimal design criterion evaluated 
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
derivative of the population determinant optimal design criterion with 
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
an approximation for the Hessian of the population determinant optimal 
design criterion with respect to $math%x%$$ evaluated at $math%xOut%$$.
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdPhiTilde_x_xOut$$.

$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPD include files.
#include "ppdOpt.h"
#include "SpdModel.h"
#include "hTilde.h"
#include "centdiff.h"

// SPK include files.
#include "DoubleMatrix.h"
#include "Optimizer.h"
#include "quasiNewtonAnyBox.h"
#include "mulByScalar.h"
#include "transposeDerivative.h"
#include "transpose.h"
#include "multiply.h"
#include "hTilde.h"
#include "det.h"
#include "inverse.h"

// Standard include files.
#include <string>
#include <cassert>
#include <exception>
#include <cmath>
#include <functional>


/*------------------------------------------------------------------------
 * Local class definitions
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  //**********************************************************************
  //
  // Class: PpdOptObj
  //
  //
  // Evaluates the negative logarithm of the population determinant
  // criterion,
  //
  //     negLogVal(x)  =  - log[ phiTilde(x) ]  ,
  //
  // and/or its derivative.  
  //
  //**********************************************************************

  class PpdOptObj : public QuasiNewtonAnyBoxObj
  {
    //----------------------------------------------------------
    // Constructors.
    //----------------------------------------------------------

  public:
    PpdOptObj( 
      int                  nXIn,
      SpdModel*            pModelIn,
      const DoubleMatrix*  pdvecAlpIn,
      const DoubleMatrix*  pdvecAlpStepIn )
      :
      nX            ( nXIn ),
      nAlp          ( pdvecAlpIn->nr() ),
      pModel        ( pModelIn ),
      pdvecAlp      ( pdvecAlpIn ),
      pdvecAlpStep  ( pdvecAlpStepIn ) 
    {
      assert( pdvecAlp->nc() == 1 );

      assert( pdvecAlpStep->nr() == nAlp );
      assert( pdvecAlpStep->nc() == 1 );
    }

    //----------------------------------------------------------
    // Data members.
    //----------------------------------------------------------

  private:
    const int nX;
    const int nAlp;

    DoubleMatrix dvecXCurr;

    SpdModel*                pModel;
    const DoubleMatrix*      pdvecAlp;
    const DoubleMatrix*      pdvecAlpStep;


    //----------------------------------------------------------
    // Functions required by quasiNewtonAnyBox.
    //----------------------------------------------------------

  public:
    //**********************************************************
    // 
    // Function: function
    //
    //
    // Evaluates the negative logarithm of the population
    // determinant criterion, negLogVal(x).
    //
    //**********************************************************

    void function( const DoubleMatrix& dvecXIn, double* pdNegLogValOut )
    {
      //------------------------------------------------------------
      // Preliminaries.
      //------------------------------------------------------------
    
      // Set the current value for x.
      dvecXCurr = dvecXIn;
      assert( dvecXIn.nr() == nX );
      assert( dvecXIn.nc() == 1 );


      //------------------------------------------------------------
      // Evaluate the population information matrix.
      //------------------------------------------------------------
    
      // Initialize this so that it wil be calculated.
      DoubleMatrix dmatHTildeOut( nAlp, nAlp );
    
      DoubleMatrix* pdmatHTildeOut = &dmatHTildeOut;
      DoubleMatrix* pdmatNull = 0;
    
      hTilde(
        *pModel,
        dvecXCurr,
        *pdvecAlp,
        *pdvecAlpStep,
        pdmatHTildeOut,
        pdmatNull,
        pdmatNull );
    
    
      //------------------------------------------------------------
      // Calculate the negative logarithm of the criterion.
      //------------------------------------------------------------
    
      // Determine p and q such that
      //                                    q
      //     det[ HTilde(x, alp) ]  =  p * 2   .
      //
      // Note that the information matrix may not be positive
      // definite.  The function det() will throw an exception if
      // this is the case, but that exception is not handled here.
      double p;
      long int q;
      det( dmatHTildeOut, &p, &q );
  
      // Calculate
      //
      //     negLogVal(x)  =  - log det[ HTilde(x, alp) ]  .
      //
      double dNegLogVal = - ( log( p ) + q * log( 2.0 ) );
    

      //------------------------------------------------------------
      // Finish up.
      //------------------------------------------------------------
    
      // Set the objective function value.
      *pdNegLogValOut = dNegLogVal;
    }


    //**********************************************************
    // 
    // Function: gradient
    //
    //
    // Evaluate the derivative of the negative logarithm of the
    // population determinant criterion, negLogVal_x(x).
    //
    //**********************************************************

    virtual void gradient( DoubleMatrix* pdrowNegLogVal_xOut ) const
    {
      //------------------------------------------------------------
      // Preliminaries.
      //------------------------------------------------------------
    
      assert( pdrowNegLogVal_xOut->nr() == 1 );
      assert( pdrowNegLogVal_xOut->nc() == nX );

    
      //------------------------------------------------------------
      // Evaluate the population information matrix and its derivative.
      //------------------------------------------------------------
    
      // Initialize these so that they wil be calculated.
      DoubleMatrix dmatHTildeOut( nAlp, nAlp );
      DoubleMatrix dmatHTilde_xOut( nAlp * nAlp, nX );

      DoubleMatrix* pdmatHTildeOut = &dmatHTildeOut;
      DoubleMatrix* pdmatHTilde_xOut = &dmatHTilde_xOut;
      DoubleMatrix* pdmatNull = 0;
    
      hTilde(
        *pModel,
        dvecXCurr,
        *pdvecAlp,
        *pdvecAlpStep,
        pdmatHTildeOut,
        pdmatHTilde_xOut,
        pdmatNull );
    
    
      //------------------------------------------------------------
      // Calculate the derivative of the negative logarithm of the criterion.
      //------------------------------------------------------------
    
      // This will hold the derivative of the negative logarithm of
      // the population determinant criterion.
      DoubleMatrix drowNegLogVal_xCurr( 1, nX );

      // Calculate
      //                           -                          -  T
      //                          |                      -1    |
      //     negLogVal_x(x)  =  - |  rvec[ HTilde(x, alp)   ]  |   * HTilde_x(x, alp)  .
      //                          |                            |
      //                           -                          -
      //
      // This expression is based on Lemma 9, of B. M. Bell, 
      // "Approximating the marginal likelihood estimate for
      // models with random parameters", Applied Mathematics 
      // and Computation, 119 (2001), pp. 57-73, and the
      // fact that HTilde is symmetric.
  
      DoubleMatrix dmatHTildeInv;
      DoubleMatrix dmatHTildeInvRvec;
      DoubleMatrix dmatHTildeInvRvecTrans;
      DoubleMatrix drowPosLogVal_x;
  
      // Construct the derivative of the negative logarithm of
      // the population determinant criterion.
      dmatHTildeInv = inverse( dmatHTildeOut );
      dmatHTildeInvRvec = rvec( dmatHTildeInv );
      transpose( dmatHTildeInvRvec, dmatHTildeInvRvecTrans );
      multiply( dmatHTildeInvRvecTrans, dmatHTilde_xOut, drowPosLogVal_x );
      mulByScalar( drowPosLogVal_x, -1.0, drowNegLogVal_xCurr );


      //------------------------------------------------------------
      // Finish up.
      //------------------------------------------------------------
    
      // Set the derivative value.
      *pdrowNegLogVal_xOut = drowNegLogVal_xCurr;
    }

  };


  //**********************************************************************
  //
  // Class:  PpdOptObj_xFuncObj
  //
  //
  // Objects of this class are unary function objects that return the
  // derivative of the negative logarithm of the population determinant
  // criterion.
  //
  //**********************************************************************

  class PpdOptObj_xFuncObj : 
    public std::unary_function<DoubleMatrix, DoubleMatrix>
  {
  private:
    PpdOptObj* pPpdOptObj;

  public: 
    PpdOptObj_xFuncObj( PpdOptObj* pPpdOptObjIn )
      : pPpdOptObj( pPpdOptObjIn )
    {
    }
    PpdOptObj_xFuncObj( const PpdOptObj_xFuncObj& right ) 
      : pPpdOptObj( right.pPpdOptObj )
    {
    }

    ~PpdOptObj_xFuncObj(){}

    //
    // Function: operator()
    //
    //
    // This is the function-like operator for the class.
    //
    const DoubleMatrix operator()( const DoubleMatrix& dvecX ) const
    {
      int nX = dvecX.nr();
      assert( dvecX.nc() == 1 );

      // The objective function must be called first because it sets
      // the current x value.
      double dNegLogOut;
      pPpdOptObj->function( dvecX, &dNegLogOut );

      // Calculate the derivative of the objective function.
      DoubleMatrix drowNegLog_xOut( 1, nX );
      pPpdOptObj->gradient( &drowNegLog_xOut );

      return drowNegLog_xOut;
    }
  };

} // [End: unnamed namespace]


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

void ppdOpt( SpdModel&             model,
             Optimizer&            optInfo,
             const DoubleMatrix&   dvecXLow,
             const DoubleMatrix&   dvecXUp,
             const DoubleMatrix&   dvecXIn,
             DoubleMatrix*         pdvecXOut,
             const DoubleMatrix&   dvecXStep,
             const DoubleMatrix&   dvecAlp,
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

  // Get the number of design parameters.
  int nX = dvecXIn.nr();


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
  if ( optInfo.getIsWarmStart() )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The input Optimizer object requested a warm start, which have been disabled for now.",
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
  if ( pdvecXOut || pdmatPhiTilde_x_xOut )
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
  double dNegLogOut;
  double* pdNegLogOut = &dNegLogOut;
  if ( pdPhiTildeOut || pdrowPhiTilde_xOut || pdmatPhiTilde_x_xOut )
  {
    dNegLogOut = 0.0;
  }
  else
  {
    pdNegLogOut = 0;
  }

  // If this value is required to calculate the output values, 
  // initialize it so that it wil be calculated.  Otherwise, 
  // set its pointer to zero so that it won't be calculated.
  DoubleMatrix drowNegLog_xOut;
  DoubleMatrix* pdrowNegLog_xOut = &drowNegLog_xOut;
  if ( pdrowPhiTilde_xOut || pdmatPhiTilde_x_xOut )
  {
    drowNegLog_xOut.resize( 1, nX );
  }
  else
  {
    pdrowNegLog_xOut = 0;
  }
  

  //------------------------------------------------------------
  // Prepare the objective function.
  //------------------------------------------------------------

  // Construct the objective function, which will evaluate the 
  // negative logarithm of the population determinant criterion.
  PpdOptObj ppdOptObj( nX, &model, &dvecAlp, &dvecAlpStep );


  //------------------------------------------------------------
  // Handle nonzero iterations for the optimization problem.
  //------------------------------------------------------------

  // If the number of iterations is not zero, then optimize the
  // objective function to determine the optimal parameter value.
  if ( optInfo.getNMaxIter() > 0 )
  {
    try
    {
      // The criterion phi is maximized by finding
      // the minumum of its negative logarithm.
      quasiNewtonAnyBox( ppdOptObj,
			 optInfo,
			 dvecXLow,
			 dvecXUp,
			 dvecXIn,
			 pdvecXOutTemp,
			 pdNegLogOut,
			 pdrowNegLog_xOut ); 
    }
    catch( SpkException& e )
    {
      throw e.push(
        SpkError::SPK_UNKNOWN_ERR,
        "Optimization of the population determinant criterion failed.",
        __LINE__,
        __FILE__ );
    }
    catch( const std::exception& stde )
    {
      throw SpkException(
        stde,
        "A standard exception occurred during the optimization of the population determinant criterion.",
        __LINE__,
        __FILE__ );
    }
    catch( ... )
    {
      throw SpkException(
        SpkError::SPK_UNKNOWN_ERR,
        "An unknown exception occurred during the optimization of the population determinant criterion.",
        __LINE__,
        __FILE__ );
    }
  }


  //------------------------------------------------------------
  // Handle zero iterations for the optimization problem.
  //------------------------------------------------------------

  // If the number of iterations is zero, then the final
  // value for the parameter is equal to its initial value.
  if ( optInfo.getNMaxIter() == 0 )
  {
    // Set the final value.
    dvecXOutTemp = dvecXIn;
    
    try
    {
      // Evaluate the negative logarithm of the criterion phi,
      // and/or its derivative, at the final value.
      if ( pdPhiTildeOut || pdrowPhiTilde_xOut )
      {
	// The objective function must be called first to set x.
	ppdOptObj.function( dvecXOutTemp, pdNegLogOut );

	// Calculate the derivative, if necessary.
	if ( pdrowPhiTilde_xOut )
        {
	  ppdOptObj.gradient( pdrowNegLog_xOut );
	}
      }
    }
    catch( SpkException& e )
    {
      throw e.push(
        SpkError::SPK_UNKNOWN_ERR,
        "Evaluation of the population determinant criterion failed.",
        __LINE__,
        __FILE__ );
    }
    catch( const std::exception& stde )
    {
      throw SpkException(
        stde,
        "A standard exception occurred during the evaluation of the population determinant criterion.",
        __LINE__,
        __FILE__ );
    }
    catch( ... )
    {
      throw SpkException(
        SpkError::SPK_UNKNOWN_ERR,
        "An unknown exception occurred during the evaluation of the population determinant criterion.",
        __LINE__,
        __FILE__ );
    }
  }
  

  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Don't set any of the values if the maximum number of
  // iterations was reached.
  if ( optInfo.getIsTooManyIter() )
  {
    return;
  }

  // Set the final parameter value, if necessary.
  if ( pdvecXOut )
  {
    *pdvecXOut = dvecXOutTemp;
  }

  // Calculate and set the final population determinant criterion
  // value, if necessary.
  double dPhiTildeOutTemp;
  if ( pdPhiTildeOut || pdrowPhiTilde_xOut || pdmatPhiTilde_x_xOut )
  {
    // Calculate
    //
    //     phiTilde(x)  =  exp[ - negLogVal(x) ] 
    //
    // at x = xOut.
    dPhiTildeOutTemp = exp( -dNegLogOut );

   if ( pdPhiTildeOut )
   {
     *pdPhiTildeOut = dPhiTildeOutTemp;
   }
  }

  // Calculate and set the first derivative of the population
  // determinant criterion at the final parameter value, if necessary.
  DoubleMatrix drowPhiTilde_xOutTemp;
  if ( pdrowPhiTilde_xOut || pdmatPhiTilde_x_xOut )
  {
    // Calculate
    //
    //     phiTilde_x(x)  =  - exp[ - negLogVal(x) ] * negLogVal_x(x)
    //
    //                    =  - phiTilde(x) * negLogVal_x(x)
    //
    // at x = xOut.
    drowPhiTilde_xOutTemp.resize( 1, nX );
    mulByScalar( drowNegLog_xOut, -dPhiTildeOutTemp, drowPhiTilde_xOutTemp );

    if ( pdrowPhiTilde_xOut )
    {
      *pdrowPhiTilde_xOut = drowPhiTilde_xOutTemp;
    }
  }

  // Calculate and set the second derivative of the population
  // determinant criterion at the final parameter value, if necessary.
  if ( pdmatPhiTilde_x_xOut )
  {
    // [Revisit - Hessian Mixes Analytic and Approximate Derivatives - Mitch]
    // Would it be better to approximate the Hessian, phiTilde_x_x(x),
    // by taking finite differences of the full derivative, phiTilde_x(x),
    // rather than approximating one term in the Hessian, negLog_x_xOut(x),
    // by taking finite differences of negLog_x(x)?
    //
    // Calculate
    //
    //     phiTilde_x_x(x)  =  exp[ - negLogVal(x) ]  *
    //
    //                            -                                                         -
    //                           |                                       T                   |
    //                           |  - negLogVal_x_x(x)  +  negLogVal_x(x)  * negLogVal_x(x)  |
    //                           |                                                           |
    //                            -                                                         -
    //
    //                      =  phiTilde(x)  *
    //
    //                            -                                                         -
    //                           |                                       T                   |
    //                           |  - negLogVal_x_x(x)  +  negLogVal_x(x)  * negLogVal_x(x)  |
    //                           |                                                           |
    //                            -                                                         -
    //
    // at x = xOut.
    //

    // Calculate the central difference of the analytic derivative
    // of the negative log of the criterion.
    DoubleMatrix dmatNegLog_x_xTilde;
    PpdOptObj_xFuncObj ppdOptObj_xFuncObj( &ppdOptObj );
    try
    {
      dmatNegLog_x_xTilde = centdiff<PpdOptObj_xFuncObj>( 
        ppdOptObj_xFuncObj,
        dvecXOutTemp,
        dvecXStep );
    }
    catch( SpkException& e )
    {
      throw e.push(
        SpkError::SPK_UNKNOWN_ERR,
        "Evaluation of the Hessian of the population determinant criterion failed.",
        __LINE__,
        __FILE__ );
    }
    catch( const std::exception& stde )
    {
      throw SpkException(
        stde,
        "A standard exception occurred during the evaluation of the Hessian of the population determinant criterion.",
        __LINE__,
        __FILE__ );
    }
    catch( ... )
    {
      throw SpkException(
        SpkError::SPK_UNKNOWN_ERR,
        "An unknown exception occurred during the evaluation of the Hessian of the population determinant criterion.",
        __LINE__,
        __FILE__ );
    }

    // Switch the order of differentiation so that the central 
    // difference comes before the analytic derivative.
    DoubleMatrix dmatNegLog_xTilde_x;
    dmatNegLog_xTilde_x = transposeDerivative( 
      drowNegLog_xOut,
      dmatNegLog_x_xTilde );

    DoubleMatrix dvecNegLogVal_xTrans;
    DoubleMatrix temp1;
    DoubleMatrix temp2;

    // Construct the second derivative of the population
    // determinant criterion.
    transpose( drowNegLog_xOut, dvecNegLogVal_xTrans );
    multiply( dvecNegLogVal_xTrans, drowNegLog_xOut, temp1 );
    subtract( temp1, dmatNegLog_xTilde_x, temp2 );
    mulByScalar( temp2, dPhiTildeOutTemp, *pdmatPhiTilde_x_xOut );
  }

}

