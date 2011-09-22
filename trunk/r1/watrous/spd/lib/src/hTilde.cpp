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
 * File: hTilde.cpp
 *
 *
 * Calculates the approximate population information matrix.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: hTilde
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*

$begin HTilde$$

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

$section The Approximate Population Information Matrix$$

$index HTilde$$
$cindex #the #approximate population information matrix$$

$table
$bold Syntax:$$ $cend
$syntax/void hTilde( SpdModel& /model/, 
             const DoubleMatrix& /dvecX/,
             const DoubleMatrix& /dvecAlp/,
             const DoubleMatrix& /dvecAlpStep/,
             DoubleMatrix* /pdmatHTildeOut/,
             DoubleMatrix* /pdmatHTilde_xOut/,
             DoubleMatrix* /pdmatHTilde_alpOut )
/$$

$tend

$fend 25$$

$head Description$$
Calculates an approximation for the population information matrix
corresponding to the negative log-likelihood of all of the data,
$math%

    HTilde(x, alp)  ,

%$$
where $math%x%$$ is a vector of design parameters and $math%alp%$$
is a vector of fixed population parameters.
This function also evaluates the derivatives of this function
with respect to $math%x%$$ and with respect to $math%alp%$$.
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

$head Return Values$$
The return value of $code HTilde$$ 
is true, if it succeeds, and false otherwise.

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

/x/
/$$
The double-precision column vector $italic x$$
specifies the value for the design parameter vector.

$syntax/

/alp/
/$$
The double-precision column vector $italic alp$$
specifies the value for the fixed population parameter vector.
$syntax/

/alpStep/
/$$
The double-precision column vector $italic alpStep$$
specifies the step size used for approximating
the derivatives with respect to the fixed population parameters.
$syntax/

/pHTildeOut/
/$$
the $xref/glossary/Output Value/output value/$$ of $italic pHTildeOut$$
is a square matrix containing the value of $math%HTilde(x, alp)%$$.
$syntax/

/pHTilde_xOut/
/$$
the $xref/glossary/Output Value/output value/$$ of $italic pHTilde_xOut$$
is a matrix containing the derivative of $math%HTilde(x, alp)%$$
with respect to $italic x$$.
$syntax/

/pHTilde_alpOut/
/$$
the $xref/glossary/Output Value/output value/$$ of $italic pHTilde_alpOut$$
is a matrix containing the derivative of $math%HTilde(x, alp)%$$
with respect to $italic x$$.

$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPD include files.
#include "SpdModel.h"
#include "FoMapsParSpkModel.h"

// SPK include files.
#include "DoubleMatrix.h"
#include "SpkValarray.h"
#include "expectedHessian.h"
#include "getSubblock.h"
#include "replaceSubblock.h"
#include "add.h"

// Standard include files.
#include <string>
#include <cassert>
#include <exception>

using SPK_VA::valarray;


 /*------------------------------------------------------------------------
 * Local class definitions
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  //
  // Class: FoMapsIndParSpkModel 
  //
  //
  // Evaluates first order approximations for the mean and covariance
  // of the data, maps individual to population parameters, but does
  // not map population to design parameters.
  //  
  class FoMapsIndParSpkModel : public FoMapsParSpkModel
  {
    //------------------------------------------------------------
    // Constructors.
    //------------------------------------------------------------

  public:
    FoMapsIndParSpkModel( SpdModel* const pSpdModelIn )
      :
      FoMapsParSpkModel( pSpdModelIn )
    {}

  private:
    // A default constructor should never be called for this class.
    // This default constructor is declared private and not defined
    // here so it won't be called.
    FoMapsIndParSpkModel();


    //------------------------------------------------------------
    // State changing functions.
    //------------------------------------------------------------

  private:
    // This function does nothing, i.e. , it does not map population
    // to design parameters.  Thus, calls to this function (via the
    // wrapper function SpkModel::setPopPar) do not change the design
    // parameters state variable for the SpdModel this object contains.
    void doSetPopPar( const valarray<double>& inVA ) {}
  };

} // [End: unnamed namespace]


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void hTilde( SpdModel& model, 
             const DoubleMatrix& dvecX,
             const DoubleMatrix& dvecAlp,
             const DoubleMatrix& dvecAlpStep,
             DoubleMatrix* pdmatHTildeOut,
             DoubleMatrix* pdmatHTilde_xOut,
             DoubleMatrix* pdmatHTilde_alpOut )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // If no evaluation is requested, return immediately.
  if ( !pdmatHTildeOut && !pdmatHTilde_xOut && !pdmatHTilde_alpOut )
  {
    return;
  }

  // Get the number of individuals, design parameters, common
  // design parameters, and fixed population parameters.
  int nInd      = model.getNInd();
  int nX        = dvecX.nr();
  int nXCommon  = model.getNCommonDesPar();
  int nAlp      = dvecAlp.nr();

  int i;

  // Get the number of design parameters for each individual.
  valarray<int> nX_i( nInd );
  for ( i = 0; i < nInd; i++ )
  {
    nX_i[i] = model.getNIndDesPar( i );
  }

  const valarray<double> x   = dvecX.toValarray();
  const valarray<double> alp = dvecAlp.toValarray();


  //------------------------------------------------------------
  // Validate the inputs (Debug mode).
  //------------------------------------------------------------

  // Check the length of the design parameter vector.
  assert( nX == model.getNDesPar() );


  //------------------------------------------------------------
  // Prepare the model.
  //------------------------------------------------------------

  // Set the current values for these parameters.  Note that
  // the design parameter must be set here because the model 
  // that will be passed to the expectedHessian function will
  // not set it.
  model.setDesPar( x );
  model.setPopPar( alp );

  // Construct a model that evaluates first order approximations for
  // the mean and covariance of the data, maps individual to population
  // parameters, but does not map population to design parameters.
  FoMapsIndParSpkModel hTildeModel( &model );  

  
  //------------------------------------------------------------
  // Prepare the objects to hold the output values.
  //------------------------------------------------------------

  DoubleMatrix dmatHTilde;
  DoubleMatrix dmatHTilde_i;
  DoubleMatrix dmatHTilde_x;
  DoubleMatrix dmatHTilde_x_common;
  DoubleMatrix dmatHTilde_i_chi;
  DoubleMatrix dmatHTilde_alp;
  DoubleMatrix dmatHTilde_i_alp;

  DoubleMatrix* pdmatHTilde_i;
  DoubleMatrix* pdmatHTilde_i_chi;
  DoubleMatrix* pdmatHTilde_i_alp;

  // If this output value should be returned, resize the quantities 
  // associated with it.  If not, set its pointer to zero so that 
  // it will not be calculated.
  if ( pdmatHTildeOut )
  {
    dmatHTilde.  resize( nAlp, nAlp );
    dmatHTilde_i.resize( nAlp, nAlp );
    dmatHTilde.fill( 0.0 );

    pdmatHTilde_i = &dmatHTilde_i;
  }
  else
  {
    pdmatHTilde_i = 0;
  }

  // If this output value should be returned, resize the quantities 
  // associated with it.  If not, set its pointer to zero so that 
  // it will not be calculated.
  if ( pdmatHTilde_xOut )
  {
    dmatHTilde_x       .resize( nAlp*nAlp, nX );
    dmatHTilde_x_common.resize( nAlp*nAlp, nXCommon );
    dmatHTilde_x_common.fill( 0.0 );
  }
  else
  {
    pdmatHTilde_i_chi = 0;
  }

  // If this output value should be returned, resize the quantities 
  // associated with it.  If not, set its pointer to zero so that 
  // it will not be calculated.
  if ( pdmatHTilde_alpOut )
  {
    dmatHTilde_alp.  resize( nAlp*nAlp, nAlp );
    dmatHTilde_i_alp.resize( nAlp*nAlp, nAlp );
    dmatHTilde_alp.fill( 0.0 );

    pdmatHTilde_i_alp = &dmatHTilde_i_alp;
  }
  else
  {
    pdmatHTilde_i_alp = 0;
  }


  //------------------------------------------------------------
  // Compute the expected Hessian and its derivatives.
  //------------------------------------------------------------

  // Only the length of this vector matters.
  DoubleMatrix dvecChi_iDummy;

  DoubleMatrix temp;

  int nX_iTotal = 0;
  int nChi_i;
  bool withD = false;

  // Compute each individual's contribution.
  for ( i = 0; i < nInd; i++ )
  {
    hTildeModel.selectIndividual( i );

    // Prepare this individual's values.
    nChi_i = nX_i[i] + nXCommon;
    dvecChi_iDummy.resize( nChi_i, 1 );
    if ( pdmatHTilde_xOut )
    {
      dmatHTilde_i_chi.resize( nAlp*nAlp, nChi_i );
      pdmatHTilde_i_chi = &dmatHTilde_i_chi;
    }

    // Compute the expected Hessian for this individual.
    try
    {
      expectedHessian(
        hTildeModel,
        dvecChi_iDummy,
        dvecAlp,
        dvecAlpStep,
        pdmatHTilde_i,
        pdmatHTilde_i_chi,
        pdmatHTilde_i_alp,
        withD );
    }
    catch( SpkException& e )
    {
      const int max = SpkError::maxMessageLen();
      char message[max];
      sprintf( message, "Evaluation of the %d-th individual's expected Hessian failed.\n", i );

      throw e.push(
        SpkError::SPK_UNKNOWN_ERR,
        message,
        __LINE__,
        __FILE__ );
    }
    catch( const std::exception& stde )
    {
      const int max = SpkError::maxMessageLen();
      char message[max];
      sprintf( message, "A standard exception occurred during the evaluation of the %d-th individual's expected Hessian.\n", i );
      
      throw SpkException(
        stde,
        message,
        __LINE__,
        __FILE__ );
    }
    catch( ... )
    {
      const int max = SpkError::maxMessageLen();
      char message[max];
      sprintf( message, "An unknown exception occurred during the evaluation of the %d-th individual's expected Hessian.\n", i );
      
      throw SpkException(
        SpkError::SPK_UNKNOWN_ERR,
        message,
        __LINE__,
        __FILE__ );
    }

    // Calculate the population information matrix as
    //                         M
    //                        ----   
    //     HTilde(x, alp)  =  >      HTilde_i(chi_i, alp)  .
    //                        ----
    //                        i = 1
    if ( pdmatHTildeOut )
    {
      dmatHTilde = add( dmatHTilde, dmatHTilde_i );
    }

    // Calculate the derivative with respect to x as
    //
    //     HTilde_x(x, alp)  =  
    //
    //          -                                                                                   -
    //         |  HTilde_1_x_1(chi_1, alp), ... , HTilde_M_x_M(chi_M, alp), HTilde_x_common(x, alp)  |  ,
    //          -                                                                                   -
    // where
    //                                  M
    //                                 ----   
    //     HTilde_x_common(x, alp)  =  >      HTilde_i_x_common(chi_i, alp)  .
    //                                 ----
    //                                 i = 1
    //
    // Note that
    //
    //                                   -                                                         -
    //     HTilde_i_chi(chi_i, alp)  =  |  HTilde_i_x_i(chi_i, alp), HTilde_i_x_common(chi_i, alp)  |  .
    //                                   -                                                         -
    //
    if ( pdmatHTilde_xOut )
    {

      // Fill in the value for HTilde_i_x_i, if necessary.
      if ( nX_i[i] )
      {
        // Get HTilde_i_x_i.
        getSubblock(
          dmatHTilde_i_chi,
          0,
          0,
          nAlp*nAlp,
          nX_i[i],
          temp );
 
        // Fill in its value.
        replaceSubblock( 
          dmatHTilde_x,
          temp,
          0,
          nX_iTotal );

        nX_iTotal += nX_i[i];
      }

      // Add the contribution from HTilde_i_x_common, if necessary.
      if ( nXCommon )
      {
        // Get HTilde_i_x_common.
        getSubblock(
          dmatHTilde_i_chi,
          0,
          nX_i[i],
          nAlp*nAlp,
          nXCommon,
          temp);
 
        // Add it to the sum.
        dmatHTilde_x_common = add( dmatHTilde_x_common, temp );
      }
    }

    // Calculate the derivative with respect to alp as
    //
    //                              M
    //                             ----   
    //      HTilde_alp(x, alp)  =  >      HTilde_i_alp(chi_i, alp)  .
    //                             ----
    //                             i = 1
    if ( pdmatHTilde_alpOut )
    {
      dmatHTilde_alp = add( dmatHTilde_alp, dmatHTilde_i_alp );
    }
  }

  // Fill in the value for HTilde_i_x_common, if necessary.
  if ( pdmatHTilde_xOut && nXCommon )
  {
    replaceSubblock( 
      dmatHTilde_x,
      dmatHTilde_x_common,
      0,
      nX_iTotal );
  }

  // Reset the fixed population parameter value because it is
  // changed by the finite differencing done in expectedHessian.
  model.setPopPar( alp );


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  if ( pdmatHTildeOut )
  {
    *pdmatHTildeOut = dmatHTilde;
  }

  if ( pdmatHTilde_xOut )
  {
    *pdmatHTilde_xOut = dmatHTilde_x;
  }

  if ( pdmatHTilde_alpOut )
  {
    *pdmatHTilde_alpOut = dmatHTilde_alp;
  }

  return;
}
