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
 * File: indStatistics.cpp
 *
 *
 * Compute the covariance matrix, standard errors, correlation matrix and
 * 95% confidence interval for individual parameter estimates.
 *
 * Author: Jiaji Du
 *
 * Modified later by: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include "indStatistics.h"
#include "popStatistics.h"
#include "SpkException.h"
#include "SpkModel.h"
#include "elsq_x.h"
#include "multiply.h"
#include "transpose.h"
#include "AkronBtimesC.h"
#include "inverse.h"
#include "WarningsManager.h"

// Standard library header files.
#include <cmath>
#include <sstream>
#include <string>

using SPK_VA::valarray;
using SPK_VA::slice;


/*------------------------------------------------------------------------
 * Local function definitions
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  //=========================================================
  // Expand the vector x to y. Insert "val" in places where
  // mask[i] is false.
  //=========================================================

  void placeVal( const valarray<bool>   & mask,
                 const valarray<double> & x,
                 valarray<double>       & y,
                 double val = NAN )
  {
    assert( mask.size() == y.size() );
    const int nX = x.size();
    const int nY = y.size();

    int i;
    int ii;
    for ( i=0, ii=0; i<nY; i++ )
      {
        if ( mask[i] )
          {
            y[i] = x[ii];
            ii++;
          }
        else
          y[i] = val;
      }
  }

} // [End: unnamed namespace]


/*------------------------------------------------------------------------
 * Local class definitions
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  //**********************************************************************
  //
  // Class: DeprecatedModel
  //
  //
  // This class evaluates the model functions required by the new
  // interface to indStatistics() that takes an SpkModel as an
  // argument.
  //
  // This class can be deleted if the deprecated indStatistics()
  // functions are deleted.
  //
  //**********************************************************************

  class DeprecatedModel : public SpkModel
  {
    //----------------------------------------------------------
    // Class members.
    //----------------------------------------------------------

  private:
    const valarray<double>* pDataMean_indPar;
    const valarray<double>* pDataVariance_indPar;
    const valarray<double>* pDataVarianceInv;


    //----------------------------------------------------------
    // Constructors.
    //----------------------------------------------------------

  public:
    // This constructor sets the individual index in the model passed
    // in to a value that will not be changed.
    DeprecatedModel(
      const valarray<double>* pDataMean_indParIn,
      const valarray<double>* pDataVariance_indParIn,
      const valarray<double>* pDataVarianceInvIn )
      :
      pDataMean_indPar     ( pDataMean_indParIn ),
      pDataVariance_indPar ( pDataVariance_indParIn ),
      pDataVarianceInv     ( pDataVarianceInvIn )
    {
    }


    //----------------------------------------------------------
    // State changing functions.
    //----------------------------------------------------------

    void doSetIndPar( const valarray<double>& indParIn )
    {
      // This does nothing since this is not a real SpkModel.
    }


    //----------------------------------------------------------
    // Model evaluation functions.
    //----------------------------------------------------------

    // This function sets the return value for f(b) equal to all
    // zeroes since the formulations that require this model function
    // are not used for the depecated indStatistics() functions.
    void doDataMean( valarray<double>& ret ) const
    {
      // Number of data points.
      const int nY = static_cast<int>(
        sqrt( static_cast<double>( pDataVarianceInv->size() ) ) );

      // Number of individual parameters.
      const int nB = pDataMean_indPar->size() / nY;

      assert( pDataMean_indPar->size() == nY * nB );
      assert( pDataVarianceInv->size() == nY * nY );

      ret.resize( nY );
      ret = 0.0;
    }

    bool doDataMean_indPar( valarray<double>& ret ) const
    {
      // Set the value.
      ret.resize( pDataMean_indPar->size() );
      ret = *pDataMean_indPar;

      // Return true if at least one partial derivative is nonzero.
      int i;
      for ( i = 0; i < ret.size(); i++ )
      {
        if ( ret[i] != 0.0 )
        {
          return true;
        }
      }
      return false;
    }

    void doDataVariance( valarray<double>& ret ) const
    {
      // Number of data points.
      const int nY = static_cast<int>(
        sqrt( static_cast<double>( pDataVarianceInv->size() ) ) );

      assert( pDataVarianceInv->size() == nY * nY );

      // Set the value.
      ret.resize( nY * nY );
      ret = inverse( *pDataVarianceInv, nY );
    }

    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
      // Set the value.
      ret.resize( pDataVariance_indPar->size() );
      ret = *pDataVariance_indPar;

      // Return true if at least one partial derivative is nonzero.
      int i;
      for ( i = 0; i < ret.size(); i++ )
      {
        if ( ret[i] != 0.0 )
        {
          return true;
        }
      }
      return false;
    }

    void doDataVarianceInv( valarray<double>& ret ) const
    {
      // Set the value.
      ret.resize( pDataVarianceInv->size() );
      ret = *pDataVarianceInv;
    }

  };

} // [End: unnamed namespace]


/*************************************************************************
 *
 * Function: indStatistics
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin indStatistics$$

$spell
  Model model
  valarray
  Cov
  Obj
  enum
  Laplace
  subvector
  dmat
  const
  dvec
  int
  cout
  endl
  nr
  nc
  iostream
  iomanip
  namespace
  std
  ios
  covariance
  ind
  cerr
  Spk
  inv
  optimizer
  fp
  Optimizer optimizer
  Fo
  Dir
  Yi
  inx
  aval
  bval
  resize
  bool
  Dinv
  Rinv
  var
  sqrt
  cbc
  covariances
  cor
  cmath
  statistics
  confint
  Enumerator
  exp
  Beal
  Raton
  Ruppert
  Sheiner
  Stefanski
  symmetrized
$$

$section Computing Statistics of Individual Parameter Estimates$$

$index indStatistics, coefficient of variation, confidence interval$$
$index covariance, standard error, correlation matrix, individual parameters$$
$cindex \Computing Statistics \of Individual \Parameter \Estimates$$

$table
$bold Enumerator:$$ $cend
$syntax/enum PopCovForm { RSR, R, S, HSH, H }/$$ $rend
$bold Prototype:$$ $cend
$syntax/void indStatistics( SpkModel&                        /model/,
                    const SPK_VA::valarray<double>&  /measurements/,
                    const SPK_VA::valarray<double>&  /indPar/,
                    const SPK_VA::valarray<bool>&    /indParMask/,
                    const SPK_VA::valarray<double>&  /indObj_indPar_indPar/,
                    const IndCovForm                 /formulation/,
                    SPK_VA::valarray<double>*        /indParCovOut/,
                    SPK_VA::valarray<double>*        /indParSEOut/,
                    SPK_VA::valarray<double>*        /indParCorOut/,
                    SPK_VA::valarray<double>*        /indParCVOut/,
                    SPK_VA::valarray<double>*        /indParCIOut/,
                    bool                             /withD/ )
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
This function computes the covariance matrix, standard error vector,
correlation matrix, coefficient of variation vector, and confidence
interval vector for individual parameter estimates.
It allows parameter elements that are not active to be specified
and removed from the statistics computations.
$pre

$$
This function allows the covariance matrix of the individual parameter
estimates to be calculated using one of the following formulations:
$math%

                                      -1     -1
    formulation "RSR":  cov[ b ]  =  R   S  R   ;

                                      -1     -1
    formulation "HSH":  cov[ b ]  =  H   S  H   ;

                                      -1
    formulation "R":    cov[ b ]  =  R   ;

                                      -1
    formulation "H":    cov[ b ]  =  H   ;

                                      -1
    formulation "S":    cov[ b ]  =  S   .

%$$
These formulations are discussed in Section (D.2.5) of the NONMEM
Users Guide Part II and in Sections (A.2.1) and (A.2.2) in
Carroll, Ruppert, and Stefanski (1998).
$pre

$$

The first approximation that can be made for the information matrix is
$math%
                -                                     -
            1  |                                    T  | 
     R  =  --- | d  d  MapObj(b)  +  d  d  MapObj(b)   |  ,
            2  |  b  b                b  b             | 
                -                                     -
%$$
which is a symmetrized version of the Hessian of the 
$xref/mapObj//individual objective function/$$ $math%MapObj(b)%$$.
The matrix $math%R%$$ is defined in this way to insure that it is symmetric
even for cases where the Hessian approximation is not.
$pre

$$
The second approximation that can be made for the information matrix is
$math%
                -               -
               |                 | 
     H  =  E   | d  d  MapObj(b) | 
            y  |  b  b           | 
                -               -

            -1            T  -1                1        T        -1      -1
        =  D    +  d  f(b)  R  (b) d  f(b)  +  - d  R(b)  kron[ R  (b), R  (b) ] d  R(b)  ,
                    b               b          2  b                               b

%$$
which is the expected value with respect to the data $math%y%$$ of the
Hessian of the individual objective function.
See Section (7.)  of Bell (2001) for its derivation.
Note that the $math%R(b)%$$ on the right hand side of this
equation is the model for the covariance of an individual's data,
which is part of $xref/SpkModel//SpkModel/$$ and
is different than the information matrix $math%R%$$.
The term involving the covariance of the individual parameters
$math%D%$$, which is also part of $xref/SpkModel//SpkModel/$$,
can be dropped from this calculation if the Map Bayesian objective
function was not used to obtain the individual parameter estimates.
See $xref/fitIndividual//fitIndividual/$$ for details.
$pre

$$
Currently, if either the covariance of the individual's data
$math%R(b)%$$ or the covariance of the individual parameters
$math%D%$$ is not diagonal, then only the R formulation may be used.
If both are diagonal, however, the other two formulations may be used
and the cross-product gradient matrix is defined as
$math%

            n%Y                  nY %+ nB
            -%--                   -%--
            \\       T             \\        T
     S  =   /%    { s   s  }  +    /%     { s   s  }  ,
            -%--     i   i         -%--      i   i
           i %= 1               i = %nY + 1

%$$
where $math%nY%$$ is the number of data values for the individual
and $math%nB%$$ is the number of individual parameters.
This is the empirical version of equation (A.2) from
Carroll, Ruppert, and Stefanski (1998) where $math%s_i%$$ is the
likelihood score.
The second sum can be dropped from this calculation if the Map
Bayesian objective function was not used to obtain the individual
parameter estimates.
For $math%i%$$ values less than or equal to $math%nY%$$,
$math%
                    -%       %                                                               -
           - 1     | %       %                                   T     -1                     |
    s   =   --- d  | \log[ 2 \pi R     (b) ]  +  [y    - f   (b)]  R     (b) [y    - f   (b)] |  .
     i       2   b | %       %    (i,i)            (i)    (i)       (i,i)      (i)    (i)     |
                    -%       %                                                               -

%$$
If the Map Bayesian objective function was used to obtain the
individual parameter estimates, then there is one additional
likelihood score for each of the individual parameters.
For $math%i%$$ values greater than $math%nY%$$,
$math%

                    -%       %                                            -
           - 1     | %       %                                -1        2  |
    s   =   --- d  | \log[ 2 \pi D            ]  +  D             b        |  .
     i       2   b | %       %    (i-nY,i-nY)        (i-nY,i-nY)   (i-nY)  |
                    -%       %                                            -

%$$
The standard error vector is calculated by taking the square roots
of the diagonal elements of the covariance matrix.
The correlation matrix is calculated by dividing each element of
the covariance matrix by the standard errors that correspond to its
row and column.
The coefficients of variation are calculated as
$math%

    CV    =  SE    / | b    | * 100   ,
     (i)       (i)      (i)

%$$
where CV is the coefficient of variation and SE is the standard error.
The 95% confidence intervals are calculated as
$math%

    ( b    -  t               * SE    ,  b    +  t               * SE    ) ,
       (i)     0.025, degFree     (i)     (i)     0.025, degFree     (i)

%$$
where
$math%

    t
     0.025, degFree

%$$
is the critical value for the $math%t%$$ distribution with
$math%degFree = nY - nB%$$ number of degrees of freedom for
which the area under the $math%t%$$ curve is $math%1 - 0.025%$$.

$head Reference$$
Beal, S. L. and Sheiner, L. B. (1988) $italic NONMEM Users Guide - Part II$$,
University of California, San Francisco.
$pre

$$
Bell, B. M. (2001) Approximating the marginal likelihood estimate for
models with random parameters, $italic Applied Mathematics and Computation$$,
$bold 119$$, 57-75.
$pre

$$
Carroll, R. J., Ruppert, D., and Stefanski, L. A. (1998)
$italic Measurement Error in Nonlinear Models$$, Chapman & Hall/CRC,
Boca Raton, Florida.

$head Return Value$$
Upon a successful completion, the function sets
the given output value place holders to point to the result values.

$pre

$$
If an error is detected or failure occurs during the evaluation, a SpkException
object is thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/
/model/
/$$
This function expects $italic model$$ to be a function of $math%b%$$.
Refer to $xref/glossary/Model Functions Depend on only b/Model
Functions Depend on only b/$$ for details.
If the Map Bayesian objective function was used to obtain the
individual parameter estimates, then the function
$tref SpkModel_indParVariance$$ must be defined for this model in
order to calculate the covariance of the individual parameters
$math%D%$$.
In this case, the argument $italic withD$$ should be set
equal to $code true$$.

$syntax/

/measurements/
/$$
The $code SPK_VA::valarray<double>$$ $italic measurements$$ contains the array
$math%y%$$, which specifies the measured data.

$syntax/

/indPar/
/$$
The $code SPK_VA::valarray<double>$$ $italic indPar$$ contains the vector
$math%b%$$, which specifies the estimates of the individual parameters.
The returned values $italic indParCovOut$$, $italic indParSEOut$$,
$italic indParCorOut$$, $italic indParCVOut$$ and $italic indParCIOut$$
will be evaluated at these estimates.
The $italic values of indPar$$ should be obtained by calling the SPK function
$xref/fitIndividual//fitIndividual/$$.

$syntax/

/indParMask/
/$$
$code indParMask$$ is a vector of boolean values of length equal to the parameter
vector, $code indPar$$.  $code indParMask[i]$$ tells as to whether $code indPar[i]$$
is active or not.  If $math%indParMask[i]%$$ is $math%false%$$, the i-th element of
the parameter vector are treated as if it does not exist and further
statistics computations are performed based upon the assumption.

$syntax/

/indObj_indPar_indPar/ 
/$$
The $code SPK_VA::valarray<double>$$ $italic indObj_indPar_indPar$$ contains 
the matrix $math%MapObj_b_b%$$, in column major order, which specifies 
an approximation for the second derivative of the individual objective 
function with respect to the individual parameter evaluated at $italic indPar$$.  
Note that the size of $italic indObj_indPar_indPar$$ should be equal to the 
square of the length of the individual parameter vector $math%b%$$.  
The $italic indObj_indPar_indPar$$ should be obtained by calling the SPK function 
$xref/fitIndividual//fitIndividual/$$. 

$syntax/

/indParCovOut/
/$$
If $italic indParCovOut$$ is not $code NULL$$, then the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCovOut$$
must be declared in the function that calls this function, and its size must
be equal to the square of the length of the individual parameter vector
$math%b%$$.  If $italic indParCovOut$$ is not $code NULL$$ and this function
completed successfully, then the $code SPK_VA::valarray<double>$$ object
pointed to by $italic indParCovOut$$ will contain the covariance matrix
of the individual parameter estimates, in column major order, that is evaluated
at $italic indPar$$.  Otherwise, this function will not attempt to change the
contents of the $code SPK_VA::valarray<double>$$ object pointed to by
$italic indParCovOut$$.

The $math%(i,j)%$$-the element of the covariance matrix
will be replaced by NaN if $code indParMask[i]$$ or $code indParMask[j]$$ is $math%false%$$.

$syntax/

/indParSEOut/
/$$
If $italic indParSEOut$$ is not $code NULL$$, then the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParSEOut$$
must be declared in the function that calls this function, and its size must
be equal to the length of the individual parameter vector
$math%b%$$.  If $italic indParSEOut$$ is not $code NULL$$ and this function
completed successfully, then the $code SPK_VA::valarray<double>$$ object
pointed to by $italic indParSEOut$$ will contain the standard error vector
of the individual parameter estimates, in column major order, that is evaluated
at $italic indPar$$.  Otherwise, this function will not attempt to change the
contents of the $code SPK_VA::valarray<double>$$ object pointed to by
$italic indParSEOut$$.

The $math%i%$$-th element of the standard error vector
will be replaced by NaN if $code indParMask[i]$$ is $math%false%$$.

$syntax/

/indParCorOut/
/$$
If $italic indParCorOut$$ is not $code NULL$$, then the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCorOut$$
must be declared in the function that calls this function, and its size must
be equal to the square of the length of the individual parameter vector
$math%b%$$.  If $italic indParCorOut$$ is not $code NULL$$ and this function
completed successfully, then the $code SPK_VA::valarray<double>$$ object
pointed to by $italic indParCorOut$$ will contain the correlation matrix
of the individual parameter estimates, in column major order, that is evaluated
at $italic indPar$$.  Otherwise, this function will not attempt to change the
contents of the $code SPK_VA::valarray<double>$$ object pointed to by
$italic indParCorOut$$.

The $math%(i, j)%$$-th element of the correlation matrix
will be replaced by NaN if $code indParMask[i]$$ or $code indParMask[j]$$ is $math%false%$$.

$syntax/

/indParCVOut/
/$$
If $italic indParCVOut$$ is not $code NULL$$, then the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCVOut$$
must be declared in the function that calls this function, and its size must
be equal to the length of the individual parameter vector
$math%b%$$.  If $italic indParCVOut$$ is not $code NULL$$ and this function
completed successfully, then the $code SPK_VA::valarray<double>$$ object
pointed to by $italic indParCVOut$$ will contain the standard error vector
of the individual parameter estimates, in column major order, that is evaluated
at $italic indPar$$.  Otherwise, this function will not attempt to change the
contents of the $code SPK_VA::valarray<double>$$ object pointed to by
$italic indParCVOut$$.

The $math%i%$$-th element of the coefficient vector
will be replaced by NaN if $code indParMask[i]$$ is $math%false%$$.

$syntax/

/indParCIOut/
/$$
If $italic indParCIOut$$ is not $code NULL$$, then the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCIOut$$
must be declared in the function that calls this function, and its size must
be equal to the two times of the length of the individual parameter vector
$math%b%$$.  If $italic indParCIOut$$ is not $code NULL$$ and this function
completed successfully, then the $code SPK_VA::valarray<double>$$ object pointed
to by $italic indParCIOut$$ will contain the 95% confidence interval values
of the individual parameter estimates, in column major order, that is evaluated
at $italic indPar$$.  There are two columns in the object.  The first column
contains the lower limit, and the second column contains the upper limit of
the confidence interval of the individual parameter estimates.  Otherwise,
this function will not attempt to change the contents of the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCIOut$$.
Note that in the calculation of the confidence interval, if the degree of freedom
(number of data - number of parameters) is greater than 120 it is treated as infinite.

The $math%(i,1)%$$ and $math%(i,2)%$$ elements of the confidence interval matrix
will be replaced by NaN if $code indParMask[i]$$ is $math%false%$$.

$syntax/

/withD/
/$$
If this flag is set equal to $code false$$, then the terms involving
the covariance of the individual parameters $math%D%$$ are dropped
from the calculations of the information matrix $math%R%$$ and the
cross-product gradient matrix $math%S%$$.
This should only be done if the Map Bayesian objective
function was not used to obtain the individual parameter estimates.
See $xref/fitIndividual//fitIndividual/$$ for details.
$pre

$$
If this flag is set equal to $code true$$, then the function
$tref SpkModel_indParVariance$$ must be defined for the input argument
$italic model$$ in order to calculate $math%D%$$.

$end
*/


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void indStatistics( SpkModel&                model,
                    const valarray<double>&  measurements,
                    const valarray<double>&  indPar,
                    const valarray<bool>&    indParMask,
                    const valarray<double>&  indObj_indPar_indPar,
                    const IndCovForm         formulation,
                    valarray<double>*        indParCovOut,
                    valarray<double>*        indParSEOut,
                    valarray<double>*        indParCorOut,
                    valarray<double>*        indParCVOut,
                    valarray<double>*        indParCIOut,
                    bool                     withD )
{
  //----------------------------------------------------------------
  // Preliminaries.
  //----------------------------------------------------------------

  using namespace std;

  // Return if there are no output values to compute.
  if ( indParCovOut == 0 &&
       indParSEOut  == 0 &&
       indParCorOut == 0 &&
       indParCVOut  == 0 &&
       indParCIOut  == 0 )
  {
    return;
  }

  // Number of data values.
  const int nY = measurements.size();

  // Number of individual parameters.
  const int nB = indPar.size();

  // Number of degrees of freedom.
  const int nDegFree = nY - nB;


  //----------------------------------------------------------------
  // Validate the inputs.
  //----------------------------------------------------------------

  // Check the number of data values.
  if ( nY < 1 )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The number of measurements must be greater than zero.",
      __LINE__,
      __FILE__ );
  }

  // Check the number of parameters.
  if ( nB < 1 )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The number of parameters must be greater than zero.",
      __LINE__,
      __FILE__ );
  }

  // Check the size of the parameter mask.
  if ( indParMask.size() != nB )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The length of the parameter mask must match the number of parameters.",
      __LINE__,
      __FILE__ );
  }

  // Check the size of the Hessian of the individual objective if one
  // of the formulations that requires it is being used.
  if ( formulation == H || formulation == HSH )
  {
    if ( indObj_indPar_indPar.size() != nB * nB )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR,
        "The length of the Hessian of the individual objective must be the square of the number of parameters.",
        __LINE__,
        __FILE__ );
    }
  }

  // Check the degrees of freedom if confidence intervals are going to
  // be calculated.
  if ( indParCIOut )
  {
    if ( nDegFree < 1 )
    {
      throw SpkException(
        SpkError::SPK_STATISTICS_ERR,
        "Confidence intervals cannot be calculated because the number of degrees of freedom is less than one.",
        __LINE__,
        __FILE__ );
    }
  }


  //----------------------------------------------------------------
  // Calculate the full model functions.
  //----------------------------------------------------------------

  valarray<double> f  ( nY );
  valarray<double> f_b( nY * nB );

  // The model functions that are related to R(b) have names that
  // begin with RR rather than R to avoid name conflict with the
  // formulation R.
  valarray<double> RR   ( nY * nY );
  valarray<double> RR_b ( nY * nY * nB );
  valarray<double> RRInv( nY * nY );

  valarray<double> DInv;

  // Evaluate
  //
  //     f( b )  .
  //
  model.dataMean( f );

  // Evaluate
  //
  //     d   f( b )  .
  //      b
  //
  bool isF_bAllZero = model.dataMean_indPar( f_b );

  // Evaluate
  //
  //     R( b )  .
  //
  // The array that holds this model function is named RR rather
  // than R to avoid name conflict with the formulation R.
  model.dataVariance( RR );

  // Evaluate
  //
  //     d   R( b )  .
  //      b
  //
  // The array that holds this model function is named RR_b rather
  // than R_b to avoid name conflict with the formulation R.
  bool isRR_bAllZero = model.dataVariance_indPar( RR_b );

  // Evaluate
  //
  //      -1
  //     R  (b)  .
  //
  // The array that holds this model function is named RRInv rather
  // than RInv to avoid name conflict with the formulation R.
  model.dataVarianceInv( RRInv );

  // Evaluate
  //
  //      -1
  //     D    .
  //
  if ( withD )
  {
    DInv.resize( nB * nB );
    model.indParVarianceInv( DInv );
  }


  //----------------------------------------------------------------
  // Validate the full model functions.
  //----------------------------------------------------------------

  // Check the data mean.
  if ( f.size() != nY )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The data mean has the wrong length.",
      __LINE__,
      __FILE__ );
  }

  // Check the derivative of the data mean.
  if ( f_b.size() != nY * nB )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The derivative of the data mean has the wrong length.",
      __LINE__,
      __FILE__ );
  }

  // Check the data variance.
  if ( RR.size() != nY * nY )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The data variance has the wrong length.",
      __LINE__,
      __FILE__ );
  }

  // Check the derivative of the data variance.
  if ( RR_b.size() != nY * nY * nB )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The derivative of the data variance has the wrong length.",
      __LINE__,
      __FILE__ );
  }

 // Check the data variance inverse.
  if ( RRInv.size() != nY * nY )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The data variance inverse has the wrong length.",
      __LINE__,
      __FILE__ );
  }

  // Check the inverse of the variance of the individual parameters.
  if ( withD )
  {
    if ( DInv.size() != nB * nB )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR,
        "The individual parameter variance inverse has the wrong dimensions.",
        __LINE__,
        __FILE__ );
    }
  }


  //----------------------------------------------------------------
  // See if the data and individual parameter covariances are diagonal.
  //----------------------------------------------------------------

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // [Revisit - Individual Statistics uses R or H formulation if Nondiagonal Covariances - Mitch]
  //
  // Currently, if either the covariance of the individual's data R(b)
  // or the covariance of the individual parameters D is not diagonal,
  // then only the R or H formulations may be used to calculate the
  // individual's statistics.  The reason is that the current code
  // that calculates the cross-product gradient matrix S assumes these
  // covariances are diagonal.
  //
  // For the nondiagonal case, it is probably possible to calculate S
  // using the formulas in Sections (A.2.1) and (A.2.2) in Carroll,
  // Ruppert, and Stefanski (1998), but it would require a change of
  // variables to come up with likelihoods that were independent so
  // that the log likelihood can be written as a sum over the logs of
  // these independent likelihoods.  For the R(b) and D matrices, the
  // variables would probably involve their Cholesky factorizations.
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  // The formulation will be changed to the R or H formulation if either
  // R(b) or D is not diagonal since this function cannot currently
  // calculate the cross-product gradient matrix S for this case.
  IndCovForm formulationToUse = formulation;

  ostringstream warning;
  string warningStr;

  int i;
  int j;
  bool isDiagonal;

  // See if the inverse of R(b) is diagonal.  Only the lower triangle
  // has to be checked because this matrix must be symmetric.
  isDiagonal = true;
  i = 0;
  while ( i < nY && isDiagonal )
  {
    j = 0;
    while ( j < i && isDiagonal )
    {
      if ( RRInv[i + j * nY] != 0.0 )
      {
        isDiagonal = false;
      }
      j++;
    }
    i++;
  }

  // If R(b) is not diagonal and a formulation other than R or H has
  // been requested, then change the formulation and issue a warning.
  if ( !isDiagonal )
  {
    if ( formulationToUse == RSR || formulationToUse == S )
    {
      formulationToUse = R;

      warning << "The individual statistics formulation has been changed to R because" << endl;
      warning << "the covariance of the individual's data is not diagonal." << endl;
      warningStr = warning.str();
      WarningsManager::addWarning( warningStr, __LINE__, __FILE__);
    }

    if ( formulationToUse == HSH )
    {
      formulationToUse = H;

      warning << "The individual statistics formulation has been changed to H because" << endl;
      warning << "the covariance of the individual's data is not diagonal." << endl;
      warningStr = warning.str();
      WarningsManager::addWarning( warningStr, __LINE__, __FILE__);
    }
  }

  // See if the inverse of D is diagonal.  Only the lower triangle
  // has to be checked because this matrix must be symmetric.
  if ( withD )
  {
    isDiagonal = true;
    i = 0;
    while ( i < nB && isDiagonal )
    {
      j = 0;
      while ( j < i && isDiagonal )
      {
        if ( DInv[i + j * nB] != 0.0 )
        {
          isDiagonal = false;
        }
        j++;
      }
      i++;
    }

    // If D is not diagonal and a formulation other than R or H has been
    // requested, then change the formulation and issue a warning.
    if ( !isDiagonal )
    {
      if ( formulationToUse == RSR || formulationToUse == S )
      {
        formulationToUse = R;

        warning << "The individual statistics formulation has been changed to R because" << endl;
        warning << "the covariance of the individual parameters is not diagonal." << endl;
        warningStr = warning.str();
        WarningsManager::addWarning( warningStr, __LINE__, __FILE__);
      }

      if ( formulationToUse == HSH )
      {
        formulationToUse = H;

        warning << "The individual statistics formulation has been changed to H because" << endl;
        warning << "the covariance of the individual parameters is not diagonal." << endl;
        warningStr = warning.str();
        WarningsManager::addWarning( warningStr, __LINE__, __FILE__);
      }
    }
  }


  //----------------------------------------------------------------
  // Calculate the reduced model functions.
  //----------------------------------------------------------------

  // Get the reduced set of parameters, i.e., the parameters that
  // should be included in the statistics calculation.
  valarray<double> x = indPar[ indParMask ];

  const int nX = x.size();

  //
  // Example:
  //
  // Given b    = { b(1), b(2), b(3), b(4), b(5) }
  //       mask = { T, F, T, F, T }
  //
  //        /                                            \
  //        |        |        |        |        |        |
  //  f_b = | f_b(1) | f_b(2) | f_b(3) | f_b(4) | f_b(5) |
  //        |        |        |        |        |        |
  //        \                                            /
  //
  //        /                          \
  //        |        |        |        |
  //  f_x = | f_b(1) | f_b(3) | f_b(5) |
  //        |        |        |        |
  //        \                          /
  //

  // Get the reduced version of f_b.
  valarray<bool> f_bMask( nY * nB );
  for ( i = 0; i < nB; i++ )
  {
    f_bMask[ slice( i * nY, nY, 1 ) ] = indParMask[i];
  }
  valarray<double> f_x = f_b[ f_bMask ];

  // Get the reduced version of R_b.
  valarray<bool> RR_bMask( nY * nY * nB );
  for ( i = 0; i < nB; i++ )
  {
    RR_bMask[ slice( i * nY * nY, nY * nY, 1 ) ] = indParMask[i];
  }
  valarray<double> RR_x = RR_b[ RR_bMask ];

  // Get the reduced version of DInv.  This will be the inverse of the
  // covariance of x.
  valarray<bool>   DInvMask;
  valarray<double> DInvReduced;
  if ( withD )
  {
    DInvMask   .resize( nB * nB );
    DInvReduced.resize( nB * nB );

    for ( i = 0; i < nB; i++ )
    {
      //  Set the mask elements for this column.
      DInvMask[ slice( i * nB, nB, 1  ) ] = indParMask[i];

      //  Set the mask elements for this row.
      DInvMask[ slice( i,      nB, nB ) ] = indParMask[i];
    }

    DInvReduced = DInv[ DInvMask ];
  }

  valarray<double> xCov( nX * nX );
  valarray<double> xSE ( nX );
  valarray<double> xCor( nX * nX );
  valarray<double> xCV ( nX );
  valarray<double> xCI ( nX * 2 );  // lower | upper


  //----------------------------------------------------------------
  // Calculate the reduced information matrix.
  //----------------------------------------------------------------

  valarray<double> infoMatrixReduced;

  // Calculate the reduced information matrix.
  if ( formulation == R || formulation == RSR )
  {
    // For these formulations, the reduced information matrix is
    // approximated by the Hessian of the individual objective
    // function with respect to the reduced set of parameters,
    //
    //       (reduced)     
    //      R           =  d  d  MapObj(b)  .
    //                      x  x
    //
    infoMatrixReduced.resize( nX * nX );

    try
    {
      // Get a symmetric version of the Hessian.
      valarray<double> indObj_indPar_indParSymm( nB * nB );
      indObj_indPar_indParSymm = 
        ( indObj_indPar_indPar + transpose( indObj_indPar_indPar, nB ) ) * 0.5;

      // Get the reduced version of the symmetrized Hessian.
      valarray<bool> infoMatrixMask( nB * nB );
      for ( i = 0; i < nB; i++ )
      {
        //  Set the mask elements for this column.
        infoMatrixMask[ slice( i * nB, nB, 1  ) ] = indParMask[i];
  
        //  Set the mask elements for this row.
        infoMatrixMask[ slice( i,      nB, nB ) ] = indParMask[i];
      }
      infoMatrixReduced = indObj_indPar_indParSymm[ infoMatrixMask ];
    }
    catch(SpkException& e)
    {
      throw e.push(
        SpkError::SPK_STATISTICS_ERR,
        "Failed to calculate the information matrix for individual statistics.",
        __LINE__,
        __FILE__ );
    }
  }
  else if ( formulation == H || formulation == HSH )
  {
    // For these formulations, the reduced information matrix is
    // approximated by the expected value for the Hessian of the
    // individual objective function with respect to the reduced set
    // of parameters, where the expected Hessian is defined in Section
    // (7.) of Bell (2001).
    //
    // The reduced information matrix is
    //
    //       (reduced)        (reduced)  -1
    //      H           =  [ D         ]
    //
    //                                 T  -1
    //                       +  d  f(b)  R  (b) d  f(b)
    //                           x               x
    //
    //                          1        T        -1      -1
    //                       +  - d  R(b)  kron[ R  (b), R  (b) ] d  R(b)  .
    //                          2  x                               x
    //
    // Note that the R(b) on the right hand side of this equation is the
    // model for the covariance of an individual's data and is different
    // than the information matrix R.
    //
    infoMatrixReduced.resize( nX * nX );

    try
    {
      // Start with this term:
      //
      //            T  -1
      //     d  f(b)  R  (b) d  f(b)  .
      //      x               x
      //
      infoMatrixReduced = multiply(
        transpose( f_x, nX ),
        nY,
        multiply( RRInv, nY, f_x, nX ),
        nX );

      // Add in this term:
      //
      //     1        T        -1      -1
      //     - d  R(b)  kron[ R  (b), R  (b) ] d  R(b)  .
      //     2  x                               x
      //
      infoMatrixReduced += 0.5 * multiply(
        transpose( RR_x, nX ),
        nY * nY,
        AkronBtimesC( RRInv, nY, RRInv, nY, RR_x, nX ),
        nX );

      // If the Map Bayesian statistics are being calculated,
      // add the reduced D inverse term:
      //
      //       (reduced)  -1
      //     [ D         ]    .
      //
      if ( withD )
      {
        infoMatrixReduced += DInvReduced;
      }
    }
    catch(SpkException& e)
    {
      throw e.push(
        SpkError::SPK_STATISTICS_ERR,
        "Failed to calculate the information matrix for individual statistics.",
        __LINE__,
        __FILE__ );
    }
  }


  //----------------------------------------------------------------
  // Calculate the reduced likelihood scores for the cross-product gradient matrix.
  //----------------------------------------------------------------

  // The approximation used for the reduced cross-product gradient
  // matrix is
  //                      nY
  //                      ---               T
  //       (reduced)      \       (reduced)    (reduced)
  //      S           =   /    { s            s           }
  //                      ---     i            i
  //                     i = 1
  //
  //                          nY + nX
  //                            ---               T
  //                            \       (reduced)    (reduced)
  //                       +    /    { s            s           }  .
  //                            ---     i            i
  //                         i = nY + 1
  //
  // This is the empirical version of equation (A.2) from Carroll,
  // Ruppert, and Stefanski (1998) where s_i is the reduced likelihood
  // score, which involves a derivative with respect to the reduced
  // set of parameters.
  //
  // The second sum will be dropped from this calculation if the Map
  // Bayesian objective function was not used to obtain the individual
  // parameter estimates.
  valarray<double> s_iReducedAll;

  // Calculate all of the reduced likelihood scores and put them into
  // a single matrix.
  if ( formulation == S || formulation == RSR )
  {
    // Set the number of reduced likelihood scores.
    int nS_i = nY;
    if ( withD )
    {
      nS_i += nX;
    }

    valarray<double> yOneByOne    ( 1 );
    valarray<double> fOneByOne    ( 1 );
    valarray<double> RROneByOne   ( 1 );
    valarray<double> RROneByOneInv( 1 );
    valarray<double> fOneByOne_x  ( nX );
    valarray<double> RROneByOne_x ( nX );
    valarray<double> s_iReduced   ( nX );

    s_iReducedAll.resize( nX * nS_i );

    try
    {
      // For i values less than or equal to nY, the reduced likelihood
      // scores are defined as
      //                             -
      //      (reduced)     - 1     |
      //     s           =   --- d  | log[ 2 pi R     (b) ]
      //      i               2   x |            (i,i)
      //                             -
      //
      //                                                                -
      //                                    T     -1                     |
      //                 +  [y    - f   (b)]  R     (b) [y    - f   (b)] |  .
      //                      (i)    (i)       (i,i)      (i)    (i)     |
      //                                                                -
      //
      // Note: the above expression assumes that the covariance
      // of the individual's data R(b) is diagonal.
      for ( i = 0; i < nY; i++ )
      {
        // Set one-by-one arrays that correspond to this data point.
        yOneByOne    [0] = measurements[i];
        fOneByOne    [0] = f           [i];
        RROneByOne   [0] = RR          [i + i * nY];
        RROneByOneInv[0] = RRInv       [i + i * nY];

        // Get the row that contains
        //
        //     d   f   ( b )  .
        //      x   (i)
        //
        fOneByOne_x = f_x[ slice( i, nX, nY ) ];

        // Get the row that contains
        //
        //     d   R   ( b )  .
        //      x   (i)
        //
        // Note that
        //
        //     d   R( b )  =  d   rvec[ R( b ) ]  .
        //      x              x
        //
        RROneByOne_x = RR_x[ slice( i + i * nY, nX, nY * nY ) ];

        // Calculate the reduced likelihood score for this data value.
        s_iReduced = elsq_x(
           yOneByOne,
           fOneByOne,
           RROneByOne,
           RROneByOneInv,
           fOneByOne_x,
           RROneByOne_x );
        s_iReduced *= -1.0;

        // Put this reduced likelihood score into the proper column of
        // the combined matrix of likelihood scores:
        //
        //                                   T             T                  T
        //                          (reduced)     (reduced)          (reduced)
        //     s_iReducedAll  =  [ s          ,  s          , ... , s           ]  .
        //                          1             2                  nS_i
        //
        s_iReducedAll[ slice( i * nX, nX, 1 ) ] = s_iReduced;
      }

      // If the Map Bayesian objective function was used to obtain the
      // individual parameter estimates, then there is one additional
      // reduced likelihood score for each of the individual parameters,
      //
      //                             -                                                   -
      //                            |                                        -1           |
      //      (reduced)     - 1     |            (reduced)          (reduced)          2  |
      //     s           =   --- d  | log[ 2 pi D            ]  +  D             x        |  .
      //      i               2   x |            (i-nY,i-nY)        (i-nY,i-nY)   (i-nY)  |
      //                             -                                                   -
      //
      // Note: the above expression assumes that the covariance of the
      // individual parameters D is diagonal.
      if ( withD )
      {
        for ( i = nY; i < nY + nX; i++ )
        {
          // Calculate the reduced likelihood score for this
          // individual parameter element.
          //
          //      (reduced)         (reduced)  -1
          //     s           =  - [ D         ]       x     .
          //      i(j)                         (j,j)   (j)
          //
          for ( j = 0; j < nX; j++ )
          {
            s_iReduced[ j ] = - DInvReduced[ j + j * nX ] * x[j];
          }

          // Put this reduced likelihood score into the proper column of
          // the combined matrix of likelihood scores:
          //
          //                                   T             T                  T
          //                          (reduced)     (reduced)          (reduced)
          //     s_iReducedAll  =  [ s          ,  s          , ... , s           ]  .
          //                          1             2                  nS_i
          //
          s_iReducedAll[ slice( i * nX, nX, 1 ) ] = s_iReduced;
        }
      }

    }
    catch(SpkException& e)
    {
      throw e.push(
        SpkError::SPK_STATISTICS_ERR,
        "Failed to calculate the likelihood scores for the cross-product gradient matrix for individual statistics.",
        __LINE__,
        __FILE__ );
    }
  }


  //----------------------------------------------------------------
  // Calculate the statistics for the reduced set of parameters.
  //----------------------------------------------------------------

  // Create a dummy data vector of all zeroes.  This is used by
  // popStatistics to determine the number of degrees of freedom.
  valarray<double> yDummy( nY );
  yDummy = 0.0;

  // Since there is no HSH or H formulation for popStatistics(),
  // change the H formulations flags to the R formulation conterparts.
  // Note that infoMatrixReduced contains R or H depending on how it
  // was calculated above.
  IndCovForm formulationForPopStat;
  if ( formulationToUse == HSH )
  {
    formulationForPopStat = RSR;
  }
  else if ( formulationToUse == H )
  {
    formulationForPopStat = R;
  }
  else
  {
    formulationForPopStat = formulationToUse;
  }

  // Call popStatistics() to calculate the statistics for the reduced
  // set of parameters because it has the machinery to handle the
  // different formulations.
  popStatistics( yDummy,
                 x,
                 s_iReducedAll,
                 infoMatrixReduced,
                 formulationForPopStat,
                 (indParCovOut? &xCov : NULL ),
                 (indParSEOut?  &xSE  : NULL ),
                 (indParCorOut? &xCor : NULL ),
                 (indParCVOut?  &xCV  : NULL ),
                 (indParCIOut?  &xCI  : NULL ) );


  //----------------------------------------------------------------
  // Set the statistics for the full set of parameters.
  //----------------------------------------------------------------

  valarray<bool> indParCIMask ( nB * 2 );
  valarray<bool> indParSEMask ( nB );
  valarray<bool> indParCVMask ( nB );
  valarray<bool> indParCovMask( nB * nB );
  valarray<bool> indParCorMask( nB * nB );
  double val = NAN;

  for ( j = 0; j < 2; j++ )
  {
    for ( i = 0; i < nB; i++ )
    {
      indParCIMask[ i + j * nB ] = indParMask[i];
    }
  }

  for ( j = 0; j < nB; j++ )
  {
    if ( indParMask[j] )
    {
      for ( i = 0; i < nB; i++ )
      {
        indParCovMask[ i + j * nB ] = indParMask[i];
        indParCorMask[ i + j * nB ] = indParMask[i];
      }

      indParSEMask[ j ] = indParMask[j];
      indParCVMask[ j ] = indParMask[j];
    }
    else
    {
      indParCovMask[ slice( j * nB, nB, 1 ) ] = false;
      indParCorMask[ slice( j * nB, nB, 1 ) ] = false;
    }
  }
  if ( indParCIOut )
  {
    placeVal( indParCIMask, xCI, *indParCIOut, val );
  }
  if ( indParCovOut )
  {
    placeVal( indParCovMask, xCov, *indParCovOut, val );
  }
  if ( indParCorOut )
  {
    placeVal( indParCorMask, xCor, *indParCorOut, val );
  }
  if ( indParSEOut )
  {
    placeVal( indParSEMask, xSE, *indParSEOut, val );
  }
  if ( indParCVOut )
  {
    placeVal( indParCVMask, xCV, *indParCVOut, val );
  }

}


/*************************************************************************
 *
 * Function: indStatistics (Deprecated)
 *
 *
 * This function is deprecated because its interface takes model
 * functions which means the interface must change if new model
 * functions are required to calculate the statistics.
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin indStatistics_Deprecated$$

$spell
  Model model
  valarray
  Cov
  Obj
  enum
  Laplace
  subvector
  dmat
  const
  dvec
  int
  cout
  endl
  nr
  nc
  iostream
  iomanip
  namespace
  std
  ios
  covariance
  ind
  cerr
  Spk
  inv
  optimizer
  fp
  Optimizer optimizer
  Fo
  Dir
  Yi
  inx
  aval
  bval
  resize
  bool
  Dinv
  Rinv
  var
  sqrt
  cbc
  covariances
  cor
  cmath
  statistics
  confint
  exp
  Beal
  Raton
  Ruppert
  Sheiner
  Stefanski
$$

$section Computing Statistics of Individual Parameter Estimates (Deprecated)$$

$index indStatistics, coefficient of variation, confidence interval$$
$index covariance, standard error, correlation matrix, individual parameters$$
$cindex \Computing Statistics \of Individual \Parameter \Estimates \(Deprecated)$$

$table
$bold Prototype:$$ $cend
$syntax/void indStatistics( const SPK_VA::valarray<double>& /indParMask/,
                    const SPK_VA::valarray<double>& /indPar/,
                    const SPK_VA::valarray<double>& /dataMean_indPar/,
                    const SPK_VA::valarray<double>& /dataVariance_indPar/,
                    const SPK_VA::valarray<double>& /dataVarianceInv/,
                    SPK_VA::valarray<double>*       /indParCovOut/,
                    SPK_VA::valarray<double>*       /indParSEOut/,
                    SPK_VA::valarray<double>*       /indParCorOut/,
                    SPK_VA::valarray<double>*       /indParCVOut/,
                    SPK_VA::valarray<double>*       /indParCIOut/ )
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
$pre

$$
$bold Note:  this function is deprecated.$$
$pre

$$
This function computes the covariance matrix, standard error vector,
correlation matrix, coefficient of variation vector, and confidence
interval vector for individual parameter estimates.
It allows parameter elements that are not active to be specified
and removed from the statistics computations.
$pre

$$
This function uses the following formulation to calculate the
covariance matrix of the individual parameter estimates:
$math%

                                      -1
    formulation "R":    cov[ b ]  =  R    .

%$$
This formulation is discussed in Section (D.2.5) of the NONMEM
Users Guide Part II and in Sections (A.2.1) and (A.2.2) in
Carroll, Ruppert, and Stefanski (1998).
$pre

$$
The approximation made for the information matrix is
$math%

     R  =  H(b)  ,

%$$
where the expected value for the Hessian of the individual
objective function is defined in Section (7.) of Bell (2001) as
$math%

                     T  -1                1        T        -1      -1
     H(b)  =  d  f(b)  R  (b) d  f(b)  +  - d  R(b)  kron[ R  (b), R  (b) ] d  R(b)  .
               b               b          2  b                               b

%$$
Note that the $math%R(b)%$$ on the right hand side of this
equation is the model for the covariance of an individual's data,
which is part of $xref/SpkModel//SpkModel/$$ and
is different than the information matrix $math%R%$$.
$pre

$$
The standard error vector is calculated by taking the square roots
of the diagonal elements of the covariance matrix.
The correlation matrix is calculated by dividing each element of
the covariance matrix by the standard errors that correspond to its
row and column.
The coefficients of variation are calculated as
$math%

    CV    =  SE    / | b    | * 100   ,
     (i)       (i)      (i)

%$$
where CV is the coefficient of variation and SE is the standard error.
The 95% confidence intervals are calculated as
$math%

    ( b    -  t               * SE    ,  b    +  t               * SE    ) ,
       (i)     0.025, degFree     (i)     (i)     0.025, degFree     (i)

%$$
where
$math%

    t
     0.025, degFree

%$$
is the critical value for the $math%t%$$ distribution with
$math%degFree = nY - nB%$$ number of degrees of freedom for
which the area under the $math%t%$$ curve is $math%1 - 0.025%$$.

$head Reference$$
Beal, S. L. and Sheiner, L. B. (1988) $italic NONMEM Users Guide - Part II$$,
University of California, San Francisco.
$pre

$$
Bell, B. M. (2001) Approximating the marginal likelihood estimate for
models with random parameters, $italic Applied Mathematics and Computation$$,
$bold 119$$, 57-75.
$pre

$$
Carroll, R. J., Ruppert, D., and Stefanski, L. A. (1998)
$italic Measurement Error in Nonlinear Models$$, Chapman & Hall/CRC,
Boca Raton, Florida.

$head Return Value$$
Upon a successful completion, the function sets
the given output value place holders to point to the result values.

$pre

$$
If an error is detected or failure occurs during the evaluation, a SpkException
object is thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/
/indParMask/
/$$
$code indParMask$$ is a vector of boolean values of length equal to the parameter
vector, $code indPar$$.  $code indParMask[i]$$ tells as to whether $code indPar[i]$$
is active or not.  If $math%indParMask[i]%$$ is $math%false%$$, the i-th element of
the parameter vector are treated as if it does not exist and further
statistics computations are performed based upon the assumption.

$syntax/

/indPar/
/$$
The $code SPK_VA::valarray<double>$$ $italic indPar$$ contains the vector
$math%b%$$, which specifies the estimates of the individual parameters.
The returned values $italic indParCovOut$$, $italic indParSEOut$$,
$italic indParCorOut$$, $italic indParCVOut$$ and $italic indParCIOut$$
will be evaluated at these estimates.
The $italic values of indPar$$ should be obtained by calling the SPK function
$xref/fitIndividual//fitIndividual/$$.

$syntax/

/dataMean_indPar/
/$$
The $code SPK_VA::valarray<double>$$ $italic dataMean_indPar$$ is the mean
of data evaluated at $italic indPar$$.

$syntax/

/dataVariance_indPar/
/$$
The $code SPK_VA::valarray<double>$$ $italic dataVariance_indPar$$ is
the value of the derivative of the variance of data with respect to the
individual parameter, evaluated at $italic indPar$$.

$syntax/

/dataVarianceInv/
/$$
The $code SPK_VA::valarray<double>$$ $italic dataVarianceInv$$is
the value of the inverse of the variance of data evaluated at $italic indPar$$.

$syntax/

/indParCovOut/
/$$
If $italic indParCovOut$$ is not $code NULL$$, then the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCovOut$$
must be declared in the function that calls this function, and its size must
be equal to the square of the length of the individual parameter vector
$math%b%$$.  If $italic indParCovOut$$ is not $code NULL$$ and this function
completed successfully, then the $code SPK_VA::valarray<double>$$ object
pointed to by $italic indParCovOut$$ will contain the covariance matrix
of the individual parameter estimates, in column major order, that is evaluated
at $italic indPar$$.  Otherwise, this function will not attempt to change the
contents of the $code SPK_VA::valarray<double>$$ object pointed to by
$italic indParCovOut$$.

The $math%(i,j)%$$-the element of the covariance matrix
will be replaced by NaN if $code indParMask[i]$$ or $code indParMask[j]$$ is $math%false%$$.

$syntax/

/indParSEOut/
/$$
If $italic indParSEOut$$ is not $code NULL$$, then the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParSEOut$$
must be declared in the function that calls this function, and its size must
be equal to the length of the individual parameter vector
$math%b%$$.  If $italic indParSEOut$$ is not $code NULL$$ and this function
completed successfully, then the $code SPK_VA::valarray<double>$$ object
pointed to by $italic indParSEOut$$ will contain the standard error vector
of the individual parameter estimates, in column major order, that is evaluated
at $italic indPar$$.  Otherwise, this function will not attempt to change the
contents of the $code SPK_VA::valarray<double>$$ object pointed to by
$italic indParSEOut$$.

The $math%i%$$-th element of the standard error vector
will be replaced by NaN if $code indParMask[i]$$ is $math%false%$$.

$syntax/

/indParCorOut/
/$$
If $italic indParCorOut$$ is not $code NULL$$, then the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCorOut$$
must be declared in the function that calls this function, and its size must
be equal to the square of the length of the individual parameter vector
$math%b%$$.  If $italic indParCorOut$$ is not $code NULL$$ and this function
completed successfully, then the $code SPK_VA::valarray<double>$$ object
pointed to by $italic indParCorOut$$ will contain the correlation matrix
of the individual parameter estimates, in column major order, that is evaluated
at $italic indPar$$.  Otherwise, this function will not attempt to change the
contents of the $code SPK_VA::valarray<double>$$ object pointed to by
$italic indParCorOut$$.

The $math%(i, j)%$$-th element of the correlation matrix
will be replaced by NaN if $code indParMask[i]$$ or $code indParMask[j]$$ is $math%false%$$.

$syntax/

/indParCVOut/
/$$
If $italic indParCVOut$$ is not $code NULL$$, then the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCVOut$$
must be declared in the function that calls this function, and its size must
be equal to the length of the individual parameter vector
$math%b%$$.  If $italic indParCVOut$$ is not $code NULL$$ and this function
completed successfully, then the $code SPK_VA::valarray<double>$$ object
pointed to by $italic indParCVOut$$ will contain the standard error vector
of the individual parameter estimates, in column major order, that is evaluated
at $italic indPar$$.  Otherwise, this function will not attempt to change the
contents of the $code SPK_VA::valarray<double>$$ object pointed to by
$italic indParCVOut$$.

The $math%i%$$-th element of the coefficient vector
will be replaced by NaN if $code indParMask[i]$$ is $math%false%$$.

$syntax/

/indParCIOut/
/$$
If $italic indParCIOut$$ is not $code NULL$$, then the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCIOut$$
must be declared in the function that calls this function, and its size must
be equal to the two times of the length of the individual parameter vector
$math%b%$$.  If $italic indParCIOut$$ is not $code NULL$$ and this function
completed successfully, then the $code SPK_VA::valarray<double>$$ object pointed
to by $italic indParCIOut$$ will contain the 95% confidence interval values
of the individual parameter estimates, in column major order, that is evaluated
at $italic indPar$$.  There are two columns in the object.  The first column
contains the lower limit, and the second column contains the upper limit of
the confidence interval of the individual parameter estimates.  Otherwise,
this function will not attempt to change the contents of the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCIOut$$.
Note that in the calculation of the confidence interval, if the degree of freedom
(number of data - number of parameters) is greater than 120 it is treated as infinite.

The $math%(i,1)%$$ and $math%(i,2)%$$ elements of the confidence interval matrix
will be replaced by NaN if $code indParMask[i]$$ is $math%false%$$.

$end
*/

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void indStatistics( const valarray<bool>&    indParMask,
                    const valarray<double>&  indPar,
                    const valarray<double>&  dataMean_indPar,
                    const valarray<double>&  dataVariance_indPar,
                    const valarray<double>&  dataVarianceInv,
                    valarray<double>*        indParCovOut,
                    valarray<double>*        indParSEOut,
                    valarray<double>*        indParCorOut,
                    valarray<double>*        indParCVOut,
                    valarray<double>*        indParCIOut )
{
  //----------------------------------------------------------------
  // Preliminaries.
  //----------------------------------------------------------------

  using namespace std;

  // Return if there are no output values to compute.
  if ( indParCovOut == 0 &&
       indParSEOut  == 0 &&
       indParCorOut == 0 &&
       indParCVOut  == 0 &&
       indParCIOut  == 0 )
  {
    return;
  }

  // Number of individual parameters.
  const int nB = indPar.size();

  // Number of data points.
  const int nY = static_cast<int>(
    sqrt( static_cast<double>( dataVarianceInv.size() ) ) );


  //----------------------------------------------------------------
  // Validate the inputs.
  //----------------------------------------------------------------

  // Check the number of parameters.
  if ( nB < 1 )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The number of parameters must be greater than zero.",
      __LINE__,
      __FILE__ );
  }

  // Check the size of the parameter mask.
  if ( indParMask.size() != nB )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The length of the parameter mask must match the number of parameters.",
      __LINE__,
      __FILE__ );
  }

  // Check the number of data values.
  if ( nY < 1 )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The number of measurements must be greater than zero.",
      __LINE__,
      __FILE__ );
  }

  // Check the derivative of the data mean.
  if ( dataMean_indPar.size() != nY * nB )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The derivative of the data mean has the wrong dimensions.",
      __LINE__,
      __FILE__ );
  }

  // Check the derivative of the data variance.
  if ( dataVariance_indPar.size() != nY * nY * nB )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The derivative of the data variance has the wrong length.",
      __LINE__,
      __FILE__ );
  }

  // Check the data variance inverse.
  if ( dataVarianceInv.size() != nY * nY )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The data variance inverse has the wrong length.",
      __LINE__,
      __FILE__ );
  }


  //----------------------------------------------------------------
  // Prepare to calculate the statistics.
  //----------------------------------------------------------------

  // Instantiate a model that returns the SpkModel functions required
  // to calculate the statistics: f_b, R_b, and RInv.
  DeprecatedModel deprecatedModel(
    &dataMean_indPar,
    &dataVariance_indPar,
    &dataVarianceInv );

  // Set the formulation for the covariance matrix of the parameter
  // estimates.
  const IndCovForm formulation = H;

  // This must be false because this function does not have access to
  // covariance of the individual parameters D that is required if this
  // flag is equal to true.
  bool withD = false;

  // Create a dummy data vector of all zeroes.
  valarray<double> yDummy( nY );
  yDummy = 0.0;

  // Create a dummy individual objective Hessian of all zeroes.
  valarray<double> indObj_indPar_indParDummy( nB * nB );
  indObj_indPar_indParDummy = 0.0;


  //----------------------------------------------------------------
  // Calculate the statistics.
  //----------------------------------------------------------------

  indStatistics( deprecatedModel,
                 yDummy,
                 indPar,
                 indParMask,
                 indObj_indPar_indParDummy,
                 formulation,
                 indParCovOut,
                 indParSEOut,
                 indParCorOut,
                 indParCVOut,
                 indParCIOut,
                 withD );

}


/*************************************************************************
 *
 * Function: indStatistics - all elements must be active (Deprecated)
 *
 *
 * This function is deprecated because its interface takes model
 * functions which means the interface must change if new model
 * functions are required to calculate the statistics.
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin indStatistics_AllElemActive_Deprecated$$

$spell
  Model model
  valarray
  Cov
  Obj
  enum
  Laplace
  subvector
  dmat
  const
  dvec
  int
  cout
  endl
  nr
  nc
  iostream
  iomanip
  namespace
  std
  ios
  covariance
  ind
  cerr
  Spk
  inv
  optimizer
  fp
  Optimizer optimizer
  Fo
  Dir
  Yi
  inx
  aval
  bval
  resize
  bool
  Dinv
  Rinv
  var
  sqrt
  cbc
  covariances
  cor
  cmath
  statistics
  confint
  exp
  Beal
  Raton
  Ruppert
  Sheiner
  Stefanski
$$

$section Computing Statistics of Individual Parameter Estimates when All Elements are Active (Deprecated)$$

$index indStatistics, coefficient of variation, confidence interval$$
$index covariance, standard error, correlation matrix, individual parameters$$
$cindex \Computing Statistics \of Individual \Parameter \Estimates \when All \Elements \are \Active \(Deprecated)$$

$table
$bold Prototype:$$ $cend
$syntax/void indStatistics( const SPK_VA::valarray<double>& /indPar/,
                    const SPK_VA::valarray<double>& /dataMean_indPar/,
                    const SPK_VA::valarray<double>& /dataVariance_indPar/,
                    const SPK_VA::valarray<double>& /dataVarianceInv/,
                    SPK_VA::valarray<double>*       /indParCovOut/,
                    SPK_VA::valarray<double>*       /indParSEOut/,
                    SPK_VA::valarray<double>*       /indParCorOut/,
                    SPK_VA::valarray<double>*       /indParCVOut/,
                    SPK_VA::valarray<double>*       /indParCIOut/ )
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
$pre

$$
$bold Note:  this function is deprecated.$$
$pre

$$
This function computes the covariance matrix, standard error vector,
correlation matrix, coefficient of variation vector, and confidence
interval vector for individual parameter estimates.
$pre

$$
This function uses the following formulation to calculate the
covariance matrix of the individual parameter estimates:
$math%

                                      -1
    formulation "R":    cov[ b ]  =  R    .

%$$
This formulation is discussed in Section (D.2.5) of the NONMEM
Users Guide Part II and in Sections (A.2.1) and (A.2.2) in
Carroll, Ruppert, and Stefanski (1998).
$pre

$$
The approximation made for the information matrix is
$math%

     R  =  H(b)  ,

%$$
where the expected value for the Hessian of the individual
objective function is defined in Section (7.) of Bell (2001) as
$math%

                     T  -1                1        T        -1      -1
     H(b)  =  d  f(b)  R  (b) d  f(b)  +  - d  R(b)  kron[ R  (b), R  (b) ] d  R(b)  .
               b               b          2  b                               b

%$$
Note that the $math%R(b)%$$ on the right hand side of this
equation is the model for the covariance of an individual's data,
which is part of $xref/SpkModel//SpkModel/$$ and
is different than the information matrix $math%R%$$.
$pre

$$
The standard error vector is calculated by taking the square roots
of the diagonal elements of the covariance matrix.
The correlation matrix is calculated by dividing each element of
the covariance matrix by the standard errors that correspond to its
row and column.
The coefficients of variation are calculated as
$math%

    CV    =  SE    / | b    | * 100   ,
     (i)       (i)      (i)

%$$
where CV is the coefficient of variation and SE is the standard error.
The 95% confidence intervals are calculated as
$math%

    ( b    -  t               * SE    ,  b    +  t               * SE    ) ,
       (i)     0.025, degFree     (i)     (i)     0.025, degFree     (i)

%$$
where
$math%

    t
     0.025, degFree

%$$
is the critical value for the $math%t%$$ distribution with
$math%degFree = nY - nB%$$ number of degrees of freedom for
which the area under the $math%t%$$ curve is $math%1 - 0.025%$$.

$head Reference$$
Beal, S. L. and Sheiner, L. B. (1988) $italic NONMEM Users Guide - Part II$$,
University of California, San Francisco.
$pre

$$
Bell, B. M. (2001) Approximating the marginal likelihood estimate for
models with random parameters, $italic Applied Mathematics and Computation$$,
$bold 119$$, 57-75.
$pre

$$
Carroll, R. J., Ruppert, D., and Stefanski, L. A. (1998)
$italic Measurement Error in Nonlinear Models$$, Chapman & Hall/CRC,
Boca Raton, Florida.

$head Return Value$$
Upon a successful completion, the function sets
the given output value place holders to point to the result values.

$pre

$$
If an error is detected or failure occurs during the evaluation, a SpkException
object is thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/

/indPar/
/$$
The $code SPK_VA::valarray<double>$$ $italic indPar$$ contains the vector
$math%b%$$, which specifies the estimates of the individual parameters.
The returned values $italic indParCovOut$$, $italic indParSEOut$$,
$italic indParCorOut$$, $italic indParCVOut$$ and $italic indParCIOut$$
will be evaluated at these estimates.
The $italic values of indPar$$ should be obtained by calling the SPK function
$xref/fitIndividual//fitIndividual/$$.

$syntax/

/dataMean_indPar/
/$$
The $code SPK_VA::valarray<double>$$ $italic dataMean_indPar$$ is the mean
of data evaluated at $italic indPar$$.

$syntax/

/dataVariance_indPar/
/$$
The $code SPK_VA::valarray<double>$$ $italic dataVariance_indPar$$ is
the value of the derivative of the variance of data with respect to the
individual parameter, evaluated at $italic indPar$$.

$syntax/

/dataVarianceInv/
/$$
The $code SPK_VA::valarray<double>$$ $italic dataVarianceInv$$ is
the value of the inverse of the variance of data evaluated at $italic indPar$$.

$syntax/

/indParCovOut/
/$$
If $italic indParCovOut$$ is not $code NULL$$, then the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCovOut$$
must be declared in the function that calls this function, and its size must
be equal to the square of the length of the individual parameter vector
$math%b%$$.  If $italic indParCovOut$$ is not $code NULL$$ and this function
completed successfully, then the $code SPK_VA::valarray<double>$$ object
pointed to by $italic indParCovOut$$ will contain the covariance matrix
of the individual parameter estimates, in column major order, that is evaluated
at $italic indPar$$.  Otherwise, this function will not attempt to change the
contents of the $code SPK_VA::valarray<double>$$ object pointed to by
$italic indParCovOut$$.

$syntax/

/indParSEOut/
/$$
If $italic indParSEOut$$ is not $code NULL$$, then the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParSEOut$$
must be declared in the function that calls this function, and its size must
be equal to the length of the individual parameter vector
$math%b%$$.  If $italic indParSEOut$$ is not $code NULL$$ and this function
completed successfully, then the $code SPK_VA::valarray<double>$$ object
pointed to by $italic indParSEOut$$ will contain the standard error vector
of the individual parameter estimates, in column major order, that is evaluated
at $italic indPar$$.  Otherwise, this function will not attempt to change the
contents of the $code SPK_VA::valarray<double>$$ object pointed to by
$italic indParSEOut$$.

$syntax/

/indParCorOut/
/$$
If $italic indParCorOut$$ is not $code NULL$$, then the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCorOut$$
must be declared in the function that calls this function, and its size must
be equal to the square of the length of the individual parameter vector
$math%b%$$.  If $italic indParCorOut$$ is not $code NULL$$ and this function
completed successfully, then the $code SPK_VA::valarray<double>$$ object
pointed to by $italic indParCorOut$$ will contain the correlation matrix
of the individual parameter estimates, in column major order, that is evaluated
at $italic indPar$$.  Otherwise, this function will not attempt to change the
contents of the $code SPK_VA::valarray<double>$$ object pointed to by
$italic indParCorOut$$.

$syntax/

/indParCVOut/
/$$
If $italic indParCVOut$$ is not $code NULL$$, then the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCVOut$$
must be declared in the function that calls this function, and its size must
be equal to the length of the individual parameter vector
$math%b%$$.  If $italic indParCVOut$$ is not $code NULL$$ and this function
completed successfully, then the $code SPK_VA::valarray<double>$$ object
pointed to by $italic indParCVOut$$ will contain the standard error vector
of the individual parameter estimates, in column major order, that is evaluated
at $italic indPar$$.  Otherwise, this function will not attempt to change the
contents of the $code SPK_VA::valarray<double>$$ object pointed to by
$italic indParCVOut$$.

$syntax/

/indParCIOut/
/$$
If $italic indParCIOut$$ is not $code NULL$$, then the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCIOut$$
must be declared in the function that calls this function, and its size must
be equal to the two times of the length of the individual parameter vector
$math%b%$$.  If $italic indParCIOut$$ is not $code NULL$$ and this function
completed successfully, then the $code SPK_VA::valarray<double>$$ object pointed
to by $italic indParCIOut$$ will contain the 95% confidence interval values
of the individual parameter estimates, in column major order, that is evaluated
at $italic indPar$$.  There are two columns in the object.  The first column
contains the lower limit, and the second column contains the upper limit of
the confidence interval of the individual parameter estimates.  Otherwise,
this function will not attempt to change the contents of the
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCIOut$$.
Note that in the calculation of the confidence interval, if the degree of freedom
(number of data - number of parameters) is greater than 120 it is treated as infinite.


$head Example$$
The following demonstrates running indStatistics() in the single processing mode.

$codep

#include <iostream>
#include <cmath>
#include "SpkModel.h"
#include "indStatistics.h"
#include "printInMatrix.h"
#include "fitIndividual.h"
#include "SpkValarray.h"

using namespace std;

/*------------------------------------------------------------------------
 * Class Definition
 *------------------------------------------------------------------------*/
/*
class UserModelIndStatisticsExampleTest : public SpkModel
{
    valarray<double> _b;
public:
    UserModelIndStatisticsExampleTest(){};
    ~UserModelIndStatisticsExampleTest(){};
private:
    void doSetIndPar(const valarray<double>& bval)
    {
        _b = bval;
    }
    void doDataMean( valarray<double>& ret ) const
    {
//
//            / b(2) \
//     f(b) = | b(2) |
//            \ b(2) /
//
        ret.resize( 3, _b[1] );
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
//
//              / 0   1 \
//     f_b(b) = | 0   1 |
//              \ 0   1 /
//
        ret.resize( 6, 0.0 );
        ret[3] = 1.0;
        ret[4] = 1.0;
        ret[5] = 1.0;

        return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
//
//            /  exp[b(1)]     0         0     \
//     R(b) = |      0     exp[b(1)]     0     |
//            \      0         0     exp[b(1)] /
//
        ret.resize( 9, 0.0 );
        ret[0] = exp( _b[0] );
        ret[4] = exp( _b[0] );
        ret[8] = exp( _b[0] );
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
//
//              /  exp[b(1)]     0  \
//     R_b(b) = |  0             0  |
//              |  0             0  |
//              |  0             0  |
//              |  exp[b(1)]     0  |
//              |  0             0  |
//              |  0             0  |
//              |  0             0  |
//              \  exp[b(1)]     0  /
//
        ret.resize( 18, 0.0 );
        ret[0] = exp( _b[0] );
        ret[4] = exp( _b[0] );
        ret[8] = exp( _b[0] );

        return true;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
//
//            /  1.0     0   \
//     D(b) = |              |
//            \  0       0.5 /
//
        ret.resize( 4 );
        ret[0] = 1.0;
        ret[1] = 0.0;
        ret[2] = 0.0;
        ret[3] = 0.5;
    }
};

int main()
{
    //------------------------------------------------------------
    // Preliminaries.
    //------------------------------------------------------------

    using namespace std;

    //------------------------------------------------------------
    // Quantities related to the user-provided model.
    //------------------------------------------------------------

    UserModelIndStatisticsExampleTest model;

    //------------------------------------------------------------
    // Quantities related to the data vector, y.
    //------------------------------------------------------------

    int nY = 3;
    valarray<double> Y(nY );
    Y[ 0 ] = 1.8;
    Y[ 1 ] = 2.0;
    Y[ 2 ] = 2.2;

    //------------------------------------------------------------
    // Quantities related to the objective function parameter, b.
    //------------------------------------------------------------

    int nB = 2;

    valarray<double> indParLow ( -4.0, nB );
    valarray<double> indParUp  (  4.0, nB );
    valarray<double> indParIn  (  2.0, nB );
    valarray<double> indParOut (       nB );
    valarray<double> indParStep( .001, nB );


    //------------------------------------------------------------
    // Quantities related to the objective function, MapObj(b).
    //------------------------------------------------------------

    double MapObjOut;

    valarray<double> MapObj_bOut  ( nB );
    valarray<double> MapObj_b_bOut( nB * nB );


    //------------------------------------------------------------
    // Remaining inputs to fitIndividual.
    //------------------------------------------------------------

    Optimizer indOptimizer( 1.e-3, 40, 0 );
    bool withD      = false;

    //------------------------------------------------------------
    // Optimize MapObj(b).
    //------------------------------------------------------------

    try
    {
        fitIndividual( model,
                       Y,
                       indOptimizer,
                       indParLow,
                       indParUp,
                       indParIn,
                       indParStep,
                       &indParOut,
                       &MapObjOut,
                       &MapObj_bOut,
                       &MapObj_b_bOut,
                       withD );
    }
    catch(...)
    {
        cerr << "fitIndividual failed" << endl;
    }

    valarray<double> dataMean_indPar( nB );
    valarray<double> dataVariance_indPar( nY * nY * nB );
    valarray<double> dataVarianceInv( nY * nY );

    model.dataMean_indPar( dataMean_indPar );
    model.dataVariance_indPar( dataVariance_indPar );
    model.dataVarianceInv( dataVarianceInv );

    //------------------------------------------------------------
    // Compute statistics of individual parameter estimates.
    //------------------------------------------------------------

    valarray<double> indParCovOut( nB * nB );
    valarray<double> indParSEOut ( nB      );
    valarray<double> indParCorOut( nB * nB );
    valarray<double> indParCVOut ( nB      );
    valarray<double> indParCIOut ( nB *  2 );

    try
    {
        indStatistics( indParOut,
                       dataMean_indPar,
                       dataVariance_indPar,
                       dataVarianceInv,
                       &indParCovOut,
                       &indParSEOut,
                       &indParCorOut,
                       &indParCVOut,
                       &indParCIOut );
    }
    catch(...)
    {
        cerr << "indStatistics failed" << endl;
        return 0;
    }

    cout << "indParOut = " << endl;
    printInMatrix( indParOut, 1 );
    cout << "indParCovOut = " << endl;
    printInMatrix( indParCovOut, nB );
    cout << "indParSEOut = " << endl;
    printInMatrix( indParSEOut, 1 );
    cout << "indParCVOut = " << endl;
    printInMatrix( indParCVOut, 1 );
    cout << "indParCorOut = " << endl;
    printInMatrix( indParCorOut, nB );
    cout << "indParCIOut = " << endl;
    printInMatrix( indParCIOut, 2 );
    return 0;
}
$$
The program will display the following when it is run:
$codep

indParOut =
[ -3.62422 ]
[ 2 ]
indParCovOut =
[ 0.666667 0 ]
[ 0 0.00889 ]
indParSEOut =
[ 0.816497 ]
[ 0.0942868 ]
indParCVOut =
[ -22.5289 ]
[ 4.71434 ]
indParCorOut =
[ 1 0 ]
[ 0 1 ]
indParCIOut =
[ -13.9986 6.75019 ]
[ 0.801992 3.19801 ]

$$

$end
*/


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void indStatistics( const valarray<double>&  indPar,
                    const valarray<double>&  dataMean_indPar,
                    const valarray<double>&  dataVariance_indPar,
                    const valarray<double>&  dataVarianceInv,
                    valarray<double>*        indParCovOut,
                    valarray<double>*        indParSEOut,
                    valarray<double>*        indParCorOut,
                    valarray<double>*        indParCVOut,
                    valarray<double>*        indParCIOut )
{
  //----------------------------------------------------------------
  // Preliminaries.
  //----------------------------------------------------------------

  using namespace std;

  // Return if there are no output values to compute.
  if ( indParCovOut == 0 &&
       indParSEOut  == 0 &&
       indParCorOut == 0 &&
       indParCVOut  == 0 &&
       indParCIOut  == 0 )
  {
    return;
  }

  // Number of individual parameters.
  const int nB = indPar.size();

  // Number of data points.
  const int nY = static_cast<int>(
    sqrt( static_cast<double>( dataVarianceInv.size() ) ) );


  //----------------------------------------------------------------
  // Validate the inputs.
  //----------------------------------------------------------------

  // Check the number of parameters.
  if ( nB < 1 )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The number of parameters must be greater than zero.",
      __LINE__,
      __FILE__ );
  }

  // Check the number of data values.
  if ( nY < 1 )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The number of measurements must be greater than zero.",
      __LINE__,
      __FILE__ );
  }

  // Check the derivative of the data mean.
  if ( dataMean_indPar.size() != nY * nB )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The derivative of the data mean has the wrong dimensions.",
      __LINE__,
      __FILE__ );
  }

  // Check the derivative of the data variance.
  if ( dataVariance_indPar.size() != nY * nY * nB )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The derivative of the data variance has the wrong length.",
      __LINE__,
      __FILE__ );
  }

  // Check the data variance inverse.
  if ( dataVarianceInv.size() != nY * nY )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,
      "The data variance inverse has the wrong length.",
      __LINE__,
      __FILE__ );
  }


  //----------------------------------------------------------------
  // Prepare to calculate the statistics.
  //----------------------------------------------------------------

  // Instantiate a model that returns the SpkModel functions required
  // to calculate the statistics: f_b, R_b, and RInv.
  DeprecatedModel deprecatedModel(
    &dataMean_indPar,
    &dataVariance_indPar,
    &dataVarianceInv );

  // Create an individual parameter mask with all true's to indicate
  // that none of the individual parameter elements are constrained.
  valarray<bool> indParMask( nB );
  indParMask = true;

  // Set the formulation for the covariance matrix of the parameter
  // estimates.
  const IndCovForm formulation = H;

  // This must be false because this function does not have access to
  // covariance of the individual parameters D that is required if this
  // flag is equal to true.
  bool withD = false;

  // Create a dummy data vector of all zeroes.
  valarray<double> yDummy( nY );
  yDummy = 0.0;

  // Create a dummy individual objective Hessian of all zeroes.
  valarray<double> indObj_indPar_indParDummy( nB * nB );
  indObj_indPar_indParDummy = 0.0;


  //----------------------------------------------------------------
  // Calculate the statistics.
  //----------------------------------------------------------------

  indStatistics( deprecatedModel,
                 yDummy,
                 indPar,
                 indParMask,
                 indObj_indPar_indParDummy,
                 formulation,
                 indParCovOut,
                 indParSEOut,
                 indParCorOut,
                 indParCVOut,
                 indParCIOut,
                 withD );

}


