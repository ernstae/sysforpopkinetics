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
 * File: derParStatistics.cpp
 *
 *
 * Calculates statistics for a parameter vector that is derived from
 * another parameter vector with a known covariance matrix.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: derParStatistics
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin derParStatistics$$

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
  Cramer-Rao
  deg
$$

$section Computing Statistics of Derived Parameter Estimates$$

$index derParStatistics$$
$index derived parameter, covariance, standard error, correlation matrix$$

$table
$bold Prototype:$$ $cend
$syntax/void derParStatistics(
  const SPK_VA::valarray<double>& /xCov/,
  const SPK_VA::valarray<double>& /z/,
  const SPK_VA::valarray<double>& /z_x/,
  int                             /nDegFreedom/,
  SPK_VA::valarray<double>*       /zCovOut/,
  SPK_VA::valarray<double>*       /zSEOut/,
  SPK_VA::valarray<double>*       /zCorOut/,
  SPK_VA::valarray<double>*       /zCVOut/,
  SPK_VA::valarray<double>*       /zCIOut/ );
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

This function calculates statistics for a parameter vector z(x) that
is derived from another parameter vector x with a known covariance
matrix.
The statistics for z(x) that can be calculated are its standard error
vector, correlation matrix, coefficient of variation vector, and
confidence interval vector.
$pre

$$
The covariance matrix for the derived parameter is calculated as
$math%
                     -          -              -          -  T
                    |            |            |            |
    Cov[ z(x) ]  =  |  d   z(x)  |  Cov[ x ]  |  d   z(x)  |    .
                    |   x        |            |   x        |
                     -          -              -          - 

%$$
The standard error vector is calculated by taking the square roots 
of the diagonal elements of this covariance matrix.
The correlation matrix is calculated by dividing each element of 
the covariance matrix by the standard errors that correspond to its 
row and column.
The coefficients of variation are calculated as
$math%
   
    CV    =  SE    / | z(x)    | * 100   ,
     (i)       (i)         (i)

%$$
where CV is the coefficient of variation and SE is the standard error.  
The 95% confidence intervals are calculated as
$math%
   
    ( z(x)    -  t               * SE    ,  z(x)    +  t               * SE    ) ,
         (i)     0.025, degFree     (i)         (i)     0.025, degFree     (i)

%$$
where
$math%
   
    t
     0.025, degFree

%$$
is the critical value for the $math%t%$$ distribution with 
$math%degFree%$$ number of degrees of freedom for which the 
area under the $math%t%$$ curve is $math%1 - 0.025%$$.
$pre

$$
This function assumes that x and z(x) have the same number of
elements, n.

$head Return Value$$
Upon a successful completion, the function sets the given output
value place holders to point to the result values.
  
$pre

$$
If an error is detected or failure occurs during the evaluation, an
SpkException object will be thrown.  
The state after an exception is thrown is defined in 
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/

/xCov/
/$$
is an n * n dimensional vector that contains the covariance matrix for
x in column major order.

$syntax/

/z/
/$$
is the n dimensional parameter vector z(x) that is derived from the
parameter vector x.  
Note that the number of elements in z specifies n.

$syntax/

/z_x/
/$$
is an n * n dimensional vector that contains the derivative with
respect to x of the derived variable z(x) in column major order.

$syntax/

/nDegFreedom/
/$$
is the number of degrees of freedom that is used to compute the
confidence intervals.  It should be set equal to m - n, where m is the
number of fitted data points.  The value must be greater than zero,
although it will not be referenced at all if zCIOut is set to NULL.

$syntax/

/zCovOut/
/$$
will point to an n * n dimensional vector containing the covariance
matrix for the derived parameter Cov[z(x)] in column major order if
the pointer points to an n * n valarray.
If it points to NULL, it will remain unchanged.
If it points to a valarray sized other than n * n, the resulting
behavior is undetermined.

$syntax/

/zSEOut/
/$$
will point to an n dimensional vector containing the standard errors
for z(x) if the pointer points to an n dimensional valarray.  
If it points to NULL, it will remain unchanged.
If it points to a valarray sized other than n, the resulting behavior
is undetermined.

$syntax/

/zCorOut/
/$$
will point to an n * n dimensional vector containing the correlation
matrix for the derived parameter Cor[z(x)] in column major order if
the pointer points to an n * n valarray.
If it points to NULL, it will remain unchanged.
If it points to a valarray sized other than n * n, the resulting
behavior is undetermined.

$syntax/

/zCVOut/
/$$
will point to an n dimensional vector containing the coefficients of variation
for z(x) if the pointer points to an n dimensional valarray.  
If it points to NULL, it will remain unchanged.
If it points to a valarray sized other than n, the resulting behavior
is undetermined.

$syntax/

/zCIOut/
/$$

will point to an n * 2 dimensional vector containing the 95%
confidence intervals if the pointer points to an n * 2 dimensional
valarray.
The first n elements are the lower bounds for the intervals, 
while the last n elements are the upper bounds.
If it points to NULL, it will remain unchanged.
If it points to a valarray sized other than n * 2, the resulting
behavior is undetermined.

$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include "derParStatistics.h"
#include "multiply.h"
#include "SpkException.h"
#include "SpkValarray.h"
#include "statistics.h"
#include "symmetrize.h"
#include "transpose.h"

using SPK_VA::valarray;
using namespace std;


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void derParStatistics(
  const SPK_VA::valarray<double>& xCov,
  const SPK_VA::valarray<double>& z,
  const SPK_VA::valarray<double>& z_x,
  int                             nDegFreedom,
  SPK_VA::valarray<double>*       zCovOut,
  SPK_VA::valarray<double>*       zSEOut,
  SPK_VA::valarray<double>*       zCorOut,
  SPK_VA::valarray<double>*       zCVOut,
  SPK_VA::valarray<double>*       zCIOut )
{
  //----------------------------------------------------------------
  // Preliminaries.
  //----------------------------------------------------------------

  // Return if there are no output values to compute.
  if( zCovOut == 0 && 
      zSEOut  == 0 && 
      zCorOut == 0 && 
      zCVOut  == 0 && 
      zCIOut  == 0 )
  {
    return;
  }

  // Get the number of parameters in z and, therefore, x.
  const int nZ = z.size();


  //----------------------------------------------------------------
  // Validate the inputs.
  //----------------------------------------------------------------

  if ( nZ < 1 )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,  
      "The number of derived parameters must be greater than zero.",
      __LINE__, 
      __FILE__ );
  }
    
  if( xCov.size() != nZ * nZ )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,  
      "The covariance matrix for the original parameter has the wrong dimensions.",
      __LINE__, 
      __FILE__ );
  }

  if( z_x.size() != nZ * nZ )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,  
      "The derivative of the derived parameter has the wrong dimensions.",
      __LINE__, 
      __FILE__ );
  }

  if ( zCIOut && nDegFreedom < 1 )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,  
      "The number of degrees of freedom must be greater than zero.",
      __LINE__, 
      __FILE__ );
  }


  //----------------------------------------------------------------
  // Calculate the covariance of the derived parameter.
  //----------------------------------------------------------------

  valarray<double> zCovTemp    ( nZ * nZ );
  valarray<double> zCovNonSymm ( nZ * nZ );
  valarray<double> z_xTimesXCov( nZ * nZ );
  valarray<double> z_xTrans    ( nZ * nZ );
 
  z_xTrans = transpose( z_x, nZ );

  // Calculate
  //                     -          -              -          -  T
  //                    |            |            |            |
  //    Cov[ z(x) ]  =  |  d   z(x)  |  Cov[ x ]  |  d   z(x)  |    .
  //                    |   x        |            |   x        |
  //                     -          -              -          - 
  //
  z_xTimesXCov = multiply( z_x,          nZ, xCov,     nZ );
  zCovNonSymm  = multiply( z_xTimesXCov, nZ, z_xTrans, nZ );

  // Force the covariance matrix to be symmetric to eliminate any
  // nonsymmetries that may have been introduced by roundoff.
  symmetrize( zCovNonSymm, nZ, zCovTemp );


  //----------------------------------------------------------------
  // Calculate the rest of the statistics for the derived parameter.
  //----------------------------------------------------------------

  statistics(
    z,
    zCovTemp,
    nDegFreedom,
    zSEOut,
    zCorOut,
    zCVOut,
    zCIOut );


  //----------------------------------------------------------------
  // Finish up.
  //----------------------------------------------------------------

  // Set the covariance of the derived parameter, if necessary.
  if ( zCovOut )
  {
    *zCovOut = zCovTemp;
  }

}
/*************************************************************************
 *
 * Function: derParStatistics - allows fixed elements
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*
$begin derParStatisticsFixedElem$$

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
  Cramer-Rao
  deg
$$

$section Computing Statistics of Derived Parameter Estimates when Some Elements are Fixed$$

$index derParStatistics$$
$index derived parameter, covariance, standard error, correlation matrix$$

$table
$bold Prototype:$$ $cend
$syntax/void derParStatistics( const SPK_VA::valarray<bool>   & /xMask/,
                       const SPK_VA::valarray<bool>   & /zMask/,
                       const SPK_VA::valarray<double> & /xCov/,
                       const SPK_VA::valarray<double> & /z/,
                       const SPK_VA::valarray<double> & /z_x/,
                       int                              /nDegOfFreedom/,
                       SPK_VA::valarray<double>       * /zCovOut/,
                       SPK_VA::valarray<double>       * /zInvCovOut/,
                       SPK_VA::valarray<double>       * /zSEOut/,
                       SPK_VA::valarray<double>       * /zCorOut/,
                       SPK_VA::valarray<double>       * /zCVOut/,
                       SPK_VA::valarray<double>       * /zCIOut/
                      )
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

This function calculates statistics for a parameter vector z(x) that
is derived from another parameter vector x with a known covariance
matrix.
The statistics for z(x) that can be calculated are its standard error
vector, correlation matrix, coefficient of variation vector, and
confidence interval vector.
$pre

$$
The covariance matrix for the derived parameter is calculated as
$math%
                     -          -              -          -  T
                    |            |            |            |
    Cov[ z(x) ]  =  |  d   z(x)  |  Cov[ x ]  |  d   z(x)  |    .
                    |   x        |            |   x        |
                     -          -              -          - 

%$$
The standard error vector is calculated by taking the square roots 
of the diagonal elements of this covariance matrix.
The correlation matrix is calculated by dividing each element of 
the covariance matrix by the standard errors that correspond to its 
row and column.
The coefficients of variation are calculated as
$math%
   
    CV    =  SE    / | z(x)    | * 100   ,
     (i)       (i)         (i)

%$$
where CV is the coefficient of variation and SE is the standard error.  
The 95% confidence intervals are calculated as
$math%
   
    ( z(x)    -  t               * SE    ,  z(x)    +  t               * SE    ) ,
         (i)     0.025, degFree     (i)         (i)     0.025, degFree     (i)

%$$
where
$math%
   
    t
     0.025, degFree

%$$
is the critical value for the $math%t%$$ distribution with 
$math%degFree%$$ number of degrees of freedom for which the 
area under the $math%t%$$ curve is $math%1 - 0.025%$$.
$pre

$$
This function assumes that x and z(x) have the same number of
elements, n.

$head Return Value$$
Upon a successful completion, the function sets the given output
value place holders to point to the result values.
  
$pre

$$
If an error is detected or failure occurs during the evaluation, an
SpkException object will be thrown.  
The state after an exception is thrown is defined in 
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/
/xMask/
/$$
$code xMask$$ is a vector of boolean values of length equal to the parameter
vector, $code x$$.  $code xMask[i]$$ tells as to whether $code x[i]$$
is fixed or not.  If $math%xMask[i]%$$ is $math%false%$$, the i-th element of
the parameter vector are treated as if it does not exist and further 
statistics computations are performed based upon the assumption.

$syntax/
/zMask/
/$$
$code zMask$$ is a vector of boolean values of length equal to the derived parameter
vector, $code z$$.  $code zMask[i]$$ tells as to whether $code z[i]$$
is fixed or not.  If $math%zMask[i]%$$ is $math%false%$$, the i-th element of
the parameter vector are treated as if it does not exist and further 
statistics computations are performed based upon the assumption.

$syntax/

/xCov/
/$$
is an n * n dimensional vector that contains the covariance matrix for
x in column major order.

The $math%(i,j)%$$-the element of the covariance matrix
will be removed from the calculation of the derived statistics if 
$code xMask[i]$$ or $code xMask[j]$$ is $math%false%$$.

$syntax/

/z/
/$$
is the n dimensional parameter vector z(x) that is derived from the
parameter vector x.  
Note that the number of elements in z specifies n.

$syntax/

/z_x/
/$$
is an n * n dimensional vector that contains the derivative with
respect to x of the derived variable z(x) in column major order.

$syntax/

/nDegFreedom/
/$$
is the number of degrees of freedom that is used to compute the
confidence intervals.  It should be set equal to m - n, where m is the
number of fitted data points.  The value must be greater than zero,
although it will not be referenced at all if zCIOut is set to NULL.

$syntax/

/zCovOut/
/$$
will point to an n * n dimensional vector containing the covariance
matrix for the derived parameter Cov[z(x)] in column major order if
the pointer points to an n * n valarray.
If it points to NULL, it will remain unchanged.
If it points to a valarray sized other than n * n, the resulting
behavior is undetermined.

The $math%(i,j)%$$-the element of the covariance matrix
will be replaced by NaN if $code zMask[i]$$ or $code zMask[j]$$ is $math%false%$$.

$syntax/

/zInvCovOut/
/$$
will point to an n * n dimensional vector containing the inverse of the covariance
matrix for the derived parameter Cov[z(x)] in column major order if
the pointer points to an n * n valarray.
If it points to NULL, it will remain unchanged.
If it points to a valarray sized other than n * n, the resulting
behavior is undetermined.

The $math%(i,j)%$$-the element of the inverse of the covariance matrix
will be replaced by NaN if $code zMask[i]$$ or $code zMask[j]$$ is $math%false%$$.

$syntax/

/zSEOut/
/$$
will point to an n dimensional vector containing the standard errors
for z(x) if the pointer points to an n dimensional valarray.  
If it points to NULL, it will remain unchanged.
If it points to a valarray sized other than n, the resulting behavior
is undetermined.

The $math%i%$$-th element of the standard error vector
will be replaced by NaN if $code zMask[i]$$ is $math%false%$$.

$syntax/

/zCorOut/
/$$
will point to an n * n dimensional vector containing the correlation
matrix for the derived parameter Cor[z(x)] in column major order if
the pointer points to an n * n valarray.
If it points to NULL, it will remain unchanged.
If it points to a valarray sized other than n * n, the resulting
behavior is undetermined.

The $math%(i, j)%$$-th element of the correlation matrix
will be replaced by NaN if $code zMask[i]$$ or $code zMask[j]$$ is $math%false%$$.

$syntax/

/zCVOut/
/$$
will point to an n dimensional vector containing the coefficients of variation
for z(x) if the pointer points to an n dimensional valarray.  
If it points to NULL, it will remain unchanged.
If it points to a valarray sized other than n, the resulting behavior
is undetermined.
$pre

$$
The $math%i%$$-th element of the coefficient vector
will be replaced by NaN if $code zMask[i]$$ is $math%false%$$.

$syntax/

/zCIOut/
/$$

will point to an n * 2 dimensional vector containing the 95%
confidence intervals if the pointer points to an n * 2 dimensional
valarray.
The first n elements are the lower bounds for the intervals, 
while the last n elements are the upper bounds.
If it points to NULL, it will remain unchanged.
If it points to a valarray sized other than n * 2, the resulting
behavior is undetermined.

The $math%(i,1)%$$ and $math%(i,2)%$$ elements of the confidence interval matrix
will be replaced by NaN if $code zMask[i]$$ is $math%false%$$.


$end
*/
#include "inverse.h"
namespace
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
    
    for ( i = 0, ii = 0; i < nY; i++ )
    {
      if( mask[i] )
      {
        y[i] = x[ii];
        ii++;
      }
      else
        y[i] = val;
    }
  }

}

void derParStatistics( const SPK_VA::valarray<bool>   & xMask,
                       const SPK_VA::valarray<double> & xCov,
                       const SPK_VA::valarray<bool>   & zMask,
                       const SPK_VA::valarray<double> & z,
                       const SPK_VA::valarray<double> & z_x,
                       int                              nDegOfFreedom,
                       SPK_VA::valarray<double>       * zCovOut,
                       SPK_VA::valarray<double>       * zInvCovOut,
                       SPK_VA::valarray<double>       * zSEOut,
                       SPK_VA::valarray<double>       * zCorOut,
                       SPK_VA::valarray<double>       * zCVOut,
                       SPK_VA::valarray<double>       * zCIOut
                      )
{
  //----------------------------------------------------------------
  // Preliminaries.
  //----------------------------------------------------------------

  // Return if there are no output values to compute.
  if ( zCovOut    == 0 && 
       zInvCovOut == 0 && 
       zSEOut     == 0 && 
       zCorOut    == 0 && 
       zCVOut     == 0 && 
       zCIOut     == 0 )
  {
    return;
  }

  // Get the number of parameters in x and z.
  const int nX = xMask.size();
  const int nZ = zMask.size();


  //----------------------------------------------------------------
  // Validate the inputs.
  //----------------------------------------------------------------

  if ( nZ != nX )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,  
      "The number of derived parameters must be equal to the number of original parameters.",
      __LINE__, 
      __FILE__ );
  }
    
  if ( nZ < 1 )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,  
      "The number of derived parameters must be greater than zero.",
      __LINE__, 
      __FILE__ );
  }
    
  if ( xCov.size() != nX * nX )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,  
      "The covariance matrix for the original parameter has the wrong dimensions.",
      __LINE__, 
      __FILE__ );
  }

  if ( z.size() != nZ )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,  
      "The derived parameter has the wrong dimensions.",
      __LINE__, 
      __FILE__ );
  }

  if ( z_x.size() != nZ * nX )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR,  
      "The derivative of the derived parameter has the wrong dimensions.",
      __LINE__, 
      __FILE__ );
  }


  //----------------------------------------------------------------
  // Get the free parameters vector, covariance, and derivative.
  //----------------------------------------------------------------

  int i;
  int j;
  int ii;
  int jj;

  // Determine the number of parameter elements that are fixed and
  // should be removed from the derived statistics calculation.
  int nXFixed = 0;
  for ( i = 0; i < nX; i++ )
  {
    if ( !xMask[i] )
    {
      nXFixed++;
    }
  }  

  // Set the number of parameter elements that are free should be
  // included the derived statistics calculation.
  const int nXFree = nX - nXFixed;
  const int nZFree = nXFree;

  valarray<double> xFreeCov   ( nXFree * nXFree );
  valarray<double> xFreeInvCov( nXFree * nXFree );
  valarray<double> xFreeSE    ( nXFree );
  valarray<double> xFreeCor   ( nXFree * nXFree );
  valarray<double> xFreeCV    ( nXFree );
  valarray<double> xFreeCI    ( nXFree * 2 );
  valarray<double> zFree      ( nZFree );
  valarray<double> zFree_xFree( nZFree * nXFree );

  // Get the covariance of the free elements in x.
  for ( j = 0, jj = 0; j<nX; j++ )
  {
    if ( xMask[j] )
    {
      for ( i = 0, ii = 0; i<nX; i++ )
      {
        if ( xMask[i] )
        {
          xFreeCov[ ii + jj * nXFree ] = xCov[ i + j * nX ];
          ii++;
        }
      }
      jj++;
    }
  }  

  // Get the free elements in z.
  for ( i = 0, ii = 0; i < nZ; i++ )
  {
    if ( zMask[i] )
    {
      zFree[ ii ] = z[ i ];
      ii++;
    }
  }

  // Get the derivative of the free elements in z with respect to the
  // free elements in x.
  for ( j = 0, jj = 0; j < nX; j++ )
  {
    if ( xMask[j] )
    {
      for ( i = 0, ii = 0; i < nZ; i++ )
      {
        if ( zMask[i] )
        {
          zFree_xFree[ ii + jj * nZFree ] = z_x[ i + j * nZ ];
          ii++;
        }
      }
      jj++;
    }
  }


  //----------------------------------------------------------------
  // Calculate the derived statistics for the free parameters.
  //----------------------------------------------------------------

  // Calculate the derived statistics for the set of free parameters.
  derParStatistics(
    xFreeCov, 
    zFree, 
    zFree_xFree, 
    nDegOfFreedom,
    ( zCovOut? &xFreeCov : NULL ),
    ( zSEOut?  &xFreeSE  : NULL ),
    ( zCorOut? &xFreeCor : NULL ),
    ( zCVOut?  &xFreeCV  : NULL ),
    ( zCIOut?  &xFreeCI  : NULL ) );

  double val = NAN;

  // Calculate the inverse of the covariance of the original
  // parameters after its fixed elements have been eliminated.
  try
  {
    xFreeInvCov = inverse( xFreeCov, nXFree );
  }
  catch ( ... )
  {
    // If the inverse calculation fails, set the inverse elements to
    // indicate that its values could not be calculated.
    xFreeInvCov = val;
  }

  valarray<bool> zCI_mask ( nZ * 2 );
  valarray<bool> zSE_mask ( nZ );
  valarray<bool> zCV_mask ( nZ );
  valarray<bool> zCov_mask( nZ * nZ );
  valarray<bool> zCor_mask( nZ * nZ );

  for ( j = 0; j < 2; j++ )
  {
    for ( i = 0; i < nZ; i++ )
    {
      zCI_mask[ i + j * nZ ] = zMask[i];
    }
  }

  for ( j = 0; j < nZ; j++ )
  {
    if ( zMask[j] )
    {
      for ( i = 0; i < nZ; i++ )
      {
        zCov_mask[ i + j * nZ ] = zMask[i];
        zCor_mask[ i + j * nZ ] = zMask[i];
      }
   
      zSE_mask[ j ] = zMask[j];
      zCV_mask[ j ] = zMask[j];
    }
    else
    {
      zCov_mask[ slice( j * nZ, nZ, 1 ) ] = false;
      zCor_mask[ slice( j * nZ, nZ, 1 ) ] = false;
    }
  }


  //----------------------------------------------------------------
  // Finish up.
  //----------------------------------------------------------------

  if ( zCIOut )
  {
    placeVal( zCI_mask, xFreeCI, *zCIOut, val );
  }
  if ( zCovOut )
  {
    placeVal( zCov_mask, xFreeCov, *zCovOut, val );
  }
  if ( zInvCovOut )
  {
    placeVal( zCov_mask, xFreeInvCov, *zInvCovOut, val );
  }
  if ( zCorOut )
  {
    placeVal( zCor_mask, xFreeCor, *zCorOut, val );
  }
  if ( zSEOut )
  {
    placeVal( zSE_mask, xFreeSE, *zSEOut, val );
  }
  if ( zCVOut )
  {
    placeVal( zCV_mask, xFreeCV, *zCVOut, val );
  }

  return;
}

// The version that takes two masks should be used instead of this
// version, which is here for backwards compatability.
void derParStatistics( const SPK_VA::valarray<bool>   & mask,
                       const SPK_VA::valarray<double> & xCov,
                       const SPK_VA::valarray<double> & z,
                       const SPK_VA::valarray<double> & z_x,
                       int                              nDegOfFreedom,
                       SPK_VA::valarray<double>       * zCovOut,
                       SPK_VA::valarray<double>       * zInvCovOut,
                       SPK_VA::valarray<double>       * zSEOut,
                       SPK_VA::valarray<double>       * zCorOut,
                       SPK_VA::valarray<double>       * zCVOut,
                       SPK_VA::valarray<double>       * zCIOut
                      )
{
  // Use the same mask for x and z.
  derParStatistics(
    mask,
    xCov,
    mask,
    z,
    z_x,
    nDegOfFreedom,
    zCovOut,
    zInvCovOut,
    zSEOut,
    zCorOut,
    zCVOut,
    zCIOut );
}

