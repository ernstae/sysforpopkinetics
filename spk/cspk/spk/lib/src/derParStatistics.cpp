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
$begin derparstatistics$$

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
The remainder of the statistics for the derived parameter are
calculated in the standard way using this covariance matrix.
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
SpkException object will be thrown.  The state after an exception is
thrown is defined in $xref/glossary/Exception Handling
Policy/Exception Handling Policy/$$.

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
confidence intervals.  It should be set equal to n - m, where m is the
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
  valarray<double> z_xTimesXCov( nZ * nZ );
  valarray<double> z_xTrans    ( nZ * nZ * nZ );
 
  z_xTrans = transpose( z_x, nZ );

  // Calculate
  //                     -          -              -          -  T
  //                    |            |            |            |
  //    Cov[ z(x) ]  =  |  d   z(x)  |  Cov[ x ]  |  d   z(x)  |    .
  //                    |   x        |            |   x        |
  //                     -          -              -          - 
  //
  z_xTimesXCov = multiply( z_x,          nZ, xCov,     nZ );
  zCovTemp     = multiply( z_xTimesXCov, nZ, z_xTrans, nZ );


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
