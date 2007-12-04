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
 * File: statistics.cpp
 *
 *
 * Compute covariance matrix, standard error, correlation matrix and 
 * 95% confidence interval of individual parameter estimates.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: statistics
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin statistics$$

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
  ci
  cv
  deg
$$

$section Computing Statistics of Parameter Estimates$$

$index statistics, coefficient of variation, confidence interval$$
$index covariance, standard error, correlation matrix, parameters$$

$table
$bold Prototype:$$ $cend
$syntax/void statistics( const SPK_VA::valarray<double>& /x/,
		 const SPK_VA::valarray<double>& /xCov/,
		 int                             /degFree/,
		 SPK_VA::valarray<double>*       /seOut/,
		 SPK_VA::valarray<double>*       /corOut/,
		 SPK_VA::valarray<double>*       /cvOut/,
		 SPK_VA::valarray<double>*       /ciOut/
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
This function computes the standard error vector, correlation
matrix, coefficient of variation vector, and confidence interval 
vector of x based on a given covariance matrix.
$pre

$$
The standard error vector is calculated by taking the square roots 
of the diagonal elements of the covariance matrix.
The correlation matrix is calculated by dividing each element of 
the covariance matrix by the standard errors that correspond to its 
row and column.
The coefficients of variation are calculated as
$math%
   
    CV    =  SE    / | x    | * 100   ,
     (i)       (i)      (i)

%$$
where CV is the coefficient of variation and SE is the standard error.  
The 95% confidence intervals are calculated as
$math%
   
    ( x    -  t               * SE    ,  x    +  t               * SE    ) ,
       (i)     0.025, degFree     (i)     (i)     0.025, degFree     (i)

%$$
where
$math%
   
    t
     0.025, degFree

%$$
is the critical value for the $math%t%$$ distribution with 
$math%degFree%$$ number of degrees of freedom for which the 
area under the $math%t%$$ curve is $math%1 - 0.025%$$.

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

/x/
/$$
is an n dimensional parameter vector.  It is used to compute
the coefficient of variance and the confidence interval.

$syntax/

/xCov/
/$$
is a covariance of x (n by n).

$syntax/

/degFree/
/$$
is the degree of freedom used to compute the confidence interval. 
It is defined as degFree = m - n, where n is the length of parameter vector,
in this case, x, and m is the number of fitted data points.
The value must be greater than zero, although
it will not be referenced at all if ciOut is set to NULL.

$syntax/

/seOut/
/$$
will point to an n dimensional vector containing the standard error for x if
the pointer points to an n dimensional valarray.  
If it points to NULL, the corresponding computation will be skipped entirely.
If it points to a valarray sized other than n, the resulting behavior is undetermined.

$syntax/

/corOut/
/$$
will point to an n * n dimensional vector containing the correlation matrix 
in the column major order if the pointer points to an n * n valarray.
If it points to NULL, the corresponding computation will be skipped entirely.
If it points to a valarray sized other than n * n, the resulting behavior is undetermined.


$syntax/

/cvOut/
/$$
will point to an n dimensional vector containing the coefficient of variance if
the pointer points to an n dimensional valarray.  If NULL is given, it will stay NULL.
If it points to NULL, the corresponding computation will be skipped entirely.
If it points to a valarray sized other than n, the resulting behavior is undetermined.

$syntax/

/ciOut/
/$$
will point to an n * 2 dimensional vector containing the 95% confidence interval if
the pointer points to an n * 2 dimensional valarray.  If NULL is given, it will stay NULL.
If it points to NULL, the corresponding computation will be skipped entirely.
If it points to a valarray sized other than n * 2, the resulting behavior is undetermined.

$end
*/

#include <cmath>
#include "inverse.h"
#include "statistics.h"
#include "SpkException.h"
#include "WarningsManager.h"
using SPK_VA::valarray;
using SPK_VA::slice;
using namespace std;

void statistics( const SPK_VA::valarray<double>& x,       // vector of which quality is to be analyzed
		 const SPK_VA::valarray<double>& xCov,    // covariance of x
		 int                             degFree, // degree of freedom
		 SPK_VA::valarray<double>*       seOut,   // standard error           
		 SPK_VA::valarray<double>*       corOut,  // correlation matrix
		 SPK_VA::valarray<double>*       cvOut,   // coefficient of variance
		 SPK_VA::valarray<double>*       ciOut    // confidence interval
                 )
{
  //----------------------------------------------------------------
  // Preliminaries.
  //----------------------------------------------------------------
  // Return if there are no output values to compute.
  if( seOut == 0 && corOut == 0 && cvOut == 0 && ciOut == 0 )
    return;

  // The length of vector containing values whose quality is to be tested.
  const int nX = static_cast<int>( x.size() );
  assert( nX > 0 );
    
  //----------------------------------------------------------------
  // Check that the covariance matrix is ok to use for statistics.
  //----------------------------------------------------------------
  // Calculate the inverse of the covariance of the original
  // parameters after its fixed elements have been eliminated.
  try
  {
    valarray<double> xCovInv( nX * nX );
    xCovInv = inverse( xCov, nX );
  }
  catch ( ... )
  {
    // If the inverse calculation fails, issue some warnings.
    WarningsManager::addWarning(
      "The parameter estimates covariance matrix could not be inverted.",
      __LINE__,
      __FILE__ );

    WarningsManager::addWarning(
      "The parameter estimates statistics are unreliable and should only be \nused for qualitative purposes.",
      __LINE__,
      __FILE__ );
   }

  //----------------------------------------------------------------
  // Calculate Standard Error of individual parameter estimates
  //----------------------------------------------------------------
  valarray<double> seTemp( nX );
  valarray<double> corTemp( nX * nX );
  valarray<double> cvTemp( nX );
  valarray<double> ciTemp( 2 * nX );
  
  if( seOut || cvOut || ciOut )
    {
      valarray<double> temp = xCov[ slice( 0, nX, nX + 1 ) ];
      for( int i = 0; i < nX; i++ )
	seTemp[ i ] = sqrt( temp[ i ] );

      *seOut = seTemp;
    }

  //----------------------------------------------------------------
  // Prepare output for Correlation 
  //----------------------------------------------------------------
  if( corOut )
    {
      int m = nX + 1;
      int n = nX * nX;
      for( int i = 0; i < n; i++ )
	corTemp[ i ] = xCov[ i ] / 
	  sqrt( xCov[ i % nX * m ] * 
		xCov[ i / nX * m ] );
      *corOut = corTemp;
    }
  
  //----------------------------------------------------------------
  // Prepare output for Coefficient of Variation 
  //----------------------------------------------------------------
  if( cvOut )
    {
      for( int i = 0; i < nX; i++ )
	cvTemp[ i ] = seTemp[ i ] / fabs( x[ i ] ) * 100.; 

      *cvOut = cvTemp; 
    }
  
  //----------------------------------------------------------------
  // Prepare output for Confidence Interval
  //----------------------------------------------------------------
  if( ciOut )
    {
      if( degFree < 1 )
	{
	  const int max = SpkError::maxMessageLen();
	  char message[max];
	  snprintf( message, max, "The degree of freedom must be positive.", degFree );
	  
	  throw SpkException(
			     SpkError::SPK_USER_INPUT_ERR, 
			     message,
			     __LINE__, __FILE__
			     );
	}

      double t[] = { 12.706, 4.303, 3.182, 2.776, 2.571, 2.447, 
		     2.365, 2.306, 2.262, 2.228, 2.201, 2.179, 
		     2.160, 2.145, 2.131, 2.120, 2.110, 2.101, 
		     2.093, 2.086, 2.080, 2.074, 2.069, 2.064, 
		     2.060, 2.056, 2.052, 2.048, 2.045, 2.042 };
      
      double tn, distance;
      
      if( degFree <= 30 )
	tn = t[ degFree - 1 ];
      if( degFree > 30 && degFree <= 40 )
	tn = 2.042 - ( degFree - 30 ) * 0.021 / 10.0;
      if( degFree > 40 && degFree <= 60 )
	tn = 2.021 - ( degFree - 40 ) * 0.021 / 20.0;
      if( degFree > 60 && degFree <= 120 )
	tn = 2.000 - ( degFree - 60 ) * 0.020 / 60.0;
      if( degFree > 120 )
	tn = 1.960;
      for( int i = 0; i < nX; i++ )
	{
	  distance = seTemp[ i ] * tn;
	  ciTemp[ i ]      = x[ i ] - distance;
	  ciTemp[ i + nX ] = x[ i ] + distance;
	}
      *ciOut = ciTemp;
    }
}
/*************************************************************************
 *
 * Function: statistics - allows inactive elements
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin statisticsInactiveElem$$

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
  ci
  cv
  deg
$$

$section Computing Statistics of Parameter Estimates when Some Elements are not Active$$

$index statistics$$
$syntax/void statistics( const SPK_VA::valarray<bool>  & mask,
		 const SPK_VA::valarray<double> & x,       
		 const SPK_VA::valarray<double> & xCov,    
		 int                              degFree, 
		 SPK_VA::valarray<double>       * seOut,            
		 SPK_VA::valarray<double>       * corOut,  
		 SPK_VA::valarray<double>       * cvOut,   
		 SPK_VA::valarray<double>       * ciOut
                 )
/$$
$head Description$$
This function computes the standard error vector, correlation
matrix, coefficient of variation vector, and confidence interval 
vector of $math%x'%$$ based on a given covariance matrix of $math%x%$$.  
$math%x'%$$ is a subset of $math%x%$$ such that { x' | x AND mask } ("AND" is logical AND).
$pre

$$
The standard error vector is calculated by taking the square roots 
of the diagonal elements of the covariance matrix.
The correlation matrix is calculated by dividing each element of 
the covariance matrix by the standard errors that correspond to its 
row and column.
The coefficients of variation are calculated as
$math%
   
    CV    =  SE    / | x    | * 100   ,
     (i)       (i)      (i)

%$$
where CV is the coefficient of variation and SE is the standard error.  
The 95% confidence intervals are calculated as
$math%
   
    ( x    -  t               * SE    ,  x    +  t               * SE    ) ,
       (i)     0.025, degFree     (i)     (i)     0.025, degFree     (i)

%$$
where
$math%
   
    t
     0.025, degFree

%$$
is the critical value for the $math%t%$$ distribution with 
$math%degFree%$$ number of degrees of freedom for which the 
area under the $math%t%$$ curve is $math%1 - 0.025%$$.

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
/mask/
/$$
is a vector of boolean values (ie. true/false) of length equal to the length of 
$math%x%$$.  A value $math%true%$$ in the i-th element of $math%mask%$$
indicates that the i-th value of $math%x%$$ has contributed to the parameter optimization.

$syntax/

/x/
/$$
is an n dimensional parameter vector.

$syntax/

/xCov/
/$$
is a covariance matrix of $math%x%$$.

$syntax/

/degFree/
/$$
is the degree of freedom used to compute the confidence interval. 
It is defined as degFree = m - n, where n is the length of parameter vector,
in this case, x, and m is the number of fitted data points.
The value must be greater than zero, although
it will not be referenced at all if ciOut is set to NULL.

$syntax/

/seOut/
/$$
will point to an n dimensional vector containing the standard error for x if
the pointer points to an n dimensional valarray.  The elements of which
corresponding $math%mask%$$ value is $math%false%$$ will be set to NAN.

If it points to NULL, the corresponding computation will be skipped entirely.
If it points to a valarray sized other than n, the resulting behavior is undetermined.

$syntax/

/corOut/
/$$
will point to an n * n dimensional vector containing the correlation matrix 
in the column major order if the pointer points to an n * n valarray.
The elements depend on those $math%x(i)%$$ of which corresponding $math%mask%$$
is $math%false%$$ will be set to NAN.

If it points to NULL, the corresponding computation will be skipped entirely.
If it points to a valarray sized other than n * n, the resulting behavior is undetermined.


$syntax/

/cvOut/
/$$
will point to an n dimensional vector containing the coefficient of variance if
the pointer points to an n dimensional valarray.  The elements of which
corresponding $math%mask%$$ value is $math%false%$$ will be set to NAN.

If it points to NULL, the corresponding computation will be skipped entirely.
If it points to a valarray sized other than n, the resulting behavior is undetermined.

$syntax/

/ciOut/
/$$
will point to an n * 2 dimensional vector containing the 95% confidence interval if
the pointer points to an n * 2 dimensional valarray. 
The elements of which corresponding $math%mask%$$ value is $math%false%$$ will be set to NAN.

If it points to NULL, the corresponding computation will be skipped entirely.
If it points to a valarray sized other than n * 2, the resulting behavior is undetermined.

$end
*/
namespace
{
  //=========================================================
  // Expand the vector x to y. Insert "val" in places where mask[i] is false.
  //=========================================================

  void placeVal( const valarray<bool>   & mask,
		 const valarray<double> & x,
		 valarray<double>       & y,
		 double val = NAN )
  {
    assert( mask.size() == y.size() );
    const int nX = x.size();
    const int nY = y.size();
    
    for( int i=0, ii=0; i<nY; i++ )
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
void statistics( const SPK_VA::valarray<bool>  & mask,    // flags indicating elements are active or not
		 const SPK_VA::valarray<double>& x,       // vector of which quality is to be analyzed
		 const SPK_VA::valarray<double>& xCov,    // covariance of x
		 int                             degFree, // degree of freedom
		 SPK_VA::valarray<double>*       seOut,   // standard error           
		 SPK_VA::valarray<double>*       corOut,  // correlation matrix
		 SPK_VA::valarray<double>*       cvOut,   // coefficient of variance
		 SPK_VA::valarray<double>*       ciOut    // confidence interval
                 )
{
  const int nX = x.size();
  assert( nX == mask.size() );
  valarray<double> y = x[ mask ];
  const int nY = y.size();
  valarray<double> yCov( nY * nY );
  valarray<double> ySE ( nY );
  valarray<double> yCor( nY * nY );
  valarray<double> yCV ( nY );
  valarray<double> yCI ( nY * 2 );

  for( int j=0, jj=0; j<nX; j++ )
    {
      if( mask[j] )
	{
	  for( int i=0, ii=0; i<nX; i++ )
	    {
	      if( mask[i] )
		{
		  yCov[ ii + jj * nY ] = xCov[ i + j * nX ]; 
		  ii++;
		}
	    }
	  jj++;
	}

    }
  // 
  statistics( y, yCov, degFree-(nX-nY), &ySE, &yCor, &yCV, &yCI );

  valarray<bool> yCI_mask ( nX * 2 );
  valarray<bool> ySE_mask ( nX );
  valarray<bool> yCV_mask ( nX );
  valarray<bool> yCor_mask( nX * nX );
  double val = NAN;

  for( int j=0; j<2; j++ )
    {
      for( int i=0; i<nX; i++ )
	{
	  yCI_mask[ i + j * nX ] = mask[i];
	}
    }

  for( int j=0; j<nX; j++ )
    {
      if( mask[j] )
	{
	  for( int i=0; i<nX; i++ )
	    {
	      yCor_mask[ i + j * nX ] = mask[i];
	    }
       
	  ySE_mask[ j ] = mask[j];
	  yCV_mask[ j ] = mask[j];
	}
      else
	{
	  yCor_mask[ slice( j * nX, nX, 1 ) ] = false;
	}
    }
  if( ciOut )
    {
      placeVal( yCI_mask, yCI, *ciOut, val );
    }
  if( corOut )
    {
      placeVal( yCor_mask, yCor, *corOut, val );
    }
  if( seOut )
    {
      placeVal( ySE_mask, ySE, *seOut, val );
    }
  if( cvOut )
    {
      placeVal( yCV_mask, yCV, *cvOut, val );
    }


  return;
}
