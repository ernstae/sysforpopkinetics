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
$head Description$$
This function computes the standard error vector, correlation
matrix, coefficient of variation and confidence interval of x based on
a given covariance matrix.
 
The coefficient of variation is calculated as:
$math%
   
               CV = SE / x * 100 

%$$
where CV stands for the coefficient of variation, SE stands for the standard 
error.  The confidence interval is calculated from the values of the standard error 
using its mathematical definition.

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
It is defined as degFree = n - m, where n is the length of parameter vector,
in this case, x, and m is the number of fitted data points.
The value must be greater than zero, although
it will not be referenced at all if ciOut is set to NULL.

$syntax/

/seOut/
/$$
will point to an n dimensional vector containing the standard error for x if
the pointer points to an n dimensional valarray.  
If it points to NULL, the corresponding computation will be skipped entirely.
If it points to a valarray sized other than n, the resulting bahavior is undetermined.

$syntax/

/corOut/
/$$
will point to an n * n dimensional vector containing the correlation matrix 
in the column major order if the pointer points to an n * n valarray.
If it points to NULL, the corresponding computation will be skipped entirely.
If it points to a valarray sized other than n * n, the resulting bahavior is undetermined.


$syntax/

/cvOut/
/$$
will point to an n dimensional vector containing the coefficient of variance if
the pointer points to an n dimensional valarray.  If NULL is given, it will stay NULL.
If it points to NULL, the corresponding computation will be skipped entirely.
If it points to a valarray sized other than n, the resulting bahavior is undetermined.

$syntax/

/ciOut/
/$$
will point to an n * 2 dimensional vector containing the 95% confidence interval if
the pointer points to an n * 2 dimensional valarray.  If NULL is given, it will stay NULL.
If it points to NULL, the corresponding computation will be skipped entirely.
If it points to a valarray sized other than n * 2, the resulting bahavior is undetermined.
*/

#include <cmath>
#include "statistics.h"
#include "SpkException.h"

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
	cvTemp[ i ] = seTemp[ i ] / x[ i ] * 100.; 

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
	  sprintf( message, "The degree of freedom must be positive.", degFree );
	  
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
