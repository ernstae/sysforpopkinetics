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
 * File: wres.cpp
 *
 *
 * Computes residuals and weighted residuals.
 *
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: wres
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin wres$$
$spell 
namespace cout endl yHat valarray SPK_VA
$$

$section Weighted Residuals$$

$index wres$$
$index residual, weighted$$

$table
$bold Prototype:$$   $cend  
$syntax/void wres( const SPK_VA::valarray<double>& /y/,
                   const SPK_VA::valarray<double>& /yHat/,
                   const SPK_VA::valarray<double>& /R/,
                   SPK_VA::valarray<double>& /r/,
                   SPK_VA::valarray<double>& /wr/ )/$$
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
Computes residuals and weighted residuals using the variance of measurement data
as the weight.  Residuals, $math%r%$$ and
Weighted residuals, $math%wr%$$, are computed as follows:

$math%
   r = y - y^
  wr = C * r, such that R = C * C^t
%$$

$head Arguments$$

$syntax/
&/y/
/$$
contains the actual measurement values to be fitted.
The size of $math%y%$$ is $math%n%$$, where $math%n >= 0%$$.
If $math%y%$$ were empty, the function returns immediately.

$syntax/

&/yHat/
/$$
contains the predicted values computed by the model.
The size of $math%y^%$$ must be equal to $math%n%$$.

$syntax/

&/R/
/$$
represents a matrix in the column major order that contains
the covariance of measurement data.  The matrix, therefore,
is assumed to have the positive definite property.
The size of $math%R%$$ must be equal to $math%n^2%$$.

$syntax/

&/r/
/$$
is a vector of length $math%n%$$ (ie. preallocated) and
will contain the residuals if computation went successful.


$syntax/

&/wr/
/$$
is a vector of length $math%n%$$ (ie. preallocated) and
will contain the weighted residuals if computation went successful.

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include <valarray>
    #include <spk/wres.h>

    using namespace std;
    void main()
    {
       int n = 2;

       //
       // y = [ 1.1  2.2 ]
       //
       double yIn[] = { 1.1, 2.2 };
       valarray<double> y( yIn, n );

       //
       // y^ = [ 1.0  2.0 ]
       //
       double yHatIn[] = { 1.0, 2.0 };
       valarray<double> yHat( yHatIn, n );

       //
       //     /            \
       //     |  1.0  0.0  |
       // R = |            |
       //     |  0.0  4.0  |
       //     \            /
       //
       double RIn[] = { 1.0, 0.0, 0.0, 4.0 };
       valarray<double> R( RIn, n*n );

       valarray<double> r(n);
       valarray<double> wr(n);

       wres( y, yHat, R, r, wr );

       cout << "r  = " <<  r << endl;
       cout << "wr = " << wr << endl;
    }

$$
then it will display the following when it is run:
$codep

    r  = { 0.1, 0.2 }
    wr = { 0.1, 0.4 }
$$
$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include "wres.h"

#include "SpkValarray.h"
#include "cholesky.h"
#include "multiply.h"
#include "SpkException.h"
#include "SpkError.h"

using SPK_VA::valarray;
using namespace std;

void wres( const valarray<double>& y, 
	   const valarray<double>& yHat,
	   const valarray<double>& R,
	   valarray<double>& r,
	   valarray<double>& wr )
{
  // w = C * r, where C is such that R = C * C^t
  int n = y.size();
  assert( yHat.size() == n );
  assert( R.size()    == n * n );
  assert( r.size()    == n );
  assert( wr.size()   == n );
  if( n == 0 )
    return;

  valarray<double> C( 0.0, n * n );

  try{
    // Compute residuals
    r = y - yHat;

    // Factorize R, assuming R is pos/sym/def.
    C = cholesky( R, n );

    // Compute weighted residuals = C * r
    wr = multiply( C, n, r, 1 );
  }
  catch( SpkException& e )
    {
      char m[ SpkError::maxMessageLen() ];
      sprintf( m, "Failed to compute weighted residuals." );
      e.push( SpkError::SPK_UNKNOWN_ERR, m, __LINE__, __FILE__ );
      throw e;
    }
  catch( ... )
    {
      char m[ SpkError::maxMessageLen() ];
      sprintf( m, "Failed to compute weighted residuals." );
      SpkException e ( SpkError::SPK_UNKNOWN_ERR, m, __LINE__, __FILE__ );
      throw e;
    }

  return;
}
