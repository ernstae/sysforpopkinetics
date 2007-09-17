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
 * Function: convertStatisticsToNonmem
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin convertStatisticsToNonmem$$
$spell 
  spk
  Nonmem
  const
  valarray
$$

$section Convert Statistics to Nonmem$$

$index convertStatisticsToNonmem$$

$table
$bold Prototype:$$   $cend  
$syntax/void convertStatisticsToNonmem(
  bool                            /isDDiagonal/,
  bool                            /isRExponential/,
  const SPK_VA::valarray<double>& /popParSpk/,
  const SPK_VA::valarray<double>& /popParCovSpk/,
  SPK_VA::valarray<double>&       /popParCovNonmem/,
  SPK_VA::valarray<double>&       /popParSeNonmem/, 
  SPK_VA::valarray<double>&       /popParCorNonmem/ )
/$$
$tend

$fend 25$$


$head Description$$
The routine $code convertStatisticsToNonmem$$ converts Spk statistics to Nonmem statistics.


$head Arguments$$

$syntax/

/isDDiagonal/
/$$
Set this argument to true if the Spk D matrix is diagonal or false otherwise.

$syntax/

/isRExponential/
/$$
Set this argument to true if the Spk R matrix elements is in exponential form 
or false if they are in direct form.

$syntax/

/popParSpk/
/$$
The $code SPK_VA::valarray<double>$$ $italic popParSpk$$ contains the vector 
$math%alp%$$, which specifies the estimates of the population parameters.  
The returned covariance matrix $italic popParCovOut$$ will be evaluated at 
these values.  
The $italic popPar$$ should be obtained by calling SPK function 
$xref/fitPopulation//fitPopulation/$$.

$syntax/

/popParCovSpk/ 
/$$
The $code SPK_VA::valarray<double>$$ $italic popParCovSpk$$ specifies the 
Spk covariance matrix of the population parameter estimates evaluated at 
$italic popParSpk$$.  
The $italic popParCovSpk$$ should be obtained by calling SPK function 
$xref/statistics//statistics/$$.

$syntax/

/popParSeSpk/ 
/$$
The $code SPK_VA::valarray<double>$$ $italic popParSeSpk$$ specifies the 
Spk standard error vector of the population parameter estimates evaluated at 
$italic popParSpk$$.  
The $italic popParSeSpk$$ should be obtained by calling SPK function 
$xref/statistics//statistics/$$.

$syntax/

/popParCorSpk/ 
/$$
The $code SPK_VA::valarray<double>$$ $italic popParCorSpk$$ specifies the 
Spk correlation matrix of the population parameter estimates evaluated at 
$italic popParSpk$$.  
The $italic popParCorSpk$$ should be obtained by calling SPK function 
$xref/statistics//statistics/$$.

$syntax/

/popParCovNonmem/ 
/$$
This is the place holder for Nonmem covariance matrix output.  It must be 
declared in the calling program. 

$syntax/

/popParSeNonmem/ 
/$$
This is the place holder for Nonmem standard error vector output.  It must be 
declared in the calling program. 

$syntax/

/popParCovNonmem/ 
/$$
This is the place holder for Nonmem correlation matrix output.  It must be 
declared in the calling program. 

$end
*/


#include "convertStatisticsToNonmem.h"
#include <cassert>
#include <cmath>
#include <spk/transpose.h>
#include <spk/multiply.h>

using SPK_VA::valarray;
using SPK_VA::slice;

void convertStatisticsToNonmem(
								bool                    isDDiagonal,
								bool                    isRExponential,
								const valarray<double>& popParSpk,
								const valarray<double>& popParCovSpk,
								const valarray<double>& popParSeSpk,
								const valarray<double>& popParCorSpk,
								valarray<double>&       popParCovNonmem,
								valarray<double>&       popParSeNonmem, 
								valarray<double>&       popParCorNonmem 
							  )
{
    const int m = popParSpk.size();
	const int n = m * m;
	assert( popParCovSpk.size() == n );
/*
    Spk popPar = a.
	Nonmem popPar = w.

    For block D:
         
    w0 = a0         
    w1 = a1 
    w2 = a2 
	w3 = a4^2           
    w4 = a4*a5 
	w5 = a4*a7 
    w6 = a5^2+a6^2                
    w7 = a5*a7+a6*a8      
	w8 = a7^2+a8^2+a9^2 
	w9 = exp(a3)  for exponential R
	w9 = a3       for direct R

    For diagonal D:
         
    w0 = a0         
    w1 = a1 
    w2 = a2 
	w3 = a4          
    w4 = a5           
    w5 = a6      
	w6 = exp(a3)  for exponential R
	w6 = a3       for direct R
                                                        T
    popParCovNonmem = d w(a) * popParCovSpk * [ d w(a) ]
                       a                         a
*/
    //----------------------------------------------------------------
    // Prepare conversion matrix C
    //----------------------------------------------------------------
	valarray<double> C( 0.0, n );

	if( !isDDiagonal )
	{
		assert( m == 10 );
		C[ 0 ] = 1.0;
		C[ 11 ] = 1.0;
		C[ 22 ] = 1.0;
		if( isRExponential )
		    C[ 39 ] = exp( popParSpk[ 3 ] );
		else
            C[ 39 ] = 1.0;
		C[ 43 ] = popParSpk[ 4 ] * 2.0;
		C[ 44 ] = popParSpk[ 5 ];
		C[ 45 ] = popParSpk[ 7 ];
		C[ 54 ] = popParSpk[ 4 ];
		C[ 56 ] = popParSpk[ 5 ] * 2.0;
		C[ 57 ] = popParSpk[ 7 ];
		C[ 66 ] = popParSpk[ 6 ] * 2.0;
		C[ 67 ] = popParSpk[ 8 ];
		C[ 75 ] = popParSpk[ 4 ];
		C[ 77 ] = popParSpk[ 5 ];
		C[ 78 ] = popParSpk[ 7 ] * 2.0;
		C[ 87 ] = popParSpk[ 6 ];
		C[ 88 ] = popParSpk[ 8 ] * 2.0;
		C[ 98 ] = popParSpk[ 9 ] * 2.0;
	}
	else
	{
        assert( m == 7 );
		C[ 0 ] = 1.0;
		C[ 8 ] = 1.0;
		C[ 16 ] = 1.0;
		if( isRExponential )
		    C[ 27 ] = exp( popParSpk[ 3 ] );
		else
            C[ 27 ] = 1.0;
        C[ 31 ] = 1.0;
		C[ 39 ] = 1.0;
		C[ 47 ] = 1.0;
	}

    //----------------------------------------------------------------
    // Construct the Spk Covariance from popParSeSpk and popParCorSpk
    //----------------------------------------------------------------
    valarray<double> SpkCov = popParCorSpk;
    for( int i = 0; i < m; i++ )
      for( int j = 0; j < m; j++ )
	SpkCov[ j + i * m ] *= popParSeSpk[ i ] * popParSeSpk[ j ];

    //----------------------------------------------------------------
    // Convert the constructed Spk Covariance to Nonmem Covariance 
    //----------------------------------------------------------------
	valarray<double> NonmemCov = multiply( C, 
		                         m, 
		                         multiply( SpkCov, m, transpose( C, m ), m ),
								 m );
 
    //----------------------------------------------------------------
    // Prepare output for Covariance 
    //----------------------------------------------------------------
    popParCovNonmem.resize( n );
	popParCovNonmem = multiply( C, 
		                        m, 
		                        multiply( popParCovSpk, m, transpose( C, m ), m ),
								m );

	//----------------------------------------------------------------
    // Prepare output for Standard Error 
    //----------------------------------------------------------------
	popParSeNonmem.resize( m );
    valarray<double> temp = NonmemCov[ slice( 0, m, m + 1 ) ];
	for( int i = 0; i < m; i++ )
        popParSeNonmem[ i ] = sqrt( temp[ i ] );

	//----------------------------------------------------------------
    // Prepare output for Correlation 
    //----------------------------------------------------------------
	popParCorNonmem.resize( n );
    int l = m + 1;
    for(int i = 0; i < n; i++ )
        popParCorNonmem[ i ] = NonmemCov[ i ] / 
                               sqrt( NonmemCov[ i % m * l ] * 
                                     NonmemCov[ i / m * l ] );
}
