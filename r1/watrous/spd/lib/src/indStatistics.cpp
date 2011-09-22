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
 * Compute covariance matrix, standard error, correlation matrix and 
 * 95% confidence interval of individual parameter estimates.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

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
  nagg
  statistics
  confint
  exp
  Cramer-Rao
$$

$section Computing statistics of individual parameter estimates$$

$index indStatistics, coefficient of variation, confidence interval$$
$index covariance, standard error, correlation matrix, individual parameters$$

$table
$bold Prototype:$$ $cend
$syntax/void indStatistics(  
                   const SPK_VA::valarray<double>& /indPar/,
                   const SPK_VA::valarray<double>& /dataMean_indPar/,
                   const SPK_VA::valarray<double>& /dataVariance_indPar/,
                   const SPK_VA::valarray<double>& /dataVarianceInv/,
                   SPK_VA::valarray<double>*       /indParCovOut/, 
                   SPK_VA::valarray<double>*       /indParSEOut/,                          
                   SPK_VA::valarray<double>*       /indParCorOut/,
                   SPK_VA::valarray<double>*       /indParCVOut/,
                   SPK_VA::valarray<double>*       /indParCIOut/
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
This function computes covariance matrix, standard error vector, correlation 
matrix, coefficient of variation and confidence interval of individual parameter 
estimates.  The covariance matrix is actually the lower limit of the true 
covariance matrix.  It is calculated using the Cramer-Rao inequality:
$math%         
                             -1
               Covariance \le A  
               
%$$
where A is the Fisher information matrix.  The standard error 
vector and the correlation matrix are calculated from the values of the 
covariance matrix using their mathematical definitions, respectively. 
The coefficient of variation is calculated as:
$math%
   
               CV = SE / b * 100 

%$$
where CV stands for the coefficient of variation, SE stands for the standard 
error and b stands for the value of the individual parameter estimate.
The confidence interval is calculated from the values of the standard error 
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

/indPar/
/$$
The $code SPK_VA::valarray<double>$$ $italic indPar$$ contains the vector 
$math%b%$$, which specifies the estimates of the individual parameters.  
The returned values $italic indParCovOut$$, $italic indParSEOut$$, 
$italic indParCorOut$$, $italic indParCVOut$$ and $italic indParCIOut$$  
will be evaluated at these estimates.  
The $italic values of indPar$$ should be obtained by calling SPK function 
$xref/fitIndividual//fitIndividual/$$.

$syntax/

/dataMean_indPar/
/$$
The $code SPK_VA::valarray<double>$$ $italic dataMean_indPar$ is the mean
of data evaluated at $italic indPar$$.

$syntax/

/dataVariance_indPar/
/$$
The $code SPK_VA::valarray<double>$$ $italic dataVariance_indPar$ is 
the value of the derivative of the variance of data with respect to the
individual parameter, evaluated at $italic indPar$$.

syntax/

/dataVarianceInv/
/$$
The $code SPK_VA::valarray<double>$$ $italic dataVarianceInv$ is 
the value of the inverse of the variance of data evaluated at $italic indPar$$.

$syntax/

/indParCovOut/ 
/$$
If $italic indParCovOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParCovOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the square of the length of the individual parameter vector 
$math%b%$$.  If $italic popParCovOut$$ is not $code NULL$$ and this function 
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
$math%b%$$.  If $italic popParCorOut$$ is not $code NULL$$ and this function 
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
The following demonstrates running popStatistics() in the single processing mode.

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

#include <cmath>
#include "indStatistics.h"
#include "SpkException.h"
#include "multiply.h"
#include "transpose.h"
#include "AkronBtimesC.h"
#include "inverse.h"
#include "statistics.h"

using SPK_VA::valarray;
using SPK_VA::slice;

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
    using std::endl;
    using std::ends;
    //----------------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------------
    // Return if there are no output values to compute.
    if( indParCovOut == 0 && indParSEOut == 0 && indParCorOut == 0 && 
		indParCVOut && indParCIOut == 0 ) 
       return;
  
    // Number of individual parameters
    const int nB = static_cast<int>( indPar.size() );
    assert( nB > 0 );

    // Number of data points
    const int nY = static_cast<int>( dataMean_indPar.size() ) / nB;
    assert( static_cast<int>( dataMean_indPar.size() ) == nY * nB );
    assert( nY > 0 );

    using namespace std;
    // Degree of freedom
    const int nFree = nY - nB;
    
    if( indParCIOut )
      {
	if( nFree < 1 )
	  {
	    const int max = SpkError::maxMessageLen();
	    char message[max];
	    sprintf( message, "The degree of freedom (#of measurements<%d> - #of random effects<%d>) must be positive.", nY, nB );
	    
	    throw SpkException(
			       SpkError::SPK_USER_INPUT_ERR, 
			       message,
			       __LINE__, __FILE__
			       );
	  }
      }
    //----------------------------------------------------------------
    // Calculate Covariance of individual parameter estimates 
    //----------------------------------------------------------------
    valarray<double> indParCov( nB * nB );
    
    try
      {
        indParCov = inverse( 0.5 * multiply( transpose( dataVariance_indPar, nB ), nY * nY,
					     AkronBtimesC( dataVarianceInv, nY, dataVarianceInv, 
							   nY, dataVariance_indPar, nB ), nB )
			     + multiply( transpose( dataMean_indPar, nB ), nY,
					 multiply( dataVarianceInv, nY, dataMean_indPar, nB ), nB ), 
			     nB );
      }
    catch(SpkException& e)
      {
        throw e.push( SpkError::SPK_NOT_INVERTABLE_ERR,
                      "Failed to invert information matrix",
                      __LINE__, __FILE__ );
      }
    
    if( indParCovOut != 0 )
      *indParCovOut = indParCov;
    statistics( indPar, indParCov, nFree, indParSEOut, indParCorOut, indParCVOut, indParCIOut );
}
