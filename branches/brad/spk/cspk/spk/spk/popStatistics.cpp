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
 * File: popStatistics.cpp
 *
 *
 * Compute covariance matrix, standard error and correlation matrix of 
 * population parameter estimates.
 *
 * Author: Jiaji Du
 *
 * Modified later by Sachiko Honda - isolated core computation portion
 *
 * Modified later by: Mitch Watrous
 *
 *************************************************************************/

#include <iostream>
#include <cmath>
#include "popStatistics.h"
#include "getCol.h"
#include "replaceJth.h"
#include "transpose.h"
#include "inverse.h"
#include "lTilde.h"
#include "firstOrderOpt.h"
#include "multiply.h"
#include "add.h"
#include "SpkException.h"
#include "SpkValarray.h"
#include "statistics.h"
#include "printInMatrix.h"
#include "intToOrdinalString.h"

using SPK_VA::valarray;
using SPK_VA::slice;
using namespace std;

namespace{
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

  //                         T      
  //     S = Sum{ gi_x * gi_x }
  //          i                          
  //
  const valarray<double> nmS( const valarray<double> & g_x, int nX )
  {
    int nF = g_x.size() / nX;
    assert( g_x.size() == nX * nF );
    
    valarray<double> S( 0.0, nX * nX );
    
    valarray<double> gi_x( nX );
    for( int i=0; i<nF; i++ )
      {
	gi_x = g_x[ slice( i * nX, nX, 1 ) ];
	S += multiply( gi_x, 1, transpose( gi_x, 1 ), nX );
      }
    return S;
  }
  //
  // R = h_x_x
  // R^(-1) = (h_x_x)^(-1)
  //
  const valarray<double> nmInvR(  const valarray<double> & h_x_x,
					 int nX )
  {
    int nF = h_x_x.size() / nX / nX;
    assert( nF == 1 );
    assert( h_x_x.size() == nX * nX * nF ); 
    
    // This makes sure the matrix to be inverted is symmetric.
    // If the attempt to invert the matrix failed, it means the matrix is not positive definite.
    valarray<double> invR( nX * nX );
    try{
      invR = inverse( ( h_x_x + transpose( h_x_x, nX ) ) * 0.5, nX );
    }
    catch( SpkException & e )
      {
	char mess[ SpkError::maxMessageLen() ];
	
	valarray<double> diag = h_x_x[ slice(0, nX, nX+1) ];
	snprintf( mess, SpkError::maxMessageLen(),
                  "The Hessian of the objective function could not be inverted.\n" );
	for( int i=0; i<nX; i++ )
	  {
	    if( diag[i] <= 0.0 )
	      snprintf( mess, SpkError::maxMessageLen(),
                        "The %s diagonal element of the Hessian is not positive.\n",
		        intToOrdinalString( i, ZERO_IS_FIRST_INT ).c_str() );
	  }
	e.push( SpkError::SPK_NOT_POS_DEF_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    catch( ... )
      {
	char mess[ SpkError::maxMessageLen() ];
	
	valarray<double> diag = h_x_x[ slice(0, nX, nX+1) ];
	snprintf( mess, SpkError::maxMessageLen(), 
                  "The Hessian of the objective function could not be inverted.\n" );
	for( int i=0; i<nX; i++ )
	  {
	    if( diag[i] < 0.0 )
	      snprintf( mess, SpkError::maxMessageLen(), 
                        "The %s diagonal element of the Hessian is not positive.\n",
		        intToOrdinalString( i, ZERO_IS_FIRST_INT ).c_str() );
	  }
	SpkException e( SpkError::SPK_NOT_POS_DEF_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }      
    
    return  invR;
  }
};


/*************************************************************************
 *
 * Function: popStatistics
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin popStatistics$$

$spell
  Enumerator
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
  Beal
  Raton
  Ruppert
  Sheiner
  Stefanski
$$

$section Computing Statistics of Population Parameter Estimates$$

$index popStatistics$$
$index covariance, standard error, correlation matrix, population parameters$$
$cindex \Computing Statistics \of Population \Parameter \Estimates$$

$table
$bold Enumerators:$$ $cend
$syntax/enum Objective { MODIFIED_LAPLACE, EXPECTED_HESSIAN, FIRST_ORDER, NAIVE_FIRST_ORDER }
enum PopCovForm { RSR, R, S }
/$$ $rend
$bold Prototype:$$ $cend
$syntax/void popStatistics( SpkModel<double>&               /popModel/,
                    enum Objective                  /objective/,
                    const SPK_VA::valarray<int>&    /nMeasurementsAll/,
                    const SPK_VA::valarray<double>& /measurementsAll/,
                    const SPK_VA::valarray<double>& /popPar/,
                    const SPK_VA::valarray<bool>&   /popParMask/,
                    const valarray<double>&         /popObj_popPar_popPar/,
                    const SPK_VA::valarray<double>& /indParAll/,
                    const SPK_VA::valarray<double>& /indParLow/,
                    const SPK_VA::valarray<double>& /indParUp/,
                    const SPK_VA::valarray<double>& /indParStep/,
                    enum PopCovForm                 /formulation/,
                    SPK_VA::valarray<double>*       /popParCovOut/, 
                    SPK_VA::valarray<double>*       /popParSEOut/,                          
                    SPK_VA::valarray<double>*       /popParCorOut/,
                    SPK_VA::valarray<double>*       /popParCVOut/,                          
                    SPK_VA::valarray<double>*       /popParCIOut/ )
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
interval vector for population parameter estimates.
It allows parameter elements that are not active to be specified 
and removed from the statistics computations.
$pre

$$
This function allows the covariance matrix of the population parameter 
estimates to be calculated using one of the following formulations:
$math%         

                                        -1     -1
    formulation "RSR":  cov[ alp ]  =  R   S  R   ;

                                        -1
    formulation "R":    cov[ alp ]  =  R   ;

                                        -1
    formulation "S":    cov[ alp ]  =  S   .

%$$
These formulations are discussed in Section (D.2.5) of the NONMEM 
Users Guide Part II and in Sections (A.2.1) and (A.2.2) in 
Carroll, Ruppert, and Stefanski (1998).
$pre

$$
The approximation made for the information matrix is
$math%

                                          T
     R = ( LTilde_alp_alp + LTilde_alp_alp  ) / 2  ,

%$$
where $math%LTilde_alp_alp%$$ is an approximation for the second order 
derivatives of the population objective function with respect to the 
population parameter $math%alp%$$.
The matrix $math%R%$$ is defined in this way to insure that it is symmetric
even for cases where the approximation $math%LTilde_alp_alp%$$ is not.
Note that this $math%R%$$ is different than $math%R(b)%$$, 
which is the model for the covariance of an individual's data 
and is part of $xref/SpkModel//SpkModel/$$.
$pre

$$
The cross-product gradient matrix is defined as
$math%

                                    T
     S = Sum{ [(LambdaTilde_i )_alp] [(LambdaTilde_i)_alp] }  ,
          i                          

%$$
where $math%(LambdaTilde_i)_alp%$$ is the derivative of the $th i$$
individual's objective function with respect to the population
parameter $math%alp%$$.
$pre

$$
The standard error vector is calculated by taking the square roots 
of the diagonal elements of the covariance matrix.
The correlation matrix is calculated by dividing each element of 
the covariance matrix by the standard errors that correspond to its 
row and column.
The coefficients of variation are calculated as
$math%
   
    CV    =  SE    / | alp    | * 100   ,
     (i)       (i)        (i)

%$$
where CV is the coefficient of variation and SE is the standard error.  
The 95% confidence intervals are calculated as
$math%
   
    ( alp    -  t               * SE    ,  alp    +  t               * SE    ) ,
         (i)     0.025, degFree     (i)       (i)     0.025, degFree     (i)

%$$
where
$math%
   
    t
     0.025, degFree

%$$
is the critical value for the $math%t%$$ distribution with 
$math%degFree = nY - nAlp%$$ number of degrees of freedom for which the 
area under the $math%t%$$ curve is $math%1 - 0.025%$$
and $math%nAlp%$$ is the number of population parameters.  

$head Reference$$
Beal, S. L. and Sheiner, L. B. (1988) $italic NONMEM Users Guide - Part II$$,
University of California, San Francisco.
$pre

$$
Carroll, R. J., Ruppert, D., and Stefanski, L. A. (1998) 
$italic Measurement Error in Nonlinear Models$$, Chapman & Hall/CRC,
Boca Raton, Florida.

$head Return Value$$
Upon a successful completion, the function sets
the given output value place holders to point to the result value.
  
$pre

$$
If an error is detected or failure occurs during the evaluation, a SpkException 
object is thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/
/popModel/
/$$
This function expects $italic model$$ to be a function of
all three parameters: $math%alp%$$, $math%b%$$ and $math%i%$$.
Refer $xref/glossary/Model Functions Depend on i - alp - b/Model Functions 
Depend on i - alp - b/$$ for details.

$syntax/
/popModelAD/
/$$
This should be the same model as $italic popModel$$ only instantiated 
with type CppAD::<Scalar>, where Scalar is the type used to instantiate 
$italic popModel$$.

$syntax/

/objective/
/$$
This enumerated type variable specifies which parametric population objective 
function will be minimized:  the modified Laplace, the modified 
Expected Hessian, or the modified First Order.
The permissible values for $italic objective$$ are defined in 
the $xref/Objective//Objective/$$ enumerated type definition.

$syntax/

/nMeasurementsAll/
/$$
The $code SPK_VA::valarray<int>$$ $italic nMeasurementsAll$$ corresponds to 
the array $math%N%$$.  The $th i$$ element of $math%N%$$
specifies the number of elements of $math%y%$$ that
correspond to the $th i$$ individual.
Note that the size of $italic nMeasurementsAll$$ specifies the number of 
individuals in the population, $math%M%$$.

$syntax/

/measurementsAll/
/$$
The $code SPK_VA::valarray<double>$$ $italic measurementsAll$$ contains the vector
$math%y%$$, which specifies the data for all the individuals.
The vector $math%y%$$ has
$math%
    N(1) + N(2) + ... + N(M)
%$$
elements where $math%M%$$ is the number of individuals.  
The data vector corresponding to the first individual is
$math%                                         
    y_1 = [ y(1) , y(2) , ... , y(N(1)) ]
%$$
Elements $math%y(N(1) + 1)%$$ through $math%y(N(1) + N(2))%$$ 
correspond to the second individual and so on.
(Note that $math%y_1%$$ refers to the first subvector of $math%y%$$ while
$math%y(1)%$$ refers to the first element of the valarray $math%y%$$.)

$syntax/

/popPar/
/$$
The $code SPK_VA::valarray<double>$$ $italic popPar$$ contains the vector 
$math%alp%$$, which specifies the estimates of the population parameters.  
The returned covariance matrix $italic popParCovOut$$ will be evaluated at 
these values.  
The $italic popPar$$ should be obtained by calling the SPK function 
$xref/fitPopulation//fitPopulation/$$.

$syntax/

/popParMask/
/$$
$code popParMask$$ is a vector of boolean values of length equal to the parameter
vector, $code popPar$$.  $code popParMask[i]$$ tells as to whether $code popPar[i]$$
is active or not.  If $math%popParMask[i]%$$ is $math%false%$$, the i-th element of
the parameter vector are treated as if it does not exist and further 
statistics computations are performed based upon the assumption.

$syntax/

/popObj_popPar_popPar/ 
/$$
The $code SPK_VA::valarray<double>$$ $italic popObj_popPar_popPar$$ contains 
the matrix $math%LTilde_alp_alp%$$, in column major order, which specifies 
an approximation for the second derivative of the population objective 
function with respect to population parameter evaluated at $italic popPar$$.  
Note that the size of $italic popObj_popPar_popPar$$ should be equal to the 
square of the length of the population parameter vector $math%alp%$$.  
The $italic popObj_popPar_popPar$$ should be obtained by calling the SPK function 
$xref/fitPopulation//fitPopulation/$$. 

$syntax/

/indParAll/
/$$
The $code SPK_VA::valarray<double>$$ $italic indParAll$$ contains the matrix 
$math%bAll%$$ in column major order.  The size of $italic indParAll$$ is
equal to the product of the length of the individual parameter vector 
$math%b%$$ and the number of individuals in the population. 
The $th i$$ column of $math%bAll%$$, $math%b_i%$$, specifies the estimates
of the individual parameters for the $th i$$ individual.
If $math%b_i%$$ is any column of $math%bAll%$$,
it is assumed that $math%bLow \le b_i \le bUp%$$.
Note that the column dimension of $math%bAll%$$ is equal to the number of 
individuals in the population, $math%M%$$.
and the number of rows in $italic indParIn$$ is equal to the 
length of the individual parameter vector $math%b_i%$$.  
The $italic indParAll$$ should be obtained by calling the SPK function 
$xref/fitPopulation//fitPopulation/$$. 

$syntax/

/indParLow/
/$$
The $code SPK_VA::valarray<double>$$ $italic indParLow$$ contains the vector
$math%bLow%$$, which specifies the lower limit for the individual parameters 
for all the individuals.  The size of $italic indParLow$$ is equal to the 
length of the individual parameter vector $math%b%$$.

$syntax/

/indParUp/
/$$
The $code SPK_VA::valarray<double>$$ $italic indParUp$$ contains the vector 
$math%bUp%$$, which specifies the upper limit for the individual parameters 
for all the individuals.  The size of $italic indParUp$$ is equal to the 
length of the individual parameter vector $math%b%$$.

$syntax/

/indParStep/
/$$
The $code SPK_VA::valarray<double>$$ $italic indParStep$$ contains the vector 
$math%bStep%$$, which specifies the step size used for approximating
the derivatives with respect to the individual parameters.
The size of $italic indParStep$$ is equal to the length of 
the individual parameter vector $math%b%$$.

$syntax/

/formulation/
/$$
The $code int$$ $italic formulation$$ specifies which formulation of the 
covariance of the population parameter estimates is selected.  See Description 
section for details.  Only formulation "R" is available for FIRST_ORDER objective.

$syntax/

/popParCovOut/ 
/$$
If $italic popParCovOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParCovOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the square of the length of the population parameter vector 
$math%alp%$$.  If $italic popParCovOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic popParCovOut$$ will contain the covariance matrix
of the population parameter estimates, in column major order, that is evaluated 
at $math%alp%$$.  Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic popParCovOut$$.  

The $math%(i,j)%$$-the element of the covariance matrix will be 
replaced by NaN if $code popParMask[i]$$ or $code popParMask[j]$$ 
is $math%false%$$.

$syntax/

/popParSEOut/ 
/$$
If $italic popParSEOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParSEOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the length of the population parameter vector 
$math%alp%$$.  If $italic popParSEOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic popParSEOut$$ will contain the standard error vector
of the population parameter estimates, in column major order, that is evaluated 
at $math%alp%$$.  Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic popParSEOut$$.  

The $math%i%$$-th element of the standard error vector will be replaced 
by NaN if $code popParMask[i]$$ is $math%false%$$.

$syntax/

/popParCorOut/ 
/$$
If $italic popParCorOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParCorOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the square of the length of the population parameter vector 
$math%alp%$$.  If $italic popParCorOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic popParCorOut$$ will contain the correlation matrix 
of the population parameter estimates, in column major order, that is evaluated 
at $math%alp%$$.  Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic popParCorOut$$. 

The $math%(i,j)%$$-the element of the correlation matrix will be 
replaced by NaN if $code popParMask[i]$$ or $code popParMask[j]$$ 
is $math%false%$$.

$syntax/

/popParCVOut/ 
/$$
If $italic popParCVOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParCVOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the length of the population parameter vector 
$math%alp%$$.  If $italic popParCVOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic popParCVOut$$ will contain the standard error vector
of the population parameter estimates, in column major order, that is evaluated 
at $italic popPar$$.  Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic popParCVOut$$.  

The $math%i%$$-th element of the coefficient of variation vector will be 
replaced by NaN if $code popParMask[i]$$ is $math%false%$$.

$syntax/

/popParCIOut/ 
/$$
If $italic popParCIOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParCIOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the two times of the length of the population parameter vector 
$math%alp%$$.  If $italic popParCIOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object pointed 
to by $italic popParCIOut$$ will contain the 95% confidence interval values 
of the population parameter estimates, in column major order, that is evaluated 
at $italic popPar$$.  There are two columns in the object.  The first column 
contains the lower limit, and the second column contains the upper limit of 
the confidence interval of the population parameter estimates.  Otherwise, 
this function will not attempt to change the contents of the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParCIOut$$.  
Note that in the calculation of the confidence interval, if the degree of freedom 
(total number of data - number of population parameters) is greater than 120, 
it is treated as infinite.

The $math%(i,1)%$$ and $math%(i,2)%$$ elements of the confidence interval matrix
will be replaced by NaN if $code popParMask[i]$$ is $math%false%$$.

$end
*/

void popStatistics( SpkModel<double>&               popModel,
                    SpkModel< CppAD::AD<double> >&  popModelAD,
                    enum Objective                  objective,
                    const valarray<int>&            nMeasurementsAll,
                    const valarray<double>&         measurementsAll,
                    const valarray<double>&         popPar,
                    const valarray<bool>&           popParMask,
                    const valarray<double>&         popObj_popPar_popPar,
                    const valarray<double>&         indParAll,
                    const valarray<double>&         indParLow,
                    const valarray<double>&         indParUp,
                    const valarray<double>&         indParStep,
                    enum PopCovForm                 formulation,
                    valarray<double>*               popParCovOut,
                    valarray<double>*               popParSEOut,                          
                    valarray<double>*               popParCorOut,
                    valarray<double>*               popParCVOut,                          
                    valarray<double>*               popParCIOut )
{
    using std::endl;
    using std::ends;

  //----------------------------------------------------------------
    // Preliminaries.
    //----------------------------------------------------------------
    // Return if there are no output values to compute.
    if( popParCovOut == 0 && popParSEOut == 0 && popParCorOut == 0 && 
		popParCVOut && popParCIOut == 0 ) 
        return;

    const int nInd = nMeasurementsAll.size();
    const int nAlp = popPar.size();
    const int nB   = indParStep.size();
    const int nY   = measurementsAll.size();

    // Degree of freedom
    const int nF = nY - nAlp;
	
    if( popParCIOut && !nF )
	{
	  char message[ SpkError::maxMessageLen() ];
          snprintf( message, SpkError::maxMessageLen(),  
		   "The number of degrees of freedom (#of measurements<%d> - #of fixed effects<%d>) must be positive.\n",
		   nY, nAlp );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR, 
                message,
                __LINE__, __FILE__
        );
	}

    //===============[Begin: Vector lengths validation]===============
    if( nY != nMeasurementsAll.sum() )
    {
      char message[ SpkError::maxMessageLen() ];
      snprintf( message, SpkError::maxMessageLen(),
                "The sum <%d> of the values contained in nMeasurementsAll vector must match \nthe size <%d> of measurementsAll vector.\n",
	        nMeasurementsAll.sum(), nY );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR, 
                message,
                __LINE__, __FILE__
        );
    }
    if( indParLow.size() != nB )
    {
      char message[ SpkError::maxMessageLen() ];
      snprintf( message, SpkError::maxMessageLen(),
                "The length <%d> of indParLow vector must match the size <%d> of indPar vector.",
	        indParLow.size(), nB );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR, 
                message,
                __LINE__, __FILE__
        );
    }
    if( indParUp.size() != nB )
    {
      char message[ SpkError::maxMessageLen() ];
      snprintf( message, SpkError::maxMessageLen(),
                "The length <%d> of indParUp vector must match the size <%d> of indPar vector.",
	        indParUp.size(), nB );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR, 
                message,
                __LINE__, __FILE__
        );
    }
    if( indParAll.size() != nB * nInd )
    {
      char message[ SpkError::maxMessageLen() ];
      strcpy( message, "The length of indParAll vector must match the product of " );
      strcat( message, "the length of the indParStep vector and the number of individuals." );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR, 
                message,
                __LINE__, __FILE__
        );
    }
    // Check the size of the parameter mask.
    if ( popParMask.size() != nAlp )
    {
        throw SpkException(
            SpkError::SPK_USER_INPUT_ERR,
            "The length of the parameter mask must match the number of parameters.",
            __LINE__,
            __FILE__ );
    }

    // This is a column vector.
    if ( popObj_popPar_popPar.size() != nAlp * nAlp )
    {
      char message[ SpkError::maxMessageLen() ];
      snprintf( message, SpkError::maxMessageLen(),
	       "Tilde_alp_alp vector that contains the Hessian of objective function must have n times n length, \nwhere n is the size of population parameter <%d>.", 
	       popObj_popPar_popPar.size() );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR,  
                message,
                __LINE__, __FILE__
        );
    }  
    //===============[End: Vector lengths validation]===============
  
    //===============[Begin: Data validation]===============

    // Check that values in N are consistent with the size of y, i.e.,
    // 
    //     nY  =  N(1)  +  N(2)  +  ...  +  N(nInd)  .
    //
    for ( int i = 0; i < nInd; i++ )
    {
        if( nMeasurementsAll[i] < 0 )
        {
	  char message[ SpkError::maxMessageLen() ];
          snprintf( message, SpkError::maxMessageLen(),
		   "The number of measurements must be greater than zero.  The %s element, %d, is invalid.", 
		   intToOrdinalString( i, ZERO_IS_FIRST_INT ).c_str(),
		   nMeasurementsAll[i] );
            throw SpkException(
                    SpkError::SPK_USER_INPUT_ERR,  
                    message,
                    __LINE__, __FILE__
            );
        }
    }
    // Verify that the initial b values are between their
    // lower and upper bounds.
    for ( int j = 0; j < nInd; j++ )
    {
        for ( int i = 0; i < nB; i++ )
        {
            if( indParAll[ i + j * nB ] < indParLow[i] || indParAll[ i + j * nB ] > indParUp[i] )
            {
	      char message[ SpkError::maxMessageLen() ];
              snprintf( message, SpkError::maxMessageLen(),
                        "The initial value for the individual parameter must \
be less than or equal to \nthe upper bound value and \
greater than or equal to the lower bound value. \
\nThe %s element, %f, is invalid.",
		       intToOrdinalString( i, ZERO_IS_FIRST_INT ).c_str(),
		       indParAll[ i + j * nB ] );

                throw SpkException(
                        SpkError::SPK_USER_INPUT_ERR,  
                        message,
                        __LINE__, __FILE__
                );
            }
        }
    }
    //===============[End: Data validation]===============
    
    //----------------------------------------------------------------
    // Compute the partial derivative of the individual level 
    // objective with respect to the population parameter.
    //----------------------------------------------------------------
    valarray<double> indObj_popParAll( nAlp * nInd );
    if( formulation == RSR || formulation == S )
    {
        DoubleMatrix dmatLambdaLTilde_alpOut( nAlp, nInd );

        //------------------------------------------------------------
        // Convert valarray to DoubleMatrix 
        //------------------------------------------------------------
        DoubleMatrix dvecN( nInd, 1 );
        double * pN = dvecN.data();
        for( int i = 0; i < nInd; i++ )
            pN[ i ] = nMeasurementsAll[i];

        DoubleMatrix dvecY( measurementsAll );
        DoubleMatrix dvecAlpIn( popPar );
        DoubleMatrix dvecBLow( indParLow );
        DoubleMatrix dvecBUp( indParUp );
        DoubleMatrix dmatBIn( indParAll, nInd );
        DoubleMatrix dvecBStep( indParStep );

        //---------------------------------------------------------------------------
        // Compute the derivative with respect to the population parameter
        // of each individual's contribution to the population objective
        // function,
        // 
        //                    /                                                   \
        //                    |          (1) |          (2) |      |          (n) | 
        // LambdaLTilde_alp = | Labmda_alp   | Lambda_alp,  | ...  | Lambda_alp   |
        //                    |              |              |      |              |
        //                    \                                                   /
        // where the superscript (i) indentifies the i-th individual and
        // n is the total number of individuals.
        //---------------------------------------------------------------------------
        
        // Set the number of iterations for the population and
        // individual level optimizers equal to zero so that the
        // values for alp and b do not change.
        Optimizer popOptimizer( 1.0e-6, 0, 0 );
        Optimizer indOptimizer( 1.0e-6, 0, 0 );
  
        // Calculate the derivatives.
        if( objective != FIRST_ORDER )
        {
            // If the first order objective is not being used, then
            // calculate the derivatives in the normal way.
            lTilde( popModel, 
                    objective, 
                    dvecY, 
                    dvecN,
                    indOptimizer,
                    dvecAlpIn,
                    dvecBLow,
                    dvecBUp,
                    dvecBStep,
                    dmatBIn,
                    0,
                    0, 
                    0, 
                    &dmatLambdaLTilde_alpOut );
        }
        else
        {
            // If the first order objective is being used, then
            // construct some tempory values that won't be used
            // because the number of population iterations is zero.
            DoubleMatrix dvecAlpLow( dvecAlpIn );
            DoubleMatrix dvecAlpUp ( dvecAlpIn );
            DoubleMatrix dvecAlpStep( nAlp );
	    dvecAlpStep.fill( 1.0e-3 );

            // Calculate the derivatives in a more efficient way.
            firstOrderOpt( popModel, 
                           popModelAD,
                           dvecN,
                           dvecY, 
                           popOptimizer,
                           dvecAlpLow,
                           dvecAlpUp,
                           dvecAlpIn,
                           0,
                           dvecAlpStep,
                           indOptimizer,
                           dvecBLow,
                           dvecBUp,
                           dmatBIn,
                           0,
                           dvecBStep,
                           0, 
                           0, 
                           0, 
                           &dmatLambdaLTilde_alpOut );
        }

        indObj_popParAll = dmatLambdaLTilde_alpOut.toValarray();}
    

   popStatistics( popParMask,
                  measurementsAll,
                  popPar,
                  indObj_popParAll,
                  popObj_popPar_popPar,
                  formulation,
                  popParCovOut,
                  popParSEOut,
                  popParCorOut,
                  popParCVOut,
                  popParCIOut );
   return;
}


/*************************************************************************
 *
 * Function: popStatistics - does not calculate individual objective derivatives
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin popStatistics_DerivExist$$

$spell
  Enumerator
  valarray
  Cov
  Obj
  enum
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
  Beal
  Raton
  Ruppert
  Sheiner
  Stefanski
$$

$section Computing Statistics of Population Parameter Estimates when Individual Objective Derivatives have been Calculated$$

$index popStatistics$$
$index covariance, standard error, correlation matrix, population parameters$$
$cindex \Computing Statistics \of Population \Parameter \Estimates \when Individual \Objective \Derivatives \have \been \Calculated$$

$table
$bold Enumerator:$$ $cend
$syntax/enum PopCovForm { RSR, R, S }/$$ $rend
$bold Prototype:$$ $cend
$syntax/void popStatistics(  
                   const SPK_VA::valarray<double>& /mask/,
                   const SPK_VA::valarray<double>& /measurementsAll/,
                   const SPK_VA::valarray<double>& /popPar/,
                   const SPK_VA::valarray<double>& /indObj_popParAll/,
                   const SPK_VA::valarray<double>& /popObj_popPar_popPar/,
                   enum PopCovForm                 /formulation/,
                   SPK_VA::valarray<double>*       /popParCovOut/, 
                   SPK_VA::valarray<double>*       /popParSEOut/,                          
                   SPK_VA::valarray<double>*       /popParCorOut/,
                   SPK_VA::valarray<double>*       /popParCVOut/,                          
                   SPK_VA::valarray<double>*       /popParCIOut/
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
This function computes the covariance matrix, standard error vector, 
correlation matrix, coefficient of variation vector, and confidence 
interval vector for population parameter estimates.
It allows parameter elements that are not active to be specified 
and removed from the statistics computations.
$pre

$$
This function allows the covariance matrix of the population parameter 
estimates to be calculated using one of the following formulations:
$math%         

                                        -1     -1
    formulation "RSR":  cov[ alp ]  =  R   S  R   ;

                                        -1
    formulation "R":    cov[ alp ]  =  R   ;

                                        -1
    formulation "S":    cov[ alp ]  =  S   .

%$$
These formulations are discussed in Section (D.2.5) of the NONMEM 
Users Guide Part II and in Sections (A.2.1) and (A.2.2) in 
Carroll, Ruppert, and Stefanski (1998).
$pre

$$
The approximation made for the information matrix is
$math%

                                          T
     R = ( LTilde_alp_alp + LTilde_alp_alp  ) / 2  ,

%$$
where $math%LTilde_alp_alp%$$ is an approximation for the second order 
derivatives of the population objective function with respect to the 
population parameter $math%alp%$$.
The matrix $math%R%$$ is defined in this way to insure that it is symmetric
even for cases where the approximation $math%LTilde_alp_alp%$$ is not.
Note that this $math%R%$$ is different than $math%R(b)%$$, 
which is the model for the covariance of an individual's data 
and is part of $xref/SpkModel//SpkModel/$$.
$pre

$$
The cross-product gradient matrix is defined as
$math%

                                    T
     S = Sum{ [(LambdaTilde_i )_alp] [(LambdaTilde_i)_alp] }  ,
          i                          

%$$
where $math%(LambdaTilde_i)_alp%$$ is the derivative of the $th i$$
individual's objective function with respect to the population
parameter $math%alp%$$.
$pre

$$
The standard error vector is calculated by taking the square roots 
of the diagonal elements of the covariance matrix.
The correlation matrix is calculated by dividing each element of 
the covariance matrix by the standard errors that correspond to its 
row and column.
The coefficients of variation are calculated as
$math%
   
    CV    =  SE    / | alp    | * 100   ,
     (i)       (i)        (i)

%$$
where CV is the coefficient of variation and SE is the standard error.  
The 95% confidence intervals are calculated as
$math%
   
    ( alp    -  t               * SE    ,  alp    +  t               * SE    ) ,
         (i)     0.025, degFree     (i)       (i)     0.025, degFree     (i)

%$$
where
$math%
   
    t
     0.025, degFree

%$$
is the critical value for the $math%t%$$ distribution with 
$math%degFree = nY - nAlp%$$ number of degrees of freedom for which the 
area under the $math%t%$$ curve is $math%1 - 0.025%$$
and $math%nAlp%$$ is the number of population parameters.  

$head Reference$$
Beal, S. L. and Sheiner, L. B. (1988) $italic NONMEM Users Guide - Part II$$,
University of California, San Francisco.
$pre

$$
Carroll, R. J., Ruppert, D., and Stefanski, L. A. (1998) 
$italic Measurement Error in Nonlinear Models$$, Chapman & Hall/CRC,
Boca Raton, Florida.

$head Return Value$$
Upon a successful completion, the function sets
the given output value place holders to point to the result value.
  
$pre

$$
If an error is detected or failure occurs during the evaluation, a SpkException 
object is thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/
/mask/
/$$
$code mask$$ is a vector of boolean values of length equal to the parameter
vector, $code popPar$$.  $code mask[i]$$ tells as to whether $code popPar[i]$$
is active or not.  If $math%mask[i]%$$ is $math%false%$$, the i-th element of
the parameter vector are treated as if it does not exist and further 
statistics computations are performed based upon the assumption.

$syntax/

/measurementsAll/
/$$
The $code SPK_VA::valarray<double>$$ $italic measurementsAll$$ contains the vector
$math%y%$$, which specifies the data for all the individuals.
The vector $math%y%$$ has
$math%
    N(1) + N(2) + ... + N(M)
%$$
elements where $math%N(i)%$$ is the number of measurements for i-th individual
and $math%M%$$ is the total number of individuals in the population.
The data vector corresponding to the first individual is
$math%                                         
    y_1 = [ y(1) , y(2) , ... , y(N(1)) ]
%$$
Elements $math%y(N(1) + 1)%$$ through $math%y(N(1) + N(2))%$$ 
correspond to the second individual and so on.
(Note that $math%y_1%$$ refers to the first subvector of $math%y%$$ while
$math%y(1)%$$ refers to the first element of the valarray $math%y%$$.)

$syntax/

/popPar/
/$$
The $code SPK_VA::valarray<double>$$ $italic popPar$$ contains the vector 
$math%alp%$$, which specifies the estimates of the population parameters.  
The returned covariance matrix $italic popParCovOut$$ will be evaluated at 
these values.  
The $italic popPar$$ should be obtained by calling the SPK function 
$xref/fitPopulation//fitPopulation/$$.

$syntax/

/popObj_popPar_popPar/ 
/$$
The $code SPK_VA::valarray<double>$$ $italic popObj_popPar_popPar$$ contains 
the matrix $math%LTilde_alp_alp%$$, in column major order, which specifies 
an approximation for the second derivative of the population objective 
function with respect to population parameter evaluated at $italic popPar$$.  
Note that the size of $italic popObj_popPar_popPar$$ should be equal to the 
square of the length of the population parameter vector $math%alp%$$.  
The $italic popObj_popPar_popPar$$ should be obtained by calling the SPK function 
$xref/fitPopulation//fitPopulation/$$. 

$syntax/

/indObj_popParAll/
/$$
The $code SPK_VA::valarray<double>$$ $italic indObj_popParAll$$ contains 
the matrix $math%LamdbdaTilde_alpAll%$$, in column major order, each 
column of which specifies the transpose of one of the derivatives, 
$math%(LambdaTilde_i)_alp%$$.
The size of  $italic indObj_popParAll$$ is equal to the product of the 
length of the population parameter vector $math%alp%$$ and the number 
of individuals in the population $math%M%$$. 
The $th i$$ column of $math%LamdbdaTilde_alpAll%$$ contains the 
transpose of the row vector $math%(LambdaTilde_i)_alp%$$, 
which specifies the derivative of the $th i$$ individual's objective 
function with respect to the population parameter $math%alp%$$.
Note that the row dimension of $math%LamdbdaTilde_alpAll%$$ is equal
to the length of the population parameter vector $math%alp%$$, and its
column dimension is equal to the number of individuals in the
population, $math%M%$$,
The $italic indObj_popParAll$$ should be obtained by calling the SPK
function $xref/lTilde//lTilde/$$.

$syntax/

/formulation/
/$$
The $code int$$ $italic formulation$$ specifies which formulation of the 
covariance of the population parameter estimates is selected.  See Description 
section for details.  Only formulation "R" is available for FIRST_ORDER objective.

$syntax/

/popParCovOut/ 
/$$
If $italic popParCovOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParCovOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the square of the length of the population parameter vector 
$math%alp%$$.  If $italic popParCovOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic popParCovOut$$ will contain the covariance matrix
of the population parameter estimates, in column major order, that is evaluated 
at $math%alp%$$.  Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic popParCovOut$$.  

The $math%(i,j)%$$-the element of the covariance matrix
will be replaced by NaN if $code mask[i]$$ or $code mask[j]$$ is $math%false%$$.

$syntax/

/popParSEOut/ 
/$$
If $italic popParSEOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParSEOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the length of the population parameter vector 
$math%alp%$$.  If $italic popParSEOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic popParSEOut$$ will contain the standard error vector
of the population parameter estimates, in column major order, that is evaluated 
at $math%alp%$$.  Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic popParSEOut$$.  

The $math%i%$$-th element of the standard error vector
will be replaced by NaN if $code mask[i]$$ is $math%false%$$.

$syntax/

/popParCorOut/ 
/$$
If $italic popParCorOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParCorOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the square of the length of the population parameter vector 
$math%alp%$$.  If $italic popParCorOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic popParCorOut$$ will contain the correlation matrix 
of the population parameter estimates, in column major order, that is evaluated 
at $math%alp%$$.  Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic popParCorOut$$. 

The $math%(i, j)%$$-th element of the correlation matrix
will be replaced by NaN if $code mask[i]$$ or $code mask[j]$$ is $math%false%$$.

$syntax/

/popParCVOut/ 
/$$
If $italic popParCVOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParCVOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the length of the population parameter vector 
$math%alp%$$.  If $italic popParCVOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic popParCVOut$$ will contain the standard error vector
of the population parameter estimates, in column major order, that is evaluated 
at $italic popPar$$.  Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic popParCVOut$$.  

The $math%i%$$-th element of the coefficient vector
will be replaced by NaN if $code mask[i]$$ is $math%false%$$.

$syntax/

/popParCIOut/ 
/$$
If $italic popParCIOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParCIOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the two times of the length of the population parameter vector 
$math%alp%$$.  If $italic popParCIOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object pointed 
to by $italic popParCIOut$$ will contain the 95% confidence interval values 
of the population parameter estimates, in column major order, that is evaluated 
at $italic popPar$$.  There are two columns in the object.  The first column 
contains the lower limit, and the second column contains the upper limit of 
the confidence interval of the population parameter estimates.  Otherwise, 
this function will not attempt to change the contents of the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParCIOut$$.  
Note that in the calculation of the confidence interval, if the degree of freedom 
(total number of data - number of population parameters) is greater than 120, 
it is treated as infinite.

The $math%(i,1)%$$ and $math%(i,2)%$$ elements of the confidence interval matrix
will be replaced by NaN if $code mask[i]$$ is $math%false%$$.

$end
*/

void popStatistics( const valarray<bool>           & mask,
		    const SPK_VA::valarray<double> & y,
		    const SPK_VA::valarray<double> & alp,
		    const SPK_VA::valarray<double> & indObj_alp,
		    const SPK_VA::valarray<double> & popObj_alp_alp,
		    enum PopCovForm                 form,
		    SPK_VA::valarray<double>      * alpCovOut,
		    SPK_VA::valarray<double>      * alpSEOut,
		    SPK_VA::valarray<double>      * alpCorOut,
		    SPK_VA::valarray<double>      * alpCVOut,
		    SPK_VA::valarray<double>      * alpCIOut
                   )
{
   const int nAlp = alp.size();
   const int nY   = y.size();
   const int nInd = indObj_alp.size() / nAlp;
   assert( nInd * nAlp == indObj_alp.size() );
                                                                                                        
   const valarray<double> x = alp[ mask ];
   const int nX = x.size();
                                                                                                          
                                                                                                        
   valarray<double> indObj_x( nInd * nX );
   valarray<double> popObj_x_x( nX * nX );
   valarray<double> xCov( nX * nX );
   valarray<double> xSE ( nX );
   valarray<double> xCor( nX * nX );
   valarray<double> xCV ( nX );
   valarray<double> xCI ( nX * 2 );

   for( int j=0; j<nInd; j++ )
   {
      for( int i=0, ii=0; i<nAlp; i++ )
      {
         if( mask[i] )
         {
            indObj_x[ ii + j * nX ] = indObj_alp[ i + j * nAlp ];
            ii++;
         }
       }
   }
                                                                                                        
   for( int j=0, jj=0; j<nAlp; j++ )
   {
      if( mask[j] )
      {
         for( int i=0, ii=0; i<nAlp; i++ )
         {
            if( mask[i] )
            {
               popObj_x_x[ ii + jj*nX ] = popObj_alp_alp[ i + j*nAlp ];
               ii++;
            }
         }
         jj++;
      }
   }
                                                                                                        
   popStatistics(y,
                 x,
                 indObj_x,
                 popObj_x_x,
                 form,
                 ( alpCovOut? &xCov : NULL ),
                 ( alpSEOut?  &xSE  : NULL ),
                 ( alpCorOut? &xCor : NULL ),
                 ( alpCVOut?  &xCV  : NULL ),
                 ( alpCIOut?  &xCI  : NULL )
                 );
                                
  valarray<bool> alpCI_mask ( nAlp * 2 );
  valarray<bool> alpSE_mask ( nAlp );
  valarray<bool> alpCV_mask ( nAlp );
  valarray<bool> alpCov_mask( nAlp * nAlp );
  valarray<bool> alpCor_mask( nAlp * nAlp );
  double val = NAN;

  for( int j=0; j<2; j++ )
    {
      for( int i=0; i<nAlp; i++ )
	{
	  alpCI_mask[ i + j * nAlp ] = mask[i];
	}
    }

  for( int j=0; j<nAlp; j++ )
    {
      if( mask[j] )
	{
	  for( int i=0; i<nAlp; i++ )
	    {
	      alpCov_mask[ i + j * nAlp ] = mask[i];
	      alpCor_mask[ i + j * nAlp ] = mask[i];
	    }
       
	  alpSE_mask[ j ] = mask[j];
	  alpCV_mask[ j ] = mask[j];
	}
      else
	{
	  alpCov_mask[ slice( j * nAlp, nAlp, 1 ) ] = false;
	  alpCor_mask[ slice( j * nAlp, nAlp, 1 ) ] = false;
	}
    }
  if( alpCIOut )
    {
      placeVal( alpCI_mask, xCI, *alpCIOut, val );
    }
  if( alpCovOut )
    {
      placeVal( alpCov_mask, xCov, *alpCovOut, val );
    }
  if( alpCorOut )
    {
      placeVal( alpCor_mask, xCor, *alpCorOut, val );
    }
  if( alpSEOut )
    {
      placeVal( alpSE_mask, xSE, *alpSEOut, val );
    }
  if( alpCVOut )
    {
      placeVal( alpCV_mask, xCV, *alpCVOut, val );
    }
                                                                        
}


/*************************************************************************
 *
 * Function: popStatistics - does not calculate individual derivatives
 *                         - all elements must be active
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin popStatistics_DerivExist_AllElemActive$$

$spell
  Enumerator
  valarray
  Cov
  Obj
  enum
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
  Beal
  Raton
  Ruppert
  Sheiner
  Stefanski
$$

$section Computing Statistics of Population Parameter Estimates when Individual Objective Derivatives have been Calculated and All Elements are Active$$

$index popStatistics$$
$index covariance, standard error, correlation matrix, population parameters$$
$cindex \Computing Statistics \of Population \Parameter \Estimates \when Individual \Objective \Derivatives \have \been \Calculated \and All \Elements \are \Active$$

$table
$bold Enumerator:$$ $cend
$syntax/enum PopCovForm { RSR, R, S }/$$ $rend
$bold Prototype:$$ $cend
$syntax/void popStatistics(  
                   const SPK_VA::valarray<double>& /measurementsAll/,
                   const SPK_VA::valarray<double>& /popPar/,
                   const SPK_VA::valarray<double>& /indObj_popParAll/,
                   const SPK_VA::valarray<double>& /popObj_popPar_popPar/,
                   enum PopCovForm                 /formulation/,
                   SPK_VA::valarray<double>*       /popParCovOut/, 
                   SPK_VA::valarray<double>*       /popParSEOut/,                          
                   SPK_VA::valarray<double>*       /popParCorOut/,
                   SPK_VA::valarray<double>*       /popParCVOut/,                          
                   SPK_VA::valarray<double>*       /popParCIOut/
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
This function computes the covariance matrix, standard error vector, 
correlation matrix, coefficient of variation vector, and confidence 
interval vector for population parameter estimates.
$pre

$$
This function allows the covariance matrix of the population parameter 
estimates to be calculated using one of the following formulations:
$math%         

                                        -1     -1
    formulation "RSR":  cov[ alp ]  =  R   S  R   ;

                                        -1
    formulation "R":    cov[ alp ]  =  R   ;

                                        -1
    formulation "S":    cov[ alp ]  =  S   .

%$$
These formulations are discussed in Section (D.2.5) of the NONMEM 
Users Guide Part II and in Sections (A.2.1) and (A.2.2) in 
Carroll, Ruppert, and Stefanski (1998).
$pre

$$
The approximation made for the information matrix is
$math%

                                          T
     R = ( LTilde_alp_alp + LTilde_alp_alp  ) / 2  ,

%$$
where $math%LTilde_alp_alp%$$ is an approximation for the second order 
derivatives of the population objective function with respect to the 
population parameter $math%alp%$$.
The matrix $math%R%$$ is defined in this way to insure that it is symmetric
even for cases where the approximation $math%LTilde_alp_alp%$$ is not.
Note that this $math%R%$$ is different than $math%R(b)%$$, 
which is the model for the covariance of an individual's data 
and is part of $xref/SpkModel//SpkModel/$$.
$pre

$$
The cross-product gradient matrix is defined as
$math%

                                    T
     S = Sum{ [(LambdaTilde_i )_alp] [(LambdaTilde_i)_alp] }  ,
          i                          

%$$
where $math%(LambdaTilde_i)_alp%$$ is the derivative of the $th i$$
individual's objective function with respect to the population
parameter $math%alp%$$.
$pre

$$
The standard error vector is calculated by taking the square roots 
of the diagonal elements of the covariance matrix.
The correlation matrix is calculated by dividing each element of 
the covariance matrix by the standard errors that correspond to its 
row and column.
The coefficients of variation are calculated as
$math%
   
    CV    =  SE    / | alp    | * 100   ,
     (i)       (i)        (i)

%$$
where CV is the coefficient of variation and SE is the standard error.  
The 95% confidence intervals are calculated as
$math%
   
    ( alp    -  t               * SE    ,  alp    +  t               * SE    ) ,
         (i)     0.025, degFree     (i)       (i)     0.025, degFree     (i)

%$$
where
$math%
   
    t
     0.025, degFree

%$$
is the critical value for the $math%t%$$ distribution with 
$math%degFree = nY - nAlp%$$ number of degrees of freedom for which the 
area under the $math%t%$$ curve is $math%1 - 0.025%$$
and $math%nAlp%$$ is the number of population parameters.  

$head Reference$$
Beal, S. L. and Sheiner, L. B. (1988) $italic NONMEM Users Guide - Part II$$,
University of California, San Francisco.
$pre

$$
Carroll, R. J., Ruppert, D., and Stefanski, L. A. (1998) 
$italic Measurement Error in Nonlinear Models$$, Chapman & Hall/CRC,
Boca Raton, Florida.

$head Return Value$$
Upon a successful completion, the function sets
the given output value place holders to point to the result value.
  
$pre

$$
If an error is detected or failure occurs during the evaluation, a SpkException 
object is thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/
/measurementsAll/
/$$
The $code SPK_VA::valarray<double>$$ $italic measurementsAll$$ contains the vector
$math%y%$$, which specifies the data for all the individuals.
The vector $math%y%$$ has
$math%
    N(1) + N(2) + ... + N(M)
%$$
elements where $math%N(i)%$$ is the number of measurements for i-th individual
and $math%M%$$ is the total number of individuals in the population.
The data vector corresponding to the first individual is
$math%                                         
    y_1 = [ y(1) , y(2) , ... , y(N(1)) ]
%$$
Elements $math%y(N(1) + 1)%$$ through $math%y(N(1) + N(2))%$$ 
correspond to the second individual and so on.
(Note that $math%y_1%$$ refers to the first subvector of $math%y%$$ while
$math%y(1)%$$ refers to the first element of the valarray $math%y%$$.)

$syntax/

/popPar/
/$$
The $code SPK_VA::valarray<double>$$ $italic popPar$$ contains the vector 
$math%alp%$$, which specifies the estimates of the population parameters.  
The returned covariance matrix $italic popParCovOut$$ will be evaluated at 
these values.  
The $italic popPar$$ should be obtained by calling the SPK function 
$xref/fitPopulation//fitPopulation/$$.

$syntax/

/popObj_popPar_popPar/ 
/$$
The $code SPK_VA::valarray<double>$$ $italic popObj_popPar_popPar$$ contains 
the matrix $math%LTilde_alp_alp%$$, in column major order, which specifies 
an approximation for the second derivative of the population objective 
function with respect to population parameter evaluated at $italic popPar$$.  
Note that the size of $italic popObj_popPar_popPar$$ should be equal to the 
square of the length of the population parameter vector $math%alp%$$.  
The $italic popObj_popPar_popPar$$ should be obtained by calling the SPK function 
$xref/fitPopulation//fitPopulation/$$. 

$syntax/

/indObj_popParAll/
/$$
The $code SPK_VA::valarray<double>$$ $italic indObj_popParAll$$ contains
the partial derivative of the individual level objective with
respect to the population parameter evaluated at $italic popPar$$.

$syntax/

/formulation/
/$$
The $code int$$ $italic formulation$$ specifies which formulation of the 
covariance of the population parameter estimates is selected.  See Description 
section for details.  Only formulation "R" is available for FIRST_ORDER objective.

$syntax/

/popParCovOut/ 
/$$
If $italic popParCovOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParCovOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the square of the length of the population parameter vector 
$math%alp%$$.  If $italic popParCovOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic popParCovOut$$ will contain the covariance matrix
of the population parameter estimates, in column major order, that is evaluated 
at $math%alp%$$.  Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic popParCovOut$$.  

$syntax/

/popParSEOut/ 
/$$
If $italic popParSEOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParSEOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the length of the population parameter vector 
$math%alp%$$.  If $italic popParSEOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic popParSEOut$$ will contain the standard error vector
of the population parameter estimates, in column major order, that is evaluated 
at $math%alp%$$.  Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic popParSEOut$$.  

$syntax/

/popParCorOut/ 
/$$
If $italic popParCorOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParCorOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the square of the length of the population parameter vector 
$math%alp%$$.  If $italic popParCorOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic popParCorOut$$ will contain the correlation matrix 
of the population parameter estimates, in column major order, that is evaluated 
at $math%alp%$$.  Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic popParCorOut$$. 

$syntax/

/popParCVOut/ 
/$$
If $italic popParCVOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParCVOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the length of the population parameter vector 
$math%alp%$$.  If $italic popParCVOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic popParCVOut$$ will contain the standard error vector
of the population parameter estimates, in column major order, that is evaluated 
at $italic popPar$$.  Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic popParCVOut$$.  

$syntax/

/popParCIOut/ 
/$$
If $italic popParCIOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParCIOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the two times of the length of the population parameter vector 
$math%alp%$$.  If $italic popParCIOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object pointed 
to by $italic popParCIOut$$ will contain the 95% confidence interval values 
of the population parameter estimates, in column major order, that is evaluated 
at $italic popPar$$.  There are two columns in the object.  The first column 
contains the lower limit, and the second column contains the upper limit of 
the confidence interval of the population parameter estimates.  Otherwise, 
this function will not attempt to change the contents of the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popParCIOut$$.  
Note that in the calculation of the confidence interval, if the degree of freedom 
(total number of data - number of population parameters) is greater than 120, 
it is treated as infinite.

$head Example$$
The following demonstrates running popStatistics().

$codep

#include <iostream>
#include <cmath>
#include "randNormal.h"
#include "SpkModel.h"
#include "lTilde.h"
#include "inverse.h"
#include "multiply.h"
#include "popStatistics.h"
#include "printInMatrix.h"
#include "fitPopulation.h"
#include "SpkValarray.h"

using namespace std;

class UserModelPopStatisticsExampleTest : public SpkModel<double>
{
    valarray<double> _a, _b;
    const int _nA;
    const int _nB;
    const int _nYi;
    int _i;
public:
    UserModelPopStatisticsExampleTest(int nA, int nB, int nYi)
    :_nA(nA), _nB(nB), _nYi(nYi)
    {};    
    ~UserModelPopStatisticsExampleTest(){};
private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const valarray<double>& aval)
    {
        _a = aval;
    }
    void doSetIndPar(const valarray<double>& bval)
    {
        _b = bval;
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
        //
        // D = [ alp[1] ]
        //
        ret.resize(_nYi);
        ret[0] = _a[1];
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        //
        // D_alp = [ 0  1 ]
        //
        ret.resize(_nYi * _nA);
        ret[0] = 0.0;
        ret[1] = 1.0;
        return true;
    }
    void doIndParVarianceInv( valarray<double>& ret ) const
    {
        //
        // Dinv = [ 1.0 / alp[1] ]
        //
        assert(_a[1] != 0.0);
        ret.resize(_nB * _nB);
        ret[0] = ( 1.0 / _a[1] );
    }
    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        // Dinv_alp = [ 0    -alp[1]^(-2) ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = -1.0 / (_a[1]*_a[1]);
        return true;
    }
    void doDataMean( valarray<double>& ret ) const
    {
        //
        // f = [ alp[0]+b[0] ]
        //
        ret.resize(_nYi);
        ret[0] = ( _a[0] + _b[0] );
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
        //
        // f_alp = [ 1   0 ]
        //
        ret.resize(_nYi * _nA);
        ret[0] = 1.0;
        ret[1] = 0.0;
        return true;
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        //
        // f_b = [ 1 ]
        //
        ret.resize(_nYi * _nB);
        ret[0] = 1.0;
        return true;
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        //
        // R = [ 1 ]
        //
        ret.resize(_nB*_nB);
        ret[0] = 1.0;
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        //
        // R_alp = [ 0   0 ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
        return false;
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        //
        // R_b = [ 0 ]
        //
        ret.resize(_nB *_nB);
        ret[0] = 0.0;
        return false;
    }
    void doDataVarianceInv( valarray<double>& ret ) const
    {
        //
        // Rinv = [ 1 ]
        //
        ret.resize(_nB * _nB);
        ret[0] = 1.0;
    }
    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
        //
        // Rinv_alp = [ 0  0 ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
        return false;
    }
    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
        //
        // Rinv_b = [ 0 ]
        //
        ret.resize(_nB * _nB * _nB);
        ret[0] = 0.0;
        return false;
    }   

};

int main()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int i;

  //preTestPrinting( "Specification Example" );

  // Objective
  enum Objective whichObjective = MODIFIED_LAPLACE;

  // Number of individuals.
  const int nInd = 10;

  // Number of measurements per individual (same for all)
  const int nYi = 1;

  // Number of measurements in total
  const int nY = nInd * nYi;

  const int nPopPar = 2;

  const int nIndPar = 1;

  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  UserModelPopStatisticsExampleTest model( nPopPar, nIndPar, nYi );


  //------------------------------------------------------------
  // Quantities that define the problem.
  //------------------------------------------------------------

  // Mean and variance of the true transfer rate, betaTrue.
  double meanBetaTrue = 1.0;
  double varBetaTrue  = 5.0;

  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  // Measurement values, y.
  valarray<double> Y( nY );

  // Number of measurements for each individual. 
  valarray<int> N( 1., nInd );

  // These will hold the generated values for the true measurement 
  // noise, eTrue, and the true random population parameters, bTrue.
  double eTrue;
  double bTrue;

  // Mean, variance, and standard deviation of eTrue and bTrue.
  double meanETrue = 0.0;
  double varETrue  = 1.0;
  double sdETrue   = sqrt( varETrue );
  double meanBTrue = 0.0;
  double varBTrue  = varBetaTrue;
  double sdBTrue   = sqrt( varBTrue );

  // Compute the measurements for each individual.
  Integer seed = 0;
  g05cbc(seed);
  for ( i = 0; i < nInd; i++ )
  {
    eTrue = randomNormal( meanETrue, sdETrue );
    bTrue = randomNormal( meanBTrue, sdBTrue );

    Y[ i ] = meanBetaTrue + bTrue + eTrue;
  }

  //------------------------------------------------------------
  // Quantities related to the fixed population parameter, popPar.
  //------------------------------------------------------------

  valarray<double> popPar    ( nPopPar );
  valarray<double> popParLow ( nPopPar );
  valarray<double> popParUp  ( nPopPar );
  valarray<double> popParIn  ( nPopPar );
  valarray<double> popParOut ( nPopPar );
  valarray<double> popParStep( nPopPar );

  // Set the values associated with popPar(1).
  popParTrue[ 0 ] = meanBetaTrue;
  popParLow [ 0 ] = -10.0;
  popParUp  [ 0 ] = 10.0;
  popParIn  [ 0 ] = -1.0;
  popParStep[ 0 ] = 1.0e-2;

  // Set the values associated with popPar(2).
  popParTrue[ 1 ] = varBetaTrue;
  popParLow [ 1 ] = 1.0e-3;
  popParUp  [ 1 ] = 100.0;
  popParIn  [ 1 ] = 0.5;
  popParStep[ 1 ] = 1.0e-2;
  

  //------------------------------------------------------------
  // Quantities related to the random population parameters, indPar.
  //------------------------------------------------------------

  valarray<double> indParLow ( -1.5e+1, nIndPar );
  valarray<double> indParUp  ( +1.0e+1, nIndPar );
  valarray<double> indParStep(  1.0e-2, nIndPar );

  valarray<double> indParIn ( 1., nIndPar * nInd );
  valarray<double> indParOut(     nIndPar * nInd );


  //------------------------------------------------------------
  // Quantities related to the population objective function.
  //------------------------------------------------------------

  double popObjOut;

  valarray<double> popObj_popParOut    ( nPopPar );
  valarray<double> popObj_popPar_popParOut( nPopPar * nPopPar );


  //------------------------------------------------------------
  // Remaining inputs to fitPopulation.
  //------------------------------------------------------------

  // Set the values associated with the individual objective function.
  Optimizer indOptimizer( 1.0e-6, 40, 0 );

  // Set the values associated with the population objective function.
  Optimizer popOptimizer( 1.0e-6, 40, 0 );

  //------------------------------------------------------------
  // Optimize the population objective function.
  //------------------------------------------------------------

  bool ok;
  try{
      fitPopulation(
                     model,
                     whichObjective,
                     N,
                     Y,
                     popOptimizer,
                     popParLow,
                     popParUp,
                     popParIn,
                     popParStep,
                     &popParOut,
                     indOptimizer,
                     indParLow,
                     indParUp,
                     indParIn,            
                     indParStep,
                     &indParOut,
                     &popObjOut,
                     &popObj_popParOut,
                     &popObj_popPar_popParOut 
                   );
      ok = true;
  }
  catch(...)
  {
    cerr << "fitPopulation failed" << endl;
    return -1;
  }

  cout << "=======================" << endl;
  cout << "objective = MODIFIED_LAPLACE" << endl;
  cout << "Population objective = " << popObjOut << endl;
  cout << "popPar = " << endl;
  printInMatrix( popParOut, 1 );
  cout << "popObj_popPar_popParOut = " << endl;
  printInMatrix( popObj_popPar_popParOut, nAlp );
  cout << "-----------------------" << endl;


  //------------------------------------------------------------
  // Obtain the partial derivative of the individual level
  // objective with respect to the population parameter.
  //------------------------------------------------------------
  valarray<double> indObj_popParAllOut( nPopPar );
  try{
       lTilde( model,
               whichObjective,
               N,
               Y,
               indOptimizer,
               popParOut,
               indParLow,
               indParUp,
               indParStep,
               indParOut,
               0,
               0,
               0,
              &indObj_popParAllOut );
  }
  catch( ... )
    {
      cerr << "lTilde failed" << endl;
      return -1;
    }

  //------------------------------------------------------------
  // Compute statistics of population parameter estimates.
  //------------------------------------------------------------
  valarray<double> popParCovOut( nPopPar* nPopPar );
  valarray<double> popParSEOut ( nPopPar );
  valarray<double> popParCorOut( nPopPar * nPopPar );
  valarray<double> popParCVOut ( nPopPar );
  valarray<double> popParCIOut ( nPopPar * 2 );

  try
  {
  for( int form = 1; form < 4; form++ )
  {
      popStatistics(
                     Y,
                     popParOut,
                     indObj_popParAll,
                     popObj_alp_alp,
                     enum PopCovForm(form),
                     &popParCovOut,
                     &popParSEOut,
                     &popParCorOut,
                     &popParCVOut,
                     &popParCIOut
                   );

    cout << "formulation = " << form << endl;
    cout << "popParCovOut = " << endl;
    printInMatrix( popParCovOut, nAlp );
    cout << "popParSEOut = " << endl;
    printInMatrix( popParSEOut, 1 );
    cout << "popParCorOut = " << endl;
    printInMatrix( popParCorOut, nAlp );
    cout << "popParCVOut = " << endl;
    printInMatrix( popParCVOut, 1 );
    cout << "popParCIOut = " << endl;
    printInMatrix( popParCIOut, 2 );
      cout << "-----------------------" << endl;
    }
  }
  catch(...)
  {
  cerr << "popStatistics failed" << endl;
    return 0;
  }
  return 0;
}
$$
The program will display the following when it is run:
$codep

=======================
objective = MODIFIED_LAPLACE
popObj = 21.8566
popPar =
[ 1.95115 ]
[ 3.63406 ]
popObj_popPar_popParOut =
[ 2.15793 -5.54357e-009 ]
[ -5.58165e-009 0.232837 ]
-----------------------
formulation = 1
popParCovOut =
[ 0.463406 -0.116585 ]
[ -0.116585 2.38831 ]
popParSEOut =
[ 0.68074 ]
[ 1.54541 ]
popParCorOut =
[ 1 -0.11082 ]
[ -0.11082 1 ]
popParCVOut =
[ 34.8891 ]
[ 42.5258 ]
popParCIOut =
[ 0.381368 3.52094 ]
[ 0.0703381 7.19779 ]
-----------------------
formulation = 2
popParCovOut =
[ 0.463406 1.1071e-008 ]
[ 1.1071e-008 4.29485 ]
popParSEOut =
[ 0.68074 ]
[ 2.0724 ]
popParCorOut =
[ 1 7.84753e-009 ]
[ 7.84753e-009 1 ]
popParCVOut =
[ 34.8891 ]
[ 57.0271 ]
popParCIOut =
[ 0.381368 3.52094 ]
[ -1.1449 8.41302 ]
-----------------------
formulation = 3
popParCovOut =
[ 0.469168 0.21226 ]
[ 0.21226 7.81939 ]
popParSEOut =
[ 0.684959 ]
[ 2.79632 ]
popParCorOut =
[ 1 0.11082 ]
[ 0.11082 1 ]
popParCVOut =
[ 35.1053 ]
[ 76.9474 ]
popParCIOut =
[ 0.371639 3.53067 ]
[ -2.81424 10.0824 ]
-----------------------

$$

$end
*/

void popStatistics( const valarray<double>& y,
                    const valarray<double>& alp,
                    const valarray<double>& indObj_alp,
                    const valarray<double>& popObj_alp_alp,
                    const PopCovForm      & formulation,
                    valarray<double>      * alpCovOut,
                    valarray<double>      * alpSEOut,
                    valarray<double>      * alpCorOut,
                    valarray<double>      * alpCVOut,
                    valarray<double>      * alpCIOut )
{
   const int nAlp = alp.size();
   const int nFree = y.size() - nAlp;

   valarray<double> alpCovTemp( nAlp * nAlp );
   if( formulation == R )
   {
      try{
        // This local routine nmInvR does symmetrize the 2nd drivative before
	// attempting to invert it.  So, if this attempt fails, it means
	// the symmetric matrix is not positive definite.
	alpCovTemp = nmInvR( popObj_alp_alp, nAlp );
      }
      catch( SpkException& e )
      {
         char mess[ SpkError::maxMessageLen() ];
	 snprintf( mess, SpkError::maxMessageLen(),
                   "Failed to invert the Hessian approximation for the information matrix.\n" );
	 e.push( SpkError::SPK_NOT_INVERTABLE_ERR, mess, __LINE__, __FILE__ );
	 throw e;  
      }
      catch( ... )
      {
         char mess[ SpkError::maxMessageLen() ];
	 snprintf( mess, SpkError::maxMessageLen(),
                   "Failed to invert the Hessian approximation for the information matrix.\n" );
	 SpkException e( SpkError::SPK_NOT_INVERTABLE_ERR, mess, __LINE__, __FILE__ );
	 throw e;  
      }
   }
   else if( formulation == S )
   {
      try
      {
         alpCovTemp = inverse( nmS( indObj_alp, nAlp ), nAlp );
      }
      catch( SpkException& e )
      {
         char mess[ SpkError::maxMessageLen() ];
	 snprintf( mess, SpkError::maxMessageLen(),
                   "Failed to invert the cross product gradient approximation for the information matrix.\n" );
         e.push( SpkError::SPK_NOT_INVERTABLE_ERR, mess, __LINE__, __FILE__ );
         throw e;  
      }
      catch( ... )
      {
         char mess[ SpkError::maxMessageLen() ];
	 snprintf( mess, SpkError::maxMessageLen(),
                   "Failed to invert the cross product gradient approximation for the information matrix.\n" );
         SpkException e( SpkError::SPK_NOT_INVERTABLE_ERR, mess, __LINE__, __FILE__ );
         throw e;  
         }
      }
   else //( formulation == RSR )
   {
      try
      {
         valarray<double> RInv = nmInvR( popObj_alp_alp, nAlp );
	 alpCovTemp = multiply( multiply( RInv, nAlp, nmS( indObj_alp, nAlp ), nAlp ), nAlp, 
                                          RInv, nAlp );
      }
      catch( SpkException& e )
      {
         char mess[ SpkError::maxMessageLen() ];
	 snprintf( mess, SpkError::maxMessageLen(),
                   "Failed to invert the sandwich approximation for the information matrix.\n" );
         e.push( SpkError::SPK_NOT_INVERTABLE_ERR, mess, __LINE__, __FILE__ );
         throw e;  
      }
      catch( ... )
      {
         char mess[ SpkError::maxMessageLen() ];
	 snprintf( mess, SpkError::maxMessageLen(),
                   "Failed to invert the sandwich approximation for the information matrix.\n" );
         SpkException e( SpkError::SPK_NOT_INVERTABLE_ERR, mess, __LINE__, __FILE__ );
         throw e;  
      }
   }
   try
   {
      statistics( alp, 
                  alpCovTemp, 
                  nFree, 
                  alpSEOut, 
		  alpCorOut, 
		  alpCVOut, 
		  alpCIOut );
   }
   catch( SpkException& e )
   {
      char mess[ SpkError::maxMessageLen() ];
      snprintf( mess, SpkError::maxMessageLen(),
                "Failed to compute some/all of the Statistics values.\n" );
      e.push( SpkError::SPK_STATISTICS_ERR, mess, __LINE__, __FILE__ );
      throw e;  
   }
   catch( ... )
   {
      char mess[ SpkError::maxMessageLen() ];
      snprintf( mess, SpkError::maxMessageLen(),
                "Failed to compute some/all of the statistics values.\n" );
      SpkException e( SpkError::SPK_UNKNOWN_ERR, mess, __LINE__, __FILE__ );
      throw e;  
   }


   *alpCovOut = alpCovTemp;
   return;
}

