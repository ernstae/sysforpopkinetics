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
 *************************************************************************/

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
  nagg
  statistics
$$

$section Computing statistics of population parameter estimates$$

$index popStatistics$$
$index covariance, standard error, correlation matrix, population parameters$$

$table
$bold Enumerator:$$ $cend
$syntax/enum PopCovForm { E, R, S }/$$ $rend
$bold Prototype:$$ $cend
$syntax/void popStatistics(  
                   SpkModel&                       /popModel/,
                   enum Objective                  /objective/,
                   const SPK_VA::valarray<int>&    /nMeasurementsAll/,
                   const SPK_VA::valarray<double>& /measurementsAll/,
                   const SPK_VA::valarray<double>& /popPar/,
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
This function computes covariance matrix, standard error vector, and correlation 
matrix of estimated population parameters.
Spk allows the user to choose the form for the covariance matrix of the 
population parameter estimates to be one of the following three formulations:
$math%         
                       -1   -1
    formulation "E":  R  S R
                       -1
    formulation "R":  R
                       -1
    formulation "S":  S

%$$
These formulations are given in NONMEM documentation.  In Spk notation,
$math%

                                          T
     R = ( LTilde_alp_alp + LTilde_alp_alp  ) / 2

                               T
     S = Sum{ [(LTilde_i )_alp] [(LTilde_i)_alp] }
          i                          
%$$
where $math%LTilde_alp_alp%$$ is an approximation for the second order 
derivatives of the population objective with respect to population 
parameter alp and $math%(LTilde_i)_alp%$$ is the first order derivative 
of individual i objective with respect to population parameter alp.  
Note that R is defined in this way to insure that it is symmetric
even for cases where the approximation $math%LTilde_alp_alp%$$ is not.
$pre

$$
The standard error vector and the correlation matrix are calculated from
the values of the covariance matrix by their mathematical definitions, 
respectively. The coefficient of variation is calculated as:
$math%
   
               CV = SE / b * 100 

%$$
where CV stands for the coefficient of variation, SE stands for the standard 
error and b stands for the value of the population parameter estimate.
The confidence interval is calculated from the values of the standard error 
of the population parameter estimate using its mathematical definition.
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
The $italic popPar$$ should be obtained by calling SPK function 
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
The $italic popObj_popPar_popPar$$ should be obtained by calling SPK function 
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
The $italic indParAll$$ should be obtained by calling SPK function 
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

/popParCovOut/ 
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
#include <nag.h>
#include <nagg05.h>
#include "SpkModel.h"
#include "lTilde.h"
#include "inverse.h"
#include "multiply.h"
#include "popStatistics.h"
#include "printInMatrix.h"
#include "fitPopulation.h"
#include "SpkValarray.h"

using namespace std;

class UserModelPopStatisticsExampleTest : public SpkModel
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

  const int nAlp = 2;

  const int nB = 1;

  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  UserModelPopStatisticsExampleTest model( nAlp, nB, nYi );


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
    eTrue = nag_random_normal( meanETrue, sdETrue );
    bTrue = nag_random_normal( meanBTrue, sdBTrue );

    Y[ i ] = meanBetaTrue + bTrue + eTrue;
  }


  //------------------------------------------------------------
  // Quantities related to the fixed population parameter, alp.
  //------------------------------------------------------------

  valarray<double> alpTrue( nAlp );
  valarray<double> alpLow ( nAlp );
  valarray<double> alpUp  ( nAlp );
  valarray<double> alpIn  ( nAlp );
  valarray<double> alpOut ( nAlp );
  valarray<double> alpStep( nAlp );

  // Set the values associated with alp(1).
  alpTrue[ 0 ] = meanBetaTrue;
  alpLow [ 0 ] = -10.0;
  alpUp  [ 0 ] = 10.0;
  alpIn  [ 0 ] = -1.0;
  alpStep[ 0 ] = 1.0e-2;

  // Set the values associated with alp(2).
  alpTrue[ 1 ] = varBetaTrue;
  alpLow [ 1 ] = 1.0e-3;
  alpUp  [ 1 ] = 100.0;
  alpIn  [ 1 ] = 0.5;
  alpStep[ 1 ] = 1.0e-2;
  

  //------------------------------------------------------------
  // Quantities related to the random population parameters, b.
  //------------------------------------------------------------

  valarray<double> bLow ( -1.5e+1, nB );
  valarray<double> bUp  ( +1.0e+1, nB );
  valarray<double> bStep(  1.0e-2, nB );

  valarray<double> bIn ( 1., nB * nInd );
  valarray<double> bOut(     nB * nInd );


  //------------------------------------------------------------
  // Quantities related to the population objective function.
  //------------------------------------------------------------

  double dLTildeOut;

  valarray<double> lTilde_alpOut    ( nAlp );
  valarray<double> lTilde_alp_alpOut( nAlp * nAlp );


  //------------------------------------------------------------
  // Remaining inputs to fitPopulation.
  //------------------------------------------------------------

  // Set the values associated with the individual objective function.
  Optimizer indOptimizer( 1.0e-6, 40, 0 );

  // Set the values associated with the population objective function.
  Optimizer popOptimizer( 1.0e-6, 40, 0 );

  // Set the parallel controls object
  DirBasedParallelControls parallelControls( false, 0, 0 );


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
                     alpLow,
                     alpUp,
                     alpIn,
                     alpStep,
                     &alpOut,
                     indOptimizer,
                     bLow,
                     bUp,
                     bIn,            
                     bStep,
                     &bOut,
                     &dLTildeOut,
                     &lTilde_alpOut,
                     &lTilde_alp_alpOut, 
                     parallelControls 
                   );
      ok = true;
  }
  catch(...)
  {
    cerr << "fitPopulation failed" << endl;
    return 0;
  }

  cout << "=======================" << endl;
  cout << "objective = MODIFIED_LAPLACE" << endl;
  cout << "LTilde = " << dLTildeOut << endl;
  cout << "popPar = " << endl;
  printInMatrix( alpOut, 1 );
  cout << "lTilde_alp_alpOut = " << endl;
  printInMatrix( lTilde_alp_alpOut, nAlp );
  cout << "-----------------------" << endl;

  //------------------------------------------------------------
  // Compute statistics of population parameter estimates.
  //------------------------------------------------------------
  valarray<double> popParCovOut( nAlp * nAlp );
  valarray<double> popParSEOut( nAlp );
  valarray<double> popParCorOut( nAlp * nAlp );
  valarray<double> popParCVOut( nAlp );
  valarray<double> popParCIOut( nAlp * 2 );

  try
  {
  for( int form = 1; form < 4; form++ )
  {
      popStatistics(
                     model,
                     whichObjective,
                     N,
                     Y,
                     alpOut,
                     lTilde_alp_alpOut,
                     bOut,
                     bLow,
                     bUp,
                     bStep,
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
LTilde = 21.8566
popPar =
[ 1.95115 ]
[ 3.63406 ]
lTilde_alp_alpOut =
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

#include <iostream>
#include <cmath>
#include "popStatistics.h"
#include "getCol.h"
#include "replaceJth.h"
#include "transpose.h"
#include "inverse.h"
#include "lTilde.h"
#include "multiply.h"
#include "add.h"
#include "SpkException.h"
#include "statistics.h"
#include "printInMatrix.h"

using SPK_VA::valarray;
using SPK_VA::slice;
using namespace std;

//                         T      
//     S = Sum{ gi_x * gi_x }
//          i                          
//
static const valarray<double> nmS( const valarray<double> & x, 
				   const valarray<double> & g_x )
{
  int nX = x.size();
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
static const valarray<double> nmInvR( const valarray<double> & x, 
				      const valarray<double> & h_x_x )
{
  int nX = x.size();
  int nF = h_x_x.size() / nX / nX;
  assert( h_x_x.size() == nX * nX * nF ); 
  valarray<double> tmp( 0.0, nF * nX * nX );
  return inverse( ( h_x_x + transpose( h_x_x, nX ) ) * 0.5, nX );
}



void popStatistics( SpkModel&                popModel,
                    enum Objective           objective,
                    const valarray<int>&     nMeasurementsAll,
                    const valarray<double>&  measurementsAll,
                    const valarray<double>&  popPar,
                    const valarray<double>&  popObj_popPar_popPar,
                    const valarray<double>&  indParAll,
                    const valarray<double>&  indParLow,
                    const valarray<double>&  indParUp,
                    const valarray<double>&  indParStep,
                    enum PopCovForm          formulation,
                    valarray<double>*        popParCovOut,
                    valarray<double>*        popParSEOut,                          
                    valarray<double>*        popParCorOut,
                    valarray<double>*        popParCVOut,                          
                    valarray<double>*        popParCIOut )
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
  
    // Return if FIRST_ORDER objective with formulation != R
    if( objective == FIRST_ORDER && formulation != R )
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
          sprintf( message, 
		   "The degree of freedom (#of measurements<%d> - #of fixed effects<%d>) must be positive.\n",
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
      sprintf( message, "The sum <%d> of the values contained in nMeasurementsAll vector must match the size <%d> of measurementsAll vector.\n",
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
      sprintf( message, "The length <%d> of indParLow vector must match the size <%d> of indPar vector.",
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
      sprintf( message, "The length <%d> of indParUp vector must match the size <%d> of indPar vector.",
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
    // This is a column vector.
    if ( popObj_popPar_popPar.size() != nAlp * nAlp )
    {
      char message[ SpkError::maxMessageLen() ];
      sprintf( message,
	       "Tilde_alp_alp vector that contains the second derivative of objective function \
must have n times n length, where n is the size of population parameter <%d>.  ", 
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
          sprintf( message, 
		   "The number of measurements must be greater than zero.  %d -th element, %d, is invalid.", 
		   i, nMeasurementsAll[i] );
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
            if( indParAll[ i + j * nB ] <= indParLow[i] || indParAll[ i + j * nB ] >= indParUp[i] )
            {
	      char message[ SpkError::maxMessageLen() ];
              sprintf( message, "The initial value for the individual parameter must \
be less than or equal to the upper bound value and \
greater than or equal to the lower boundary value.  \
message %d-the element, %f, is invalid.", i, indParAll[ i + j * nB ] );

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
    // Declare R inverse and S variables. 
    //----------------------------------------------------------------
    valarray<double> indObj_popPar( nAlp * nInd );
    valarray<double> popParCovTemp( nAlp * nAlp );
    //----------------------------------------------------------------
    // Compute RInv 
    //----------------------------------------------------------------
    if( formulation == R )
    {
      popParCovTemp = nmInvR( popPar, popObj_popPar_popPar );
    }
    //----------------------------------------------------------------
    // Compute S or RSR
    //----------------------------------------------------------------
    else //( formulation == RSR || formulation == S )
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
        DoubleMatrix dvecAlp( popPar );
        DoubleMatrix dvecBLow( indParLow );
        DoubleMatrix dvecBUp( indParUp );
        DoubleMatrix dmatBIn( indParAll, nInd );
        DoubleMatrix dvecBStep( indParStep );

         //---------------------------------------------------------------------------
        // Compute the derivative of individual level objectives with
	// respect to the population parameter.
	// 
	//                    /                                                   \
	//                    |          (1) |          (2) |      |          (n) | 
	// LambdaLTilde_alp = | Labmda_alp   | Lambda_alp,  | ...  | Lambda_alp   |
	//                    |              |              |      |              |
	//                    \                                                   /
	// where the superscript (i) indentifies the i-th individial and
	// n is the total number of individuals.
        //---------------------------------------------------------------------------
        
        // let lTilde routine simply calculate the objectives at the given alp and b.
	Optimizer optimizer( 1.0e-6, 0, 0 );
  
	lTilde( popModel, 
		objective, 
		dvecY, 
		dvecN,
		optimizer,
		dvecAlp,
		dvecBLow,
		dvecBUp,
		dvecBStep,
		dmatBIn,
		0,
		0, 
		0, 
		&dmatLambdaLTilde_alpOut );

	indObj_popPar = dmatLambdaLTilde_alpOut.toValarray();
	if( formulation == S )
	  {
	    popParCovTemp = inverse( nmS( popPar, indObj_popPar ), nAlp );
	  }
	else //( formulation == RSR )
	  {
	    valarray<double> RInv = nmInvR( popPar, popObj_popPar_popPar );
	    popParCovTemp = multiply( multiply( RInv, nAlp, nmS( popPar, indObj_popPar ), nAlp ), nAlp, RInv, nAlp );
	  }
    }

    statistics( popPar, popParCovTemp, nF, popParSEOut, popParCorOut, popParCVOut, popParCIOut );

    if( popParCovOut != 0 )
      *popParCovOut = popParCovTemp;
}
