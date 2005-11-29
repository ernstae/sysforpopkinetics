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
 * File: firstOrderOpt.cpp
 *
 *
 * Optimizes the parametric population objective functions using first order
 * approximation.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: firstOrderOpt
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*

$begin FirstOrderOpt$$

$spell
        throwExcepIfMaxIter
        struct
        Model model
	cbc
	cerr
	const
	cout
	covariance
	Covariances
	cmath
	dmatInd
	drow
	dvec
	dvecInd
	endl
	Fp
	ind
	Inv
	iomanip
	iostream
	Iter
	namespace
	Obj
	optimizer
	indOptimizer
	popOptimizer
	paramatric
	pd
	pdalp
	pdmat
	pdmatInd
	pdrow
	pdvec
	dvecInd
	Spk
	sqrt
	std
	stdout
	subvector
	var
	Varbl
	Vi
    bool
    Ri
    ind
    valarray
$$

$section Optimizing paramatric objective functions using first order approximation$$

$index firstOrderOpt$$
$cindex optimizing \the parametric population objective \functions \using first order approximation$$
$index first order Optimization$$
$index population $$

$table
$bold Prototype:$$ $cend
$syntax/void firstOrderOpt(
              SpkModel&               /model/,
              const DoubleMatrix&     /dvecN/,
              const DoubleMatrix&     /dvecY/,
              Optimizer&              /popOptimizer/,
              const DoubleMatrix&     /dvecPopLow/,
              const DoubleMatrix&     /dvecPopUp/,
              const DoubleMatrix&     /dvecPopIn/,
              DoubleMatrix*           /pdvecPopOut/,
              const DoubleMatrix&     /dvecPopStep/,
              Optimizer&              /indOptimizer/,
              const DoubleMatrix&     /dvecIndLow/,
              const DoubleMatrix&     /dvecIndUp/,
              const DoubleMatrix&     /dmatIndIn/,
              DoubleMatrix*           /pdmatIndOut/,
              const DoubleMatrix&     /dvecIndStep/,
              double*                 /pdLTildeOut/,
              DoubleMatrix*           /pdrowLTilde_popOut/,
              DoubleMatrix*           /pdmatLTilde_pop_popOut/
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
Minimizes the parametric population objective function using modified 
first order approximation of the model functions. It is assumed that
$pre 
$$
$math%
    f(alp, b) = f(alp, 0) + f_b(alp, 0) * b
    R(alp, b) = R(alp, 0)
%$$
$pre 
$$
First, an equivalent individual model is created from the user 
provided population model based on the first order approximation of 
the model functions with respect to the individual parameters.  
This equivalent individual model is then used together with the data 
measured from the population to minimize the first order objective 
function to determine the fixed population parameters and the objective 
as well as its first and second order derivatives with respect to the 
fixed population parameters.  Finally, using the obtained values of the 
fixed population parameters and the original population model, the
objective functions for each individual are minimized to determine the 
realized value of the random individual parameters on each individual.   

$head Return Value$$
Upon a successful completion, the function returns normally and
set the given output value place holders if it is able to 
obtain an acceptable estimate for $math%alpHat%$$, 
within a specified number of iterations. 
Acceptable sets of values must also be found for $math%bHat_i%$$ 
that are calculated using the estimate for $math%alpHat%$$.  
The case that too-many-iter occurred during 
the optimization process is not a successful completion. 
$pre

$$
Upon the entry, the $bold universal$$ floating-point error flag set is 
cleared.  Hence, any pending floating-point errors will be lost.
The universal floating-point error detection bits are set to
detect a certain set of errors (for details, see $xref/FpErrorChecker//FpErrorChecker/$$).
The detection bits are restored upon the exit.

If an error is detected or failure occurs during the evaluation, a SpkException object is
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/
/model/
/$$
This function expects $italic model$$ to be a function of
all three parameters: $math%alp%$$, $math%b%$$ and $math%i%$$.
Refer $xref/glossary/Model Functions Depend on i - alp - b/Model Functions Depend on i - alp - b/$$
for details.


$syntax/

/dvecN/
/$$
The $code DoubleMatrix$$ $italic dvecN$$ contains the column vector 
$math%N%$$.  
The $th i$$ element of $math%N%$$
specifies the number of elements of $math%y%$$ that
correspond to the $th i$$ individual.
Note that the length of $italic dvecN$$ specifies the number of 
individuals in the population, $math%M%$$.

$syntax/

/dvecY/
/$$
The $code DoubleMatrix$$ $italic dvecY$$ contains the column vector 
$math%y%$$, which specifies the data for all the individuals.
The vector $math%y%$$ has
$math%
    N(1) + N(2) + ... + N(M)
%$$
elements where $math%M%$$ is the number of rows in $math%N%$$.
The data vector corresponding to the first individual is
$math%
                                         T
    y_1 = [ y(1) , y(2) , ... , y(N(1)) ]
%$$
Elements $math%y(N(1) + 1)%$$ through $math%y(N(1) + N(2))%$$ 
correspond to the second individual and so on.
(Note that $math%y_1%$$ refers to the first subvector or $math%y%$$ while
$math%y(1)%$$ refers to the first element of the vector $math%y%$$.)
$syntax/

/popOptimizer/
/$$
This $xref/Optimizer//Optimizer/$$ object contains the information 
that controls the population level optimization process.
$pre

$$
It has attributes for holding the optimization state information 
that is required to perform a warm start, i.e., to start the
optimization process using a previous set of optimization state
information.
If a warm start is being performed, then before this function is 
called the optimization state information must be set.
This information may have been set during a previous call to this
function, or the information may be set directly using the
Optimizer class member function, setStateInfo().
Note that the upper and lower bounds for $math%alp%$$ must be the 
same as they were during the earlier call to this function.
$pre

$$
Most of the optimizer information is accessible directly via public
get functions, e.g., the value epsilon is returned by the Optimizer 
class function $code getEpsilon()$$.  
The following subsections specify how this function uses 
some of the elements of the Optimizer object that are accessed 
directly using get functions.

$subhead popOptimizer.optInfo.epsilon$$
This real number is used to specify the convergence criteria
for the optimizer.
It must be greater than $math%0.0%$$.
$pre

$$
A population parameter value $math%alpOut%$$ is accepted as an estimate for 
$math%alpHat%$$ if 
$math%
        abs( alpOut - alpHat ) \le epsilon ( alpUp - alpLow )  ,
%$$
where $math%abs%$$ is the element-by-element absolute value function
and $math%alpHat%$$ is a local minimizer of the population level 
objective function with respect to $math%alp%$$.
Since $math%alpHat%$$ is unknown, this function estimates the left hand
side of this inequality in a way that is a good approximation when 
the Hessian of the objective function is positive definite.
$pre

$$
Note that if $italic nMaxIter$$ is set to zero, then $math%alpIn%$$ is 
accepted as the estimate for $math%alpHat%$$.

$subhead popOptimizer.optInfo.nMaxIter$$
This integer must be greater than or equal to zero.
It specifies the maximum number of 
iterations to attempt before giving up on convergence.
If it is equal to zero, then the initial
value for $math%alp%$$ is accepted as the final value, and any requested output
values are evaluated at that final value.

$subhead popOptimizer.optInfo.traceLevel$$
This integer scalar specifies the amount of tracing.
Larger values of $italic traceLevel$$ entail more tracing, 
with $math%4%$$ being the highest level of tracing.
If $math%level \ge 1%$$, trace values are directed to standard output 
(stdout).  
$pre

$$
Tracing is done using a scaled version of the
objective function.  For this scaled version the elements of
the parameter vector are constrained to the interval $math%[0, 1]%$$. 
$pre

$$
If $italic traceLevel$$ is equal to $math%4%$$, then the tracing 
will include the gradient of the objective and a finite difference 
approximation for that gradient.
These two gradients can be compared as a check on the consistency 
of the objective function and its gradient.
$pre

$$
For more details on the tracing see the description of the level 
parameter for the optimizer $code QuasiNewton01Box$$.

$subhead popOptimizer.optInfo.nIterCompleted$$
This integer scalar holds the number of iteration that have been 
completed in the optimizer.

$subhead popOptimizer.optInfo.isTooManyIter$$
This flag indicates whether the too-many-iteration failure has occurred.  

$subhead popOptimizer.optInfo.saveStateAtEndOfOpt$$
This flag indicates if the state information required for a warm start
should be saved at the end of the optimization process.
This state information will not be saved if the optimization process
results in an exception being thrown by $code quasiNewtonAnyBox$$.

$subhead popOptimizer.optInfo.throwExcepIfMaxIter$$
This flag indicates if the optimizer should throw an exception when
the maximum number of iterations is exhausted.
If this parameter is true, then when
the maximum number of iterations is exhausted, an exception will
be thrown and the output values for this function will not be set.
Otherwise, the calling program will
need to check the parameter isTooManyIter to see if the 
maximum number of iterations was exhausted.

$subhead popOptimizer.optInfo.isWarmStartPossible$$
This flag indicates whether it is possible to perform a warm start 
using the current optimizer state information.

$subhead popOptimizer.optInfo.isWarmStart$$
This flag indicates whether the optimization should run a warm start.  

$subhead popOptimizer.optInfo.stateInfo$$
This $code StateInfo$$ struct contains the optimization state information
required to perform a warm start.
Each of its elements is accessed using the Optimizer class member
functions, $code getStateInfo()$$ and $code setStateInfo()$$.

$syntax/

/dvecPopLow/
/$$
The $code DoubleMatrix$$ $italic dvecPopLow$$ contains the column vector 
$math%popLow%$$, which specifies the lower limit for $math%pop%$$ during 
the optimization procedure.
The length of $italic dvecPopLow$$ is equal to the length of 
the fixed population parameter vector $math%pop%$$.

$syntax/

/dvecPopUp/
/$$
The $code DoubleMatrix$$ $italic dvecPopUp$$ contains the column vector 
$math%popUp%$$, which specifies the upper limit for $math%pop%$$ during 
the optimization procedure.
The length of $italic dvecPopUp$$ specifies the length of 
the fixed population parameter vector $math%pop%$$.

$syntax/

/dvecPopIn/
/$$
The $code DoubleMatrix$$ $italic dvecPopIn$$ contains the column vector 
$math%popIn%$$, which specifies the initial value for the fixed population 
parameters.
The $xref/glossary/Ordering Of Vectors/order condition/$$,
$math%popLow \le popIn \le popUp%$$, is assumed to hold.
Note that the length of $italic dvecPopIn$$ specifies the length of 
the fixed population parameter vector $math%pop%$$.

$syntax/

/pdvecPopOut/
/$$
If $italic pdvecPopOut$$ is not $code NULL$$, then the 
$code DoubleMatrix$$ pointed to by $italic pdvecPopOut$$ must 
be declared in the function that calls this function, and it 
must have the same dimensions as $italic dvecPopIn$$.
If $italic pdvecPopOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then the $code DoubleMatrix$$ pointed to by $italic pdvecPopOut$$ 
will contain the column vector $math%popOut%$$, which is the 
estimate for the true minimizer of the population objective function.
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdvecPopOut$$.

$syntax/

/dvecPopStep/
/$$
The $code DoubleMatrix$$ $italic dvecPopStep$$ contains the column vector 
$math%popStep%$$, which specifies the step size used for approximating
the derivatives with respect to the fixed population parameters.
The value of this parameter does not matter if
$italic pdmatLTilde_pop_popOut$$ is $code NULL$$.
The length of $italic dvecPopStep$$ is equal to the length of 
the fixed population parameter vector $math%pop%$$.

$syntax/

/indOptimizer/
/$$
This $xref/Optimizer//Optimizer/$$ object contains the information 
that controls the individual level optimization process.
$pre

$$
Note that warm starts are not supported for the individual 
level optimization.
$pre

$$
Most of the optimizer information is accessible directly via public
get functions, e.g., the value epsilon is returned by the Optimizer 
class function $code getEpsilon()$$.  
The following subsections specify how this function uses 
some of the elements of the Optimizer object that are accessed 
directly using get functions.

$subhead indOptimizer.optInfo.epsilon$$
This real number is used to specify the convergence criteria
for the optimizer.
It must be greater than $math%0.0%$$.
$pre

$$
For a particular value of $math%alp%$$ and for the $math%i%$$-th 
individual in the population, an individual parameter value 
$math%bOut_i%$$ is accepted as an estimate for $math%bHat_i%$$ if 
$math%
        abs( bOut_i - bHat_i ) \le epsilon ( bUp - bLow )  ,
%$$
where $math%abs%$$ is the element-by-element absolute value function
and $math%bHat_i%$$ is a local minimizer of the individual level 
objective function with respect to $math%b%$$.
Since $math%bHat_i%$$ is unknown, this function estimates the left hand
side of this inequality in a way that is a good approximation when 
the Hessian of the objective function is positive definite.
$pre

$$
Note that if $italic nMaxIter$$ is set to zero, then the $th i$$ 
column of $math%bIn%$$ is accepted as the estimate for 
$math%bHat_i%$$.

$subhead indOptimizer.optInfo.nMaxIter$$
This integer must be greater than or equal to zero.
It specifies the maximum number of 
iterations to attempt before giving up on convergence.
If it is equal to zero, then the initial
value for $math%b%$$ is accepted as the final value, and any requested output
values are evaluated at that final value.

$subhead indOptimizer.optInfo.traceLevel$$
This integer scalar specifies the amount of tracing.
Larger values of $italic traceLevel$$ entail more tracing, 
with $math%4%$$ being the highest level of tracing.
If $math%level \ge 1%$$, trace values are directed to standard output 
(stdout).  
$pre

$$
Tracing is done using a scaled version of the
objective function.  For this scaled version the elements of
the parameter vector are constrained to the interval $math%[0, 1]%$$. 
$pre

$$
If $italic traceLevel$$ is equal to $math%4%$$, then the tracing 
will include the gradient of the objective and a finite difference 
approximation for that gradient.
These two gradients can be compared as a check on the consistency 
of the objective function and its gradient.
$pre

$$
For more details on the tracing see the description of the level 
parameter for the optimizer $code QuasiNewton01Box$$.

$subhead indOptimizer.optInfo.nIterCompleted$$
This integer scalar holds the number of iteration that have been 
completed in the optimizer.

$subhead indOptimizer.optInfo.isTooManyIter$$
This flag indicates whether the too-many-iteration failure has occurred.  

$subhead indOptimizer.optInfo.saveStateAtEndOfOpt$$
This flag is not used for the individual level optimization.

$subhead indOptimizer.optInfo.throwExcepIfMaxIter$$
This flag is not used for the individual level optimization.

$subhead indOptimizer.optInfo.isWarmStartPossible$$
This flag is not used for the individual level optimization.

$subhead indOptimizer.optInfo.isWarmStart$$
This flag is not used for the individual level optimization.

$subhead indOptimizer.optInfo.stateInfo$$
This $code StateInfo$$ struct is not used for the individual 
level optimization.

$syntax/

/dvecIndLow/
/$$
The $code DoubleMatrix$$ $italic dvecIndLow$$ contains the column vector 
$math%bLow%$$, which specifies the lower limit for the random parameters 
during the optimization procedure for all the individuals.
The length of $italic dvecIndLow$$ is equal to the length of 
the individual parameter vector $math%ind%$$.

$syntax/

/dvecIndUp/
/$$
The $code DoubleMatrix$$ $italic dvecIndUp$$ contains the column vector 
$math%bUp%$$, which specifies the upper limit for the random parameters 
during the optimization procedure for all the individuals.
The length of $italic dvecIndUp$$ is equal to the length of 
the individual parameter vector $math%ind%$$.

$syntax/

/dmatIndIn/
/$$
The $code DoubleMatrix$$ $italic dmatIndIn$$ contains the matrix 
$math%bIn%$$.  
The $th i$$ column of $math%bIn%$$ specifies the initial value for 
the random parameters for the $th i$$ individual.
If $math%ind_i%$$ is any column of $math%bIn%$$,
it is assumed that $math%bLow \le ind_i \le bUp%$$.
The column dimension of $math%bIn%$$ is equal to the number of 
individuals in the population, $math%M%$$.
Note that the number of rows in $italic dmatIndIn$$ specifies the 
length of the individual parameter vector $math%ind%$$.

$syntax/

/pdmatIndOut/
/$$
If $italic pdmatIndOut$$ is not $code NULL$$, 
then the $code DoubleMatrix$$ pointed to by $italic pdmatIndOut$$ must 
be declared in the function that calls this function, 
and it must have the same dimensions as $math%bIn%$$.
If $italic pdmatIndOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then the $code DoubleMatrix$$ pointed to by $italic pdmatIndOut$$ will 
contain $math%bOut%$$, which is the matrix of estimates for the true 
minimizers of the individual objective functions.
Otherwise, this function will not attempt to change the contents of 
the $code DoubleMatrix$$ pointed to by $italic pdmatIndOut$$.
To be specific, the $th i$$ column of $math%bOut%$$ contains a column
vector that is an estimate for $math%bHat_i%$$, the minimizer 
of $math%Lambda_i(popOut, ind)%$$ with respect to $math%ind%$$. 
This is under the assumption that $math%popOut%$$
is the true value for the fixed population parameters.
The value $math%epsilon(1)%$$ is used for accepting the minimizers with 
respect to the individual parameters.

$syntax/

/dvecIndStep/
/$$
The $code DoubleMatrix$$ $italic dvecIndStep$$ contains the column vector 
$math%bStep%$$, which specifies the step size used for approximating
the derivatives with respect to the individual parameters.
The length of $italic dvecIndStep$$ is equal to the length of 
the individual parameter vector $math%ind%$$.

$syntax/

/pdLTildeOut/
/$$
If $italic pdLTildeOut$$ is not $code NULL$$, then the $code double$$ 
value pointed to by $italic pdLTildeOut$$ must be declared in the 
function that calls this function.
If $italic pdLTildeOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then the $code double$$ value pointed to by $italic pdLTildeOut$$ will 
be equal to $math%LTilde(popOut)%$$, which is the value of the population 
objective function evaluated at $math%popOut%$$.
Otherwise, this function will not attempt to change the contents of the 
$code double$$ value pointed to by $italic pdLTildeOut$$.

$syntax/

/pdrowLTilde_popOut/
/$$
If $italic pdrowLTilde_popOut$$ is not $code NULL$$, then the 
$code DoubleMatrix$$ pointed to by $italic pdrowLTilde_popOut$$ 
must be declared in the function that calls this function, and it 
must be a row vector that is the same length as
the fixed population parameter vector $math%pop%$$.
If $italic pdrowLTilde_popOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then the $code DoubleMatrix$$ pointed to by $italic pdrowLTilde_popOut$$ 
will contain the row vector $math%LTilde_pop(popOut)%$$, which is
the derivative of the population objective function evaluated at 
$math%popOut%$$.
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdrowLTilde_popOut$$.

$syntax/

/pdmatLTilde_pop_popOut/ 
/$$
If $italic pdmatLTilde_pop_popOut$$ is not $code NULL$$, then the 
$code DoubleMatrix$$ pointed to by $italic pdmatLTilde_pop_popOut$$ 
must be declared in the function that calls this function, and it 
must have the same number of rows and columns as the length of
the fixed population parameter vector $math%pop%$$.
If $italic pdmatLTilde_pop_popOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then the $code DoubleMatrix$$ pointed to by 
$italic pdmatLTilde_pop_popOut$$ will contain the matrix 
$math%LTilde_pop_pop(popOut)%$$, which is an approximation 
for the second derivative of the population objective function 
evaluated at $math%popOut%$$. 
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdmatLTilde_pop_popOut$$.
The approximation for the second derivative is formed using central
differences of the function $math%LTilde_pop(pop)%$$ with
step sizes specified by $italic dvecPopStep$$.

$syntax/

/dmatLambdaTilde_popOut/
/$$

If $italic dmatLambdaTilde_popOut$$ is not $code NULL$$, then the
$code DoubleMatrix$$ pointed to by $italic dmatLambdaTilde_popOut$$
must be declared in the function that calls this function, and its
number of columns must be equal to the number of individuals and its
number of rows must be equal to the length of the population parameter
vector $math%pop%$$.
If $italic dmatLambdaTilde_popOut$$ is not $code NULL$$, and if this
function completed the optimization successfully, then the $code
DoubleMatrix$$ pointed to by $italic dmatLambdaTilde_popOut$$ will
contain the derivatives of this individuals' contributions to
the population objective function.
Each column of the matrix contains the transpose of the derivative
 for a single individual.
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic dmatLambdaTilde_popOut$$.

$head Example$$
The following demonstrates running firstOrderOpt(). 
$codep

#include <iostream>
#include <iomanip>
#include <string>
#include <cmath>
#include "firstOrderOpt.h"
#include "namespace_population_analysis.h"
#include "identity.h"
#include "pi.h"
#include "SpkModel.h"
#include "File.h"
#include "Optimizer.h"
#include "randNormal.h"

using std::string;

static DoubleMatrix funF    ( const DoubleMatrix &alp, 
                            const DoubleMatrix &b );
static DoubleMatrix funF_alp( const DoubleMatrix &dvecF,   
                            const DoubleMatrix &alp, 
                            const DoubleMatrix &b );
static DoubleMatrix funF_b  ( const DoubleMatrix &dvecF, 
                            const DoubleMatrix &alp,   
                            const DoubleMatrix &b );
static DoubleMatrix funR    ( const DoubleMatrix &alp, 
                            const DoubleMatrix &b );
static DoubleMatrix funR_alp( const DoubleMatrix &dmatR,   
                            const DoubleMatrix &alp, 
                            const DoubleMatrix &b );
static DoubleMatrix funR_b  ( const DoubleMatrix &dmatR, 
                            const DoubleMatrix &alp,   
                            const DoubleMatrix &b );
static DoubleMatrix funD    ( const DoubleMatrix &alp );
static DoubleMatrix funD_alp( const DoubleMatrix &dmatD,   
                            const DoubleMatrix &alp );

class PopModel : public SpkModel
{
    DoubleMatrix _a, _b;
    int _i;
    const int _nAlp, _nB;
    const DoubleMatrix _N;

public:
    PopModel( int nAlp, int nB, const DoubleMatrix & dvecN )
    : _nAlp(nAlp), _nB(nB), _N(dvecN)
    {}
    ~PopModel(){}
protected:
    void doSelectIndividual(int i)
    {
        _i = i;
    }
    void doSetPopPar(const valarray<double>& alp)
    {
        _a = alp.toValarray();
    }
    void doSetIndPar(const valarray<double>& b)
    {
        _b = b.toValarray();
    }
    void doDataMean( valarray<double>& ret ) const
    {
        ret = funF(_a, _b).toValarray();
    }
    bool dataMean_popPar( valarray<double>& ret ) const
    {
        valarray<double> f;
        doDataMean(f);
        DoubleMatrix dvecF( f, 1 );

        ret = funF_alp(dvecF, _a, _b).toValarray();
        return !allZero(ret);
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        valarray<double> f;
        doDataMean(f);
        DoubleMatrix dvecF( f, 1 );

        ret = funF_b(dvecF, _a, _b).toValarray();
        return !allZero(ret);
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        ret = funR(_a, _b);
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        valarray<double> R;
        doDataVariance(R);
        DoubleMatrix dmatR( R, _N[ _i ] ) ;
        ret = funR_alp(dmatR, _a, _b).toValarray();
        return !allZero(ret);
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        valarray<double> R;
        doDataVariance(R);
        DoubleMatrix dmatR( R, _N[ _i ] ) ;

        ret = funR_b(dmatR, _a, _b).toValarray();
        return !allZero(ret);
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
        ret = funD(_a).toValarray();
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        doIndParVariance(D);
        DoubleMatrix dmatD( D, _nB );

        ret = funD_alp(dmatD, _a).toValarray();
        return !allZero(ret);
    }
};
//--------------------------------------------------------------
//
// Function: main
//
//--------------------------------------------------------------

void main()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;
  int i;


  //------------------------------------------------------------
  // Quantities that define the problem.
  //------------------------------------------------------------

  // Mean and variance of the true transfer rate, betaTrue.
  double meanBetaTrue = 1.0;
  double varBetaTrue  = 5.0;

  // Number of individuals.
  int nB = 10;


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  // Number of measurements.
  int nY = nB;

  // Measurement values, y.
  DoubleMatrix dvecY( nY, 1 );
  double* pdYData = dvecY.data();

  // Number of measurements for each individual. 
  DoubleMatrix dvecN( nB, 1 );
  dvecN.fill( (double) 1 );

  // These will hold the generated values for the true measurement 
  // noise, eTrue, and the true individual parameters, bTrue.
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
  for ( i = 0; i < nB; i++ )
  {
    eTrue = randNormal( meanETrue, sdETrue );
    bTrue = randNormal( meanBTrue, sdBTrue );

    pdYData[ i ] = meanBetaTrue + bTrue + eTrue;
  }


  //------------------------------------------------------------
  // Quantities related to the fixed population parameter, alp.
  //------------------------------------------------------------

  int nAlp = 2;

  DoubleMatrix alpTrue( nAlp, 1 );
  DoubleMatrix alpLow ( nAlp, 1 );
  DoubleMatrix alpUp  ( nAlp, 1 );
  DoubleMatrix alpIn  ( nAlp, 1 );
  DoubleMatrix alpOut ( nAlp, 1 );
  DoubleMatrix alpStep( nAlp, 1 );

  double* pdAlpTrueData = alpTrue.data();
  double* pdAlpLowData  = alpLow .data();
  double* pdAlpUpData   = alpUp  .data();
  double* pdAlpInData   = alpIn  .data();
  double* pdAlpOutData  = alpOut .data();
  double* pdAlpStepData = alpStep.data();

  // Set the values associated with alp(1).
  pdAlpTrueData[ 0 ] = meanBetaTrue;
  pdAlpLowData [ 0 ] = -10.0;
  pdAlpUpData  [ 0 ] = 10.0;
  pdAlpInData  [ 0 ] = -1.0;
  pdAlpStepData[ 0 ] = 1.0e-2;

  // Set the values associated with alp(2).
  pdAlpTrueData[ 1 ] = varBetaTrue;
  pdAlpLowData [ 1 ] = 1.0e-3;
  pdAlpUpData  [ 1 ] = 100.0;
  pdAlpInData  [ 1 ] = 0.5;
  pdAlpStepData[ 1 ] = 1.0e-2;
  

  //------------------------------------------------------------
  // Quantities related to the individual parameters, b.
  //------------------------------------------------------------

  int nB = 1;

  DoubleMatrix bLow ( nB, 1 );
  DoubleMatrix bUp  ( nB, 1 );
  DoubleMatrix bStep( nB, 1 );

  bLow .fill( -1.5e+1 );
  bUp  .fill( +1.0e+1 );
  bStep.fill(  1.0e-2 );

  DoubleMatrix dmatBIn ( nB, nB );
  DoubleMatrix dmatBOut( nB, nB );

  dmatBIn.fill( 1.0 );


  //------------------------------------------------------------
  // Quantities related to the population objective function.
  //------------------------------------------------------------

  double dLTildeOut;

  DoubleMatrix drowLTilde_alpOut    ( 1,    nAlp );
  DoubleMatrix dmatLTilde_alp_alpOut( nAlp, nAlp );


  //------------------------------------------------------------
  // Remaining inputs to firstOrderOpt.
  //------------------------------------------------------------

  Optimizer indOptimizer( 1.0e-6, 40, 0 );
  Optimizer popOptimizer( 1.0e-6, 40, 0 );


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  PopModel model( nAlp, nB, dvecN );

  try
  {
    firstOrderOpt(  
				  model,
				  dvecN,
				  dvecY,
				  popOptimizer,
				  alpLow,
				  alpUp,
				  alpIn,
				  &alpOut,
				  alpStep,
				  indOptimizer,
				  bLow,
				  bUp,
				  dmatBIn,
				  &dmatBOut,
				  bStep,
				  &dLTildeOut,
				  &drowLTilde_alpOut,
				  &dmatLTilde_alp_alpOut,
				  );
  }
  catch(...)
  {
    cerr << "firstOrderOpt failed" << endl;
    abort();
  }


  //------------------------------------------------------------
  // Print the results.
  //------------------------------------------------------------

  cout << endl;

  cout << "alpOut = " << endl;
  alpOut.print(); 
  cout << endl;

  cout << "bOut = " << endl;
  dmatBOut.print(); 
  cout << endl;

  cout << "LTildeOut   = " << dLTildeOut << endl;
  cout << endl;

  cout << "LTilde_alpOut  = " << endl;
  drowLTilde_alpOut.print();
  cout << endl;

  cout << "LTilde_alp_alpOut  = " << endl;
  dmatLTilde_alp_alpOut.print();
  cout << endl;
}


//--------------------------------------------------------------
//
// Function: funF
//
//
// Calculates
//
//                 /                   \ 
//     f(alp, b) = |  alp(1) + b(1)  |  .
//                 \                  / 
//
//--------------------------------------------------------------

static DoubleMatrix funF( const DoubleMatrix &alp, 
                                          const DoubleMatrix &b )
{
  DoubleMatrix dvecF( 1, 1 );

  double* pdAlpData = alp.data();
  double* pdBData   = b  .data();

  dvecF.fill( pdAlpData[ 0 ] + pdBData[ 0 ] );

  return dvecF;
}


//--------------------------------------------------------------
//
// Function: funF_alp
//
//
// Calculates
//
//                     /           \ 
//     f_alp(alp, b) = |  1     0  |  .
//                     \           / 
//
//--------------------------------------------------------------

static DoubleMatrix funF_alp( const DoubleMatrix &dvecF, 
                                              const DoubleMatrix &alp, 
                                              const DoubleMatrix &b )
{
  DoubleMatrix drowF_alp( 1, 2 );

  double* pdF_alpData = drowF_alp.data();

  pdF_alpData[ 0 ] = 1.0;
  pdF_alpData[ 1 ] = 0.0;

  return drowF_alp;
}


//--------------------------------------------------------------
//
// Function: funF_b
//
//
// Calculates
//
//                   /     \ 
//     f_b(alp, b) = |  1  |  .
//                   \     / 
//
//--------------------------------------------------------------

static DoubleMatrix funF_b( const DoubleMatrix &dvecF, 
                                            const DoubleMatrix &alp, 
                                            const DoubleMatrix &b )
{
  return identity( 1 );
}


//--------------------------------------------------------------
//
// Function: funR
//
//
// Calculates
//
//                 /     \ 
//     R(alp, b) = |  1  |  .
//                 \     / 
//
//--------------------------------------------------------------

static DoubleMatrix funR( const DoubleMatrix &alp, 
                                          const DoubleMatrix &b )
{
  return identity( 1 );
}


//--------------------------------------------------------------
//
// Function: funR_alp
//
//
// Calculates
//
//                     /           \ 
//     R_alp(alp, b) = |  0     0  |  .
//                     \           / 
//
//--------------------------------------------------------------

static DoubleMatrix funR_alp( const DoubleMatrix &dmatR,   
                                              const DoubleMatrix &alp, 
                                              const DoubleMatrix &b )
{
  DoubleMatrix dmatR_alp( 1, 2 );

  dmatR_alp.fill(0.0);

  return dmatR_alp;
}


//--------------------------------------------------------------
//
// Function: funR_b
//
//
// Calculates
//
//                   /     \ 
//     R_b(alp, b) = |  0  |  .
//                   \     / 
//
//--------------------------------------------------------------

static DoubleMatrix funR_b( const DoubleMatrix &dmatR, 
                                            const DoubleMatrix &alp,   
                                            const DoubleMatrix &b )
{
  DoubleMatrix dmatR_b( 1, 1 );

  dmatR_b.fill(0.0);

  return dmatR_b;
}


//--------------------------------------------------------------
//
// Function: funD
//
//
// Calculates
//
//              /          \ 
//     D(alp) = |  alp(2)  |  .
//              \          / 
//
//--------------------------------------------------------------

static DoubleMatrix funD( const DoubleMatrix &alp )
{
  DoubleMatrix dmatD( 1, 1 );

  double* pdalpData = alp.data();

  dmatD.fill( pdalpData[ 1 ] );

  return dmatD;
}


//--------------------------------------------------------------
//
// Function: funD_alp
//
//
// Calculates
//
//                  /           \ 
//     D_alp(alp) = |  0     1  |  .
//                  \           / 
//
//--------------------------------------------------------------

static DoubleMatrix funD_alp( const DoubleMatrix &dmatD,
                                              const DoubleMatrix &alp )
{
  DoubleMatrix dmatD_alp( 1, 2 );

  double* pdD_alpData = dmatD_alp.data();

  pdD_alpData[ 0 ] = 0.0;
  pdD_alpData[ 1 ] = 1.0;

  return dmatD_alp;
}


$$
then it will display the following when it is run:
$codep

ok = True

alpOut =
[1.95115]
[3.63406]

bOut =
[1.94171, 0.446611, -0.312347, -0.938796, -3.12919, 2.01348, 2.47441, -1.48642, -1.16138, 0.151919]

LTildeOut   = 21.8566

LTilde_alpOut  =
[-2.767e-007, 1.50158e-007]

LTilde_alp_alpOut  =
[2.15793, 5.97158e-008]
[5.97208e-008, 0.232837]

$$
  
$end
*/

/*------------------------------------------------------------------------
 *
 * Implementation Notes
 * --------------------
 *
 * None.
 *
 *------------------------------------------------------------------------*/
#pragma warning( disable : 4006 )  
#pragma warning( disable : 4786 )  

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <cassert>
#include <iostream>
#include <cerrno>

#include "EqIndModel.h"
#include "SpkModel.h"
#include "mapOpt.h"
#include "mapObj.h"
#include "replaceJth.h"
#include "getSubblock.h"
#include "getCol.h"
#include "transpose.h"
#include "isDmatEpsEqual.h"
#include "File.h"
#include "SpkException.h"
#include "System.h"
#include "Objective.h"

using namespace std;

/*------------------------------------------------------------------------
 * Local class definitions
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  //**********************************************************************
  //
  // Class: IndIndexFixedModel
  //
  //
  // This class behaves the same as the class passed in when it is
  // constructed except that it does not allow the individual index to
  // be changed from the value set at construction time.
  //
  //**********************************************************************

  class IndIndexFixedModel : public SpkModel
  {
    //----------------------------------------------------------
    // Class members.
    //----------------------------------------------------------

  private:
    SpkModel* pModel;


    //----------------------------------------------------------
    // Constructors.
    //----------------------------------------------------------

  public:
    // This constructor sets the individual index in the model passed
    // in to a value that will not be changed.
    IndIndexFixedModel( SpkModel* pModelIn, int iFixed )
      :
      pModel ( pModelIn )
    {
      pModel->selectIndividual( iFixed );
    }


    //----------------------------------------------------------
    // Functions that do nothing.
    //----------------------------------------------------------

    // This function does nothing because it is not allowed to change
    // the individual index from the value set at construction time.
    void doSelectIndividual( int iIn )
    {
      assert ( iIn == 0 );
    }


    //----------------------------------------------------------
    // State changing functions that just call the contained model.
    //----------------------------------------------------------

    void doSetPopPar( const valarray<double>& popParIn )
    {
      pModel->setPopPar( popParIn );
    }

    void doSetIndPar( const valarray<double>& indParIn )
    {
      pModel->setIndPar( indParIn );
    }


    //----------------------------------------------------------
    // Model evaluation functions that just call the contained model.
    //----------------------------------------------------------

    void doDataMean( valarray<double>& ret ) const
    {
      pModel->dataMean( ret );
    }

    bool doDataMean_indPar( valarray<double>& ret ) const
    {
      return pModel->dataMean_indPar( ret );
    }

    bool doDataMean_popPar( valarray<double>& ret ) const
    {
      return pModel->dataMean_popPar( ret );
    }

    void doDataVariance( valarray<double>& ret ) const
    {
      pModel->dataVariance( ret );
    }

    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
      return pModel->dataVariance_indPar( ret );
    }

    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
      return pModel->dataVariance_popPar( ret );
    }

    void doDataVarianceInv( valarray<double>& ret ) const
    {
      pModel->dataVarianceInv( ret );
    }

    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
      return pModel->dataVarianceInv_indPar( ret );
    }

    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
      return pModel->dataVarianceInv_popPar( ret );
    }

    void doIndParVariance( valarray<double>& ret ) const
    {
      pModel->indParVariance( ret );
    }

    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
      return pModel->indParVariance_popPar( ret );
    }

    void doIndParVarianceInv( valarray<double>& ret ) const
    {
      pModel->indParVarianceInv( ret );
    }

    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
      return pModel->indParVarianceInv_popPar( ret );
    }

  };

} // [End: unnamed namespace]

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
void firstOrderOpt( 
                    SpkModel&               model,
                    const DoubleMatrix&     dvecN,
                    const DoubleMatrix&     dvecY,
                    Optimizer&              popOptimizer,
                    const DoubleMatrix&     dvecAlpLow,
                    const DoubleMatrix&     dvecAlpUp,
                    const DoubleMatrix&     dvecAlpIn,
                    DoubleMatrix*           pdvecAlpOut,
                    const DoubleMatrix&     dvecAlpStep,
                    Optimizer&              indOptimizer,
                    const DoubleMatrix&     dvecBLow,
                    const DoubleMatrix&     dvecBUp,
                    const DoubleMatrix&     dmatBIn,
                    DoubleMatrix*           pdmatBOut,
                    const DoubleMatrix&     dvecBStep,
                    double*                 pdLTildeOut,
                    DoubleMatrix*           pdrowLTilde_alpOut,
                    DoubleMatrix*           pdmatLTilde_alp_alpOut,
                    DoubleMatrix*           pdmatLambdaTilde_alpOut
                  )
{  
    //------------------------------------------------------------
    // Preliminaries.
    //------------------------------------------------------------
    // Return if there are no output values to compute.
    if( pdvecAlpOut             == 0 && 
        pdmatBOut               == 0 && 
        pdLTildeOut             == 0 && 
        pdrowLTilde_alpOut      == 0 && 
        pdmatLTilde_alp_alpOut  == 0 && 
        pdmatLambdaTilde_alpOut == 0    )
	{
        return;
	}

    int nInd = dvecN    .nr();
    int nAlp = dvecAlpIn.nr();
    int nB   = dmatBIn  .nr();

	const double* pdNData        = dvecN       .data();
    const double* pdAlpLowData   = dvecAlpLow  .data();
    const double* pdAlpUpData    = dvecAlpUp   .data();
    const double* pdAlpInData    = dvecAlpIn   .data();
    const double* pdBLowData     = dvecBLow    .data();
    const double* pdBUpData      = dvecBUp     .data();
    const double* pdBInData      = dmatBIn     .data();

  //------------------------------------------------------------
  // Set indOptimizer as a sub-level optimizer. 
  //------------------------------------------------------------

  bool oldIndSaveState  = indOptimizer.getSaveStateAtEndOfOpt();
  bool oldIndThrowExcep = indOptimizer.getThrowExcepIfMaxIter();

  // Set these flags so that an exception is thrown if the maximum number
  // of iterations is exceeded when optimizing an individual and so that
  // no individual level optimizer state information is saved.
  indOptimizer.setSaveStateAtEndOfOpt( false );
  indOptimizer.setThrowExcepIfMaxIter( true);


  //------------------------------------------------------------
  // Prepare the objects to hold the output values.
  //------------------------------------------------------------

  // Instantiate a temporary column vector to hold the final alp 
  // value that will be returned by mapOpt.
  DoubleMatrix dvecAlpOutTemp( nAlp, 1 );
  DoubleMatrix* pdvecAlpOutTemp = &dvecAlpOutTemp;

  // Instantiate a temporary matrix to hold the optimal b values
  // for each individual.
  DoubleMatrix dmatBOutTemp;
  DoubleMatrix* pdmatBOutTemp = 0;
  if ( pdmatBOut )
  {
	  dmatBOutTemp.resize( nB, nInd );
      pdmatBOutTemp = &dmatBOutTemp;
  }

  // If this function is going to return the population objective  
  // function value, initialize the temporary value to hold it.  
  // Otherwise, set the temporary pointer to zero so that mapOpt 
  // will not return it either.
  double dLTildeOutTemp;
  double* pdLTildeOutTemp = &dLTildeOutTemp;
  if ( pdLTildeOut )
  {
    dLTildeOutTemp = 0.0;
  }
  else
  {
    pdLTildeOutTemp = 0;
  }

  // If this function is going to return the derivative of the
  // population objective function with respect to alp, or if it is
  // going to return the derivatives with respect to alp of the
  // individuals' contributions to the population objective function,
  // instantiate a temporary row vector to hold it.  Otherwise, set
  // the temporary pointer to zero so that mapOpt will not return it
  // either.
  DoubleMatrix drowLTilde_alpOutTemp;
  DoubleMatrix* pdrowLTilde_alpOutTemp = &drowLTilde_alpOutTemp;
  if ( pdrowLTilde_alpOut || pdmatLambdaTilde_alpOut )
  {
    drowLTilde_alpOutTemp.resize( 1, nAlp );
  }
  else
  {
    pdrowLTilde_alpOutTemp = 0;
  }
  
  // If this function is going to return the second derivative of 
  // the population objective function with respect to alp, instantiate 
  // a temporary matrix to hold it.  Otherwise, set the temporary 
  // pointer to zero so that mapOpt will not return it either.
  DoubleMatrix dmatLTilde_alp_alpOutTemp;
  DoubleMatrix* pdmatLTilde_alp_alpOutTemp = &dmatLTilde_alp_alpOutTemp;
  if ( pdmatLTilde_alp_alpOut )
  {
    dmatLTilde_alp_alpOutTemp.resize( nAlp, nAlp );
  }
  else
  {
    pdmatLTilde_alp_alpOutTemp = 0;
  }

  // If this function is going to return the derivatives with respect
  // to alp of the individuals' contributions to the population
  // objective function, instantiate a temporary row vector to hold
  // it.  Otherwise, set the temporary pointer to zero so that mapOpt
  // will not return it either.
  DoubleMatrix dmatLambdaTilde_alpOutTemp;
  DoubleMatrix* pdmatLambdaTilde_alpOutTemp = &dmatLambdaTilde_alpOutTemp;
  if ( pdmatLambdaTilde_alpOut )
  {
    dmatLambdaTilde_alpOutTemp.resize( nAlp, nInd );
  }
  else
  {
    pdmatLambdaTilde_alpOutTemp = 0;
  }
  
  //----------------------------------------------------------------
  // Construct an equivalent individual model from population model.
  //----------------------------------------------------------------

  valarray<double> bStep = dvecBStep.toValarray();
  valarray<int> N(nInd);
  const double * pN = dvecN.data();
  int i;
  for( i=0; i<nInd; i++ )
  {
    N[i] = (int)pN[i];
  }

  EqIndModel eqIndModel( &model, N, bStep, nAlp );

  //----------------------------------------------------------------
  // Optimize the equivalent objective function to determine Alp.
  //----------------------------------------------------------------

  try
  {
	  mapOpt( 
			  eqIndModel,
			  dvecY, 
			  popOptimizer, 
			  dvecAlpLow, 
			  dvecAlpUp, 
			  dvecAlpIn,
			  pdvecAlpOutTemp, 
			  dvecAlpStep, 
			  pdLTildeOutTemp, 
			  pdrowLTilde_alpOutTemp, 
			  pdmatLTilde_alp_alpOutTemp,
			  false,
			  true,
			  &dvecN
			);
  }
  catch( SpkException& e )
  {
    throw e.push(
      SpkError::SPK_OPT_ERR,
      "Population level optimization failed. This message overrides the last message.", 
      __LINE__, __FILE__);
  }
  catch( const std::exception& e )
  {
    throw SpkException(e,
      "A standard exception was thrown during the population level optimization.", 
      __LINE__, __FILE__);
  }
  catch( ... )
  {
    throw SpkException(SpkError::SPK_UNKNOWN_OPT_ERR,
      "An unknown exception was thrown during the population level optimization.", 
      __LINE__, __FILE__);
  }

  //----------------------------------------------------------------
  // Evaluate the derivatives of each individual's contribution.
  //----------------------------------------------------------------

  valarray<int> nYi( 1 );
  DoubleMatrix dvecYi;
  DoubleMatrix dvecNi;
  double* pdNull = 0;
  int startRow;

  if( pdmatLambdaTilde_alpOut && !popOptimizer.getIsTooManyIter() )
  {
    try
    {
      // In order to get the derivatives of each individual's
      // contribution to the population objective function, an
      // equivalent individual model that contains only a single
      // individual will be created for each individual.
      startRow = 0;
      for ( i = 0; i < nInd; i++ )
      {
        // Get this individual's data.
        dvecNi   =  getSubblock( dvecN, i,        0, 1,    1 );
        dvecYi   =  getSubblock( dvecY, startRow, 0, N[i], 1 );
        nYi[0]   =  N[i];
        startRow += N[i];
      
        // Construct a population model with its individual index fixed to
        // that of the current individual.
        IndIndexFixedModel indIndexFixedModel( &model, i );
      
        // Construct an equivalent individual model that only includes the
        // current individual.
        EqIndModel oneIndEqIndModel( &indIndexFixedModel, nYi, bStep, nAlp );
        
        // Evaluate the derivative of this individual's contribution to
        // the population objective function.
        mapObj(
                oneIndEqIndModel,
                dvecYi,
                dvecAlpOutTemp,
                pdNull,
                pdrowLTilde_alpOutTemp,
                false,
                true,
                &dvecNi
              );

        // Save the derivative of this individual's contribution to
        // the population objective function.
        replaceJth(
                    dmatLambdaTilde_alpOutTemp,
                    i,
                    transpose( drowLTilde_alpOutTemp )
                   );
      }
    }
    catch( SpkException& e )
    {
      throw e.push(
        SpkError::SPK_OPT_ERR,
        "Evaluation of the derivatives of the individuals' objective contributions failed.", 
        __LINE__, __FILE__);
    }
    catch( const std::exception& e )
    {
      throw SpkException(e,
        "A standard exception was thrown during the evaluation of the derivatives of \nthe individuals' objective contributions.", 
        __LINE__, __FILE__);
    }
    catch( ... )
    {
      throw SpkException(SpkError::SPK_UNKNOWN_OPT_ERR,
        "An unknown exception was thrown during the evaluation of the derivatives of \nthe individuals' objective contributions.",
        __LINE__, __FILE__);
    }
  }

  //----------------------------------------------------------------
  // Optimize the individual objective function to determine b.
  //---------------------------------------------------------------- 

  if( pdmatBOut && !popOptimizer.getIsTooManyIter() )
  {
	  DoubleMatrix dvecBIn( nB, 1 );
	  DoubleMatrix dvecBOut( nB, 1 );

	  model.setPopPar( pdvecAlpOutTemp->toValarray() );
	  startRow = 0;
	  for( int i = 0; i < nInd; i++ )
	  {
		  model.selectIndividual( i );
		  dvecYi = getSubblock( dvecY, startRow, 0, (int)pdNData[ i ], 1 );
		  dvecBIn = getCol( dmatBIn, i );
		  try
		  {
			  mapOpt( 
					  model,
					  dvecYi, 
					  indOptimizer,
					  dvecBLow, 
					  dvecBUp, 
					  dvecBIn,
					  &dvecBOut, 
					  dvecBStep, 
					  0, 
					  0, 
					  0,
					  true 
					 );
		  }
		  catch( SpkException& e )
		  {
			throw e.push(
			  SpkError::SPK_OPT_ERR,
			  "Individual level optimization failed.", 
			  __LINE__, __FILE__);
		  }
		  catch( const std::exception& e )
		  {
			throw SpkException(e,
			  "A standard exception was thrown during the individual level optimization.", 
			  __LINE__, __FILE__);
		  }
		  catch( ... )
		  {
		  	throw SpkException(SpkError::SPK_UNKNOWN_OPT_ERR,
			  "An unknown exception was thrown during the individual level optimization.", 
			  __LINE__, __FILE__);
		  }

		  replaceJth( dmatBOutTemp, i, dvecBOut );
		  startRow += (int)pdNData[ i ];
	  }
  }

  //------------------------------------------------------------
  // Set the values to be returned.
  //------------------------------------------------------------

  // Set the final alp value, if necessary.
  if ( pdvecAlpOut && !popOptimizer.getIsTooManyIter() )
  {
        *pdvecAlpOut = dvecAlpOutTemp;
  }

  // Set the matrix of final b values, if necessary.
  if ( pdmatBOut && !popOptimizer.getIsTooManyIter() )
  {
    *pdmatBOut = dmatBOutTemp;
  }

  // Set the final population objective function value, if necessary.
  if ( pdLTildeOut && !popOptimizer.getIsTooManyIter() )
  {
        *pdLTildeOut = dLTildeOutTemp;
  }

  // Set the first derivative of the population objective 
  // function at the final alp value, if necessary.
  if ( pdrowLTilde_alpOut && !popOptimizer.getIsTooManyIter() )
  {
    *pdrowLTilde_alpOut = drowLTilde_alpOutTemp;
  }
    
  // Set the second derivative of the population objective 
  // function at the final alp value, if necessary.
  if ( pdmatLTilde_alp_alpOut && !popOptimizer.getIsTooManyIter() )
  {
    *pdmatLTilde_alp_alpOut = dmatLTilde_alp_alpOutTemp;
  }

  // Set the derivatives of the individuals' contributions to
  // the population objective function.
  if ( pdmatLambdaTilde_alpOut && !popOptimizer.getIsTooManyIter() )
  {
    *pdmatLambdaTilde_alpOut = dmatLambdaTilde_alpOutTemp;
  }
    
  // Reset these individual optimizer flags to their original values.
  indOptimizer.setSaveStateAtEndOfOpt( oldIndSaveState );
  indOptimizer.setThrowExcepIfMaxIter( oldIndThrowExcep );
}
