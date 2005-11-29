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
 * File: ppkaOpt.cpp
 *
 *
 * Optimizes the parametric population objective functions.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: ppkaOpt
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*

$begin ppkaOpt$$

$spell
  throwExcepIfMaxIter
  struct
  Model model
  bool
  cbc
  cmath
  const
  cout
  covariances
  dmat
  drow
  dvec
  endl
  eps
  epsilon
  Hessian
  Ind
  int
  iostream
  iomanip
  Iter
  Laplace
  Max
  namespace
  Obj
  ok
  pd
  pdmat
  pdrow
  pdvec
  ppka
  th
  sd
  sqrt
  std
  stdout
  str
  subvector
  var
  Varbl
  Vi
  ppkaoptexample 
  int 
  pf 
  pb
  Spk   
  cerr
  covariance
  inv
  optimizer
  indOptimizer
  popOptimizer
  valarray
  enum
  enumulator
  Objective objective
  Fo
$$

$section Optimizing the Parametric Population Objective Functions$$

$index ppkaOpt$$
$cindex optimizing \the parametric population objective \functions$$

$table
$bold Prototype:$$ $cend
$syntax/void ppkaOpt( SpkModel&       /model/,
              enum Objective&         /objective/,
              const DoubleMatrix&     /dvecN/,
              const DoubleMatrix&     /dvecY/,
              Optimizer&              /popOptimizer/,
              const DoubleMatrix&     /dvecAlpLow/,
              const DoubleMatrix&     /dvecAlpUp/,
              const DoubleMatrix&     /dvecAlpIn/,
              DoubleMatrix*           /pdvecAlpOut/,
              const DoubleMatrix&     /dvecAlpStep/,
              Optimizer&              /indOptimizer/,
              const DoubleMatrix&     /dvecBLow/,
              const DoubleMatrix&     /dvecBUp/,
              const DoubleMatrix&     /dmatBIn/,
              DoubleMatrix*           /pdmatBOut/,
              const DoubleMatrix&     /dvecBStep/,
              double*                 /pdLTildeOut/,
              DoubleMatrix*           /pdrowLTilde_alpOut/,
              DoubleMatrix*           /pdmatLTilde_alp_alpOut/ )
/$$

$tend

See also: $xref/fitPopulation//fitPopulation/$$
$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Minimizes one of the parametric population objective functions:  
the modified Laplace, the modified Expected Hessian, or the 
modified First Order.
$pre

$$
To be specific, this function solves the problem 
$math%

    \minimize LTilde(alp) \with \respect \to alp
    \subject \to alpLow \le alp \le alpUp  ,

%$$
where $math%LTilde(alp)%$$ is one of the above 
objective functions.
Each of these objectives is the negative log of a different 
approximation for the likelihood of all of the individuals' data.
$pre

$$
The parametric population objective function for a population 
of $math%M%$$ individuals is defined as 
$math%
                    M
                   ----
    LTilde(alp) =  >      LTilde_i(alp)  ,
                   ----
                   i = 1
%$$
where the negative log-likelihood of the $th i$$ individual's 
data $math%y_i%$$ is defined as
$math%

                    1 %                                  %
    LTilde_i(alp) = - \logdet[ HTilde_i(alp, bTilde_i) / (2\pi) ] + Lambda_i(alp, bHat_i) ,
                    2 %                                  %  

%$$
the joint negative log-likelihood of $math%y_i%$$ and 
the random population parameter vector $math%b%$$ is defined as
$math%

                       1 %          %                   1                    T            -1
    Lambda_i(alp, b) = - \logdet[ 2 \pi R_i(alp, b) ] + - [y_i - f_i(alp, b)]  R_i(alp, b)   [y_i - f_i(alp, b)]
                       2 %          %                   2

                       1 %          %                   1  T       -1
                     + - \logdet[ 2 \pi D(alp) ]      + - b  D(alp)   b  ,
                       2 %          %                   2

%$$
and the form of $math%HTilde_i(alp, b)%$$ depends on the 
particular parametric population objective function.
$pre

$$
The random population parameter vector $math%bHat_i%$$ is the true 
minimizer of $math%Lambda_i(alp, b)%$$ with respect to $math%b%$$. 
The random population parameter vector $math%bTilde_i%$$, on the other
hand, is the point where the approximate projected 
gradient of $math%Lambda_i(alp, b)%$$ with respect to $math%b%$$ is zero. 
The approximate projected gradient replaces the gradient by its 
central difference approximate in the definition of the projected
gradient.
$pre

$$
For the case of the modified Laplace objective function,
$math%HTilde_i(alp, b)%$$ is the finite difference approximation 
for the Hessian of $math%Lambda_i(alp, b)%$$ with respect 
to $math%b%$$.
$pre

$$
For the case of the modified Expected Hessian objective function,
$math%HTilde_i(alp, b)%$$ is the expected value of the Hessian of 
$math%Lambda_i(alp, b)%$$ with respect to $math%b%$$, where  
the derivatives with respect to $math%b%$$ are replaced 
by their finite difference approximations.
$pre

$$
(The above equations use
$xref/glossary/Population Notation/population notation/$$.)

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Model Assumptions$$
The following model assumptions are stated using 
$xref/glossary/Population Notation/population notation/$$.
The bar above $math%alp%$$ and $math%b_i%$$ denote the true, but unknown,
values for the fixed population parameters and the random population parameters
for the $th i$$ individual, respectively.
$math%
               ___   ___
    y_i = f_i( alp , b_i ) + e_i
                    ___  ___
    e_i ~ N[0, R_i( alp, b_i)]
    ___          ___
    b_i ~ N[0, D(alp)]
%$$

$head Return Value$$
Upon a successful completion, the function returns normally and
set the given output value place holders if it is able to 
obtain an acceptable estimate for $math%alpHat%$$, the true minimizer
of $math%LTilde(alp)%$$, within a specified number of iterations. 
In order for an acceptable estimate for $math%alpHat%$$ to be 
found, acceptable sets of values must also be found for $math%bHat_i%$$ 
and $math%bTilde_i%$$ that are calculated using the estimate for 
$math%alpHat%$$.  The case that too-many-iter occurred during 
the optimization process is not a successful completion. 
If a failure occurs during the evaluation, a SpkException object is
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Note$$
The number of individuals in the population, $math%M%$$, is
specified by the number of rows in the argument $italic dvecN$$,
which is a column vector containing the number of data elements 
for each individual.
$pre

$$
The length of the fixed population parameter vector $math%alp%$$ is 
specified by the number of rows in the argument $italic dvecAlpIn$$, 
which is a column vector containing the initial value for $math%alp%$$.
$pre

$$
The length of the random population parameter vector $math%b%$$ is 
specified by the number of rows in the argument $italic dmatBIn$$, 
which is a matrix containing the initial values for $math%b%$$ for
each of the individuals in the population.

$head Arguments$$
$syntax/
/model/
/$$
This function expects $italic model$$ to be a function of
all three parameters: $math%alp%$$, $math%b%$$ and $math%i%$$.
Refer $xref/glossary/Model Functions Depend on i - alp - b/Model Functions Depend on i - alp - b/$$
for details.
$pre

$$
When $italic whichObjective$$ is specified $code NAIVE_FIRST_ORDER$$, $italic model$$ must
be an object of $xref/NaiveFoModel//NaiveFoModel/$$ which is a subclass of $xref/SpkModel//SpkModel/$$.


$syntax/

/objective/
/$$
This enumulator specifies which parametric population objective 
function will be minimized:  the modified Laplace, the modified 
Expected Hessian, or the modified First Order.
The allowed values for $italic objective$$ are defined in 
the header of $xref/Objective//Objective/$$.

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
$comment 
[Revisit - Maximum number of iterations - Brad]
We should still define all the return values when the maximim number
of iterations is exhausted without converging. In this case, ppkopt
(and mapOpt) would need to return true. One way to do this is to
have a separate return value that is the number of iterations 
required for convergence (in both ppkOpt and mapOpt). 
This could be set to the maximum number plus
one when convergence cannot be achieved with in the required number.
$$
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
and $math%alpHat%$$ is a local minimizer of the parametric population 
objective function.
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

/dvecAlpLow/
/$$
The $code DoubleMatrix$$ $italic dvecAlpLow$$ contains the column vector 
$math%alpLow%$$, which specifies the lower limit for $math%alp%$$ during 
the optimization procedure.
The length of $italic dvecAlpLow$$ is equal to the length of 
the fixed population parameter vector $math%alp%$$.

$syntax/

/dvecAlpUp/
/$$
The $code DoubleMatrix$$ $italic dvecAlpUp$$ contains the column vector 
$math%alpUp%$$, which specifies the upper limit for $math%alp%$$ during 
the optimization procedure.
The length of $italic dvecAlpUp$$ specifies the length of 
the fixed population parameter vector $math%alp%$$.

$syntax/

/dvecAlpIn/
/$$
The $code DoubleMatrix$$ $italic dvecAlpIn$$ contains the column vector 
$math%alpIn%$$, which specifies the initial value for the fixed population 
parameters.
The $xref/glossary/Ordering Of Vectors/order condition/$$,
$math%alpLow \le alpIn \le alpUp%$$, is assumed to hold.
Note that the length of $italic dvecAlpIn$$ specifies the length of 
the fixed population parameter vector $math%alp%$$.

$syntax/

/pdvecAlpOut/
/$$
If $italic pdvecAlpOut$$ is not $code NULL$$, then the 
$code DoubleMatrix$$ pointed to by $italic pdvecAlpOut$$ must 
be declared in the function that calls this function, and it 
must have the same dimensions as $italic dvecAlpIn$$.
If $italic pdvecAlpOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then the $code DoubleMatrix$$ pointed to by $italic pdvecAlpOut$$ 
will contain the column vector $math%alpOut%$$, which is the 
estimate for the true minimizer of the population objective function.
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdvecAlpOut$$.

$syntax/

/dvecAlpStep/
/$$
The $code DoubleMatrix$$ $italic dvecAlpStep$$ contains the column vector 
$math%alpStep%$$, which specifies the step size used for approximating
the derivatives with respect to the fixed population parameters.
The value of this parameter does not matter if
$italic pdmatLTilde_alp_alpOut$$ is $code NULL$$.
The length of $italic dvecAlpStep$$ is equal to the length of 
the fixed population parameter vector $math%alp%$$.

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
and $math%bHat_i%$$ is a local minimizer of $math%Lambda_i(alp, b)%$$ 
with respect to $math%b%$$.
Since $math%bHat_i%$$ is unknown, this function estimates the left hand
side of this inequality in a way that is a good approximation when 
the Hessian of the objective function is positive definite.
$pre

$$
Note that if $italic nMaxIter$$ is set to zero, then the $th i$$ 
column of $math%bIn%$$ is accepted as the estimate for 
$math%bHat_i%$$.
$pre

$$
For a particular value of $math%alp%$$ and for the $math%i%$$-th 
individual in the population, an individual parameter value 
$math%bOut_i%$$ is accepted as an estimate for $math%bTilde_i%$$ if 
$math%
        abs( bOut_i - bTilde_i ) \le epsilon ( bUp - bLow )  ,
%$$
where $math%abs%$$ is the element-by-element absolute value function
and $math%bTilde_i%$$ is the point where the approximate projected 
gradient of $math%Lambda_i(alp, b)%$$ with respect to $math%b%$$ 
is zero.
Since $math%bTilde_i%$$ is unknown, this function estimates the left hand
side of this inequality in a way that is a good approximation when 
the Hessian of the objective function is positive definite.
$pre

$$
Note that if $italic nMaxIter$$ is set to zero, then the $th i$$ 
column of $math%bIn%$$ is accepted as the estimate for 
$math%bTilde_i%$$.

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

/dvecBLow/
/$$
The $code DoubleMatrix$$ $italic dvecBLow$$ contains the column vector 
$math%bLow%$$, which specifies the lower limit for the random parameters 
during the optimization procedure for all the individuals.
The length of $italic dvecBLow$$ is equal to the length of 
the random population parameter vector $math%b%$$.

$syntax/

/dvecBUp/
/$$
The $code DoubleMatrix$$ $italic dvecBUp$$ contains the column vector 
$math%bUp%$$, which specifies the upper limit for the random parameters 
during the optimization procedure for all the individuals.
The length of $italic dvecBUp$$ is equal to the length of 
the random population parameter vector $math%b%$$.

$syntax/

/dmatBIn/
/$$
The $code DoubleMatrix$$ $italic dmatBIn$$ contains the matrix 
$math%bIn%$$.  
The $th i$$ column of $math%bIn%$$ specifies the initial value for 
the random parameters for the $th i$$ individual.
If $math%b_i%$$ is any column of $math%bIn%$$,
it is assumed that $math%bLow \le b_i \le bUp%$$.
The column dimension of $math%bIn%$$ is equal to the number of 
individuals in the population, $math%M%$$.
Note that the number of rows in $italic dmatBIn$$ specifies the 
length of the random population parameter vector $math%b%$$.

$syntax/

/pdmatBOut/
/$$
If $italic pdmatBOut$$ is not $code NULL$$, 
then the $code DoubleMatrix$$ pointed to by $italic pdmatBOut$$ must 
be declared in the function that calls this function, 
and it must have the same dimensions as $math%bIn%$$.
If $italic pdmatBOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then the $code DoubleMatrix$$ pointed to by $italic pdmatBOut$$ will 
contain $math%bOut%$$, which is the matrix of estimates for the true 
minimizers of the individual objective functions.
Otherwise, this function will not attempt to change the contents of 
the $code DoubleMatrix$$ pointed to by $italic pdmatBOut$$.
To be specific, the $th i$$ column of $math%bOut%$$ contains a column
vector that is an estimate for $math%bHat_i%$$, the minimizer 
of $math%Lambda_i(alpOut, b)%$$ with respect to $math%b%$$. 
This is under the assumption that $math%alpOut%$$
is the true value for the fixed population parameters.
The value $math%epsilon(1)%$$ is used for accepting the minimizers with 
respect to the random population parameters.

$syntax/

/dvecBStep/
/$$
The $code DoubleMatrix$$ $italic dvecBStep$$ contains the column vector 
$math%bStep%$$, which specifies the step size used for approximating
the derivatives with respect to the random population parameters.
The length of $italic dvecBStep$$ is equal to the length of 
the random population parameter vector $math%b%$$.

$syntax/

/pdLTildeOut/
/$$
If $italic pdLTildeOut$$ is not $code NULL$$, then the $code double$$ 
value pointed to by $italic pdLTildeOut$$ must be declared in the 
function that calls this function.
If $italic pdLTildeOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then the $code double$$ value pointed to by $italic pdLTildeOut$$ will 
be equal to $math%LTilde(alpOut)%$$, which is the value of the population 
objective function evaluated at $math%alpOut%$$.
Otherwise, this function will not attempt to change the contents of the 
$code double$$ value pointed to by $italic pdLTildeOut$$.

$syntax/

/pdrowLTilde_alpOut/
/$$
If $italic pdrowLTilde_alpOut$$ is not $code NULL$$, then the 
$code DoubleMatrix$$ pointed to by $italic pdrowLTilde_alpOut$$ 
must be declared in the function that calls this function, and it 
must be a row vector that is the same length as
the fixed population parameter vector $math%alp%$$.
If $italic pdrowLTilde_alpOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then the $code DoubleMatrix$$ pointed to by $italic pdrowLTilde_alpOut$$ 
will contain the row vector $math%LTilde_alp(alpOut)%$$, which is
the derivative of the population objective function evaluated at 
$math%alpOut%$$.
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdrowLTilde_alpOut$$.

$syntax/

/pdmatLTilde_alp_alpOut/ 
/$$
If $italic pdmatLTilde_alp_alpOut$$ is not $code NULL$$, then the 
$code DoubleMatrix$$ pointed to by $italic pdmatLTilde_alp_alpOut$$ 
must be declared in the function that calls this function, and it 
must have the same number of rows and columns as the length of
the fixed population parameter vector $math%alp%$$.
If $italic pdmatLTilde_alp_alpOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then the $code DoubleMatrix$$ pointed to by 
$italic pdmatLTilde_alp_alpOut$$ will contain the matrix 
$math%LTilde_alp_alp(alpOut)%$$, which is an approximation 
for the second derivative of the population objective function 
evaluated at $math%alpOut%$$. 
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdmatLTilde_alp_alpOut$$.
The approximation for the second derivative is formed using central
differences of the function $math%LTilde_alp(alp)%$$ with
step sizes specified by $italic dvecAlpStep$$.

$head Example$$

$pre

$$
If you compile, link, and run the following program:
$codep

#include "ppkaOpt.h"
#include "namespace_population_analysis.h"
#include "identity.h"
#include "pi.h"
#include "allZero.h"
#include "randNormal.h"
#include <iostream>
#include <iomanip>
#include <string>
#include <cmath>
#include "SpkValarray.h"

using std::string;

namespace ppkaoptexample
{
  static DoubleMatrix funF    ( const DoubleMatrix &dvecAlp, 
                                const DoubleMatrix &dvecB );
  static DoubleMatrix funF_alp( const DoubleMatrix &dvecF,   
                                const DoubleMatrix &dvecAlp, 
                                const DoubleMatrix &dvecB );
  static DoubleMatrix funF_b  ( const DoubleMatrix &dvecF, 
                                const DoubleMatrix &dvecAlp,   
                                const DoubleMatrix &dvecB );
  static DoubleMatrix funR    ( const DoubleMatrix &dvecAlp, 
                                const DoubleMatrix &dvecB );
  static DoubleMatrix funR_alp( const DoubleMatrix &dmatR,   
                                const DoubleMatrix &dvecAlp, 
                                const DoubleMatrix &dvecB );
  static DoubleMatrix funR_b  ( const DoubleMatrix &dmatR, 
                                const DoubleMatrix &dvecAlp,   
                                const DoubleMatrix &dvecB );
  static DoubleMatrix funD    ( const DoubleMatrix &dvecAlp );
  static DoubleMatrix funD_alp( const DoubleMatrix &dmatD,   
                                const DoubleMatrix &dvecAlp );
}

class PopModel : public SpkModel
{
    DoubleMatrix _a, _b;
    int _i;

public:
    PopModel(){}
    ~PopModel(){}
protected:
    void doSelectIndividual(int i)
    {
        _i = i;
    }
    void doSetPopPar(const valarray<double>& alp )
    {
        _a = DoubleMatrix( alp, 1 );
    }
    void doSetIndPar(const valarray<double>& b )
    {
        _b = DoubleMatrix( b, 1 );
    }
    void doDataMean( valarray<double>& ret ) const 
    {
        using namespace ppkaoptexample;
        ret = funF(_a, _b).toValarray();
    }
    bool doDataMean_popPar( valarray<double>& ret ) const 
    {
        using namespace ppkaoptexample;
        doDataMean(ret);
        ret = funF_alp(ret, _a, _b).toValarray();
        return !allZero(ret);
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        using namespace ppkaoptexample;
        doDataMean(ret);
        ret = funF_b(ret, _a, _b).toValarray();
        return !allZero(ret);
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        using namespace ppkaoptexample;
        ret = funR(_a, _b).toValarray();
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        using namespace ppkaoptexample;
        doDataVariance(ret);
        ret = funR_alp(ret, _a, _b).toValarray();
        return !allZero(ret);
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        using namespace ppkaoptexample;
        doDataVariance(ret);
        ret = funR_b(ret, _a, _b).toValarray();
        return !allZero(ret);
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
        using namespace ppkaoptexample;
        ret = funD(_a).toValarray();
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        using namespace ppkaoptexample;
        doIndParVariance(ret);
        ret = funD_alp(ret, _a).toValarray();
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

  using namespace population_analysis;

  using namespace ppkaoptexample;

  int i;


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  PopModel model;


  //------------------------------------------------------------
  // Quantities that define the problem.
  //------------------------------------------------------------

  // Mean and variance of the true transfer rate, betaTrue.
  double meanBetaTrue = 1.0;
  double varBetaTrue  = 5.0;

  // Number of individuals.
  int nInd = 10;


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  // Number of measurements.
  int nY = nInd;

  // Measurement values, y.
  DoubleMatrix dvecY( nY, 1 );
  double* pdYData = dvecY.data();

  // Number of measurements for each individual. 
  DoubleMatrix dvecN( nInd, 1 );
  dvecN.fill( (double) 1 );

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
    eTrue = randNormal( meanETrue, sdETrue );
    bTrue = randNormal( meanBTrue, sdBTrue );

    pdYData[ i ] = meanBetaTrue + bTrue + eTrue;
  }


  //------------------------------------------------------------
  // Quantities related to the fixed population parameter, alp.
  //------------------------------------------------------------

  int nAlp = 2;

  DoubleMatrix dvecAlpTrue( nAlp, 1 );
  DoubleMatrix dvecAlpLow ( nAlp, 1 );
  DoubleMatrix dvecAlpUp  ( nAlp, 1 );
  DoubleMatrix dvecAlpIn  ( nAlp, 1 );
  DoubleMatrix dvecAlpOut ( nAlp, 1 );
  DoubleMatrix dvecAlpStep( nAlp, 1 );

  double* pdAlpTrueData = dvecAlpTrue.data();
  double* pdAlpLowData  = dvecAlpLow .data();
  double* pdAlpUpData   = dvecAlpUp  .data();
  double* pdAlpInData   = dvecAlpIn  .data();
  double* pdAlpOutData  = dvecAlpOut .data();
  double* pdAlpStepData = dvecAlpStep.data();

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
  // Quantities related to the random population parameters, b.
  //------------------------------------------------------------

  int nB = 1;

  DoubleMatrix dvecBLow ( nB, 1 );
  DoubleMatrix dvecBUp  ( nB, 1 );
  DoubleMatrix dvecBStep( nB, 1 );

  dvecBLow .fill( -1.5e+1 );
  dvecBUp  .fill( +1.0e+1 );
  dvecBStep.fill(  1.0e-2 );

  DoubleMatrix dmatBIn ( nB, nInd );
  DoubleMatrix dmatBOut( nB, nInd );

  dmatBIn.fill( 1.0 );


  //------------------------------------------------------------
  // Quantities related to the population objective function.
  //------------------------------------------------------------

  double dLTildeOut;

  DoubleMatrix drowLTilde_alpOut    ( 1,    nAlp );
  DoubleMatrix dmatLTilde_alp_alpOut( nAlp, nAlp );


  //------------------------------------------------------------
  // Remaining inputs to ppkaOpt.
  //------------------------------------------------------------

  enum Objective objective = MODIFIED_LAPLACE;

  Optimizer popOptimizer( 1.0e-6, 40, 0 );
  Optimizer indOptimizer( 1.0e-6, 40, 0 );


  //------------------------------------------------------------
  // Optimize the population objective function.
  //------------------------------------------------------------

  try{
    ppkaOpt(  model,
              objective,
              dvecN,
              dvecY,
              popOptimizer,
              dvecAlpLow,
              dvecAlpUp,
              dvecAlpIn,
              &dvecAlpOut,
              dvecAlpStep,
              indOptimizer,
              dvecBLow,
              dvecBUp,
              dmatBIn,
              &dmatBOut,
              dvecBStep,
              &dLTildeOut,
              &drowLTilde_alpOut,
              &dmatLTilde_alp_alpOut );
  }
  catch(...)
  {
    cerr << "ppkaOpt failed" << endl;
    abort();
  }

  //------------------------------------------------------------
  // Print the results.
  //------------------------------------------------------------

  cout << endl;

  cout << "alpOut = " << endl;
  dvecAlpOut.print(); 
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
//                 /                 \ 
//     f(alp, b) = |  alp(1) + b(1)  |  .
//                 \                 / 
//
//--------------------------------------------------------------

static DoubleMatrix ppkaoptexample::funF( const DoubleMatrix &dvecAlp, 
                                          const DoubleMatrix &dvecB )
{
  DoubleMatrix dvecF( 1, 1 );

  double* pdAlpData = dvecAlp.data();
  double* pdBData   = dvecB  .data();

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

static DoubleMatrix ppkaoptexample::funF_alp( const DoubleMatrix &dvecF, 
                                              const DoubleMatrix &dvecAlp, 
                                              const DoubleMatrix &dvecB )
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

static DoubleMatrix ppkaoptexample::funF_b( const DoubleMatrix &dvecF, 
                                            const DoubleMatrix &dvecAlp, 
                                            const DoubleMatrix &dvecB )
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

static DoubleMatrix ppkaoptexample::funR( const DoubleMatrix &dvecAlp, 
                                          const DoubleMatrix &dvecB )
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

static DoubleMatrix ppkaoptexample::funR_alp( const DoubleMatrix &dmatR,   
                                              const DoubleMatrix &dvecAlp, 
                                              const DoubleMatrix &dvecB )
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

static DoubleMatrix ppkaoptexample::funR_b( const DoubleMatrix &dmatR, 
                                            const DoubleMatrix &dvecAlp,   
                                            const DoubleMatrix &dvecB )
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

static DoubleMatrix ppkaoptexample::funD( const DoubleMatrix &dvecAlp )
{
  DoubleMatrix dmatD( 1, 1 );

  double* pdAlpData = dvecAlp.data();

  dmatD.fill( pdAlpData[ 1 ] );

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

static DoubleMatrix ppkaoptexample::funD_alp( const DoubleMatrix &dmatD,
                                              const DoubleMatrix &dvecAlp )
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

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
#include "DoubleMatrix.h"
#include "ppkaOpt.h"
#include "SpkModel.h"
#include "SpkException.h"

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
void ppkaOpt( SpkModel&               model,
              enum  Objective         objective,
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
              DoubleMatrix*           pdmatLTilde_alp_alpOut )
{
    ppkaOpt(
        model,
        objective,
        dvecN,
        dvecY,
        popOptimizer,
        dvecAlpLow,
        dvecAlpUp,
        dvecAlpIn,
        pdvecAlpOut,
        dvecAlpStep,
		indOptimizer,
        dvecBLow,
        dvecBUp,
        dmatBIn,
        pdmatBOut,
        dvecBStep,
        pdLTildeOut,
        pdrowLTilde_alpOut,
        pdmatLTilde_alp_alpOut,
        false
        );
    return;
}
