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
 * File: fitPopulation.cpp
 *
 *
 * Optimizes the parametric population objective functions.
 *
 * Author: Jiaji Du
 *
 * Later updated by Sachiko, 10/10/2002
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: fitPopulation
 *
 * [ Implementation Note --- Sachiko ]
 * This function serves as a user entry point.  
 * It takes care of setting off the universal floating point error 
 * check, initiating the subprocess acting as a co-node if parallel
 * execution is requested and redirecting execution paths depending on
 * a choice of objectives.
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*

$begin fitPopulation$$

$spell
  throwExcepIfMaxIter
  struct
  Model model
  Objective objective
  SPK_VA
  bool  
  cbc  
  cmath  
  const  
  cout  
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
  ind 
  inv 
  distrib 
  Fab 
  Rab 
  Da 
  multi  
  pdalp 
  holden
  covariances
  str
  Spk
  cerr
  covariance
  optimizer
  pathname
  fp
  spawn
  Ri
  valarray
  dir
  enum
  Yi
  inx
  aval
  bval
  resize
  Dinv
  Rinv
  Fo
  Sachiko
$$

$section Fitting Population Parameter$$

$index ppkaOptParallel$$
$cindex optimizing \the parametric population objective \functions \in multi-process mode$$
$index parallel, ppkaOpt$$
$index population, fitting mode$$

$table
$bold Header:$$ $cend
fitPopulation.h $rend

$bold Prototype:$$ $cend
$syntax/void fitPopulation(
				SpkModel&                       /popModel/,
				enum Objective                  /objective/,
				const SPK_VA::valarray<int>&    /nMeasurementsAll/,
				const SPK_VA::valarray<double>& /measurementsAll/,
				Optimizer&                      /popOptimizer/,
				const SPK_VA::valarray<double>& /popParLow/,
				const SPK_VA::valarray<double>& /popParUp/,
				const SPK_VA::valarray<double>& /popParIn/,
				const SPK_VA::valarray<double>& /popParStep/,
				SPK_VA::valarray<double>*       /popParOut/,
				Optimizer&                      /indOptimizer/,
				const SPK_VA::valarray<double>& /indParLow/,
				const SPK_VA::valarray<double>& /indParUp/,
				const SPK_VA::valarray<double>& /indParAllIn/,
				const SPK_VA::valarray<double>& /indParStep/,
				SPK_VA::valarray<double>*       /indParAllOut/,
				double*                         /popObjOut/,
				SPK_VA::valarray<double>*       /popObj_popParOut/,
				SPK_VA::valarray<double>*       /popObj_popPar_popParOut/,
				const DirBasedParallelControls& /dirBasedParallelControls/
                     )
/$$

$tend

See also: $xref/SpkModel//SpkModel/$$, $xref/SpkValarray//SPK_VA::valarray/$$,
$xref/Optimizer//Optimizer/$$, $xref/Objective//Objective/$$ 

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
the individual parameter vector $math%b%$$ is defined as
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
The individual parameter vector $math%bHat_i%$$ is the true 
minimizer of $math%Lambda_i(alp, b)%$$ with respect to $math%b%$$. 
The individual parameter vector $math%bTilde_i%$$, on the other
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
For the case of the modified First-order objective function,
$math%HTilde_i(alp, b)%$$ is the same approximation used in the modified
laplace objective (described above) but with 
$math%f_i(alp, b)%$$ replaced by a linear approximation in $math%b%$$ and 
ignoring the dependence of $math%R_i(alp, b)%$$ on $math%b%$$.
In addition, we use finite differences in place of derivatives in the objective.

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
values for the population parameters and the individual parameters
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
/popModel/
/$$
This function expects $italic model$$ to be a function of
all three parameters: $math%alp%$$, $math%b%$$ and $math%i%$$.
Refer $xref/glossary/Model Functions Depend on i - alp - b/Model Functions Depend on i - alp - b/$$
for details.


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

/popParLow/
/$$
The $code SPK_VA::valarray<double>$$ $italic popParLow$$ contains the vector 
$math%alpLow%$$, which specifies the lower limit for $math%alp%$$ during 
the optimization procedure.
The size of $italic popParLow$$ is equal to the length of 
the population parameter vector $math%alp%$$.

$syntax/

/popParUp/
/$$
The $code SPK_VA::valarray<double>$$ $italic popUp$$ contains the vector
$math%alpUp%$$, which specifies the upper limit for $math%alp%$$ during 
the optimization procedure.
The size of $italic popParUp$$ is equal to the length of 
the population parameter vector $math%alp%$$.

$syntax/

/popParIn/
/$$
The $code SPK_VA::valarray<double>$$ $italic popParIn$$ contains the vector 
$math%alpIn%$$, which specifies the initial value for the population 
parameters.
The $xref/glossary/Ordering Of Vectors/order condition/$$,
$math%alpLow \le alpIn \le alpUp%$$, is assumed to hold.
Note that the size of $italic popParIn$$ is the same as the length of 
the population parameter vector $math%alp%$$.

$syntax/

/popParStep/
/$$
The $code  SPK_VA::valarray<double>$$ $italic popParStep$$ contains the vector 
$math%alpStep%$$, which specifies the step size used for approximating
the derivatives with respect to the population parameters.
The value of this parameter is not used if
$italic popObj_popPar_popParOut$$ is $code NULL$$.
The size of $italic popParStep$$ is equal to the length of 
the population parameter vector $math%alp%$$.

$syntax/

* /popParOut/
/$$
If $italic popParOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by &$italic popParOut$$ must 
be declared in the function that calls this function, and it 
must have the same size as $italic popParIn$$.
If $italic popParOut$$ is not NULL, 
and if this function completed the optimization successfully, 
then the $code  SPK_VA::valarray<double>$$ object pointed to by $italic popParOut$$ 
will contain the vector $math%popOut%$$ that is the 
estimate for the true minimizer of the population objective function.
Otherwise, this function will not attempt to change the contents of the 
$code  SPK_VA::valarray<double>$$ object pointed to by $italic popParOut$$.

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

/indParLow/
/$$
The $code SPK_VA::valarray<double>$$ $italic indParLow$$ contains the vector
$math%bLow%$$, which specifies the lower limit for the individual parameters 
during the optimization procedure for all the individuals.
The size of $italic indParLow$$ is equal to the length of 
the individual parameter vector $math%b%$$.

$syntax/

/indParUp/
/$$
The $code SPK_VA::valarray<double>$$ $italic indParUp$$ contains the vector 
$math%bUp%$$, which specifies the upper limit for the individual parameters 
during the optimization procedure for all the individuals.
The size of $italic indParUp$$ is equal to the length of 
the individual parameter vector $math%b%$$.

$syntax/

/indParIn/
/$$
The $code SPK_VA::valarray<double>$$ $italic indParIn$$ contains the matrix 
$math%bIn%$$ in column major order.  The size of $italic indParIn$$ is
equal to the product of the length of the individual parameter vector 
$math%b%$$ and the number of individuals in the population. 
The $th i$$ column of $math%bIn%$$ specifies the initial value for 
the individual parameters for the $th i$$ individual.
If $math%b_i%$$ is any column of $math%bIn%$$,
it is assumed that $math%bLow \le ind_i \le bUp%$$.
Note that the column dimension of $math%bIn%$$ is equal to the number of 
individuals in the population, $math%M%$$.
and the number of rows in $italic indParIn$$ is equal to the 
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

* /indParOut/
/$$
If $italic indParOut$$ is not $code NULL$$, then the $code SPK_VA::valarray<double>$$ 
object pointed to by $italic indParOut$$ must be declared in the function that 
calls this function, and it must have the same size as $italic indParIn$$.
If $italic indParOut$$ is not $code NULL$$ and this function completed the optimization 
successfully, 
then the $code SPK_VA::valarray<double>$$ object pointed to by $italic indParOut$$ 
will contain the matrix $math%bOut%$$, in the column major order, that is the 
matrix of the estimates for the true minimizers of the individual objective 
functions.  Otherwise, this function will not attempt to change the contents of the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic indParOut$$.
To be specific, the $th i$$ column of $math%bOut%$$ contains a column
vector that is an estimate for $math%bHat_i%$$, the minimizer 
of $math%Lambda_i(bOut, b)%$$ with respect to $math%b%$$. 
This is under the assumption that the estimated $math%alpOut%$$
is the true value for the population parameters.

$syntax/

* /popObjOut/
/$$
If $italic popObjOut$$ is not $code NULL$$, then the $code double$$ 
value pointed to by $italic popObjOut$$ must be declared in the 
function that calls this function.
If $italic popObjOut$$ is not $code NULL$$ and this function completed the 
optimization successfully, then the $code double$$ value pointed to by 
$italic popObjOut$$ will be equal to $math%LTilde(alpOut)%$$ that is 
the value of the population objective function evaluated at $math%alpOut%$$.
Otherwise, this function will not attempt to change the contents of the 
$code double$$ value pointed to by $italic popObjOut$$.

$syntax/

* /popObj_popParOut/
/$$
If $italic popObj_popParOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popObj_popParOut$$ 
must be declared in the function that calls this function, and its size must
be equal to the length of the population parameter vector $math%alp%$$.  If $italic 
popObj_popParOut$$ is not $code NULL$$ and this function completed the optimization 
successfully, then the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic popObj_popParOut$$ will contain the vector $math%LTilde_alp(alpOut)%$$ 
that is the derivative of the population objective function with respect to 
the population parameter vector $math%alp%$$ evaluated at $math%alpOut%$$.  
Otherwise, this function will not attempt to change the contents of the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popObj_popParOut$$.

$syntax/

* /popObj_popPar_popParOut/ 
/$$
If $italic popObj_popPar_popParOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popObj_popPar_popParOut$$ 
must be declared in the function that calls this function, and its size 
must be equal to the square of the length of the population parameter vector 
$math%alp%$$.  If $italic popObj_popPar_popParOut$$ is not $code NULL$$
and this function completed the optimization successfully, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic popObj_popPar_popParOut$$ 
will contain the matrix $math%LTilde_alp_alp(alpOut)%$$, in column major order, 
that is an approximation for the second derivative of the population objective 
function evaluated at $math%alpOut%$$.  Otherwise, this function will not attempt 
to change the contents of the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic popObj_popPar_popParOut$$.  The approximation for the second derivative 
is calculated using central differences of the function $math%LTilde_alp(alp)%$$ 
with step sizes specified by $italic popParStep$$.

$syntax/

/dirBasedParallelControls/ (optional)
/$$
This option is disabled.

$head Example$$
The following demonstrates running fitPopulation() in the single processing mode.

$codep

#include <iostream>
#include <iomanip>
#include <string>
#include <cmath>

#include "fitPopulation.h"
#include "SpkValarray.h"
#include "randNormal.h"

// For generating random numbers

using std::string;

class UserModelFitPopulationExampleTest : public SpkModel
{
    SPK_VA::valarray<double> _a, _b;
	const int _nA;
    const int _nB;
    const int _nYi;
    int _i;
public:
    UserModelFitPopulationExampleTest(int nA, int nB, int nYi)
	:_nA(nA), _nB(nB), _nYi(nYi)
	{};    
    ~UserModelFitPopulationExampleTest(){};
private:
    void doSelectIndividual(int inx)
    {
        _i = inx;
    }
    void doSetPopPar(const SPK_VA::valarray<double>& aval)
    {
        _a = aval;
    }
    void doSetIndPar(const SPK_VA::valarray<double>& bval)
    {
        _b = bval;
    }
    void doIndParVariance( SPK_VA::valarray<double>& ret ) const
    {
        //
        // D = [ alp[1] ]
        //
        ret.resize(_nYi);
        ret[0] = _a[1];
    }
    bool doIndParVariance_popPar( SPK_VA::valarray<double>& ret ) const
    {
        //
        // D_alp = [ 0  1 ]
        //
        ret.resize(_nYi * _nA);
        ret[0] = 0.0;
        ret[1] = 1.0;
        return true;
    }
    void doIndParVarianceInv( SPK_VA::valarray<double>& ret ) const
    {
        //
        // Dinv = [ 1.0 / alp[1] ]
        //
        assert(_a[1] != 0.0);
        ret.resize(_nB * _nB);
        ret[0] = ( 1.0 / _a[1] );
    }
    bool doIndParVarianceInv_popPar( SPK_VA::valarray<double>& ret ) const
    {
        //
        // Dinv_alp = [ 0    -alp[1]^(-2) ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = -1.0 / (_a[1]*_a[1]);
        return true;
    }
    void doDataMean( SPK_VA::valarray<double>& ret ) const
    {
        //
        // f = [ alp[0]+b[0] ]
        //
        ret.resize(_nYi);
        ret[0] = ( _a[0] + _b[0] );
    }
    bool doDataMean_popPar( SPK_VA::valarray<double>& ret ) const
    {
        //
        // f_alp = [ 1   0 ]
        //
        ret.resize(_nYi * _nA);
        ret[0] = 1.0;
        ret[1] = 0.0;
        return true;
    }
    bool doDataMean_indPar( SPK_VA::valarray<double>& ret ) const
    {
        //
        // f_b = [ 1 ]
        //
        ret.resize(_nYi * _nB);
        ret[0] = 1.0;
        return true;
    }
    void doDataVariance( SPK_VA::valarray<double>& ret ) const
    {
        //
        // R = [ 1 ]
        //
        ret.resize(_nB*_nB);
        ret[0] = 1.0;
    }
    bool doDataVariance_popPar( SPK_VA::valarray<double>& ret ) const
    {
        //
        // R_alp = [ 0   0 ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
        return false;
    }
    bool doDataVariance_indPar( SPK_VA::valarray<double>& ret ) const
    {
        //
        // R_b = [ 0 ]
        //
        ret.resize(_nB *_nB);
        ret[0] = 0.0;
        return false;
    }
    void doDataVarianceInv( SPK_VA::valarray<double>& ret ) const
    {
        //
        // Rinv = [ 1 ]
        //
        ret.resize(_nB * _nB);
        ret[0] = 1.0;
    }
    bool doDataVarianceInv_popPar( SPK_VA::valarray<double>& ret ) const
    {
        //
        // Rinv_alp = [ 0  0 ]
        //
        ret.resize(_nB * _nA);
        ret[0] = 0.0;
        ret[1] = 0.0;
        return false;
    }
    bool doDataVarianceInv_indPar( SPK_VA::valarray<double>& ret ) const
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

  using namespace std;

  using namespace population_analysis;

  int i;

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

  UserModelFitPopulationExampleTest model( nAlp, nB, nYi );


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
  SPK_VA::valarray<double> dvecY( nY );

  // Number of measurements for each individual. 
  SPK_VA::valarray<double> dvecN( 1., nInd );

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

    dvecY[ i ] = meanBetaTrue + bTrue + eTrue;
  }


  //------------------------------------------------------------
  // Quantities related to the fixed population parameter, alp.
  //------------------------------------------------------------

  SPK_VA::valarray<double> dvecAlpTrue( nAlp );
  SPK_VA::valarray<double> dvecAlpLow ( nAlp );
  SPK_VA::valarray<double> dvecAlpUp  ( nAlp );
  SPK_VA::valarray<double> dvecAlpIn  ( nAlp );
  SPK_VA::valarray<double> dvecAlpOut ( nAlp );
  SPK_VA::valarray<double> dvecAlpStep( nAlp );

  // Set the values associated with alp(1).
  dvecAlpTrue[ 0 ] = meanBetaTrue;
  dvecAlpLow [ 0 ] = -10.0;
  dvecAlpUp  [ 0 ] = 10.0;
  dvecAlpIn  [ 0 ] = -1.0;
  dvecAlpStep[ 0 ] = 1.0e-2;

  // Set the values associated with alp(2).
  dvecAlpTrue[ 1 ] = varBetaTrue;
  dvecAlpLow [ 1 ] = 1.0e-3;
  dvecAlpUp  [ 1 ] = 100.0;
  dvecAlpIn  [ 1 ] = 0.5;
  dvecAlpStep[ 1 ] = 1.0e-2;
  

  //------------------------------------------------------------
  // Quantities related to the random population parameters, b.
  //------------------------------------------------------------

  SPK_VA::valarray<double> dvecBLow ( -1.5e+1, nB );
  SPK_VA::valarray<double> dvecBUp  ( +1.0e+1, nB );
  SPK_VA::valarray<double> dvecBStep(  1.0e-2, nB );

  SPK_VA::valarray<double> dmatBIn ( 1., nB * nInd );
  SPK_VA::valarray<double> dmatBOut(     nB * nInd );


  //------------------------------------------------------------
  // Quantities related to the population objective function.
  //------------------------------------------------------------

  double dLTildeOut;

  SPK_VA::valarray<double> drowLTilde_alpOut    ( nAlp );
  SPK_VA::valarray<double> dmatLTilde_alp_alpOut( nAlp * nAlp );


  //------------------------------------------------------------
  // Remaining inputs to fitPopulation.
  //------------------------------------------------------------

  // Set objective
  enum Objective objective = MODIFIED_LAPLACE;

  // Set the optimizer for individual level optimization.
  Optimizer indOptimizer( 1.0e-6, (double)40, (double)0 );

  // Set the optimizer for population level optimization.
  Optimizer popOptimizer( 1.0e-6, (double)40, (double)0 );

  // Set the parallel controls object
  DirBasedParallelControls parallelControls( false, NULL, NULL );


  //------------------------------------------------------------
  // Optimize the population objective function.
  //------------------------------------------------------------

  bool ok;
  try{
       fitPopulation(
					  model,
					  objective,
					  dvecN,
					  dvecY,
					  popOptimizer,
					  dvecAlpLow,
					  dvecAlpUp,
					  dvecAlpIn,
					  dvecAlpStep,
					  &dvecAlpOut,
					  indOptimizer,
					  dvecBLow,
					  dvecBUp,
					  dmatBIn,            
					  dvecBStep,
					  &dmatBOut,
					  &dLTildeOut,
					  &drowLTilde_alpOut,
					  &dmatLTilde_alp_alpOut, 
					  parallelControls 
			        );
       ok = true;
  }
  catch(...)
  {
    assertImplementation(false, "fitPopulation failed", __LINE__, __FILE__);
  }

  //------------------------------------------------------------
  // Print the results.
  //------------------------------------------------------------
  
  cout << "ok = " << ( ok ? "True" : "False" );
  cout << endl;

  cout << "popParOut = " << endl;
  DoubleMatrix alp( popParOut );
  alp.print(); 
  cout << endl;

  cout << "indParAllOut = " << endl;
  DoubleMatrix b( indParAllOut, nInd ); 
  b.print(); 
  cout << endl;

  cout << "popObjOut   = " << *popObjOut << endl;
  cout << endl;

  cout << "popObj_popParOut  = " << endl;
  DoubleMatrix LTilde_alp( popObj_popParOut, nAlp );
  LTilde_alp.print();
  cout << endl;

  cout << "popObj_popPar_popParOut  = " << endl;
  DoubleMatrix LTilde_alp_alp( popObj_popPar_popParOut, nAlp );
  LTilde_alp_alp.print();
  cout << endl;
}

$$
The program will display the following when it is run:
$codep

ok = True

popParOut =
[1.95115]
[3.63406]

indParAllOut =
[1.94171, 0.446611, -0.312347, -0.938796, -3.12919, 2.01348, 2.47441, -1.48642, -1.16138, 0.151919]

popObjOut   = 21.8566

popObj_popParOut  =
[-2.767e-007, 1.50158e-007]

popObj_popPar_popParOut  =
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
 * Global variable declarations
 *------------------------------------------------------------------------*/
//
// This timestamp serves as the current session ID.
//
#include <ctime>
time_t SESSION_ID;


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <iostream>
#include <iomanip>
#include <sstream>

#include "SpkValarray.h"
#include "fitPopulation.h"
#include "ppkaOpt.h"
#include "NaiveFoModel.h"
#include "firstOrderOpt.h"
#include "File.h"
#include "broadCastEndOfSpk.h"
#include "WarningsManager.h"

using SPK_VA::valarray;


/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  void checkPopPar(
    const Optimizer&    popOptimizer,
    const DoubleMatrix& dvecAlpLow,
    const DoubleMatrix& dvecAlpUp,
    const DoubleMatrix& dvecAlpOut );

  void checkIndPar(
    const Optimizer&    indOptimizer,
    const DoubleMatrix& dvecBLow,
    const DoubleMatrix& dvecBUp,
    const DoubleMatrix& dmatBOut );

} // [End: unnamed namespace]


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void fitPopulation( 
				    SpkModel&                   model,
					enum Objective              objective,
                    const valarray<int>&        nMeasurementsAll,
                    const valarray<double>&     measurementsAll,
                    Optimizer&                  popOptimizer,
                    const valarray<double>&     popParLow,
                    const valarray<double>&     popParUp,
					const valarray<double>&     popParIn,
					const valarray<double>&     popParStep,
                    valarray<double>*           popParOut,
					Optimizer&                  indOptimizer,
					const valarray<double>&     indParLow,
                    const valarray<double>&     indParUp,
					const valarray<double>&     indParAllIn,
					const valarray<double>&     indParStep,
					valarray<double>*           indParAllOut,
					double*                     popObjOut,
					valarray<double>*           popObj_popParOut,
					valarray<double>*           popObj_popPar_popParOut,
					const DirBasedParallelControls& dirBasedParallelControls
				  )
{
  using std::endl;
  using std::ends;

    
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------
  // Return if there are no output values to compute.
  if( popParOut            == 0 && 
      indParAllOut              == 0 && 
      popObjOut            == 0 && 
      popObj_popParOut     == 0 && 
      popObj_popPar_popParOut == 0    )
  {
    return;
  }


  const int nInd = nMeasurementsAll.size();
  const int nAlp = popParIn.size();
  const int nB   = indParStep.size();
  const int nY   = measurementsAll.size();

  int i;
  //===============[Begin: Vector lengths validation]===============
  if( nY != nMeasurementsAll.sum() )
  {
    int max = SpkError::maxMessageLen();
    char message[ max ];
    snprintf( message, max, 
"The sum of the values contained in nMeasurementsAll vector \
must match the lentth of measurementsAll vector.  %d does not match %d.",
	nMeasurementsAll.sum(), nY );

    throw SpkException(
            SpkError::SPK_USER_INPUT_ERR, 
            message,
            __LINE__, __FILE__
    );
  }
  if( popParLow.size() != nAlp )
  {
    int max = SpkError::maxMessageLen();
    char message[ max ];
    snprintf( message, max,
              "The length of popParLow vector must match the length of the popParIn vector." );

    throw SpkException(
            SpkError::SPK_USER_INPUT_ERR, 
            message,
            __LINE__, __FILE__
    );
  }
  if( popParUp.size() != nAlp )
  {
    int max = SpkError::maxMessageLen();
    char message[ max ];
    snprintf( message, max,
	      "The length of popParUp vector must match the length of the popParIn vector." );

    throw SpkException(
            SpkError::SPK_USER_INPUT_ERR, 
            message,
            __LINE__, __FILE__
    );
  }
  if( popParStep.size() != nAlp )
  {
    int max = SpkError::maxMessageLen();
    char message[ max ];
    snprintf( message, max,
              "The length of popParStep vector must match the length of the popParIn vector." );

    throw SpkException(
            SpkError::SPK_USER_INPUT_ERR, 
            message,
            __LINE__, __FILE__
    );
  }
  if( indParLow.size() != nB )
  {
    int max = SpkError::maxMessageLen();
    char message[ max ];
    snprintf( message, max,
              "The length of indParLow vector must match the length of the indParStep vector." );

    throw SpkException(
            SpkError::SPK_USER_INPUT_ERR, 
            message,
            __LINE__, __FILE__
    );
  }
  if( indParUp.size() != nB )
  {
    int max = SpkError::maxMessageLen();
    char message[ max ];
    snprintf( message, max,
              "The length of indParUp vector must match the length of the indParStep vector." );

    throw SpkException(
            SpkError::SPK_USER_INPUT_ERR, 
            message,
            __LINE__, __FILE__
    );
  }
  if( indParAllIn.size() != nB * nInd )
  {
    int max = SpkError::maxMessageLen();
    char message[ max ];
    snprintf( message, max,
"The length of indParAllIn vector must match the product of  \
the length of the indParStep vector and the number of individuals." );

    throw SpkException(
            SpkError::SPK_USER_INPUT_ERR, 
            message,
            __LINE__, __FILE__
    );
  }
  // This is a column vector.
  if ( popParOut )
  {
    int max = SpkError::maxMessageLen();
    char message[ max ];
      if( popParOut->size() != nAlp )
      {
        snprintf( message, max,
"PopParOut vector that will contain the estimated population parameter \
must be preallocated and have n length, where n is the lenghth of popPar.  %d is invalid.",
                  popParOut->size() );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR,  
                message,
                __LINE__, __FILE__
        );      
      }
  }

  if ( indParAllOut )
  {
      if( indParAllOut->size() != nB * nInd )
      {
        int max = SpkError::maxMessageLen();
        char message[ max ];
        snprintf( message, max,
        "indParOut vector that will receive the estimated individuals parameters \
must be preallocated and have m times n length, \
where m is the length of indPar and n is the # of individuals.  \
%d is invalid.", indParAllOut->size() );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR,  
                message,
                __LINE__, __FILE__
        );      
      }
  }

  if( popObj_popParOut )
  {
      if( popObj_popParOut->size() != nAlp )
      {
        int max = SpkError::maxMessageLen();
        char message[ max ];
        snprintf( message, max,
"popObj_popParOut vector that will contain the derivative of objective function \
must be preallocated and have n length, \
where n is the size of population parameter.  %d is invalid.",
                  popObj_popParOut->size() );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR, 
                message,
                __LINE__, __FILE__
        );
      }
  }

  if ( popObj_popPar_popParOut )
  {
      if( popObj_popPar_popParOut->size() != nAlp * nAlp )
      {
        int max = SpkError::maxMessageLen();
        char message[ max ];
        snprintf( message, max,
"Tilde_alp_alpOut vector that will contain the second derivative of objective function \
must be preallocated and have n times n length, \
where n is the size of population parameter.  %d is invalid.",
                  popObj_popPar_popParOut->size() );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR,  
                message,
                __LINE__, __FILE__
        );
      }
  }  
  //===============[End: Vector lengths validation]===============
  
  //===============[Begin: Data validation]===============

  if( indOptimizer.getEpsilon() <= 0.0 || indOptimizer.getEpsilon() > 1.0 )
  {
        int max = SpkError::maxMessageLen();
        char message[ max ];
        snprintf( message, max,
"The epsilon value must be greater than 0.0 and less than or equal to 1.0.  \
The epsilon for individual optimization, %d, is invalid.", indOptimizer.getEpsilon() );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR, 
                message,
                __LINE__, __FILE__
        );
  }

  if( popOptimizer.getEpsilon() <= 0.0 || popOptimizer.getEpsilon() > 1.0 )
  {
        int max = SpkError::maxMessageLen();
        char message[ max ];
        snprintf( message, max,
"The epsilon value must be greater than 0.0 and less than or equal to 1.0.  \
The epsilon for population optimization , %d, is invalid.", popOptimizer.getEpsilon() );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR, 
                message,
                __LINE__, __FILE__
        );
  }
  if( indOptimizer.getNMaxIter() < 0 )
  {
        int max = SpkError::maxMessageLen();
        char message[ max ];
        snprintf( message, max,
"The maximum number of iterations must be greater than or equal to zero.  \
The nMaxIter for individual optimization, %d, is invalid.", indOptimizer.getNMaxIter() );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR,  
                message,
                __LINE__, __FILE__
        );
  }
  if( popOptimizer.getNMaxIter() < 0 )
  {
        int max = SpkError::maxMessageLen();
        char message[ max ];
        snprintf( message, max,
"The maximum number of iterations must be greater than or equal to zero.  \
The nMaxIter for population optimization, %d, is invalid.", 
                  popOptimizer.getNMaxIter() );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR,  
                message,
                __LINE__, __FILE__
        );
  }
  if( indOptimizer.getLevel() < 0 )
  {
        int max = SpkError::maxMessageLen();
        char message[ max ];
        snprintf( message, max,
"The print level must be greater than or equal to zero.  \
The level for individual optimization, %d, is invalid.", 
                  indOptimizer.getLevel() );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR, 
                message,
                __LINE__, __FILE__ );
  }
  if( popOptimizer.getLevel() < 0 )
  {
        int max = SpkError::maxMessageLen();
        char message[ max ];
        snprintf( message, max,
"The print level must be greater than or equal to zero.  \
The level for population optimization, %d, is invalid.", 
                  popOptimizer.getLevel() );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR, 
                message,
                __LINE__, __FILE__ );
  }
  // Check that values in N are consistent with the size of y, i.e.,
  // 
  //     nY  =  N(1)  +  N(2)  +  ...  +  N(nInd)  .
  //
  for ( i = 0; i < nInd; i++ )
  {
    if( nMeasurementsAll[i] < 0 )
    {
        int max = SpkError::maxMessageLen();
        char message[ max ];
        snprintf( message, max,
"The number of measurements must be greater than zero.  %d-th element, %d, is invalid.",
                  i, nMeasurementsAll[i] );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR,  
                message,
                __LINE__, __FILE__
        );
    }
  }


  // Verify that the initial alp values are between their
  // lower and upper bounds.
  for ( i = 0; i < nAlp; i++ )
  {
      if( popParIn[i] < popParLow[i] || popParIn[i] > popParUp[i] )
      {
        int max = SpkError::maxMessageLen();
        char message[ max ];
        snprintf( message, max,
"The initial value for the population parameter must \
be less than or equal to the upper bound value and \
greater than or equal to the lower boundary value.  %d-ith element, %d, is invalid.",
                  i, popParIn[i] );

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
    for ( i = 0; i < nB; i++ )
    {
      if( indParAllIn[ i + j * nB ] < indParLow[i] || indParAllIn[ i + j * nB ] > indParUp[i] )
      {
        int max = SpkError::maxMessageLen();
        char message[ max ];
        snprintf( message, max,
"The initial value for the individual parameter must \
be less than or equal to the upper bound value and \
greater than or equal to the lower boundary value.  %d-th element, %d, is invalid.",
                  i, indParAllIn[ i + j * nB ] );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR,  
                message,
                __LINE__, __FILE__
        );
      }
    }
  }
  //===============[End: Data validation]===============

  //------------------------------------------------------------
  // Convert valarray to DoubleMatrix 
  //------------------------------------------------------------
  DoubleMatrix dvecN( nInd, 1 );
  double * pN = dvecN.data();
  for( i=0; i<nInd; i++ )
    pN[i] = static_cast<int>( nMeasurementsAll[i] ) ;

  DoubleMatrix dvecY( measurementsAll );
  DoubleMatrix dvecEpsilon( 2, 1 );
  dvecEpsilon.data()[ 0 ] = indOptimizer.getEpsilon();
  dvecEpsilon.data()[ 1 ] = popOptimizer.getEpsilon();
  DoubleMatrix dvecNMaxIter( 2, 1 );
  dvecNMaxIter.data()[ 0 ] = indOptimizer.getNMaxIter();
  dvecNMaxIter.data()[ 1 ] = popOptimizer.getNMaxIter();
  DoubleMatrix dvecLevel( 2, 1 );
  dvecLevel.data()[ 0 ] = indOptimizer.getLevel();
  dvecLevel.data()[ 1 ] = popOptimizer.getLevel();
  DoubleMatrix dvecAlpLow( popParLow );
  DoubleMatrix dvecAlpUp( popParUp );
  DoubleMatrix dvecAlpIn( popParIn );
  DoubleMatrix dvecAlpStep( popParStep );
  DoubleMatrix dvecBLow( indParLow );
  DoubleMatrix dvecBUp( indParUp );
  DoubleMatrix dmatBIn( indParAllIn, nInd );
  DoubleMatrix dvecBStep( indParStep );

  // Convert output parameters to DoubleMatrix
  DoubleMatrix dvecAlpOut, dmatBOut, drowLTilde_alpOut, dmatLTilde_alp_alpOut;
  DoubleMatrix* pdvecAlpOut = 0;
  DoubleMatrix* pdmatBOut = 0;
  DoubleMatrix* pdrowLTilde_alpOut = 0;
  DoubleMatrix* pdmatLTilde_alp_alpOut = 0;
  if( popParOut )
  {
	  dvecAlpOut.resize( nAlp, 1 );
	  pdvecAlpOut = &dvecAlpOut;
  }
  if( indParAllOut )
  {
	  dmatBOut.resize( indParUp.size(), nInd );
	  pdmatBOut = &dmatBOut;
  }
  if( popObj_popParOut )
  {
      drowLTilde_alpOut.resize( 1, nAlp );
	  pdrowLTilde_alpOut = &drowLTilde_alpOut;
  }
  if( popObj_popPar_popParOut )
  {
      dmatLTilde_alp_alpOut.resize( nAlp, nAlp );
	  pdmatLTilde_alp_alpOut = &dmatLTilde_alp_alpOut;
  }

  // Get parallelcontrols parameters
  bool isMultiple = dirBasedParallelControls.getIsParallel();
  const char* c_sharedDirectory = dirBasedParallelControls.getSharedDirectory();
  const char* coNodeCommand = dirBasedParallelControls.getCoNodeCommand();

  //------------------------------------------------------------
  // Validate the input matrix dimensions (debug only)
  //------------------------------------------------------------

  assert( dvecN.nc() == 1 );

  assert( dvecY.nc() == 1 );

  assert( dvecAlpLow.nc() == 1 );
  assert( dvecAlpLow.nr() == nAlp );

  assert( dvecAlpUp.nc() == 1 );
  assert( dvecAlpUp.nr() == nAlp );

  assert( dvecAlpIn.nc() == 1 );
  assert( dvecAlpIn.nr() == nAlp );

  assert( dvecAlpStep.nc() == 1 );
  assert( dvecAlpStep.nr() == nAlp );

  assert( dvecBLow.nc() == 1 );
  assert( dvecBLow.nr() == nB );

  assert( dvecBUp.nc() == 1 );
  assert( dvecBUp.nr() == nB );

  assert( dmatBIn.nc() == nInd );
  assert( dmatBIn.nr() == nB );

  assert( dvecBStep.nc() == 1 );
  assert( dvecBStep.nr() == nB );

  // This is a column vector.
  if ( pdvecAlpOut )
  {
      if( pdvecAlpOut->nr() != nAlp || pdvecAlpOut->nc() != 1 )
      {
	char message[ SpkError::maxMessageLen() ];
        snprintf( message, SpkError::maxMessageLen(),
"The output vector that will contain the estimated population parameter \
must be preallocated and have n by 1 dimensions, \
 where n is the size of population parameter.  %d by %d is invalid.",
                  pdvecAlpOut->nr(), pdvecAlpOut->nc() );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR,  
                message,
                __LINE__, __FILE__
        );      
      }
  }

  // There is one bOut vector for every individual.
  if ( pdmatBOut )
  {
      if( pdmatBOut->nr() != nB || pdmatBOut->nc() != nInd )
      {
	char message[ SpkError::maxMessageLen() ];
        snprintf( message, SpkError::maxMessageLen(),
"The output matrix that will contain the estimated individuals parameters \
must be preallocated and have m by n dimensions, \
where m is the size of any individual's parameter and \
n is the number of indviduals in the population. \
%d by %d is invalid.",
        pdmatBOut->nr(), pdmatBOut->nc() );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR,  
                message,
                __LINE__, __FILE__
        );      
      }
  }

  // This is a row vector.
  if ( pdrowLTilde_alpOut )
  {
      if( pdrowLTilde_alpOut->nr() != 1 || pdrowLTilde_alpOut->nc() != nAlp )
      {

	char message[ SpkError::maxMessageLen() ];
        snprintf( message, SpkError::maxMessageLen(),
"The output row vector that will contain the derivative of objective function \
must be preallocated and have n dimension, \
where n is the size of population parameter. \
%d by %d is invalid.",
                  pdrowLTilde_alpOut->nr(), pdrowLTilde_alpOut->nc() );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR, 
                message,
                __LINE__, __FILE__
        );
      }
  }

  // This has the same number of rows and columns as alp.
  if ( pdmatLTilde_alp_alpOut )
  {
      if( pdmatLTilde_alp_alpOut->nr() != nAlp || pdmatLTilde_alp_alpOut->nc() != nAlp )
      {
	char message[ SpkError::maxMessageLen() ];
        snprintf( message, SpkError::maxMessageLen(),
"The output matrix that will contain the second derivative of objective function \
must be preallocated and have n by n dimensions, \
where n is the size of population parameter. \
%d by %d is invalid.",
                  pdmatLTilde_alp_alpOut->nr(), pdrowLTilde_alpOut->nc() );

        throw SpkException(
                SpkError::SPK_USER_INPUT_ERR,  
                message,
                __LINE__, __FILE__
        );
      }
  }
  //===============[End: Data validation]===============
  
  //
  // This intializes the universal session ID.
  //
  time(&SESSION_ID);

  using namespace std;
  using namespace parallel_const;
  //
  // By creating a FpErrorChecker object, it activates the universal floating-point error detection
  // mechanism.  As it goes out of scope, the detector control word is restored as before.
  //
  FpErrorChecker checkerON;
  File sharedDirectory;

  //------------------------------------------------------------
  // Direct the execution path to firstOrderOpt() if
  // FIRST_ORDER (the version of FO that treats a population
  // problem as a big individual problem) were specified.
  //------------------------------------------------------------
  if( objective==FIRST_ORDER )
  {
    //
    // [ Comment by Sachiko, 10/09/2002 ]
    // This routine runs only in single processing mode.
    //
    DoubleMatrix* pdmatNull = 0;
    firstOrderOpt( model,
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
				   popObjOut,
				   pdrowLTilde_alpOut,
				   pdmatLTilde_alp_alpOut,
				   pdmatNull );

  }  
  //
  // [ Comment by Sachiko, 09/25/2002 ]
  //
  // The routines below this routine expect
  // the model to be of NaiveFoModel when NAIVE_FIRST_ORDER
  // is specified by the user.
  //
  else {
    //------------------------------------------------------------
    // Spawn a Node process if isMultiple == true 
    // and *nodeCommand
    //------------------------------------------------------------
    if( isMultiple )
    {
        sharedDirectory.setPath(c_sharedDirectory);
        if( coNodeCommand!=NULL )
        {
          std::vector< std::string > array;
          char *mutable_string = new char[strlen(coNodeCommand)+1];
          char buffer[512];
          strcpy(mutable_string, coNodeCommand);
          istringstream stream(mutable_string);
          int argc = 0;
          int i;
          while(stream.good())
          {
              stream >> buffer;
              array.push_back(buffer);
              ++argc;
          }
          char **argv = new char*[argc+1];
          for( i=0; i<argc+1; i++ )
              argv[i] = NULL;
          for( i=0; i<argc; i++ )
          {
              argv[i] = new char[ array[i].size() + 1];
              strcpy(argv[i], array[i].c_str());
          }
          int ret = System::spawnAsyncProcess( std::string(argv[0]), argv );
          if(  ret < 0 )
          {
              cerr << "Failed to spawn a co-process as a node: " << endl;
              switch(-ret){
              case E2BIG: 
                  cerr << "Argument list exceeds 1024 bytes" << endl;
                  break;
              case EINVAL: 
                  cerr << "Some mode argument is invalid" << endl;
                  break;
              case ENOENT:
                  cerr << argv[0] << " is not found" << endl;
                  break;
              case ENOEXEC:
                  cerr << argv[0];
                  cerr << "  is not executable or has invalid executable-file format" << endl;
                  break;
              case ENOMEM:
                  cerr << "Not enough resources" << endl;
                  break;
              default:
                  cerr << "Unknown error" << endl;
                  break;
              }
              for( i=0; i<argc; i++ )
                  delete [] argv[i];
              delete [] mutable_string;
              exit(-1);
          }
  #ifndef NDEBUG
          cout << "Successfully spawned a co-process as a node: " << coNodeCommand << endl;
  #endif
          for( i=0; i<argc; i++ )
              delete [] argv[i];
          delete [] mutable_string;
        }
    }
    if( objective == NAIVE_FIRST_ORDER )
    {
      NaiveFoModel foModel ( &model, indParStep );
      ppkaOpt( 
            foModel,
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
            popObjOut,
            pdrowLTilde_alpOut,
            pdmatLTilde_alp_alpOut,
            isMultiple,
            c_sharedDirectory,
            coNodeCommand
          );
    }
    else
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
            popObjOut,
            pdrowLTilde_alpOut,
            pdmatLTilde_alp_alpOut,
            isMultiple,
            c_sharedDirectory,
            coNodeCommand
          );
    }
  }

  //------------------------------------------------------------
  // Issue warning messages for parameters that are constrained.
  //------------------------------------------------------------

  // Check for population level parameters that are constrained.
  checkPopPar( popOptimizer, dvecAlpLow, dvecAlpUp, dvecAlpOut );

  // Check for individual level parameters that are constrained.
  checkIndPar( indOptimizer, dvecBLow, dvecBUp, dmatBOut );

  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Convert results to valarray
  if( popParOut )
  {
      (*pdvecAlpOut).toValarray( (*popParOut) );
  }
  if( indParAllOut )
  {
	  (*pdmatBOut).toValarray( (*indParAllOut) );
  }
  if( popObj_popParOut )
  {
	  (*pdrowLTilde_alpOut).toValarray( (*popObj_popParOut) );
  }
  if( popObj_popPar_popParOut )
  {
      (*pdmatLTilde_alp_alpOut).toValarray( (*popObj_popPar_popParOut) );
  }
  if(isMultiple)
    broadCastEndOfSpk(sharedDirectory);
}


/*========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 *========================================================================*/

namespace // [Begin: unnamed namespace]
{

/*************************************************************************
 *
 * Function: checkPopPar
 *
 *
 * Checks the vector of output population parameters to see if any of
 * its elements is constrained by its corresponding lower or upper
 * limit but not by both.
 *
 *************************************************************************/

void checkPopPar(
  const Optimizer&    popOptimizer,
  const DoubleMatrix& dvecAlpLow,
  const DoubleMatrix& dvecAlpUp,
  const DoubleMatrix& dvecAlpOut )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  const double* pdAlpLowData = dvecAlpLow.data();
  const double* pdAlpUpData  = dvecAlpUp .data();
  const double* pdAlpOutData = dvecAlpOut.data();

  int nAlp   = dvecAlpOut.nr();


  //------------------------------------------------------------
  // Check the parameters to see if any are constrained.
  //------------------------------------------------------------

  // Prepare a warning message that will only be issued if there
  // are constrained parameters.
  ostringstream warning;

  int k;
  double maxDistFromBound_k;

  int colWidth1 = 9 - 2;
  int colWidth2 = 12 + 2;
  int colWidth3 = 12;
  string colSpacer = "  ";

  warning << "The following population parameters are at or near their bounds." << endl;
  warning << endl;
  warning << "Parameter      Value            Bound"      << endl;
  warning << "---------  --------------  ---------------" << endl;

  // Check the final population parameter value to see if it 
  // is constrained by its lower or upper bound;
  bool isAnyAlpAtOrNearLimit = false;
  for ( k = 0; k < nAlp; k++ )
  {
    // Don't give a warning if the value is constrained by both
    // of its bounds.
    if ( pdAlpLowData[k] != pdAlpUpData[k] )
    {
      // Set the maximum distance allowed from either bound.
      maxDistFromBound_k = 
        popOptimizer.getEpsilon() * ( pdAlpUpData[k] - pdAlpLowData[k] );

      // Give a warning if the value is within the maximum distance of
      // either bound.
      if ( pdAlpOutData[k] - pdAlpLowData[k] <= maxDistFromBound_k ||
           pdAlpUpData[k]  - pdAlpOutData[k] <= maxDistFromBound_k )
      {
        isAnyAlpAtOrNearLimit = true;
    
        // Column 1.
        warning << setw( colWidth1 ) << k + 1 << colSpacer;
    
        // Column 2.
        warning << setw( colWidth2 ) << scientific 
                << setprecision( 2 ) << pdAlpOutData[k] << colSpacer;
    
        // Column 3.
        warning << colSpacer << colSpacer << setw( colWidth3 );
        if ( pdAlpOutData[k] == pdAlpLowData[k] )
        {
          warning << "Lower (at)  ";
        }
        else if ( pdAlpOutData[k] == pdAlpUpData[k] )
        {
          warning << "Upper (at)  ";
        }
        else if ( pdAlpUpData[k] - pdAlpLowData[k] <= maxDistFromBound_k ) 
        {
          warning << "Both (near) ";
        }
        else if ( pdAlpOutData[k] - pdAlpLowData[k] <= maxDistFromBound_k )
        {
          warning << "Lower (near)";
        }
        else
        {
          warning << "Upper (near)";
        }

        warning << endl;
      }
    }
  }


  //------------------------------------------------------------
  // Issue a warning message if necessary.
  //------------------------------------------------------------

  // Only issue the warning message if at least one of the
  // values is constrained.
  if ( isAnyAlpAtOrNearLimit )
  {
    string warningStr = warning.str();
    WarningsManager::addWarning( warningStr, __LINE__, __FILE__);
  }
}


/*************************************************************************
 *
 * Function: checkIndPar
 *
 *
 * Checks the matrix of output individual parameters to see if any of
 * its elements is constrained by its corresponding lower or upper
 * limit but not by both.
 *
 *************************************************************************/

void checkIndPar(
  const Optimizer&    indOptimizer,
  const DoubleMatrix& dvecBLow,
  const DoubleMatrix& dvecBUp,
  const DoubleMatrix& dmatBOut )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  const double* pdBLowData = dvecBLow.data();
  const double* pdBUpData  = dvecBUp .data();
  const double* pdBOutData = dmatBOut.data();

  int nB   = dmatBOut.nr();
  int nInd = dmatBOut.nc();


  //------------------------------------------------------------
  // Check the parameters to see if any are constrained.
  //------------------------------------------------------------

  // Prepare a warning message that will only be issued if there
  // are constrained parameters.
  ostringstream warning;

  int i;
  int k;
  double bOut_i_k;
  double maxDistFromBound_k;

  int colWidth1 = 10 - 2;
  int colWidth2 = 9;
  int colWidth3 = 13 + 2;
  int colWidth4 = 12;
  string colSpacer = "  ";

  warning << "The following individual parameters are at or near their bounds." << endl;
  warning << endl;
  warning << "Individual  Parameter       Value            Bound"      << endl;
  warning << "----------  ---------  ---------------  ---------------" << endl;

  // Check each individual's final parameter value to see if they
  // are constrained by their lower or upper bound;
  bool isAnyBAtOrNearLimit = false;
  bool printIndex;
  for ( i = 0; i < nInd; i++ )
  {
    printIndex = true;

    for ( k = 0; k < nB; k++ )
    {
      // Don't give a warning if the value is constrained by both
      // of its bounds.
      if ( pdBLowData[k] != pdBUpData[k] )
      {
        // Set the maximum distance allowed from either bound.
        maxDistFromBound_k = 
          indOptimizer.getEpsilon() * ( pdBUpData[k] - pdBLowData[k] );
    
        bOut_i_k = pdBOutData[k + i * nB];
    
        // Give a warning if the value is within the maximum distance of
        // either bound.
        if ( bOut_i_k     - pdBLowData[k] <= maxDistFromBound_k ||
             pdBUpData[k] - bOut_i_k      <= maxDistFromBound_k )
        {
          isAnyBAtOrNearLimit = true;
    
          // Column 1.
          warning << setw( colWidth1 );
          if ( printIndex )
          {
            warning << i + 1;
          }
          else
          {
            warning << "";
          }
          warning << colSpacer;
    
          // Column 2.
          warning << setw( colWidth2 ) << k + 1 << colSpacer;
    
          // Column 3.
          warning << setw( colWidth3 ) << scientific 
                  << setprecision( 3 ) << bOut_i_k << colSpacer;
    
          // Column 4.
          warning << colSpacer << colSpacer << setw( colWidth4 );
          if ( bOut_i_k == pdBLowData[k] )
          {
            warning << "Lower (at)  ";
          }
          else if ( bOut_i_k == pdBUpData[k] )
          {
            warning << "Upper (at)  ";
          }
          else if ( pdBUpData[k] - pdBLowData[k] <= maxDistFromBound_k ) 
          {
            warning << "Both (near) ";
          }
          else if ( bOut_i_k - pdBLowData[k] <= maxDistFromBound_k )
          {
            warning << "Lower (near)";
          }
          else
          {
            warning << "Upper (near)";
          }
    
          warning << endl;
    
          printIndex = false;
        }
      }
    }
  }


  //------------------------------------------------------------
  // Issue a warning message if necessary.
  //------------------------------------------------------------

  // Only issue the warning message if at least one of the
  // values is constrained.
  if ( isAnyBAtOrNearLimit )
  {
    string warningStr = warning.str();
    WarningsManager::addWarning( warningStr, __LINE__, __FILE__);
  }
}

} // [End: unnamed namespace]

