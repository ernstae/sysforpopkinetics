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
 * File: Optimizer.cpp
 *
 *
 * A wrapper for optimizer substitution and control.
 *
 * Author: Jiaji Du
 *
 * Modified later by: Sachiko Honda
 * Modified later by: Mitch Watrous
 *
 *************************************************************************/

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// [Revisit - Optimizer Class Should be Renamed - Mitch]
//
// Since this class only contains optimizer control parameters,
// it is not actually an optimizer itself.  Its name should be
// changed to something that is more accurate.  For example,
//
//     OptimizerInformation    OptInformation    OptInfo
//     OptimizerController     OptController     OptControl
//     OptimizerManager        OptManager        OptMan
//
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// [Revisit - Optimizer Class Should be Generalized - Mitch]
//
// This class should be generalized so that it is reusable and 
// doesn't have to be modified everytime the optimizer is changed
// and/or a new optimizer is added to SPK.
//
// This coulde be done by making this be an abstract base class with
// its concrete subclasses providing the optimizer specific parts or
// by making it be a templated class that takes the optimizer specific
// parts as argument(s).  An abstract base class might be the better
// solution since each optimizer will have its own convergence criteria.
//
// Either way, the goals would be to remove information from this 
// class that is specific to a particular optimizer and to allow the
// user of this class to provide that information plus anything else
// required by their optimizer.
//
/// The information this class contains that is currently optimizer
// specific is the state information used for warm starts. An example
// functionality of an optimizer that is optimizer specific is the
// way it determines if it has converged using the elements of this
// class.
//
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


/*************************************************************************
 *
 * Class: Optimizer
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/
/*
-------------------------------------------------------------
   Default Constructor 
-------------------------------------------------------------
$begin OptimizerDefaultConstructor$$

$spell const int optimizer NMaxIter initializes Bayesian stdout mapObj
 struct Vi Varbl$$

$section Default Constructor$$

$index Optimizer, default constructor$$

$table
$bold Prototype:$$ $cend
$syntax/Optimizer::Optimizer()/$$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The default constructor.  It constructs an object of $code Optimizer$$ with
the following default values:
$table
$syntax//epsilon//$$ $cend
0.0001 $rend
$syntax//nMaxIter//$$ $cend
40 $rend
$syntax//traceLevel//$$ $cend
1 $rend
$syntax//nIterCompleted//$$ $cend
0 $rend
$syntax//isTooManyIter//$$ $cend
false $rend
$syntax//saveStateAtEndOfOpt//$$ $cend
false $rend
$syntax//throwExcepIfMaxIter//$$ $cend
true $rend
$syntax//isWarmStartPossible//$$ $cend
false $rend
$syntax//isWarmStart//$$ $cend
false $rend
$syntax//didOptFinishOk//$$ $cend
false $rend
$syntax//isBeginOfIterStateInfo//$$ $cend
false $rend
$tend
and setting all the element pointers of $code StateInfo$$ to $code NULL$$.
The meanings of these parameters are explained under
$tref OptimizerConstructor$$.
Note that these default values do not constitute a recommendation.

$head Arguments$$
N/A

$head Example$$
There is no example for this class.
$end
*/

/*
-------------------------------------------------------------
   Three Arguments Constructor 
-------------------------------------------------------------
$begin OptimizerConstructor$$

$spell const int nlp optimizer NMaxIter initializes Bayesian stdout mapObj
 sub Vi Varbl$$

$section Constructor$$

$index Optimizer, constructor$$

$table
$bold Prototype:$$ $cend
$syntax/Optimizer::Optimizer(double /epsilon/, int /nMaxIter/, int /traceLevel/)/$$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
A constructor that takes three argument values and
set the current state of the object to the given information.

$head Arguments$$
$syntax/
/epsilon/
/$$
This real number is used to specify the convergence criteria
for the optimizer.
It must be greater than $math%0.0%$$.

$syntax/

/nMaxIter/
/$$
This integer must be greater than or equal to zero.
It specifies the maximum number of 
iterations to attempt before giving up on convergence.

$syntax/

/traceLevel/
/$$
This integer scalar specifies the amount of tracing.
Larger values of $italic traceLevel$$ entail more tracing.

$head Other data members$$
$syntax/

/nIterCompleted/
/$$
This integer scalar holds the number of iteration that have been 
completed in the optimizer.  The initial value of $code  0$$ is set 
at the construction time.

$syntax/

/isTooManyIter/
/$$
This flag indicates whether the too-many-iteration failure has occurred.  
It is set to $code false$$ at the construction time.

$syntax/

/saveStateAtEndOfOpt/
/$$
This flag indicates if the state information required for a warm start
should be saved at the end of the optimization process.
It is set to $code false$$ at the construction time.

$syntax/

/throwExcepIfMaxIter/
/$$
This flag indicates if the optimizer should throw an exception when
the maximum number of iterations is exhausted.
It is set to $code true$$ at the construction time.

$syntax/

/isWarmStartPossible/
/$$
This flag indicates whether it is possible to perform a warm start 
using the current optimizer state information.
It is set to $code false$$ at the construction time.

$syntax/

/isWarmStart/
/$$
This flag indicates whether the optimization should run a warm start.  
It is set to $code false$$ at the construction time.

$syntax/

/didOptFinishOk/
/$$
This flag indicates whether the optimizer completed without an 
error occurring during the previous time it was called.
It is set to $code false$$ at the construction time.

$syntax/

/isBeginOfIterStateInfo/
/$$
This flag indicates whether the current optimizer state information 
is from the beginning of the last iteration during the previous 
time the optimizer was called.
It is set to $code false$$ at the construction time.

$syntax/

/stateInfo/
/$$
This $code stateInfo$$ structure contains the optimizer state information
required to perform a warm start.
Each of its elements is described separately below.

$subhead stateInfo.n$$
The element $italic n$$ specifies the number of components
in the element vector $italic x$$.

$subhead stateInfo.b$$
The element $italic b$$ specifies the number of Bfgs updates
that have been made to the Hessian approximation $italic h$$.

$subhead stateInfo.r$$
The element $italic r$$ contains the current trust region radius
(as an infinity norm bound on the step size).

$subhead stateInfo.f$$
The element $italic f$$ contains the value for $math%f(x)%$$
at the point $math%x%$$.

$subhead stateInfo.x$$
The element $italic x$$ is a vector of length $italic n$$.
It specifies the point at which the objective function, 
its gradient, and its Hessian were evaluated.

$subhead stateInfo.g$$
The vector $italic g$$ must have length $math%n%$$.
It contains the gradient of $math%f(x)%$$
at the point $math%x%$$.

$subhead stateInfo.h.$$
The vector $italic h$$ must have length $math%n^2%$$.
It contains an approximation for the Hessian of $math%f(x)%$$
at the point $math%x%$$.

$head Example$$
There is no example for this class.
$end
*/

/*
-------------------------------------------------------------
   Destructor
-------------------------------------------------------------
$begin OptimizerDestructor$$

$spell 
  destructor 
  destructs 
  optimizer
  const  
$$

$section Destructor$$

$index Optimizer, destructor$$

$table
$bold Prototype: $$ $cend
$syntax/Optimizer::~Optimizer()/$$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Frees memory allocated to hold the optimizer state information used
in warm starts.

$head Example$$
There is no example for this class.

$end

*/
/*
-------------------------------------------------------------
   Copy constructor
-------------------------------------------------------------
$begin OptimizerCopyConstructor $$

$spell 
  Optimizer 
  Iter
  const
$$

$section Copy Constructor$$

$index Optimizer, copy constructor$$

$table
$bold Prototype:$$ $cend
$syntax/Optimizer::Optimizer(const Optimizer& /original/)/$$
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Constructs a new $code Optimizer$$ object identical to $italic original$$.
Only shallow copy is implemented. 

$head Example$$
There is no example for this class.

$end
*/

/*
-------------------------------------------------------------
   Assignment operator
-------------------------------------------------------------
$begin AssignmentOperator$$

$spell
  const int const Optimizer Iter
$$

$section Assignment operator$$

$index Optimizer, assignment operator$$

$table
$bold Prototype$$ $cend
$syntax/Optimizer& Optimizer::operator=(const Optimizer &/right/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
(Deep) copies information from $italic right$$ to $italic *this$$ and
returns $italic this$$ (where $italic *this$$ is the $code Optimizer$$
object on the left side of the = sign).  Only shallow copy is implemented.

$head Example$$
There is no example for this class.

$end
*/

/* 
-------------------------------------------------------------
   Get the convergence tolerance epsilon
-------------------------------------------------------------
$begin getEpsilon$$

$spell
  getEpsilon epsilon Optimizer
    const
$$

$section Get convergence tolerance epsilon$$

$index Optimizer, epsilon, getEpsilon$$

$table
$bold Prototype$$ $cend
$syntax/double Optimizer::getEpsilon() const/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code getEpsilon()$$ returns the same exact value 
as $italic epsilon$$ at the construction time or the most recent value 
altered via $code setEpsilon()$$.

$head Example$$
There is no example for this class.

$end
*/


/* 
-------------------------------------------------------------
   Get the maximum number of iterations
-------------------------------------------------------------
$begin getNMaxIter$$

$spell
  getNMaxIter nMaxIter int Optimizer
$$

$section Get maximum number of iterations$$

$index Optimizer, NMaxIter, getNMaxIter$$

$table
$bold Prototype$$ $cend
$syntax/int Optimizer::getNMaxIter()/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code getNMaxIter()$$ returns the same exact value as $italic nMaxIter$$
given at the construction time or the most recent value altered via
$code setNMaxIter()$$.

$head Example$$
There is no example for this class.

$end
*/


/* 
-------------------------------------------------------------
   Get the level of tracing
-------------------------------------------------------------
$begin getLevel$$

$spell
  getLevel level int Optimizer
    const
$$

$section Get tracing level$$

$index Optimizer, level, getLevel$$

$table
$bold Prototype$$ $cend
$syntax/int Optimizer::getLevel() const/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code getLevel()$$ returns the same exact value given as
$italic traceLevel$$ at the construction time or the most recent value 
altered via $code setLevel()$$.

$head Example$$
There is no example for this class.
$end
*/


/* 
-------------------------------------------------------------
   Get the number of iterations completed
-------------------------------------------------------------
$begin getNIterCompleted$$

$spell
  getNIterCompleted int Optimizer
    const
$$

$section Get number of iterations completed$$

$index Optimizer, nIterCompleted, getNIterCompleted$$

$table
$bold Prototype$$ $cend
$syntax/int Optimizer::getNIterCompleted() const/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code getNIterCompleted()$$ returns the value of $italic nIterCompleted$$
either $code 0$$ given at the construction time or the most recent value 
altered via $code setNIterCompleted()$$.

$head Example$$
There is no example for this class.
$end
*/


/* 
-------------------------------------------------------------
   Get the is-too-many iterations flag
-------------------------------------------------------------
$begin getIsTooManyIter$$

$spell
  getIsTooManyIter bool Optimizer
    const
$$

$section Get is-too-many iterations flag$$

$index Optimizer, isTooManyIter, getIsTooManyIter$$

$table
$bold Prototype$$ $cend
$syntax/bool Optimizer::getIsTooManyIter() const/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code getIsTooManyIter()$$ returns the value of $italic isTooManyIter$$
either $code false$$ given at the construction time or the most recent 
value altered via $code setIsTooManyIter()$$.

$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Get the save state at end of optimization flag
-------------------------------------------------------------
$begin getSaveStateAtEndOfOpt$$

$spell
  getSaveStateAtEndOfOpt bool Optimizer
$$

$section Get Save State at End of Optimization Flag$$

$index Optimizer, state, getSaveStateAtEndOfOpt$$

$table
$bold Prototype$$ $cend
$syntax/bool Optimizer::getSaveStateAtEndOfOpt() const/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code getSaveStateAtEndOfOpt()$$ returns the value of $italic saveStateAtEndOfOpt$$
either $code false$$ given at the construction time or the most recent 
value altered via $code setSaveStateAtEndOfOpt()$$.


$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Get the thrown exception if maxIter is exhausted flag
-------------------------------------------------------------
$begin getThrowExcepIfMaxIter$$

$spell
  getThrowExcepIfMaxIter bool Optimizer
$$

$section Get Throw Exception if Maximum Iterations Exhausted Flag$$

$index Optimizer, maximum iterations, getThrowExcepIfMaxIter$$

$table
$bold Prototype$$ $cend
$syntax/bool Optimizer::getThrowExcepIfMaxIter() const/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code getThrowExcepIfMaxIter()$$ returns the value of $italic throwExcepIfMaxIter$$
either $code true$$ given at the construction time or the most recent 
value altered via $code setThrowExcepIfMaxIter()$$.


$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Get the is warm start possible flag
-------------------------------------------------------------
$begin getIsWarmStartPossible$$

$spell
  getIsWarmStartPossible bool Optimizer
    const 
$$

$section Get is warm start possible flag$$

$index Optimizer, is warm start possible, isWarmStartPossible, getIsWarmStartPossible$$

$table
$bold Prototype$$ $cend
$syntax/bool Optimizer::getIsWarmStartPossible() const/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code getIsWarmStartPossible()$$ returns the value of $italic
isWarmStartPossible$$ either $code false$$ given at the construction
time or the most recent value altered via $code setIsWarmStartPossible()$$.

$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Get the warm start flag
-------------------------------------------------------------
$begin getIsWarmStart$$

$spell
  getIsWarmStart bool Optimizer
    const 
$$

$section Get warm start flag$$

$index Optimizer, warm start, isWarmStart, getIsWarmStart$$

$table
$bold Prototype$$ $cend
$syntax/bool Optimizer::getIsWarmStart() const/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code getIsWarmStart()$$ returns the value of $italic isWarmStart$$
either $code false$$ given at the construction time or the most recent 
value altered via $code setIsWarmStart()$$.

$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Get the did the optimizer finish ok flag
-------------------------------------------------------------
$begin getDidOptFinishOk$$

$spell
  getDidOptFinishOk bool Optimizer
    const 
$$

$section Get the did the optimizer finish ok flag$$

$index Optimizer, optimizer finish ok, didOptFinishOk, getDidOptFinishOk$$

$table
$bold Prototype$$ $cend
$syntax/bool Optimizer::getDidOptFinishOk() const/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code getDidOptFinishOk()$$ returns the value of $italic didOptFinishOk$$
either $code false$$ given at the construction time or the most recent 
value altered via $code setDidOptFinishOk()$$.

$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Get the is beginning of iteration state information flag
-------------------------------------------------------------
$begin getIsBeginOfIterStateInfo$$

$spell
  getIsBeginOfIterStateInfo bool Optimizer
    const 
$$

$section Get the is beginning of iteration state information flag$$

$index Optimizer, beginning of iteration state, isBeginOfIterStateInfo, getIsBeginOfIterStateInfo$$

$table
$bold Prototype$$ $cend
$syntax/bool Optimizer::getIsBeginOfIterStateInfo() const/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code getIsBeginOfIterStateInfo()$$ returns the value of $italic isBeginOfIterStateInfo$$
either $code false$$ given at the construction time or the most recent 
value altered via $code setIsBeginOfIterStateInfo()$$.

$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Set the convergence tolerance epsilon
-------------------------------------------------------------
$begin setEpsilon$$

$spell
  const setEpsilon epsilon double Optimizer
$$

$section Set convergence tolerance epsilon$$

$index Optimizer, epsilon, setEpsilon$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::setEpsilon(double /e/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code setEpsilon()$$ sets $italic epsilon$$ as 
the convergence tolerance epsilon.

$head Example$$
There is no example for this class.

$end
*/


/* 
-------------------------------------------------------------
   Set the maximum number of iterations
-------------------------------------------------------------
$begin setNMaxIter$$

$spell
  const setNMaxIter nMaxIter int Optimizer
$$

$section Set maximum number of iterations$$

$index Optimizer, NMaxIter, setNMaxIter$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::setNMaxIter(int /n/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code setNMaxIter()$$ sets $italic nMaxIter$$ as 
the maximum number of iterations.

$head Example$$
There is no example for this class.

$end
*/


/* 
-------------------------------------------------------------
   Set the level of tracing
-------------------------------------------------------------
$begin setLevel$$

$spell
  const setLevel level int Optimizer
$$

$section Set level of tracing$$

$index Optimizer, level, setLevel$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::setLevel(int /e/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code setLevel()$$ sets $italic level$$ as 
the level of tracing.

$head Example$$
There is no example for this class.

$end
*/


/* 
-------------------------------------------------------------
   Set the number of iterations completed
-------------------------------------------------------------
$begin setNIterCompleted$$

$spell
  setNIterCompleted int Optimizer
    const
$$

$section Set number of iterations completed$$

$index Optimizer, nIterCompleted, setNIterCompleted$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::setNIterCompleted(int /n/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code setNIterCompleted()$$ sets the value $italic nIterCompleted$$
as the number of iterations completed.

$head Example$$
There is no example for this class.
$end
*/


/* 
-------------------------------------------------------------
   Set the is-too-many iterations flag
-------------------------------------------------------------
$begin setIsTooManyIter$$

$spell
  setIsTooManyIter bool Optimizer
    const
$$

$section Set is-too-many iterations flag$$

$index Optimizer, isTooManyIter, setIsTooManyIter$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::setIsTooManyIter(bool /b/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code setIsTooManyIter()$$ sets the value $italic isTooManyIter$$
as a flag to indicate whether too-many-iteration failure has occurred.

$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Set the save state at end of optimization flag
-------------------------------------------------------------
$begin setSaveStateAtEndOfOpt$$

$spell
  setSaveStateAtEndOfOpt bool Optimizer
$$

$section Set Save State at End of Optimization Flag$$

$index Optimizer, state, setSaveStateAtEndOfOpt$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::setSaveStateAtEndOfOpt(bool /b/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code setSaveStateAtEndOfOpt()$$ sets the value $italic saveStateAtEndOfOpt$$
as a flag to indicate if the state information required for a warm start
should be saved at the end of the optimization process.


$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Set the thrown exception if maxIter is exhausted flag
-------------------------------------------------------------
$begin setThrowExcepIfMaxIter$$

$spell
  setThrowExcepIfMaxIter bool Optimizer
$$

$section Set Throw Exception if Maximum Iterations Exhausted Flag$$

$index Optimizer, throwExceptionIfMaxIter, setThrowExcepIfMaxIter$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::setThrowExcepIfMaxIter(bool /b/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code setThrowExcepIfMaxIter()$$ sets the value $italic throwExceptionIfMaxIter$$
as a flag to indicate if the optimizer should throw an exception when
the maximum number of iterations is exhausted.

$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Set the is warm start possible flag
-------------------------------------------------------------
$begin setIsWarmStartPossible$$

$spell
  setIsWarmStartPossible bool Optimizer
    const
$$

$section Set the is warm start possible flag$$

$index Optimizer, warm startPossible, isWarmStartPossible, setIsWarmStartPossible$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::setIsWarmStartPossible(bool /b/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$

$code setIsWarmStartPossible()$$ sets the value 
$italic isWarmStartPossible$$ as a flag to indicate whether
it is possible to perform a warm start using the current 
optimizer state information.

$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Set the is a warm start flag
-------------------------------------------------------------
$begin setIsWarmStart$$

$spell
  setIsWarmStart bool Optimizer
    const
$$

$section Set the is a warm start flag$$

$index Optimizer, warm start, isWarmStart, setIsWarmStart$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::setIsWarmStart(bool /b/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code setIsWarmStart()$$ sets the value $italic isWarmStart$$
as a flag to indicate whether the optimization should run a warm start.

$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Set the did the optimizer finish ok flag
-------------------------------------------------------------
$begin setDidOptFinishOk$$

$spell
  setDidOptFinishOk bool Optimizer
    const
$$

$section Set the did the optimizer finish ok flag$$

$index Optimizer, optimizer finish ok, didOptFinishOk, setDidOptFinishOk$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::setDidOptFinishOk(bool /b/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code setDidOptFinishOk()$$ sets the value $italic didOptFinishOk$$
as a flag to indicate whether the optimizer completed without an 
error occurring during the previous time it was called.

$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Set the is beginning of iteration state information flag
-------------------------------------------------------------
$begin setIsBeginOfIterStateInfo$$

$spell
  setIsBeginOfIterStateInfo bool Optimizer
    const
$$

$section Set the is beginning of iteration state information flag$$

$index Optimizer, beginning of iteration state, isBeginOfIterStateInfo, setIsBeginOfIterStateInfo$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::setIsBeginOfIterStateInfo(bool /b/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
$code setIsBeginOfIterStateInfo()$$ sets the value 
$italic isBeginOfIterStateInfo$$ as a flag to indicate the current 
optimizer state information is from the beginning of the last 
iteration during the previous time the optimizer was called.

$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Get the state information for warm start
-------------------------------------------------------------
$begin getStateInfo$$

$spell
  getStateInfo stateInfo Optimizer
    const nlp
$$

$section Get state information for a warm start$$

$index Optimizer, state optimization, getStateInfo$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::getStateInfo(
  int            /nIn/,
  size_t         /bOut/,
  double         /rOut/,
  double         /fOut/,
  double* const  /xOut/,
  double* const  /gOut/,
  double* const  /hOut/ )
/$$
$rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This function gets state information required for a later warm start.

$head Arguments$$
$syntax/
/nIn/
/$$
The argument $italic nIn$$ specifies the number of components
in the argument vector $italic xOut$$.

$syntax/

/bOut/
/$$
The argument $italic bOut$$ will be set equal to the number of 
Bfgs updates that have been made to the Hessian approximation 
$italic hOut$$.

$syntax/

/rOut/
/$$
The argument $italic rOut$$ will be set equal to the current 
trust region radius (as an infinity norm bound on the step size).

$syntax/

/fOut/
/$$
The argument $italic fOut$$ will be set equal to the value for
$math%f(x)%$$ at the point $math%xOut%$$.

$syntax/

/xOut/
/$$
The array pointed to by $italic xOut$$ must have length 
$italic nIn$$.
It will be set equal to the point at which the objective 
function, its gradient, and its Hessian were evaluated.

$syntax/

/gOut/
/$$
The array pointed to by $italic gOut$$ must have length 
$italic nIn$$.
It will be set equal to the gradient of $math%f(x)%$$
at the point $math%xOut%$$.

$syntax/

/hOut/
/$$
The array pointed to by $italic hOut$$ must have length 
$italic nIn$$ * $italic nIn$$.
It will be set equal to an approximation for the Hessian 
of $math%f(x)%$$ at the point $math%xOut%$$.

$head Example$$
There is no example for this class.
$end
*/


/* 
-------------------------------------------------------------
   Set state information for warm start
-------------------------------------------------------------
$begin setStateInfo$$

$spell
  setStateInfo Optimizer
    Integer const
$$

$section Set state information for a warm start$$

$index Optimizer, warm start, state information, setStateInfo$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::setStateInfo(
  int                  /nIn/,
  size_t               /bIn/,
  double               /rIn/,
  double               /fIn/,
  const double* const  /xIn/,
  const double* const  /gIn/,
  const double* const  /hIn/,
  int                  /mIn/,
  const double* const  /lowIn/,
  const double* const  /upIn/,
  const int*    const  /posIn/ )
/$$
$rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This function sets state information required for a later warm start.

$head Arguments$$
$syntax/
/nIn/
/$$
The argument $italic nIn$$ specifies the number of free objective
function parameters.

$syntax/

/bIn/
/$$
The argument $italic bIn$$ specifies the number of Bfgs updates
that have been made to the Hessian approximation $italic hIn$$.

$syntax/

/rIn/
/$$
The argument $italic rIn$$ contains the current trust region radius
(as an infinity norm bound on the step size).

$syntax/

/fIn/
/$$
The argument $italic fIn$$ contains the value for $math%f(x)%$$
at the point $math%xIn%$$.

$syntax/

/xIn/
/$$
The array pointed to by $italic xIn$$ must have length 
$italic nIn$$.
It specifies the point at which the objective function, 
its gradient, and its Hessian were evaluated.

$syntax/

/gIn/
/$$
The array pointed to by $italic gIn$$ must have length 
$italic nIn$$.
It contains the gradient of $math%f(x)%$$
at the point $math%xIn%$$.

$syntax/

/hIn/
/$$
The array pointed to by $italic hIn$$ must have length 
$italic nIn$$ * $italic nIn$$.
It contains an approximation for the Hessian of $math%f(x)%$$
at the point $math%xIn%$$.

$syntax/
/mIn/
/$$
The argument $italic mIn$$ specifies the total number of objective 
function parameters, i.e., the number of free parameters plus the 
number of parameters that are constrained by both their lower and 
upper bounds.

$syntax/

/lowIn/
/$$
The array pointed to by $italic lowIn$$ must have length 
$italic mIn$$.
It specifies the lower bounds for all of the objective function 
parameters in their original coordinates.

$syntax/

/upIn/
/$$
The array pointed to by $italic upIn$$ must have length 
$italic mIn$$.
It specifies the upper bounds for all of the objective function 
parameters in their original coordinates.

$syntax/

/posIn/
/$$
The array pointed to by $italic posIn$$ must have length 
$italic nIn$$.
It specifies the positions, i.e., indices, of the free objective 
function parameters in the full objective function parameter.

$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Determine if there is any error information
-------------------------------------------------------------
$begin isThereErrorInfo$$

$spell
  isThereErrorInfo bool Optimizer
    const 
$$

$section Determine if there is error information$$

$index Optimizer, error information, isThereErrorInfo$$

$table
$bold Prototype$$ $cend
$syntax/bool Optimizer::isThereErrorInfo() const/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Returns true if there was an error during the previous call to the
optimizer and if there is state information from the beginning of the
iteration that caused the error.

$head Example$$
There is no example for this class.
$end
*/

/* 
-------------------------------------------------------------
   Get the error information
-------------------------------------------------------------
$begin getErrorInfo$$

$spell
  getErrorInfo errorInfo Optimizer
    const nlp
$$

$section Get error information$$

$index Optimizer, error information, getErrorInfo$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::getErrorInfo( 
  const std::string  /headerStr/,
  std::string&       /messageStr/,
  unsigned int       /lineNumber/,
  const char*        /fileName/ )
/$$
$rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$

This function gets any error information that is available and formats
it in the same form as the $xref/SpkError//SpkError/$$ serialize function.

$head Arguments$$
$syntax/
/headerStr/
/$$
This string is added to the beginning of the error message before
it is formatted.

$syntax/

/messageStr/
/$$
This string will contain the formatted error message.
It will be set equal to an empty string if there is no error
information.

$syntax/

/lineNumber/
/$$
This is the line number in the file where the error was generated.

$syntax/

/fileName/
/$$
This is the name of the file where the error was generated.

$head Example$$
There is no example for this class.
$end
*/

/*
-------------------------------------------------------------
   The inserter
-------------------------------------------------------------
$begin OptimizerInserter$$

$spell
  inserter ostream os cout const ob
    Iter Optimizer
$$

$section Inserter <<$$

$index Optimizer, operator<<$$
$index output the optimizer to ostream$$

$table
$bold Prototype$$ $cend
$syntax/friend ostream& operator<<(ostream& /os/, const Optimizer& /ob/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The operator inserts a $code Optimizer$$ object, $italic ob$$, to the left hand side
$code ostream&$$, $italic os$$, in the format shown below.
(where each $code xxx$$ corresponds to an actual value).
$codep

    epsilon
    xxx
    nMaxIter
    xxx
    level
    xxx
    isTooManyIter
    xxx
    nIterCompleted
    xxx

$$
$end
*/


/*
-------------------------------------------------------------
   The extractor
-------------------------------------------------------------
$begin OperatorExtractor$$

$spell
  cout ob istream ostream
    Spk Optimizer
$$

$section Extractor >>$$

$index Optimizer, operator>>$$
$index extract an optimizer from istream$$

$table
$bold Prototype$$ $cend
$syntax/friend istream& operator>>(istream& /is/, Optimizer& /ob/)/$$ $rend
$tend

$fend 20$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
The operator extracts an $code Optimizer$$ object from $italic is$$ and 
puts it into $italic ob$$.  $italic is$$ is assumed to have the same exact 
format as an Optimizer object inserted to ostream by $code operator<<$$.
If the format is wrong, this throws a $xref/SpkException//SpkException/$$ 

$end
*/


#include "Optimizer.h"
#include "SpkException.h"
#include <spkopt/Memory.h>
#include <iomanip>
#include <iostream>
#include <sstream> 
#include <string> 

// Default constructor
//Goddard review 9/17/02: Default values should be reasonable; "arbitrary" isn't useful.
Optimizer::Optimizer()
  :
  epsilon                 ( 0.0001 ),
  nMaxIter                ( 40 ),
  level                   ( 1 ),
  nIterCompleted          ( 0 ),
  isTooManyIter           ( false ),
  saveStateAtEndOfOpt     ( false ),
  throwExcepIfMaxIter     ( true ),
  isWarmStartPossible     ( false ),
  isWarmStart             ( false ),
  didOptFinishOk          ( false ),
  isBeginOfIterStateInfo  ( false )
{
  stateInfo.n   = 0;
  stateInfo.b   = 0;
  stateInfo.r   = 0;
  stateInfo.f   = 0;
  stateInfo.x   = 0;
  stateInfo.g   = 0;
  stateInfo.h   = 0;
  stateInfo.m   = 0;
  stateInfo.low = 0;
  stateInfo.up  = 0;
  stateInfo.pos = 0;
}

// Constructor
Optimizer::Optimizer( double Epsilon, int NMaxIter, int Level )
  :
  epsilon                 ( Epsilon ),
  nMaxIter                ( NMaxIter ),
  level                   ( Level ),
  nIterCompleted          ( 0 ),
  isTooManyIter           ( false ),
  saveStateAtEndOfOpt     ( false ),
  throwExcepIfMaxIter     ( true ),
  isWarmStartPossible     ( false ),
  isWarmStart             ( false ),
  didOptFinishOk          ( false ),
  isBeginOfIterStateInfo  ( false )
{
  stateInfo.n   = 0;
  stateInfo.b   = 0;
  stateInfo.r   = 0;
  stateInfo.f   = 0;
  stateInfo.x   = 0;
  stateInfo.g   = 0;
  stateInfo.h   = 0;
  stateInfo.m   = 0;
  stateInfo.low = 0;
  stateInfo.up  = 0;
  stateInfo.pos = 0;
}

// Copy constructor
Optimizer::Optimizer( const Optimizer& right ) 
  :
  epsilon                 ( right.epsilon ),
  nMaxIter                ( right.nMaxIter ),
  level                   ( right.level ),
  nIterCompleted          ( right.nIterCompleted ),
  isTooManyIter           ( right.isTooManyIter ),
  saveStateAtEndOfOpt     ( right.saveStateAtEndOfOpt ),
  throwExcepIfMaxIter     ( right.throwExcepIfMaxIter ),
  isWarmStartPossible     ( right.isWarmStartPossible ),
  isWarmStart             ( right.isWarmStart ),
  didOptFinishOk          ( right.didOptFinishOk ),
  isBeginOfIterStateInfo  ( right.isBeginOfIterStateInfo )
{
  stateInfo.n   = 0;
  stateInfo.b   = 0;
  stateInfo.r   = 0;
  stateInfo.f   = 0;
  stateInfo.x   = 0;
  stateInfo.g   = 0;
  stateInfo.h   = 0;
  stateInfo.m   = 0;
  stateInfo.low = 0;
  stateInfo.up  = 0;
  stateInfo.pos = 0;

  setStateInfo(
    right.stateInfo.n,
    right.stateInfo.b,
    right.stateInfo.r,
    right.stateInfo.f,
    right.stateInfo.x,
    right.stateInfo.g,
    right.stateInfo.h,
    right.stateInfo.m,
    right.stateInfo.low,
    right.stateInfo.up,
    right.stateInfo.pos );
}

// Destructor
Optimizer::~Optimizer()
{
  // Free any memory that has allocated and reset the state variables.
  stateInfo.n = 0;
  stateInfo.b = 0;
  if( stateInfo.x ) 
  {
    delete [] stateInfo.x;
    stateInfo.x = 0;
  }
  if( stateInfo.g ) 
  {
    delete [] stateInfo.g;
    stateInfo.g = 0;
  }
  if( stateInfo.h )
  {
    delete [] stateInfo.h;
    stateInfo.h = 0;
  }
  if( stateInfo.low )
  {
    delete [] stateInfo.low;
    stateInfo.low = 0;
  }
  if( stateInfo.up )
  {
    delete [] stateInfo.up;
    stateInfo.up = 0;
  }
  if( stateInfo.pos )
  {
    delete [] stateInfo.pos;
    stateInfo.pos = 0;
  }
}

// Assignment operator
Optimizer& Optimizer::operator=( const Optimizer& right ) 
{
  epsilon                = right.epsilon;
  nMaxIter               = right.nMaxIter;
  level                  = right.level;
  nIterCompleted         = right.nIterCompleted;
  isTooManyIter          = right.isTooManyIter;
  saveStateAtEndOfOpt    = right.saveStateAtEndOfOpt;
  throwExcepIfMaxIter    = right.throwExcepIfMaxIter;
  isWarmStartPossible    = right.isWarmStartPossible;
  isWarmStart            = right.isWarmStart;
  didOptFinishOk         = right.didOptFinishOk;
  isBeginOfIterStateInfo = right.isBeginOfIterStateInfo;

  stateInfo.n   = 0;
  stateInfo.b   = 0;
  stateInfo.r   = 0;
  stateInfo.f   = 0;
  stateInfo.x   = 0;
  stateInfo.g   = 0;
  stateInfo.h   = 0;
  stateInfo.m   = 0;
  stateInfo.low = 0;
  stateInfo.up  = 0;
  stateInfo.pos = 0;

  setStateInfo(
    right.stateInfo.n,
    right.stateInfo.b,
    right.stateInfo.r,
    right.stateInfo.f,
    right.stateInfo.x,
    right.stateInfo.g,
    right.stateInfo.h,
    right.stateInfo.m,
    right.stateInfo.low,
    right.stateInfo.up,
    right.stateInfo.pos );

  return *this;
}

// Get state information
void Optimizer::getStateInfo(
  int            nIn,
  size_t&        bOut,
  double&        rOut,
  double&        fOut,
  double* const  xOut,
  double* const  gOut,
  double* const  hOut )
{
  // Check that there are state variables to get.
  if( stateInfo.n == 0 )
  {
    throw SpkException( 
        SpkError::SPK_USER_INPUT_ERR,
        "There are currently no stored optimizer state variables to get.",
        __LINE__,
        __FILE__ );
  }

  // Check the dimensions of the requested state variables.
  if( stateInfo.n != nIn )
  {
    throw SpkException( 
        SpkError::SPK_USER_INPUT_ERR,
        "The number of optimizer state variables requested does not match the number stored.",
        __LINE__,
        __FILE__ );
  }

  int i;

  // Set the output values.
  if( stateInfo.x && stateInfo.g && stateInfo.h ) 
  {
    bOut = stateInfo.b;
    rOut = stateInfo.r;
    fOut = stateInfo.f;
    for ( i = 0; i < nIn; i++ )
    {
      xOut[i] = stateInfo.x[i];
      gOut[i] = stateInfo.g[i];
    }
    for ( i = 0; i < nIn * nIn; i++ )
    {
      hOut[i] = stateInfo.h[i];
    }
  }
  else
  {
    throw SpkException( 
        SpkError::SPK_NOT_READY_WARM_START_ERR,
        "The optimizer state variables were not allocated properly.",
        __LINE__,
        __FILE__ );
  }
}

// Set state information
void Optimizer::setStateInfo(
  int                  nIn,
  size_t               bIn,
  double               rIn,
  double               fIn,
  const double* const  xIn,
  const double* const  gIn,
  const double* const  hIn,
  int                  mIn,
  const double* const  lowIn,
  const double* const  upIn,
  const int*    const  posIn )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // If it has not already been done, then prepare the variables to
  // hold the state information.
  if( stateInfo.n == 0 )
  {
    stateInfo.n = nIn;
    stateInfo.b = 0;
    if ( stateInfo.x ) delete [] stateInfo.x;;
    if ( stateInfo.g ) delete [] stateInfo.g;;
    if ( stateInfo.h ) delete [] stateInfo.h;;
    stateInfo.x = new double[ nIn ];
    stateInfo.g = new double[ nIn ];
    stateInfo.h = new double[ nIn * nIn ];

    if( !stateInfo.x || !stateInfo.g || !stateInfo.h )
    {
      char errmsg[] = "Failed to allocate memory for the optimzer warm start information.";
      throw SpkException( SpkError::SPK_INSUFFICIENT_MEM_ERR, errmsg, __LINE__, __FILE__ );
    }
  }

  // Check the dimensions of the input state variables.
  if( stateInfo.n != nIn )
  {
    throw SpkException( 
        SpkError::SPK_USER_INPUT_ERR,
        "The number of input optimizer state variables does not match the number stored.",
        __LINE__,
        __FILE__ );
  }

  // If it has not already been done, then prepare the variables to
  // hold the original coordinate information.
  if( stateInfo.m == 0 )
  {
    stateInfo.m = mIn;
    if ( stateInfo.low ) delete [] stateInfo.low;;
    if ( stateInfo.up )  delete [] stateInfo.up;;
    if ( stateInfo.pos ) delete [] stateInfo.pos;;
    stateInfo.low = new double[ mIn ];
    stateInfo.up  = new double[ mIn ];
    stateInfo.pos = new int   [ nIn ];

    if( !stateInfo.low || !stateInfo.up || !stateInfo.pos )
    {
      char errmsg[] = "Failed to allocate memory for the original coordinate information.";
      throw SpkException( SpkError::SPK_INSUFFICIENT_MEM_ERR, errmsg, __LINE__, __FILE__ );
    }
  }

  // Check the dimensions of the input original coordinate
  // information.
  if( stateInfo.m != mIn )
  {
    throw SpkException( 
        SpkError::SPK_USER_INPUT_ERR,
        "The dimensions of the original coordinate information input do not match the dimensions stored.",
        __LINE__,
        __FILE__ );
  }

  // Check that there are at least as many free parameters as there
  // are total parameters.
  if( mIn < nIn )
  {
    throw SpkException( 
        SpkError::SPK_USER_INPUT_ERR,
        "The dimensions of the input state variables and the original coordinate information do not match.",
        __LINE__,
        __FILE__ );
  }


  //------------------------------------------------------------
  // Set the stored optimizer state variables.
  //------------------------------------------------------------

  int i;

  if( stateInfo.x && stateInfo.g && stateInfo.h ) 
  {
    stateInfo.b = bIn;
    stateInfo.r = rIn;
    stateInfo.f = fIn;
    for ( i = 0; i < nIn; i++ )
    {
      stateInfo.x[i] = xIn[i];
      stateInfo.g[i] = gIn[i];
    }
    for ( i = 0; i < nIn * nIn; i++ )
    {
      stateInfo.h[i] = hIn[i];
    }
  }
  else
  {
    throw SpkException( 
        SpkError::SPK_NOT_READY_WARM_START_ERR,
        "The optimizer state variables were not allocated properly.",
        __LINE__,
        __FILE__ );
  }


  //------------------------------------------------------------
  // Set the stored original coordinate information.
  //------------------------------------------------------------

  if( stateInfo.low && stateInfo.up && stateInfo.pos ) 
  {
    for ( i = 0; i < mIn; i++ )
    {
      stateInfo.low[i] = lowIn[i];
      stateInfo.up[i]  = upIn[i];
    }
    for ( i = 0; i < nIn; i++ )
    {
      stateInfo.pos[i] = posIn[i];
    }
  }
  else
  {
    throw SpkException( 
        SpkError::SPK_NOT_READY_WARM_START_ERR,
        "The original coordinate information was not allocated properly.",
        __LINE__,
        __FILE__ );
  }
}

// Check to see if there is any error information to get.
bool Optimizer::isThereErrorInfo() const
{
  // Return true if there was an error during the optimization
  // and if the current state information is from the beginning
  // of the last iteration.
  return ( !didOptFinishOk && isBeginOfIterStateInfo );
}

// Get the error information.
void Optimizer::getErrorInfo( 
  const std::string  headerStr,
  std::string&       messageStr,
  unsigned int       lineNumber,
  const char*        fileName )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // If there isn't any error information, set the message 
  // equal to an empty string and return.
  if ( !isThereErrorInfo() )
  {
    messageStr = "";
    return;
  }

  // Check that there are state variables to get.
  if( stateInfo.n == 0 )
  {
    throw SpkException( 
        SpkError::SPK_OPT_ERR,
        "There are currently no stored optimizer state variables to generate the error information.",
        __LINE__,
        __FILE__ );
  }


  //------------------------------------------------------------
  // Prepare to get the error information.
  //------------------------------------------------------------

  // Create the message stream with the header at the beginning.
  ostringstream message;
  message << headerStr;

  Memory<double> memoryDbl( stateInfo.m );
  double* diff = memoryDbl( stateInfo.m );

  double xOrig;
  double gOrig;
  double gProjOrig;


  //------------------------------------------------------------
  // Get the parameter and gradient information.
  //------------------------------------------------------------

  int i;

  bool isAnyElemConstrained = false;

  int colWidth1 = 9 - 2;
  int colWidth2 = 12 + 2;
  int colWidth3 = 9;
  int colWidth4 = colWidth2;
  int colWidth5 = colWidth2;
  string colSpacer = "  ";

  message << "An error occurred in the optimizer for the iteration that" << endl;
  message << "started with the following parameters:" << endl;
  message << endl;
  message << "Parameter      Value       At Bound?     Gradient     Proj. Gradient"   << endl;
  message << "---------  --------------  ---------  --------------  --------------" << endl;

  for ( i = 0; i < stateInfo.m; i++ )
  {
    //------------------------------------------------------------
    // Convert the state information back to the original coordinates.
    //------------------------------------------------------------

    // Calculate the distance between the original bounds.
    diff[i] = stateInfo.up[i] - stateInfo.low[i];

    // Calculate the parameter in the original coordinates.
    xOrig = stateInfo.low[i] + diff[i] * stateInfo.x[i];

    // Calculate the gradient in the original coordinates.
    if ( diff[i] != 0.0 )
    {
      gOrig = stateInfo.g[i] / diff[i];
    }
    else
    {
      gOrig = 0.0;
    }

    // Calculate the projected gradient in the original coordinates.
    // The projected gradient is the gradient multiplied by the 
    // distance to the parameter's upper or lower bound depending on
    // whether the gradient is negative or nonnegative, respectively.
    if ( gOrig >= 0.0 )
    {
      gProjOrig = ( xOrig - stateInfo.low[i] ) * gOrig;
    }
    else
    {
      gProjOrig = ( stateInfo.up[i] - xOrig ) * gOrig;
    }


    //------------------------------------------------------------
    // Put the parameter and gradient information into the message.
    //------------------------------------------------------------

    // Column 1.
    message << setw( colWidth1 ) << i + 1 << colSpacer;

    // Column 2.
    message << setw( colWidth2 ) << scientific 
          << setprecision( 2 ) << xOrig << colSpacer;

    // Column 3.
    message << setw( colWidth3 );
    if ( stateInfo.low[i] == stateInfo.up[i] )
    {
      message << "Both ";
      isAnyElemConstrained = true;
    }
    else if ( xOrig == stateInfo.low[i] )
    {
      message << "Lower";
    }
    else if ( xOrig == stateInfo.up[i] )
    {
      message << "Upper";
    }
    else
    {
      message << "No   ";
    }
    message << colSpacer;

    // Column 4.
    message << setw( colWidth4 ) << scientific 
          << setprecision( 2 ) << gOrig << colSpacer;

    // Column 5.
    message << setw( colWidth5 ) << scientific 
          << setprecision( 2 ) << gProjOrig << colSpacer;

    message << endl;
  }

  message << endl;
  message << "The projected gradient is the gradient multiplied by the distance to" << endl;
  message << "the upper (lower) bound if the gradient is negative (nonnegative)." << endl;

  message << endl;


  //------------------------------------------------------------
  // Get the Hessian information.
  //------------------------------------------------------------

  int j;

  int colWidth = 10;

  message << "The Hessian approximation used internally by the optimizer" << endl;
  message << "at that point was as follows:" << endl;
  message << endl;
  message << "Hessian =" << endl;
  message << endl;

  // Put the Hessian information into the message after converting
  // them to the original coordinates.
  for ( i = 0; i < stateInfo.n; i++ )
  {
    message << "[ ";

    for ( j = 0; j < stateInfo.n; j++ )
    {
      message << setw( colWidth ) << scientific << setprecision( 2 )
              << stateInfo.h[i + j * stateInfo.n] /
                   ( diff[stateInfo.pos[i]] * diff[stateInfo.pos[j]] );

      if ( j < stateInfo.n - 1 )
      {
        message << ", ";
      }
    }

    message << " ]" << endl;
  }

  // Add a warning if any elements are contrained by both bounds.
  if ( isAnyElemConstrained )
  {
    message << "Note:  some of the elements of the parameter are not included in" << endl;
    message << "the above Hessian because they are constrained by both their" << endl;
    message << "lower and upper bounds and are not optimized over." << endl;
    message << endl;
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Format the error information message in the same way as an
  // SpkError message.  This error message is too long to be stored in
  // one of the SpkError objects that is contained in an SpkException,
  // and so the message must be formatted here.
  formatLongError(
    SpkError::SPK_OPT_ERR,
    message.str(),
    lineNumber,
    fileName,
    messageStr );

}

// Stream insertion operator
std::ostream& operator<<(std::ostream& stream, const Optimizer& right)
{
    stream << "epsilon"            << std::endl;
    stream << right.epsilon        << std::endl;
    stream << "nMaxIter"           << std::endl;
    stream << right.nMaxIter       << std::endl;
    stream << "level"              << std::endl;
    stream << right.level          << std::endl;
    stream << "isTooManyIter"      << std::endl;
    stream << right.isTooManyIter  << std::endl;
    stream << "nIterCompleted"     << std::endl;
    stream << right.nIterCompleted << std::endl;
    return stream;
}

// Stream extraction operator
std::istream& operator>>(std::istream& stream, Optimizer& right)
{
    char title[40];
    bool failed = false;

    stream >> title;
    if(strcmp(title, "epsilon" ) == 0)
        stream >> right.epsilon;
    else
        failed = true;

    stream >> title;
    if(strcmp(title, "nMaxIter") == 0)
        stream >> right.nMaxIter;
    else
        failed = true;
    
    stream >> title;
    if(strcmp(title, "level") == 0)
        stream >> right.level;
    else
        failed = true;

    stream >> title;
    if(strcmp(title, "isTooManyIter") == 0)
        stream >> right.isTooManyIter;
    else
        failed = true;

    stream >> title;
    if(strcmp(title, "nIterCompleted") == 0 )
        stream >> right.nIterCompleted;
    else
        failed = true;

    if(failed)
    {
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "Incorrect data format", __LINE__, __FILE__);
    }

    return stream;
}
