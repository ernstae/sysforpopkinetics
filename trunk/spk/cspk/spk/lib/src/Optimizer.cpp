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
 *   Note:  Function arguments of basic data type (ex. int, double)
 *          were unnecessarily passed by const reference.
 *          Modified to pass by mere values.
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
$syntax//isWarmStart//$$ $cend
false $rend
$tend
and setting all the element pointers of $code StateInfo$$ to $code NULL$$.
The meanings of these parameters are explained under
$tref OptimizerConstructor$$.
Note that these default values do not constitute a recommendation.

$head Arguments$$
N/A

$head Example$$
See $xref/Optimizer/Example/Example/$$
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
This flag indicates that if the too-many-iteration failure has occurred.  
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

/isWarmStart/
/$$
This flag indicates that if the optimization should run a warm start.  
It is set to $code false$$ at the construction time.

$syntax/

/StateInfo/
/$$
This $code StateInfo$$ object contains the optimizer state information
required to perform a warm start.

$head Example$$
See $xref/Optimizer/Example/Example/$$
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
Destroy itself with no memory leak.

$head Example$$
See $xref/Optimizer/Example/Example/$$

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
See $xref/Optimizer/Example/Example/$$

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
See $xref/Optimizer/Example/Example/$$

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
See $xref/Optimizer/Example/Example/$$

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
See $xref/Optimizer/Example/Example/$$

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
See $xref/Optimizer/Example/Example/$$
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
See $xref/Optimizer/Example/Example/$$
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
See $xref/Optimizer/Example/Example/$$
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
See $xref/Optimizer/Example/Example/$$
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
See $xref/Optimizer/Example/Example/$$
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
See $xref/Optimizer/Example/Example/$$
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

$section Get state information for warm start$$

$index Optimizer, state optimization, getStateInfo$$

$table
$bold Prototype$$ $cend
$syntax/StateInfo Optimizer::getStateInfo() const/$$ $rend
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
$code getStateInfo()$$ returns the value of $italic StateInfo$$ 
object that contains the information required for later warm start.
The content of the $italic StateInfo$$ object is described in 
$xref/OptimizerConstructor//Constructor/$$ section.


$head Example$$
See $xref/Optimizer/Example/Example/$$
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
See $xref/Optimizer/Example/Example/$$

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
See $xref/Optimizer/Example/Example/$$

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
See $xref/Optimizer/Example/Example/$$

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
See $xref/Optimizer/Example/Example/$$
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
as a flag to indicate that if too-many-iteration failure has occurred.

$head Example$$
See $xref/Optimizer/Example/Example/$$
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
$syntax/void Optimizer::setSaveStateAtEndOfOpt(bool /s/)/$$ $rend
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
See $xref/Optimizer/Example/Example/$$
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
$syntax/void Optimizer::setThrowExcepIfMaxIter(bool /t/)/$$ $rend
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
See $xref/Optimizer/Example/Example/$$
$end
*/

/* 
-------------------------------------------------------------
   Set the isWarmStart flag
-------------------------------------------------------------
$begin setIsWarmStart$$

$spell
  setIsWarmStart bool Optimizer
    const
$$

$section Set warm start flag$$

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
as a flag to indicate that if the optimization should run a warm start.

$head Example$$
See $xref/Optimizer/Example/Example/$$
$end
*/

/* 
-------------------------------------------------------------
   Set up warm start
-------------------------------------------------------------
$begin setupWarmStart$$

$spell
  setupWarmStart Optimizer
    Integer NCLin NCNlin const
$$

$section Set up warm start$$

$index Optimizer, warm start, setupWarmStart$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::setupWarmStart(int /N/)/$$ $rend
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
$code setupWarmStart()$$ // Allocate memory for storing state 
information for later warm start.  If future warm start is 
intended,this function must be called before 
$xref/fitIndividual//fitIndividual()/$$ or 
$xref/fitPopulation//fitPopulation()/$$ is called.

$head Arguments$$
$syntax/
/N/
/$$
This int number is the number of variables.  
It must be greater than $math%0.0%$$.


$head Example$$
See $xref/Optimizer/Example/Example/$$
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

$section Set state information for warm start$$

$index Optimizer, warm start, state information, setStateInfo$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::setStateInfo(const StateInfo& /s/)/$$ $rend
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
$code setStateInfo( const StateInfo& s )$$ sets state information required 
for later warm start.  This function is for SPK internal use only.

$head Arguments$$
$syntax/
/s/
/$$
This $italic StateInfo$$ object contains the information required by warm start.  
The content of the $italic StateInfo$$ object is described in 
$xref/OptimizerConstructor//Constructor/$$ section.


$head Example$$
See $xref/Optimizer/Example/Example/$$
$end
*/

/* 
-------------------------------------------------------------
   Delete state information
-------------------------------------------------------------
$begin deleteStateInfo$$

$spell
  deleteStateInfo Optimizer
$$

$section Delete state information$$

$index Optimizer, warm start, state information, deleteStateInfo$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::deleteStateInfo()/$$ $rend
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
$code deleteStateInfo()$$ deletes state information used in 
warm start.  It is for SPK internal use only.


$head Example$$
See $xref/Optimizer/Example/Example/$$
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

// Default constructor
//Goddard review 9/17/02: Default values should be reasonable; "arbitrary" isn't useful.
Optimizer::Optimizer()
                    : epsilon( 0.0001 ), nMaxIter( 40 ), level( 1 ), 
            nIterCompleted( 0 ), isTooManyIter( false ),
            saveStateAtEndOfOpt( false ), throwExcepIfMaxIter( true ),
            isWarmStart( false )
{
  // Note: the state information maintained by this class 
  // is specific to the optimizer QuasiNewton01Box.
  stateInfo.n = 0;
  stateInfo.b = 0;
  stateInfo.r = 0;
  stateInfo.f = 0;
  stateInfo.x = 0;
  stateInfo.g = 0;
  stateInfo.h = 0;
}

// Constructor
Optimizer::Optimizer( double Epsilon, int NMaxIter, int Level )
              : epsilon( Epsilon ), nMaxIter( NMaxIter ), level( Level ),
            nIterCompleted( 0 ), isTooManyIter( false ), 
            saveStateAtEndOfOpt( false ), throwExcepIfMaxIter( true ),
            isWarmStart( false )
{
  // Note: the state information maintained by this class 
  // is specific to the optimizer QuasiNewton01Box.
  stateInfo.n = 0;
  stateInfo.b = 0;
  stateInfo.r = 0;
  stateInfo.f = 0;
  stateInfo.x = 0;
  stateInfo.g = 0;
  stateInfo.h = 0;
}

// Copy constructor
Optimizer::Optimizer( const Optimizer& right ) 
                    : epsilon( right.epsilon ), nMaxIter( right.nMaxIter ),
              level( right.level ), nIterCompleted( right.nIterCompleted ), 
            isTooManyIter( right.isTooManyIter ),
            saveStateAtEndOfOpt( right.saveStateAtEndOfOpt ),
	    throwExcepIfMaxIter( right.throwExcepIfMaxIter ),
            isWarmStart( right.isWarmStart ), stateInfo( right.stateInfo )
{}

// Destructor
Optimizer::~Optimizer()
{
    deleteStateInfo();
}

// Assignment operator
Optimizer& Optimizer::operator=( const Optimizer& right ) 
{
  epsilon             = right.epsilon;
  nMaxIter            = right.nMaxIter;
  level               = right.level;
  nIterCompleted      = right.nIterCompleted;
  isTooManyIter       = right.isTooManyIter;
  saveStateAtEndOfOpt = right.saveStateAtEndOfOpt;
  throwExcepIfMaxIter = right.throwExcepIfMaxIter;
  isWarmStart         = right.isWarmStart;
  stateInfo           = right.stateInfo;
  return *this;
}

// Allocate memory for returning state information for warm start
void Optimizer::setupWarmStart( int n )
{
  stateInfo.n = n;
  stateInfo.b = 0;
  if ( stateInfo.x ) delete [] stateInfo.x;;
  if ( stateInfo.g ) delete [] stateInfo.g;;
  if ( stateInfo.h ) delete [] stateInfo.h;;
  stateInfo.x = new double[ n ];
  stateInfo.g = new double[ n ];
  stateInfo.h = new double[ n * n ];
  if( !stateInfo.x || !stateInfo.g || !stateInfo.h )
  {
        char errmsg[] = "setUpWarmStart() failed to allocate memory.";
        throw SpkException( SpkError::SPK_INSUFFICIENT_MEM_ERR, errmsg, __LINE__, __FILE__ );
    }
}

// Set turning on/off warm start flag 
void Optimizer::setIsWarmStart( bool w ) 
{ 
  // Note: the state information maintained by this class 
  // is specific to the optimizer QuasiNewton01Box.
  if( w )
  {
    if( stateInfo.x && stateInfo.g && stateInfo.h ) 
    {
      isWarmStart = true;
    }
    else
    {
            char errmsg[] = "It's not ready for warm start. Call setupWarmStart().";
            throw SpkException( SpkError::SPK_NOT_READY_WARM_START_ERR, errmsg, __LINE__, __FILE__ );
    }
  }
  else
  {
        isWarmStart = false;
  }
}

// Set state info
void Optimizer::setStateInfo( const StateInfo& s )
{
  // Note: the state information maintained by this class 
  // is specific to the optimizer QuasiNewton01Box.
    if( stateInfo.n != s.n )
  {
        char errmsg[] = "The number of variables is incorrect. Check calling setupWarmStart().";
        throw SpkException( SpkError::SPK_USER_INPUT_ERR, errmsg, __LINE__, __FILE__ );
  }
  stateInfo.b = s.b;
  stateInfo.r = s.r;
  stateInfo.f = s.f;
  if( stateInfo.x && stateInfo.g && stateInfo.h ) 
  {
      for( int i = 0; i < stateInfo.n; i++ )
    {
            stateInfo.x[ i ] = s.x[ i ];
            stateInfo.g[ i ] = s.g[ i ];
    }
      for( int i = 0; i < stateInfo.n * stateInfo.n; i++ )
    {
        stateInfo.h[ i ] = s.h[ i ];
    }
  }
  else
  {
        char errmsg[] = "It's not ready for warm start. Check calling setupWarmStart().";
        throw SpkException( SpkError::SPK_NOT_READY_WARM_START_ERR, errmsg, __LINE__, __FILE__ );
  }
}

// Delete state info
void Optimizer::deleteStateInfo()
{
  // Note: the state information maintained by this class 
  // is specific to the optimizer QuasiNewton01Box.
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
