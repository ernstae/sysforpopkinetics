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
 * A wrapper for optimizer control.
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


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include "Optimizer.h"
#include "QuasiNewtonAnyBoxObj.h"
#include "SpkException.h"
#include "SpkValarray.h"

// SPK optimizer header files.
#include <QN01Box/Memory.h>

// Xerces XML parser header files.
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/dom/DOM.hpp>

// Standard library header files.
#include <cstdio> 
#include <fstream> 
#include <iomanip>
#include <iostream>
#include <limits> 
#include <sstream> 
#include <string> 
#include <sys/stat.h>

using std::string;;

using SPK_VA::valarray;

/*------------------------------------------------------------------------
 * Local variable declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  const char* pop_restart_infoTag             = "pop_restart_info";
  const char* isTooManyIterAttribute          = "is_too_many_iter";
  const char* saveStateAtEndOfOptAttribute    = "save_state_at_end_of_opt";
  const char* throwExcepIfMaxIterAttribute    = "throw_excep_if_max_iter";
  const char* isWarmStartPossibleAttribute    = "is_warm_start_possible";
  const char* isWarmStartAttribute            = "is_warm_start";
  const char* didOptFinishOkAttribute         = "did_opt_finish_ok";
  const char* isBeginOfIterStateInfoAttribute = "is_begin_of_iter_state_info";
  const char* epsilonTag                      = "epsilon";
  const char* nIterCompletedTag               = "n_iter_completed";
  const char* stateInfo_nTag                  = "state_info_n";
  const char* stateInfo_bTag                  = "state_info_b";
  const char* stateInfo_rTag                  = "state_info_r";
  const char* stateInfo_fTag                  = "state_info_f";
  const char* stateInfo_xTag                  = "state_info_x";
  const char* stateInfo_gTag                  = "state_info_g";
  const char* stateInfo_hTag                  = "state_info_h";
  const char* stateInfo_mTag                  = "state_info_m";
  const char* stateInfo_lowTag                = "state_info_low";
  const char* stateInfo_upTag                 = "state_info_up";
  const char* stateInfo_posTag                = "state_info_pos";
  const char* stateInfo_acceptStepCountTag    = "state_info_accept_step_count";
  const char* yesStr                          = "yes";
  const char* valueTag                        = "value";
  const char* lengthAttribute                 = "length";

} // [End: unnamed namespace]


/*************************************************************************
 *
 * Class: Optimizer
 *
 *************************************************************************/

/*
-------------------------------------------------------------
   Basic constructor 
-------------------------------------------------------------
$begin OptimizerConstructor$$

$spell
  Curr
  Func
  Obj
  Bfgs
  bool
  const
  Excep
  int
  Iter
  NMaxIter
  optimizer
  pos
  std
$$

$section Basic Constructor$$

$index Optimizer, constructor$$

$table
$bold Prototype:$$ $cend
$syntax/Optimizer::Optimizer(
  double             /epsilonIn/,
  int                /nMaxIterIn/,
  int                /levelIn/,
  const std::string  /restartFileNameIn/  = "",
  bool               /readRestartInfoIn/  = false,
  bool               /writeRestartInfoIn/ = false )
/$$
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
Constructor for the class.
$pre

$$
The last three arguments for this constructor are optional and only
need to be provided when restart information for this Optimizer object
needs to saved to or retrieved from a restart file.

$head Arguments$$
$syntax/
/epsilonIn/
/$$
This real number is used to specify the convergence criteria
for the optimizer.
It must be greater than $math%0.0%$$.

$syntax/

/nMaxIterIn/
/$$
This integer must be greater than or equal to zero.
It specifies the maximum number of 
iterations to attempt before giving up on convergence.

$syntax/

/levelIn/
/$$
This integer scalar specifies the amount of tracing.
Larger values of $italic levelIn$$ entail more tracing.

$syntax/

/restartFileNameIn/
/$$
This string specifies the name of the XML format restart file.

$syntax/

/readRestartInfoIn/
/$$
This bool specifies whether or not restart information should be 
read from the restart file.

$syntax/

/writeRestartInfoIn/
/$$
This bool specifies whether or not restart information should be 
written to the restart file.

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
The element $code n$$ specifies the number of components
in the element vector $code x$$.

$subhead stateInfo.b$$
The element $code b$$ specifies the number of Bfgs updates
that have been made to the Hessian approximation $code h$$.

$subhead stateInfo.r$$
The element $code r$$ contains the current trust region radius
(as an infinity norm bound on the step size).

$subhead stateInfo.f$$
The element $code f$$ contains the value for $math%f(x)%$$
at the point $math%x%$$.

$subhead stateInfo.x$$
The element $code x$$ is a vector of length $code n$$.
It specifies the point at which the objective function, 
its gradient, and its Hessian were evaluated.

$subhead stateInfo.g$$
The element $code g$$ is a vector of length $code n$$.
It contains the gradient of $math%f(x)%$$
at the point $math%x%$$.

$subhead stateInfo.h$$
The element $code h$$ is a vector of length $code n * n$$.
It contains an approximation for the Hessian of $math%f(x)%$$
at the point $math%x%$$.

$subhead stateInfo.m$$
The element $code m$$ specifies the total number of objective 
function parameters, i.e., the number of free parameters plus the 
number of parameters that are constrained by both their lower and 
upper bounds.

$subhead stateInfo.low$$
The element $code low$$ is a vector of length $code m$$.
It specifies the lower bounds for all of the objective function 
parameters in their original coordinates.

$subhead stateInfo.up$$
The element $code up$$ is a vector of length $code m$$.
It specifies the upper bounds for all of the objective function 
parameters in their original coordinates.

$subhead stateInfo.pos$$
The element $code pos$$ is a vector of length $code n$$.
It specifies the positions, i.e., indices, of the free objective 
function parameters in the full objective function parameter.

$subhead stateInfo.acceptStepCount$$
The element $code acceptStepCount$$ specifies 
the number of consecutive iterations that acceptable 
step values have been calculated.

$end
*/

Optimizer::Optimizer(
  double             epsilonIn,
  int                nMaxIterIn,
  int                levelIn,
  const std::string  restartFileNameIn,
  bool               readRestartInfoIn,
  bool               writeRestartInfoIn )
  :
  epsilon                 ( epsilonIn ),
  nMaxIter                ( nMaxIterIn ),
  level                   ( levelIn ),
  nIterCompleted          ( 0 ),
  isTooManyIter           ( false ),
  saveStateAtEndOfOpt     ( false ),
  throwExcepIfMaxIter     ( true ),
  isWarmStartPossible     ( false ),
  isWarmStart             ( false ),
  didOptFinishOk          ( false ),
  isBeginOfIterStateInfo  ( false ),
  restartFileName         ( restartFileNameIn ),
  readRestartInfo         ( readRestartInfoIn ),
  writeRestartInfo        ( writeRestartInfoIn ),
  pObjFunc                ( 0 ),
  pParser                 ( 0 ),
  pCurrElement            ( 0 ),
  freeXMLMemory           ( false ),
  xmlLevel                ( 0 ),
  xmlTab                  ( 2 )
{
  // Zero all of the state information.
  stateInfo.n               = 0;
  stateInfo.b               = 0;
  stateInfo.r               = 0;
  stateInfo.f               = 0;
  stateInfo.x               = 0;
  stateInfo.g               = 0;
  stateInfo.h               = 0;
  stateInfo.m               = 0;
  stateInfo.low             = 0;
  stateInfo.up              = 0;
  stateInfo.pos             = 0;
  stateInfo.acceptStepCount = 0;

  // link QN01Box to Spk version of its error handler
  extern void QN01BoxErrorSetup(void);
  QN01BoxErrorSetup();
}


/*
-------------------------------------------------------------
   Default constructor 
-------------------------------------------------------------
$begin OptimizerDefaultConstructor$$

$spell
  Curr
  Func
  Obj
  Bfgs
  bool
  const
  Excep
  int
  Iter
  NMaxIter
  optimizer
  pos
  std
$$

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
Default constructor for the class.
$pre

$$
It constructs an object of $code Optimizer$$ with
the following default values:
$table
$syntax//epsilon//$$ $cend
0.001 $rend
$syntax//nMaxIter//$$ $cend
0 $rend
$syntax//traceLevel//$$ $cend
0 $rend
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
$syntax//restartFileName//$$ $cend
"" $rend
$syntax//readRestartInfo//$$ $cend
false $rend
$syntax//writeRestartInfo//$$ $cend
false $rend
$tend
The meanings of these variables are explained under
$tref OptimizerConstructor$$.

$end
*/

Optimizer::Optimizer()
  :
  epsilon                 ( 0.001 ),
  nMaxIter                ( 0 ),
  level                   ( 0 ),
  nIterCompleted          ( 0 ),
  isTooManyIter           ( false ),
  saveStateAtEndOfOpt     ( false ),
  throwExcepIfMaxIter     ( true ),
  isWarmStartPossible     ( false ),
  isWarmStart             ( false ),
  didOptFinishOk          ( false ),
  isBeginOfIterStateInfo  ( false ),
  restartFileName         ( "" ),
  readRestartInfo         ( false ),
  writeRestartInfo        ( false ),
  pObjFunc                ( 0 ),
  pParser                 ( 0 ),
  pCurrElement            ( 0 ),
  freeXMLMemory           ( false ),
  xmlLevel                ( 0 ),
  xmlTab                  ( 2 )
{
  // Zero all of the state information.
  stateInfo.n               = 0;
  stateInfo.b               = 0;
  stateInfo.r               = 0;
  stateInfo.f               = 0;
  stateInfo.x               = 0;
  stateInfo.g               = 0;
  stateInfo.h               = 0;
  stateInfo.m               = 0;
  stateInfo.low             = 0;
  stateInfo.up              = 0;
  stateInfo.pos             = 0;
  stateInfo.acceptStepCount = 0;
}


/*
-------------------------------------------------------------
   Copy constructor
-------------------------------------------------------------
$begin OptimizerCopyConstructor $$

$spell
  Curr
  Func
  Obj
  Bfgs
  bool
  const
  Excep
  int
  Iter
  NMaxIter
  optimizer
  pos
  std
$$

$section Copy Constructor$$

$index Optimizer, copy constructor$$

$table
$bold Prototype:$$ $cend
$syntax/Optimizer::Optimizer( const Optimizer& /original/ )/$$
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
Constructs a new $code Optimizer$$ object that is identical to 
$italic original$$, except for 
the $code freeXMLMemory$$ flag, which is set equal to false,
and except for the following pointers, which are all set equal to zero:
the pointer to the objective function, $code pObjFunc$$;
the pointer to the current XML element in the restart file,
$code pCurrElement$$;
and the pointer to the XML parser, $code pParser$$.

$end
*/

Optimizer::Optimizer( const Optimizer& original ) 
  :
  epsilon                 ( original.epsilon ),
  nMaxIter                ( original.nMaxIter ),
  level                   ( original.level ),
  nIterCompleted          ( original.nIterCompleted ),
  isTooManyIter           ( original.isTooManyIter ),
  saveStateAtEndOfOpt     ( original.saveStateAtEndOfOpt ),
  throwExcepIfMaxIter     ( original.throwExcepIfMaxIter ),
  isWarmStartPossible     ( original.isWarmStartPossible ),
  isWarmStart             ( original.isWarmStart ),
  didOptFinishOk          ( original.didOptFinishOk ),
  isBeginOfIterStateInfo  ( original.isBeginOfIterStateInfo ),
  restartFileName         ( original.restartFileName ),
  readRestartInfo         ( original.readRestartInfo ),
  writeRestartInfo        ( original.writeRestartInfo ),
  pObjFunc                ( 0 ),
  pParser                 ( 0 ),
  pCurrElement            ( 0 ),
  freeXMLMemory           ( false ),
  xmlLevel                ( 0 ),
  xmlTab                  ( 2 )
{
  // Zero all of the state information.
  stateInfo.n               = 0;
  stateInfo.b               = 0;
  stateInfo.r               = 0;
  stateInfo.f               = 0;
  stateInfo.x               = 0;
  stateInfo.g               = 0;
  stateInfo.h               = 0;
  stateInfo.m               = 0;
  stateInfo.low             = 0;
  stateInfo.up              = 0;
  stateInfo.pos             = 0;
  stateInfo.acceptStepCount = 0;

  // Copy all of the state information.
  setStateInfo(
    original.stateInfo.n,
    original.stateInfo.b,
    original.stateInfo.r,
    original.stateInfo.f,
    original.stateInfo.x,
    original.stateInfo.g,
    original.stateInfo.h,
    original.stateInfo.m,
    original.stateInfo.low,
    original.stateInfo.up,
    original.stateInfo.pos,
    original.stateInfo.acceptStepCount );
}


/*
-------------------------------------------------------------
   Assignment operator
-------------------------------------------------------------
$begin OptimizerAssignmentOperator$$

$spell
  Curr
  Func
  Obj
  Optimizer 
  const
$$

$section Assignment Operator$$

$index Optimizer, assignment operator$$

$table
$bold Prototype:$$ $cend
$syntax/Optimizer& Optimizer::operator=( const Optimizer& /right/ )/$$
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
Copies information from $italic right$$ to $italic *this$$ and
returns $italic this$$, where $italic *this$$ is the $code Optimizer$$
object on the left side of the = sign.
$pre

$$
The $italic *this$$ object will be identical to the $italic right$$, 
except for the $code freeXMLMemory$$ flag, which is set equal to false,
and except for the following pointers, which are all set equal to zero:
the pointer to the objective function, $code pObjFunc$$;
the pointer to the current XML element in the restart file,
$code pCurrElement$$;
and the pointer to the XML parser, $code pParser$$.

$end
*/

Optimizer& Optimizer::operator=( const Optimizer& right )
{
  // Handle the case of self assignment, i.e., the case where an
  // Optimizer object is assigned to itself.
  if ( this == &right )
  {
    return *this;
  }

  // Copy these variables.
  epsilon                 = right.epsilon;
  nMaxIter                = right.nMaxIter;
  level                   = right.level;
  nIterCompleted          = right.nIterCompleted;
  isTooManyIter           = right.isTooManyIter;
  saveStateAtEndOfOpt     = right.saveStateAtEndOfOpt;
  throwExcepIfMaxIter     = right.throwExcepIfMaxIter;
  isWarmStartPossible     = right.isWarmStartPossible;
  isWarmStart             = right.isWarmStart;
  didOptFinishOk          = right.didOptFinishOk;
  isBeginOfIterStateInfo  = right.isBeginOfIterStateInfo;
  restartFileName         = right.restartFileName;
  readRestartInfo         = right.readRestartInfo;
  writeRestartInfo        = right.writeRestartInfo;

  // Don't copy these variables.
  pObjFunc                = 0;
  pParser                 = 0;
  pCurrElement            = 0;
  freeXMLMemory           = false;
  xmlLevel                = 0;

  // Free memory that has been allocated for the state information.
  freeStateInfo();

  // Copy all of the state information.
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
    right.stateInfo.pos,
    right.stateInfo.acceptStepCount );

  return *this;
}


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

$end

*/

Optimizer::~Optimizer()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace xercesc;


  //------------------------------------------------------------
  // Do clean ups related to the state information.
  //------------------------------------------------------------

  // Free memory that has been allocated for the state information.
  freeStateInfo();


  //------------------------------------------------------------
  // Do clean ups related to parsing the restart file.
  //------------------------------------------------------------

  // If the memory that was allocated in order to parse the
  // restart file has not been freed, then free it now.
  if ( freeXMLMemory )
  {
    // Free the memory allocated for the XML parser.
    if ( pParser )
    {
      delete pParser;
    }

    // Free the memory allocated for all of the XML strings.
    XMLString::release( &pPop_restart_infoTag             );
    XMLString::release( &pIsTooManyIterAttribute          );
    XMLString::release( &pSaveStateAtEndOfOptAttribute    );
    XMLString::release( &pThrowExcepIfMaxIterAttribute    );
    XMLString::release( &pIsWarmStartPossibleAttribute    );
    XMLString::release( &pIsWarmStartAttribute            );
    XMLString::release( &pDidOptFinishOkAttribute         );
    XMLString::release( &pIsBeginOfIterStateInfoAttribute );
    XMLString::release( &pEpsilonTag                      );
    XMLString::release( &pNIterCompletedTag               );
    XMLString::release( &pStateInfo_nTag                  );
    XMLString::release( &pStateInfo_bTag                  );
    XMLString::release( &pStateInfo_rTag                  );
    XMLString::release( &pStateInfo_fTag                  );
    XMLString::release( &pStateInfo_xTag                  );
    XMLString::release( &pStateInfo_gTag                  );
    XMLString::release( &pStateInfo_hTag                  );
    XMLString::release( &pStateInfo_mTag                  );
    XMLString::release( &pStateInfo_lowTag                );
    XMLString::release( &pStateInfo_upTag                 );
    XMLString::release( &pStateInfo_posTag                );
    XMLString::release( &pStateInfo_acceptStepCountTag    );
    XMLString::release( &pYesStr                          );
    XMLString::release( &pValueTag                        );
    XMLString::release( &pLengthAttribute                 );
  
    // Clean up the XML workspace.
    XMLPlatformUtils::Terminate();
  }


  //------------------------------------------------------------
  // Do cleanups related to writing the restart file.
  //------------------------------------------------------------

  // If the temporary restart file has not been closed, 
  // then close it now.
  if ( tempRestartFileStream.is_open() )
  {
    tempRestartFileStream.close();
  }

}


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

$end
*/

/* 
-------------------------------------------------------------
   Get the save state at end of optimization flag
-------------------------------------------------------------
$begin getSaveStateAtEndOfOpt$$

$spell
  getSaveStateAtEndOfOpt bool Optimizer
  const
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


$end
*/

/* 
-------------------------------------------------------------
   Get the thrown exception if maxIter is exhausted flag
-------------------------------------------------------------
$begin getThrowExcepIfMaxIter$$

$spell
  getThrowExcepIfMaxIter bool Optimizer
  const
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

$end
*/

/* 
-------------------------------------------------------------
   Set the did the optimizer finish ok flag
-------------------------------------------------------------
$begin setDidOptFinishOk$$

$spell
  Bfgs
  pos
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

$end
*/

/* 
-------------------------------------------------------------
   Set the is beginning of iteration state information flag
-------------------------------------------------------------
$begin setIsBeginOfIterStateInfo$$

$spell
  Bfgs
  pos
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

$end
*/

/* 
-------------------------------------------------------------
   Get the current objective function value
-------------------------------------------------------------
$begin getObj$$

$spell
  const 
  getObj
  Optimizer
  obj
$$

$section Get the current objective function value$$

$index Optimizer, current objective, getObj$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::getObj( double& /objOut/ ) const
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
This function gets the current objective function value.

$head Arguments$$
$syntax/
/objOut/
/$$
This will be set equal to the value for the objective function 
at the end of the last successful iteration.
If the optimizer failed during an iteration, then this will be 
set equal to the objective value at the beginning of the iteration that
failed.

$end
*/

void Optimizer::getObj( double& objOut ) const
{
  // Check that there is stored state information to use to get the
  // current objective function value.
  if ( stateInfo.n == 0 )
  {
    // Set the output value equal to a Not a Number (NaN).
    double zero = 0.0;
    objOut = zero / zero;
  }

  // Set the output value equal to the current objective value.
  objOut = stateInfo.f;
}


/* 
-------------------------------------------------------------
   Get the current objective function parameter value
-------------------------------------------------------------
$begin getPar$$

$spell
  const 
  getPar
  Optimizer
  par
  valarray
$$

$section Get the current objective function parameter value$$

$index Optimizer, current parameter, getPar$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::getPar( SPK_VA::valarray<double>& /parOut/ ) const
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
This function gets the current objective function parameter value.

$head Arguments$$
$syntax/
/parOut/
/$$
This array will be set equal to the value for the objective function 
parameter at the end of the last successful iteration.
If the optimizer failed during an iteration, then this array will be 
set equal to the parameter value at the beginning of the iteration that
failed.
On output, it will have the same length as the objective function 
parameter.

$end
*/

void Optimizer::getPar( valarray<double>& parOut ) const
{
  //------------------------------------------------------------
  // See if it is possible to get the current objective parameter.
  //------------------------------------------------------------

  // Check that there is stored state information to use to get the
  // current objective function parameter.
  if ( stateInfo.n == 0 )
  {
    // Set the output value equal to a Not a Number (NaN).
    double zero = 0.0;
    parOut = zero / zero;
    return;
  }


  //------------------------------------------------------------
  // Set the output value equal to the current objective parameter.
  //------------------------------------------------------------

  // The total number of objective function parameters is the number
  // of free parameters plus the number of parameters that are
  // constrained by both their lower and upper bounds.
  parOut.resize( stateInfo.m );

  int i;
  int nObjParFree = 0;

  // Calculate the objective function parameter value by converting
  // the state information back to the original coordinates.
  if ( stateInfo.x && stateInfo.low && stateInfo.up ) 
  {
    for ( i = 0; i < stateInfo.m; i++ )
    {
      if ( stateInfo.up[i] != stateInfo.low[i] )
      {
        parOut[i] = stateInfo.low[i] + 
          ( stateInfo.up[i] - stateInfo.low[i] ) * stateInfo.x[nObjParFree];

        nObjParFree++;
      }
      else
      {
        parOut[i] = stateInfo.low[i];
      }
    }
  }
  else
  {
    throw SpkException( 
      SpkError::SPK_NOT_READY_WARM_START_ERR,
      "The optimizer state information was not allocated properly.",
      __LINE__,
      __FILE__ );
  }

}


/* 
-------------------------------------------------------------
   Get the state information for warm start
-------------------------------------------------------------
$begin getStateInfo$$

$spell
  Bfgs
  pos
  getStateInfo stateInfo Optimizer
    const 
$$

$section Get state information for a warm start$$

$index Optimizer, state information, getStateInfo$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::getStateInfo(
  int                  /nIn/,
  size_t&              /bOut/,
  double&              /rOut/,
  double&              /fOut/,
  double* const        /xOut/,
  double* const        /gOut/,
  double* const        /hOut/,
  int                  /mIn/,
  const double* const  /lowIn/,
  const double* const  /upIn/,
  const int*    const  /posIn/,
  int&                 /acceptStepCountOut/ )
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
This function gets state information required for a warm start.
$pre

$$
If restart information should be read from a file for this Optimizer
object, then it will be done when this function is called.

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
Its values will not be changed.

$syntax/

/upIn/
/$$
The array pointed to by $italic upIn$$ must have length 
$italic mIn$$.
It specifies the upper bounds for all of the objective function 
parameters in their original coordinates.
Its values will not be changed.

$syntax/

/posIn/
/$$
The array pointed to by $italic posIn$$ must have length 
$italic nIn$$.
It specifies the positions, i.e., indices, of the free objective 
function parameters in the full objective function parameter.
Its values will not be changed.

$syntax/

/acceptStepCountOut/
/$$
The argument $italic acceptStepCountOut$$ will be set 
equal to the number of consecutive iterations that 
acceptable step values have been calculated.

$end
*/

void Optimizer::getStateInfo(
  int                  nIn,
  size_t&              bOut,
  double&              rOut,
  double&              fOut,
  double* const        xOut,
  double* const        gOut,
  double* const        hOut,
  int                  mIn,
  const double* const  lowIn,
  const double* const  upIn,
  const int*    const  posIn,
  int&                 acceptStepCountOut )
{
  //------------------------------------------------------------
  // Prepare the stored optimizer state information.
  //------------------------------------------------------------

  // See if the restart information should be read in from a file.
  // If not, then the current stored optimizer state information
  // will be copied to the output arguments.
  if ( readRestartInfo )
  {
    // Allocate memory required for the state information.
    initStateInfo( nIn, mIn );

    // Set the stored optimizer state information equal to 
    // the optimizer state information in the restart file.
    readRestartInfoFromFile();
  }

  // Check to see if a warm start is possible.
  if ( !isWarmStartPossible )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "It is not possible to perform a warm start using the current optimizer state information.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // See if it is possible to get the state information.
  //------------------------------------------------------------

  // Check that there is state information to get.
  if ( stateInfo.n == 0 )
  {
    throw SpkException( 
      SpkError::SPK_USER_INPUT_ERR,
      "There is currently no stored optimizer state information to get.",
      __LINE__,
      __FILE__ );
  }

  // Check the dimensions of the requested state information.
  if ( stateInfo.n != nIn )
  {
    throw SpkException( 
      SpkError::SPK_USER_INPUT_ERR,
      "The dimensions of the requested optimizer state information are not equal to those stored.",
      __LINE__,
      __FILE__ );
  }

  // Check the dimensions of the input original coordinate
  // information.
  if ( stateInfo.m != mIn )
  {
    throw SpkException( 
      SpkError::SPK_USER_INPUT_ERR,
      "The dimensions of the input original coordinate information are not equal to those stored.",
      __LINE__,
      __FILE__ );
  }

  // Check the input bounds to make sure they're the same as the
  // stored bounds.
  int i;
  for ( i = 0; i < mIn; i++ )
  {
    if ( stateInfo.low[i] != lowIn[i] )
    {
      throw SpkException( 
        SpkError::SPK_USER_INPUT_ERR,
        "One of the input original coordinate lower bounds is not equal to one of those stored.",
        __LINE__,
        __FILE__ );
    }

    if ( stateInfo.up[i] != upIn[i] )
    {
      throw SpkException( 
        SpkError::SPK_USER_INPUT_ERR,
        "One of the input original coordinate upper bounds is not equal to one of those stored.",
        __LINE__,
        __FILE__ );
    }
  }

  // Check the input positions to make sure they're the same as the
  // stored positions.
  for ( i = 0; i < nIn; i++ )
  {
    if ( stateInfo.pos[i] != posIn[i] )
    {
      throw SpkException( 
        SpkError::SPK_USER_INPUT_ERR,
        "One of the input original coordinate positions is not equal to one of those stored.",
        __LINE__,
        __FILE__ );
    }
  }


  //------------------------------------------------------------
  // Copy the state information to the output arguments.
  //------------------------------------------------------------

  if ( stateInfo.x && stateInfo.g && stateInfo.h ) 
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
      "The optimizer state information was not allocated properly.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Copy the acceptance criteria information to output argument.
  //------------------------------------------------------------

  acceptStepCountOut = stateInfo.acceptStepCount;

}


/* 
-------------------------------------------------------------
   Set state information for warm start
-------------------------------------------------------------
$begin setStateInfo$$

$spell
  Bfgs
  pos
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
  const int*    const  /posIn/,
  int                  /acceptStepCountIn/ )
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
This function sets state information required for a warm start.

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

$syntax/

/acceptStepCountIn/
/$$
The argument $italic acceptStepCountIn$$ specifies 
the number of consecutive iterations that acceptable 
step values have been calculated.

$end
*/

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
  const int*    const  posIn,
  int                  acceptStepCountIn )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Check that there are not more free parameters than there
  // are total parameters.
  if ( nIn > mIn )
  {
    throw SpkException( 
      SpkError::SPK_USER_INPUT_ERR,
      "The number of free objective parameters is larger than the total number of objective parameters.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Prepare the variables to hold the state information.
  //------------------------------------------------------------

  // If it has not already been done, then initialize the state
  // information.
  if ( stateInfo.n == 0 )
  {
    initStateInfo( nIn, mIn );
  }

  // Check the dimensions of the input state variables.
  if ( stateInfo.n != nIn )
  {
    throw SpkException( 
      SpkError::SPK_USER_INPUT_ERR,
      "The dimensions of the input optimizer state information are not equal to those stored.",
      __LINE__,
      __FILE__ );
  }

  // Check the dimensions of the input original coordinate
  // information.
  if ( stateInfo.m != mIn )
  {
    throw SpkException( 
      SpkError::SPK_USER_INPUT_ERR,
      "The dimensions of the input original coordinate information do not match the dimensions stored.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Set the stored optimizer state information.
  //------------------------------------------------------------

  int i;

  if ( stateInfo.x && stateInfo.g && stateInfo.h ) 
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
      "The optimizer state information was not allocated properly.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Set the stored original coordinate information.
  //------------------------------------------------------------

  if ( stateInfo.low && stateInfo.up && stateInfo.pos ) 
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


  //------------------------------------------------------------
  // Set the acceptance criteria information.
  //------------------------------------------------------------

  stateInfo.acceptStepCount = acceptStepCountIn;


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Write the restart information to a file, if necessary.
  if ( writeRestartInfo )
  {
    writeRestartInfoToFile();
  }

}


/* 
-------------------------------------------------------------
   Allocate memory for the state information
-------------------------------------------------------------
$begin initStateInfo$$

$spell
  Bfgs
  pos
  initStateInfo stateInfo Optimizer
$$

$section Allocates Memory for the State Information$$

$index Optimizer, state information, initStateInfo$$
$cindex Allocates Memory \for \the State Information$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::initStateInfo( int /nIn/, int /mIn/ )/$$
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
Allocates memory for the state information that is required for
a warm start.

$end
*/

void Optimizer::initStateInfo( int nIn, int mIn )
{
  //------------------------------------------------------------
  // Initialize the state information.
  //------------------------------------------------------------

  stateInfo.n = nIn;
  stateInfo.b = 0;

  if ( stateInfo.x ) 
  {
    delete [] stateInfo.x;
  }
  if ( stateInfo.g ) 
  {
    delete [] stateInfo.g;
  }
  if ( stateInfo.h )
  {
    delete [] stateInfo.h;
  }

  stateInfo.x = new double[ nIn ];
  stateInfo.g = new double[ nIn ];
  stateInfo.h = new double[ nIn * nIn ];

  if ( !stateInfo.x || !stateInfo.g || !stateInfo.h )
  {
    throw SpkException(
      SpkError::SPK_INSUFFICIENT_MEM_ERR,
      "Failed to allocate memory for the optimzer warm start information.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Initialize the original coordinate information.
  //------------------------------------------------------------

  stateInfo.m = mIn;

  if ( stateInfo.low )
  {
    delete [] stateInfo.low;
  }
  if ( stateInfo.up )
  {
    delete [] stateInfo.up;
  }
  if ( stateInfo.pos )
  {
    delete [] stateInfo.pos;
  }

  stateInfo.low = new double[ mIn ];
  stateInfo.up  = new double[ mIn ];
  stateInfo.pos = new int   [ nIn ];

  if ( !stateInfo.low || !stateInfo.up || !stateInfo.pos )
  {
    throw SpkException(
      SpkError::SPK_INSUFFICIENT_MEM_ERR,
      "Failed to allocate memory for the original coordinate information.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Initialize the acceptance criteria information.
  //------------------------------------------------------------

  stateInfo.acceptStepCount = 0;
}


/* 
-------------------------------------------------------------
   Free memory allocated for the state information
-------------------------------------------------------------
$begin freeStateInfo$$

$spell
  Bfgs
  pos
  freeStateInfo stateInfo Optimizer
$$

$section Free Memory Allocated for the State Information$$

$index Optimizer, state information, freeStateInfo$$
$cindex Free Memory \Allocated \for \the State Information$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::freeStateInfo()/$$
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
Frees memory allocated for the state information that is 
required for a warm start.

$end
*/

void Optimizer::freeStateInfo()
{
  //------------------------------------------------------------
  // Clean up the state information.
  //------------------------------------------------------------

  stateInfo.n = 0;
  stateInfo.b = 0;
  if ( stateInfo.x ) 
  {
    delete [] stateInfo.x;
    stateInfo.x = 0;
  }
  if ( stateInfo.g ) 
  {
    delete [] stateInfo.g;
    stateInfo.g = 0;
  }
  if ( stateInfo.h )
  {
    delete [] stateInfo.h;
    stateInfo.h = 0;
  }


  //------------------------------------------------------------
  // Clean up the original coordinate information.
  //------------------------------------------------------------

  stateInfo.m = 0;
  if ( stateInfo.low )
  {
    delete [] stateInfo.low;
    stateInfo.low = 0;
  }
  if ( stateInfo.up )
  {
    delete [] stateInfo.up;
    stateInfo.up = 0;
  }
  if ( stateInfo.pos )
  {
    delete [] stateInfo.pos;
    stateInfo.pos = 0;
  }


  //------------------------------------------------------------
  // Clean up the acceptance criteria information.
  //------------------------------------------------------------

  stateInfo.acceptStepCount = 0;
}


/* 
-------------------------------------------------------------
   Set the objective function
-------------------------------------------------------------
$begin setObjFunc$$

$spell
  Bfgs
  pos
  setObjFunc int Optimizer
    const
$$

$section Set the objective function$$

$index Optimizer, objFunc, setObjFunc$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::setObjFunc( const QuasiNewtonAnyBoxObj* const /pObjFuncIn/ )
/$$ $rend
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
$code setObjFunc()$$ sets the value $italic pObjFuncIn$$ as the pointer
to the objective function that is going to be optimized.

$end
*/

void Optimizer::setObjFunc( QuasiNewtonAnyBoxObj* pObjFuncIn )
{
  // Set the pointer to the objective function.  This must be done
  // before restart information can be read from the file because only
  // the objective function knows what information it requires for a
  // warm start.
  pObjFunc = pObjFuncIn;
}


/* 
-------------------------------------------------------------
   Read the restart information from the file
-------------------------------------------------------------
$begin readRestartInfoFromFile$$

$spell
  Bfgs
  pos
  readRestartInfoFromFile bool Optimizer
    const 
$$

$section Read the restart information from the file$$

$index Optimizer, restart information, readRestartInfoFromFile$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::readRestartInfoFromFile()/$$ $rend
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
Reads the restart information from the restart file that was 
specified at construction time.
$pre

$$
The information will only be read if the $italic readRestartInfo$$
flag was set equal to true at construction time.

$end
*/

void Optimizer::readRestartInfoFromFile()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace xercesc;


  //------------------------------------------------------------
  // See if restart information can be read.
  //------------------------------------------------------------

  // Check to see if restart information should be read from a file.
  if ( !readRestartInfo )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "Restart information can not be read from a file because the Optimizer read flag was not set.",
      __LINE__,
      __FILE__ );
  }

  // Check to see if the objective function has been set.
  if ( !pObjFunc )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "Restart information can not be read from a file because the objective function has not been set.",
      __LINE__,
      __FILE__ );
  }

  // Check to see if the state information is ready.
  if ( !( stateInfo.x && stateInfo.g && stateInfo.h ) ) 
  {
    throw SpkException( 
      SpkError::SPK_NOT_READY_WARM_START_ERR,
      "The optimizer state information was not allocated properly.",
      __LINE__,
      __FILE__ );
  }

  // Check to see if the original coordinate information is ready.
  if ( !( stateInfo.low && stateInfo.up && stateInfo.pos ) )
  {
    throw SpkException( 
      SpkError::SPK_NOT_READY_WARM_START_ERR,
      "The original coordinate information was not allocated properly.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Make sure that the restart file exists.
  //------------------------------------------------------------

  // This structure will contain information about the restart file.
  struct stat restartFileStat;

  // Check that the restart file exists.
  if ( stat( restartFileName.c_str(), &restartFileStat ) )
  {
    throw SpkException(
      SpkError::SPK_OPT_ERR,
      "Restart file does not exist and can not be opened for reading.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Prepare to use the XML parser.
  //------------------------------------------------------------

  // Initialize the XML parser workspace.
  try
  {
    XMLPlatformUtils::Initialize();
  }
  catch ( const XMLException& e )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "An XML exception was thrown during the initialization of the XML parser workspace.",
      __LINE__,
      __FILE__ );
  }
  catch ( ... )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "An unknown exception was thrown during the initialization of the XML parser workspace.",
      __LINE__,
      __FILE__ );
  }

  // Set this flag to indicate that the memory allocated in order
  // to parse the restart file must be freed.
  freeXMLMemory = true;

  // Set all of the XML strings.
  pPop_restart_infoTag             = XMLString::transcode( pop_restart_infoTag             );
  pIsTooManyIterAttribute          = XMLString::transcode( isTooManyIterAttribute          );
  pSaveStateAtEndOfOptAttribute    = XMLString::transcode( saveStateAtEndOfOptAttribute    );
  pThrowExcepIfMaxIterAttribute    = XMLString::transcode( throwExcepIfMaxIterAttribute    );
  pIsWarmStartPossibleAttribute    = XMLString::transcode( isWarmStartPossibleAttribute    );
  pIsWarmStartAttribute            = XMLString::transcode( isWarmStartAttribute            );
  pDidOptFinishOkAttribute         = XMLString::transcode( didOptFinishOkAttribute         );
  pIsBeginOfIterStateInfoAttribute = XMLString::transcode( isBeginOfIterStateInfoAttribute );
  pEpsilonTag                      = XMLString::transcode( epsilonTag                      );
  pNIterCompletedTag               = XMLString::transcode( nIterCompletedTag               );
  pStateInfo_nTag                  = XMLString::transcode( stateInfo_nTag                  );
  pStateInfo_bTag                  = XMLString::transcode( stateInfo_bTag                  );
  pStateInfo_rTag                  = XMLString::transcode( stateInfo_rTag                  );
  pStateInfo_fTag                  = XMLString::transcode( stateInfo_fTag                  );
  pStateInfo_xTag                  = XMLString::transcode( stateInfo_xTag                  );
  pStateInfo_gTag                  = XMLString::transcode( stateInfo_gTag                  );
  pStateInfo_hTag                  = XMLString::transcode( stateInfo_hTag                  );
  pStateInfo_mTag                  = XMLString::transcode( stateInfo_mTag                  );
  pStateInfo_lowTag                = XMLString::transcode( stateInfo_lowTag                );
  pStateInfo_upTag                 = XMLString::transcode( stateInfo_upTag                 );
  pStateInfo_posTag                = XMLString::transcode( stateInfo_posTag                );
  pStateInfo_acceptStepCountTag    = XMLString::transcode( stateInfo_acceptStepCountTag    );
  pYesStr                          = XMLString::transcode( yesStr                          );
  pValueTag                        = XMLString::transcode( valueTag                        );
  pLengthAttribute                 = XMLString::transcode( lengthAttribute                 );


  //------------------------------------------------------------
  // Parse the restart file.
  //------------------------------------------------------------

  // Construct the XML parser.
  XercesDOMParser* pParser = new XercesDOMParser;
  
  // Prepare the parser.
  pParser->setValidationScheme            ( XercesDOMParser::Val_Auto );
  pParser->setDoNamespaces                ( true );
  pParser->setDoSchema                    ( true );
  pParser->setValidationSchemaFullChecking( true );
  pParser->setCreateEntityReferenceNodes  ( true );

  // Create a DOM document tree that contains the contents of 
  // the XML restart file.
  try
  {
    pParser->parse( restartFileName.c_str() );
  }
  catch ( const XMLException& e )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "An XML exception was thrown during the parsing of the restart file.",
      __LINE__,
      __FILE__ );
  }
  catch ( const DOMException& e )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "A DOM exception was thrown during the parsing of the restart file.",
      __LINE__,
      __FILE__ );
  }
  catch ( ... )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "An unknown exception was thrown during the parsing of the restart file.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Get the DOM node that contains the restart information.
  //------------------------------------------------------------

  // Get the DOM document which contains the entire XML document.
  DOMDocument* pDocument = pParser->getDocument();

  // Make the root element of the DOM tree be the current element.
  pCurrElement = pDocument->getDocumentElement();

  // Get the current element's tag.
  const XMLCh* pCurrTag = pCurrElement->getTagName();

  // Check to be sure that the population restart information element
  // is the root element.
  if ( !XMLString::equals( pCurrTag, pPop_restart_infoTag ) )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "The pop_restart_info tag could not be found in the restart file.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Get the optimizer flags.
  //------------------------------------------------------------

  bool isWarmStartPossibleInFile;
  bool didOptFinishOkInFile;
  bool isBeginOfIterStateInfoInFile;

  // Get the optimizer flags that are related to the state information
  // but not those related to the behavior of quasiNewtonAnyBox.
  getAttributeValue( pIsWarmStartPossibleAttribute,    isWarmStartPossibleInFile    );
  getAttributeValue( pDidOptFinishOkAttribute,         didOptFinishOkInFile         );
  getAttributeValue( pIsBeginOfIterStateInfoAttribute, isBeginOfIterStateInfoInFile );


  //------------------------------------------------------------
  // See if a restart is possible.
  //------------------------------------------------------------

  // Check to see that a warm start is possible for the state
  // information in the restart file.
  if ( !isWarmStartPossibleInFile )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "The state information in the restart file can not be used to perform a restart.",
      __LINE__,
      __FILE__ );
  }

  // Check to see if the value for epsilon in the restart file is the
  // same as the current value.  The reason for this is that the path
  // the optimizer takes after it restarts depends on epsilon.
  double epsilonInFile;
  getValue<double>( pEpsilonTag, epsilonInFile );
  if ( epsilonInFile != epsilon )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "The epsilon tolerance in the restart file is not equal to the current value.",
      __LINE__,
      __FILE__ );
  }

  // Set the flags that are related to the state information but 
  // not those related to the behavior of quasiNewtonAnyBox.
  isWarmStartPossible    = isWarmStartPossibleInFile;
  didOptFinishOk         = didOptFinishOkInFile;
  isBeginOfIterStateInfo = isBeginOfIterStateInfoInFile;


  //------------------------------------------------------------
  // Get the stored optimizer state information.
  //------------------------------------------------------------

  getValue( pStateInfo_nTag, stateInfo.n );
  getValue( pStateInfo_bTag, stateInfo.b );
  getValue( pStateInfo_rTag, stateInfo.r );
  getValue( pStateInfo_fTag, stateInfo.f );

  getArray( pStateInfo_xTag, stateInfo.n, stateInfo.x );
  getArray( pStateInfo_gTag, stateInfo.n, stateInfo.g );

  getArray( pStateInfo_hTag, stateInfo.n * stateInfo.n, stateInfo.h );


  //------------------------------------------------------------
  // Get the stored original coordinate information.
  //------------------------------------------------------------

  getValue( pStateInfo_mTag, stateInfo.m );

  getArray( pStateInfo_lowTag, stateInfo.m, stateInfo.low );
  getArray( pStateInfo_upTag,  stateInfo.m, stateInfo.up  );

  getArray( pStateInfo_posTag, stateInfo.n, stateInfo.pos );


  //------------------------------------------------------------
  // Get the stored acceptance criteria information.
  //------------------------------------------------------------

  getValue( pStateInfo_acceptStepCountTag, stateInfo.acceptStepCount );


  //------------------------------------------------------------
  // Get any extra information required by the objective function.
  //------------------------------------------------------------

  pObjFunc->readRestartInfoFromFile();


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Free the memory allocated for the XML parser.
  delete pParser;

  // Free the memory allocated for all the XML strings.
  XMLString::release( &pPop_restart_infoTag             );
  XMLString::release( &pIsTooManyIterAttribute          );
  XMLString::release( &pSaveStateAtEndOfOptAttribute    );
  XMLString::release( &pThrowExcepIfMaxIterAttribute    );
  XMLString::release( &pIsWarmStartPossibleAttribute    );
  XMLString::release( &pIsWarmStartAttribute            );
  XMLString::release( &pDidOptFinishOkAttribute         );
  XMLString::release( &pIsBeginOfIterStateInfoAttribute );
  XMLString::release( &pEpsilonTag                      );
  XMLString::release( &pNIterCompletedTag               );
  XMLString::release( &pStateInfo_nTag                  );
  XMLString::release( &pStateInfo_bTag                  );
  XMLString::release( &pStateInfo_rTag                  );
  XMLString::release( &pStateInfo_fTag                  );
  XMLString::release( &pStateInfo_xTag                  );
  XMLString::release( &pStateInfo_gTag                  );
  XMLString::release( &pStateInfo_hTag                  );
  XMLString::release( &pStateInfo_mTag                  );
  XMLString::release( &pStateInfo_lowTag                );
  XMLString::release( &pStateInfo_upTag                 );
  XMLString::release( &pStateInfo_posTag                );
  XMLString::release( &pStateInfo_acceptStepCountTag    );
  XMLString::release( &pYesStr                          );
  XMLString::release( &pValueTag                        );
  XMLString::release( &pLengthAttribute                 );

  // Clean up the XML workspace.
  XMLPlatformUtils::Terminate();

  // Set this flag to indicate that the memory allocated in order
  // to parse the restart file has been freed.
  freeXMLMemory = false;
}


/* 
-------------------------------------------------------------
   Write the restart information to the file
-------------------------------------------------------------
$begin writeRestartInfoToFile$$

$spell
  Bfgs
  pos
  writeRestartInfoToFile bool Optimizer
    const 
$$

$section Write the restart information to the file$$

$index Optimizer, restart information, writeRestartInfoToFile$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::writeRestartInfoToFile()/$$ $rend
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
Writes the restart information to the restart file that was 
specified at construction time.
$pre

$$
The information will only be written if the $italic writeRestartInfo$$
flag was set equal to true at construction time.

$end
*/

void Optimizer::writeRestartInfoToFile()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // Check to see if restart information should be written to a file.
  if ( !writeRestartInfo )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "Restart information can not be written to a file because the Optimizer write flag was not set.",
      __LINE__,
      __FILE__ );
  }

  // Check to see if the objective function has been set.
  if ( !pObjFunc )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "Restart information can not be written to a file because the objective function has not been set.",
      __LINE__,
      __FILE__ );
  }

  // Check to see if the state information is ready.
  if ( !( stateInfo.x && stateInfo.g && stateInfo.h ) ) 
  {
    throw SpkException( 
      SpkError::SPK_NOT_READY_WARM_START_ERR,
      "The optimizer state information was not allocated properly.",
      __LINE__,
      __FILE__ );
  }

  // Check to see if the original coordinate information is ready.
  if ( !( stateInfo.low && stateInfo.up && stateInfo.pos ) )
  {
    throw SpkException( 
      SpkError::SPK_NOT_READY_WARM_START_ERR,
      "The original coordinate information was not allocated properly.",
      __LINE__,
      __FILE__ );
  }


  //----------------------------------------------------------
  // Prepare the temporary restart file.
  //----------------------------------------------------------

  // After the restart information has been written to the temporary
  // file, its name will be changed to that of the restart file.  That
  // way if the writing fails, it won't leave a restart file that is
  // only partially complete.
  string tempRestartFileName = restartFileName + ".tempCopy";

  // Erase the temporary restart file when it is opened for writing.
  tempRestartFileStream.open( tempRestartFileName.c_str(), 
    fstream::trunc | fstream::out );

  // Check that the temporary file was opened properly.
  if ( !tempRestartFileStream.good() )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "The temporary restart file could not be opened for writing.",
      __LINE__,
      __FILE__ );
  }
  
  // Prepare the temporary restart file stream so that it will write
  // symbolic bool values, i.e., "true" or "false".
  tempRestartFileStream << boolalpha;

  // Prepare the temporary restart file stream so that it will write
  // floating point values with as many digits as possible.
  tempRestartFileStream << scientific
    << setprecision( 2 + numeric_limits<double>::digits10 );


  //------------------------------------------------------------
  // Write the beginning XML tags and the optimizer flags.
  //------------------------------------------------------------

  // Write the XML version.
  writeXMLVersion();

  // Write the beginning of the start tag for the root element.
  const bool writeClosingBracket = false;
  const bool newLineAfter        = false;
  writeStartTag( pop_restart_infoTag, writeClosingBracket, newLineAfter );

  // Write the optimizer flags.
  writeAttribute( isTooManyIterAttribute,          isTooManyIter          );
  writeAttribute( saveStateAtEndOfOptAttribute,    saveStateAtEndOfOpt    );
  writeAttribute( throwExcepIfMaxIterAttribute,    throwExcepIfMaxIter    );
  writeAttribute( isWarmStartPossibleAttribute,    isWarmStartPossible    );
  writeAttribute( isWarmStartAttribute,            isWarmStart            );
  writeAttribute( didOptFinishOkAttribute,         didOptFinishOk         );
  writeAttribute( isBeginOfIterStateInfoAttribute, isBeginOfIterStateInfo );

  // Write the end of the start tag for the root element.
  writeStartTagFinalBracket();

  // Write the tolerance pararameter and the number of iterations that
  // were completed.
  writeValue( epsilonTag,        epsilon );
  writeValue( nIterCompletedTag, nIterCompleted );


  //------------------------------------------------------------
  // Write the stored optimizer state information.
  //------------------------------------------------------------

  writeValue( stateInfo_nTag, stateInfo.n );
  writeValue( stateInfo_bTag, stateInfo.b );
  writeValue( stateInfo_rTag, stateInfo.r );
  writeValue( stateInfo_fTag, stateInfo.f );

  writeArray( stateInfo_xTag, stateInfo.n, stateInfo.x );
  writeArray( stateInfo_gTag, stateInfo.n, stateInfo.g );

  writeArray( stateInfo_hTag, stateInfo.n * stateInfo.n, stateInfo.h );


  //------------------------------------------------------------
  // Write the stored original coordinate information.
  //------------------------------------------------------------

  writeValue( stateInfo_mTag, stateInfo.m );

  writeArray( stateInfo_lowTag, stateInfo.m, stateInfo.low );
  writeArray( stateInfo_upTag,  stateInfo.m, stateInfo.up  );

  writeArray( stateInfo_posTag, stateInfo.n, stateInfo.pos );


  //------------------------------------------------------------
  // Write the stored acceptance criteria information.
  //------------------------------------------------------------

  writeValue( stateInfo_acceptStepCountTag, stateInfo.acceptStepCount );


  //------------------------------------------------------------
  // Write any extra information required by the objective function.
  //------------------------------------------------------------

  pObjFunc->writeRestartInfoToFile();


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Write the end tag for the root element.
  writeEndTag( pop_restart_infoTag );

  // Close the temporary file.
  tempRestartFileStream.close();

  // Once all of the writing has been completed, change the name of
  // the temporary restart file.  This will replace any existing
  // restart files in an atomic manner.
  if ( rename( tempRestartFileName.c_str(), restartFileName.c_str() ) )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "The temporary restart file could not be renamed after it had been written.",
      __LINE__,
      __FILE__ );
  }

}


/* 
-------------------------------------------------------------
   Get the value of an attribute in the current element
-------------------------------------------------------------
$begin getAttributeValue$$

$spell
  bool
  Ch
  istream
  ostream
  Spk
  std
  str
  subelement
  Bfgs
  pos
  getAttributeValue Optimizer
    const 
$$

$section Get the Value of an Attribute in the Current Element$$

$cindex Get \the \Value \of \an Attribute \in \the \Current \Element$$
$index Optimizer, attribute value, getAttributeValue$$

$table
$bold Prototype$$ $cend
$syntax/template<class ValType> void Optimizer::getAttributeValue(
  const XMLCh* const  /pAttributeName/,
  /ValType/&            /valueOut/ )
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
Gets the value of an attribute that is contained in the current 
element, i.e.,
$code

    <current_element attribute_name="########">

$$
where the sequence of number signs (#) represents the sequence of
characters that constitute the attribute's value.
$pre

$$
This function assumes that the current element has been set
before this function is called.

$head Arguments$$
$syntax/
/ValType/
/$$
The type of the value.

$syntax/

/pAttributeName/
/$$
This argument specifies the name of the attribute in the 
current element for which to get the value.

$syntax/

/valueOut/
/$$
This argument will be set equal to the value that corresponds to 
the attribute's value after it has been converted to the type of 
$italic ValueType$$ using the std::istream extraction operator (>>).

$end
*/

template<class ValType> void Optimizer::getAttributeValue(
  const XMLCh* const  pAttributeName,
  ValType&            valueOut )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace xercesc;


  //------------------------------------------------------------
  // See if it is possible to get the attribute value.
  //------------------------------------------------------------

  // Check that the current element has been set.
  if ( !pCurrElement )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "The pointer to the current element in the XML file has not been set.",
      __LINE__,
      __FILE__ );
  }

  // Check that the current element has the requested attribute.
  if ( !pCurrElement->hasAttribute( pAttributeName ) )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "The current element in the XML file does not have the requested attribute.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Set the output value.
  //------------------------------------------------------------

  // Get the attribute's value as an XML string.
  const XMLCh* pXMLChValue = pCurrElement->getAttribute( pAttributeName );

  // Get the value as a C style string.
  char* pCharValue = XMLString::transcode( pXMLChValue );

  // Set the output value using the stream extraction operator (>>)
  // to convert the string to the appropriate value type.
  istringstream textToValueConverter( pCharValue );
  textToValueConverter >> boolalpha >> valueOut;

  // Free the memory allocated for the C string.  There was no memory
  // allocated here for the XML string because it belongs to the DOM
  // document and will be freed when it is done being used.
  XMLString::release( &pCharValue );
}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

template void Optimizer::getAttributeValue<bool>(
  const XMLCh* const  pAttributeName,
  bool&               valueOut );

template void Optimizer::getAttributeValue<double>(
  const XMLCh* const  pAttributeName,
  double&             valueOut );

template void Optimizer::getAttributeValue<int>(
  const XMLCh* const  pAttributeName,
  int&                valueOut );


/* 
-------------------------------------------------------------
   Get a single value from a subelement of the current element (XMLCh tag version)
-------------------------------------------------------------
$begin getValueXMLChTag$$

$spell
  bool
  Ch
  istream
  ostream
  Spk
  std
  str
  subelement
  Bfgs
  pos
  getValue Optimizer
    const 
$$

$section Get a Single Value from a Subelement of the Current Element (XMLCh Tag Version)$$

$cindex Get \a \Single \Value \from \a Subelement \of \the Current Element \(XMLCh \Tag \Version)$$
$index Optimizer, single value, getValue$$

$table
$bold Prototype$$ $cend
$syntax/template<class /ValType/>void Optimizer::getValue(
  const XMLCh* const  /pSubelementTag/,
  /ValType/&          /valueOut/ )
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
Gets the value from a single "value" element that 
is contained in a subelement of the current element, i.e.,
$code

    <current_element>
        ...
        <subelement>
            <value>########</value>
        </subelement>
        ...
    </current_element>

$$
where the sequence of number signs (#) represents the sequence of
characters that constitute a single value.
$pre

$$
This function assumes that the current element has been set
before this function is called.

$head Arguments$$
$syntax/
/ValType/
/$$
The type of the value.

$syntax/

/pSubelementTag/
/$$
This argument specifies the name of the subelement of the current
element that contains the single "value" element.

$syntax/

/valueOut/
/$$
This argument will be set equal to the value that 
corresponds to the contents of the "value" element.
after it has been converted to the type of $italic ValueType$$ 
using the std::istream extraction operator (>>).

$end
*/

template<class ValType>void Optimizer::getValue(
  const XMLCh* const  pSubelementTag,
  ValType&            valueOut )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace xercesc;


  //------------------------------------------------------------
  // See if it is possible to get the value.
  //------------------------------------------------------------

  // Check that the current element has been set.
  if ( !pCurrElement )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "The pointer to the current element in the XML file has not been set.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Get the subelement that contains the single "value" element.
  //------------------------------------------------------------

  // Get a list of the subelements of the current element that match
  // the requested subelement.
  DOMNodeList* pNodeList = 
    pCurrElement->getElementsByTagName( pSubelementTag );

  // Check to be sure there is at least one match.
  if ( pNodeList->getLength() == 0 )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "The requested subelement could not be found in the current XML element.",
      __LINE__,
      __FILE__ );
  }

  // Check to be sure there is only a single match.
  if ( pNodeList->getLength() != 1 )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "There is more than one of the requested subelements in the current XML element.",
      __LINE__,
      __FILE__ );
  }

  // Make the subelement that contains the single "value" element 
  // be the current element.
  pCurrElement = dynamic_cast<DOMElement*>( pNodeList->item( 0 ) );


  //------------------------------------------------------------
  // Get the single "value" element.
  //------------------------------------------------------------

  // Get a list of all of the "value" elements in this subelement.
  pNodeList = pCurrElement->getElementsByTagName( pValueTag );

  // Check to be sure there is at least one match.
  if ( pNodeList->getLength() == 0 )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "The value element could not be found in the current XML element.",
      __LINE__,
      __FILE__ );
  }

  // Check to be sure there is only a single match.
  if ( pNodeList->getLength() != 1 )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "There is more than one of the value elements in the current XML element.",
      __LINE__,
      __FILE__ );
  }

  // Make the single "value" element be the current element.
  pCurrElement = dynamic_cast<DOMElement*>( pNodeList->item( 0 ) );


  //------------------------------------------------------------
  // Get the single value.
  //------------------------------------------------------------

  // Get the "value" element's value as an XML string.
  const XMLCh* pXMLChValue = pCurrElement->getFirstChild()->getNodeValue();

  // Get the value as a C style string.
  char* pCharValue = XMLString::transcode( pXMLChValue );

  // Set the output value using the stream extraction operator (>>)
  // to convert the string to the appropriate value type.
  istringstream textToValueConverter( pCharValue );
  textToValueConverter >> boolalpha >> valueOut;

  // Free the memory allocated for the C string.  There was no memory
  // allocated here for the XML string because it belongs to the DOM
  // document and will be freed when it is done being used.
  XMLString::release( &pCharValue );


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Make the original current element be the current element 
  // again by backing up two levels in the DOM tree.
  pCurrElement = dynamic_cast<DOMElement*>( 
    pCurrElement->getParentNode()->getParentNode() );

}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

template void Optimizer::getValue<bool>(
  const XMLCh* const  pSubelementTag,
  bool&               valueOut );

template void Optimizer::getValue<double>(
  const XMLCh* const  pSubelementTag,
  double&             valueOut );

template void Optimizer::getValue<int>(
  const XMLCh* const  pSubelementTag,
  int&                valueOut );


/* 
-------------------------------------------------------------
   Get an array of values from a subelement of the current element (XMLCh tag version)
-------------------------------------------------------------
$begin getArrayXMLChTag$$

$spell
  bool
  Ch
  istream
  ostream
  Spk
  std
  str
  subelement
  Bfgs
  pos
  getArray Optimizer
    const 
$$

$section Get an Array of Values from a Subelement of the Current Element (XMLCh \Tag \Version)$$

$cindex Get \an Array \of \Values \from \a Subelement \of \the Current Element \(XMLCh \Tag \Version)$$
$index Optimizer, array, getArray$$

$table
$bold Prototype$$ $cend
$syntax/template<class /ValType/> void Optimizer::getArray(
  const XMLCh* const  /pSubelementTag/,
  int                 /nValue/,
  /ValType/* const      /arrayOut/ )
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
Gets the values from an array of "value" elements that 
are contained in a subelement of the current element, i.e.,
$code

    <current_element>
        ...
        <subelement length="##">
            <value>########</value>
            <value>########</value>
            ...
            <value>########</value>
        </subelement>
        ...
    </current_element>

$$
where the sequences of number signs (#) represent the sequences of
characters that constitute a single value.
$pre

$$
This function assumes that the current element has been set
before this function is called.

$head Arguments$$
$syntax/
/ValType/
/$$
The type of the array.

$syntax/

/pSubelementTag/
/$$
This argument specifies the name of the subelement of the current
element that contains the array of "value" elements.

$syntax/

/nValue/
/$$
This argument specifies the number of "value" elements to get.

$syntax/

/arrayOut/
/$$
This argument will be set equal to an array of values 
that correspond to the contents of the "value" elements
after they have been converted to the type of $italic ValueType$$ 
using the std::istream extraction operator (>>).

$end
*/

template<class ValType> void Optimizer::getArray(
  const XMLCh* const  pSubelementTag,
  int                 nValue,
  ValType* const      arrayOut )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  using namespace xercesc;


  //------------------------------------------------------------
  // See if it is possible to get the array.
  //------------------------------------------------------------

  // Check that the current element has been set.
  if ( !pCurrElement )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "The pointer to the current element in the XML file has not been set.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Get the subelement that contains the array of "value" elements.
  //------------------------------------------------------------

  // Get a list of the subelements of the current element that match
  // the requested subelement.
  DOMNodeList* pNodeList = 
    pCurrElement->getElementsByTagName( pSubelementTag );

  // Check to be sure there is at least one match.
  if ( pNodeList->getLength() == 0 )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "The requested subelement could not be found in the current XML element.",
      __LINE__,
      __FILE__ );
  }

  // Check to be sure there is only a single match.
  if ( pNodeList->getLength() != 1 )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "There is more than one of the requested subelements in the current XML element.",
      __LINE__,
      __FILE__ );
  }

  // Make the subelement that contains the array of "value" elements 
  // be the current element.
  pCurrElement = dynamic_cast<DOMElement*>( pNodeList->item( 0 ) );


  //------------------------------------------------------------
  // Get the array of "value" elements.
  //------------------------------------------------------------

  // Get a list of all of the "value" elements in this subelement.
  pNodeList = pCurrElement->getElementsByTagName( pValueTag );

  // Check to be sure there is at least one match.
  if ( pNodeList->getLength() == 0 )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "The array of value elements could not be found in the current XML element.",
      __LINE__,
      __FILE__ );
  }

  // Check that the number of "value" elements in the file matches the
  // number of values requested.
  if ( pNodeList->getLength() != nValue )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "The number of value elements is not equal to the requested number in the current XML element.",
      __LINE__,
      __FILE__ );
  }

  // Get the number of "value" elements that the subelement is
  // supposed to have.
  int nValueElemExpected;
  getAttributeValue( pLengthAttribute, nValueElemExpected );

  // Check that the number of "value" elements in the file matches the
  // number of values expected.
  if ( nValueElemExpected != pNodeList->getLength() )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "The length attribute is not equal to the number of value elements in the current XML element.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Get the array of values.
  //------------------------------------------------------------

  int i;

  ValType valueTemp;

  const XMLCh* pXMLChValue;
  char*  pCharValue;

  // Get all of the values.
  for ( i = 0; i < nValue; i++ )
  {
    // Make this "value" element be the current element.
    pCurrElement = dynamic_cast<DOMElement*>( pNodeList->item( i ) );
    
    // Get the "value" element's value as an XML string.
    pXMLChValue = pCurrElement->getFirstChild()->getNodeValue();
    
    // Get the value as a C style string.
    pCharValue = XMLString::transcode( pXMLChValue );

    // Set this output value using the stream extraction operator (>>)
    // to convert the string to the appropriate value type.
    istringstream textToValueConverter( pCharValue );
    textToValueConverter >> boolalpha >> arrayOut[i];

    // Free the memory allocated for the C string.  There was no memory
    // allocated here for the XML string because it belongs to the DOM
    // document and will be freed when it is done being used.
    XMLString::release( &pCharValue );
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Make the original current element be the current element 
  // again by backing up two levels in the DOM tree.
  pCurrElement = dynamic_cast<DOMElement*>( 
    pCurrElement->getParentNode()->getParentNode() );

}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

template void Optimizer::getArray<bool>(
  const XMLCh* const  pSubelementTag,
  int                 nValue,
  bool* const         arrayOut );

template void Optimizer::getArray<double>(
  const XMLCh* const  pSubelementTag,
  int                 nValue,
  double* const       arrayOut );

template void Optimizer::getArray<int>(
  const XMLCh* const  pSubelementTag,
  int                 nValue,
  int* const          arrayOut );


/* 
-------------------------------------------------------------
   Get a single value from a subelement of the current element (char tag version)
-------------------------------------------------------------
$begin getValueCharTag$$

$spell
  bool
  Ch
  istream
  ostream
  Spk
  std
  str
  subelement
  Bfgs
  pos
  getValue Optimizer
    const 
$$

$section Get a Single Value from a Subelement of the Current Element (Char Tag Version)$$

$cindex Get \a \Single \Value \from \a Subelement \of \the Current Element \(Char \Tag \Version)$$
$index Optimizer, single value, getValue$$

$table
$bold Prototype$$ $cend
$syntax/template<class /ValType/>void Optimizer::getValue(
  const char* const  /pSubelementTag/,
  /ValType/&         /valueOut/ )
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
Gets the value from a single "value" element that 
is contained in a subelement of the current element, i.e.,
$code

    <current_element>
        ...
        <subelement>
            <value>########</value>
        </subelement>
        ...
    </current_element>

$$
where the sequence of number signs (#) represents the sequence of
characters that constitute a single value.
$pre

$$
This function assumes that the current element has been set
before this function is called.

$head Arguments$$
$syntax/
/ValType/
/$$
The type of the value.

$syntax/

/pSubelementTag/
/$$
This argument specifies the name of the subelement of the current
element that contains the single "value" element.

$syntax/

/valueOut/
/$$
This argument will be set equal to the value that 
corresponds to the contents of the "value" element.
after it has been converted to the type of $italic ValueType$$ 
using the std::istream extraction operator (>>).

$end
*/

template<class ValType> void Optimizer::getValue(
  const char* const  pSubelementTag,
  ValType&           valueOut )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace xercesc;


  //------------------------------------------------------------
  // Get the value.
  //------------------------------------------------------------

  // Get the subelement tag as an XML string.
  XMLCh* pXMLChSubelementTag = XMLString::transcode( pSubelementTag );

  // Get the value from the single "value" element in the subelement.
  getValue( pXMLChSubelementTag, valueOut );


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Free the memory allocated for the XML string.
  XMLString::release( &pXMLChSubelementTag );

}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

template void Optimizer::getValue<bool>(
  const char* const  pSubelementTag,
  bool&              valueOut );

template void Optimizer::getValue<double>(
  const char* const  pSubelementTag,
  double&            valueOut );

template void Optimizer::getValue<int>(
  const char* const  pSubelementTag,
  int&               valueOut );


/* 
-------------------------------------------------------------
   Get an array of values from a subelement of the current element (char tag version)
-------------------------------------------------------------
$begin getArrayCharTag$$

$spell
  bool
  Ch
  istream
  ostream
  Spk
  std
  str
  subelement
  Bfgs
  pos
  getArray Optimizer
    const 
$$

$section Get an Array of Values from a Subelement of the Current Element (Char \Tag \Version)$$

$cindex Get \an Array \of \Values \from \a Subelement \of \the Current Element \(Char \Tag \Version)$$
$index Optimizer, array, getArray$$

$table
$bold Prototype$$ $cend
$syntax/template<class /ValType/> void Optimizer::getArray(
  const char* const  /pSubelementTag/,
  int                /nValue/,
  /ValType/* const     /arrayOut/ )
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
Gets the values from an array of "value" elements that 
are contained in a subelement of the current element, i.e.,
$code

    <current_element>
        ...
        <subelement length="##">
            <value>########</value>
            <value>########</value>
            ...
            <value>########</value>
        </subelement>
        ...
    </current_element>

$$
where the sequences of number signs (#) represent the sequences of
characters that constitute a single value.
$pre

$$
This function assumes that the current element has been set
before this function is called.

$head Arguments$$
$syntax/
/ValType/
/$$
The type of the array.

$syntax/

/pSubelementTag/
/$$
This argument specifies the name of the subelement of the current
element that contains the array of "value" elements.

$syntax/

/nValue/
/$$
This argument specifies the number of "value" elements to get.

$syntax/

/arrayOut/
/$$
This argument will be set equal to an array of values 
that correspond to the contents of the "value" elements
after they have been converted to the type of $italic ValueType$$ 
using the std::istream extraction operator (>>).

$end
*/

template<class ValType> void Optimizer::getArray(
  const char* const  pSubelementTag,
  int                nValue,
  ValType* const     arrayOut )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace xercesc;


  //------------------------------------------------------------
  // Get the array of values.
  //------------------------------------------------------------

  // Get the subelement tag as an XML string.
  XMLCh* pXMLChSubelementTag = XMLString::transcode( pSubelementTag );

  // Get the array of values from the "value" elements in the subelement.
  getArray( pXMLChSubelementTag, nValue, arrayOut );


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Free the memory allocated for the XML string.
  XMLString::release( &pXMLChSubelementTag );

}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

template void Optimizer::getArray<bool>(
  const char* const  pSubelementTag,
  int                nValue,
  bool* const        arrayOut );

template void Optimizer::getArray<int>(
  const char* const  pSubelementTag,
  int                nValue,
  int* const         arrayOut );

template void Optimizer::getArray<double>(
  const char* const  pSubelementTag,
  int                nValue,
  double* const      arrayOut );


/* 
-------------------------------------------------------------
   Write the XML version to the restart file
-------------------------------------------------------------
$begin writeXMLVersion$$

$spell
  bool
  Ch
  istream
  ostream
  Spk
  std
  str
  subelement
  Bfgs
  pos

writeXMLVersion Optimizer
$$

$section Write the XML version to the Restart File$$

$cindex Write \the XML version \to \the \Restart \File$$
$index Optimizer, XML version, writeXMLVersion$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::writeXMLVersion()/$$
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
Writes to the restart file the version of XML that is being used.

$end
*/

void Optimizer::writeXMLVersion()
{
  using namespace std;

  tempRestartFileStream << "<?xml version=\"1.0\"?>" << endl;
}


/* 
-------------------------------------------------------------
   Write an attribute name and value to the restart file
-------------------------------------------------------------
$begin writeAttribute$$

$spell
  bool
  Ch
  istream
  ostream
  Spk
  std
  str
  subelement
  Bfgs
  pos
  writeAttribute Optimizer
    const 
$$

$section Write an Attribute Name and Value to the Restart File$$

$cindex Write \an Attribute Name \and Value \to \the \Restart \File$$
$index Optimizer, attribute value, writeAttribute$$

$table
$bold Prototype$$ $cend
$syntax/template<class /ValType/>void Optimizer::writeAttribute(
  const char* const  /pAttributeName/,
  /ValType/            /valueIn/ )
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
Writes to the restart file the name and value of an attribute 
that is contained in the current element's start tag, i.e.,
$code

    <current_element attribute_name="########">

$$
where the sequence of number signs (#) represents the sequence of
characters that constitute the attribute's value.
$pre

$$

This function assumes that the current element start tag 
except for its final angle bracket (>) has been written to 
the restart file before this function is called 
and that its final angle bracket will be written after.

$head Arguments$$
$syntax/
/ValType/
/$$
The type of the value.

$syntax/

/pAttributeName/
/$$
This argument specifies the name of the attribute to write.

$syntax/

/valueIn/
/$$
This argument will be written as the value of the attribute using the
std::ostream insertion operator (<<), which converts it from type
$italic ValueType$$ to a sequence of characters.

$end
*/

template<class ValType> void Optimizer::writeAttribute(
  const char* const  pAttributeName,
  ValType            valueIn )
{
  using namespace std;

  // Write the attribute's name and value.
  tempRestartFileStream << " " << pAttributeName << "=\"" 
                        << valueIn
                        << "\"";
}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

template void Optimizer::writeAttribute<bool>(
  const char* const  pAttributeName,
  bool               valueIn );

template void Optimizer::writeAttribute<double>(
  const char* const  pAttributeName,
  double             valueIn );

template void Optimizer::writeAttribute<int>(
  const char* const  pAttributeName,
  int               valueIn );


/* 
-------------------------------------------------------------
   Write a subelement of the current element that contains a single value (char tag version)
-------------------------------------------------------------
$begin writeValue$$

$spell
  bool
  Ch
  istream
  ostream
  Spk
  std
  str
  subelement
  Bfgs
  pos
  writeValue Optimizer
    const 
$$

$section Write a Subelement of the Current Element that Contains a Single Value (Char Tag Version)$$

$cindex Write \a Subelement \of \the Current Element \that \Contains \a \Single \Value \(Char \Tag \Version)$$
$index Optimizer, single value, writeValue$$

$table
$bold Prototype$$ $cend
$syntax/template<class /ValType/>void Optimizer::writeValue(
  const char* const  /pSubelementTag/,
  /ValType/            /valueIn/ )
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
Writes to the restart file a subelement of the current element
that contains a single "value" element, i.e.,
$code

    <current_element>
        ...
        <subelement>
            <value>########</value>
        </subelement>
        ...
    </current_element>

$$
where the sequence of number signs (#) represents the sequence of
characters that constitute a single value.
$pre

$$
This function assumes that the current element start tag has 
been written to the restart file before this function is called
and that its end tag will be written after.

$head Arguments$$
$syntax/
/ValType/
/$$
The type of the value.

$syntax/

/pSubelementTag/
/$$
This argument specifies the name of the subelement of the current
element that contains the single "value" element.

$syntax/

/valueIn/
/$$
This argument will be written as the contents of the single "value"
element using the std::ostream insertion operator (<<), which converts
it from type $italic ValueType$$ to a sequence of characters.

$end
*/

template<class ValType> void Optimizer::writeValue(
  const char* const  pSubelementTag,
  ValType            valueIn )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Write the subelement and its single "value" element.
  //------------------------------------------------------------

  const bool writeClosingBracket = true;
  const bool newLineAfter        = false;
  const bool printSpacesBefore   = false;

  writeStartTag( pSubelementTag );
  writeStartTag( valueTag, writeClosingBracket, newLineAfter );

  // Use the ostream insertion operator to convert the value to a
  // sequence of characters.
  tempRestartFileStream << valueIn;

  writeEndTag( valueTag, printSpacesBefore );
  writeEndTag( pSubelementTag );
}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

template void Optimizer::writeValue<bool>(
  const char* const  pSubelementTag,
  bool               valueIn );

template void Optimizer::writeValue<double>(
  const char* const  pSubelementTag,
  double             valueIn );

template void Optimizer::writeValue<int>(
  const char* const  pSubelementTag,
  int                valueIn );


/* 
-------------------------------------------------------------
   Write a subelement of the current element that contains an array of values (char tag version)
-------------------------------------------------------------
$begin writeArray$$

$spell
  bool
  Ch
  istream
  ostream
  Spk
  std
  str
  subelement
  Bfgs
  pos
  writeArray Optimizer
    const 
$$

$section Write a Subelement of the Current Element that Contains an Array of Values (Char Tag Version)$$

$cindex Write \a Subelement \of \the Current Element \that \Contains \an \Array of \Values \(Char \Tag \Version)$$
$index Optimizer, array of values, writeArray$$

$table
$bold Prototype$$ $cend
$syntax/template<class /ValType/> void Optimizer::writeArray(
  const char* const    /pSubelementTag/,
  int                  /nValue/,
  const /ValType/* const /arrayIn/ )
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
Writes to the restart file a subelement of the current element
that contains an array of "value" elements, i.e.,
$code

    <current_element>
        ...
        <subelement length="##">
            <value>########</value>
            <value>########</value>
            ...
            <value>########</value>
        </subelement>
        ...
    </current_element>

$$
where the sequences of number signs (#) represent the sequences of
characters that constitute a single value.
$pre

$$
This function assumes that the current element start tag has 
been written to the restart file before this function is called
and that its end tag will be written after.

$head Arguments$$
$syntax/
/ValType/
/$$
The type of the array.

$syntax/

/pSubelementTag/
/$$
This argument specifies the name of the subelement of the current
element that contains the array of "value" elements.

$syntax/

/nValue/
/$$
This argument specifies the number of "value" elements to write.

$syntax/

/arrayIn/
/$$

This argument specifies the array of values that will be written as
the contents of the "value" elements using the std::ostream insertion
operator (<<), which converts it from type $italic ValueType$$ to a
sequence of characters.

$end
*/

template<class ValType> void Optimizer::writeArray(
  const char* const    pSubelementTag,
  int                  nValue,
  const ValType* const arrayIn )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Write the subelement and its array of "value" elements.
  //------------------------------------------------------------

  int i;

  bool       writeClosingBracket = false;
  const bool newLineAfter        = false;
  const bool printSpacesBefore   = false;

  // Write the array start tag with an attribute that specifies
  // the number of "value" elements.
  writeStartTag( pSubelementTag, writeClosingBracket, newLineAfter );
  writeAttribute( lengthAttribute, nValue );
  writeStartTagFinalBracket();

  writeClosingBracket = true;

  // Write all of the values.
  for ( i = 0; i < nValue; i++ )
  {
    writeStartTag( valueTag, writeClosingBracket, newLineAfter );

    // Use the ostream insertion operator to convert the value to a
    // sequence of characters.
    tempRestartFileStream << arrayIn[i];
    
    writeEndTag( valueTag, printSpacesBefore );
  }

  writeEndTag( pSubelementTag );
}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

template void Optimizer::writeArray<bool>(
  const char* const    pSubelementTag,
  int                  nValue,
  const bool* const    arrayIn );

template void Optimizer::writeArray<double>(
  const char* const    pSubelementTag,
  int                  nValue,
  const double* const  arrayIn );

template void Optimizer::writeArray<int>(
  const char* const    pSubelementTag,
  int                  nValue,
  const int* const     arrayIn );


/* 
-------------------------------------------------------------
   Write an XML start tag.
-------------------------------------------------------------
$begin writeStartTag$$

$spell
  bool
  Ch
  istream
  ostream
  Spk
  std
  str
  subelement
  Bfgs
  pos
  writeStartTag Optimizer
    const 
$$

$section Write an XML Start Tag$$

$cindex Write \an \XML Start Tag$$
$index Optimizer, start tag, writeStartTag$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::writeStartTag( 
  const char* const /pTag/,
  bool              /writeClosingBracket/ = true,
  bool              /newLineAfter/ = true )
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
Writes the start tag for an XML element.

$head Arguments$$
$syntax/
/pTag/
/$$
This argument specifies the name of the start tag to write.

$syntax/

/writeClosingBracket/
/$$
If this argument is equal to true, then the closing bracket (>) 
will be written after the start tag name.

$syntax/

/newLineAfter/
/$$
If this argument is equal to true, then a new line will be written
after the start tag.

$end
*/

void Optimizer::writeStartTag( 
  const char* const pTag,
  bool              writeClosingBracket,
  bool              newLineAfter )
{
  using namespace std;

  // Write the start tag with the proper number of spaces before it
  // for its level in the XML tree.
  tempRestartFileStream << setw( xmlLevel * xmlTab ) << ""
                        << "<" << pTag;
  if ( writeClosingBracket )
  {
    tempRestartFileStream << ">";
  }
  if ( newLineAfter )
  {
    tempRestartFileStream << endl;
  }

  // Increase the level in the XML tree.
  xmlLevel++;
}


/* 
-------------------------------------------------------------
   Write the final bracket for an XML start tag.
-------------------------------------------------------------
$begin writeStartTagFinalBracket$$

$spell
  bool
  Ch
  istream
  ostream
  Spk
  std
  str
  subelement
  Bfgs
  pos
  writeStartTagFinalBracket Optimizer
    const 
$$

$section Write the Final Bracket for an XML Start Tag$$

$cindex Write \the \Final Bracket \for \an \XML Start Tag$$
$index Optimizer, start tag final bracket, writeStartTagFinalBracket$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::writeStartTagFinalBracket( bool /newLineAfter/ = true )
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
Writes the final bracket for the start tag for an XML element.

$head Arguments$$
$syntax/
/newLineAfter/
/$$
If this argument is equal to true, then a new line will be written
after the final bracket for the start tag.

$end
*/

void Optimizer::writeStartTagFinalBracket( bool newLineAfter )
{
  using namespace std;

  // Write the final bracket for the start tag.
  tempRestartFileStream << ">";
  if ( newLineAfter )
  {
    tempRestartFileStream << endl;
  }
}


/* 
-------------------------------------------------------------
   Write an XML end tag.
-------------------------------------------------------------
$begin writeEndTag$$

$spell
  bool
  Ch
  istream
  ostream
  Spk
  std
  str
  subelement
  Bfgs
  pos
  writeEndTag Optimizer
    const 
$$

$section Write an XML End Tag$$

$cindex Write \an \XML End Tag$$
$index Optimizer, end tag, writeEndTag$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::writeEndTag( 
  const char* const /pTag/,
  bool              /printSpacesBefore/ = true )
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
Writes the end tag for an XML element.

$head Arguments$$
$syntax/
/pTag/
/$$
This argument specifies the name of the end tag to write.

$syntax/

/printSpacesBefore/
/$$
If this argument is equal to true, then a new line will be written
before the end tag.

$end
*/

void Optimizer::writeEndTag(
  const char* const pTag,
  bool              printSpacesBefore )
{
  using namespace std;

  // Decrease the level in the XML tree.
  xmlLevel--;

  // Write the end tag with the proper number of spaces before it
  // for its level in the XML tree.
  if ( printSpacesBefore )
  {
    tempRestartFileStream << setw( xmlLevel * xmlTab ) << "";
  }
  tempRestartFileStream << "</" << pTag << ">"
                        << endl;
}


/* 
-------------------------------------------------------------
   Determine if there is any error information
-------------------------------------------------------------
$begin isThereErrorInfo$$

$spell
  bool
  Ch
  istream
  ostream
  Spk
  std
  str
  subelement
  Bfgs
  pos
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

$end
*/

bool Optimizer::isThereErrorInfo() const
{
  // Return true if there was an error during the optimization
  // and if the current state information is from the beginning
  // of the last iteration.
  return ( !didOptFinishOk && isBeginOfIterStateInfo );
}


/* 
-------------------------------------------------------------
   Get the error information
-------------------------------------------------------------
$begin getErrorInfo$$

$spell
  bool
  Ch
  istream
  ostream
  Spk
  std
  str
  subelement
  Bfgs
  pos
  getErrorInfo errorInfo Optimizer
    const 
$$

$section Get error information$$

$index Optimizer, error information, getErrorInfo$$

$table
$bold Prototype$$ $cend
$syntax/void Optimizer::getErrorInfo( 
  const std::string  /headerStr/,
  std::string&       /messageStr/,
  unsigned int       /lineNumber/,
  const char* const  /fileName/ ) const
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

$end
*/

// Get the error information.
void Optimizer::getErrorInfo( 
  const std::string  headerStr,
  std::string&       messageStr,
  unsigned int       lineNumber,
  const char* const  fileName ) const
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

  // Check that there is state information to get.
  if ( stateInfo.n == 0 )
  {
    throw SpkException( 
      SpkError::SPK_OPT_ERR,
      "There is currently no stored optimizer state information to generate the error information.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Prepare to get the error information.
  //------------------------------------------------------------

  // Create the message stream with the header at the beginning.
  ostringstream message;
  message << headerStr;

  QN01Box::Memory<double> memoryDbl( stateInfo.m );
  double* diff = memoryDbl( stateInfo.m );

  double xOrig;
  double gOrig;
  double gProjOrig;


  //------------------------------------------------------------
  // Get the parameter and gradient information.
  //------------------------------------------------------------

  bool isAnyElemConstrained = false;

  int colWidth1 = 9 - 2;
  int colWidth2 = 12 + 2;
  int colWidth3 = 16;
  int colWidth4 = colWidth2;
  int colWidth5 = colWidth2;
  string colSpacer = "  ";

  message << "An error occurred in the optimizer for the iteration that" << endl;
  message << "started with the following parameters:" << endl;
  message << endl;
  message << "Parameter      Value       At or Near Bound?     Gradient     Proj. Gradient" << endl;
  message << "---------  --------------  -----------------  --------------  --------------" << endl;

  int i;
  int nObjParFree = 0;
  double maxDistFromBound_i;

  for ( i = 0; i < stateInfo.m; i++ )
  {
    //------------------------------------------------------------
    // Convert the state information back to the original coordinates.
    //------------------------------------------------------------

    // Calculate the distance between the original bounds.
    diff[i] = stateInfo.up[i] - stateInfo.low[i];

    // Set the maximum distance allowed from either bound.
    maxDistFromBound_i = epsilon * diff[i];

    // Calculate the parameter and gradient in the original coordinates.
    if ( diff[i] != 0.0 )
    {
      xOrig = stateInfo.low[i] + diff[i] * stateInfo.x[nObjParFree];
      gOrig = stateInfo.g[nObjParFree] / diff[i];

      nObjParFree++;
    }
    else
    {
      xOrig = stateInfo.low[i];
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
      message << "Both (at)    ";
      isAnyElemConstrained = true;
    }
    else if ( xOrig == stateInfo.low[i] )
    {
      message << "Lower (at)   ";
    }
    else if ( xOrig == stateInfo.up[i] )
    {
      message << "Upper (at)   ";
    }
    else if ( diff[i] <= maxDistFromBound_i ) 
    {
      message << "Both (near)  ";
    }
    else if ( xOrig - stateInfo.low[i] <= maxDistFromBound_i )
    {
      message << "Lower (near) ";
    }
    else if ( stateInfo.up[i] - xOrig <= maxDistFromBound_i )
    {
      message << "Upper (near) ";
    }
    else
    {
      message << "No           ";
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


/*
-------------------------------------------------------------
   The inserter
-------------------------------------------------------------
$begin OptimizerInserter$$

$spell
  Bfgs
  pos
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


/*
-------------------------------------------------------------
   The extractor
-------------------------------------------------------------
$begin OperatorExtractor$$

$spell
  Bfgs
  pos
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

// Stream extraction operator
std::istream& operator>>(std::istream& stream, Optimizer& right)
{
    char title[40];
    bool failed = false;

    stream >> title;
    if (strcmp(title, "epsilon" ) == 0)
        stream >> right.epsilon;
    else
        failed = true;

    stream >> title;
    if (strcmp(title, "nMaxIter") == 0)
        stream >> right.nMaxIter;
    else
        failed = true;
    
    stream >> title;
    if (strcmp(title, "level") == 0)
        stream >> right.level;
    else
        failed = true;

    stream >> title;
    if (strcmp(title, "isTooManyIter") == 0)
        stream >> right.isTooManyIter;
    else
        failed = true;

    stream >> title;
    if (strcmp(title, "nIterCompleted") == 0 )
        stream >> right.nIterCompleted;
    else
        failed = true;

    if (failed)
    {
        throw SpkException(
            SpkError::SPK_USER_INPUT_ERR,
            "Incorrect data format",
            __LINE__,
            __FILE__);
    }

    return stream;
}

