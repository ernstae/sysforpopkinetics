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
 * File: quasiNewtonAnyBox.cpp
 *
 *
 * Minimizes an arbitrary smooth function subject to simple bounds on
 * the variables using a quasi-Newton method. 
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: quasiNewtonAnyBox
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin quasinewtonanybox$$
$spell 
  bool
  cmath
  const
  cout
  dat
  diag
  dir
  dmat
  dvec
  dx
  elsq
  endl
  epsilon
  fmin
  fval
  grad
  Info
  int
  ios
  iostream
  iomanip
  Iter
  itr
  Kuhn
  Max
  namespace
  nMaxIter
  ndir
  nr
  nlp
  obj
  ok
  optim
  Pars
  Param
  pd
  pdrow
  pdvec
  pk
  pow
  Rel
  rowdim
  seq
  setiosflags
  setprecision
  Spk
  quasiNewton
  QuasiNewton
  std
  stdout
  th
  tol
  ucc
  Varbl
  Vi
  xmin
  optimizer
  Optimizer optimizer
$$

$section Quasi-Newton Optimization with Derivatives and Box Constraints$$

$index quasiNewtonAnyBox$$
$cindex quasi-Newton optimization 
  \with derivatives \and box constraints$$

$table
$bold Prototype:$$   $cend  
$syntax/void quasiNewtonAnyBox( 
  QuasiNewtonAnyBoxObj&  /objective/,
  Optimizer&             /optimizer/,
  const DoubleMatrix&    /dvecXLow/,
  const DoubleMatrix&    /dvecXUp/,
  const DoubleMatrix&    /dvecXIn/,
  DoubleMatrix*          /pdvecXOut/,
  double*                /pdFOut/,
  DoubleMatrix*          /pdrowF_xOut/ )
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
Uses a quasi-Newton interior-trust method with exact complimentarity 
to solve the problem
$math%
    \minimize f(x) \with \respect \to x
    \subject \to xLow \le x \le xUp  .
%$$
In order to solve the above problem, this function scales the elements 
of $math%x%$$ so that they are constrained to the interval 
$math%[0, 1]%$$.  It then solves the scaled problem
$math%
    \minimize fScaled(y) \with \respect \to y
    \subject \to 0 \le y \le 1 ,
%$$
where
$math%
    fScaled(y) = f(x) .
%$$
The elements of the vectors $math%x%$$ and $math%y%$$ are related 
as follows: 
$math%
    y(i) = [ x(i) - xLow(i) ] / [ xUp(i) - xLow(i) ] .
%$$
Note that if $math%xLow(i) = xUp(i)%$$, then $math%y(i)%$$ is 
constrained to be $math%0%$$.

$head Return Value$$
If the convergence criteria specified below for the $italic optimizer$$
argument is satisfied, then the output value pointers are set to point
to their respective results.
If the convergence criteia is not satisfied, then an 
$xref/SpkException//exception/$$ will be thrown.  
The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.
In the case of the maximum number of iterations being exhausted,
an exception may or may not be thrown depending on the value for
the $italic optimizer.throwExcepIfMaxIter$$ parameter.

$head Arguments$$
$syntax/
objective
/$$
This $code QuasiNewtonAnyBoxObj$$ function object is used to evaluate the 
objective function $math%f(x)%$$ and the gradient of the objective 
function $math%f_x(x)%$$ for a particular value of $math%x%$$.
$pre

$$
The class $code QuasiNewtonAnyBoxObj$$ is an abstract base class,
and $italic objective$$ is an instance of a concrete subclass
of $code QuasiNewtonAnyBoxObj$$ that is specialized for this objective
function.
Any values that are required to evaluate the objective function
and its gradient can be stored as private data in the class
and initialized when $italic objective$$ is constructed.
$pre

$$
The concrete subclass that is the type of $italic objective$$ 
must define the following pure virtual member functions of the 
abstract base class $code QuasiNewtonAnyBoxObj$$.

$subhead Objective Function$$
The function that evaluates the objective function has this prototype:
$syntax%
    virtual void function( const DoubleMatrix& /dvecXIn/, double* /pdFOut/ ) = 0;
%$$
If there is a problem during the evaluation of this function,
it should be indicated by throwing an exception.
The function $code quasiNewtonAnyBox$$ will catch any exception,
and then rethrow it as an $xref/SpkException//SpkException/$$.
$pre

$$
If no exceptions are thrown, then on return the scalar value
pointed to by $italic pdFout$$ will be equal to the objective
function at $italic dvecXIn$$.
The vector $italic dvecXIn$$ has the same dimension as the vector 
$italic dvecXLow$$ described below.

$subhead Gradient$$
The function that evaluates the gradient of the objective function 
has this prototype:
$syntax%
    virtual void gradient( DoubleMatrix* /pdrowF_xOut/ ) const = 0;
%$$
If there is a problem during the evaluation of this function,
it should be indicated by throwing an exception.
The function $code quasiNewtonAnyBox$$ will catch any exception,
and then rethrow it as an $xref/SpkException//SpkException/$$.
$pre

$$
Note that this function does not have an argument that specifies 
the value at which the gradient should be evaluated.
The reason for this is that during the optimization process, i.e.,
during the call to $code quasiNewtonAnyBox$$, the objective function
is alway evaluated at the same $italic dvecXIn$$ value directly before
the gradient of the objective function is evaluated.
$pre

$$
If no exceptions are thrown, then on return the $code DoubleMatrix$$
pointed to by $italic pdrowF_xOut$$ will be equal to the gradient of
the objective evaluated at the value of $italic dvecXIn$$ from the
previous call to the objective function.

$syntax/

/optimizer/
/$$
This $xref/Optimizer//Optimizer/$$ object contains the information 
that controls the optimization process.
$pre

$$
It has attributes for holding the optimization state information 
that is required to perform a warm start, i.e., to start the
optimization process using a previous set of optimizer state
information.
If restart is intended, then before this function is called 
the member function of the Optimizer object, setupWarmStart(), 
must be called in order to set up the warm start information.
$pre

$$
Most of the optimizer information is accessible via public get functions,
e.g., the value epsilon is returned by the function getEpsilon.
The following subsections specify how this function uses each
of the elements of the Optimizer object that is accessed in
this way.

$subhead optimizer.epsilon$$
This real number is used to specify the convergence criteria
for the optimizer.
It must be greater than $math%0.0%$$.
$pre

$$
A  value $math%xOut%$$ is accepted as an estimate for 
$math%xHat%$$ if 
$math%
	abs( xOut - xHat ) \le epsilon ( xUp - xLow )  ,
%$$
where $math%abs%$$ is the element-by-element absolute value function
and $math%xHat%$$ is the true minimizer of the objective function.
Since $math%xHat%$$ is unknown, the optimization algorithm must 
estimate the left hand side of this inequality.
$pre

$$
Note that if $italic nMaxIter$$ is set to zero, then $math%xIn%$$ is 
accepted as the estimate for $math%xHat%$$.

$subhead optimizer.nMaxIter$$
This integer must be greater than or equal to zero.
It specifies the maximum number of 
iterations to attempt before giving up on convergence.
If it is equal to zero, then the input
value for $math%x%$$ is accepted as the final value, and any requested output
values are evaluated at that final value.
In this case, a warm start will not be performed in order to ensure
that the objective function and its gradient will be
evaluated at the input value for $math%x%$$.

$subhead optimizer.traceLevel$$
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
parameter for the optimizer $xref/QuasiNewton01Box//QuasiNewton01Box/$$.

$subhead optimizer.nIterCompleted$$
This integer scalar holds the number of iteration that have been 
completed in the optimizer.

$subhead optimizer.isTooManyIter$$
This flag indicates that if the too-many-iteration failure has occurred.  

$subhead optimizer.saveStateAtEndOfOpt$$
This flag indicates if the state information required for a warm start
should be saved at the end of the optimization process.
This state information will not be saved if the optimization process
results in an exception being thrown by $code quasiNewtonAnyBox$$.

$subhead optimizer.throwExcepIfMaxIter$$
This flag indicates if the optimizer should throw an exception when
the maximum number of iterations is exhausted.
If this parameter is true, then when
the maximum number of iterations is exhausted, an exception will
be thrown and the output values for this function will not be set.
Otherwise, the calling program will
need to check the parameter isTooManyIter to see if the 
maximum number of iterations was exhausted.

$subhead optimizer.isSubLevelOpt$$
This flag indicates that if the optimizer is for a sub level optimization.  
It is for SPK internal use only.

$subhead optimizer.isWarmStart$$
This flag indicates that if the optimization should run a warm start.  

$subhead optimizer.stateInfo$$
This $code StateInfo$$ object contains the optimizer state information
required to perform a warm start.
Each of its elements is described separately below.

$subhead optimizer.stateInfo.n$$
The element $italic n$$ specifies the number of components
in the element vector $italic x$$.

$subhead optimizer.stateInfo.r$$
The element $italic r$$ contains the current trust region radius
(as an infinity norm bound on the step size).

$subhead optimizer.stateInfo.f$$
The element $italic f$$ contains the value for $math%f(x)%$$
at the point $math%x%$$.

$subhead optimizer.stateInfo.x$$
The element $italic x$$ is a vector of length $italic n$$.
It specifies the point at which the objective function, 
its gradient, and its Hessian were evaluated.

$subhead optimizer.stateInfo.g$$
The vector $italic g$$ must have length $math%n%$$.
It contains the gradient of $math%f(x)%$$
at the point $math%x%$$.

$subhead optimizer.stateInfo.h.$$
The vector $italic h$$ must have length $math%n^2%$$.
It contains an approximation for the hessian of $math%f(x)%$$
at the point $math%x%$$.

$syntax/

/dvecXLow/
/$$
The $code DoubleMatrix$$ $italic dvecXLow$$ contains the column vector 
$math%xLow%$$.  It specifies the lower limit for the box constraints 
in the problem.  

$syntax/

/dvecXUp/
/$$
The $code DoubleMatrix$$ $italic dvecXUp$$ contains the column vector 
$math%xUp%$$.  It specifies the upper limit for the box constraints 
in the problem, and it has the same dimensions as $italic dvecXLow$$.

$syntax/

/dvecXIn/
/$$
The $code DoubleMatrix$$ $italic dvecXIn$$ contains the column vector 
$math%xIn%$$.  It specifies the initial estimate for the argument that 
solves the problem, and it has the same dimension as $italic dvecXLow$$.  
The initial estimate satisfies the box constraints
$math%xLow \le xIn \le xUp%$$.

$syntax/

/pdvecXOut/
/$$
If the return value for $code quasiNewtonAnyBox$$ is true, and 
if $italic pdvecXOut$$ is not equal to zero, then on output the 
$code DoubleMatrix$$ pointed to by $italic pdvecXOut$$ will contain 
the column vector $math%xOut%$$.  It is the final approximation for 
the solution to the problem, and
it has the same dimensions as $italic xLow$$.
The final approximation satisfies the box constraints
$math%xLow \le xOut \le xUp%$$.  
Note that the $code DoubleMatrix$$ pointed to by $italic pdvecXOut$$ 
must be constructed by the user.

$syntax/

/pdFOut/
/$$
If the return value for $code quasiNewtonAnyBox$$ is true, i.e., the algorithm
converged successfully, and 
if $italic pdFOut$$ is not equal to null, then on output the $code 
double$$ value pointed to by $italic pdFOut$$ will be equal to the 
value of the objective function $math%f(x)%$$ at the final iteration.
Note that the user must allocate memory for the value pointed 
to by $italic pdFOut$$.

$syntax/

/pdrowF_xOut/
/$$
If the return value for $code quasiNewtonAnyBox$$ is true, i.e., the algorithm
converged successfully, and 
if $italic pdrowF_xOut$$ is not equal to null, then on output the matrix
value pointed to by $italic pdrowF_xOut$$ will be equal to the 
value of the gradient of objective function $math%f(x)%$$ with respect
to the variable at the final iteration.
Note that the user must allocate memory for the value pointed 
to by $italic pdrowF_xOut$$.


$head Example$$
There is currently no example for this function.

$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <iostream>
#include <fstream>
#include <strstream>
#include <iomanip>
#include <cassert>
#include <cmath>
#include <cfloat>
#include <vector>
//#include <direct.h>
extern "C" {
#include "nag.h"
#include "nag_types.h"
#include "nag_stdlib.h"
#include "nagf04.h"
}
#include "transpose.h"
#include "quasiNewtonAnyBox.h"
#include "DoubleMatrix.h"
#include "isLessThanOrEqualTo.h"
#include "allTrue.h"
#include "SpkException.h"

using SPK_VA::valarray;


/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{

} // [End: unnamed namespace]



/*------------------------------------------------------------------------
 * Local class declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  //
  // Class: QuasiNewton01BoxObj
  //
  // 
  // Objects of this class are passed to QuasiNewton01Box in order to
  // evaluate the scaled version of the objective function, fScaled(y),
  // and its gradient, gScaled(y), at the current point y.
  //
  class QuasiNewton01BoxObj
  {
    //----------------------------------------------------------
    // Constructors.
    //----------------------------------------------------------

  public:
    QuasiNewton01BoxObj(
      QuasiNewtonAnyBoxObj*  pObjectiveIn;
      const DoubleMatrix*    pdvecXLowIn,
      const DoubleMatrix*    pdvecXUpIn,
      const DoubleMatrix*    pdvecXDiffIn )
      :
      pObjective  ( pObjectiveIn ),
      pdvecXLow   ( pdvecXLowIn ),
      pdvecXUp    ( pdvecXUpIn ),
      pdvecXDiff  ( pdvecXDiffIn ),
      nX          ( pdvecXDiff->nr() )
    {
      dvecXCurr.resize( nX, 1 );

      pdXLowData  = pdvecXLow.data();
      pdXUpData   = pdvecXUp.data();
      pdXDiffData = pdvecXDiff.data();
      pdXCurrData = dvecXCurr.data();
    }

  private:
    // This is private and not defined so that it can't be used.
    QuasiNewton01BoxObj();


    //----------------------------------------------------------
    // Quantities related to the unscaled objective and its gradient.
    //----------------------------------------------------------

  private:
    QuasiNewtonAnyBoxObj* const pObjective;

    const DoubleMatrix* const pdvecXLow;      // Unscaled lower bounds.
    const DoubleMatrix* const pdvecXUp;       // Unscaled upper bounds.
    const DoubleMatrix* const pdvecXDiff;     // Difference between unscaled  
                                              // lower and upper bounds.

    const int nX;                             // Number of elements.

    DoubleMatrix dvecXCurr;                   // Current parameter value.
    DoubleMatrix drowF_xCurr;                 // Current gradient value.

    double* pdvecXLowData;
    double* pdvecXUpData;
    double* pdvecXDiffData;
    double* pdXCurrData;
    double* pdrowF_xCurrData;


    //----------------------------------------------------------
    // Functions required by QuasiNewton01Box.
    //----------------------------------------------------------

  public:
    //**********************************************************
    // 
    // Function: function
    //
    //
    // Evaluate the scaled version of the objective function, 
    // fScaled(y), at the current point y.
    //
    //**********************************************************

    const char* function( const double* yCurr, double& fScaledOut )
    {
      //--------------------------------------------------------
      // Prepare the parameters for the unscaled objective function.
      //--------------------------------------------------------

      // Transform the elements of the y vector back to their unscaled form. 
      unscaleElem( nX, yCurr, pdXLowData, pdXUpData, pdXDiffData, pdXCurrData );


      //--------------------------------------------------------
      // Evaluate the unscaled objective function.
      //--------------------------------------------------------

      try
      {
        // Note that the scaled and unscaled objective function 
        // values are the same.
        pObjective->function( dvecXCurr, &fScaledOut );
      }
      catch( SpkException& e )
      {
        throw e.push(
          SpkError::SPK_OPT_ERR, 
          "An SpkException was thrown during the evaluation of the objective function.",
          __LINE__, 
          __FILE__ );
      }
      catch( const std::exception& stde )
      {
        throw SpkException(
          stde,
          "An standard exception was thrown during the evaluation of the objective function.",
          __LINE__, 
          __FILE__ );
      }  
      catch( ... )
      {
        throw SpkException(
          SpkError::SPK_UNKNOWN_ERR, 
          "An unknown exception was thrown during the evaluation of the objective function.",
          __LINE__, 
          __FILE__ );
      }


      //--------------------------------------------------------
      // Finish up.
      //--------------------------------------------------------

      return "ok";
    }

    //**********************************************************
    // 
    // Function: gradient
    //
    //
    // Evaluate the gradient of the scaled version of the objective
    // function, gScaled(y), at the current point y.
    //
    //**********************************************************

    const char* gradient( double* gScaledOut )
    {
      //--------------------------------------------------------
      // Evaluate the gradient of the unscaled objective function.
      //--------------------------------------------------------

      try
      {
        pObjective->gradient( &dRowF_xCurr );

        assert( drowF_xCurr->nc() == nX );
      }
      catch( SpkException& e )
      {
        throw e.push(
          SpkError::SPK_OPT_ERR, 
          "An SpkException was thrown during the evaluation of the gradient of the objective.",
          __LINE__, 
          __FILE__ );
      }
      catch( const std::exception& stde )
      {
        throw SpkException(
          stde,
          "An standard exception was thrown during the evaluation of the gradient of the objective.",
          __LINE__, 
          __FILE__ );
      }  
      catch( ... )
      {
        throw SpkException(
          SpkError::SPK_UNKNOWN_ERR, 
          "An unknown exception was thrown during the evaluation of the gradient of the objective.",
          __LINE__, 
          __FILE__ );
      }

      // Reset this pointer since it could be changed during
      // the call to gradient.
      pdrowF_xCurrData = drowF_xCurr.data();

      // Transform the elements of the gradient vector to their scaled form. 
      scaleGradElem( nX, pdrowF_xCurrData, pdXDiffData, gScaledOut );


      //--------------------------------------------------------
      // Finish up.
      //--------------------------------------------------------

      return "ok";
    }

  };

} // [End: unnamed namespace]


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void quasiNewtonAnyBox( 
  QuasiNewtonAnyBoxObj&  objective,
  Optimizer&             optimizer,
  const DoubleMatrix&    dvecXLow,
  const DoubleMatrix&    dvecXUp,
  const DoubleMatrix&    dvecXIn,
  DoubleMatrix*          pdvecXOut,
  double*                pdFOut,
  DoubleMatrix*          pdrowF_xOut )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i;

  double epsilon  = optimizer.getEpsilon();
  int    nMaxIter = optimizer.getNMaxIter();
  int    level    = optimizer.getLevel();

  // Don't allow a warm start if zero iterations are requested.
  // This ensures that the objective function, its gradient, and
  // its Hessian are all evaluated at xIn.
  bool isWarmStart = optimizer.getIsWarmStart() && nMaxIter > 0;
  

  //------------------------------------------------------------
  // Validate the inputs (debug mode).
  //------------------------------------------------------------

  assert( epsilon > 0.0 );

  assert( dvecXLow.nr() == dvecXUp.nr() );
  assert( dvecXLow.nr() == dvecXIn.nr() );

  assert( dvecXLow.nc() == 1 );
  assert( dvecXUp.nc()  == 1 );
  assert( dvecXIn.nc()  == 1 );

  // Validate the lower and upper bounds and verify that the 
  // initial x value is between them.
  assert( allTrue( dvecXLow <= dvecXUp ) );
  assert( allTrue( dvecXLow <= dvecXIn ) );
  assert( allTrue( dvecXIn  <= dvecXUp ) );


  //------------------------------------------------------------
  // Initializations for the unscaled objective function.
  //------------------------------------------------------------

  // Set the number of objective function parameters.
  int nObjPar = dvecXIn.nr();

  const double* pdXLowData = dvecXLow.data();
  const double* pdXUpData  = dvecXUp.data();
  const double* pdXInData  = dvecXIn.data();

  // If the final x value should be returned, do some initializations.
  double* pdXOutData;
  if ( pdvecXOut )
  {
    int nXOutRows = pdvecXOut->nr();
    pdXOutData    = pdvecXOut->data();
    assert( nXOutRows == nObjPar );
  }

  DoubleMatrix dvecXDiff( nObjPar, 1 );

  double* pdXDiffData = dvecXDiff.data();


  //------------------------------------------------------------
  // Allocate all of the memory at the same time.
  //------------------------------------------------------------

  Memory<double> memoryDbl( 7 * nObjPar + 3 * nObjPar * nObjPar );
  Memory<bool> memoryBool(  1 * nObjPar );

  // The various y vectors are scaled versions of their x counterparts.
  double* yLow  = memoryDbl( nObjPar );
  double* yUp   = memoryDbl( nObjPar );
  double* yCurr = memoryDbl( nObjPar );

  // These are the scaled gradient, gScaled(y) = fScaled_y(y),
  // and scaled Hessian, hScaled(y) = fScaled_y_y(y).
  double* gScaled = memoryDbl( nObjPar );
  double* hScaled = memoryDbl( nObjPar * nObjPar );

  // These variables are used by the function isWithinTol.  They
  // are allocated here so that they don't have to be reallocated
  // everytime that function is called.
  double* deltaY         = memoryDbl( nObjPar );
  double* gScaledProj    = memoryDbl( nObjPar );
  double* hScaledWork    = memoryDbl( nObjPar * nObjPar );
  double* hScaledChol        = memoryDbl( nObjPar * nObjPar );
  double* hScaledCholDiagRec = memoryDbl( nObjPar );

  // These variables are used by the function isWithinTol.
  bool* isElemFree = memoryBool( nObjPar );


  //------------------------------------------------------------
  // Initializations for the scaled objective function.
  //------------------------------------------------------------

  // Set the bounds and initial values for y.
  for ( i = 0; i < nObjPar; i++ )
  {
    pdXDiffData[i] = pdXUpData[i] - pdXLowData[i]; 

    if ( pdXDiffData[i] != 0.0 ) 
    {
      // The x bounds are not equal, so constrain this element 
      // to the interval [0,1].
      yLow[i]  = 0.0;
      yUp[i]   = 1.0;
      yCurr[i] = ( pdXInData[i] - pdXLowData[i] ) / pdXDiffData[i];
    }
    else
    {
      // The x bounds are equal, so constrain this element 
      // to the point 0.
      yLow[i]  = 0.0;
      yUp[i]   = 0.0;
      yCurr[i] = 0.0
    }
  }


  //------------------------------------------------------------
  // Prepare the objective function object.
  //------------------------------------------------------------

  QuasiNewton01BoxObj objective( &dvecXLow, &dvecXUp, &dvecXDiff );


  //------------------------------------------------------------
  // Prepare the optimizer state information.
  //------------------------------------------------------------

  double rScaled;
  double fScaled;
  string message;

  // Since the optimizer always does a warm start, i.e. it always
  // makes use of the current values for y, the objective function,
  // its gradient and its Hessian, set those values here.
  if ( !isAWarmRestart )
  {
    // Evaluate the objective function and its gradient at the
    // initial value for y.
    try
    {
      message.assign( objective.function( yCurr, fScaled ) );
      assert( message == "ok" );

      message.assign( objective.gradient( gScaled ) );
      assert( message == "ok" );
    }
    catch( SpkException& e )
    {
      throw e.push(
        SpkError::SPK_OPT_ERR, 
        "An SpkException was thrown during the initial evaluation of the objective and gradient.",
        __LINE__, 
        __FILE__ );
    }
    catch( const std::exception& stde )
    {
      throw SpkException(
        stde,
        "An standard exception was thrown during the initial evaluation of the objective and gradient.",
        __LINE__, 
        __FILE__ );
    }  
    catch( ... )
    {
      throw SpkException(
        SpkError::SPK_UNKNOWN_ERR, 
        "An unknown exception was thrown during the initial evaluation of the objective and gradient.",
        __LINE__, 
        __FILE__ );
    }

    if ( nMaxIter > 0 )
    {
      // Set the number of quasi-Newton iterations high enough that 
      // the optimizer can build up a reasonably accurate approximation
      // for the Hessian the first time it is called, but not so high
      // that it will perform too many iterations before this function's
      // convergence criterion is checked.
      nIterMax = nObjPar;

      // Set the initial value for the trust region radius
      // equal to one half of the radius of the box.
      rScaled = 0.5;
  
      // Set the initial value for the positive definite approximation 
      // for the Hessian equal to the identity matrix.
      for ( i = 0; i < nObjPar; i++ )
      {
	for ( j = 0; j < nObjPar; j++ )
	{
	  hScaled[i * nObjPar + j] = static_cast<double>( i == j );
	}
      }
    }
  }
  else
  {
    // This function assumes that the Hessian provided during
    // a warm start is sufficiently accurate that the optimizer
    // does not need extra iterations in order to approximate
    // the Hessian the first time it is called.
    nIterMax = 1;

    // Retrieve the previous state information.
    StateInfo stateInfo = optimizer.getStateInfo();
    if ( stateInfo.n == nObjPar )
    {
      rScaled = stateInfo.r;
      fScaled = stateInfo.f;
      for ( i = 0; i < nObjPar; i++ )
      {
	yCurr[i]   = stateInfo.x[i];
	gScaled[i] = stateInfo.g[i];
      }
      for ( i = 0; i < nObjPar * nObjPar; i++ )
      {
	hScaled[i] = stateInfo.h[i];
      }
    }
    else
    {
      throw SpkException( 
        SpkError::SPK_USER_INPUT_ERR,
	"The sizes of the warm start values do not match the number of objective parameters.",
	__LINE__,
	__FILE__ );
    }
  }


  //------------------------------------------------------------
  // Set the parameters that control the optimization.
  //------------------------------------------------------------

  // Set the maximum number of interior point iterations so
  // that the optimizer can solve the quadratic subproblems
  // with sufficient accuracy.
  nQuadMax = 20 * nObjPar;

  // If the return value of QuasiNewton01Box is "ok", then the
  // infinity norm (element with the maximum absolute value) 
  // of the projected gradient is less than or equal to delta.
  // The scale is the value this norm will be divided.
  double delta;
  double deltaScale = 10.0;

  // Initialize the convergence flag and iteration counter.
  bool isAcceptable;
  int iterCurr;
  if ( nMaxIter > 0 )
  {
    isAcceptable = false;
    iterCurr = 1;
  }
  else
  {
    // If zero iterations have been requested, then the input value
    // for x is accepted as the final value.
    isAcceptable = true;
    iterCurr = 0;
  }

  // Initialize the convergence flag and iteration counter.
  bool isAcceptable = false;
  int iterCurr = 1;
  if ( nMaxIter == 0 )
  {
    // If zero iterations have been requested, then the input value
    // for x is accepted as the final value.
    isAcceptable = true;
    iterCurr = 0;
  }


  //------------------------------------------------------------
  // Set the remaining optimizer parameters.
  //------------------------------------------------------------

  // Send the output to standard cout.
  std::ostream outputStream = std::cout;


  //------------------------------------------------------------
  // Optimize the scaled objective function.
  //------------------------------------------------------------

  valarray<double> hScaledVA( nObjPar * nObjPar );
  valarray<double> hScaledCholVA( nObjPar * nObjPar );

  try
  {
    // Attempt to satisfy this function's convergence criterion before
    // the maximum number of iterations have been performed.
    while ( !isAcceptable && iterCurr <= nMaxIter )
    {
      // Get the Cholesky factor of the scaled Hessian in lower triangular 
      // form and column-major order, and then put it in row-major order.
      doubleArrayToValarray( hScaled, hScaledVA );
      hScaledCholVA = cholesky( hScaledVA, nObjPar );
      valarrayToDoubleArray_SquareMatrixTrans( hScaledCholVA, hScaledChol );

      // See if this function's convergence criterion has been met.
      if ( isWithinTol( 
	nObjPar,
        epsilon,
        yCurr,
        yLow,
        yUp,
        gScaled,
	hScaled,
        hScaledChol,
	deltaY,
	gScaledProj,
	hScaledWork,
	hScaledCholDiagRec,
	isElemFree ) )
      {
        isAcceptable = true;
      }
      else
      {
        // Set delta to be less than the maximum of the absolute values of
        // the elements of the projected gradient so that the subproblems
        // only be solved with accuracy sufficient for the current y value.
        delta = maxAbsProjGrad( gScaled ) / deltaScale;

        // Save the number of iterations that have been performed.
        iterCurrPrev = iterCurr;

        // Ask the optimizer to take perform a limited number of iterations.
        message.assign( QuasiNewton01Box(
          outputStream,
          level,
          nIterMax,
          nQuadMax,
          nObjPar,
          delta,
          objective,
          iterCurr,
          quadCurr,
          rScaled,
          fScaled,
          yCurr,
          gScaled,
          hScaled ) );

        // After the first call to the optimizer the approximation for the
        // Hessian should be accurate enough that this can be reset.
        nIterMax = 1;

        // This function assumes that delta is set small enough that the
        // optimizer's convergence criterion will not be satisfied for the
        // current y value and that the optimizer will therefore be able to
        // perform at least one Quasi-Newton itertion.  If that is not the
        // case, then throw an exception.
        if ( iterCurr == iterCurrPrev )
        {
          throw SpkException( 
            SpkError::SPK_OPT_ERR,
            "QuasiNewton01Box failed to perform at least one Quasi-Newton iteration.",
            __LINE__,
            __FILE__ );
        }
      }
    }
  }
  catch( SpkException& e )
  {
    throw e.push(
      SpkError::SPK_OPT_ERR, 
      "An SpkException was thrown during the optimization of the objective function.",
      __LINE__, 
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    throw SpkException(
      stde,
      "An standard exception was thrown during the optimization of the objective function.",
      __LINE__, 
      __FILE__ );
  }  
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR, 
      "An unknown exception was thrown during the optimization of the objective function.",
      __LINE__, 
      __FILE__ );
  }


  //------------------------------------------------------------
  // Save state information for future warm starts, if necessary.
  //------------------------------------------------------------

  if ( optimizer.getSaveStateAtEndOfOpt() )
  {
    if ( optimizer.stateInfo.n != nObjPar )
    {
      optimizer.deleteStateInfo();
      optimizer.setupWarmStart( nObjPar );
    }

    stateInfo.n = nObjPar;
    stateInfo.r = rScaled;
    stateInfo.f = fScaled;
    stateInfo.x = yCurr;
    stateInfo.g = gScaled;
    stateInfo.h = hScaled;

    optimizer.setStateInfo( stateInfo );
  }
  else
  {
    optimizer.deleteStateInfo();
  }


  //------------------------------------------------------------
  // Check the status of the optimization.
  //------------------------------------------------------------

  bool ok = false;
  SpkError::ErrorCode errorCode;
  StateInfo stateInfo;

  optimizer.setNIterCompleted( iterCurr );

  if ( isAcceptable )               // This function's convergence
                                    // criterion was satisfied.
  {
    optimizer.setIsTooManyIter( false );
    ok = true;
  }
  else if ( iterCurr == nMaxIter )  // The maximum number of iterations 
                                    // have been performed.
  {
    optimizer.setIsTooManyIter( true );
    if ( optimizer.getThrowExcepIfMaxIter() )
    {
      errorCode = SpkError::SPK_TOO_MANY_ITER;
      message = "Maximum number of iterations performed without convergence.";
      ok = false;
    }
    else
    {
      ok = true;
    }
  }
  else                              // This function's convergence
                                    // criterion was not satisfied.
  {
    optimizer.setIsTooManyIter( false );
    errorCode = SpkError::SPK_NOT_CONVERGED;
    message = "Unable to satisfy convergence criterion for quasiNewtonAnyBox.";
    ok = false;
  }

  // If something went wrong, throw an exception.
  if ( !ok )
  {
    throw SpkException( errorCode, message.str(), __LINE__, __FILE__ );
  }


  //------------------------------------------------------------
  // If the optimization didn't cause an exception, set the return values.
  //------------------------------------------------------------

  // If the final x value should be returned, then compute it
  // from the final y value.
  if ( pdvecXOut ) 
  {
    unscaleElem( nObjPar, yCurr, pdXLowData, pdXUpData, pdXDiffData, pdXOutData );
  }

  // If the final value for the objective function should be
  // returned, then set it equal to the value from nag_opt_nlp.
  if ( pdFOut )
  {
    *pdFOut = fScaled;
  }
  
  if ( pdrowF_xOut )
  {
    double* pdF_xOutData = pdrowF_xOut->data();

SHOULD gScaled BE UNSCALED BEFORE ITS COPIED TO f_x?
SHOULD gScaled BE UNSCALED BEFORE ITS COPIED TO f_x?
SHOULD gScaled BE UNSCALED BEFORE ITS COPIED TO f_x?
SHOULD gScaled BE UNSCALED BEFORE ITS COPIED TO f_x?
SHOULD gScaled BE UNSCALED BEFORE ITS COPIED TO f_x?
SHOULD gScaled BE UNSCALED BEFORE ITS COPIED TO f_x?
SHOULD gScaled BE UNSCALED BEFORE ITS COPIED TO f_x?

    std::copy( gScaled, gScaled + nObjPar, pdF_xOutData );
  }
}


/*=========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 =========================================================================*/

namespace // [Begin: unnamed namespace]
{

/*************************************************************************
 * Function: unscaleElem
 *
 *
 * Returns the unscaled value for y.   
 *
 *************************************************************************/

void unscaleElem(
  int            n,
  const double*  y, 
  const double*  xLow, 
  const double*  xUp, 
  const double*  xDiff,
  double*        x )
{
  int i;

  for ( i = 0; i < n; i++ )
  {
    if ( y[i] != 1.0 ) 
    {
      // Transform the element of the y vector back to the unscaled 
      // form. Note that for those elements of x for which the
      // lower and upper bounds are the same, xDiff is zero, and
      // the x values are equal to their lower bounds.
      x[i] = xLow[i] + y[i] * xDiff[i];
    }
    else
    {
      // If y is at the upper bound, calculate the unscaled value
      // in a way that avoids roundoff error.
      x[i] = xUp[i];
    }
  }
}


/*************************************************************************
 * Function: scaleGradElem
 *
 *
 * Returns the scaled value for g.
 *
 *************************************************************************/

void scaleGradElem(
  int                  n,
  const double*  g, 
  const double*  xDiff,
  double*              gScaled )
{
  int i;

  for ( i = 0; i < n; i++ )
  {
    gScaled[i] = xDiff[i] * g[i];
  }
}


/*************************************************************************
 * Function: doubleArrayToValarray
 *
 *
 * Sets the elements in the valarray of doubles xVA equal to those in
 * the array of doubles x.  This function assumes that xVA and x have
 * the same number of elements.
 *
 *************************************************************************/

void doubleArrayToValarray( const double* x, valarray<double>& xVA )
{
  int i;

  for ( i = 0; i < xVA.size(); i++ )
  {
    xVA[i] = x[i];
  }
}


/*************************************************************************
 * Function: valarrayToDoubleArray_SquareMatrixTrans
 *
 *
 * Assuming that the array of doubles x and the valarray of doubles xVA
 * both contain the elements from a square matrix with the same number
 * of elements, this function sets the elements in x equal to the matrix
 * transpose of those in xVA.
 *
 *************************************************************************/

void valarrayToDoubleArray_SquareMatrixTrans( const valarray<double>& xVA, double* x )
{
  int i;
  int j;

  int n = sqrt( xVA.size() );
  assert( n * n = xVA.size() );

  for ( i = 0; i < n; i++ )
  {
    for ( j = 0; j < n; j++ )
    {
      x[i + j * n] = xVA[j + i * n];
    }
  }
}


/*************************************************************************
 * Function: isWithinTol
 *
 *
 * Description
 * -----------
 *
 * Returns true if xHat is sufficiently close to xTrue, the true 
 * minimizer of the function f(x).
 *
 * Specifically, this function calculates deltaX, an approximation 
 * for the distance that xHat is from xTrue as the solution of 
 *
 *     H  deltaX  =  g  ,                              (1)
 *
 * where both the gradient of f(x),
 *
 *     g = f_x(xHat) 
 *
 * and the Hessian of f(x) 
 *
 *     H = f_x_x(xHat)
 *
 * are evaluated at xHat.  Note that equation (1) follows from the 
 * derivative of the Taylor expansion of f(x) about xTrue. 
 *
 * This function returns true if 
 *
 *     abs(deltaX)  <  tol  .
 *
 * Otherwise it returns false.
 *
 *
 * Arguments
 * ---------
 *
 * tol
 *
 * The tolerance for how close xHat must be to xTrue.  It must be greater than 0.0.
 *
 *
 * n
 *
 * Number of elements in x.
 *
 *
 * xHat
 *
 * The estimate for the true minimizer xTrue.  It must be of length n.
 *
 *
 * xLow
 *
 * The lower bound for x.  It must be of length n.
 *
 *
 * xUp
 *
 * The upper bound for x.  It must be of length n.
 *
 *
 * g
 *
 * The gradient g(x) evaluated at xHat.  It must be of length n.
 *
 *
 * h
 *
 * The Hessian H(x) evaluated at xHat.  It must be of length n * n, and
 * its elements must be in row-major order.
 *
 *
 * hChol
 *
 * The lower triangular Cholesky factor of the Hessian H(x) evaluated
 * at xHat.  Note that the existence of this Cholesky factor implies 
 * that H is symmetric and positive-definite.  It must be of length 
 * n * n, and its elements must be in row-major order.
 *
 *
 * deltaX
 *
 * On input, this must be allocated to hold n elements.  On output, it
 * contains an approximation for the distance that xHat is from xTrue.
 *
 *
 * gProj
 *
 * On input, this must be allocated to hold n elements.  On output, it
 * contains the projected gradient evaluated at xHat.
 *
 *
 * hWork
 *
 * On input, this must be allocated to hold n * n elements.  On output, 
 * it contains a modified version of the Hessian with its elements in
 * row-major order and with some of its elements replaced by some of the 
 * elements of its Cholesky factor.
 *
 *
 * hCholDiagRec
 *
 * On input, this must be allocated to hold n elements.  On output, it
 * contains the reciprocals of the diagonals of the Cholesky factor of
 * the Hessian H(x) evaluated at xHat.
 *
 *
 * isElemFree
 *
 * On input, this must be allocated to hold n elements.  On output, its
 * elements will be equal to true if the corresponding xHat element is
 * free and will be included in the tolerance calculation.
 *
 *************************************************************************/

bool isWithinTol(
  double         tol,
  int            n,
  const double*  xHat,
  const double*  xLow,
  const double*  xUp,
  const double*  g,
  const double*  h,
  const double*  hChol,
  double*        deltaX,
  double*        gProj,
  double*        hWork,
  double*        hCholDiagRec,
  bool*          isElemFree )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int i;
  int j;
  int k;

  assert( isLowerTriangular( hChol ) );


  //------------------------------------------------------------
  // Prepare the modified version of the Hessian.
  //------------------------------------------------------------

  // Create a version of the Hessian with its sub-diagonal elements
  // replaced by the sub-diagonal elements of its Cholesky factor.
  for ( i = 0; i < n; i++ )
  {
    // Copy the upper triangle elements from the Hessian.
    for ( j = 0; j <= i ; j++ )
    {
      hWork[i * n + j] = h[i * n + j];
    }
  
    // Copy the sub-diagonal elements from its Cholesky factor.
    for ( j = i + 1; j < n; j++ )
    {
      hWork[i * n + j] = hChol[i * n + j];
    }
  }


  //------------------------------------------------------------
  // Calculate the projected gradient, modified Hessian, and reciprocals.
  //------------------------------------------------------------

  // Set the elements of the projected gradient, finish preparing the
  // modified Hessian, and calculate the reciprocals of the diagonals
  // of the Cholesky factor of the Hessian.
  for ( i = 0; i < n; i++ )
  {
    // Determine which elements are free, i.e., are not being held by 
    // their bounds.
    if ( ( xHat[i] >  xLow[i] && xHat[i] < xUp[i] )
      || ( xHat[i] == xLow[i] && g[i]    < 0.0 ) 
      || ( xHat[i] == xUp[i]  && g[i]    > 0.0 ) )
    {
      //--------------------------------------------------------
      // This element is free:  its deltaX should be computed.
      //--------------------------------------------------------

      isElemFree[i] = true;

      // Set the corresponding element of the projected gradient.
      gProj[i] = g[i];

      // Set the reciprocal of the corresponding R diagonal.
      assert( hChol[i * n + i] != 0.0 );
      hCholDiagRec[i] = 1.0 / hChol[i * n + i];
    }
    else
    {
      //--------------------------------------------------------
      // This element is not free:  force its deltaX to be zero.
      //--------------------------------------------------------

      isElemFree[i] = false;

      // Zero the corresponding element of the projected gradient.
      gProj[i] = 0.0;

      // Set the correponding diagonal element of the modified Hessian
      // equal to one.
      hWork[i * n + i] = 1.0;

      // Zero the rest of the elements in the corresponding row
      // and column of the modified Hessian.
      for ( j = 0; j < i; j++ )
      {
        hWork[i * n + j] = 0.0;
        hWork[j * n + i] = 0.0;
      }
      for ( j = i + 1; j < n; j++ )
      {
        hWork[i * n + j] = 0.0;
        hWork[j * n + i] = 0.0;
      }
  
      // Set the reciprocal of the corresponding L diagonal equal to one.
      hCholDiagRec[i] = 1.0;
    }
  }


  //------------------------------------------------------------
  // Define the parameters for nag_real_cholesky_solve_mult_rhs.
  //------------------------------------------------------------

  //
  // Review - Sachiko: suggestion
  //
  // This routine may be useful in general.  
  // It's a verion of backDiv (solve Ax=B) taking advantage of positive definiteness.
  //

  // ***********************************************************
  // * In general, nag_real_cholesky_solve_mult_rhs solves the
  // * system of equations
  // *
  // *     A  X  =  B .
  // *
  // * For this particular problem
  // *
  // *     A  =  H ,
  // *
  // *     X  =  deltaX ,
  // *
  // * and
  // *
  // *     B  =  gProj .
  // * 
  // * Note that 
  // *               T
  // *     A  =  L  L  ,
  // *
  // * where
  // *
  // *     L  = hChol .
  // *
  // ***********************************************************

  // Parameter: n.
  // Input: n, the order of the matrix A. 
  // Output: unspecified.
  // Constraint: n >= 1. 
  assert( n >= 1 );

  // Parameter: nrhs.
  // Input: r, the number of right-hand sides.
  // Output: unspecified.
  // Constraint: nrhs >= 1.
  Integer nrhs = 1;
  assert ( nrhs >= 1 );

  // Parameter: a[n][tda].
  // Input:the upper triangle of the n by n positive-definite
  // symmetric matrix A, and the sub-diagonal elements of its
  // Cholesky factor L, as returned by nag_real_cholesky
  // (f03aec).
  // Output: unspecified.
  double* a = hWork;

  // Parameter: tda.
  // Input:the last dimension of the array a as declared in the
  // function from which nag_real_cholesky_solve_mult_rhs is
  // called.
  // Output: unspecified.
  // Constraint: tda >= n.
  Integer tda = n;

  // Parameter: p[n].
  // Input:the reciprocals of the diagonal elements of L, as
  // returned by nag_real_cholesky (f03aec).
  // Output: unspecified.
  double* p = hCholDiagRec;

  // Parameter: b[n][tdb].
  // Input:the n by r right-hand side matrix B.  
  // Output: unspecified.
  double* b = gProj;

  // Parameter: tdb.
  // Input:the last dimension of the array b as declared in the
  // function from which nag_real_cholesky_solve_mult_rhs is
  // called.
  // Output: unspecified.
  // Constraint: tdb >= nrhs.
  Integer tdb = nrhs;

  // Parameter: x[n][tdx].
  // Input: unspecified.
  // Output: the n by r solution matrix X.  
  double* x = deltaX;

  // Parameter: tdx.
  // Input:the last dimension of the array x as declared in the
  // function from which nag_real_cholesky_solve_mult_rhs is
  // called.
  // Output: unspecified.
  // Constraint:  tdx >= nrhs.
  Integer tdx = nrhs;


  //------------------------------------------------------------
  // Solve the system of equations to compute deltaX.
  //------------------------------------------------------------
 
  // Revisit - Exceptions - Mitch: if an error occurs in this
  // NAG routine, the program will be stopped using exit or abort. 
  //
  nag_real_cholesky_solve_mult_rhs(n, nrhs, a, tda, p, b, tdb, x, 
    tdx, NAGERR_DEFAULT);
    

  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Check to see if deltaX is within tolerance.
  for ( i = 0; i < n; i++ )
  {
    // Only check the elements of xHat that were determined
    // earlier to be necessary for this tolerance calculation.
    if ( isElemFree[i] )
    {
      if ( fabs( deltaX[i] ) > tol )
      {
        // Get out of the routine as soon as possible if
	// one of the elements is not within tolerance.
        return false;
      }
    }
  }

  return true;
}


/*************************************************************************
 * Function: isLowerTriangular
 *
 *
 * Returns true if the square matrix x, which has n rows and columns, 
 * is lower triangular, i.e., if all of its elements above the diagonal
 * are zero.  This function assumes that x has been allocated with the
 * proper number of elements.
 *
 *************************************************************************/

bool isLowerTriangular( int n, const double* x )
{
  int i;
  int j;

  for ( i = 0; i < n - 1; i++ ) 
  {
    for ( j = i + 1; j < n; j++ )
    {
      if ( x[i + j * n] != 0.0 )
      {
          return false;
      }
    }
  }

  return true;
}


} // [End: unnamed namespace]
