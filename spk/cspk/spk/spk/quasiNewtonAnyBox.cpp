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
  Bfgs
  pos
  bool
  cmath
  complimentarity
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
  Excep
  fmin
  Fout
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
  rethrow
  rowdim
  seq
  setiosflags
  setprecision
  Spk
  struct
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
  optInfo
  Optimizer optInfo
$$

$section Quasi-Newton Optimization with Derivatives and Box Constraints$$

$index quasiNewtonAnyBox$$
$cindex quasi-Newton optimization 
  \with derivatives \and box constraints$$

$table
$bold Prototype:$$   $cend  
$syntax/void quasiNewtonAnyBox( 
  QuasiNewtonAnyBoxObj&  /objective/,
  Optimizer&             /optInfo/,
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
The elements of the vectors $math%x%$$ and $math%y%$$ for which 
$math%xLow(i)%$$ and $math%xUp(i)%$$ are not equal are related 
as follows: 
$math%
    y(j) = [ x(i) - xLow(i) ] / [ xUp(i) - xLow(i) ] ,
%$$
where $math%j%$$ is the position of the element in $math%y%$$ 
that corresponds to $math%x(i)%$$.
Note that if $math%xLow(i) = xUp(i)%$$, then $math%x(i)%$$ is 
not included in $math%y%$$.

$head Return Value$$
If the convergence criteria specified below by the $italic optInfo$$
argument is satisfied, then the output value pointers are set to point
to their respective results.
If the convergence criteria is not satisfied, then an 
$xref/SpkException//exception/$$ will be thrown.  
The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.
In the case of the maximum number of iterations being exhausted,
an exception may or may not be thrown depending on the value for
the $italic optInfo.throwExcepIfMaxIter$$ parameter.

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

/optInfo/
/$$
This $xref/Optimizer//Optimizer/$$ object contains the information 
that controls the optimization process.
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
Note that the upper and lower bounds for $math%x%$$ must be the 
same as they were during the earlier call to this function.
$pre

$$
Most of the optimizer information is accessible directly via public
get functions, e.g., the value epsilon is returned by the Optimizer 
class function $code getEpsilon()$$.  
The following set of subsections specify how this function uses 
some of the elements of the Optimizer object that are accessed 
directly using get functions.

$subhead optInfo.epsilon$$
This real number is used to specify the convergence criteria
for the optimizer.
It must be greater than $math%0.0%$$.
$pre

$$
If the preprocessor symbol $code SPK_CALIBRATE$$ is false,
$math%xOut%$$ is accepted as an estimate for $math%xHat%$$ if 
$math%
        abs( xOut - xHat ) \le epsilon ( xUp - xLow )  ,
%$$
where $math%abs%$$ is the element-by-element absolute value function
and $math%xHat%$$ is a local minimizer of the objective function.
Since $math%xHat%$$ is unknown, this function estimates the left hand
side of this inequality in a way that is a good approximation when 
the Hessian of the objective function is positive definite.
$pre

$$
Note that if $italic nMaxIter$$ is set to zero, then $math%xIn%$$ is 
accepted as the estimate for $math%xHat%$$.
$pre

$$
If the preprocessor symbol $code SPK_CALIBRATE$$ is true,
$math%xOut%$$ is accepted as an estimate for $math%xHat%$$ if 
$math%
        abs( xOut - xHat ) \le epsilon ( xUp - xLow ) / 10 
%$$


$subhead optInfo.nMaxIter$$
This integer must be greater than or equal to zero.
It specifies the maximum number of 
iterations to attempt before giving up on convergence.
If it is equal to zero, then the initial
value for $math%x%$$ is accepted as the final value, and any requested output
values are evaluated at that final value.

$subhead optInfo.traceLevel$$
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

$subhead optInfo.nIterCompleted$$
This integer scalar holds the number of iteration that have been 
completed in the optimizer.

$subhead optInfo.isTooManyIter$$
This flag indicates whether the too-many-iteration failure has occurred.  

$subhead optInfo.saveStateAtEndOfOpt$$
This flag indicates if the state information required for a warm start
should be saved at the end of the optimization process.
This state information will not be saved if the optimization process
results in an exception being thrown by $code quasiNewtonAnyBox$$.

$subhead optInfo.throwExcepIfMaxIter$$
This flag indicates if the optimizer should throw an exception when
the maximum number of iterations is exhausted.
If this parameter is true, then when
the maximum number of iterations is exhausted, an exception will
be thrown and the output values for this function will not be set.
Otherwise, the calling program will
need to check the parameter isTooManyIter to see if the 
maximum number of iterations was exhausted.

$subhead optInfo.isWarmStartPossible$$
This flag indicates whether it is possible to perform a warm start 
using the current optimizer state information.

$subhead optInfo.isWarmStart$$
This flag indicates whether the optimization should run a warm start.  

$subhead optInfo.stateInfo$$
This $code StateInfo$$ struct contains the optimization state information
required to perform a warm start.
Each of its elements is accessed using the Optimizer class member
functions, $code getStateInfo()$$ and $code setStateInfo()$$.
$pre

$$
The following set of subsections specify how this function uses 
the elements of $code optInfo.stateInfo$$.

$subhead optInfo.stateInfo.n$$
The element $code n$$ specifies the number of components
in the element vector $code x$$.

$subhead optInfo.stateInfo.b$$
The element $code b$$ specifies the number of Bfgs updates
that have been made to the Hessian approximation $code h$$.
This function assumes that once the number of Bfgs updates
is equal to the number of objective function parameters $code n$$,
then the Hessian approximation is accurate enough to use.
If the Hessian is known, rather than being an approximation,
then $code b$$ should be set equal to $code n$$.

$subhead optInfo.stateInfo.r$$
The element $code r$$ contains the current trust region radius
(as an infinity norm bound on the step size).

$subhead optInfo.stateInfo.f$$
The element $code f$$ contains the value for $math%f(x)%$$
at the point $math%x%$$.

$subhead optInfo.stateInfo.x$$
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
$pre

$$
Note that if a warm start is being performed, then the initial $math%x%$$
estimate will come from the warm start optimizer state information,
and this initial estimate will not be used.

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
value of the gradient of the objective function $math%f(x)%$$ with respect
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

// SPK library header files.
#include "transpose.h"
#include "quasiNewtonAnyBox.h"
#include "DoubleMatrix.h"
#include "isLessThanOrEqualTo.h"
#include "isNotANumber.h"
#include "isUnnormNumber.h"
#include "allTrue.h"
#include "SpkException.h"
#include "cholesky.h"

// SPK optimizer header files.
#include <QN01Box/QuasiNewton01Box.h>
#include <QN01Box/MaxAbs.h>
#include <QN01Box/Memory.h>
#include <QN01Box/PlusInfinity.h>

// Standard library header files.
#include <iostream>
#include <cassert>
#include <cmath>
#include <limits>

// include file generated by configure script
#include <spk/config.h>


/*------------------------------------------------------------------------
 * Local class declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  class QuasiNewton01BoxObj;

} // [End: unnamed namespace]


/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{

  void unscaleElem(
    int            nX,
    const double*  y, 
    const double*  xLow, 
    const double*  xUp, 
    const double*  xDiff,
    const int*     indexXFreeInY, 
    double*        x );

  void scaleGradElem(
    int            nY,
    const double*  g, 
    const double*  xDiff,
    const int*     indexYInX, 
    double*        gScaled );

  void unscaleGradElem(
    int            nX,
    const double*  gScaled, 
    const double*  xDiff,
    const int*     indexXFreeInY, 
    double*        g );

  void initHessApprox(
    int            n,
    const double*  gCurr,
    double*        h );

  bool meetsConvCrit( int nAccept, int nPar );

  void calcScaledProjGrad(
    int            n,
    const double*  yCurr,
    const double*  gScaled,
    double*        gScaledProj );

  bool isAllZero( int n, const double* x );
  
} // [End: unnamed namespace]



/*------------------------------------------------------------------------
 * Local class definitions
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
      QuasiNewtonAnyBoxObj*  pObjectiveAnyBoxIn,
      const DoubleMatrix*    pdvecXLowIn,
      const DoubleMatrix*    pdvecXUpIn,
      const DoubleMatrix*    pdvecXDiffIn,
      int                    nYIn,
      const int*             indexXFreeInYIn,
      const int*             indexYInXIn )
      :
      pObjectiveAnyBox  ( pObjectiveAnyBoxIn ),
      pdvecXLow         ( pdvecXLowIn ),
      pdvecXUp          ( pdvecXUpIn ),
      pdvecXDiff        ( pdvecXDiffIn ),
      pdXLowData        ( pdvecXLow->data() ),
      pdXUpData         ( pdvecXUp->data() ),
      pdXDiffData       ( pdvecXDiff->data() ),
      nX                ( pdvecXDiff->nr() ),
      nY                ( nYIn ),
      indexXFreeInY     ( indexXFreeInYIn ),
      indexYInX         ( indexYInXIn )
    {
      dvecXCurr  .resize( nX, 1 );
      drowF_xCurr.resize( 1,  nX );

      pdXCurrData = dvecXCurr.data();
    }

  private:
    // This is private and not defined so that it can't be used.
    QuasiNewton01BoxObj();


    //----------------------------------------------------------
    // Quantities related to the unscaled objective and its gradient.
    //----------------------------------------------------------

  private:
    QuasiNewtonAnyBoxObj* const pObjectiveAnyBox;

    const DoubleMatrix* const pdvecXLow;      // Unscaled lower bounds.
    const DoubleMatrix* const pdvecXUp;       // Unscaled upper bounds.
    const DoubleMatrix* const pdvecXDiff;     // Difference between unscaled  
                                              // lower and upper bounds.
    const double* pdXLowData;
    const double* pdXUpData;
    const double* pdXDiffData;

    const int nX;                             // Number of unscaled objective parameters.
    const int nY;                             // Number of scaled objective parameters.

    DoubleMatrix dvecXCurr;                   // Current unscaled parameter value.
    DoubleMatrix drowF_xCurr;                 // Current unscaled gradient value.

    double* pdXCurrData;
    double* pdF_xCurrData;

    const int* indexXFreeInY;                 // Indices for free unscaled parameter
                                              // elements in the scaled parameter.
    const int* indexYInX;                     // Indices for scaled parameter elements
                                              // in the unscaled parameter.


    //----------------------------------------------------------
    // Functions required by QuasiNewton01Box.
    //----------------------------------------------------------

  public:
    // *********************************************************
    // Function: Hessian is not available
    const char* Hessian(double *HCur)
    {	return "not available"; }
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
      //------------------------------------------------------------
      // Preliminaries.
      //------------------------------------------------------------

      using namespace std;


      //--------------------------------------------------------
      // Prepare the parameters for the unscaled objective function.
      //--------------------------------------------------------

      // Transform the elements of the y vector back to their unscaled form. 
      unscaleElem(
        nX,
        yCurr,
        pdXLowData,
        pdXUpData,
        pdXDiffData,
        indexXFreeInY,
        pdXCurrData );


      //--------------------------------------------------------
      // Evaluate the unscaled objective function.
      //--------------------------------------------------------

      try
      {
        // Note that the scaled and unscaled objective function 
        // values are the same.
        pObjectiveAnyBox->function( dvecXCurr, &fScaledOut );
      }
      catch( SpkException& e )
      {
        throw e.push(
          SpkError::SPK_OPT_ERR, 
          "The evaluation of the objective function failed.",
          __LINE__, 
          __FILE__ );
      }
      catch( const std::exception& stde )
      {
        throw SpkException(
          stde,
          "A standard exception was thrown during the evaluation of the objective function.",
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

      // Make sure that the objective function value is not a Nan.
      //
      // Note that infinite objective function values are allowed
      // because they can be returned by the objective function to
      // indicate that QuasiNewton01Box should back up.
      if ( isNotANumber( fScaledOut ) )
      {
        throw SpkException(
          SpkError::SPK_OPT_ERR, 
          "A value that is Not a Number (NaN) was generated for the objective function.",
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
        pObjectiveAnyBox->gradient( &drowF_xCurr );

        assert( drowF_xCurr.nc() == nX );
      }
      catch( SpkException& e )
      {
        throw e.push(
          SpkError::SPK_OPT_ERR, 
          "The evaluation of the gradient of the objective function failed.",
          __LINE__, 
          __FILE__ );
      }
      catch( const std::exception& stde )
      {
        throw SpkException(
          stde,
          "A standard exception was thrown during the evaluation of the gradient of the objective function.",
          __LINE__, 
          __FILE__ );
      }  
      catch( ... )
      {
        throw SpkException(
          SpkError::SPK_UNKNOWN_ERR, 
          "An unknown exception was thrown during the evaluation of the gradient of the objective function.",
          __LINE__, 
          __FILE__ );
      }

      // Reset this pointer since it could be changed during
      // the call to gradient.
      pdF_xCurrData = drowF_xCurr.data();

      // Transform the elements of the gradient vector to their scaled form. 
      scaleGradElem( nY, pdF_xCurrData, pdXDiffData, indexYInX, gScaledOut );


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
  QuasiNewtonAnyBoxObj&  objectiveAnyBox,
  Optimizer&             optInfo,
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
  int j;

  double epsilon           = optInfo.getEpsilon();
  int    nMaxIterAnyBox    = optInfo.getNMaxIter();
  int    level             = optInfo.getLevel();
  bool   isWarmStartAnyBox = optInfo.getIsWarmStart();

  string stringMessage;


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

  DoubleMatrix dvecXDiff( nObjPar, 1 );

  double* pdXDiffData = dvecXDiff.data();

  // Determine the number of parameters that are not constrained 
  // by their bounds.
  int nObjParFree = 0;
  for ( i = 0; i < nObjPar; i++ )
  {
    if ( pdXUpData[i] != pdXLowData[i] )
    {
      nObjParFree++;
    }
  }

  // If all of the elements of x are constrained by their bounds,
  // there is no need to optimize the objective.
  if ( nObjParFree == 0 )
  {
    nMaxIterAnyBox = 0;
  }


  //------------------------------------------------------------
  // Allocate all of the memory at the same time.
  //------------------------------------------------------------

  QN01Box::Memory<int> memoryInt( 1 * nObjPar + 1 * nObjParFree );

  // These store the indices for the free x elements in Y and the
  // indices for all of the y elements in x.
  int* indexXFreeInY = memoryInt( nObjPar );
  int* indexYInX     = memoryInt( nObjParFree );

  QN01Box::Memory<double> memoryDbl( 6 * nObjParFree + 1 * ( nObjParFree * nObjParFree ) );

  // The various y vectors are scaled versions of their x counterparts.
  double* yLow  = memoryDbl( nObjParFree );
  double* yUp   = memoryDbl( nObjParFree );
  double* yCurr = memoryDbl( nObjParFree );

  // This is the step to the solution of the second-order approximation
  // for the objective function that is used by the optimizer.
  double* sScaled = memoryDbl( nObjParFree );

  // These are the scaled gradient, gScaled(y) = fScaled_y(y),
  // the projected version of the scaled gradient, gScaledProj,
  // and the approximation for the scaled Hessian, 
  // hScaled(y) = fScaled_y_y(y).
  double* gScaled     = memoryDbl( nObjParFree );
  double* gScaledProj = memoryDbl( nObjParFree );
  double* hScaled     = memoryDbl( nObjParFree * nObjParFree );


  //------------------------------------------------------------
  // Initializations for the scaled objective function.
  //------------------------------------------------------------

  // Determine which elements of x are not constrained by their
  // bounds and will therefore be included in y.
  j = 0;
  for ( i = 0; i < nObjPar; i++ )
  {
    pdXDiffData[i] = pdXUpData[i] - pdXLowData[i]; 

    // Only include this x element if its bounds are not equal.
    if ( pdXDiffData[i] != 0.0 ) 
    {
      // Constrain its corresponding y value to the interval [0,1].
      yLow[j]  = 0.0;
      yUp[j]   = 1.0;

      indexXFreeInY[i] = j;
      indexYInX[j]     = i;
      j++;
    }
    else
    {
      // Set this to indicate this element is not free and is not 
      // included in y.
      indexXFreeInY[i] = -1;
    }
  }

  // Instantiate the scaled objective function object.
  QuasiNewton01BoxObj objective01Box( 
    &objectiveAnyBox,
    &dvecXLow,
    &dvecXUp,
    &dvecXDiff,
    nObjParFree,
    indexXFreeInY,
    indexYInX );


  //------------------------------------------------------------
  // Prepare the optimization state information for QuasiNewto01Box.
  //------------------------------------------------------------

  // Set this so that this function's convergence criterion
  // will be checked at the beginning of each iteration.
  bool checkConvAtBeginOfIter = true;

  // Initialize the convergence flag.
  bool isWithinTol;
  if ( nMaxIterAnyBox > 0 )
  {
    // Set the value for the case of one or more iterations.
    isWithinTol = false;
  }
  else
  {
    // If zero iterations have been requested, then accept the input
    // value for x as the final value.
    isWithinTol = true;
  }

  size_t bfgsCurr       = 0;
  int    nSScaledAccept = 0;

  double rScaled;
  double fScaled;

  // Send the output to standard cout.
  std::ostream& outputStream = std::cout;

  const char* charMessage;

  // Even if quasiNewtonAnyBox is not doing a warm start, QuasiNewton01Box
  // always does a warm start itself.  This means that QuasiNewton01Box must
  // always be provided with optimization state information when it is called.
  if ( !isWarmStartAnyBox )
  {
    //----------------------------------------------------------
    // Prepare for a quasiNewtonAnyBox normal (non-warm) start.
    //----------------------------------------------------------

    // Set the initial y value using the input x value.
    for ( i = 0; i < nObjParFree; i++ )
    {
      yCurr[i] = ( pdXInData[indexYInX[i]] - pdXLowData[indexYInX[i]] ) / 
        pdXDiffData[indexYInX[i]];
    }

    // Get initial values for the objective and its gradient.
    try
    {
      charMessage = objective01Box.function( yCurr, fScaled );
      assert( strcmp( charMessage, "ok" ) == 0 );

      charMessage = objective01Box.gradient( gScaled );
      assert( strcmp( charMessage, "ok" ) == 0 );
    }
    catch( SpkException& e )
    {
      throw e.push(
        SpkError::SPK_OPT_ERR, 
        "The initial evaluation of the objective function and gradient failed.",
        __LINE__, 
        __FILE__ );
    }
    catch( const std::exception& stde )
    {
      throw SpkException(
        stde,
        "A standard exception was thrown during the initial evaluation of the objective function and gradient.",
        __LINE__, 
        __FILE__ );
    }  
    catch( ... )
    {
      throw SpkException(
        SpkError::SPK_UNKNOWN_ERR, 
        "An unknown exception was thrown during the initial evaluation of the objective function and gradient.",
        __LINE__, 
        __FILE__ );
    }

    // If more than zero iterations were requested, then prepare the rest
    // of the optimization state information.
    if ( nMaxIterAnyBox > 0 )
    {
      // Set the initial value for the trust region radius
      // equal to one half of the radius of the box.
      rScaled = 0.5;
  
      // Calculate an initial value for the approximate Hessian.
      initHessApprox( nObjParFree, gScaled, hScaled );
    }
  }
  else
  {
    //----------------------------------------------------------
    // Prepare for a quasiNewtonAnyBox warm start.
    //----------------------------------------------------------

    // Set the current values equal to the previous values.
    optInfo.getStateInfo( 
      nObjParFree,
      bfgsCurr,
      rScaled,
      fScaled,
      yCurr,
      gScaled,
      hScaled,
      nObjPar,
      pdXLowData,
      pdXUpData,
      indexYInX,
      nSScaledAccept );

    // See if enough consecutive acceptable sScaled values have been
    // calculated to meet this function's convergence criterion.
    if ( meetsConvCrit( nSScaledAccept, nObjParFree ) )
    {
      // Set this to indicate the optimizer converged the
      // previous time.
      isWithinTol = true;
    }
    else
    {
      // Set this so that the first convergence check will
      // not occur until after a step has been taken.
      checkConvAtBeginOfIter = false;
    }
  }

  // Throw an exception if the initial value for the objective
  // function is equal to infinity because QuasiNewtonAnyBox cannot
  // back up in this case.
  if ( fScaled == QN01Box::PlusInfinity( double( 0 ) ) )
  {
    throw SpkException(
      SpkError::SPK_OPT_ERR, 
      "The initial value for the objective function was equal to infinity.",
      __LINE__,
      __FILE__ );
  }

  // Set this flag to indicate the main optimization loop has not yet
  // completed.  Note that this will be reset after the main
  // optimization loop if it does not cause an error.
  optInfo.setDidOptFinishOk( false );

  // Set these flags to indicate no iterations have been completed and
  // that the maximum number of iterations has not been reached.
  optInfo.setNIterCompleted( 0 );
  optInfo.setIsTooManyIter ( false );


  //------------------------------------------------------------
  // See if the initial parameters are already optimal.
  //------------------------------------------------------------

  // Calculate the initial scaled projected gradient.
  calcScaledProjGrad( nObjParFree, yCurr, gScaled, gScaledProj ); 
  
  // If the scaled projected gradient is already equal to zero,
  // then the starting value is the optimal value, and the
  // optimizer does not need to perform any iterations.
  if ( nObjParFree > 0 )
  {
    if ( QN01Box::MaxAbs( nObjParFree, gScaledProj ) == 0.0 )
    {
      isWithinTol = true;

      if ( level > 0 && nMaxIterAnyBox > 0 )
      {
        outputStream << endl;
        outputStream << "Initial parameter values are optimal." << endl;
        outputStream << endl;
      }

      nMaxIterAnyBox = 0;
    }
  }


  //------------------------------------------------------------
  // Set the rest of the parameters that control the optimization.
  //------------------------------------------------------------

  // The iteration counter tracks the number of quasi-Newton iterations 
  // that have been performed by the optimizer.
  size_t iterCurr = 1;
  size_t iterBefore;
  size_t iterMax;

  // Set the maximum number of interior point iterations so
  // that the optimizer can solve the quadratic subproblems
  // with sufficient accuracy.
  size_t nQuadMax = 100;
  size_t quadCurr = 0;

  // The optimizer's convergence criterion is based on the infinity
  // norm (element with the maximum absolute value) of the scaled
  // projected gradient.
  double delta = epsilon / 10.0;
# ifndef SPK_CALIBRATE
  The_preprocessor_symbol_SPK_CALIBRATE_is_not_defined
# endif
# if SPK_CALIBRATE
  delta /= 10.0;
# endif

  // If this flag is true, then the current sScaled value solves the
  // optimizer's quadratic subproblem.
  bool isSScaledOk = false;

  // Set the negative value for level.
  int negLevel = - level;

  // Set the value for level that won't produced any trace information.
  int noTracingLevel = 0;


  //------------------------------------------------------------
  // Optimize the scaled objective function.
  //------------------------------------------------------------

  if ( level > 0 && nMaxIterAnyBox > 0 )
  {
    outputStream << endl;
    outputStream << "Begin search for optimal parameter values." << endl;
    outputStream << endl;
  }

  // Attempt to satisfy this function's convergence criterion before
  // the maximum number of iterations have been performed.
  try
  {
    while ( !isWithinTol && ( iterCurr <= nMaxIterAnyBox + 1 ) )
    {
# if 0
      //--------------------------------------------------------
      // See if this function's convergence criterion has been met.
      //--------------------------------------------------------

      // Calculate the current scaled projected gradient.
      calcScaledProjGrad( nObjParFree, yCurr, gScaled, gScaledProj );

      // If the scaled projected gradient is close to zero relative
      // to the Hessian and the smallest double value, then the
      // current y value is a local minimizer of the objective.
      if ( QN01Box::MaxAbs( nObjParFree, gScaledProj ) < 
        QN01Box::MaxAbs( nObjParFree * nObjParFree, hScaled ) * 1000.0 * DBL_EPSILON )
      {
        // If the scaled projected gradient is small enough, then
        // accept the current y value.
        isWithinTol = true;
        break;
      }
      else
      {
        // Set delta so that the optimizer will be able to perform at
        // least one Quasi-Newton iteration and so that the quadratic
        // subproblems will be solved with sufficient accuracy.
        delta = QN01Box::MaxAbs( nObjParFree, gScaledProj ) / 100.0;
    
        // The current sScaled is no longer valid because it depends
        // on the value for delta.
        isSScaledOk = false;

        // Check this function's convergence criterion 
        // unless the flag has been turned off.
        if ( checkConvAtBeginOfIter )
        {
          // Set the maximum number of iterations equal to the current
          // number of iterations.  This will allow the optimizer to
          // determine an accurate sScaled value without taking a step.
          iterMax  = iterCurr;
          quadCurr = 0;
    
          // Try to get the current sScaled value using a value for
          // level that turns off any tracing information.
          charMessage = QN01Box::QuasiNewton01Box(
            outputStream,
            noTracingLevel,
            iterMax,
            nQuadMax,
            nObjParFree,
            QN01Box::GradMaxAbs,
            delta,
            objective01Box,
            isSScaledOk,
            iterCurr,
            quadCurr,
            bfgsCurr,
            rScaled,
            fScaled,
            yCurr,
            sScaled,
            gScaled,
            hScaled );
    
          // Check this function's convergence criterion, if necessary.
          if ( isSScaledOk )
          {
            // The current y value satisfies this function's convergence 
            // criterion if
            //
            //     | yCurr - yHat |  <=  epsilon  ,
            //
            // where yHat is a local minimizer of the objective function.
            //
            // Note that sScaled is the solution to the optimizer's quadratic
            // subproblem and is the step the optimizer will take at the next
            // iteration, if it takes one.  This function assumes that if
            // sScaled is small enough, then the current y value is close to
            // yHat and sScaled is an accurate estimate of the distance to
            // the true local miminizer, i.e.,
            //
            //              ~
            //     sScaled  =  yCurr - yHat  .
            //
            // This assumption makes use of the fact that the Hessian
            // approximation is postive definite.
            //
            // To ensure that the final y value is within epsilon of the
            // solution, this function requires that sScaled be smaller
            // than epsilon.  The reason for this is that the quadratic
            // subproblem uses a quadratic approximation for the objective, 
            // which may not necessarily be accurate for the current y value.  
            //
            if ( QN01Box::MaxAbs( nObjParFree, sScaled ) < epsilon / 5.0 )
            {
              // If sScaled is not too large, increment the counter.
              nSScaledAccept++;
            }
            else
            {
              // If sScaled is too large, zero the counter.
              nSScaledAccept = 0;
            }
    
            // This function requires that nObjParFree + 1 consecutive acceptable
            // sScaled values must be calculated.  The reason for this is to
            // build up an accurate Hessian approximation so that the current
            // sScaled value, which is assumed to be the distance to the
            // solution, is accurate.
            if ( meetsConvCrit( nSScaledAccept, nObjParFree ) )
            {
              // If enough consecutive acceptable sScaled values have been
              // calculated, then accept the current y value.
              isWithinTol = true;
              break;
            }
          }
        }
      }

      // Reset this so that this function's convergence criterion
      // will be checked at the beginning of each iteration
      checkConvAtBeginOfIter = true;
# endif

      //--------------------------------------------------------
      // Ask the optimizer to perform a Quasi-Newton iteration.
      //--------------------------------------------------------

      // Don't perform any more iterations if the maximum have
      // already been performed.
      if ( iterCurr >= nMaxIterAnyBox + 1 )
      {
        break;
      }

      // Save the current optimizer state information before
      // attempting to perform the Quasi-Newton iteration.
      optInfo.setStateInfo( 
        nObjParFree,
        bfgsCurr,
        rScaled,
        fScaled,
        yCurr,
        gScaled,
        hScaled,
        nObjPar,
        pdXLowData,
        pdXUpData,
        indexYInX,
        nSScaledAccept );

      // Set these flags to indicate that the state information being
      // saved is from the beginning of the iteration and that a warm
      // start is possible.
      optInfo.setIsBeginOfIterStateInfo( true );
      optInfo.setIsWarmStartPossible   ( true );

      // Only perform a single Quasi-Newton iteration.
      iterBefore = iterCurr;
      iterMax    = iterCurr + 1;
      quadCurr   = 0;

      // Perform the single iteration using a negative value for level
      // that prevents the final values from being printed.
      charMessage = QN01Box::QuasiNewton01Box(
        outputStream,
        negLevel,
        iterMax,
        nQuadMax,
        nObjParFree,
        QN01Box::StepMaxAbs,
        delta,
        objective01Box,
        isSScaledOk,
        iterCurr,
        quadCurr,
        bfgsCurr,
        rScaled,
        fScaled,
        yCurr,
        sScaled,
        gScaled,
        hScaled );

      stringMessage = string(charMessage);
      if ( stringMessage == string("ok") )
          isWithinTol = true;
      else if ( stringMessage != string("ItrMax") )
      {
        // See if any of the free elements of the parameter is near
        // its bounds, i.e., within epsilon.
        for ( i = 0; i < nObjParFree; i++ )
        {
          if ( yCurr[i] < epsilon || 1.0 - yCurr[i] < epsilon )
          {
            stringMessage += "\nNote that some parameter values are at or near their bounds.";
            break;
          }
        }

        if ( level > 0 )
        {
          outputStream << endl;
          outputStream << stringMessage << endl;
          outputStream << endl;
        }

        throw SpkException( 
          SpkError::SPK_OPT_ERR,
          stringMessage.c_str(),
          __LINE__,
          __FILE__ );
      }

      // The number of iterations completed is one less than the
      // current iteration number.
      optInfo.setNIterCompleted( iterCurr - 1 );
    }
  }
  catch( SpkException& e )
  {
    throw e.push(
      SpkError::SPK_OPT_ERR, 
      "The optimization of the objective function failed.",
      __LINE__, 
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    throw SpkException(
      stde,
      "A standard exception was thrown during the optimization of the objective function.",
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
  // Print tracing for the final iteration if necessary.
  //------------------------------------------------------------

  // If the optimization was successful and there were more than
  // zero iterations requested, then print the tracing information
  // for the final iteration.
  if ( isWithinTol && nMaxIterAnyBox > 0 )
  {
    try
    {
      // Set this so that the optimizer won't try to perform another
      // Quasi-Newton iteration.
      iterMax  = iterCurr;
      quadCurr = 0;

      // Print the tracing information for the final iteration.
      charMessage = QN01Box::QuasiNewton01Box(
        outputStream,
        level,
        iterMax,
        nQuadMax,
        nObjParFree,
        QN01Box::StepMaxAbs,
        delta,
        objective01Box,
        isSScaledOk,
        iterCurr,
        quadCurr,
        bfgsCurr,
        rScaled,
        fScaled,
        yCurr,
        sScaled,
        gScaled,
        hScaled );
    }
    catch( SpkException& e )
    {
      throw e.push(
        SpkError::SPK_OPT_ERR, 
        "The optimization of the objective function failed.",
        __LINE__, 
        __FILE__ );
    }
    catch( const std::exception& stde )
    {
      throw SpkException(
        stde,
        "A standard exception was thrown during the optimization of the objective function.",
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
  }


  //------------------------------------------------------------
  // Check for parameter values that are at or near their bounds.
  //------------------------------------------------------------

  const string parAtOrNearBoundsMessage = 
    "Note that some parameter values are at or near their bounds.";

  bool parAtOrNearBounds = false;

  // See if any of the free elements of the parameter is at or near
  // its bounds, i.e., within epsilon.
  if ( !isWithinTol )
  {
    for ( i = 0; i < nObjParFree; i++ )
    {
      if ( yCurr[i] < epsilon || 1.0 - yCurr[i] < epsilon )
      {
        bool parAtOrNearBounds = true;
        break;
      }
    }
  }


  //------------------------------------------------------------
  // Check the status of the optimization.
  //------------------------------------------------------------

  bool ok = false;
  SpkError::ErrorCode errorCode;

  if ( isWithinTol )
  {
    //----------------------------------------------------------
    // This function's convergence criterion was satisfied.
    //----------------------------------------------------------

    if ( level > 0 && nMaxIterAnyBox > 0 )
    {
      outputStream << endl;
      outputStream << "Optimal parameter values found." << endl;
      outputStream << endl;
    }

    optInfo.setIsTooManyIter( false );
    ok = true;
  }
  else if ( iterCurr == nMaxIterAnyBox + 1 )
  {
    //----------------------------------------------------------
    // The maximum number of iterations have been performed.
    //----------------------------------------------------------

    if ( level > 0 && nMaxIterAnyBox > 0 )
    {
      outputStream << endl;
      outputStream << "Maximum number of iterations performed without convergence." << endl;
      outputStream << endl;
      if ( parAtOrNearBounds )
      {
        outputStream << parAtOrNearBoundsMessage << endl;
        outputStream << endl;
      }
    }

    optInfo.setIsTooManyIter( true );
    if ( optInfo.getThrowExcepIfMaxIter() )
    {
      errorCode = SpkError::SPK_TOO_MANY_ITER;
      stringMessage = "Maximum number of iterations performed without convergence.";
      if ( parAtOrNearBounds )
      {
        stringMessage += "\n" + parAtOrNearBoundsMessage;
      }
      ok = false;
    }
    else
    {
      ok = true;
    }
  }
  else
  {
    //----------------------------------------------------------
    // This function's convergence criterion was not satisfied.
    //----------------------------------------------------------

    if ( level > 0 && nMaxIterAnyBox > 0 )
    {
      outputStream << endl;
      outputStream << "Unable to find optimal parameter values." << endl;
      outputStream << endl;
      if ( parAtOrNearBounds )
      {
        outputStream << parAtOrNearBoundsMessage << endl;
        outputStream << endl;
      }
    }

    optInfo.setIsTooManyIter( false );
    errorCode = SpkError::SPK_NOT_CONVERGED;
    stringMessage = "Unable to find optimal parameter values in quasiNewtonAnyBox.";
    if ( parAtOrNearBounds )
    {
      stringMessage += "\n" + parAtOrNearBoundsMessage;
    }
    ok = false;
  }

  // If something went wrong, exit without setting the return values.
  if ( !ok )
  {
    throw SpkException(
      errorCode,
      stringMessage.c_str(),
      __LINE__,
      __FILE__ );
  }

  // Set this flag to indicate the main optimization loop did not
  // cause an error.
  optInfo.setDidOptFinishOk( true );


  //------------------------------------------------------------
  // Prepare for future quasiNewtonAnyBox warm starts.
  //------------------------------------------------------------
 
  // Save the optimization state information, if necessary.
  if ( optInfo.getSaveStateAtEndOfOpt() )
  {
    // Save the values at the end of the optimization.
    optInfo.setStateInfo( 
      nObjParFree,
      bfgsCurr,
      rScaled,
      fScaled,
      yCurr,
      gScaled,
      hScaled,
      nObjPar,
      pdXLowData,
      pdXUpData,
      indexYInX,
      nSScaledAccept );

    // Set these flags to indicate that the state information
    // corresponds to the state at the end of the last iteration 
    // and that a warm start is possible.
    optInfo.setIsBeginOfIterStateInfo( false );
    optInfo.setIsWarmStartPossible   ( true );
  }


  //------------------------------------------------------------
  // If the optimization didn't cause an exception, set the return values.
  //------------------------------------------------------------

  // If the final value for x should be returned, then compute it
  // from the final y value.
  if ( pdvecXOut ) 
  {
    double* pdXOutData = pdvecXOut->data();
    assert( pdvecXOut->nr() == nObjPar );
    assert( pdvecXOut->nc() == 1 );

    unscaleElem(
      nObjPar,
      yCurr,
      pdXLowData,
      pdXUpData,
      pdXDiffData,
      indexXFreeInY,
      pdXOutData );
  }

  // If the final value for the objective function should be
  // returned, then set it equal to the scaled value.
  if ( pdFOut )
  {
    *pdFOut = fScaled;
  }
  
  // If the final value for the unscaled gradient should be returned,
  // then compute it from the final scaled value.
  if ( pdrowF_xOut )
  {
    double* pdF_xOutData = pdrowF_xOut->data();
    assert( pdrowF_xOut->nr() == 1 );
    assert( pdrowF_xOut->nc() == nObjPar );

    unscaleGradElem(
      nObjPar,
      gScaled,
      pdXDiffData,
      indexXFreeInY,
      pdF_xOutData );
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
 *
 * Function: unscaleElem
 *
 *
 * Description
 * -----------
 *
 * Calculates the unscaled x value that corresponds to y.
 *
 *
 * Arguments
 * ---------
 *
 * nX
 *
 * Number of elements in x.
 *
 *
 * y
 *
 * The current value for y.  It must be of length nX or less.
 *
 *
 * xLow
 *
 * The lower bound for x.  It must be of length nX.
 *
 *
 * xUp
 *
 * The upper bound for x.  It must be of length nX.
 *
 *
 * xDiff
 *
 * The difference between the upper and lower bound for x.  It must be of
 * length nX.
 *
 *
 * indexXFreeInY
 *
 *
 * The indices for the free x elements in y.  It must be of length nX.
 * The elements corresponding to constrained elements of x are not used.
 *
 *
 * x
 *
 * On input, this must be allocated to hold nX elements.  On output, it
 * will contain the x value that corresponds to y.
 *
 *************************************************************************/

void unscaleElem(
  int            nX,
  const double*  y, 
  const double*  xLow, 
  const double*  xUp, 
  const double*  xDiff,
  const int*     indexXFreeInY, 
  double*        x )
{
  int i;

  for ( i = 0; i < nX; i++ )
  {
    if ( xDiff[i] != 0.0 )
    {
      // Transform the element of the y vector back to the unscaled 
      // form.  If it is at the upper bound, calculate the unscaled 
      // value in a way that avoids roundoff error.
      if ( y[indexXFreeInY[i]] != 1.0 )
      {
        x[i] = xLow[i] + y[indexXFreeInY[i]] * xDiff[i];
      }
      else
      {
        x[i] = xUp[i];
      }
    }
    else 
    {
      // This element is constrained by its lower and upper bounds.
      x[i] = xLow[i];
    }
  }

}


/*************************************************************************
 *
 * Function: scaleGradElem
 *
 *
 * Description
 * -----------
 *
 * Calculates the gradient of the scaled objective function with respect to y.
 *
 *
 * Arguments
 * ---------
 *
 * nY
 *
 * Number of elements in y.
 *
 *
 * g
 *
 * The gradient g(x) evaluated at the current x value.  It must be of length nY
 * or greater.
 *
 *
 * xDiff
 *
 * The difference between the upper and lower bound for x.  It must be of
 * the same length as g.
 *
 *
 * indexYInX
 *
 *
 * The indices for all of the y elements in x.  It must be of length nY.
 *
 *
 * gScaled
 *
 * On input, this must be allocated to hold nY elements.  On output, it
 * will contain the elements of the gradient of the scaled objective
 * function with respect to y.
 *
 *************************************************************************/

void scaleGradElem(
  int            nY,
  const double*  g, 
  const double*  xDiff,
  const int*     indexYInX, 
  double*        gScaled )
{
  int i;

  for ( i = 0; i < nY; i++ )
  {
    gScaled[i] = xDiff[indexYInX[i]] * g[indexYInX[i]];
  }
}


/*************************************************************************
 *
 * Function: unscaleGradElem
 *
 *
 * Description
 * -----------
 *
 * Calculates the gradient of the unscaled objective function with respect to x.
 *
 *
 * Arguments
 * ---------
 *
 * nX
 *
 * Number of elements in x.
 *
 *
 * gScaled
 *
 * The gradient of the scaled objective function with respect to y evaluated at
 * the current y value.  It must be of length nX or less.
 *
 *
 * xDiff
 *
 * The difference between the upper and lower bound for x.  It must be of
 * length nX.
 *
 *
 * indexXFreeInY
 *
 *
 * The indices for the free x elements in y.  It must be of length nX.
 * The elements corresponding to constrained elements of x are not used.
 *
 *
 * g
 *
 * On input, this must be allocated to hold nX elements.  On output, it
 * will contain the gradient of the unscaled objective function with respect 
 * to x.
 *
 *************************************************************************/

void unscaleGradElem(
  int            nX,
  const double*  gScaled, 
  const double*  xDiff,
  const int*     indexXFreeInY, 
  double*        g )
{
  int i;

  for ( i = 0; i < nX; i++ )
  {
    if ( xDiff[i] != 0.0 )
    {
      // Transform this element of the gradient back to the unscaled form.
      g[i] = gScaled[indexXFreeInY[i]] / xDiff[i];
    }
    else
    {
      // Set this scaled gradient element equal to zero since its corresponding
      // x element is constrained by its lower and upper bounds.
      g[i] = 0.0;
    }
  }
}


/*************************************************************************
 *
 * Function: initHessApprox
 *
 *
 * Description
 * -----------
 *
 * Calculates an initial version of the approximation for the Hessian of
 * the objective function that is positive definite and well conditioned.
 *
 *
 * Arguments
 * ---------
 *
 * n
 *
 * Number of elements in x.
 *
 *
 * gCurr
 *
 * The gradient g(x) evaluated at xCurr.  It must be of length n.
 *
 *
 * h
 *
 * On input, this must be allocated to hold n * n elements.  On output, 
 * it will contain the approximation for the Hessian.
 *
 *************************************************************************/

void initHessApprox(
  int            n       ,
  const double*  gCurr   ,
  double*        h       )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int i;
  int j;


  //------------------------------------------------------------
  // Set the diagonal elements of the Hessian approximation.
  //------------------------------------------------------------

  double hDiagMin = 0.0;
  double hDiagMax = 0.0;

  // Calculate approximations for the diagonals of the Hessian.
  for ( i = 0; i < n; i++ )
  {
    h[i * n + i] = 2.0 * fabs( gCurr[i] );

    // Save the largest diagonal absolute value.
    if ( fabs( h[i * n + i] ) > hDiagMax )
    {
      hDiagMax = fabs( h[i * n + i] );
    }
  }


  //------------------------------------------------------------
  // Precondition the approximate Hessian.
  //------------------------------------------------------------

  // Set the minimum diagonal element that will be allowed.
  if ( hDiagMax > 0.0 )
  {
    // If at least one of the Hessian diagonals is greater than
    // zero, then set the minimum value so that the condition
    // number of the Hessian approximation won't be too large.
    hDiagMin = 1.0e-5 * hDiagMax;
  }
  else
  {
    // If the Hessian diagonals are all less than or equal
    // to zero, then set the minimum value so that the Hessian 
    // approximation will just be the identity matrix.
    hDiagMin = 1.0;
  }

  // Precondition the approximate Hessian by setting its diagonal
  // elements in a way that keeps its condition number reasonable.
  for ( i = 0; i < n; i++ )
  {
    // Reset diagonal elements that are too small;
    if ( h[i * n + i] < hDiagMin )
    {
      h[i * n + i] = hDiagMin;
    }

    // Set the off diagonal elements.
    for ( j = 0; j < n; j++ )
    {
      if ( i != j )
      {
        h[i * n + j] = 0.0;
      }
    }
  }

}


/*************************************************************************
 *
 * Function: meetsConvCrit
 *
 *
 * Returns true if the number of consecutive acceptable values
 * nAccept is greater than the number of parameters nPar.
 *
 *************************************************************************/

bool meetsConvCrit( int nAccept, int nPar )
{
  return ( nAccept > nPar );
}


/*************************************************************************
 *
 * Function: calcScaledProjGrad
 *
 *
 * Description
 * -----------
 *
 * Calculates the scaled projected gradient.
 *
 * The projected gradient is the gradient multiplied by the 
 * distance to the parameter's upper or lower bound depending on
 * whether the gradient is negative or nonnegative, respectively.
 *
 *
 * Arguments
 * ---------
 *
 * n
 *
 * Number of elements in y.
 *
 *
 * yCurr
 *
 * The current value for y.  It must be of length n.
 *
 *
 * gScaled
 *
 * The gradient gScaled(y) evaluated at yCurr.  It must be of length n.
 *
 *
 * gScaledProj
 *
 * On input, this must be allocated to hold n elements.  On output, it
 * will contain the scaled projected gradient evaluated at yCurr.
 *
 *************************************************************************/

void calcScaledProjGrad(
  int            n,
  const double*  yCurr,
  const double*  gScaled,
  double*        gScaledProj )
{
  int i;

  // Calculate the scaled projected gradient.
  for ( i = 0; i < n; i++ )
  {
    if ( gScaled[i] >= 0.0 )
    {
      gScaledProj[i] = ( yCurr[i] - 0.0 ) * gScaled[i];
    }
    else
    {
      gScaledProj[i] = ( 1.0 - yCurr[i] ) * gScaled[i];
    }
  }

}


/*************************************************************************
 *
 * Function: isAllZero
 *
 *
 * Returns true if all of the elements of x are zero.  Otherwise it 
 * returns false.
 *
 *************************************************************************/

bool isAllZero( int n, const double* x )
{
  int i;

  for ( i = 0; i < n; i++ )
  {
    if ( x[i] )
    {
      return false;
    }
  }

  return true;
}


} // [End: unnamed namespace]
