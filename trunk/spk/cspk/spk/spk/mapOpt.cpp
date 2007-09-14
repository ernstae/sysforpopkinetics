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
 * File: mapOpt.cpp
 *
 *
 * Minimizes the map Bayesian objective function.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: mapOpt
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*

$begin mapOpt$$

$spell
  throwExcepIfMaxIter
  struct 
  Model model
  valarray
  Rval
  fval
  Bayesian
  ok
  exp
  mitr
  diag
  Obj
  pdvec
  dmat
  const
  dvec
  int
  Max
  Iter
  pd
  pdrow
  pdmat
  bool
  epsilon
  inteval
  stdout
  th
  Vi
  Varbl
  Pars
  drow
  Info
  cout
  setiosflags
  setprecision
  endl
  nr
  nc
  iostream
  iomanip
  cmath
  Fb
  Rb
  namespace
  std
  ios
  Goddard
  covariance
  covariances
  cstdlib 
  doSetIndPar 
  int 
  pf 
  pb 
  ind
  cerr
  Spk
  inv
  optInfo
  fp
  Ri
  Optimizer optInfo
  Fo
  pdvec
$$

$section Optimizing The Map Bayesian Objective Function$$

$cindex map bayesian optimization$$
$index mapOpt$$
$index individual, map bayesian optimization$$

$table
$bold Prototype:$$ $cend
$syntax/void mapOpt(  
    SpkModel<double>   & /model/,
    const DoubleMatrix & /dvecY/,
    Optimizer          & /optInfo/,
    const DoubleMatrix & /dvecBLow/,
    const DoubleMatrix & /dvecBUp/,
    const DoubleMatrix & /dvecBIn/,
    DoubleMatrix       * /pdvecBOut/,
    const DoubleMatrix & /dvecBStep/,
    double             * /pdMapObjOut/,
    DoubleMatrix       * /pdrowMapObj_bOut/,
    DoubleMatrix       * /pdmatMapObj_b_bOut/,
    bool                 /withD/,
    bool                 /isFo/ = false,
    const DoubleMatrix* /pdvecN/ = NULL,
    const DoubleMatrix* /pdvecBMean/ = NULL
)/$$

$tend

$table
$bold See also: $$ $cend
$tref Optimizer$$  $rend
$cend
$tref mapObj$$     $rend
$cend
$tref mapObjDiff$$ $rend
$cend
$tref mapOpt$$     $rend
$tend

$fend 35$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
Minimizes the map Bayesian objective function.  
$pre

$$
To be specific, this function solves the problem 
$math%
    \minimize MapObj(b) \with \respect \to b
    \subject \to bLow \le b \le bUp
%$$
where the function $math%MapObj(b)%$$ is defined by
$math%
                1 %          %            1           T     -1
    MapObj(b) = - \logdet[ 2 \pi R(b) ] + - [y - f(b)]  R(b)   [y - f(b)]
                2 %          %            2

                1 %          %            1            T  -1
              + - #logdet[ 2 #pi D ]    + - [bMean - b]  D  [bMean - b]  .
                2 %          %            2
%$$
Note that this objective function allows a nonzero mean value
$math%bMean%$$ to be specified for the parameter $math%b%$$.
$pre

$$
(The equation above uses
$xref/glossary/Individual Notation/individual notation/$$.)

$head Reference$$
B. M. Bell, $italic Approximating The Marginal Likelihood Estimate 
For Models With Random Parameters$$, Applied Mathematics and Computation, 
Accepted 1999. 

$head Return Value$$
Upon a successful completion, the function sets
the given output value place holders to point to the result values 
(ones that are requested).  The case that too-many-iter occurred during 
the optimization process is not a successful completion. 
$pre

$$
If an error is detected or failure occurs during the evaluation, a SpkException object is
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Note$$

The length of the random population parameter vector $math%b%$$ is 
specified by the number of rows in the argument $italic dvecBIn$$, 
which is a column vector containing the initial value for $math%b%$$.

$head Arguments$$
$syntax/
/model/
/$$
is a pointer to a SpkModel object that is a function of $math%b%$$ if $italic withD$$ is $math%false%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on only b/$$)
or a function of $math%alp%$$ and $math%b%$$ if $italic withD$$ is $math%true%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on alp and b/$$).
for details.

$syntax/

/dvecY/
/$$
The $code DoubleMatrix$$ $italic dvecY$$ contains the column vector 
$math%y%$$, which specifies the data.

$syntax/

/optInfo/
/$$

This $xref/Optimizer//Optimizer/$$ object contains the information 
that controls the individual level optimization process.
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
Note that the upper and lower bounds for $math%b%$$ must be the 
same as they were during the earlier call to this function.
$pre

$$
Most of the optimizer information is accessible directly via public
get functions, e.g., the value epsilon is returned by the Optimizer 
class function $code getEpsilon()$$.  
The following subsections specify how this function uses 
some of the elements of the Optimizer object that are accessed 
directly using get functions.

$subhead optInfo.epsilon$$
This real number is used to specify the convergence criteria
for the optimizer.
It must be greater than $math%0.0%$$.
$pre

$$
A individual parameter value $math%bOut%$$ is accepted as an estimate for 
$math%bHat%$$ if 
$math%
        abs( bOut - bHat ) \le epsilon ( bUp - bLow )  ,
%$$
where $math%abs%$$ is the element-by-element absolute value function
and $math%bHat%$$ is a local minimizer of $math%MapObj(b)%$$.
Since $math%bHat%$$ is unknown, this function estimates the left hand
side of this inequality in a way that is a good approximation when 
the Hessian of the objective function is positive definite.
$pre

$$
Note that if $italic nMaxIter$$ is set to zero, then $math%bIn%$$ is 
accepted as the estimate for $math%bHat%$$.

$subhead optInfo.nMaxIter$$
This integer must be greater than or equal to zero.
It specifies the maximum number of 
iterations to attempt before giving up on convergence.
If it is equal to zero, then the initial
value for $math%b%$$ is accepted as the final value, and any requested output
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

$syntax/

/dvecBLow/
/$$
The $code DoubleMatrix$$ $italic dvecBLow$$ contains the column vector 
$math%bLow%$$, which specifies the lower limit for the parameter vector 
$math%b%$$ during the optimization procedure.
The length of $italic dvecBLow$$ is equal to the length of 
the random population parameter vector $math%b%$$.

$syntax/

/dvecBUp/
/$$
The $code DoubleMatrix$$ $italic dvecBUp$$ contains the column vector 
$math%bUp%$$, which specifies the upper limit for the parameter vector 
$math%b%$$ during the optimization procedure.
The length of $italic dvecBUp$$ is equal to the length of 
the random population parameter vector $math%b%$$.

$syntax/

/dvecBIn/
/$$
The $code DoubleMatrix$$ $italic dvecBIn$$ contains the column vector 
$math%bIn%$$, which specifies the initial value for the parameter vector 
$math%b%$$.
The $xref/glossary/Ordering Of Vectors/order condition/$$,
$math%bLow \le bIn \le bUp%$$, is assumed to hold.
Note that the length of $italic dvecBIn$$ specifies the length of 
the random population parameter vector $math%b%$$.

$syntax/

/pdvecBOut/
/$$
If $italic pdvecBOut$$ is not $code NULL$$, then the $code DoubleMatrix$$ 
pointed to by $italic pdvecBOut$$ must be declared in the function that 
calls this function, and it must have the same dimensions as $italic dvecBIn$$.
If $italic pdvecBOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then the $code DoubleMatrix$$ pointed to by $italic pdvecBOut$$ will contain 
the column vector $math%bOut%$$, which is the estimate for the true minimizer 
of the objective function.
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdvecBOut$$.

$syntax/

/dvecBStep/
/$$
The $code DoubleMatrix$$ $italic dvecBStep$$ contains the column vector 
$math%bStep%$$, which specifies the step size used for approximating
the derivative of $math%MapObj_b(b)%$$.
This is not used if $italic pdmatMapObj_b_bOut$$ is equal to zero.
The length of $italic dvecBStep$$ is equal to the length of 
the random population parameter vector $math%b%$$.

$syntax/

/pdMapObjOut/
/$$
If $italic pdMapObjOut$$ is not $code NULL$$, then the $code double$$ 
value pointed to by $italic pdMapObjOut$$ must be declared in the 
function that calls this function.
If $italic pdMapObjOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then the $code double$$ value pointed to by $italic pdMapObjOut$$ will 
be equal to $math%MapObj(bOut)%$$, which is the value of the objective 
function evaluated at $math%bOut%$$.
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdMapObjOut$$.

$syntax/

/pdrowMapObj_bOut/
/$$
If $italic pdrowMapObj_bOut$$ is not $code NULL$$, then the 
$code DoubleMatrix$$ pointed to by $italic pdrowMapObj_bOut$$ 
must be declared in the function that calls this function, and it 
must be a row vector that is the same length as
the random population parameter vector $math%b%$$.
If $italic pdrowMapObj_bOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then the $code DoubleMatrix$$ pointed to by $italic pdrowMapObj_bOut$$ 
will contain the row vector $math%MapObj_b(bOut)%$$, which is
the derivative of the objective function evaluated at $math%bOut%$$. 
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdrowMapObj_bOut$$.

$syntax/

/pdmatMapObj_b_bOut/
/$$
If $italic pdmatMapObj_b_bOut$$ is not $code NULL$$, then the 
$code DoubleMatrix$$ pointed to by $italic pdmatMapObj_b_bOut$$ 
must be declared in the function that calls this function, and it 
must have the same number of rows and columns as the length of
the random population parameter vector $math%b%$$.
If $italic pdmatMapObj_b_bOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then the $code DoubleMatrix$$ pointed to by 
$italic pdmatMapObj_b_bOut$$ will contain the matrix 
$math%MapObj_b_b(bOut)%$$, which is an approximation for the
second derivative of the objective function evaluated at $math%bOut%$$.  
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdmatMapObj_b_bOut$$.
The approximation for the second derivative is formed using central
differences of the function $math%MapObj_b(b)%$$ with
step sizes specified by $italic dvecBStep$$.

$syntax/

/withD/
/$$
When $italic withD$$ is specified $math%false%$$,
the system assumes that the prior distribution is provided by the user.
In such a case, $italic withD$$ is a function of $math%alp%$$ and $math%b%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on alp and b/$$).
Otherwise, $italic model$$ is a function of only $math%b%$$
(refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend on only b/$$)

$syntax/

/isFO/
/$$
If $italic isFO$$ is $math%false%$$, then the Modified First Order will be chosen for approximation.
If $math%true%$$ is given, other approximations are assumed.


$syntax/

/pdvecN/(null by default)
/$$ 
If $italic isFO$$ is specified as $math%true%$$, $italic pdvecN$$ points to a DoubleMatrix 
object that contains the column vector $math%N%$$.  The $th i$$ element of $math%N%$$
specifies the number of elements of $math%y%$$ that correspond to the $th i$$ individual.
If $italic isFO$$ is specified as $math%false%$$, set $italic N$$ to null.

$syntax/

/pdvecBMean/(null by default)
/$$ 
If the pointer $italic pdvecBMean$$ is not equal to null, then it points to a DoubleMatrix 
object that contains the column vector $math%bMean%$$.  The $th j$$ element of $math%bMean%$$
specifies the mean value for the $th j$$ element of $math%b%$$.
If the mean values for all of the elements of $math%b%$$ are equal to zero, 
set $italic pdvecBMean$$ to null.


$head Example$$

$escape #$$

Suppose that
$math%
       /  #exp[b(1)]     0  \         / 1  0 \
R(b) = |  %                 |    D  = |      |
       \  0      #exp[b(1)] /         \ 0 .5 /

       / b(2) \        / 2 \
f(b) = |      |   y =  |   |
       \ b(2) /        \ 2 /
%$$
It follows that
$math%                         
MapObj(b) = #log{2 #pi #exp[b(1)]} + [2 - b(2)]^2 #exp[-b(1)]
          + #log(2 #pi)%           + (1/2) b(1)^2 +  b(2)^2
%$$
The gradient of $math%MapObj(b)%$$ is equal to
$math%
  / 1 - [2 - b(2)]^2 #exp[-b(1)] + b(1) \
  |               %                     |
  \ -2 [2 - b(2)] #exp[-b(1)] + 2 b(2)  /
%$$
The first order necessary condition for a minimum is 
that the gradient is zero. This is true when
$math%b(1) = 0%$$ and $math%b(2) = 1%$$.
$pre

$$
Taking the derivative of the gradient above,
$math%
                / [2-b(2)]^2#exp[-b(1)]+1   2[2-b(2)]#exp[-b(1)] \
MapObj_b_b(b) = |           %                  %                 |
                \  2[2-b(2)]#exp[-b(1)]       2#exp[-b(1)] + 2   /
%$$
substituting in 
$math%b(1) = 0%$$ and $math%b(2) = 1%$$, we obtain
$math%
  / 2  2 \
  |      |  .
  \ 2  4 /
%$$

$pre

$$
If you compile, link, and run the following program:
$codep

#include "mapOpt.h"
#include "SpkModel.h"
#include "DoubleMatrix.h"
#include "pi.h"
#include "allZero.h"
#include "Optimizer.h"
#include <iostream>
#include <iomanip>
#include <string>
#include <cmath>
#include <cstdlib>

static DoubleMatrix funF(  const DoubleMatrix &dvecB );
static DoubleMatrix funF_b(const DoubleMatrix &dvecFb, 
                           const DoubleMatrix &dvecB );
static DoubleMatrix funR(  const DoubleMatrix &dvecB );
static DoubleMatrix funR_b(const DoubleMatrix &dmatRb, 
                           const DoubleMatrix &dvecB );

class IndModel : public SpkModel<double>
{
    DoubleMatrix _b;
public:
    IndModel(){}
    ~IndModel(){}
protected:
    void doSetIndPar(const valarray<double>& b)
    {
        _b = DoubleMatrix( b, 1 );
    }
    void doDataMean( valarray<double>& ret ) const
    {
        ret = funF(_b).toValarray();
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        doDataMean(ret);
        ret = funF_b(ret, _b).toValarray();
        return !allZero(ret);
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        ret = funR(_b).toValarray();
    }
    void doDataVariance_indPar( valarray<double>& ret ) const
    {
        doDataVariance(ret);
        ret = funR_b(ret, _b).toValarray();
        return !allZero(ret);
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
      DoubleMatrix dmatD( 2, 2 );
      double* pdDData = dmatD.data();
      pdDData[0] = 1.0;
      pdDData[1] = 0.0;
      pdDData[2] = 0.0;
      pdDData[3] = 0.5;
      re = dmatD.toValarray();
    }
};

//--------------------------------------------------------------
//
// Function: main
//
//--------------------------------------------------------------

int main()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  IndModel model;


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  int nY = 2;
  DoubleMatrix dvecY( nY, 1 );
  dvecY.fill( 2.0 );


  //------------------------------------------------------------
  // Quantities related to the objective function parameter, b.
  //------------------------------------------------------------

  int nObjPars = 2;

  DoubleMatrix dvecBLow ( nObjPars, 1 );
  DoubleMatrix dvecBUp  ( nObjPars, 1 );
  DoubleMatrix dvecBIn  ( nObjPars, 1 );
  DoubleMatrix dvecBOut ( nObjPars, 1 );
  DoubleMatrix dvecBStep( nObjPars, 1 );

  dvecBLow .fill( -4.0 );
  dvecBUp  .fill(  4.0 );
  dvecBIn  .fill(  2.0 );
  dvecBStep.fill(  0.001 );


  //------------------------------------------------------------
  // Quantities related to the objective function, MapObj(b).
  //------------------------------------------------------------

  double dMapObjOut;

  DoubleMatrix drowMapObj_bOut  ( 1, nObjPars );
  DoubleMatrix dmatMapObj_b_bOut( nObjPars, nObjPars );


  //------------------------------------------------------------
  // Remaining inputs to mapOpt.
  //------------------------------------------------------------

  double epsilon  = 1.e-3; 
  int nMaxIter    = 40; 
  double fOut     = 0.0; 
  int level       = 0;
  Optimizer optInfo(epsilon, nMaxIter, level);
  void* pFvalInfo = 0;
  bool withD      = true;
  bool isFO       = false;
  DoubleMatrix * pdvecN = NULL;
  DoubleMatrix * pdvecBMean = NULL;


  //------------------------------------------------------------
  // Optimize MapObj(b).
  //------------------------------------------------------------

  // try mapOpt(...) which may throw an exception.
  bool ok = true;
  try{
     mapOpt(model,
            dvecY,
            optInfo,
            dvecBLow,
            dvecBUp,
            dvecBIn,
            &dvecBOut,
            dvecBStep,
            &dMapObjOut,
            &drowMapObj_bOut,
            &dmatMapObj_b_bOut,
            withD,
            isFO
            pdvecN,
            pdvecBMean
            );
  }
  catch( ... )
  {
    cerr << "mapOpt failed" << endl;
    abort();
  }

  //------------------------------------------------------------
  // Print the results.
  //------------------------------------------------------------

  double dMapObjKnown = 2.0 * log( 2.0 * PI ) - 0.5 * log( 2.0 ) + 2.0;

  cout << setiosflags(ios::scientific) << setprecision(15);

  cout << "ok             = " << ( ok ? "True" : "False" ) << endl;

  cout << "bOut           = " << endl;
  dvecBOut.print();
  cout << "MapObjOut      = " << dMapObjOut << endl;
  cout << "MapObj (known) = " << dMapObjKnown << endl;
  cout << "MapObj_bOut    = " << endl;
  drowMapObj_bOut.print();
  cout << "MapObj_b_bOut  = " << endl;
  dmatMapObj_b_bOut.print();

  return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}


//--------------------------------------------------------------
//
// Function: funR
//
//
// Calculates the covariance of the measurement error:
//
//            /  exp[b(1)]     0  \ 
//     R(b) = |                   |   .
//            \  0      exp[b(1)] / 
//
//--------------------------------------------------------------

static DoubleMatrix funR( const DoubleMatrix &dvecB )
{
  DoubleMatrix dmatR( 2, 2 );
  double *pdRData = dmatR.data();
  const double *pdBData = dvecB.data();
  
  pdRData[0] = exp( pdBData[0] );
  pdRData[1] = 0.0;
  pdRData[2] = 0.0;
  pdRData[3] = exp( pdBData[0] );
  
  return dmatR;
}


//--------------------------------------------------------------
//
// Function: funR_b
//
//
// Calculates the derivative of the measurement covariance
// with respect to b:
//
//              /  exp[b(1)]     0  \ 
//     R_b(b) = |  0             0  |   .
//              |  0             0  | 
//              \  exp[b(1)]     0  / 
//
//--------------------------------------------------------------

static DoubleMatrix funR_b( const DoubleMatrix &dmatR, 
                            const DoubleMatrix &dvecB )
{
  DoubleMatrix dmatR_b( dmatR.nr()*dmatR.nc(), dvecB.nr() );
  double *pdR_bData = dmatR_b.data();
  const double *pdBData   = dvecB.data();
  
  dmatR_b.fill(0.0);
  pdR_bData[0] = exp( pdBData[0] );
  pdR_bData[3] = exp( pdBData[0] );
  
  return dmatR_b;
}


//--------------------------------------------------------------
//
// Function: funF
//
//
// Calculates the model for the mean of the measurements:
//
//            / b(2) \ 
//     f(b) = |      |   .
//            \ b(2) /
//
//--------------------------------------------------------------

static DoubleMatrix funF( const DoubleMatrix &dvecB )
{
  DoubleMatrix dvecF(2,1);
  double *pdFData = dvecF.data();
  const double *pdBData = dvecB.data();

  pdFData[0] = pdBData[1];
  pdFData[1] = pdBData[1];

  return dvecF;
}


//--------------------------------------------------------------
//
// Function: funF_b
//
//
// Calculates the derivative of the model for the mean of the 
// measurements with respect to b:
//
//              / 0   1 \ 
//     f_b(b) = |       |   .
//              \ 0   1 /
//
//--------------------------------------------------------------

static DoubleMatrix funF_b( const DoubleMatrix &dvecF, 
                            const DoubleMatrix &dvecB )
{
  DoubleMatrix dmatF_b(2,2);
  double *pdF_bData = dmatF_b.data();
  
  pdF_bData[0] = 0.0;
  pdF_bData[1] = 0.0;
  pdF_bData[2] = 1.0;
  pdF_bData[3] = 1.0;
  
  return dmatF_b;
}
  
$$
then it will display the following when it is run:
$codep

ok             = True
bOut           =
[ 2.260754053295955e-006 ]
[ 9.999994638370806e-001 ]
MapObjOut      = 5.329180542541980e+000
MapObj (known) = 5.329180542538718e+000
MapObj_bOut    =
[ 3.449181849135563e-006 2.376853741825613e-006 ]
MapObj_b_bOut  =
[ 1.999998978238548e+000 1.999996550820227e+000 ]
[ 1.999996884152977e+000 3.999995478496610e+000 ]
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

// SPK library header files.
#include "mapOpt.h"
#include "mapObj.h"
#include "mapObjDiff.h"
#include "quasiNewtonAnyBox.h"
#include "SpkException.h"
#include "WarningsManager.h"

// SPK optimizer header files.
#include <QN01Box/PlusInfinity.h>

// Standard library header files.
#include <cmath>
#include <cassert>


/*------------------------------------------------------------------------
 * Local class declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  //**********************************************************************
  //
  // Class: MapOptObj
  //
  //
  // Evaluates the MAP Bayesian objective function and/or its gradient.
  //
  //**********************************************************************

  class MapOptObj : public QuasiNewtonAnyBoxObj
  {
    //----------------------------------------------------------
    // Constructors.
    //----------------------------------------------------------

  public:
    MapOptObj( 
      int                  nBIn,
      SpkModel<double>*    pModelIn,
      const DoubleMatrix*  pdvecYIn,
      const bool*          pbWithDIn,
      const bool*          pbIsFoIn,
      const DoubleMatrix*  pdvecNIn,
      const DoubleMatrix*  pdvecBMeanIn,
      Optimizer*           pOptInfoIn )
      :
      nB           ( nBIn ),
      pModel       ( pModelIn ),
      pdvecY       ( pdvecYIn ),
      pbWithD      ( pbWithDIn ),
      pbIsFo       ( pbIsFoIn ),
      pdvecN       ( pdvecNIn ),
      pdvecBMean   ( pdvecBMeanIn ),
      pOptInfo     ( pOptInfoIn )
    {
      // Give the optimizer controller a pointer to this objective.
      pOptInfo->setObjFunc( this );
    }

    //----------------------------------------------------------
    // Data members.
    //----------------------------------------------------------

  private:
    const int nB;

    DoubleMatrix dvecBCurr;

    // This information is required by mapObj.
    SpkModel<double>*    pModel;
    const DoubleMatrix*  pdvecY;
    const bool*          pbWithD;
    const bool*          pbIsFo;
    const DoubleMatrix*  pdvecN;
    const DoubleMatrix*  pdvecBMean;

    Optimizer*  pOptInfo;

  public:
    static int  nBackupMessage;


    //----------------------------------------------------------
    // Functions required by quasiNewtonAnyBox.
    //----------------------------------------------------------

  public:
    //**********************************************************
    // 
    // Function: function
    //
    //
    // Evaluates the MAP Bayesian objective function MapObj(b).
    //
    //**********************************************************

    void function( const DoubleMatrix& dvecBIn, double* pdMapObjOut )
    {
      // Set the current value for b.
      dvecBCurr = dvecBIn;
      assert( dvecBIn.nr() == nB );
      assert( dvecBIn.nc() == 1 );

      // Evaluate the MAP Bayesian objective function.
      double dMapObjCurr = 0.0;
      DoubleMatrix* pdmatNull = 0;
      try
      {
        mapObj( 
          *pModel, 
          *pdvecY, 
          dvecBCurr, 
          &dMapObjCurr,
          pdmatNull,
          *pbWithD,
          *pbIsFo, 
          pdvecN,
          pdvecBMean );
      }
      catch( SpkException& e )
      {
        // See if the model for the individual's data mean was equal
        // to a Not a Number (NaN) or infinity and that there were no
        // standard errors.
        if ( e.find( SpkError::SPK_MODEL_DATA_MEAN_NAN_OR_INF_ERR ) >= 0 &&
             e.find( SpkError::SPK_STD_ERR                        ) <  0    )
        {
          // Issue a warning message if one hasn't been issued.
          if ( nBackupMessage == 0 )
          {
            // Set the message.
              WarningsManager::addWarning(
                "Backed up individual optimization because an individual's data mean could \nnot be calculated for a particular value of the individual parameters.",
                __LINE__,
                __FILE__ );
          }

          // Set the individual objective value that indicates
          // to the individual optimizer that it should back up.
          *pdMapObjOut = QN01Box::PlusInfinity( double( 0 ) );

          // Increment this to indicate this objective has backed up.
          nBackupMessage++;

          return;
        }
        else
        {
          throw e.push(
            SpkError::SPK_OPT_ERR, 
            "The individual's objective function could not be calculated.",
            __LINE__, 
            __FILE__ );
        }
      }

      // Set the objective function value.
      *pdMapObjOut = dMapObjCurr;
    }


    //**********************************************************
    // 
    // Function: gradient
    //
    //
    // Evaluate the gradient of the MAP Bayesian objective function MapObj_b(b).
    //
    //**********************************************************

    virtual void gradient( DoubleMatrix* pdrowMapObj_bOut ) const
    {
      assert( pdrowMapObj_bOut->nr() == 1 );
      assert( pdrowMapObj_bOut->nc() == nB );

      // Evaluate the gradient of the MAP Bayesian objective function.
      double* pdNull = 0;
      DoubleMatrix drowMapObj_bCurr( 1, nB );
      try
      {
        mapObj(
          *pModel,
          *pdvecY,
          dvecBCurr,
          pdNull,
          &drowMapObj_bCurr,
          *pbWithD,
          *pbIsFo,
          pdvecN,
          pdvecBMean );
      }
      catch( SpkException& e )
      {
        throw e.push(
          SpkError::SPK_OPT_ERR, 
          "The gradient of the individual's objective function could not be calculated.",
          __LINE__, 
          __FILE__ );
      }

      // Set the gradient value.
      *pdrowMapObj_bOut = drowMapObj_bCurr;
    }

  };

  int MapOptObj::nBackupMessage = 0;

} // [End: unnamed namespace]



/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void mapOpt(  SpkModel<double>&   model,
              const DoubleMatrix& dvecY,
              Optimizer& optInfo,
              const DoubleMatrix& dvecBLow,
              const DoubleMatrix& dvecBUp,
              const DoubleMatrix& dvecBIn,
              DoubleMatrix* pdvecBOut,
              const DoubleMatrix& dvecBStep,
              double* pdMapObjOut,
              DoubleMatrix* pdrowMapObj_bOut,
              DoubleMatrix* pdmatMapObj_b_bOut,
              bool withD,
              bool isFo,
              const DoubleMatrix* pdvecN,
              const DoubleMatrix* pdvecBMean
           )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nBRows = dvecBIn.nr();

  double epsilon  = optInfo.getEpsilon();
  int    nMaxIter = optInfo.getNMaxIter();
  int    level    = optInfo.getLevel();


  //------------------------------------------------------------
  // Validate the inputs.
  //------------------------------------------------------------

  if ( dvecY.nr() == 0  )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR, 
      "Individual level optimization was not attempted because there \nwere no data values for this individual.", 
      __LINE__,
      __FILE__ );  
  }


  //------------------------------------------------------------
  // Prepare the inputs for quasiNewtonAnyBox.
  //------------------------------------------------------------

  // Construct the MAP Bayesian objective function.
  MapOptObj mapOptObj(
    nBRows,
    &model,
    &dvecY,
    &withD,
    &isFo,
    pdvecN,
    pdvecBMean,
    &optInfo );

  // Instantiate a temporary column vector to hold the final b 
  // value that will be returned by quasiNewtonAnyBox.
  DoubleMatrix dvecBOutTemp( nBRows, 1 );

  // If this function is going to return the objective function 
  // value, initialize the temporary value to hold it.  Otherwise, 
  // set the temporary pointer to zero so that quasiNewtonAnyBox will not 
  // return it either.
  double dMapObjOutTemp;
  double* pdMapObjOutTemp = &dMapObjOutTemp;
  if ( pdMapObjOut )
  {
    dMapObjOutTemp = 0.0;
  }
  else
  {
    pdMapObjOutTemp = 0;
  }
  
  // If this function is going to return the derivative of the
  // objective function with respect to b, instantiate a 
  // temporary row vector to hold it.  Otherwise, set the temporary 
  // pointer to zero so that mapObj will not return it either.
  DoubleMatrix drowMapObj_bOutTemp;
  DoubleMatrix* pdrowMapObj_bOutTemp = &drowMapObj_bOutTemp;
  if ( pdrowMapObj_bOut )
  {
    drowMapObj_bOutTemp.resize( 1, nBRows );
  }
  else
  {
    pdrowMapObj_bOutTemp = 0;
  }
  
  // If this function is going to return the second derivative of 
  // the objective function with respect to b, instantiate a 
  // temporary matrix to hold it.  Otherwise, set the temporary 
  // pointer to zero so that mapObj will not return it either.
  DoubleMatrix dmatMapObj_b_bOutTemp;
  DoubleMatrix* pdmatMapObj_b_bOutTemp = &dmatMapObj_b_bOutTemp;
  if ( pdmatMapObj_b_bOut )
  {
    dmatMapObj_b_bOutTemp.resize( nBRows, nBRows );
  }
  else
  {
    pdmatMapObj_b_bOutTemp = 0;
  }


  //------------------------------------------------------------
  // Handle nonzero iterations for the objective function.
  //------------------------------------------------------------

  if ( nMaxIter > 0 )
  {
    // If the number of iterations is not zero, then the objective 
    // function must be optimized in order to determine bOut.
    try
    {
      quasiNewtonAnyBox(
        mapOptObj,
        optInfo, 
        dvecBLow, 
        dvecBUp, 
        dvecBIn, 
        &dvecBOutTemp, 
        pdMapObjOutTemp, 
        pdrowMapObj_bOutTemp );
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
        "An exception of unknown type was thrown during the individual level optimization.", 
        __LINE__, __FILE__);
    }
  }


  //------------------------------------------------------------
  // Handle zero iterations for the objective function.
  //------------------------------------------------------------

  if ( nMaxIter == 0 )
  {
    // If the number of iterations is zero, then bIn is the
    // desired value for bOut.
    dvecBOutTemp = dvecBIn;
    
    // Calculate MapObj(bOut) and MapObj_b(bOut), if either of
    // their values is going to be returned by this function.
    if ( pdMapObjOut || pdrowMapObj_bOut )
    {
      mapObj( 
        model, 
        dvecY, 
        dvecBOutTemp,
        pdMapObjOutTemp,
        pdrowMapObj_bOutTemp,
        withD,
        isFo,
        pdvecN,
        pdvecBMean );
    }
  }


  //------------------------------------------------------------
  // Compute the second derivative of the objective function.
  //------------------------------------------------------------

  // Compute the second derivative of the  objective function 
  // at the final b value, if necessary.
  if ( pdmatMapObj_b_bOut && !optInfo.getIsTooManyIter() )
  {
    try
    {
      DoubleMatrix* pdmatNull = 0;
      mapObjDiff(
        model,
        dvecY,
        dvecBStep,
        dvecBOutTemp,
        pdmatNull,
        pdmatMapObj_b_bOutTemp,
        withD,
        isFo,
        pdvecN,
        pdvecBMean );
    }
    catch( SpkException& e )
    {
      //
      // Revisit - Sachiko:
      //
      // This should dump all the parameter values to a file and 
      // give the filename as an error message.
      //
      throw e.push(SpkError::SPK_DIFF_ERR, "An attempt to approximate the derivative of mapObj_b with respect to b failed.", __LINE__, __FILE__);
    }
    catch( const std::exception& e )
    {
      //
      // Revisit - Sachiko:
      //
      // This should dump all the parameter values to a file and 
      // give the filename as an error message.
      //
      const int max = SpkError::maxMessageLen();
      char buf[max];
      snprintf( buf, max,  "An attempt to approximate the derivative of mapObj_b with respect to b failed." );
      throw SpkException( e, buf, __LINE__, __FILE__ );
    }
    catch( ... )
    {
      //
      // Revisit - Sachiko:
      //
      // This should dump all the parameter values to a file and 
      // give the filename as an error message.
      //
      throw SpkException(SpkError::SPK_DIFF_ERR, "Unknown exception was thrown during an attempt to \
      approximate the derivative of mapObj_b with respect to b failed.", __LINE__, __FILE__);
    }

  }


  //------------------------------------------------------------
  // Set the values to be returned.
  //------------------------------------------------------------

  // Set the final b value, if necessary.
  if ( pdvecBOut && !optInfo.getIsTooManyIter() )
  {
    *pdvecBOut = dvecBOutTemp;
  }

  // Set the final objective function value, if necessary.
  if ( pdMapObjOut && !optInfo.getIsTooManyIter() ) 
  {
    *pdMapObjOut = dMapObjOutTemp;
  }

  // Set the first derivative of the objective function at the 
  // final b value, if necessary.
  if ( pdrowMapObj_bOut && !optInfo.getIsTooManyIter() ) 
  {
    *pdrowMapObj_bOut = drowMapObj_bOutTemp;
  }

  // Set the second derivative of the objective function at the 
  // final b value, if necessary.
  if ( pdmatMapObj_b_bOut && !optInfo.getIsTooManyIter() ) 
  {
    *pdmatMapObj_b_bOut = dmatMapObj_b_bOutTemp;
  }
}

