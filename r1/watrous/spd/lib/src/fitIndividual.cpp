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
 * File: fitIndividual.cpp
 *
 *
 * Minimizes the map Bayesian objective function.
 *
 * Author: Jiaji Du
 *
 * Later modified by Sachiko
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: fitIndividual
 *
 * [ Implementation Note --- Sachiko, 10/10/2002 ]
 * This function serves as a user entry point.
 * The only user entry point is allowed and responsible for
 * validating user input and initiating the universal floating point error
 * checker.
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin fitIndividual$$

$spell
	Model model
  SPK_VA
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
  optimizer
  fp
  Ri
  valarray
  resize
$$

$section Fitting Individual Parameter$$

$cindex map bayesian optimization$$
$index fitIndividual$$
$index individual, map bayesian optimization$$

$table
$bold Header:$$ $cend
fitIndividual.h $rend

$bold Prototype:$$ $cend
$syntax/void fitIndividual(  
                SpkModel& /indModel/,
                const SPK_VA::valarray<double>& /measurements/,
                Optimizer&                      /indOptimizer/,
                const SPK_VA::valarray<double>& /indParLow/,
                const SPK_VA::valarray<double>& /indParUp/,
                const SPK_VA::valarray<double>& /indParIn/,
                const SPK_VA::valarray<double>& /indParStep/,
                SPK_VA::valarray<double>*       /indParOut/,
                double*                         /indObjOut/,
                SPK_VA::valarray<double>*       /indObj_indParOut/,
                SPK_VA::valarray<double>*       /indObj_indPar_indParOut/,
                bool /withD/ = true
               )
/$$

$tend

See also: $xref/SpkModel//SpkModel/$$, $xref/SpkValarray//SPK_VA::valarray/$$,
$xref/Optimizer//Optimizer/$$.

$fend 25$$

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

                1 %          %            1  T  -1
              + - \logdet[ 2 \pi D ]    + - b  D   b  .
                2 %          %            2
%$$
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
Upon the entry, the $bold universal$$ floating-point error flag set is 
cleared.  Hence, any pending floating-point errors will be lost.
The universal floating-point error detection bits are set to
detect a certain set of errors (for details, see $xref/FpErrorChecker//FpErrorChecker/$$).
The detection bits are restored upon the exit.

If an error is detected or failure occurs during the evaluation, a SpkException object is
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.

$head Note$$

The length of the individual parameter vector $math%b%$$ is 
equal to the length of $italic indParIn$$, 
which is a vector containing the initial value for $math%b%$$.

$head Arguments$$
$syntax/
/indModel/
/$$
This function expects $italic indModel$$ to be a function of
all three parameters: $math%alp%$$, $math%b%$$ and $math%i%$$.
Refer $xref/glossary/Model Functions Depend on only b/Model Functions Depend 
on only b/$$ for details.

$syntax/

/measurements/
/$$
The $code SPK_VA::valarray<double>$$ $italic measurements$$ contains the array 
$math%y%$$, which specifies the measured data.

$syntax/

/indOptimizer/
/$$
This $xref/Optimizer//Optimizer/$$ class object has attributes
that describe the individual level optimizer's behavior.
One of them specifies $math%epsilon%$$ which is used during the optimization
procedures in order to determine if acceptable estimates have 
been found for $math%bTilde_i%$$, and $math%bHat_i%$$.
It has other attributes for handling running out of 
maximum iterations and for holding the optimization state information 
that is required by later restart(warm start) run.  

If restart is intended, the member function of the Optimizer object 
setupWarmStart() must be called to set up warm start before this function
is called for the first time.  If warm start has been set up, when this 
function returns, the Optimizer object contains the state information and 
the object's member function getIsTooManyIter() returns true if and only if 
the too-many-iter occurred during the optimization process.  
$pre

$$
For a particular value of $math%alp%$$ and for the $math%i%$$-th 
individual in the population, an individual parameter vector 
$math%b_i%$$ is accepted as an approximation for $math%bHat_i%$$ if
$math%
	abs(b_i - bHat_i) \le epsilon (bUp - bLow) ,
%$$
where $math%abs%$$ is the element-by-element absolute value function
and $math%bHat_i%$$ is the true minimizer of $math%Lambda_i(alp, b)%$$
with respect to $math%b%$$.  
Since $math%bHat_i%$$ is unknown, the optimization algorithm must 
estimate the left hand side of this inequality.
Note that if $math%nMaxIter%$$ is zero, then 
the $th i$$ column of $math%bIn%$$ is 
accepted as the estimate for $math%bHat_i%$$.
$pre

$$
For a particular value of $math%alp%$$ and for the $math%i%$$-th 
individual in the population, a random population parameter vector 
$math%b_i%$$ is accepted as an approximation for $math%bTilde_i%$$ if
$math%
	abs(b_i - bTilde_i) \le epsilon (bUp - bLow) ,
%$$
where $math%abs%$$ is the element-by-element absolute value function 
and $math%bTilde_i%$$ is the point where the approximate projected 
gradient of $math%Lambda_i(alp, b)%$$ with respect to $math%b%$$ 
is zero.
Since $math%bTilde_i%$$ is unknown, the optimization algorithm must 
estimate the left hand side of this inequality.
Note that if $math%nMaxIter(1)%$$ is zero, then 
the $th i$$ column of $math%bIn%$$ is 
accepted as the estimate for $math%bTilde_i%$$.
$syntax/

/indParLow/
/$$
The $code SPK_VA::valarray<double>$$ $italic indParLow$$ contains the array 
$math%bLow%$$, which specifies the lower limit for the parameter vector 
$math%b%$$ during the optimization procedure.  The size of $italic indParLow$$ 
is equal to the length of the individual parameter vector $math%b%$$.

$syntax/

/indParUp/
/$$
The $code SPK_VA::valarray<double>$$ $italic indParUp$$ contains the vector 
$math%bUp%$$, which specifies the upper limit for the parameter vector 
$math%b%$$ during the optimization procedure.  The size of $italic indParUp$$ 
is equal to the length of the individual parameter vector $math%b%$$.

$syntax/

/indParIn/
/$$
The $code SPK_VA::valarray<double>$$ $italic indParIn$$ contains the vector 
$math%bIn%$$, which specifies the initial value for the parameter vector 
$math%b%$$.
The $xref/glossary/Ordering Of Vectors/order condition/$$,
$math%bLow \le bIn \le bUp%$$, is assumed to hold.
Note that the size of $italic indParIn$$ is equal to the length of 
the individual parameter vector $math%b%$$.

$syntax/

/indParStep/
/$$
The $code SPK_VA::valarray<double>$$ $italic indParStep$$ contains the vector 
$math%bStep%$$, which specifies the step size used for approximating
the derivative of $math%MapObj_b(b)%$$.  This argument is not used in the 
function if the argument $italic indObj_indPar_indParOut$$ is set to $code NULL$$.
The size of $italic indParStep$$ is equal to the length of the individual 
parameter vector $math%b%$$.

$syntax/

* /indParOut/
/$$
If $italic indParOut$$ is not $code NULL$$, then the $code SPK_VA::valarray<double>$$ 
object pointed to by $italic indParOut$$ must be declared in the function that 
calls this function, and it must have the same size as $italic indParIn$$.
If $italic indParOut$$ is not $code NULL$$ and this function completed the optimization 
successfully, then the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic indParOut$$ will contain the vector $math%bOut%$$ that is the estimate 
for the true minimizer of the objective function.  Otherwise, this function will 
not attempt to change the contents of the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic indParOut$$.

$syntax/

* /indObjOut/
/$$
If $italic indObjOut$$ is not $code NULL$$, then the $code double$$ value pointed 
to by $italic indObjOut$$ must be declared in the function that calls this function.
If $italic indObjOut$$ is not $code NULL$$ and this function completed the optimization 
successfully, then the $code double$$ value pointed to by $italic indObjOut$$ will 
be equal to $math%MapObj(bOut)%$$, which is the value of the objective function 
evaluated at $math%bOut%$$.  Otherwise, this function will not attempt to change 
the contents of the $code double$$ value pointed to by $italic indObjOut$$.

$syntax/

* /indObj_indParOut/
/$$
If $italic indObj_indParOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic indObj_indParOut$$ 
must be declared in the function that calls this function, and its size
must be equal to the length of the individual parameter vector $math%b%$$.
If $italic indObj_bOut$$ is not $code NULL$$ and this function completed the optimization 
successfully, then the $code SPK_VA::valarray<double>$$ object pointed to by $italic indObj_indParOut$$ 
will contain the vector $math%indObj_indPar(bOut)%$$ that is the derivative of the 
objective function with respect to the individual parameter $math%b%$$
evaluated at $math%bOut%$$.  Otherwise, this function will not attempt 
to change the contents of the $code SPK_VA::valarray<double>$$ object pointed to 
by $italic indObj_indParOut$$.

$syntax/

* /indObj_indPar_indParOut/
/$$
If $italic indObj_indPar_indParOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic indObj_indPar_indParOut$$ 
must be declared in the function that calls this function, and its size 
must equal to the square of the length of the individual parameter vector $math%b%$$.
If $italic indObj_indPar_indParOut$$ is not $code NULL$$ and this function completed the 
optimization successfully, then the $code SPK_VA::valarray<double>$$ object pointed to by 
$italic indObj_indPar_indPar$$ will contain the matrix $math%MapObj_b_b(bOut)%$$, 
which is an approximation for the second derivative of the objective function with 
respect to with respect to the individual parameter $math%b%$$ evaluated at 
$math%bOut%$$.  Otherwise, this function will not attempt to change the contents of 
the $code SPK_VA::valarray<double>$$ object pointed to by $italic indObj_indPar_indParOut$$.
The approximation for the second derivative is obtained using central differences of 
the function $math%MapObj_b(b)%$$ with step sizes specified by $italic indParStep$$.

$syntax/

/withD/ (optional)
/$$
When this flag is set to $math%false%$$, the terms involving the
matrix $math%D%$$ are dropped from the map Bayesian objective 
function $math%MapObj(b)%$$.  
The default value for $italic withD$$ is $math%true%$$.


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

#include "fitIndividual.h"
#include "SpkModel.h"
#include "DoubleMatrix.h"
#include "pi.h"
#include "Optimizer.h"
#include "SpkValarray.h"

#include <iostream>
#include <iomanip>
#include <string>
#include <cmath>
#include <cstdlib>

using namespace std;

static SPK_VA::valarray<double> funF(  const SPK_VA::valarray<double> &dvecB );
static SPK_VA::valarray<double> funF_b(const SPK_VA::valarray<double> &dvecFb, 
                                       const SPK_VA::valarray<double> &dvecB );
static SPK_VA::valarray<double> funR(  const SPK_VA::valarray<double> &dvecB );
static SPK_VA::valarray<double> funR_b(const SPK_VA::valarray<double> &dmatRb, 
                                       const SPK_VA::valarray<double> &dvecB );

class IndModel : public SpkModel
{
    SPK_VA::valarray<double> _b;
public:
    IndModel(){}
    ~IndModel(){}
protected:
    void doSetIndPar(const SPK_VA::valarray<double>& b)
    {
        _b = b;
    }
    void doDataMean( SPK_VA::valarray<double>& ret ) const
    {
        ret = funF(_b);
    }
    bool doDataMean_indPar( SPK_VA::valarray<double>& ret ) const
    {
        doDataMean(ret);
        ret = funF_b(ret, _b);
        return !( ret.max() == 0 && ret.min() == 0 );
    }
    void doDataVariance( SPK_VA::valarray<double>& ret ) const
    {
        ret = funR(_b);
    }
    void doDataVariance_indPar( SPK_VA::valarray<double>& ret ) const
    {
        doDataVariance(ret);
        ret = funR_b(ret, _b);
        return !( ret.max() == 0 && ret.min() == 0 );
    }
    void doIndParVariance( SPK_VA::valarray<double>& ret ) const
    {
      ret.resize( 4 );
      ret[0] = 1.0;
      ret[1] = 0.0;
      ret[2] = 0.0;
      ret[3] = 0.5;
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

  IndModel indModel;


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  int nY = 2;
  SPK_VA::valarray<double> measurements( 2., nY );


  //------------------------------------------------------------
  // Quantities related to the objective function parameter, b.
  //------------------------------------------------------------

  int nB = 2;

  SPK_VA::valarray<double> indParLow (  -4., nB );
  SPK_VA::valarray<double> indParUp  (   4., nB );
  SPK_VA::valarray<double> indParIn  (   2., nB );
  SPK_VA::valarray<double> indParStep( .001, nB );
  SPK_VA::valarray<double> indParOut (       nB );


  //------------------------------------------------------------
  // Quantities related to the objective function, MapObj(b).
  //------------------------------------------------------------

  double indObjOut;

  SPK_VA::valarray<double> indObj_indParOut  ( nB );
  SPK_VA::valarray<double> indObj_indPar_indParOut( nB * nB );


  //------------------------------------------------------------
  // Remaining inputs to mapOpt.
  //------------------------------------------------------------

  Optimizer indOptimizer( 1.e-3, 40, 0 );


  //------------------------------------------------------------
  // Optimize MapObj(b).
  //------------------------------------------------------------

  bool ok = true;
  try
  {
    fitIndividual( 
                    indModel,
                    measurements,
                    indOptimizer,
                    indParLow,
                    indParUp,
                    indParIn,
                    indParStep,
                    indParOut,
                    indObjOut,
                    indObj_indParOut,
                    indObj_indPar_indParOut,
                    true
                 )
  }
  catch( ... )
  {
    cerr << "fitIndividual failed" << endl;
    abort();
  }

  //------------------------------------------------------------
  // Print the results.
  //------------------------------------------------------------

  double indObjKnown = 2.0 * log( 2.0 * PI ) - 0.5 * log( 2.0 ) + 2.0;

  cout << setiosflags(ios::scientific) << setprecision(15);

  cout << "ok             = " << ( ok ? "True" : "False" ) << endl;

  cout << "indParOut           = " << endl;
  DoubleMatrix BOut( indParOut );
  BOut.print();
  cout << "indObjOut      = " << indObjOut << endl;
  cout << "indObjKnown    = " << indObjKnown << endl;
  cout << "indObj_indParOut    = " << endl;
  DoubleMatrix MapObj_bOut( indObj_indParOut, nB );
  MapObj_bOut.print();
  cout << "indObj_indPar_indParOut  = " << endl;
  DoubleMatrix MapObj_b_bOut( indObj_indPar_indParOut, nB );
  MapObj_b_bOut.print();

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

static SPK_VA::valarray<double> funR( const SPK_VA::valarray<double> &dvecB )
{
  SPK_VA::valarray<double> dmatR( 4 );

  dmatR[0] = exp( dvecB[0] );
  dmatR[1] = 0.0;
  dmatR[2] = 0.0;
  dmatR[3] = exp( dvecB[0] );
  
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

static SPK_VA::valarray<double> funR_b( const SPK_VA::valarray<double> &dmatR, 
                                const SPK_VA::valarray<double> &dvecB )
{
  SPK_VA::valarray<double> dmatR_b( 0., dmatR.size() * dvecB.size() );

  dmatR_b[0] = exp( dvecB[0] );
  dmatR_b[3] = exp( dvecB[0] );
  
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

static SPK_VA::valarray<double> funF( const SPK_VA::valarray<double> &dvecB )
{
  SPK_VA::valarray<double> dvecF( 2 );

  dvecF[0] = dvecB[1];
  dvecF[1] = dvecB[1];

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

static SPK_VA::valarray<double> funF_b( const SPK_VA::valarray<double> &dvecF, 
                                const SPK_VA::valarray<double>
  SPK_VA::valarray<double> dmatF_b( 4 );

  dmatF_b[0] = 0.0;
  dmatF_b[1] = 0.0;
  dmatF_b[2] = 1.0;
  dmatF_b[3] = 1.0;
  
  return dmatF_b;
}
  
$$
then it will display the following when it is run:
$codep

ok             = True
indParOut           =
[ 2.260754053295955e-006 ]
[ 9.999994638370806e-001 ]
indObjOut      = 5.329180542541980e+000
indObjKnown    = 5.329180542538718e+000
indObj_indParOut    =
[ 3.449181849135563e-006 2.376853741825613e-006 ]
indObj_indPar_indParOut  =
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
 * Include file
 *------------------------------------------------------------------------*/
#include "SpkValarray.h"
#include "fitIndividual.h"
#include "mapOpt.h"
#include "FpErrorChecker.h"

using SPK_VA::valarray;

void fitIndividual( 
                    SpkModel&               indModel,
                    const valarray<double>& measurements,
                    Optimizer&              indOptimizer,
                    const valarray<double>& indParLow,
                    const valarray<double>& indParUp,
                    const valarray<double>& indParIn,
                    const valarray<double>& indParStep,
                    valarray<double>*       indParOut,
                    double*                 indObjOut,
                    valarray<double>*       indObj_indParOut,
                    valarray<double>*       indObj_indPar_indParOut,
                    bool                    withD
                  )
{
  //
  // Early return.
  // If user requests no output, get out of here!
  //
  if( indParOut == NULL && indObjOut == NULL && indObj_indParOut == NULL && indObj_indPar_indParOut == NULL )
    return;

  //
  // Turn on the universal floating point error checker.
  //
  FpErrorChecker checkerON;

  const int nYi = measurements.size();
  const int nB  = indParIn.size();

  //------------------------------------------------------------
  // Validate the inputs (debug version only).
  //------------------------------------------------------------

  if( indOptimizer.getEpsilon() <=  0.0 || indOptimizer.getEpsilon() > 1.0 )
  {
      throw SpkException(SpkError::SPK_USER_INPUT_ERR, "0.0 < epsilon <= 1.0", __LINE__, __FILE__); 
  }
  if( indOptimizer.getNMaxIter() < 0 )
  {
      throw SpkException(SpkError::SPK_USER_INPUT_ERR, "maxItr >= 0", __LINE__, __FILE__); 
  }
  if( indOptimizer.getLevel() < 0 )
  {
      throw SpkException(SpkError::SPK_USER_INPUT_ERR, "level >= 0", __LINE__, __FILE__); 
  }

  // Further more these are all assumed to have the same number of rows as b.
  if( indParLow.size() != nB  )
  {
      throw SpkException(SpkError::SPK_USER_INPUT_ERR, "The length of indParLow must be equal to the length of indParIn.", __LINE__, __FILE__); 
  }
  if( indParUp.size() != nB )
  {
      throw SpkException(SpkError::SPK_USER_INPUT_ERR, "The length of indParUp must be be equal to the length of indParIn.", __LINE__, __FILE__); 
  }

  if( indParIn.size() != nB )
  {
      throw SpkException(SpkError::SPK_USER_INPUT_ERR, "The length of indParIn must be be equal to the length of indParIn.", __LINE__, __FILE__); 
  }
  if( indParStep.size() != nB )
  {
      throw SpkException(SpkError::SPK_USER_INPUT_ERR, "The length of indParStep must be be equal to the length of indParIn.", __LINE__, __FILE__); 
  }


  // Review Goddard 6/15/00 Suggest: Put the following block in a utility
  // function and call it from an assert. That function would be general
  // enough to be useful elsewhere too, wherever bounds are checked.
  //
  // Response Watrous 7/3/00: Apply from now on.
  //
  //
  // Response Sachiko 10/22/01: 
  // There are versions of isGreaterThanOrEqualTo() and 
  // isLessThanOrEqualTo() that compare two matrices element-by-element.  
  // They could have been used here.
  //

  //
  // Verify that the initial b value is between the lower and upper bounds.
  //
  for ( int i = 0; i < nB; i++ )
  {
    if( indParIn[i] < indParLow[i] || indParIn[i] > indParUp[i] )
    {
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, "indParLow[i] <= indParIn[i] <= indParUp[i].", __LINE__, __FILE__ );  
    }
  }

  // This is assumed to be a column vector.
  if ( indParOut )
  {
    if( indParOut->size() != nB )
    {
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, 
          "indParOut must be preallocated and has the same length as indParIn.", 
          __LINE__, __FILE__ );  
    }
  }

  // This is assumed to be a row vector.
  if ( indObj_indParOut )
  {
      if( indObj_indParOut->size() != nB )
      {
        throw SpkException(SpkError::SPK_USER_INPUT_ERR, 
          "indObj_indParOut must be preallocated and its length must be equal to the length of indParIn", 
          __LINE__, __FILE__ );  
      }
  }

  // This is assumed to have the same number of rows and columns as b.
  if ( indObj_indPar_indParOut )
  {
      if( indObj_indPar_indParOut->size() != nB * nB )
      {
        throw SpkException(SpkError::SPK_USER_INPUT_ERR,
          "indObj_indPar_indParOut must be preallocated and its length must be equal to the square of the length of indParIn.", 
          __LINE__, __FILE__);  
      }
  }

  // Convert input parameters to DoubleMatrix
    DoubleMatrix dvecY( measurements );
    DoubleMatrix dvecBLow( indParLow );
    DoubleMatrix dvecBUp( indParUp );
    DoubleMatrix dvecBIn( indParIn );
    DoubleMatrix dvecBStep( indParStep );
    
    // Convert output parameters to DoubleMatrix
    DoubleMatrix dvecBOut, drowMapObj_bOut, dmatMapObj_b_bOut;
    DoubleMatrix* pdvecBOut = 0;
    DoubleMatrix* pdrowMapObj_bOut = 0;
    DoubleMatrix* pdmatMapObj_b_bOut = 0;
    
    if( indParOut )
    {
        dvecBOut.resize( nB, 1 );
        pdvecBOut = &dvecBOut;
    }
    if( indObj_indParOut )
    {
        drowMapObj_bOut.resize( 1, nB );
        pdrowMapObj_bOut = &drowMapObj_bOut;
    }
    if( indObj_indPar_indParOut )
    {
        dmatMapObj_b_bOut.resize( nB, nB );
        pdmatMapObj_b_bOut = &dmatMapObj_b_bOut;
    }

    assert( dvecY.nc() == 1 );

    assert( dvecBLow.nr() == nB );
    assert( dvecBLow.nc() == 1 );

    assert( dvecBUp.nr() == nB );
    assert( dvecBUp.nc() == 1 );

    assert( dvecBIn.nc() == 1 );
    
    assert( dvecBStep.nr() == nB );
    assert( dvecBStep.nc() == 1 );


    // Call mapOpt
    mapOpt( 
            indModel,
            dvecY,
            indOptimizer,
            dvecBLow,
            dvecBUp,
            dvecBIn,
            pdvecBOut,
            dvecBStep,
            indObjOut,
            pdrowMapObj_bOut,
            pdmatMapObj_b_bOut,
            withD
          );

    // Convert results to valarray
    if( indParOut )
        (*pdvecBOut).toValarray( (*indParOut) );
    if( indObj_indParOut )
        (*pdrowMapObj_bOut).toValarray( (*indObj_indParOut) );
    if( indObj_indPar_indParOut )
        (*pdmatMapObj_b_bOut).toValarray( (*indObj_indPar_indParOut) );
}
