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

// Review Goddard 6/15/00: Suggest $italic traceLevel$$ as a more informative 
// name than level.
//
// Response Watrous 6/23/00: Revisit.


// Goddard 6/15/00: In all of the matrix expressions above, the backslash
// characters that are supposed to represent part of a big parenthesis are not
// displayed by the browser, so the expressions come out garbled. Also,
// each math block should be set off by spaces above and below it.
//
// Response Watrous 6/23/00: (1.) Fixed garbled big paranthesis, and
// (2.) Revisit spaces above and below math block.


// Review Goddard 6/15/00: After each instance of "derivative of (anything)", 
// add "with respect to $math%b%$$".
//
// Response Watrous 6/23/00: Ignore, the derivative notation is defined in
// the glossary.


// Review Goddard 6/15/00: Inserter for bool is built in. Simplify
// the following statement from the example, 
//
//      cout << "ok             = " << ( ok ? "True" : "False" ) << endl;
//
// Response Watrous 7/3/00: Apply from now on.

/*

$begin mapOpt$$

$spell
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
  optimizer
  fp
  Ri
  Optimizer optimizer
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
    SpkModel           & /model/,
    const DoubleMatrix & /dvecY/,
    Optimizer          & /optimizer/,
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
    const DoubleMatrix* /pdvecN/ = NULL
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

/optimizer/
/$$
This $xref/Optimizer//Optimizer/$$ class object has three attributes.  
These attributes are parameters of the optimizer used in the individual 
level optimization.  It has other attributes for handling running out of 
maximum iterations and for holding the optimization state information 
that is required by later restart(warm start) run.  

If restart is intended, the member function of the Optimizer object 
setupWarmStart() must be called to set up warm start before this function
is called for the first time.  If warm start has been set up, when this 
function returns, the Optimizer object contains the state information and 
the object's member function getIsTooManyIter() returns true if and only 
if the too-many-iter occurred during the optimization process.  

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

/pN/(null by default)
/$$ 
If $italic isFO$$ is specified as $math%true%$$, $italic pN$$ points to a DoubleMatrix 
object that contains the column vector $math%N%$$.  The $th i$$ element of $math%N%$$
specifies the number of elements of $math%y%$$ that correspond to the $th i$$ individual.
If $italic isFO$$ is specified as $math%false%$$, set $italic N$$ to null.


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

class IndModel : public SpkModel
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
  Optimizer optimizer(epsilon, nMaxIter, level);
  void* pFvalInfo = 0;
  bool withD      = true;
  bool isFO       = false;
  DoubleMatrix * pN = NULL;

  //------------------------------------------------------------
  // Optimize MapObj(b).
  //------------------------------------------------------------

  // try mapOpt(...) which may throw an exception.
  bool ok = true;
  try{
     mapOpt(model,
            dvecY,
            optimizer,
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
            pN
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

#include <cmath>
#include <cassert>
#include "mapOpt.h"
#include "mapObj.h"
#include "mapObjDiff.h"
#include "sqpAnyBox.h"
#include "SpkException.h"

/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

static void evalMapObj( const DoubleMatrix& dvecB, 
                        double* pdMapObjOut, 
                        DoubleMatrix* pdrowMapObj_bOut,
                        const void* pInfo 
                      );


/*------------------------------------------------------------------------
 * Namespace declarations
 *------------------------------------------------------------------------*/

namespace 
{
  //
  // Structure: MapObjInfo
  //
  struct MapObjInfo
  {
    SpkModel* pModel;
    Optimizer optimizer;
    const DoubleMatrix* pdvecY;
    const bool* pbWithD;
    const bool* pbIsFo;
    const DoubleMatrix* pdvecN;
  };
}


// Updated 2-5-01 Alyssa
// fixed for const correctness

/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

//***************************************************************************
// [Revisit - Next SPK Iteration - Individual Level Interface - Mitch]
//
// Consider removing the random effects variance, D, from the list of
// arguments for mapOpt.  The pro's for this are that the model class
// should be able to calculate D, which makes the argument redundant.
// The con's for this are that users working at the individual level
// may not define D in their model class and may want to be able to pass
// it in as an argument. 
//
//
// [ Revisited by Sachiko ]
// Having considered, requiring user to provide a model for D when
// user is only interested in the individual level analysis seems
// wrong.  All D arguments appear in the upper routines (poplulation level)
// have been eliminated.
//
//***************************************************************************
void mapOpt(  SpkModel& model,
              const DoubleMatrix& dvecY,
              Optimizer& optimizer,
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
              const DoubleMatrix* pdvecN
           )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int nBRows = dvecBIn.nr();

  double epsilon  = optimizer.getEpsilon();
  int    nMaxIter = optimizer.getNMaxIter();
  int    level    = optimizer.getLevel();


  //------------------------------------------------------------
  // Prepare the inputs for sqpAnyBox.
  //------------------------------------------------------------

  // Set the information required by mapObj.
  MapObjInfo mapObjInfo;
  mapObjInfo.pModel     = &model;
  mapObjInfo.pdvecY     = &dvecY;
  mapObjInfo.pbWithD    = &withD;
  mapObjInfo.pbIsFo     = &isFo;
  mapObjInfo.pdvecN     = pdvecN;

  // Review Goddard 6/15/00: Am I missing something? I can't see any
  // purpose at all for all of these "Temp" variables. Why not just
  // pass the corresponding arguments from this function down into
  // sqpAnyBox? Without the Temp's, this function is simple and obvious.
  // With them it is complicated, opaque, and error prone.
  //    OK, I see one potential use: IF the lower-level function makes
  // no guarantees about the output value if ok=false, AND IF the
  // calling function DOES guarantee that the output values remain unchanged
  // in that case, then this complexity might sorta kinda almost make sense.
  // But this function makes no such guarantee!
  // And if it did make that guarantee, there are better ways to accomplish
  // it -- although some redesign would be necessary. Herb Sutter
  // discusses this issue at length (in the context of exception safety)
  // in Exceptional C++.
  //
  // Response Watrous 7/3/00: Revisit.  The specifications for this 
  // function state that the output value pointers, e.g., pdMapObjOut, 
  // point to values, e.g., *pdMapObjOut, that will only change if the 
  // return value for this function is true.  So, these temporary objects 
  // are created here and then passed to sqpAnyBox, rather than directly 
  // passing the output value pointers themselves.  I agree that there 
  // are better ways to accomplish this such as using get functions, 
  // e.g., getMapObj(), that would provide the user with the quantities 
  // they want after the optimization has completed.
  //
  // Instantiate a temporary column vector to hold the final b 
  // value that will be returned by sqpAnyBox.
  DoubleMatrix dvecBOutTemp( nBRows, 1 );

  // If this function is going to return the objective function 
  // value, initialize the temporary value to hold it.  Otherwise, 
  // set the temporary pointer to zero so that sqpAnyBox will not 
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
    try{
      sqpAnyBox( evalMapObj, 
                &mapObjInfo, 
                optimizer,
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
        mapObj( model, 
              dvecY, 
              dvecBOutTemp,
              pdMapObjOutTemp,
              pdrowMapObj_bOutTemp,
              withD,
        isFo,
        pdvecN );
      
    }
  }


  //------------------------------------------------------------
  // Compute the second derivative of the objective function.
  //------------------------------------------------------------

  // Compute the second derivative of the  objective function 
  // at the final b value, if necessary.
  if ( pdmatMapObj_b_bOut && !optimizer.getIsTooManyIter() )
  {
      try{
          DoubleMatrix* pdmatNull = 0;
          mapObjDiff( model,
                      dvecY,
                      dvecBStep,
                      dvecBOutTemp,
                      pdmatNull,
                      pdmatMapObj_b_bOutTemp,
                      withD,
                      isFo,
                      pdvecN
                      );
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
          sprintf( buf, "%s\nAn attempt to approximate the derivative of mapObj_b with respect to b failed.",
            e.what() );
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
  if ( pdvecBOut && !optimizer.getIsTooManyIter() )
  {
        *pdvecBOut = dvecBOutTemp;
  }

  // Set the final objective function value, if necessary.
  if ( pdMapObjOut && !optimizer.getIsTooManyIter() ) 
  {
        *pdMapObjOut = dMapObjOutTemp;
  }

  // Set the first derivative of the objective function at the 
  // final b value, if necessary.
  if ( pdrowMapObj_bOut && !optimizer.getIsTooManyIter() ) 
  {
    *pdrowMapObj_bOut = drowMapObj_bOutTemp;
  }

  // Set the second derivative of the objective function at the 
  // final b value, if necessary.
  if ( pdmatMapObj_b_bOut && !optimizer.getIsTooManyIter() ) 
  {
    *pdmatMapObj_b_bOut = dmatMapObj_b_bOutTemp;
  }
}


/*========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 *========================================================================*/

/*************************************************************************
 *
 * Function: evalMapObj
 *
 *
 * Description
 * -----------
 *
 * Calls mapObj in order to evaluate the map Bayesian objective function 
 * and/or its gradient.  If the evaluation fails, an exception object is 
 * passed through the argument by reference.  In this case, all non-const
 * arguments passed by reference will come back unchaged.
 * 
 *
 * Arguments
 * ---------
 *
 * dvecB
 * The column vector b where the objective function should be evaluated.
 *
 * pdMapObjOut
 * If the return value of evalMapObj is true, and if this pointer is not 
 * equal to zero, then on output the double value it points to will be 
 * equal to the map Bayesian objective function  evaluated at b. Note 
 * that the routine calling this function must allocate memory for the 
 * value pointed to by pdMapObjOut.
 *
 * pdrowMapObj_bOut
 * If the return value of evalMapObj is true, and if this pointer is not 
 * equal to zero, then on output the DoubleMatrix it points to will 
 * contain a row vector equal to the derivative of the map Bayesian 
 * objective function with respect to b. Note that the DoubleMatrix 
 * pointed to by pdrowMapObj_bOut must have the same number of elements
 * as dvecB and must be constructed by the routine calling this function.
 * 
 * pInfo
 * The structure pointed to by pInfo contains information required by 
 * mapObj in order to evaluate the objective function.
 *
 *************************************************************************/
static void evalMapObj( const DoubleMatrix& dvecB, 
                        double* pdMapObjOut, 
                        DoubleMatrix* pdrowMapObj_bOut,
                        const void* pInfo 
                      )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Create a pointer to the information required by mapObj.
  MapObjInfo* pMapObjInfo = (MapObjInfo*) pInfo;

  // If there are no output values to calculate, then the 
  // evaluation is considered to have succeeded.

  if ( !pdMapObjOut && !pdrowMapObj_bOut )
  {
    return;
  }

  //------------------------------------------------------------
  // Validate the inputs (debug version only).
  //------------------------------------------------------------

  assert( dvecB.nc() == 1 );

  if ( pdrowMapObj_bOut )
  {
    assert( dvecB.nr() == pdrowMapObj_bOut->nc() );
  }


  //------------------------------------------------------------
  // Evaluate the map Bayesian objective function.
  //------------------------------------------------------------

  mapObj( *( pMapObjInfo->pModel ), 
          *( pMapObjInfo->pdvecY ), 
          dvecB, 
          pdMapObjOut, 
          pdrowMapObj_bOut, 
          *( pMapObjInfo->pbWithD ),
          *( pMapObjInfo->pbIsFo ), 
          pMapObjInfo->pdvecN );
}


