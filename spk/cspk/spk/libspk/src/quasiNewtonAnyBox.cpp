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
    Optimizer& /optimizer/,
    const DoubleMatrix& /dvecXLow/,
    const DoubleMatrix& /dvecXUp/,
    const DoubleMatrix& /dvecXIn/, 
    DoubleMatrix* /pdvecXOut/, 
    double* /pdFOut/, 
    DoubleMatrix* /pF_xOut/
)/$$
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
Uses a quasi-Newton method to solve the problem
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
The given pointers are set to pointing to the results, if the algorithm converged 
successfully.  If failed, a $xref/SpkException//exception/$$ will be
thrown.  The state at which an exception is thrown is defined in
$xref/glossary/Exception Handling Policy/Exception Handling Policy/$$.
In the case of too-many-iterations, the final estimate of the solution is still 
normally returned.  

$head Arguments$$
$syntax/void (* /fval/)( 
    const DoubleMatrix& /dvecX/, 
    double* /pdFOut/, 
    DoubleMatrix* /pdrowGOut/, 
    const void* /pFvalInfo/
)
/$$
The function $italic fval$$ evaluates $math%f(x)%$$ for a particular value 
of the column vector $math%x%$$.  The return value of $italic fval$$ is true,
if it succeeds and false otherwise.  The $code DoubleMatrix$$ $italic dvecX$$ 
contains $math%x%$$, and it has the same dimensions as $italic dvecXIn$$.
It specifies the point at which to evaluate $math%f(x)%$$.  
If the return value of $italic fval$$ is true, and 
if $italic pdFOut$$ is not equal to zero, then on output the $code double$$ 
value pointed to by $italic pdFOut$$ will be equal to $math%f(x)%$$. 
Note that the user must allocate memory for the value pointed 
to by $italic pdFOut$$.
If the return value of $italic fval$$ is true, and 
if $italic pdrowGOut$$ is not equal to zero, then on output the 
$code DoubleMatrix$$ pointed to by $italic pdrowGOut$$ will contain 
a row vector equal to the derivative $math%f_x(x)%$$, which is
also referred to as the gradient.  
Note that the $code DoubleMatrix$$ pointed to by $italic pdrowGOut$$ must 
have the same number of elements as $italic dvecX$$ and must be constructed 
by the user.
All of the elements of the derivative must be calculated by $italic fval$$.
The pointer $italic pFvalInfo$$ is passed to the $italic fval$$ when it 
is called by $code quasiNewtonAnyBox$$.
It allows arbitrary information to be passed to $italic fval$$.

$syntax/

/pFvalInfo/
/$$
This pointer is passed to the function $italic fval$$ when it is called
by $code quasiNewtonAnyBox$$.
It allows arbitrary information to be passed to $italic fval$$.

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

/optimizer/
/$$
This $xref/Optimizer//Optimizer/$$ class object has three attributes.  
These attributes are parameters of the optimizer used in the optimization.
If the number of iterations parameter is equal to zero, then the input
value for x is accepted as the final value, and any requested output
values are evaluated at that final value.
If the throwxEcepIfMaxIter parameter is true, then when
the maximum number of iterations is exhausted, an exception will
be thrown and the output values for this function will not be set.
Otherwise, the calling program will
need to check the parameter isTooManyIter to see if the 
maximum number of iterations was exhausted, 

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

/pF_xOut/
/$$
If the return value for $code quasiNewtonAnyBox$$ is true, i.e., the algorithm
converged successfully, and 
if $italic pF_xOut$$ is not equal to null, then on output the matrix
value pointed to by $italic pF_xOut$$ will be equal to the 
value of the gradient of objective function $math%f(x)%$$ with respect
to the variable at the final iteration.
Note that the user must allocate memory for the value pointed 
to by $italic pF_xOut$$.


$head Example$$
The following example program minimizes the function
$math%
                 N
             1  ----                               2
     f(x) =  -  >       i^2 * [ x(i) + (-1)^i * i ]
             2  ----
                i = 1
%$$
subject to the constraint that 
$math%
     -(N-1) \le x \le +(N-1) .
%$$
In the case below, where $math%N%$$ is $4$$, the minimizer
is the vector $math%(1, -2, 3, -3)%$$.
$pre 

$$
If you compile, link, and run the following program:
$codep


#include <iostream>
#include <iomanip>
#include <cmath>
#include "DoubleMatrix.h"
#include "quasiNewtonAnyBox.h"
#include "Optimizer.h"


//---------------------------------------------
// Function: fourParamQuadratic
//---------------------------------------------

static void fourParamQuadratic( const DoubleMatrix& dvecX, double* pdFOut, 
                DoubleMatrix* pdrowGOut, const void* pFvalInfo )
{
  int nObjPar = 4;
  
  double* pdXData = dvecX.data();
  int nXRows = dvecX.nr();

  int i, j;
  double tot;

  if ( pdFOut ) 
  {
    // Set the objective function value.
    tot = 0.0;
    for ( j = 0; j < nObjPar; j++ )
    {
      i = j + 1;
      tot += pow(i, 2.0) * pow( pdXData[j] + pow( -1.0, i ) * i, 2.0 );
    }
    *pdFOut = 0.5 * tot;
  }

  if ( pdrowGOut )
  {
    double* pdGOutData = pdrowGOut->data();

    // Set the elements of the gradient.
    for ( j = 0; j < nObjPar; j++ )
    {
      i = j + 1;
      pdGOutData[j] =  pow(i, 2.0) * ( pdXData[j] + pow( -1.0, i ) * i );
    }
  }
}

  
//---------------------------------------------
// Function: main
//---------------------------------------------

void main()
{
  using namespace std;

  bool ok;

  int nObjPar = 4;

  DoubleMatrix dvecXLow(nObjPar, 1);
  DoubleMatrix dvecXUp(nObjPar, 1);
  DoubleMatrix dvecXIn(nObjPar, 1);
  DoubleMatrix dvecXOut(nObjPar, 1);

  double* pdXLowData = dvecXLow.data();
  double* pdXUpData  = dvecXUp .data();
  double* pdXInData  = dvecXIn .data();
  double* pdXOutData = dvecXOut.data();

  for ( int i = 0; i < nObjPar; i++ )
  {
    pdXLowData[i] = -(nObjPar - 1.0);
    pdXUpData[i]  = +(nObjPar - 1.0);

    pdXInData[i]  = 0.0;
  }

  double epsilon  = 1.e-5; 
  int nMaxIter    = 50; 
  double fOut     = 0.0;
  DoubleMatrix f_xOut(1, nObjPar);
  int level       = 0;
  Optimizer optimizer( epsilon, nMaxIter, level );

  void* pFvalInfo = 0;

  ok = quasiNewtonAnyBox( fourParamQuadratic, pFvalInfo, optimizer, 
    dvecXLow, dvecXUp, dvecXIn, &dvecXOut, &fOut, &f_xOut );

  cout << setiosflags(ios::scientific) << setprecision(15);

  cout << "ok   =  " << ( ok ? "true" : "false" ) << endl;
  cout << "xOut =  " << endl;
  dvecXOut.print();
  cout << "fOut =  " << fOut << endl;
  cout << "f_xOut= " << f_xOut << endl;
}

$$
then it will display the following when it is run:
$codep

ok   =  true

xOut =
[1.000000093264926e+000]
[-2.000000087050101e+000]
[3.000000000000000e+000]
[-3.000000000000000e+000]

fOut =  8.000000000000020e+000

f_xOut=
[ 1.1368702459435553e-006 -4.1424990371297099e-007 0.0000000000000000e+000 9.6000000000000000e+001 ]

$$
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


/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  void scaleElem(
    int n,
    const double* const px, 
    const double* const pxLow, 
    const double* const pxDiff,
    double* py );

  void unscaleElem(
    int n,
    const double* const py, 
    const double* const pxLow, 
    const double* const pxUp, 
    const double* const pxDiff,
    double* px );

  void scaleGradElem(
    int n,
    const double* const pg, 
    const double* const pxDiff,
    double* pScaledG );

  bool isWithinTol(
    double tol, 
    const DoubleMatrix& dvecXHat, 
    const DoubleMatrix& dvecXLow, 
    const DoubleMatrix& dvecXUp, 
    const DoubleMatrix& drowG,
    const DoubleMatrix& dmatR );

  DoubleMatrix arrayToDoubleMatrix(
    const double* const pdAIn, 
    int nRows, 
    int nCols );

  DoubleMatrix getLowerTriangle( const DoubleMatrix& dmatA );

  bool isLowerTriangular( const DoubleMatrix& dmatA );

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
      QuasiNewtonAnyBoxObj* const  pObjectiveIn;
      const DoubleMatrix* const    pdvecXLowIn,
      const DoubleMatrix* const    pdvecXUpIn,
      const DoubleMatrix* const    pdvecXDiffIn )
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
    // This is not defined so that it won't be used.
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

    const char* function( const double* const yCurr, double& fScaledOut )
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

    const char* gradient( double* gScaledOut );
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
          "An SpkException was thrown during the evaluation of the gradient of the function.",
          __LINE__, 
          __FILE__ );
      }
      catch( const std::exception& stde )
      {
        throw SpkException(
          stde,
          "An standard exception was thrown during the evaluation of the gradient of the function.",
          __LINE__, 
          __FILE__ );
      }  
      catch( ... )
      {
        throw SpkException(
          SpkError::SPK_UNKNOWN_ERR, 
          "An unknown exception was thrown during the evaluation of the gradient of the function.",
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
  DoubleMatrix*          pdrowF_xOut );
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i;

  double epsilon     = optimizer.getEpsilon();
  int    nMaxIter    = optimizer.getNMaxIter();
  int    level       = optimizer.getLevel();
  bool   isWarmStart = optimizer.getIsWarmStart();
  

  //------------------------------------------------------------
  // Validate the inputs (debug mode).
  //------------------------------------------------------------

  assert( epsilon >  0.0 );
  assert( epsilon <= 1.0 );

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
  // Initializations for the scaled objective function.
  //------------------------------------------------------------

  Memory<double> memoryDbl( 3 * nObjPar + nObjPar + nObjPar * nObjPar );

  // The various y vectors are scaled versions of their x counterparts.
  double* yLow  = memoryDbl( nObjPar );
  double* yUp   = memoryDbl( nObjPar );
  double* yCurr = memoryDbl( nObjPar );

  // These are the scaled gradient, gScaled(y) = fScaled_y(y),
  // and scaled Hessian, hScaled(y) = fScaled_y_y(y).
  double* gScaled = memoryDbl( nObjPar );
  double* hScaled = memoryDbl( nObjPar * nObjPar );

  // Check to see if the lower and upper bounds for each element of x are 
  // equal and then set the bounds and the initial value y accordingly.
  //
  // Sachiko: For efficiency, intialize these vectors with default values before the loop.
  yLow.fill(0);
  yUp.fill(0);
  y.fill(0);
  for ( i = 0; i < nObjPar; i++ )
  {
    pdXDiffData[i] = pdXUpData[i] - pdXLowData[i]; 

    if ( pdXDiffData[i] != 0.0 ) 
    {
      // The x bounds are not equal, so constrain this element 
      // to the interval [0,1].
      //yLow[i] = 0.0;
      yUp[i]  = 1.0;
      
      //yCurr[i] = scaleElem( pdXInData[i], pdXLowData[i], pdXDiffData[i] );
    }
  }

  // This function sets 0.0 to the corresponding output element when diff[i] is 0.0.
  scaleElem(nObjPar, pdXInData, pdXLowData, pdXDiffData, yCurr);


  //------------------------------------------------------------
  // Prepare the objective function object.
  //------------------------------------------------------------
  
  QuasiNewton01BoxObj objective( &dvecXLow, &dvecXUp, &dvecXDiff );


??  //***************************************************************************
  // [Revisit - Next SPK Iteration - Improved Diagnostics - Mitch]
  //
  // Currently the tracing done for the NAG optimizer (called by quasiNewtonAnyBox) is
  // pretty crude.  What's there right now is basically a cobbled together
  // version of the tracing done in the O-Matrix version of SPK and the built
  // in tracing for the NAG optimizer.  The result is that the specifications
  // for the level parameter are complicated and hard to understand.
  // 
  // The NAG optimizer allows the user (us) to define our own tracing function
  // that prints whatever we want using whatever format we want to code up.
  // For example, one thing we could do is to unscale the parameters so that
  // they are back in the original space that the user understands.
  // 
  // The NAG optimizer also provides the user-defined print function with
  // derivative checking information.  Another thing we could do in quasiNewtonAnyBox
  // is to analyze the derivative information from the NAG optimizer and
  // provide diagnostics to the user that lets them know which component(s) of
  // the user's derivative are in error.
  // 
  // Therefore, for the next iteration, we could improve the diagnostics for
  // quasiNewtonAnyBox by defining our own tracing function and also our own
  // derivative checking function.  I think that once we do that, then the
  // specifications for the level parameter will be much easier to write.
  //***************************************************************************


  //------------------------------------------------------------
  // Prepare to optimize the scaled objective function.
  //------------------------------------------------------------

  std::ostream    os = std::cout;
  size_t        level;
  size_t       nIterMax;
  size_t      nQuadMax;
  size_t            n;
  double        delta;
  Fun          &objective;
  // Input+Output Arguments
  size_t      &iterCurr;
  size_t     &quadCurr;
  double        &rScaled;
  double        &fScaled;
  double        *yCurr; // length n 
  double        *gScaled; // length n 
  const double  *hScaled; // length n * n 

  const char *msg;
  const size_t              n = 5;
  const size_t        nQuadMax = 20 * n;
  const bool      exponential = true;
  const double          delta = 1e-7;


  // Evaluate the objective and its gradient if this is not a
  // warm start or if zero iterations have been requested.
  if ( !isAWarmRestart || nMaxIter == 0 )
  {
    objective.function( yCurr, fScaled, gScaled, ... );
  }


     // construct function object
     Fun obj(exponential, n, Q, b);

     /*
     Current iterate values
     */
     size_t        ItrCur = 0;
     size_t       QuadCur = 0;
     double          rCur = .5;
     double          fCur;
     /*
     Output values
     */
     double         fOut;

     // initial yCur
     for(i = 0; i < n; i++)
          yCur[i] = .5;

     // fCur is objective function value at yCur
     msg = obj.function(yCur, fCur); 
     ok &= strcmp(msg, "ok") == 0;

     // gCur is gradient at yCur
     msg = obj.gradient(gCur); 
     ok &= strcmp(msg, "ok") == 0;

     // initialize the HCur as the identity matrix
     for(i = 0; i < n; i++)
          for(j = 0; j < n; j++)
               HCur[i * n + j ] = static_cast<double>( i == j );


  if ( isAWarmRestart )
  {
    // This function assumes that the Hessian provided during
    // a warm start is sufficiently accurate that the optimizer
    // does not need extra iterations in order to approximate
    // the Hessian the first time it is called.
    nIterMax = 1;

    // Retrieve the previous state information.
    if ( isWarmStart )
    {

GET THE PROPER WARM START STUFF 
GET THE PROPER WARM START STUFF 
GET THE PROPER WARM START STUFF 
GET THE PROPER WARM START STUFF 
GET THE PROPER WARM START STUFF 
GET THE PROPER WARM START STUFF 
GET THE PROPER WARM START STUFF 

      options.start  = Nag_Warm;
      StateInfo stateInfo = optimizer.getStateInfo();

      for( int j = 0; j < n; j++ )
      {
	yCurr[ j ] = stateInfo.x[ j ];
      }

      options.state  = stateInfo.state;
      options.lambda = stateInfo.lambda;
      options.h      = stateInfo.h;
    }
  }
  else
  {
    // Set the number of quasi-Newton iterations high enough that 
    // the optimizer can build up a reasonably accurate approximation
    // for the Hessian the first time it is called, but not so high
    // that it will perform too many iterations before this function's
    // convergence criterion is checked.
    nIterMax = nObjPar;

    // Create an approximation for the Hessian.
    hScaled = ... ;
  }

  // Set the maximum number of interior point iterations so
  // that the optimizer can solve the quadratic subproblems
  // with sufficient accuracy.
  nQuadMax = 20 * nObjPar;

  // If the return value of QuasiNewton01Box is "ok", then the
  // infinity norm (element with the maximum absolute value) 
  // of the projected gradient is less than or equal to delta.
  double delta;
  double deltaScale = 10.0;

  // Initialize the convergence flag and iteration counter.
  bool isWithinTol;
  int iterCurr;
  if ( nMaxIter > 0 )
  {
    isWithinTol = false;
    iterCurr = 1;
  }
  else
  {
    // If zero iterations have been requested, then the input value
    // for x is accepted as the final value.
    isWithinTol = true;
    iterCurr = 0;
  }


  //------------------------------------------------------------
  // Optimize the scaled objective function.
  //------------------------------------------------------------

  try
  {
    // Attempt to satisfy this function's convergence criterion before
    // the maximum number of iterations have been performed.
    while ( !isWithinTol && iterCurr <= nMaxIter )
    {
      // See if this function's convergence criterion has been met.
      if ( isWithinTol( 
        epsilon,
        yCurr,
        yLow,
        yUp,
        gScaled,
        hScaled ) )
      {
        isWithinTol = true;
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
        msg = QuasiNewton01Box(
          os,
          level,
          nIterMax,
          nQuadMax,
          n,
          delta,
          objective,
          iterCurr,
          quadCurr,
          rScaled,
          fScaled,
          yCurr,
          gScaled,
          hScaled );

        // After the first call to the optimizer the approximation for the
        // Hessian should be accurate enough that this can reset.
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
  std::strstream message;

  optimizer.setNIterCompleted( iterCurr );

  if ( isWithinTol )                // This function's convergence
                                    // criterion was satisfied.
  {
    optimizer.setIsTooManyIter( false );
    ok = true;
  }
  else if ( iterCurr == nMaxIter )  // The maximum number of iterations 
                                    // have been performed.
  {
    optimizer.setIsTooManyIter( true );
    if( optimizer.getThrowExcepIfMaxIter() )
    {
      errorCode = SpkError::SPK_TOO_MANY_ITER;
      message << "Maximum number of iterations performed without convergence.";
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
    message << "Unable to satisfy convergence criterion for quasiNewtonAnyBox.";
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
  
  if( pdrowF_xOut )
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
 * Function: scaleElem
 *
 *
 * Returns the scaled value for x.  
 * Sets 0.0 to the corresponding output element when diff[i] is 0.0. 
 *
 * Note - Sachiko:
 * No possibility for "div by zero" error.
 *
 *************************************************************************/

void scaleElem(
  int n,
  const double* const px, 
  const double* const pxLow, 
  const double* const pxDiff,
  double* py )
{
    for(int i=0; i<n; i++)
    {
      if (pxDiff[i] != 0.0)
        py[i] = ( px[i] - pxLow[i] ) / pxDiff[i];
      else 
        py[i] = 0.0;
    }
}


/*************************************************************************
 * Function: unscaleElem
 *
 *
 * Returns the unscaled value for y.   
 *
 * Note - Sachiko:
 * No possibility for "div by zero" error.
 *
 *************************************************************************/

void unscaleElem(
  int n,
  const double* const py, 
  const double* const pxLow, 
  const double* const pxUp, 
  const double* const pxDiff,
  double* px )
{
    for(int i=0; i<n; i++)
    {
      if ( py[i] != 1.0 ) 
      {
        // Transform the element of the y vector back to the unscaled 
        // form. Note that for those elements of x for which the
        // lower and upper bounds are the same, xDiff is zero, and
        // the x values are equal to their lower bounds.
        px[i] = pxLow[i] + py[i] * pxDiff[i];
      }
      else
      {
        // If y is at the upper bound, calculate the unscaled value
        // in a way that avoids roundoff error.
        px[i] = pxUp[i];
      }
    }
}


/*************************************************************************
 * Function: scaleGradElem
 *
 *
 * Returns the scaled value for g.
 *
 * Note - Sachiko:
 * No possibility for "div by zero" error.
 *
 *************************************************************************/

void scaleGradElem(
  int n,
  const double* const pg, 
  const double* const pxDiff,
  double* pScaledG )
{
    for(int i=0; i<n; i++)
        pScaledG[i] = pxDiff[i] * pg[i];
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
 *     H  deltaX  =  G  ,                              (1)
 *
 * where both the gradient of f(x),
 *
 *     G = f_x(xHat) 
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
 * Tolerance that deltaX must be less than.  tol must be greater than 0.0 and 
 * less than 1.0.   
 *
 *
 * dvecXHat
 *
 * Contains the estimate for the true minimizer xTrue.
 *
 *
 * dvecXLow
 *
 * Contains the lower bound for x.
 *
 *
 * dvecXUp
 *
 * Contains the upper bound for x.
 *
 *
 * drowG
 *
 * Contains the gradient G(x) evaluated at xHat.
 *
 *
 * dmatR
 *
 * Contains the lower triangular Cholesky factor R(x) of the 
 * Hessian H(x) evaluated at xHat.  Note that the existence of R 
 * implies that H is symmetric and positive-definite.  
 *
 *************************************************************************/

#include "multiply.h"

MAKE THIS WORK FOR C ARRAYS?
MAKE THIS WORK FOR C ARRAYS?
MAKE THIS WORK FOR C ARRAYS?
MAKE THIS WORK FOR C ARRAYS?
MAKE THIS WORK FOR C ARRAYS?
MAKE THIS WORK FOR C ARRAYS?
MAKE THIS WORK FOR C ARRAYS?

bool isWithinTol( 
  double tol, 
  const DoubleMatrix& dvecXHat, 
  const DoubleMatrix& dvecXLow, 
  const DoubleMatrix& dvecXUp, 
  const DoubleMatrix& drowG,
  const DoubleMatrix& dmatR )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  int i, j, k;

  int n = dvecXHat.nr();

  assert( tol > 0.0 && tol < 1.0 );

  assert( dvecXHat.nr() == n && dvecXHat.nc() == 1 );
  assert( dvecXLow.nr() == n && dvecXLow.nc() == 1 );
  assert( dvecXUp .nr() == n && dvecXUp .nc() == 1 );
  assert( drowG   .nr() == 1 && drowG   .nc() == n );
  assert( dmatR   .nr() == n && dmatR   .nc() == n );

  assert( isLowerTriangular( dmatR ) );

  DoubleMatrix dvecDeltaX( n, 1 );
  DoubleMatrix drowGTemp( 1, n );


  const double* pdXHatData   = dvecXHat  .data();
  const double* pdXLowData   = dvecXLow  .data();
  const double* pdXUpData    = dvecXUp   .data();
  const double* pdGData      = drowG     .data();
  const double* pdRData      = dmatR     .data();
  double* pdDeltaXData = dvecDeltaX.data();
  double* pdGTempData  = drowGTemp .data();

  // This is not necessary.  Sachiko
  //drowGTemp.fill(0.0);

  //------------------------------------------------------------
  // Calculate the Hessian, temporary gradient, and diagonal reciprocals.
  //------------------------------------------------------------

  // Prepare a version of the Hessian matrix H = R * R^(T) with its 
  // super-diagonal elements replaced by the sub-diagonal elements
  // of its (lower triangular) Cholesky factor R.
  DoubleMatrix dmatH(n,n);
  double* pdHData = dmatH.data();
  for ( i = 0; i < n; i++ )
  {
    // Compute the elements in this row of the lower triangle of H.
    for ( j = 0; j <= i ; j++ )
    {
      pdHData[i + j * n] = 0.0;

      // This loop does not include k > j since j <= i and R is lower
      // triangular, i.e., its (j,k)th element is zero for k > j.
      for ( k = 0; k <= j; k++ )
      {
        pdHData[i + j * n] +=  pdRData[i + k * n] * pdRData[j + k * n];
      }
    }
  
    // Copy the elements in this row of the super-diagonal of H.
    for ( j = i + 1; j < n; j++ )
    {
      pdHData[i + j * n] = pdRData[j + i * n];
    }
  }

  // Instantiate a column vector p that will contain the 
  // reciprocals of the diagonal elements of R.
  DoubleMatrix dvecP( n, 1 );
  double* pdPData = dvecP.data();

  // Each of these flags will be true if the corresponding 
  // xHat element will be included in the tolerance calculation 
  // at the end of this function.
  std::vector<bool> isElemIncluded( n );

  // Modify the Hessian, and set the elements of GTemp and p.
  for ( i = 0; i < n; i++ )
  {
      //----------------------------------------------------------
      // Compute the corresponding elements of H, GTemp, and P.
      //----------------------------------------------------------
      if( (pdXHatData[i] >  pdXLowData[i] && pdXHatData[i] < pdXUpData[i])
          || (pdXHatData[i] == pdXLowData[i] && pdGData[i] < 0.0 )  
          || (pdXHatData[i] == pdXUpData[i]  && pdGData[i] > 0.0 ) )
      {
            isElemIncluded[i] = true;

            //--------------------------------------------------------
            // This element will be included:  its deltaX should be computed.
            //--------------------------------------------------------

            // Set the corresponding element of the temporary gradient.
            pdGTempData[i] = pdGData[i];

            // Set the reciprocal of the corresponding R diagonal.
            assert( pdRData[i + i * n] != 0.0 );
            pdPData[i] = 1.0 / pdRData[i + i * n];
      }
      else
      {
            isElemIncluded[i] = false;

            //--------------------------------------------------------
            // This element won't be included:  force its deltaX to be zero.
            //--------------------------------------------------------

            // 
            // Review - Sachiko: suggestion for readability.
            //
            // The following indented block can be reduced to a single statement:
            // 
            //      pdHData[i + i * n] = 1.0;
            //
            // if H, GTemp and P were initialized to zero or one outside of the outer loop.
            // 

                // Zero the elements from the corresponding row and column of 
                // H that come before the diagonal element.
                for ( j = 0; j < i; j++ )
                {
                  pdHData[i + j * n] = 0.0;
                  pdHData[j + i * n] = 0.0;
                }
      
                // Set the correponding diagonal element of H equal to one.
                pdHData[i + i * n] = 1.0;
      
                // Zero the elements from the corresponding row and column of 
                // H that come after the diagonal element.
                for ( j = i + 1; j < n; j++ )
                {
                  pdHData[i + j * n] = 0.0;
                  pdHData[j + i * n] = 0.0;
                }
      
                // Zero the corresponding element of the temporary gradient.
                pdGTempData[i] = 0.0;

                // Set the reciprocal of the corresponding R diagonal equal to one.
                pdPData[i] = 1.0;
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
  // *     B  =  GTemp .
  // * 
  // * Note that 
  // *               T
  // *     A  =  L  L  ,
  // *
  // * and
  // *               T
  // *     H  =  R  R  ,
  // *
  // * but 
  // *            T
  // *     L  =  R  .
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
  // Note: NAG functions expect the elements of a[n][tda] to be 
  // in row-major order while the elements of the DoubleMatrix 
  // dmatH are stored in column-major order.  This is the reason  
  // the sub-diagonal elements of dmatR were copied to the 
  // super-diagonal elements of dmatH.
  double* a = pdHData;

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
  double* p = pdPData;

  // Parameter: b[n][tdb].
  // Input:the n by r right-hand side matrix B.  
  // Output: unspecified.
  double* b = pdGTempData;

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
  double* x = pdDeltaXData;

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
  // Check to see if deltaX is within tolerance.
  //------------------------------------------------------------

  //
  // Review - Sachiko: efficiency
  // 
  // There seems no real need for "isWithinTol" boolean variable.
  //

  bool isWithinTol = true;

  for ( i = 0; i < n; i++ )
  {
    // Only check the elements of xHat that were determined
    // earlier to be necessary for this tolerance calculation.
    if ( isElemIncluded[i] )
    {
      if ( fabs( pdDeltaXData[i] ) > tol )
      {
        isWithinTol = false;

        // Get out of the routine as soon as disprove.
        return isWithinTol;
      }
    }
  }

  return isWithinTol;
}


/*************************************************************************
 * Function: arrayToDoubleMatrix
 *
 *
 * Copies the elements of an array of double values to a DoubleMatrix
 * object.  This function assumes that the elements of the array are
 * stored in column-major order.
 *
 *************************************************************************/

DoubleMatrix arrayToDoubleMatrix( 
  const double* const pdAIn, 
  int nRows, 
  int nCols )
{
  DoubleMatrix dmatAOut( nRows, nCols );

  double* pdAOut = dmatAOut.data();
  
  std::copy( pdAIn, pdAIn+( nRows * nCols ), pdAOut );
  
  return dmatAOut; 
}


/*************************************************************************
 * Function: getLowerTriangle
 *
 *
 * Returns a matrix B that contains a copy of the square matrix A 
 * with all of its elements above the diagonal set equal to zero.
 *
 *************************************************************************/

DoubleMatrix getLowerTriangle( const DoubleMatrix& dmatA ) 
{
  DoubleMatrix dmatB( dmatA );

  double* pdBData = dmatB.data();

  int n = dmatA.nr();

  // Zero the elements of B that are above the diagonal.
  int i, j;
  for ( i = 0; i < n - 1; i++ ) 
  {
    for ( j = i + 1; j < n; j++ )
    {
      pdBData[i + j*n] = 0.0;
    }
  }

  return dmatB;
}



/*************************************************************************
 * Function: isLowerTriangular
 *
 *
 * Returns true if the square matrix A is lower triangular, i.e., if
 * all of its elements above the diagonal are zero.
 *
 *************************************************************************/

bool isLowerTriangular( const DoubleMatrix& dmatA )
{
  int n = dmatA.nr();

  assert( dmatA.nc() == n );   // A must be square.

  const double* pdAData = dmatA.data();

  int i, j;
  for ( i = 0; i < n - 1; i++ ) 
  {
    for ( j = i + 1; j < n; j++ )
    {
      if ( pdAData[i + j*n] != 0.0 )
      {
          return false;
      }
    }
  }

  return true;
}


} // [End: unnamed namespace]
