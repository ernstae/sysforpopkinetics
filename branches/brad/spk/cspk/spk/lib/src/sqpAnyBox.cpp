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
 * File: sqpAnyBox.cpp
 *
 *
 * Minimizes an arbitrary smooth function subject to  simple bounds on
 * the variables using a sequential quadratic programming (SQP) method. 
 *
 * Author: Mitch Watrous
 * Follow-up: Sachiko Honda for speed up
 *
 * Reviewed by: Sachiko Honda 11/15/2001
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: sqpAnyBox
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin sqpanybox$$
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
  sqp
  Sqp
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

$section Sequential Quadratic Programming Optimization with Derivatives and Box Constraints$$

$index sqpAnyBox$$
$cindex sequential quadratic programming optimization 
  \with derivatives \and box constraints$$

$table
$bold Prototype:$$   $cend  
$syntax/void sqpAnyBox(  
    void (* /fval/)( const DoubleMatrix&, double*, DoubleMatrix*, const void* )
    const void* /pFvalInfo/, 
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
Uses a sequential quadratic programming (SQP) method to solve the problem
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
is called by $code sqpAnyBox$$.
It allows arbitrary information to be passed to $italic fval$$.

$syntax/

/pFvalInfo/
/$$
This pointer is passed to the function $italic fval$$ when it is called
by $code sqpAnyBox$$.
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
If the return value for $code sqpAnyBox$$ is true, and 
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

$syntax/

/pdFOut/
/$$
If the return value for $code sqpAnyBox$$ is true, i.e., the algorithm
converged successfully, and 
if $italic pdFOut$$ is not equal to null, then on output the $code 
double$$ value pointed to by $italic pdFOut$$ will be equal to the 
value of the objective function $math%f(x)%$$ at the final iteration.
Note that the user must allocate memory for the value pointed 
to by $italic pdFOut$$.

$syntax/

/pF_xOut/
/$$
If the return value for $code sqpAnyBox$$ is true, i.e., the algorithm
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
#include "sqpAnyBox.h"
#include "Optimizer.h"


//---------------------------------------------
// Function: fourParamQuadratic
//---------------------------------------------

static void fourParamQuadratic( const DoubleMatrix& dvecX, double* pdFOut, 
                DoubleMatrix* pdrowGOut, const void* pFvalInfo )
{
  int nObjPars = 4;
  
  double* pdXData = dvecX.data();
  int nXRows = dvecX.nr();

  int i, j;
  double tot;

  if ( pdFOut ) 
  {
    // Set the objective function value.
    tot = 0.0;
    for ( j = 0; j < nObjPars; j++ )
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
    for ( j = 0; j < nObjPars; j++ )
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

  int nObjPars = 4;

  DoubleMatrix dvecXLow(nObjPars, 1);
  DoubleMatrix dvecXUp(nObjPars, 1);
  DoubleMatrix dvecXIn(nObjPars, 1);
  DoubleMatrix dvecXOut(nObjPars, 1);

  double* pdXLowData = dvecXLow.data();
  double* pdXUpData  = dvecXUp .data();
  double* pdXInData  = dvecXIn .data();
  double* pdXOutData = dvecXOut.data();

  for ( int i = 0; i < nObjPars; i++ )
  {
    pdXLowData[i] = -(nObjPars - 1.0);
    pdXUpData[i]  = +(nObjPars - 1.0);

    pdXInData[i]  = 0.0;
  }

  double epsilon  = 1.e-5; 
  int nMaxIter    = 50; 
  double fOut     = 0.0;
  DoubleMatrix f_xOut(1, nObjPars);
  int level       = 0;
  Optimizer optimizer( epsilon, nMaxIter, level );

  void* pFvalInfo = 0;

  ok = sqpAnyBox( fourParamQuadratic, pFvalInfo, optimizer, 
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
 *
 * Implementation Notes 1
 * -----------------------
 *
 * In order to estimate a minimum value for the objective function, 
 * fval, sqpAnyBox calls the function nag_opt_nlp (e04ucc), which 
 * minimizes an arbitrary smooth function subject to constraints 
 * using a sequential quadratic programming (SQP) method.  The 
 * constraints allowed by nag_opt_nlp may include simple bounds on 
 * the variables, general linear constraints, and smooth nonlinear 
 * constraints.
 *
 * Although nag_opt_nlp supports both general linear constraints  
 * and smooth nonlinear constraints, they are not currently utilized  
 * by sqpAnyBox.  
 *
 * The function nag_opt_nlp is distributed by the Numerical Algorithm 
 * Group (NAG).
 *
 * Note: NAG routines expect arrays to be stored in row-major order.
 *
 *------------------------------------------------------------------------*/
/*------------------------------------------------------------------------
 *
 * Implementation Notes 2
 * -----------------------
 *
 * sqpAnyBox (nag_opt_nlp, in particular) may be (and indeed) called 
 * recursively.  This means, to avoid unexpected result and behavior,
 * never use global (static) variables.  It also ensures thread-able
 * in the future.
 *
 *------------------------------------------------------------------------*/

// 
// Review - Sachiko: suggestion
//
// The name "fval" for the objective function is somewhat misleading.
// It seems to suggest a tagible value, instead of a function.
// "objf" or "evalf" may clarify?
//
// 
// Review - Sachiko: suggestion
//
// This file contains so much information.  It's almost too much for a 
// single file.  Consider pulling out some static functions such as
// nag_real_cholesky_solve_mult_rhs which appears useful in general.
//
/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <iostream>
#include <fstream>
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
#include "nage04.h"
#include "nagf04.h"
}
#include "transpose.h"
#include "sqpAnyBox.h"
#include "DoubleMatrix.h"
#include "SpkException.h"

/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

// This function sets 0.0 to the corresponding output element when diff[i] is 0.0.
static void scaleElem(   int n,
                         const double* px, 
                         const double* pxLow, 
                         const double* pxDiff,
                         double* py);

static void unscaleElem(   int n,
                           const double* py, 
                           const double* pxLow, 
                           const double* pxUp, 
                           const double* pxDiff,
                           double* px);


static void scaleGradElem(   int n,
                             const double* pg, 
                             const double* pxDiff,
                             double* pScaledG);

static void NAG_CALL fvalScaled( Integer n, 
                                 double *y, 
                                 double *objf,
                                 double *gvalScaled, 
                                 Nag_Comm *comm );

static bool isWithinTol( double tol, 
                         const DoubleMatrix& dvecXHat, 
                         const DoubleMatrix& dvecXLow, 
                         const DoubleMatrix& dvecXUp, 
                         const DoubleMatrix& drowG,
                         const DoubleMatrix& dmatR );

static DoubleMatrix arrayToDoubleMatrix( const double* pdAIn, 
                                         int nRows, 
                                         int nCols );

static DoubleMatrix getLowerTriangle( const DoubleMatrix& dmatA );

static bool isLowerTriangular( const DoubleMatrix& dmatA );

static void NAG_CALL confun( Integer, Integer, 
                             Integer *, 
                             double *, 
                             double *, 
                             double *, 
                             Nag_Comm *);


/*------------------------------------------------------------------------
 * Namespace declarations
 *------------------------------------------------------------------------*/
//
// Review - Sachiko: Intentional?  but confusing.
//
// The same namespace is defined in sqpAnyBox.h too.  One in the header
// defines only FVAL_PROTOTYPE.  The definition here must be appending
// more information but it's a bit confusing.  Since FVAL_PROTOTYPE
// is not needed to be defined in the header, perhaps just move that
// portion to here and compress them.
// 
namespace sqpanybox 
{
  //
  // Structure: FvalScaledInfo
  //
  struct FvalScaledInfo 
  {
    //
    // Review - Sachiko:  Error
    // 
    // This function prototype "FVAL_PROTOTYPE" is not documented
    // anywhere even though it is defined in the header (ie. visible to external).
    // Suggest moving the definition local to this file.
    //
    FVAL_PROTOTYPE fval;                // Pointer to fval.
    const void* pFvalInfo;              // Pointer to information for fval.
    const DoubleMatrix* pdvecXLow;      // Unscaled lower bounds.
    const DoubleMatrix* pdvecXUp;       // Unscaled upper bounds.
    const DoubleMatrix* pdvecXDiff;     // Difference between unscaled  
                                        // lower and upper bounds.
    SpkException exceptionOb;
  } ;

  //
  // Class: NagOptWrapper
  //
  //
  // This class ensures that the NAG allocation and deallocation functions 
  // are called when the NAG options structure is created and destroyed.
  // It does this by including the allocation and deallocation functions 
  // in the constructor and destructor functions, respectively.
  //
  class NagOptWrapper
  {
  public:
    Nag_E04_Opt options;

    NagOptWrapper()  { e04xxc( &options ); }
    ~NagOptWrapper() { e04xzc( &options, "all", NAGERR_DEFAULT ); }
  };

  //
  // Review - Sachiko: Obsolete
  //
  // Thanks for the thoughtfulness here, Mitch!  This can go away
  // completely because we have defined our exception structure.
  //

  // Class: SqpAnyBoxException
  //
  //
  // This class is an empty exception class that is thrown when an
  // error occurs in sqpAnyBox which requires the program to terminate.
  //
  // [Revisit - Exceptions - Mitch] More information should be added to this 
  // exception so that it can possibly be handled when it is caught.
  //
  class SqpAnyBoxException 
  {
  public:
    SqpAnyBoxException() {}
    ~SqpAnyBoxException() {}
  };

}

using namespace sqpanybox;


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
void sqpAnyBox( FVAL_PROTOTYPE fval,
                const void* pFvalInfo, 
                Optimizer& optimizer,
                const DoubleMatrix& dvecXLow,
                const DoubleMatrix& dvecXUp,
                const DoubleMatrix& dvecXIn, 
                DoubleMatrix* pdvecXOut, 
                double* pdFOut, 
                DoubleMatrix* pF_xOut )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  int i;
  double epsilon  = optimizer.getEpsilon();
  int    nMaxIter = optimizer.getNMaxIter();
  int    level    = optimizer.getLevel();
  
  assert( epsilon >  0.0 );
  assert( epsilon <= 1.0 );

  const double* pdXLowData = dvecXLow.data();
  const double* pdXUpData  = dvecXUp.data();
  const double* pdXInData  = dvecXIn.data();

  int     nXLowRows  = dvecXLow.nr();
  int     nXUpRows   = dvecXUp.nr();
  int     nXInRows   = dvecXIn.nr();

  assert( nXLowRows == nXUpRows );
  assert( nXLowRows == nXInRows );

  // Set the number of objective function parameters.
  int nObjPars  = nXInRows;

  // If the final x value should be returned, do some initializations.
  double* pdXOutData;
  if ( pdvecXOut ) {
    int nXOutRows = pdvecXOut->nr();
    pdXOutData    = pdvecXOut->data();
    assert( nXOutRows == nObjPars );
  }

  DoubleMatrix dvecXDiff( nObjPars, 1 );

  double* pdXDiffData = dvecXDiff.data();

  //======================[Begin: debug only code]======================
  #ifdef _DEBUG
  //
  // Validate the lower and upper bounds and verify that the 
  // initial x value is between them.
  //
  for ( i = 0; i < nObjPars; i++ ) {
    assert( pdXLowData[i] <= pdXUpData[i] );
    assert( pdXInData[i] >= pdXLowData[i] && pdXInData[i] <= pdXUpData[i] );
  }
  //
  #endif
  //======================[End:   debug only code]======================


  //------------------------------------------------------------
  // Initializations for the scaled objective function.
  //------------------------------------------------------------

  // The various y vectors are scaled versions of their x counterparts.
  DoubleMatrix dvecYLow( nObjPars, 1 );
  DoubleMatrix dvecYUp( nObjPars, 1 );
  DoubleMatrix dvecY( nObjPars, 1 );

  double* pdYLowData = dvecYLow.data();
  double* pdYUpData  = dvecYUp .data();
  double* pdYData    = dvecY   .data();

  // Check to see if the lower and upper bounds for each element of x are 
  // equal and then set the bounds and the initial value y accordingly.
  //
  // Sachiko: For efficiency, intialize these vectors with default values before the loop.
  dvecYLow.fill(0);
  dvecYUp.fill(0);
  dvecY.fill(0);
  for ( i = 0; i < nObjPars; i++ )
  {
    pdXDiffData[i] = pdXUpData[i] - pdXLowData[i]; 

    if ( pdXDiffData[i] != 0.0 ) 
    {
      // The x bounds are not equal, so constrain this element 
      // to the interval [0,1].
      //pdYLowData[i] = 0.0;
      pdYUpData[i]  = 1.0;
      
      //pdYData[i] = scaleElem( pdXInData[i], pdXLowData[i], pdXDiffData[i] );
    }
  }

  // This function sets 0.0 to the corresponding output element when diff[i] is 0.0.
  scaleElem(nObjPars, pdXInData, pdXLowData, pdXDiffData, pdYData);

  // Instantiate a row vector to hold the scaled gradient, 
  // gScaled(y) = fScaled_y(y).
  DoubleMatrix drowGScaled( 1, nObjPars );
  double* pdGScaledData = drowGScaled.data();


  //------------------------------------------------------------
  // Define the options parameter for the function nag_opt_nlp.
  //------------------------------------------------------------
  
  // Parameter: options
  // Input: a structure whose members are optional parameters 
  // for nag_opt_nlp. 
  // Output: some elements of the options structure will supply
  // details of the results of the optimization. 

  // 
  // Review - Sachiko: Error
  //
  // Simply "NagOptWrapper nagOptWrapper;" would do what you expect.
  // Currently there are two objects created in the following statement.
  //
  NagOptWrapper nagOptWrapper = NagOptWrapper();
  Nag_E04_Opt& options = nagOptWrapper.options;

  // Parameter: options.print_level
  // Parameter: options.minor_print_level
  // Parameter: options.list
  // Input: the level of results printed at each major iteration;  
  // the level of results printed at each minor iteration, i.e.,  
  // the iterations of the QP subproblem; and a boolean flag 
  // indicating if the parameter settings will be printed.
  //
  //
  //***************************************************************************
  // [Revisit - Next SPK Iteration - Improved Diagnostics - Mitch]
  //
  // Currently the tracing done for the NAG optimizer (called by sqpAnyBox) is
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
  // derivative checking information.  Another thing we could do in sqpAnyBox
  // is to analyze the derivative information from the NAG optimizer and
  // provide diagnostics to the user that lets them know which component(s) of
  // the user's derivative are in error.
  // 
  // Therefore, for the next iteration, we could improve the diagnostics for
  // sqpAnyBox by defining our own tracing function and also our own
  // derivative checking function.  I think that once we do that, then the
  // specifications for the level parameter will be much easier to write.
  //***************************************************************************
  //
  //
  switch ( level ) 
  {
  case 1:
    options.print_level       = Nag_Soln_Iter;
    options.minor_print_level = Nag_NoPrint;
    options.list              = FALSE;
    break;
  case 2:
    options.print_level       = Nag_Soln_Iter_Long;
    options.minor_print_level = Nag_NoPrint;
    options.list              = FALSE;
    break;
  case 3:
    options.print_level       = Nag_Soln_Iter_Long;
    options.minor_print_level = Nag_Soln_Iter;
    options.list              = FALSE;
    break;
  case 4:
    options.print_level       = Nag_Soln_Iter_Long;
    options.minor_print_level = Nag_Soln_Iter_Long;
    options.list              = TRUE;
    break;
  default:
    options.print_level       = Nag_NoPrint;
    options.minor_print_level = Nag_NoPrint;
    options.list              = FALSE;
  }

  //
  // Review - Sachiko: suggestion for efficiency
  // 
  // Compress the following indented block into the above switch statement block.
  // Though the effect will be neglegible, get a habit in minimizing 
  // if-then-else or switch statements in order to reduce run-time overhead
  // 
      // Parameter: options.obj_deriv
      // Input: indicates whether all the derivatives of the objective
      // function are provided in function fvalScaled. 
      options.obj_deriv = TRUE;

      // Parameter: options.verify_grad
      // Input: specifies the level of derivative checking to be
      // performed by nag_opt_nlp on the gradient elements computed
      // by the user supplied functions fvalScaled and confun.
      // Parameter: options.print_deriv
      // Input: controls whether the results of any derivative
      // checking are printed out.
      switch ( level )
      {
      case 4:
        options.verify_grad = Nag_CheckObj;     // Component check of the objective gradient.
        options.print_deriv = Nag_D_Full;       // Print full details of gradient check. 
        break;
      default:
        options.verify_grad = Nag_NoCheck;      // Don't check the objective gradient.
        options.print_deriv = Nag_D_NoPrint;    // Don't print checked gradients.
      }
  
  // Parameter: options.f_diff_int
  // Input: defines an interval used to estimate derivatives by
  // finite differences.
  // Constraint: DBL_EPSILON <= options.f_diff_int < 1.0, where DBL_EPSILON
  // is machine precision.
  // Note: see the note for the parameter optim_tol.
  options.f_diff_int = epsilon;  
  assert( options.f_diff_int >= DBL_EPSILON && options.f_diff_int < 1.0 );

  // Parameter: options.c_diff_int
  // Input: if the algorithm switches to central differences
  // because the forward-difference approximation is not
  // sufficiently accurate the value of c_diff_int is used as the
  // difference interval for every element of x. 
  // Constraint: DBL_EPSILON <= options.c_diff_int < 1.0.
  options.c_diff_int = options.f_diff_int;
  assert( options.c_diff_int >= DBL_EPSILON && options.c_diff_int < 1.0 );

  // Parameter: options.max_iter 
  // Input: maximum number of major iterations allowed before termination.
  // Constraint: options.max_iter >= 0.  
  options.max_iter = nMaxIter;
  assert( options.max_iter >= 0 );

  // Parameter: options.minor_max_iter
  // Input: the maximum number of iterations for finding a
  // feasible point with respect to the bounds and linear
  // constraints (if any). The value also specifies the maximum
  // number of minor iterations for the optimality phase of each
  // QP subproblem.
  //
  // Review - Sachiko: question
  //
  // Where did "50" come from?  Just a curiosity.
  //
  options.minor_max_iter = 50 * ( nMaxIter != 0 );
  assert( options.minor_max_iter >= 0 );

  // Parameter: options.optim_tol
  // Input: specifies the accuracy to which the user wishes the
  // final iterate to approximate a solution of the problem.
  // Constraint: options.f_prec <= options.optim_tol < 1.0.
  // Note:  nag_opt_nlp will terminate successfully if the iterative
  // sequence of y-values is judged to have converged and the
  // final point satisfies the first-order Kuhn-Tucker conditions.
  // nag_opt_nlp considers the sequence of iterates to have 
  // converged at y if
  //                                 1/2
  //     alpha ||p||  <=  (optim_tol)     (1 + ||y||),
  //
  // where p is the search direction and alpha the step length from 
  // equation (3), which appears in section 9.1 of the specification 
  // for nag_opt_nlp that is contained in the NAG C Library Manual.
  //
  // If the tolerance for nag_opt_nlp is set to be
  //
  //                         2
  //     optim_tol  = epsilon  ,
  //
  // then nag_opt_nlp will converge when
  //
  //     alpha ||p||  <=  epsilon  (1 + ||y||),
  //
  // The above convergence criteria used by nag_opt_nlp is equivalent 
  // to the following convergence criteria used by sqpAnyBox:
  //
  //     abs( xOut - xStar )  <=  epsilon (xUp - xLow) .
  //
  // where abs is the element-by-element absolute value function.
  //
  // **********************************************************************
  // * The equivalence of the two convergence criteria will now be shown. *
  // *                                                                    *
  // * It is assumed that the distance between the final y value, yOut,   *
  // * and the true minimizer of the scaled objective function, yStar,    *
  // * is less than the step taken during the last iteration, alpha * p.  *
  // * That is,                                                           *
  // *                                                                    *
  // *     ||yOut - yStar||  <=  alpha ||p|| .                            *
  // *                                                                    *
  // * Since the final step satisfied the nag_opt_nlp convergence         *
  // * criteria,                                                          *
  // *                                                                    *
  // *    alpha ||p||  <=  epsilon  (1 + ||y||),                          *
  // *                                                                    *
  // * and since the y values are constrained to [0, 1], it follows that  *
  // *                                                                    *
  // *     ||yOut - yStar||  <=  n  epsilon ,                             *
  // *                                                                    *
  // * where n is the number of objective function parameters.  Thus,     * 
  // * follows that                                                       *
  // *                                                                    *
  // *     ||yOut - yStar||  <=  epsilon .                                *
  // *                                                                    *
  // * The above equation implies that every element of the vector        * 
  // * (yOut - yStar) must be less than or equal to epsilon, which means  *
  // *                                                                    *
  // *   abs( yOut - yStar )  <=  epsilon .                               *
  // *                                                                    *
  // * Since the i-th elements of x and y are related as follows,         *
  // *                                                                    *
  // *     y(i) = [ x(i) - xLow(i) ] / [ xUp(i) - xLow(i) ] ,             *
  // *                                                                    *
  // * we have the final result,                                          *
  // *                                                                    *
  // *     abs( xOut - xStar )  <=  epsilon (xUp - xLow) .                *
  // *                                                                    *
  // * Therefore, the two convergence criteria are equivalent.            *
  // **********************************************************************
  //
  options.optim_tol = epsilon * epsilon;  
  assert( options.optim_tol >= options.f_prec && options.optim_tol < 1.0 ); 
  
  // Brad: 12/31/00 ===============================

  // Parameter: f_prec
  // Input: Specifies the accuracy with which the objective function
  // is calculated. Because calculation of the objective often involes
  // solution of differential equations, set this for as inaccurate as possible.
  options.f_prec =  options.optim_tol;


  // Brad: 12/28/00 ===============================

  // Parameter: step_limit
  // Input: Specifies the maximum change in the arguments at the first
  // step in a line search. If xnext is the first point of a line search
  // that starts at xstart, |xnext - xstart| <= step_limit * (1 + |xstart|).
  // For our case, |xstart| is bounded by 1.
  //
  // [Revisit - Optimizer Parameters - Mitch] This nag_opt_nlp 
  // parameter is currently hard-coded.  It should be an argument
  // to sqpAnyBox that the calling routine can change.
  //
  options.step_limit = .05;
  assert(   options.step_limit > 0.0 );


  // ==============================================

  // Parameter: crash_tol
  // Input: crash_tol is used during a `cold start' when
  // nag_opt_nlp selects an initial working set (options.start =
  // Nag_Cold).  The initial working set will include (if
  // possible) bounds or general inequality constraints that lie
  // within crash_tol of their bounds.  In particular, a
  // constraint of the form (a(j)**T)x >= l will be included in
  // the initial working set if |(a(j)**T)x-l| <= crash_tol *
  // (1+|l|).
  // Constraint: 0.0 <= options.crash_tol <= 1.0.
  //
  // [Revisit - Optimizer Parameters - Mitch] This nag_opt_nlp 
  // parameter is currently hard-coded.  It should be an argument
  // to sqpAnyBox that the calling routine can change.
  //
  options.crash_tol = 1.0e-4;

  // Parameter: hessian
  // Input: controls the contents of the optional parameter h on
  // return from nag_opt_nlp.  
  // Note: h need not be set if the default option of start =
  // Nag_Cold is used as n*n values of memory will be
  // automatically allocated by nag_opt_nlp.
  // If hessian = TRUE, then h contains the upper triangular 
  // Cholesky factor R of H, the approximate (untransformed) Hessian 
  // of the Lagrangian, with the variables in the natural order.
  options.hessian = true;


  //------------------------------------------------------------
  // Define the other parameters for the function nag_opt_nlp.
  //------------------------------------------------------------
  
  // Parameter: n
  // Input: n, the number of variables.
  // Output: unspecified.
  // Constraint: n > 0.
  Integer n = nObjPars;
  assert( n > 0 );

  // Parameter: nclin
  // Input: n(L), the number of general linear constraints.
  // Output: unspecified.
  // Constraint: nclin >= 0.
  Integer nclin = 0;
  assert( nclin >= 0 );

  // Parameter: ncnlin
  // Input: n(N), the number of nonlinear constraints.
  // Output: unspecified.
  // Constraint: ncnlin >= 0.
  Integer ncnlin = 0;
  assert( ncnlin >= 0);

  // Parameter: a[nclin][tda]
  // Input: the ith row of a must contain the coefficients of the
  // ith general linear constraint (the ith row of the matrix
  // A(L) in (1)), for i = 1,2,...,n(L).
  // Output: unspecified.
  // Note: If nclin = 0 then the array a is not referenced.
  double* a = 0;

  // Parameter: tda
  // Input: the second dimension of the array a as declared in
  // the function from which nag_opt_nlp is called.
  // Output: unspecified.
  // Constraint: tda >= n if nclin > 0.
  Integer tda = 0;
  assert( tda >= n || nclin == 0 );

  // Parameter: bl[n+nclin+ncnlin]
  // Parameter: bu[n+nclin+ncnlin]
  // Input: bl must contain the lower bounds and bu the upper
  // bounds, for all the constraints in the following order. The
  // first n elements of each array must contain the bounds on
  // the variables, the next n(L) elements the bounds for the
  // general linear constraints (if any), and the next n(N)
  // elements the bounds for the nonlinear constraints (if any).
  // Constraint: bl[j] <= bu[j], for j = 0,1,...,n+nclin+ncnlin-1,
  // Constraint: |beta| < inf_bound when bl[j] = bu[j] = beta.
  double* bl = pdYLowData;
  double* bu = pdYUpData;

  //======================[Begin: debug only code]======================
  #ifdef _DEBUG
  //
  // Validate the lower and upper bounds.
  //
  for ( i = 0; i < n+nclin+ncnlin; i++ ) {
    assert( bl[i] <= bu[i] );
    if ( bl[i] == bu[i] )
      
      // This will satisfy the constraint because the default 
      // value for inf_bound is 10^20. 
      assert( bl[i] <= 1.0 );
  }
  //
  #endif
  //======================[End:   debug only code]======================

  // Parameter: y[n]
  // Input: an initial estimate of the scaled solution.
  // Output: the final estimate of the scaled solution.
  double* y = pdYData;

  // Parameter: objf
  // Input: unspecified.
  // Output: the value of the scaled objective function at the final iterate.
  double objf;

  // Parameter: gvalScaled[n]
  // Input: unspecified.
  // Output: the gradient of the scaled objective function at the final
  // iterate (or its finite difference approximation).
  double* gvalScaled = pdGScaledData;

  // Parameter: comm
  // Input/Output: structure containing pointers for communication 
  // to the user-supplied functions objfun (fvalScaled) and 
  // confun, and the optional user-defined printing function.
  Nag_Comm comm;
  FvalScaledInfo info;
  info.fval       = fval;
  info.pFvalInfo  = pFvalInfo;
  info.pdvecXLow  = &dvecXLow;
  info.pdvecXUp   = &dvecXUp;
  info.pdvecXDiff = &dvecXDiff;

  comm.p = (Pointer) &info;

  // Parameter: fail
  // Input/Output:  see the discussion of the NAG error parameter in
  // "Essential Introduction to the NAG C Library".
  // Note:  the macro INIT_FAIL initializes the fail structure so that
  // NAG error messages are not printed when an error is found and so 
  // that control is returned here after an error.
  static NagError fail;
  INIT_FAIL(fail);


  //------------------------------------------------------------
  // Retrieve state information for warm start
  //------------------------------------------------------------

  if ( optimizer.getIsWarmStart() )
  {
    options.start  = Nag_Warm;
	StateInfo stateInfo = optimizer.getStateInfo();

	for( int j = 0; j < n; j++ )
	{
	    y[ j ] = stateInfo.x[ j ];
	}

	options.state  = stateInfo.state;
	options.lambda = stateInfo.lambda;
	options.h      = stateInfo.h;
  }


  //------------------------------------------------------------
  // Optimize the scaled objective function.
  //------------------------------------------------------------

  //
  // Revisit - Exception - Sachiko:
  //
  // Extract the exception information from comm structure and
  // throw it if there was.
  //

  nag_opt_nlp( n, nclin, ncnlin, a, tda, bl, bu, fvalScaled, confun, 
    y, &objf, gvalScaled, &options, &comm, &fail );

  
  //------------------------------------------------------------
  // Determine if the optimization was successful.
  //------------------------------------------------------------

  bool ok;
  SpkError::ErrorCode errorcode;
  StateInfo stateInfo;

  switch ( fail.code ) 
  {
  case NE_NOERROR:
	  optimizer.setIsTooManyIter( false );
	  optimizer.setNIterCompleted( options.iter );
      optimizer.deleteStateInfo();
      ok = true;
      break;
  case NW_NOT_CONVERGED:    // The final iterate y satisfies 
                            // the first-order Kuhn-Tucker
                            // conditions for a minimum up to the 
                            // accuracy requested, but the
                            // sequence of iterates, i.e., y
                            // values, has not yet converged.
                            // (See the description of optim_tol 
                            // and see section 9.1 of the
                            // documentation for nag_opt_nlp in 
                            // the NAG C Library Manual.)

      // If the final y value is actally within epsilon tolerance of 
      // the true value yStar, then go on.  Note that NAG arrays use
      // row-major order.
      if ( isWithinTol( epsilon, dvecY, dvecYLow, dvecYUp, drowGScaled, 
               getLowerTriangle( arrayToDoubleMatrix( options.h, n, n ) ) ) )
      {
        ok = true;
        break;
      }
      else
      {
        ok = false;
        errorcode = SpkError::SPK_NOT_CONVERGED;
        break;
    
      }

  case NW_KT_CONDITIONS:          // The current point cannot be 
                                  // improved upon. The final point
                                  // does not satisfy the first-order 
                                  // Kuhn-Tucker conditions and
                                  // no improved point for the merit 
                                  // function could be found
                                  // during the final line search.

      // If the final y value is actally within epsilon tolerance of 
      // the true value yStar, then go on.  Note that NAG arrays use
      // row-major order.
      if ( isWithinTol( epsilon, dvecY, dvecYLow, dvecYUp, drowGScaled, 
               getLowerTriangle( arrayToDoubleMatrix( options.h, n, n ) ) ) )
      {
        ok = true;
        break;
      }
      else
      {
        ok = false;
        errorcode = SpkError::SPK_KT_CONDITIONS;
        break;
      }

  case NW_LIN_NOT_FEASIBLE:       // No feasible point was found for 
                                  // the linear constraints and bounds.
      ok = false;
      errorcode = SpkError::SPK_LIN_NOT_FEASIBLE;
      break;

  case NW_NONLIN_NOT_FEASIBLE:    // No feasible point could be found 
                                  // for the nonlinear constraints.
      ok = false;
      errorcode = SpkError::SPK_NONLIN_NOT_FEASIBLE;
      break;

  case NW_TOO_MANY_ITER:          // The maximum number of iterations 
                                  // have been performed.
	  optimizer.setIsTooManyIter( true );
      optimizer.setNIterCompleted( options.iter );

	  //------------------------------------------------------------
      // Save state information for warm start
      //------------------------------------------------------------

      if( !optimizer.getIsSubLevelOpt() && optimizer.getStateInfo().n )
	  {
		  stateInfo.n      = n;
	      stateInfo.x      = y;
          stateInfo.state  = options.state;
          stateInfo.lambda = options.lambda;
          stateInfo.h      = options.h;
	      optimizer.setStateInfo( stateInfo );

          ok = true;
	  }
	  else
	  {
	      ok = false;
          errorcode = SpkError::SPK_TOO_MANY_ITER;
	  }

      break;

  default:

      // If any other errors or warnings are returned by nag_opt_nlp,
      // then throw an exception.
      ok = false;
      errorcode = SpkError::SPK_UNKNOWN_OPT_ERR;
      break;
  }

  if( !ok )
  {
      const int max = SpkError::maxMessageLen();
      char message[max];
      snprintf( message, max, "%d%s%d\0", fail.code, fail.message, fail.errnum );
      SpkError err(errorcode, message, __LINE__, __FILE__ );

      throw info.exceptionOb.push(err);
  }

  //------------------------------------------------------------
  // If the optimization succeeded, set the values to be returned.
  //------------------------------------------------------------
  // If the final x value should be returned, then compute it
  // from the final y value.
  if ( pdvecXOut ) 
  {
    unscaleElem( nObjPars, pdYData, pdXLowData, pdXUpData, pdXDiffData, pdXOutData );
  }

  // If the final value for the objective function should be
  // returned, then set it equal to the value from nag_opt_nlp.
  if ( pdFOut )
  {
    *pdFOut = objf;
  }
  
  if( pF_xOut )
  {
      double* pdF_x = pF_xOut->data();
      std::copy(gvalScaled, gvalScaled+nObjPars, pdF_x);
  }
}


/*=========================================================================
 *
 *
 * Local Function Definitions
 *
 *
 =========================================================================*/

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
static void scaleElem(   int n,
                         const double* px, 
                         const double* pxLow, 
                         const double* pxDiff,
                         double* py)
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

static void unscaleElem(   int n,
                           const double* py, 
                           const double* pxLow, 
                           const double* pxUp, 
                           const double* pxDiff,
                           double* px)
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

static void scaleGradElem(   int n,
                             const double* pg, 
                             const double* pxDiff,
                             double* pScaledG)
{
    for(int i=0; i<n; i++)
        pScaledG[i] = pxDiff[i] * pg[i];
}

/*************************************************************************
 * Function: fvalScaled
 *
 *
 * Note:  in the NAG documentation the objective function for nag_opt_nlp
 * (e04ucc) has the more generic name objfun, i.e., this function is an 
 * implementation of objfun specific to sqpAnyBox.
 *
 *
 * Description
 * -----------
 *
 * Evaluates the scaled version of the objective function, fScaled(y),
 * and (optionally) its gradient, gScaled(y), at the current point y.
 *
 *
 * Arguments
 * ---------
 *
 * n
 * Input: n, the number of variables.
 *
 * y[n]
 * Input: y, the vector of variables at which the value of fScaled(y) 
 * and/or all available elements of its gradient, gScaled(Y), are to 
 * be evaluated.
 *
 * objf
 * Output: if comm->flag = 0 or 2, this function sets objf to the 
 * value of the scaled objective function fScaled(y) at the current
 * point y. If it is not possible to evaluate fScaled(y), then
 * this function assigns a negative value to comm-> flag;
 * nag_opt_nlp will then terminate.
 *
 * gvalScaled[n]
 * Output: if comm->flag = 2, gvalScaled must contain all the
 * elements of the vector gScaled(y) given by
 *
 *                   -                                   -
 *                  |  (partial fScaled(y)/partial y(1))  | 
 *                  |  (partial fScaled(y)/partial y(2))  |
 *                  |                   .                 |
 *     gScaled(y) = |                   .                 |  ,
 *                  |                   .                 |
 *                  |  (partial fScaled(y)/partial y(n))  |
 *                   -                                   -
 *
 * where (partial fScaled(y)/partial y(i)) is the partial
 * derivative of the scaled objective function with respect to
 * the ith variable evaluated at the point y.
 * If the optional parameter obj_deriv = TRUE (the
 * default), all elements of gvalScaled must be set; if obj_deriv =
 * FALSE, any available elements of the vector gScaled(y) must
 * be assigned to the elements of gvalScaled; the remaining
 * elements must remain unchanged.
 *
 * comm
 * Pointer to structure of type Nag_Comm; the following
 * members are relevant to fvalScaled.
 *
 *      flag - Integer
 *      Input: fvalScaled is called with comm->flag set to 0
 *      or 2.
 *      If comm->flag = 0, then only fScaled(y) is returned.
 *      If comm->flag = 2, then both fScaled(y) and gScaled(y) 
 *      are returned.
 *      Output: if it is not possible to evaluate the scaled objective 
 *      and/or its gradient, then fvalScaled will reset comm->flag to 
 *      some negative number in order to force nag_opt_nlp to terminate
 *      immediately with the error indicator NE_USER_STOP.
 *      If fail is supplied to nag_opt_nlp, fail.errnum
 *      will be set to the setting of comm->flag.
 *
 *      first - Boolean
 *      Input: will be set to TRUE on the first call to
 *      fvalScaled and FALSE for all subsequent calls.
 *
 *      nf - Integer
 *      Input: the number of evaluations of the objective
 *      function; this value will be equal to the number
 *      of calls made to fvalScaled including the current one.
 *
 *      user - double *
 *      iuser - Integer *
 *      p - Pointer
 *      The type Pointer will be void * with a C compiler
 *      that defines void * and char * otherwise.
 *      Before calling nag_opt_nlp these pointers may be
 *      allocated memory by the user and initialized with
 *      various quantities for use by fvalScaled when called
 *      from nag_opt_nlp.
 *
 *************************************************************************/
//
// Note - Sachiko: Throw NO exception!
//
// This rotuine is called by the C nag optimizer.  Don't ever attempt
// to throw an exception!  It will be never caught.
//
static void NAG_CALL fvalScaled( Integer n, 
                                 double *y, 
                                 double *objf,
                                 double *gvalScaled, 
                                 Nag_Comm *comm )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------
  //
  // Review - Sachiko:  suggestion
  //
  // Instead of returning successfully, why not terminate the
  // program.  comm->flag is only set by the optimizer callers,
  // who are Rfpk programmers.  It seems like a successfully return
  // is more troublesome later than early assert.
  //
  if ( comm->flag != 0 && comm->flag != 2 ) 
  {
    return;
  }

  // Cast the void pointer in the NAG communication structure so 
  // that it now points to the scaling information structure.
  FvalScaledInfo * pInfo = (FvalScaledInfo*) (comm->p);

  // Get the pointer to the unscaled objective function.
  FVAL_PROTOTYPE fval = pInfo->fval;

  // Sachiko:  Avoid aliasing.  It creates excessive temporary objects.
  //
  // Get the pointers to the unscaled lower and upper bounds 
  // and the differences between them.
  /*
  const DoubleMatrix* pdvecXLow  = pInfo->pdvecXLow;
  const DoubleMatrix* pdvecXUp   = pInfo->pdvecXUp;
  const DoubleMatrix* pdvecXDiff = pInfo->pdvecXDiff;
  const double* pdXLowData  = pdvecXLow->data();
  const double* pdXUpData   = pdvecXUp->data();
  const double* pdXDiffData = pdvecXDiff->data();
  */
  const double *const pdXLowData  = pInfo->pdvecXLow->data();
  const double *const pdXUpData   = pInfo->pdvecXUp->data();
  const double *const pdXDiffData = pInfo->pdvecXDiff->data();

  //------------------------------------------------------------
  // Prepare the parameters for the unscaled objective function.
  //------------------------------------------------------------

  // Transform the elements of the y vector back to their 
  // unscaled form. 
  DoubleMatrix dvecX(n, 1);
  double* pdXData = dvecX.data();
  unscaleElem( n, y, pdXLowData, pdXUpData, pdXDiffData, pdXData );

  // Check to see if the scaled objective function, fScaled(y), needs 
  // to be returned by this function.  If not, set the pointer to zero
  // so fval will not evaluate the unscaled objective function, f(x).
  double  dFOut  = 0.0;
  double* pdFOut = 0;
  
  //
  // Review - Sachiko: redundant
  //
  // comm-flag is validated eariler.  It's only either 0 or 2 at this point.
  // Assert, as a pre-condition, instead of if statement which has some 
  // run time overhead.
  //
  if ( comm->flag == 0 || comm->flag == 2 ) 
  {
    pdFOut = &dFOut;
  }
  
  // Check to see if the gradient of the scaled objective function, 
  // gScaled(y), needs to be returned by this function.  If not, set 
  // the pointer to zero so that fval will not evaluate the unscaled 
  // gradient, g(x).s
  DoubleMatrix drowGOut( 1, n );
  DoubleMatrix* pdrowGOut = 0;

  if ( comm->flag == 2 )
  {
    pdrowGOut = &drowGOut;
  }


  //------------------------------------------------------------
  // Evaluate the unscaled objective function.
  //------------------------------------------------------------

  //
  // Sachiko
  // 
  // the objective function now returns nothing.  Instead
  // throw an exception.
  // Catch it and pass it back through a placeholder permitted
  // in the arbitrary information package.
  //

  try{
	  fval( dvecX, pdFOut, pdrowGOut, pInfo->pFvalInfo );
  }
  catch( SpkException& e )
  {
      // If there was a problem, set the flag to terminate nag_opt_nlp.
      //
      comm->flag = -1; 

      // Extract the exception object from pFvalInfo.
      pInfo->exceptionOb = e;
      return;
  }
  catch( const std::exception& stde )
  {
      // If there was a problem, set the flag to terminate nag_opt_nlp.
      //
      comm->flag = -1; 

      // Extract the exception object from pFvalInfo.
      pInfo->exceptionOb = SpkException(
          stde, 
          "A standard exception was thrown during an attempt to evaluate the given objective function.",
          __LINE__, 
          __FILE__ 
        );
      return;
  }

  catch(...)
  {
      // If there was a problem, set the flag to terminate nag_opt_nlp.
      //
      comm->flag = -1; 

      // Extract the exception object from pFvalInfo.
      pInfo->exceptionOb = SpkException(
          SpkError::SPK_UNKNOWN_ERR, 
          "Unknown exception was thrown during an attempt to evaluate the given objective function.",
          __LINE__, 
          __FILE__ 
        );
      return;
  }


  //------------------------------------------------------------
  // Set the scaled objective function and/or its gradient.
  //------------------------------------------------------------

  //
  // Review - Sachiko: redundant
  //
  // comm-flag is validated eariler.  It's only either 0 or 2 at this point.
  // Assert, as a pre-condition, instead of if statement which has some 
  // run time overhead.
  //
      // If this function should return the value for the scaled 
      // objective function, then set it.
      if (comm->flag == 0 || comm->flag == 2)
      {
        assert( pdFOut != 0 );
        *objf = *pdFOut;
      }

  // If this function should return the value for the gradient 
  // of the scaled objective function, then set it.
      if (comm->flag == 2)
      {
        assert( pdrowGOut != 0 );
        double* pdGOutData = pdrowGOut->data();
        int nGCols = pdrowGOut->nc();
        assert( n == nGCols );
    
        scaleGradElem( n, pdGOutData, pdXDiffData, gvalScaled);
      }

  // If the fval function returns false, set the flag to -2.
  // This is to terminate nag_opt_nlp and to escape from throwing SpkException.

  return;
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
static bool isWithinTol( double tol, 
                         const DoubleMatrix& dvecXHat, 
                         const DoubleMatrix& dvecXLow, 
                         const DoubleMatrix& dvecXUp, 
                         const DoubleMatrix& drowG,
                         const DoubleMatrix& dmatR
                       )
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


  // Updated 2-5-01 Alyssa
  // fixed for const correctness
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

static DoubleMatrix arrayToDoubleMatrix( const double* pdAIn, 
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

static DoubleMatrix getLowerTriangle( const DoubleMatrix& dmatA ) 
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



// Updated 2-5-01 Alyssa
// fixed for const correctness

/*************************************************************************
 * Function: isLowerTriangular
 *
 *
 * Returns true if the square matrix A is lower triangular, i.e., if
 * all of its elements above the diagonal are zero.
 *
 *************************************************************************/

static bool isLowerTriangular( const DoubleMatrix& dmatA )
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
          //
          // Review - Sachiko: Error
          //
          // The caller is using this routine to test a certain property of a matrix
          // and expectes this to return a boolean value.
          // The test, in particular, is done in an assert() statement.
          // If this routine terminates the program as a whole, it seems
          // countereffective.  Remove assert(0), back to return false.
          //
          assert(0);      // Sooner assert by Brad 12/28/00
          return false;
      }
    }
  }

  return true;
}


/*************************************************************************
 * Function: confun
 *
 *
 * Calculates the vector c(y) of nonlinear constraint functions and 
 * (optionally) its Jacobian (=(partial c/partial y) for a specified 
 * n element vector y. 
 *
 *************************************************************************/

static void NAG_CALL confun( Integer n, 
                             Integer ncnlin, 
                             Integer *needc,  
                             double *y, 
                             double *conf, 
                             double *conjac, 
                             Nag_Comm *comm )
{


  // Since sqpAnyBox does not currently make use of the general 
  // linear and smooth nonlinear constraints that are supported  
  // by nag_opt_nlp, this function is not defined in this file.


}
