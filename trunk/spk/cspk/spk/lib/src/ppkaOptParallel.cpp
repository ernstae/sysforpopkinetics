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
 * File: ppkaOptParallel.cpp
 *
 *
 * Optimizes the parametric population objective functions.
 *
 * Author: Sachiko Honda, based upon Mitch's implementation for the
 *         single processing version of ppkaOpt.cpp
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: ppkaOptParallel
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*

$begin PpkaOptParallel$$

$spell
	Model model
  bool  
  cbc  
  cmath  
  const  
  cout  
  dmat 
  drow  
  dvec  
  endl  
  eps
  epsilon  
  Hessian  
  Ind  
  int  
  iostream  
  iomanip  
  Iter  
  Laplace
  Max  
  nagg  
  namespace  
  Obj  
  pd  
  pdmat  
  pdrow  
  pdvec  
  ppka
  th  
  sd  
  sqrt  
  std  
  stdout  
  str
  subvector  
  var  
  Varbl  
  Vi  
  ppkaoptexample
  ind 
  inv 
  distrib 
  Fab 
  Rab 
  Da 
  multi  
  pdalp 
  holden
  covariances
  str
  Spk
  cerr
  covariance
  optInfo
  pathname
  fp
  spawn
  Ri
  valarray
  enum
  enumlator
  Objective objective
  Fo
  optimizer
  popOptInfo
  indOptInfo
  Sachiko
$$

$section ppkaOpt Parallel Version$$

$index ppkaOptParallel$$
$cindex optimizing \the parametric population objective \functions \in multi-process mode$$
$index parallel, ppkaOpt$$
$index population, parallel mode$$

$table
$bold Prototype:$$ $cend
$syntax/void ppkaOpt(
              SpkModel&               /model/,
              enum Objective          /objective/,
              const DoubleMatrix&     /dvecN/,
              const DoubleMatrix&     /dvecY/,
              Optimizer&              /popOptInfo/,
              const DoubleMatrix&     /dvecPopLow/,
              const DoubleMatrix&     /dvecPopUp/,
              const DoubleMatrix&     /dvecPopIn/,
              DoubleMatrix*           /pdvecPopOut/,
              const DoubleMatrix&     /dvecPopStep/,
              Optimizer&              /indOptInfo/,
              const DoubleMatrix&     /dvecIndLow/,
              const DoubleMatrix&     /dvecIndUp/,
              const DoubleMatrix&     /dmatIndIn/,
              DoubleMatrix*           /pdmatIndOut/,
              const DoubleMatrix&     /dvecIndStep/,
              double*                 /pdLTildeOut/,
              DoubleMatrix*           /pdrowLTilde_popOut/,
              DoubleMatrix*           /pdmatLTilde_pop_popOut/,
              bool                    /isMultiple/,
              const char*             /sharedDirectory/,
              const char*             /nodeCommand/
)
/$$

$tend

See also: $xref/ppkaOpt//ppkaOpt non-parallel version/$$, $xref/fitPopulation//fitPopulation/$$

$fend 25$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This is the core body of $bold Master$$ and the modified version of $xref/ppkaOpt//ppkaOpt()/$$
so that it can handle parallel computation.  To enable parallel computation, 
you specify $italic isMultiple$$ $math%true%$$.  If $italic isMultiple$$
is $math%false%$$, it switches to the ordinary $xref/ppkaOpt//ppkaOpt()/$$.
$pre

$$
When running in parallel, this routine expects individual parameters to be
optimized and returned by $xref/Node//Node(s)/$$.  If no Node participates, the system
still sits and waits forever.
$pre

$$
Master packages information necessary for an individual's parameter estimation 
in an $xref/IndInputDataPackage//request package/$$ and
send it though $xref/MasterEndChannel//Channel/$$ to Node.  It issues all
individuals' request packages at once and wait for results to come back randomly.
$pre

$$
If some individuals' results do not come back by the time most others
have come back, Master re-issues these delayed individuals' requests to be grabbed by an available Node.
When Master receives duplicated results, it makes use of the one which
arrived first and simply ignore the following ones.
$pre

$$
Master keeps track of which individuals have been issued or which ones 
have returned results using a $xref/StatusList//StatusList class/$$ object which 
is like a bookkeeper.
$pre

$$
It repeats the above process for all iterations and sends out $xref/broadCastEndOfSpk//the
end of SPK signal/$$.

$head Return Value$$
Upon a successful completion, the function returns normally and
set the given output value place holders if it is able to 
obtain an acceptable estimate for $math%alpHat%$$, the true minimizer
of $math%LTilde(alp)%$$, within a specified number of iterations. 
In order for an acceptable estimate for $math%alpHat%$$ to be 
found, acceptable sets of values must also be found for $math%bHat_i%$$ 
and $math%bTilde_i%$$ that are calculated using the estimate for 
$math%alpHat%$$.  The case that too-many-iter occurred during 
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

$head Arguments$$
$syntax/
/model/
/$$
This function expects $italic model$$ to be a function of
all three parameters: $math%alp%$$, $math%b%$$ and $math%i%$$.
Refer $xref/glossary/Model Functions Depend on i - alp - b/Model Functions Depend on i - alp - b/$$
for details.
$pre

$$
When $italic whichObjective$$ is specified $code NAIVE_FIRST_ORDER$$, $italic model$$ must
be an object of $xref/NaiveFoModel//NaiveFoModel/$$ which is a subclass of $xref/SpkModel//SpkModel/$$.

$syntax/

/objective/
/$$
This string specifies which parametric population objective 
function will be minimized:  the modified Laplace, the modified 
Expected Hessian or the modified First Order.  
The valid values are:
$code MODIFIED_LAPLACE$$ specifies the modified laplace, 
$code EXPECTED_HESSIAN$$ for the modified expected hessian and 
$code NAIVE_FIRST_ORDER$$ for the straight translation version of
the modified first order.
$pre

$$
Note that there is another version of the modified first order,
which is implemented in the way a population problem is treated as a
large individual problem.  This latter version runs faster
although it consumes a larger heap space. 
For details, see $xref/firstOrderOpt//firstOrderOpt()/$$.

$syntax/

/dvecN/
/$$
The $code DoubleMatrix$$ $italic dvecN$$ contains the column vector 
$math%N%$$.  
The $th i$$ element of $math%N%$$
specifies the number of elements of $math%y%$$ that
correspond to the $th i$$ individual.
Note that the length of $italic dvecN$$ specifies the number of 
individuals in the population, $math%M%$$.

$syntax/

/dvecY/
/$$
The $code DoubleMatrix$$ $italic dvecY$$ contains the column vector 
$math%y%$$, which specifies the data for all the individuals.
The vector $math%y%$$ has
$math%
    N(1) + N(2) + ... + N(M)
%$$
elements where $math%M%$$ is the number of rows in $math%N%$$.
The data vector corresponding to the first individual is
$math%
                                         T
    y_1 = [ y(1) , y(2) , ... , y(N(1)) ]
%$$
Elements $math%y(N(1) + 1)%$$ through $math%y(N(1) + N(2))%$$ 
correspond to the second individual and so on.
(Note that $math%y_1%$$ refers to the first subvector or $math%y%$$ while
$math%y(1)%$$ refers to the first element of the vector $math%y%$$.)

$syntax/

/popOptInfo/
/$$
This $xref/Optimizer//Optimizer/$$ class object has three attributes.  
These attributes are parameters of the optimizer used in the population 
level optimization.  It has other attributes for handling running out of 
maximum iterations and for holding the optimization state information 
that is required by later restart(warm start) run.  

If restart is intended, the member function of the Optimizer object 
setupWarmStart() must be called to set up warm start before this function 
is called for the first time.  If warm start has been set up, when this 
function returns, the Optimizer object contains the state information and 
the object's member function getIsTooManyIter() returns true if and only if 
the too-many-iter occurred during the population level optimization process.  

$syntax/

/dvecPopLow/
/$$
The $code DoubleMatrix$$ $italic dvecPopLow$$ contains the column vector 
$math%popLow%$$, which specifies the lower limit for $math%pop%$$ during 
the optimization procedure.
The length of $italic dvecPopLow$$ is equal to the length of 
the fixed population parameter vector $math%pop%$$.

$syntax/

/dvecPopUp/
/$$
The $code DoubleMatrix$$ $italic dvecPopUp$$ contains the column vector 
$math%popUp%$$, which specifies the upper limit for $math%pop%$$ during 
the optimization procedure.
The length of $italic dvecPopUp$$ specifies the length of 
the fixed population parameter vector $math%pop%$$.

$syntax/

/dvecPopIn/
/$$
The $code DoubleMatrix$$ $italic dvecPopIn$$ contains the column vector 
$math%popIn%$$, which specifies the initial value for the fixed population 
parameters.
The $xref/glossary/Ordering Of Vectors/order condition/$$,
$math%popLow \le popIn \le popUp%$$, is assumed to hold.
Note that the length of $italic dvecPopIn$$ specifies the length of 
the fixed population parameter vector $math%pop%$$.

$syntax/

/pdvecPopOut/
/$$
If $italic pdvecPopOut$$ is not $code NULL$$, then the 
$code DoubleMatrix$$ pointed to by $italic pdvecPopOut$$ must 
be declared in the function that calls this function, and it 
must have the same dimensions as $italic dvecPopIn$$.
If $italic pdvecPopOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully,  
then the $code DoubleMatrix$$ pointed to by $italic pdvecPopOut$$ 
will contain the column vector $math%popOut%$$, which is the 
estimate for the true minimizer of the population objective function.
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdvecPopOut$$.

$syntax/

/dvecPopStep/
/$$
The $code DoubleMatrix$$ $italic dvecPopStep$$ contains the column vector 
$math%popStep%$$, which specifies the step size used for approximating
the derivatives with respect to the fixed population parameters.
The value of this parameter does not matter if
$italic pdmatLTilde_pop_popOut$$ is $code NULL$$.
The length of $italic dvecPopStep$$ is equal to the length of 
the fixed population parameter vector $math%pop%$$.

$syntax/

/indOptInfo/
/$$
This $xref/Optimizer//Optimizer/$$ class object has three attributes.  
These attributes are parameters of the optimizer used in the individual 
level optimization.

$syntax/

/dvecIndLow/
/$$
The $code DoubleMatrix$$ $italic dvecIndLow$$ contains the column vector 
$math%bLow%$$, which specifies the lower limit for the random parameters 
during the optimization procedure for all the individuals.
The length of $italic dvecIndLow$$ is equal to the length of 
the random population parameter vector $math%ind%$$.

$syntax/

/dvecIndUp/
/$$
The $code DoubleMatrix$$ $italic dvecIndUp$$ contains the column vector 
$math%bUp%$$, which specifies the upper limit for the random parameters 
during the optimization procedure for all the individuals.
The length of $italic dvecIndUp$$ is equal to the length of 
the random population parameter vector $math%ind%$$.

$syntax/

/dmatIndIn/
/$$
The $code DoubleMatrix$$ $italic dmatIndIn$$ contains the matrix 
$math%bIn%$$.  
The $th i$$ column of $math%bIn%$$ specifies the initial value for 
the random parameters for the $th i$$ individual.
If $math%ind_i%$$ is any column of $math%bIn%$$,
it is assumed that $math%bLow \le ind_i \le bUp%$$.
The column dimension of $math%bIn%$$ is equal to the number of 
individuals in the population, $math%M%$$.
Note that the number of rows in $italic dmatIndIn$$ specifies the 
length of the random population parameter vector $math%ind%$$.

$syntax/

/pdmatIndOut/
/$$
If $italic pdmatIndOut$$ is not $code NULL$$, 
then the $code DoubleMatrix$$ pointed to by $italic pdmatIndOut$$ must 
be declared in the function that calls this function, 
and it must have the same dimensions as $math%bIn%$$.
If $italic pdmatIndOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully,  
then the $code DoubleMatrix$$ pointed to by $italic pdmatIndOut$$ will 
contain $math%bOut%$$, which is the matrix of estimates for the true 
minimizers of the individual objective functions.
Otherwise, this function will not attempt to change the contents of 
the $code DoubleMatrix$$ pointed to by $italic pdmatIndOut$$.
To be specific, the $th i$$ column of $math%bOut%$$ contains a column
vector that is an estimate for $math%bHat_i%$$, the minimizer 
of $math%Lambda_i(popOut, ind)%$$ with respect to $math%ind%$$. 
This is under the assumption that $math%popOut%$$
is the true value for the fixed population parameters.
The value $math%epsilon(1)%$$ is used for accepting the minimizers with 
respect to the random population parameters.

$syntax/

/dvecIndStep/
/$$
The $code DoubleMatrix$$ $italic dvecIndStep$$ contains the column vector 
$math%bStep%$$, which specifies the step size used for approximating
the derivatives with respect to the random population parameters.
The length of $italic dvecIndStep$$ is equal to the length of 
the random population parameter vector $math%ind%$$.

$syntax/

/pdLTildeOut/
/$$
If $italic pdLTildeOut$$ is not $code NULL$$, then the $code double$$ 
value pointed to by $italic pdLTildeOut$$ must be declared in the 
function that calls this function.
If $italic pdLTildeOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully, 
then the $code double$$ value pointed to by $italic pdLTildeOut$$ will 
be equal to $math%LTilde(popOut)%$$, which is the value of the population 
objective function evaluated at $math%popOut%$$.
Otherwise, this function will not attempt to change the contents of the 
$code double$$ value pointed to by $italic pdLTildeOut$$.

$syntax/

/pdrowLTilde_popOut/
/$$
If $italic pdrowLTilde_popOut$$ is not $code NULL$$, then the 
$code DoubleMatrix$$ pointed to by $italic pdrowLTilde_popOut$$ 
must be declared in the function that calls this function, and it 
must be a row vector that is the same length as
the fixed population parameter vector $math%pop%$$.
If $italic pdrowLTilde_popOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully,  
then the $code DoubleMatrix$$ pointed to by $italic pdrowLTilde_popOut$$ 
will contain the row vector $math%LTilde_pop(popOut)%$$, which is
the derivative of the population objective function evaluated at 
$math%popOut%$$.
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdrowLTilde_popOut$$.

$syntax/

/pdmatLTilde_pop_popOut/ 
/$$
If $italic pdmatLTilde_pop_popOut$$ is not $code NULL$$, then the 
$code DoubleMatrix$$ pointed to by $italic pdmatLTilde_pop_popOut$$ 
must be declared in the function that calls this function, and it 
must have the same number of rows and columns as the length of
the fixed population parameter vector $math%pop%$$.
If $italic pdmatLTilde_pop_popOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully,  
then the $code DoubleMatrix$$ pointed to by 
$italic pdmatLTilde_pop_popOut$$ will contain the matrix 
$math%LTilde_pop_pop(popOut)%$$, which is an approximation 
for the second derivative of the population objective function 
evaluated at $math%popOut%$$. 
Otherwise, this function will not attempt to change the contents of the 
$code DoubleMatrix$$ pointed to by $italic pdmatLTilde_pop_popOut$$.
The approximation for the second derivative is formed using central
differences of the function $math%LTilde_pop(pop)%$$ with
step sizes specified by $italic dvecPopStep$$.
$syntax/

/isMultiple/
/$$
When this is specified $math%true%$$, $italic SPK$$ runs on the parallel mode, requiring
one or more Nodes to participate in computation.  If this flag is specified $math%false%$$,
$italic SPK$$ runs in the ordinary single-processed mode, minimizing communication overhead
that is otherwise significant.
$syntax/

/sharedDirectory/
/$$
A directory on SPK Parallel Network that has been set to be $bold shared$$ by all
Participants and where Participants have both read and write access.  The string
can be terminated by a path delimiter.

$syntax/

/nodeCommand/
/$$

  $pre
    [ Deprecated! --- Sachiko 10/09/2002 ]
    This argument should be eliminated since this routine is no longer the user
    entry point and the new user point fitPopulation() is taking care 
    of spawning a subprocess as a node.
  $$
A null-terminated exact string that invokes the driver as a node.
When this parameter is given, Master spawns a child process acting as a co-node.
It ensures that Master keeps going even if no remote nodes participates; for
example, the entire network goes down.
The string should represent exactly what is typed to start a node process on the Master machine.  
If NULL is given, Master does not spawn a co-node process.

$head Example$$
The following demonstrates running ppkaOpt() in the parallel mode, 
assuming at least one Node is waiting or will participate.
$codep

#include <iostream>
#include <iomanip>
#include <string>
#include <cmath>
#include <nag.h>
#include <nagg05.h>
#include "ppkaOpt.h"
#include "namespace_population_analysis.h"
#include "identity.h"
#include "pi.h"
#include "SpkModel.h"
#include "File.h"
#include "allZero.h"
#include "Optimizer.h"

using std::string;

static DoubleMatrix funF    ( const DoubleMatrix &alp, 
                            const DoubleMatrix &b );
static DoubleMatrix funF_alp( const DoubleMatrix &dvecF,   
                            const DoubleMatrix &alp, 
                            const DoubleMatrix &b );
static DoubleMatrix funF_b  ( const DoubleMatrix &dvecF, 
                            const DoubleMatrix &alp,   
                            const DoubleMatrix &b );
static DoubleMatrix funR    ( const DoubleMatrix &alp, 
                            const DoubleMatrix &b );
static DoubleMatrix funR_alp( const DoubleMatrix &dmatR,   
                            const DoubleMatrix &alp, 
                            const DoubleMatrix &b );
static DoubleMatrix funR_b  ( const DoubleMatrix &dmatR, 
                            const DoubleMatrix &alp,   
                            const DoubleMatrix &b );
static DoubleMatrix funD    ( const DoubleMatrix &alp );
static DoubleMatrix funD_alp( const DoubleMatrix &dmatD,   
                            const DoubleMatrix &alp );

class PopModel : public SpkModel
{
    DoubleMatrix _a, _b;
    int _i;

public:
    PopModel(){}
    ~PopModel(){}
protected:
    void doSelectIndividual(int i)
    {
        _i = i;
    }
    void doSetPopPar(const valarray<double>& alp )
    {
        _a = DoubleMatrix( alp, 1 );
    }
    void doSetIndPar(const valarray<double>& b )
    {
        _b = DoubleMatrix( b, 1 );
    }
    void doDataMean( valarray<double>& ret ) const
    {
        ret = funF(_a, _b).toValarray();
    }
    bool doDataMean_popPar( valarray<double>& ret ) const
    {
        doDataMean(ret);
        ret = funF_alp(ret, _a, _b).toValarray();
    }
    bool doDataMean_indPar( valarray<double>& ret ) const
    {
        doDataMean(ret);
        ret = funF_b(ret, _a, _b).toValarray();
    }
    void doDataVariance( valarray<double>& ret ) const
    {
        ret = funR(_a, _b);
    }
    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
        doDataVariance(ret);
        ret = funR_alp(ret, _a, _b).toValarray();
    }
    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
        doDataVariance(ret);
        ret = funR_b(ret, _a, _b).toValarray();
    }
    void doIndParVariance( valarray<double>& ret ) const
    {
        ret = funD(_a);
    }
    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
        doIndParVariance(ret);
        ret = funD_alp(ret, _a).toValarray();
    }
};
//--------------------------------------------------------------
//
// Function: main
//
//--------------------------------------------------------------

void main()
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;
  int i;


  //------------------------------------------------------------
  // Quantities related to the user-provided model.
  //------------------------------------------------------------

  PopModel model;


  //------------------------------------------------------------
  // Quantities that define the problem.
  //------------------------------------------------------------

  // Mean and variance of the true transfer rate, betaTrue.
  double meanBetaTrue = 1.0;
  double varBetaTrue  = 5.0;

  // Number of individuals.
  int nB = 10;


  //------------------------------------------------------------
  // Quantities related to the data vector, y.
  //------------------------------------------------------------

  // Number of measurements.
  int nY = nB;

  // Measurement values, y.
  DoubleMatrix dvecY( nY, 1 );
  double* pdYData = dvecY.data();

  // Number of measurements for each individual. 
  DoubleMatrix dvecN( nB, 1 );
  dvecN.fill( (double) 1 );

  // These will hold the generated values for the true measurement 
  // noise, eTrue, and the true random population parameters, bTrue.
  double eTrue;
  double bTrue;

  // Mean, variance, and standard deviation of eTrue and bTrue.
  double meanETrue = 0.0;
  double varETrue  = 1.0;
  double sdETrue   = sqrt( varETrue );
  double meanBTrue = 0.0;
  double varBTrue  = varBetaTrue;
  double sdBTrue   = sqrt( varBTrue );

  // Compute the measurements for each individual.
  Integer seed = 0;
  g05cbc(seed);
  for ( i = 0; i < nB; i++ )
  {
    eTrue = nag_random_normal( meanETrue, sdETrue );
    bTrue = nag_random_normal( meanBTrue, sdBTrue );

    pdYData[ i ] = meanBetaTrue + bTrue + eTrue;
  }


  //------------------------------------------------------------
  // Quantities related to the fixed population parameter, alp.
  //------------------------------------------------------------

  int nAlp = 2;

  DoubleMatrix alpTrue( nAlp, 1 );
  DoubleMatrix alpLow ( nAlp, 1 );
  DoubleMatrix alpUp  ( nAlp, 1 );
  DoubleMatrix alpIn  ( nAlp, 1 );
  DoubleMatrix alpOut ( nAlp, 1 );
  DoubleMatrix alpStep( nAlp, 1 );

  double* pdAlpTrueData = alpTrue.data();
  double* pdAlpLowData  = alpLow .data();
  double* pdAlpUpData   = alpUp  .data();
  double* pdAlpInData   = alpIn  .data();
  double* pdAlpOutData  = alpOut .data();
  double* pdAlpStepData = alpStep.data();

  // Set the values associated with alp(1).
  pdAlpTrueData[ 0 ] = meanBetaTrue;
  pdAlpLowData [ 0 ] = -10.0;
  pdAlpUpData  [ 0 ] = 10.0;
  pdAlpInData  [ 0 ] = -1.0;
  pdAlpStepData[ 0 ] = 1.0e-2;

  // Set the values associated with alp(2).
  pdAlpTrueData[ 1 ] = varBetaTrue;
  pdAlpLowData [ 1 ] = 1.0e-3;
  pdAlpUpData  [ 1 ] = 100.0;
  pdAlpInData  [ 1 ] = 0.5;
  pdAlpStepData[ 1 ] = 1.0e-2;
  

  //------------------------------------------------------------
  // Quantities related to the random population parameters, b.
  //------------------------------------------------------------

  int nB = 1;

  DoubleMatrix bLow ( nB, 1 );
  DoubleMatrix bUp  ( nB, 1 );
  DoubleMatrix bStep( nB, 1 );

  bLow .fill( -1.5e+1 );
  bUp  .fill( +1.0e+1 );
  bStep.fill(  1.0e-2 );

  DoubleMatrix dmatBIn ( nB, nB );
  DoubleMatrix dmatBOut( nB, nB );

  dmatBIn.fill( 1.0 );


  //------------------------------------------------------------
  // Quantities related to the population objective function.
  //------------------------------------------------------------

  double dLTildeOut;

  DoubleMatrix drowLTilde_alpOut    ( 1,    nAlp );
  DoubleMatrix dmatLTilde_alp_alpOut( nAlp, nAlp );


  //------------------------------------------------------------
  // Remaining inputs to ppkaOpt.
  //------------------------------------------------------------

  enum Objective objective = MODIFIED_LAPLACE;

  Optimizer popOptInfo( 1.0e-6, 40, 0 );
  Optimizer indOptInfo( 1.0e-6, 40, 0 );


  //------------------------------------------------------------
  // Optimize the population objective function in the parallel mode.
  //------------------------------------------------------------
  
  // Set a flag requesting parallel mode
  bool isMultiple = true;

  // Set a directory mapped to S: drive on the machine called holden 
  File sharedDirectory("holden\\\\S:\\", "");

  // Set the char array to NULL so that Mater does not spawn a child process.
  char * nodeCommand = NULL; 

  try{
    ppkaOpt(  
              model,
              objective,
              dvecN,
              dvecY,
              popOptInfo,
              alpLow,
              alpUp,
              alpIn,
              &alpOut,
              alpStep,
              indOptInfo,
              bLow,
              bUp,
              dmatBIn,
              &dmatBOut,
              bStep,
              &dLTildeOut,
              &drowLTilde_alpOut,
              &dmatLTilde_alp_alpOut,
              isMultiple,
              sharedDirectory,
              nodeCommand);
  }
  catch(...)
  {
    cerr << "ppkaOpt failed" << endl;
    abort();
  }


  //------------------------------------------------------------
  // Print the results.
  //------------------------------------------------------------

  cout << endl;

  cout << "alpOut = " << endl;
  alpOut.print(); 
  cout << endl;

  cout << "bOut = " << endl;
  dmatBOut.print(); 
  cout << endl;

  cout << "LTildeOut   = " << dLTildeOut << endl;
  cout << endl;

  cout << "LTilde_alpOut  = " << endl;
  drowLTilde_alpOut.print();
  cout << endl;

  cout << "LTilde_alp_alpOut  = " << endl;
  dmatLTilde_alp_alpOut.print();
  cout << endl;
}


//--------------------------------------------------------------
//
// Function: funF
//
//
// Calculates
//
//                 /                   \ 
//     f(alp, b) = |  alp(1) + b(1)  |  .
//                 \                  / 
//
//--------------------------------------------------------------

static DoubleMatrix funF( const DoubleMatrix &alp, 
                                          const DoubleMatrix &b )
{
  DoubleMatrix dvecF( 1, 1 );

  double* pdAlpData = alp.data();
  double* pdBData   = b  .data();

  dvecF.fill( pdAlpData[ 0 ] + pdBData[ 0 ] );

  return dvecF;
}


//--------------------------------------------------------------
//
// Function: funF_alp
//
//
// Calculates
//
//                     /           \ 
//     f_alp(alp, b) = |  1     0  |  .
//                     \           / 
//
//--------------------------------------------------------------

static DoubleMatrix funF_alp( const DoubleMatrix &dvecF, 
                                              const DoubleMatrix &alp, 
                                              const DoubleMatrix &b )
{
  DoubleMatrix drowF_alp( 1, 2 );

  double* pdF_alpData = drowF_alp.data();

  pdF_alpData[ 0 ] = 1.0;
  pdF_alpData[ 1 ] = 0.0;

  return drowF_alp;
}


//--------------------------------------------------------------
//
// Function: funF_b
//
//
// Calculates
//
//                   /     \ 
//     f_b(alp, b) = |  1  |  .
//                   \     / 
//
//--------------------------------------------------------------

static DoubleMatrix funF_b( const DoubleMatrix &dvecF, 
                                            const DoubleMatrix &alp, 
                                            const DoubleMatrix &b )
{
  return identity( 1 );
}


//--------------------------------------------------------------
//
// Function: funR
//
//
// Calculates
//
//                 /     \ 
//     R(alp, b) = |  1  |  .
//                 \     / 
//
//--------------------------------------------------------------

static DoubleMatrix funR( const DoubleMatrix &alp, 
                                          const DoubleMatrix &b )
{
  return identity( 1 );
}


//--------------------------------------------------------------
//
// Function: funR_alp
//
//
// Calculates
//
//                     /           \ 
//     R_alp(alp, b) = |  0     0  |  .
//                     \           / 
//
//--------------------------------------------------------------

static DoubleMatrix funR_alp( const DoubleMatrix &dmatR,   
                                              const DoubleMatrix &alp, 
                                              const DoubleMatrix &b )
{
  DoubleMatrix dmatR_alp( 1, 2 );

  dmatR_alp.fill(0.0);

  return dmatR_alp;
}


//--------------------------------------------------------------
//
// Function: funR_b
//
//
// Calculates
//
//                   /     \ 
//     R_b(alp, b) = |  0  |  .
//                   \     / 
//
//--------------------------------------------------------------

static DoubleMatrix funR_b( const DoubleMatrix &dmatR, 
                                            const DoubleMatrix &alp,   
                                            const DoubleMatrix &b )
{
  DoubleMatrix dmatR_b( 1, 1 );

  dmatR_b.fill(0.0);

  return dmatR_b;
}


//--------------------------------------------------------------
//
// Function: funD
//
//
// Calculates
//
//              /          \ 
//     D(alp) = |  alp(2)  |  .
//              \          / 
//
//--------------------------------------------------------------

static DoubleMatrix funD( const DoubleMatrix &alp )
{
  DoubleMatrix dmatD( 1, 1 );

  double* pdalpData = alp.data();

  dmatD.fill( pdalpData[ 1 ] );

  return dmatD;
}


//--------------------------------------------------------------
//
// Function: funD_alp
//
//
// Calculates
//
//                  /           \ 
//     D_alp(alp) = |  0     1  |  .
//                  \           / 
//
//--------------------------------------------------------------

static DoubleMatrix funD_alp( const DoubleMatrix &dmatD,
                                              const DoubleMatrix &alp )
{
  DoubleMatrix dmatD_alp( 1, 2 );

  double* pdD_alpData = dmatD_alp.data();

  pdD_alpData[ 0 ] = 0.0;
  pdD_alpData[ 1 ] = 1.0;

  return dmatD_alp;
}


$$
then it will display the following when it is run:
$codep

alpOut =
[1.95115]
[3.63406]

bOut =
[1.94171, 0.446611, -0.312347, -0.938796, -3.12919, 2.01348, 2.47441, -1.48642, -1.16138, 0.151919]

LTildeOut   = 21.8566

LTilde_alpOut  =
[-2.767e-007, 1.50158e-007]

LTilde_alp_alpOut  =
[2.15793, 5.97158e-008]
[5.97208e-008, 0.232837]

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
#pragma warning( disable : 4006 )  
#pragma warning( disable : 4786 )  

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

#include <cmath>
#include <cassert>
#include <cerrno>
#include <string>
#include <vector>

#include "firstOrderOpt.h"
#include "ppkaOpt.h"
#include "lTilde.h"
#include "quasiNewtonAnyBox.h"
#include "matabs.h"
#include "isDmatEpsEqual.h"
#include "File.h"
#include "broadCastEndOfSpk.h"
#include "PARALLEL_FILE_CONSTS.h"
#include "SpkException.h"
#include "System.h"
#include "FpErrorChecker.h"
#include "namespace_population_analysis.h"


/*------------------------------------------------------------------------
 * Local class declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  //**********************************************************************
  //
  // Class: PpkaOptObj
  //
  //
  // Evaluates the population objective function and/or its gradient.
  //
  //**********************************************************************

  class PpkaOptObj : public QuasiNewtonAnyBoxObj
  {
    //----------------------------------------------------------
    // Constructors.
    //----------------------------------------------------------

  public:
    PpkaOptObj( 
      bool                 isLTildeBestSetIn,
      const DoubleMatrix&  dvecAlpBestIn,
      const DoubleMatrix&  dmatBBestIn,
      const File*          pSharedDirectoryIn,
      SpkModel*            pModelIn,
      enum  Objective      objectiveIn,
      const DoubleMatrix*  pdvecNIn,
      const DoubleMatrix*  pdvecYIn,
      const DoubleMatrix*  pdvecBLowIn,
      const DoubleMatrix*  pdvecBUpIn,
      const DoubleMatrix*  pdvecBStepIn,
      Optimizer*           pIndOptInfoIn,
      bool                 isMultiProcessedIn )
      :
      nInd              ( dmatBBestIn.nc() ),
      nAlp              ( dvecAlpBestIn.nr() ),
      nB                ( dmatBBestIn.nr() ),
      isLTildeBestSet   ( isLTildeBestSetIn ),  
      dvecAlpBest       ( dvecAlpBestIn ),      
      dmatBBest         ( dmatBBestIn ),        
      pSharedDirectory  ( pSharedDirectoryIn ), 
      pModel            ( pModelIn ),           
      objective         ( objectiveIn ),        
      pdvecN            ( pdvecNIn ),           
      pdvecY            ( pdvecYIn ),           
      pdvecBLow         ( pdvecBLowIn ),        
      pdvecBUp          ( pdvecBUpIn ),         
      pdvecBStep        ( pdvecBStepIn ),       
      pIndOptInfo       ( pIndOptInfoIn ),      
      isMultiProcessed  ( isMultiProcessedIn ) 
    {
    }


    //----------------------------------------------------------
    // Data members.
    //----------------------------------------------------------

  private:
    const int nInd;
    const int nAlp;
    const int nB;

    DoubleMatrix dvecAlpCurr;

    // These hold the best value for the population objective 
    // function that has been computed by lTilde() so far, along
    // with the corresponding set of b values.
    double            dLTildeBest;
    bool              isLTildeBestSet;
    DoubleMatrix      dvecAlpBest;
    DoubleMatrix      dmatBBest;

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    // [Revisit - Caching Individual Optimization State Information - Mitch]
    // Note that optimizer QuasiNewton01Box, which is currently being
    // used by SPK, asks the objective function object to evaluate its
    // function value and then, in a separate call, asks it to evaluate
    // its gradient.  If this class stored the optimizer state information
    // for all of the individuals in the population, e.g.
    //
    //     vector<StateInfo> indOptStateInfoBest;
    //
    // then when the gradient of the population objective function is
    // evaluated at the same alpha value as the function was evaluate,
    // then the individual level optimization problems would not take
    // any time since they have all of the optimization information
    // for each of the individuals.  Subsequent individual level
    // optimizations would probably be faster, too, since they would
    // have good approximations for the Hessians of the individual level
    // objectives.
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    // These point to ppkaOptParallel inputs.
    const File*          pSharedDirectory;
    SpkModel*            pModel;
    enum  Objective      objective;
    const DoubleMatrix*  pdvecN;
    const DoubleMatrix*  pdvecY;
    const DoubleMatrix*  pdvecBLow;
    const DoubleMatrix*  pdvecBUp;
    const DoubleMatrix*  pdvecBStep;
    
    Optimizer*  pIndOptInfo;
    bool        isMultiProcessed;


    //----------------------------------------------------------
    // Public helper functions.
    //----------------------------------------------------------

  public:
    DoubleMatrix getAlpBest() const
    {
      return dvecAlpBest;
    }

    DoubleMatrix getBBest() const
    {
      return dmatBBest;
    }


    //----------------------------------------------------------
    // Functions required by quasiNewtonAnyBox.
    //----------------------------------------------------------

  public:
    //**********************************************************
    // 
    // Function: function
    //
    //
    // Evaluates the population objective function LTilde(alp).
    //
    //**********************************************************

    void function( const DoubleMatrix& dvecAlpIn, double* pdLTildeOut )
    {
      //----------------------------------------------------------
      // Preliminaries.
      //----------------------------------------------------------

      // Set the current value for alp.
      dvecAlpCurr = dvecAlpIn;
      assert( dvecAlpIn.nr() == nAlp );
      assert( dvecAlpIn.nc() == 1 );

    
      //----------------------------------------------------------
      // Evaluate the population objective function.
      //----------------------------------------------------------

      // Use the best matrix of b values as the initial guess for b.
      DoubleMatrix dmatBCurr( nB, nInd );
      double dLTildeCurr = 0.0;
      DoubleMatrix* pdmatNull = 0;
      lTilde(
        isMultiProcessed,
	*pSharedDirectory,
	*pModel,
	objective,
	*pdvecY,
	*pdvecN,
	*pIndOptInfo,
	dvecAlpCurr,
	*pdvecBLow,
	*pdvecBUp,
	*pdvecBStep,
	dmatBBest,
	&dmatBCurr,
	&dLTildeCurr,
	pdmatNull );
    
      
      //----------------------------------------------------------
      // Finish up.
      //----------------------------------------------------------
    
      // Save the best matrix of b values that has been determined so far. 
      if ( !isLTildeBestSet )
      {
        dLTildeBest     = dLTildeCurr;
        isLTildeBestSet = true;
        dvecAlpBest     = dvecAlpCurr;
        dmatBBest       = dmatBCurr;
      }
      else if ( dLTildeCurr < dLTildeBest )
      {
        dLTildeBest     = dLTildeCurr;
        dvecAlpBest     = dvecAlpCurr;
        dmatBBest       = dmatBCurr;
      }
    
      // Set the objective function value.
      *pdLTildeOut = dLTildeCurr;
    }


    //**********************************************************
    // 
    // Function: gradient
    //
    //
    // Evaluate the gradient of the population objective function 
    // LTilde_alp(alp).
    //
    //**********************************************************

    virtual void gradient( DoubleMatrix* pdrowLTilde_alpOut ) const
    {
      //----------------------------------------------------------
      // Preliminaries.
      //----------------------------------------------------------

      assert( pdrowLTilde_alpOut->nr() == 1 );
      assert( pdrowLTilde_alpOut->nc() == nAlp );

    
      //----------------------------------------------------------
      // Evaluate the gradient of the population objective function.
      //----------------------------------------------------------

      // Use the best matrix of b values as the initial guess for b.
      DoubleMatrix dmatBCurr( nB, nInd );
      double* pdNull = 0;
      double dLTildeCurr = 0.0;
      DoubleMatrix drowLTilde_alpCurr( 1, nAlp );
      lTilde(
        isMultiProcessed,
	*pSharedDirectory,
	*pModel,
	objective,
	*pdvecY,
	*pdvecN,
	*pIndOptInfo,
	dvecAlpCurr,
	*pdvecBLow,
	*pdvecBUp,
	*pdvecBStep,
	dmatBBest,
	&dmatBCurr,
        pdNull,
        &drowLTilde_alpCurr );
    
      
      //----------------------------------------------------------
      // Finish up.
      //----------------------------------------------------------
    
      // Set the gradient value.
      *pdrowLTilde_alpOut = drowLTilde_alpCurr;
    }

  };

} // [End: unnamed namespace]


/*------------------------------------------------------------------------
 * Temporary declarations that should be removed.
 *------------------------------------------------------------------------*/

// [Revisit - removal of trancendiff from ppkaOptParallel - Mitch: All of the stuff 
// related to trancendiff should be removed from this function and located 
// in a separate file.  It is located in this function for the moment since 
// there is no generic trancendiff that works with arbitray functions and
// thus ppkaOptParallel is the only routine using the specific version of trancendiff
// that is located here.
//
namespace ltildetrancendiff{

  typedef void (* LTILDE_PROTOTYPE)(
                  bool               isMultiple,
                  const File         &sharedDirectory,
                  SpkModel           &model,
                  enum Objective     whichObjective,
                  const DoubleMatrix &dvecY_forAll,
                  const DoubleMatrix &dvecNumsOfDataforEachSubject,
                  Optimizer&         optInfo,
                  const DoubleMatrix &dvecAlp,
                  const DoubleMatrix &dvecBlow,
                  const DoubleMatrix &dvecBup,
                  const DoubleMatrix &dvecBstep,
                  const DoubleMatrix &dmatBin_forAll,
                  DoubleMatrix       *dmatBout,
                  double             *dLTildeOut,
                  DoubleMatrix       *drowLTilde_alpOut
          );

  static DoubleMatrix trancendiff(
          LTILDE_PROTOTYPE,
          bool               isMultiple,
          const File         &sharedDirectory,
          SpkModel           &model,
          enum  Objective    whichObjective,
          const DoubleMatrix &dvecY_forAll,
          const DoubleMatrix &dvecNumsOfDataforEachSubject,
          Optimizer&         optInfo,
          const DoubleMatrix &dvecAlp,
          const DoubleMatrix &dvecAlpStep,
          const DoubleMatrix &dvecBlow,
          const DoubleMatrix &dvecBup,
          const DoubleMatrix &dvecBstep,
          const DoubleMatrix &dmatBin_forAll,
          int                withRespectToX,
          DoubleMatrix       *dmatLTilde_alp_xOut
                           );

}

using namespace ltildetrancendiff;


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/
void ppkaOpt( 
              SpkModel&               model,
              enum  Objective         objective,
              const DoubleMatrix&     dvecN,
              const DoubleMatrix&     dvecY,
              Optimizer&              popOptInfo,
              const DoubleMatrix&     dvecAlpLow,
              const DoubleMatrix&     dvecAlpUp,
              const DoubleMatrix&     dvecAlpIn,
              DoubleMatrix*           pdvecAlpOut,
              const DoubleMatrix&     dvecAlpStep,
              Optimizer&              indOptInfo,
              const DoubleMatrix&     dvecBLow,
              const DoubleMatrix&     dvecBUp,
              const DoubleMatrix&     dmatBIn,
              DoubleMatrix*           pdmatBOut,
              const DoubleMatrix&     dvecBStep,
              double*                 pdLTildeOut,
              DoubleMatrix*           pdrowLTilde_alpOut,
              DoubleMatrix*           pdmatLTilde_alp_alpOut,
              bool                    isMultiple,
              const char*             c_sharedDirectory,
              const char*             nodeCommand
              )
{

    
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------
  using std::endl;
  using std::cerr;
  using std::cout;

  // Return if there are no output values to compute.
  if( pdvecAlpOut            == 0 && 
      pdmatBOut              == 0 && 
      pdLTildeOut            == 0 && 
      pdrowLTilde_alpOut     == 0 && 
      pdmatLTilde_alp_alpOut == 0    )
  {
    return;
  }

  int nInd = dvecN    .nr();
  int nAlp = dvecAlpIn.nr();
  int nB   = dmatBIn  .nr();

  //------------------------------------------------------------
  // Set indOptInfo as a sub-level optimizer. 
  //------------------------------------------------------------

  bool oldIndSaveState  = indOptInfo.getSaveStateAtEndOfOpt();
  bool oldIndThrowExcep = indOptInfo.getThrowExcepIfMaxIter();

  // Set these flags so that an exception is thrown if the maximum number
  // of iterations is exceeded when optimizing an individual and so that
  // no individual level optimizer state information is saved.
  indOptInfo.setSaveStateAtEndOfOpt( false );
  indOptInfo.setThrowExcepIfMaxIter( true);

  //------------------------------------------------------------
  // Convert the c string specifying the communication
  // directory to a File object which is the type expected
  // by lTilde().
  // Set the directory if the user has requested parallel 
  // execution.  Otherwise, leave it unset (empty).
  //------------------------------------------------------------
  File sharedDirectory;
  if( isMultiple )
  {
    sharedDirectory.setPath(c_sharedDirectory);
  }

  //------------------------------------------------------------
  // Prepare the objects to hold the output values.
  //------------------------------------------------------------

  // Instantiate a temporary column vector to hold the final alp 
  // value that will be returned by quasiNewtonAnyBox.
  DoubleMatrix dvecAlpOutTemp( nAlp, 1 );

  // Instantiate a temporary matrix to hold the optimal b values
  // for each individual.
  DoubleMatrix dmatBOutTemp( nB, nInd );

  // If this function is going to return the population objective  
  // function value, initialize the temporary value to hold it.  
  // Otherwise, set the temporary pointer to zero so that quasiNewtonAnyBox 
  // will not return it either.
  double dLTildeOutTemp;
  double* pdLTildeOutTemp = &dLTildeOutTemp;
  if ( pdLTildeOut )
  {
    dLTildeOutTemp = 0.0;
  }
  else
  {
    pdLTildeOutTemp = 0;
  }

  // If this function is going to return the derivative of the 
  // population objective function with respect to alp, instantiate 
  // a temporary row vector to hold it.  Otherwise, set the temporary 
  // pointer to zero so that lTilde will not return it either.
  DoubleMatrix drowLTilde_alpOutTemp;
  DoubleMatrix* pdrowLTilde_alpOutTemp = &drowLTilde_alpOutTemp;
  if ( pdrowLTilde_alpOut )
  {
    drowLTilde_alpOutTemp.resize( 1, nAlp );
  }
  else
  {
    pdrowLTilde_alpOutTemp = 0;
  }
  
  // If this function is going to return the second derivative of 
  // the population objective function with respect to alp, instantiate 
  // a temporary matrix to hold it.  Otherwise, set the temporary 
  // pointer to zero so that lTilde will not return it either.
  DoubleMatrix dmatLTilde_alp_alpOutTemp;
  DoubleMatrix* pdmatLTilde_alp_alpOutTemp;
 
  if ( pdmatLTilde_alp_alpOut )
  {
    dmatLTilde_alp_alpOutTemp.resize( nAlp, nAlp );
    pdmatLTilde_alp_alpOutTemp = &dmatLTilde_alp_alpOutTemp;

  }
  else
  {
    pdmatLTilde_alp_alpOutTemp = 0;
  }

  //------------------------------------------------------------
  // Prepare the population objective function.
  //------------------------------------------------------------

  bool isLTildeBestSet = false;

  // Construct the objective function, providing it with initial guesses
  // for the best values for alp and for the best matrix of individual b
  // values, with pointers to the ppkaOptParallel inputs, and with
  // miscellaneous other information.
  PpkaOptObj ppkaOptObj( isLTildeBestSet,
			 dvecAlpIn,
			 dmatBIn,
			 &sharedDirectory,
			 &model,
			 objective,
			 &dvecN,
			 &dvecY, 
			 &dvecBLow,
			 &dvecBUp,
			 &dvecBStep,
			 &indOptInfo,
			 isMultiple );


  //------------------------------------------------------------
  // Handle nonzero iterations for the population objective function.
  //------------------------------------------------------------

  if ( popOptInfo.getNMaxIter() > 0 )
  {
      // If the number of iterations is not zero, then the population
      // objective function must be optimized in order to determine alpOut.
      // Note that the best matrix of b values for each individual that 
      // has been found so far is cached in indLevelInfo.
      try{
          quasiNewtonAnyBox( ppkaOptObj,
			     popOptInfo, 
			     dvecAlpLow, 
			     dvecAlpUp, 
			     dvecAlpIn, 
			     &dvecAlpOutTemp, 
			     pdLTildeOutTemp,
			     pdrowLTilde_alpOutTemp ); 
      }
      catch( SpkException& e )
      {
//        if(isMultiple)
//            broadCastEndOfSpk(sharedDirectory);
        throw e.push(
          SpkError::SPK_OPT_ERR,
          "Population level optimization failed.", 
          __LINE__, __FILE__);
      }
      catch( const std::exception& e )
      {
        if(isMultiple)
            broadCastEndOfSpk(sharedDirectory);
        throw SpkException(e,
          "A standard exception was thrown during the population level optimization.", 
          __LINE__, __FILE__);
      }
      catch( ... )
      {
        if(isMultiple)
            broadCastEndOfSpk(sharedDirectory);
        throw SpkException(SpkError::SPK_UNKNOWN_OPT_ERR,
          "An exception of unknown type was thrown during the population level optimization.", 
          __LINE__, __FILE__);
      }
      
      // If the best alp value stored in indLevelInfo isn't the same 
      // as the one in alpOutTemp, then the matrix of b values for 
      // each individual must be recalculated.
      bool recalcBBest = !isDmatEpsEqual( dvecAlpOutTemp, 
                                          ppkaOptObj.getAlpBest(),
                                          matabs( dvecAlpOutTemp ) );
    
      // If the matrix of b values for each 
      // individual needs to be recalculated, then compute them both 
      // with a single call to lTilde.
      if ( recalcBBest && !popOptInfo.getIsTooManyIter() )
      {
          double* pdNull = 0;
      
          // [Revisit - Caching Previous Values - Mitch]:  the following call  
          // to lTilde() uses values for alp and bIn that are the optimal values
          // found during the optimization over alp, which was carried out
          // by quasiNewtonAnyBox().  Thus, it asks lTilde() to evaluate the population
          // objective at alp values which were just determined to be the
          // solution.  This will make for unneccesary recalculations of
          // the population and individual objective functions.  If lTilde() 
          // and mapTilde() somehow cached the values they calculated for 
          // previous values of the parameters, then it would prevent this
          // extra work.
          lTilde( isMultiple,
                  sharedDirectory,
                  model,
                  objective,
                  dvecY,
                  dvecN,
                  indOptInfo,
                  dvecAlpOutTemp,
                  dvecBLow,
                  dvecBUp,
                  dvecBStep,
                  ppkaOptObj.getBBest(),
                  &dmatBOutTemp,
                  pdNull,
                  0 );
      }
      else
      {
        // Copy the b values that were computed during the call to 
        // quasiNewtonAnyBox into the temporary matrix.
        dmatBOutTemp = ppkaOptObj.getBBest();
      }
    }
  //------------------------------------------------------------
  // Handle zero iterations for the population objective function.
  //------------------------------------------------------------

  if ( popOptInfo.getNMaxIter() == 0 )
  {
    // If the number of iterations is zero, then alpIn is the
    // desired value for alpOut.
    dvecAlpOutTemp = dvecAlpIn;
    
    // The individual objective functions must still be computed
    // in order to evaluate lTilde(alpOut).
    lTilde( isMultiple,
          sharedDirectory,
          model,
          objective,
          dvecY,
          dvecN,
          indOptInfo,
          dvecAlpOutTemp,
          dvecBLow,
          dvecBUp,
          dvecBStep,
          dmatBIn,
          &dmatBOutTemp,
          pdLTildeOutTemp,
          pdrowLTilde_alpOutTemp );
  }
  
  //------------------------------------------------------------
  // Compute the second derivative of the population objective function.
  //------------------------------------------------------------

  // Compute the second derivative of the population objective 
  // function at the final alp value, if necessary.
  if ( pdmatLTilde_alp_alpOut && !popOptInfo.getIsTooManyIter() )
  {
    // [Revisit - trancendiff for LTilde_alp_alp - Mitch:  The version
    // of trancendiff that is currently located in this file and that 
    // computes LTilde_alp_alp returns a DoubleMatrix that contains
    // the value for LTilde_alp_alp.  The versions of trancendiff for
    // other objectives, e.g. mapObjDiff, put the value for the matrix
    // of second derivatives in LTilde_alp_alpOut and the return value
    // for the functions is a bool that indicates success or failure.
    // As trancediff is currently written, there is no way to check
    // for a successful completion of trancendiff before copying the
    // matrix containing LTilde_alp_alp into LTilde_alp_alpOutTemp.
    //
    // [Comment - Sachiko]
    // The new version of trancendiff located in this file 
    // now throws an exception if something goes wrong.
    //
      try{
          dmatLTilde_alp_alpOutTemp = trancendiff(  &lTilde,
                                              isMultiple,
                                              sharedDirectory,
                                              model,
                                              objective,
                                              dvecY,
                                              dvecN,
                                              indOptInfo,
                                              dvecAlpOutTemp,
                                              dvecAlpStep,
                                              dvecBLow,
                                              dvecBUp,
                                              dvecBStep,
                                              dmatBOutTemp,
                                              population_analysis::WITH_RESPECT_TO_ALP,
                                              pdmatLTilde_alp_alpOutTemp );
      }
      catch( SpkException& e )
      {
          //
          // Revisit - Sachiko:
          //
          // This should dump all the parameter values to a file and 
          // give the filename as an error message.
          //
//        if(isMultiple)
//          broadCastEndOfSpk(sharedDirectory);

        throw e.push(
            SpkError::SPK_DIFF_ERR, 
            "An attempt to approximate the second derivative of lTilde with respect to alp failed.",
            __LINE__, __FILE__);
      }
      catch( const std::exception& e )
      {
          //
          // Revisit - Sachiko:
          //
          // This should dump all the parameter values to a file and 
          // give the filename as an error message.
          //
//        if(isMultiple)
//          broadCastEndOfSpk(sharedDirectory);

        throw SpkException(e, 
              "A standard exception was thrown during an attempt to approximate the second derivative of lTilde with respect to alp.",
              __LINE__, __FILE__);
      }
      catch( ... )
      {
          //
          // Revisit - Sachiko:
          //
          // This should dump all the parameter values to a file and 
          // give the filename as an error message.
          //
//        if(isMultiple)
//          broadCastEndOfSpk(sharedDirectory);

        throw SpkException(
            SpkError::SPK_DIFF_ERR,  
              "Unknown exception was thrown during an attempt to approximate the second derivative of lTilde with respect to alp",
              __LINE__, __FILE__);
      }
  }


  //------------------------------------------------------------
  // Set the values to be returned.
  //------------------------------------------------------------

  // Set the final alp value, if necessary.
  if ( pdvecAlpOut && !popOptInfo.getIsTooManyIter() )
  {
        *pdvecAlpOut = dvecAlpOutTemp;
  }

  // Set the matrix of final b values, if necessary.
  if ( pdmatBOut && !popOptInfo.getIsTooManyIter() )
  {
    *pdmatBOut = dmatBOutTemp;
  }

  // Set the final population objective function value, if necessary.
  if ( pdLTildeOut && !popOptInfo.getIsTooManyIter() )
  {
        *pdLTildeOut = dLTildeOutTemp;
  }

  // Set the first derivative of the population objective 
  // function at the final alp value, if necessary.
  if ( pdrowLTilde_alpOut && !popOptInfo.getIsTooManyIter() )
  {
    *pdrowLTilde_alpOut = drowLTilde_alpOutTemp;
  }
    
  // Set the second derivative of the population objective 
  // function at the final alp value, if necessary.
  if ( pdmatLTilde_alp_alpOut && !popOptInfo.getIsTooManyIter() )
  {
    *pdmatLTilde_alp_alpOut = dmatLTilde_alp_alpOutTemp;
  }

/*
  if(isMultiple)
  {
      // Notify all Nodes with the end of ppkaOpt
      broadCastEndOfSpk(sharedDirectory);
  }
*/

  // Reset these individual optimizer flags to their original values.
  indOptInfo.setSaveStateAtEndOfOpt( oldIndSaveState );
  indOptInfo.setThrowExcepIfMaxIter( oldIndThrowExcep );

  return;
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
 * Function: trancendiff for L_alp
 *
 *                                                           
 * Author: Sachiko Honda
 *
 *
 * The central difference approximation with respect to X (alp or B)
 * of the true derivative of L with respect to alp.
 *
 * This function simply propagate exceptions.
 *
 *************************************************************************/
#include <string>
#include "subtract.h"
#include "add.h"
#include "divByScalar.h"
#include "getRow.h"
#include "namespace_population_analysis.h"
#include "DoubleMatrix.h"
#include "replaceIth.h"
#include "Objective.h"

static DoubleMatrix ltildetrancendiff::trancendiff(
        LTILDE_PROTOTYPE pfLTilde,
        bool               isMultiple,
        const File         &sharedDirectory,
        SpkModel           &model,
        enum Objective     whichObjective,
        const DoubleMatrix &dvecY_forAll,
        const DoubleMatrix &dvecNumsOfDataforEachSubject,
        Optimizer&         indOptInfo,
        const DoubleMatrix &dvecAlp,
        const DoubleMatrix &dvecAlpStep,
        const DoubleMatrix &dvecBlow,
        const DoubleMatrix &dvecBup,
        const DoubleMatrix &dvecBstep,
        const DoubleMatrix &dmatBin_forAll,
        int                withRespectToX,
        DoubleMatrix       *dmatLTilde_alp_xOut
                         )
{
    using namespace std;
    using namespace population_analysis;

    assert( dvecAlp.nr()     == dvecAlpStep.nr() );
    assert( dvecAlp.nc()     == 1 );
    assert( dvecAlpStep.nc() == 1 );

    DoubleMatrix dmatJ; // the final matrix that'll be returned
    int m;              // the number of variable's(alp/B) elements

    // Return control immediately if nothing is requested.
    if( dmatLTilde_alp_xOut == 0 )
        return dmatJ;

    // Choose the variable from either alp or B
    if( withRespectToX == population_analysis::WITH_RESPECT_TO_ALP )
        m = dvecAlpStep.nr();
    else if( withRespectToX == population_analysis::WITH_RESPECT_TO_B )
        m = dvecBstep.nr();
    else{
        cerr << "lTilde.trancendiff: Invalid argument ";
        cerr << "(population_analysis::WITH_RESPECT_TO_ALP / population_analysis::WITH_RESPECT_TO_B)" << endl;
        abort();
    }

    // Grab resources
    DoubleMatrix drowTempLTilde_alp(1,m);
    DoubleMatrix drowPlusLTilde_alpOut(1,m);
    DoubleMatrix drowMinusLTilde_alpOut(1,m);
    DoubleMatrix dvecTempAlp(dvecAlp.nr(), dvecAlp.nc());
    DoubleMatrix dmatTempBin_forAll(dmatBin_forAll.nr(), dmatBin_forAll.nc());
    DoubleMatrix dvecXtemp(m,1);
    DoubleMatrix dvecX(m,1);
    DoubleMatrix drowKthStep( 1, 1 );
    
    // Updated 2-7-01 Alyssa
    // fixed for const correctness

    const double *pdStep;
    double *pdXtemp;
    int k;

    // Determine the size of the matrix returned by lTilde.
    //
    // Revisit - Exception - Sachiko
    //
    // This call may throw an exception but let it propagate.
    //
    lTilde( isMultiple, sharedDirectory, model, whichObjective, dvecY_forAll, dvecNumsOfDataforEachSubject,
        indOptInfo,
        dvecAlp, dvecBlow, dvecBup, dvecBstep, dmatBin_forAll,
        0,0,&drowTempLTilde_alp );

    assert( drowTempLTilde_alp.nr() == 1 );
    dmatJ.resize(drowTempLTilde_alp.nc(), m);

    // Choose the variable and step size
    if( withRespectToX == population_analysis::WITH_RESPECT_TO_ALP ){
        dvecX = dvecAlp;
        pdStep = dvecAlpStep.data();
    }
    else{
        dvecX = dmatBin_forAll;
        pdStep = dvecBstep.data();
    }

    // Iterate over the variable (alp/B)
    for( k=0; k<m; k++ ){

        // Compute (variable + step)
        dvecXtemp  = dvecX;
        pdXtemp    = dvecXtemp.data();
        drowKthStep.fill(pdStep[k]);
        replaceIth( dvecXtemp, k, add(getRow(dvecX, k), drowKthStep) );        
        dvecTempAlp = (withRespectToX == population_analysis::WITH_RESPECT_TO_ALP? dvecXtemp : dvecAlp);
        dmatTempBin_forAll = (withRespectToX == population_analysis::WITH_RESPECT_TO_B? dvecXtemp : dmatBin_forAll);

        // get Ltilde_alp for (variable + step)
        //
        // Revisit - Exception - Sachiko
        //
        // This call may throw an exception but let it propagate.
        //
        lTilde( isMultiple, sharedDirectory, model, whichObjective, dvecY_forAll, dvecNumsOfDataforEachSubject,
            indOptInfo, dvecTempAlp, dvecBlow,dvecBup,dvecBstep,dmatTempBin_forAll,
            '\0','\0',&drowPlusLTilde_alpOut );

        // Compute (variable - step )
        dvecXtemp  = dvecX;
        pdXtemp    = dvecXtemp.data();
        drowKthStep.fill(pdStep[k]);
        replaceIth( dvecXtemp, k, subtract(getRow(dvecX, k), drowKthStep) );        
        dvecTempAlp = (withRespectToX == WITH_RESPECT_TO_ALP? dvecXtemp : dvecAlp);
        dmatTempBin_forAll = (withRespectToX == WITH_RESPECT_TO_B? dvecXtemp : dmatBin_forAll);
        
        // get Ltilde_alp for (variable - step)
        //
        // Revisit - Exception - Sachiko
        //
        // This call may throw an exception but let it propagate.
        //
        lTilde( isMultiple, sharedDirectory, model, whichObjective, dvecY_forAll, dvecNumsOfDataforEachSubject,
            indOptInfo, dvecTempAlp, dvecBlow,dvecBup,dvecBstep,dmatTempBin_forAll,
            '\0','\0',&drowMinusLTilde_alpOut );

        // Take difference
        drowTempLTilde_alp = 
            divByScalar( 
                subtract(drowPlusLTilde_alpOut, drowMinusLTilde_alpOut), 
                (2.0*pdStep[k]) );

        // Store the row vector (drowTempLTilde_alp) in the final matrix
        replaceIth(dmatJ, k, drowTempLTilde_alp);
    }

    return dmatJ;
}
