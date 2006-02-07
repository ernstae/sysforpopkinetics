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
 * File: twoStageMethod.cpp
 *
 *
 * Uses one of the two-stage methods to determine the population mean
 * and covariance of the individual parameters.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: twoStageMethod
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin twoStageMethod$$

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
  Ind  
  int  
  iostream  
  iomanip  
  Iter  
  Max  
  namespace  
  Obj  
  pd  
  pdmat  
  pdrow  
  pdvec  
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
  Ri
  valarray
  enum
  enumlator
  Objective method
  Fo
  optimizer
  popOptInfo
  indOptInfo
$$

$section Determining the Population Mean and Covariance of Individual Parameters Using a Two-Stage Method$$

$index twoStageMethod$$
$cindex determining /the population mean /and covariance /of individual parameters /using /a Two-Stage /method$$

$table
$bold Prototype:$$ $cend
$syntax/void twoStageMethod(
              SpkModel&               /model/,
              enum Objective          /method/,
              const DoubleMatrix&     /dvecN/,
              const DoubleMatrix&     /dvecY/,
              Optimizer&              /popOptInfo/,
              Optimizer&              /indOptInfo/,
              const DoubleMatrix&     /dvecBLow/,
              const DoubleMatrix&     /dvecBUp/,
              const DoubleMatrix&     /dmatBIn/,
              DoubleMatrix*           /pdmatBOut/,
              const DoubleMatrix&     /dvecBStep/,
              DoubleMatrix*           /pdvecBMeanOut/,
              DoubleMatrix*           /pdmatBCovOut/ )
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
Uses one of the Two-Stage methods to determine the population mean
and covariance of the individual parameters.
$pre

$$
This function allows the following Two-Stage methods to be used:
$pre

    STANDARD_TWO_STAGE            =  Standard Two-Stage (STS) method,
    ITERATIVE_TWO_STAGE           =  Iterative Two-Stage (ITS) method,
    GLOBAL_TWO_STAGE              =  Global Two-Stage (GTS) method.
    MAP_BAYES_STANDARD_TWO_STAGE  =  Standard Two-Stage (STS) method
                                     with MAP Bayesian objective,
    MAP_BAYES_ITERATIVE_TWO_STAGE =  Iterative Two-Stage (ITS) method
                                     with MAP Bayesian objective, and
    MAP_BAYES_GLOBAL_TWO_STAGE    =  Global Two-Stage (GTS) method 
                                     with MAP Bayesian objective.

$$
For the Standard Two-Stage method (STS), the population mean of
the individuals' parameter estimates is calculated as
$math%

                           nInd
                           ----
         (STS)       1     \    
    bMean       =  ------  /     bOut_i   
                    nInd   ----      
                           i = 1 

$$
and the population covariance of the individuals' estimates is
calculated as
$math%$$

                          nInd
                          ----
        (STS)       1     \                                              T
    bCov       =  ------  /      ( bOut_i  -  bMean ) ( bOut_i  -  bMean )   .
                   nInd   ----          
                          i = 1 

$$


$head Reference$$
A. Schumitzky, EM algorithms and two stage methods in phamacokinetic population analysis.
in $italic Advanced Methods of Pharmacokinetic and Pharmacodynamic Systems Analysis$$, 
edited by D. Z. D'Argenio. New York: Plenum, 1995, p. 145-160.

$head Model Assumptions$$
The following model assumptions are stated using 
$xref/glossary/Population Notation/population notation/$$.
The bar above $math%alp%$$ and $math%b_i%$$ denote the true, but unknown,
values for the population parameters and the individual parameters
for the $th i$$ individual, respectively.
$math%
               ___   ___
    y_i = f_i( alp , b_i ) + e_i
                    ___  ___
    e_i ~ N[0, R_i( alp, b_i)]
    ___          ___
    b_i ~ N[0, D(alp)]
%$$

$head Return Value$$
Upon a successful completion, the function returns normally and set
the given output value place holders if it is able to obtain
acceptable estimates for the population mean and covariance of the
individuals parameters.

If an error is detected or failure occurs during the evaluation, an
SpkException object is thrown.  The state at which an exception is
thrown is defined in $xref/glossary/Exception Handling
Policy/Exception Handling Policy/$$.

$head Arguments$$
$syntax/
/model/
/$$
This function expects $italic model$$ to be a function of
two parameters: $math%b%$$ and $math%i%$$.
Refer $xref/glossary/Model Functions Depend on i - b/Model Functions Depend on i - b/$$
for details.

$syntax/

/method/
/$$
This enumerated type variable specifies which Two-Stage method will be used.
The permissible values for $italic objective$$ are defined in 
the $xref/Objective//Objective/$$ enumerated type definition.

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
This $xref/Optimizer//Optimizer/$$ object contains the information 
that controls the population level optimization process.
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
for the iterative and global two-stage methods.
It must be greater than $math%0.0%$$.

$subhead optInfo.nMaxIter$$
This integer must be greater than or equal to zero.
It specifies the maximum number of iterations for the iterative and 
global Two-Stage methods to attempt before giving up on convergence.
If it is equal to zero, then the initial values for the population mean
and covariance are accepted as the final values.

$subhead optInfo.traceLevel$$
This integer scalar specifies the amount of tracing for the iterative 
and global Two-Stage methods.
If $math%level \ge 1%$$, trace values are directed to standard output 
(stdout).  

$subhead optInfo.nIterCompleted$$
This integer scalar holds the number of iteration that have been 
completed for the iterative and global Two-Stage methods..

$subhead optInfo.isTooManyIter$$
This flag indicates whether the too-many-iteration failure has occurred.  

$subhead optInfo.throwExcepIfMaxIter$$
This flag indicates if the optimizer should throw an exception when
the maximum number of iterations is exhausted.
If this parameter is true, then when
the maximum number of iterations is exhausted, an exception will
be thrown and the output values for this function will not be set.
Otherwise, the calling program will
need to check the parameter isTooManyIter to see if the 
maximum number of iterations was exhausted.

$syntax/

/indOptInfo/
/$$
This $xref/Optimizer//Optimizer/$$ object contains the information 
that controls the individual level optimization process.
$pre

$$
Note that warm starts are not supported for the individual 
level optimization.
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
For a particular value of $math%alp%$$ and for the $math%i%$$-th 
individual in the population, an individual parameter value 
$math%bOut_i%$$ is accepted as an estimate for $math%bHat_i%$$ if 
$math%
        abs( bOut_i - bHat_i ) \le epsilon ( bUp - bLow )  ,
%$$
where $math%abs%$$ is the element-by-element absolute value function
and $math%bHat_i%$$ is a local minimizer of $math%MapObj(b)%$$ 
with respect to $math%b%$$.
Since $math%bHat_i%$$ is unknown, this function estimates the left hand
side of this inequality in a way that is a good approximation when 
the Hessian of the objective function is positive definite.
$pre

$$
Note that if $italic nMaxIter$$ is set to zero, then the $th i$$ 
column of $math%bIn%$$ is accepted as the estimate for 
$math%bHat_i%$$.

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
parameter for the optimizer $xref/QuasiNewton01Box//QuasiNewton01Box/$$.

$subhead optInfo.nIterCompleted$$
This integer scalar holds the number of iteration that have been 
completed in the optimizer.

$subhead optInfo.isTooManyIter$$
This flag indicates whether the too-many-iteration failure has occurred.  

$subhead optInfo.saveStateAtEndOfOpt$$
This flag is not used for the individual level optimization.

$subhead optInfo.throwExcepIfMaxIter$$
This flag is not used for the individual level optimization.

$subhead optInfo.isWarmStartPossible$$
This flag is not used for the individual level optimization.

$subhead optInfo.isWarmStart$$
This flag is not used for the individual level optimization.

$subhead optInfo.stateInfo$$
This $code StateInfo$$ struct is not used for the individual 
level optimization.

$syntax/

/dvecBLow/
/$$
The $code DoubleMatrix$$ $italic dvecBLow$$ contains the column vector 
$math%bLow%$$, which specifies the lower limit for the individual parameters 
during the optimization procedure for all the individuals.
The length of $italic dvecBLow$$ is equal to the length of 
the individual parameter vector $math%b_i%$$.

$syntax/

/dvecBUp/
/$$
The $code DoubleMatrix$$ $italic dvecBUp$$ contains the column vector 
$math%bUp%$$, which specifies the upper limit for the individual parameters 
during the optimization procedure for all the individuals.
The length of $italic dvecBUp$$ is equal to the length of 
the individual parameter vector $math%b_i%$$.

$syntax/

/dmatBIn/
/$$
The $code DoubleMatrix$$ $italic dmatBIn$$ contains the matrix 
$math%bIn%$$.  
The $th i$$ column of $math%bIn%$$ specifies the initial value for 
the individual parameters for the $th i$$ individual.
If $math%ind_i%$$ is any column of $math%bIn%$$,
it is assumed that $math%bLow \le ind_i \le bUp%$$.
The column dimension of $math%bIn%$$ is equal to the number of 
individuals in the population, $math%M%$$.
Note that the number of rows in $italic dmatBIn$$ specifies the 
length of the individual parameter vector $math%b_i%$$.

$syntax/

/pdmatBOut/
/$$
If $italic pdmatBOut$$ is not $code NULL$$, 
then the $code DoubleMatrix$$ pointed to by $italic pdmatBOut$$ must 
be declared in the function that calls this function, 
and it must have the same dimensions as $math%bIn%$$.
If $italic pdmatBOut$$ is not $code NULL$$, 
and if this function completed the optimization successfully,  
then the $code DoubleMatrix$$ pointed to by $italic pdmatBOut$$ will 
contain $math%bOut%$$, which is the matrix of estimates for the true 
minimizers of the individual objective functions.
Otherwise, this function will not attempt to change the contents of 
the $code DoubleMatrix$$ pointed to by $italic pdmatBOut$$.
To be specific, the $th i$$ column of $math%bOut%$$ contains a column
vector that is an estimate for $math%bHat_i%$$, the minimizer 
of $math%MapObj(b)%$$ with respect to $math%b_i%$$. 
The value $math%epsilon(1)%$$ is used for accepting the minimizers with 
respect to the individual parameters.

$syntax/

/dvecBStep/
/$$
The $code DoubleMatrix$$ $italic dvecBStep$$ contains the column vector 
$math%bStep%$$, which specifies the step size used for approximating
the derivatives with respect to the individual parameters.
The length of $italic dvecBStep$$ is equal to the length of 
the individual parameter vector $math%b_i%$$.

$syntax/

/pdvecBMeanOut/
/$$
If $italic pdvecBMeanOut$$ is not $code NULL$$, 
then the $code DoubleMatrix$$ pointed to by $italic pdvecBMeanOut$$ must 
be declared in the function that calls this function, 
and it must have the same length as the individual parameter 
vector $math%b_i%$$.
If $italic pdvecBMeanOut$$ is not $code NULL$$, 
and if this function completed successfully,  
then the $code DoubleMatrix$$ pointed to by $italic pdvecBMeanOut$$ will 
contain $math%bMeanOut%$$, which is a vector containing the 
population mean of the matrix of estimates for the true minimizers of
the individual objective functions, $math%bOut%$$.
Otherwise, this function will not attempt to change the contents of 
the $code DoubleMatrix$$ pointed to by $italic pdvecBMeanOut$$.

$syntax/

/pdmatBCovOut/
/$$
If $italic pdmatBCovOut$$ is not $code NULL$$, 
then the $code DoubleMatrix$$ pointed to by $italic pdmatBCovOut$$ must 
be declared in the function that calls this function, 
and it must have the same dimensions as $math%bIn%$$.
If $italic pdmatBCovOut$$ is not $code NULL$$, 
and if this function completed successfully,  
then the $code DoubleMatrix$$ pointed to by $italic pdmatBCovOut$$ will 
contain $math%bCovOut%$$, which is a matrix containing the 
population covariance of the matrix of estimates for the true minimizers of
the individual objective functions, $math%bOut%$$.
Otherwise, this function will not attempt to change the contents of 
the $code DoubleMatrix$$ pointed to by $italic pdmatBCovOut$$.

$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include "add.h"
#include "divByScalar.h"
#include "getCol.h"
#include "getSubblock.h"
#include "intToOrdinalString.h"
#include "mapOpt.h"
#include "multiply.h"
#include "namespace_population_analysis.h"
#include "replaceJth.h"
#include "SpkException.h"
#include "subtract.h"
#include "transpose.h"
#include "twoStageMethod.h"

// Standard library header files.
#include <cmath>
#include <string>


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void twoStageMethod( SpkModel&            model,
                     enum  Objective      method,
                     const DoubleMatrix&  dvecN,
                     const DoubleMatrix&  dvecY,
                     Optimizer&           popOptInfo,
                     Optimizer&           indOptInfo,
                     const DoubleMatrix&  dvecBLow,
                     const DoubleMatrix&  dvecBUp,
                     const DoubleMatrix&  dmatBIn,
                     DoubleMatrix*        pdmatBOut,
                     const DoubleMatrix&  dvecBStep,
                     DoubleMatrix*        pdvecBMeanOut,
                     DoubleMatrix*        pdmatBCovOut )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  // Return if there are no output values to calculate.
  if( pdmatBOut == 0 || pdvecBMeanOut == 0 || pdmatBCovOut == 0 )
  {
    return;
  }

  const int nInd = dvecN    .nr();
  const int nB   = dmatBIn  .nr();


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
  // Prepare the objects to hold the output values.
  //------------------------------------------------------------

  // If this function is going to return the individual parameter
  // estimates for each individual or the population covariance of the
  // individual parameter estimates for each individual, instantiate a
  // temporary matrix to hold them.  Otherwise, set the temporary
  // pointer to zero.
  DoubleMatrix dmatBOutTemp;
  DoubleMatrix* pdmatBOutTemp;
  if ( pdmatBOut || pdmatBCovOut )
  {
    dmatBOutTemp.resize( nB, nInd );
    pdmatBOutTemp = &dmatBOutTemp;
  }
  else
  {
    pdmatBOutTemp = 0;
  }

  // Since this function always needs to calculate the population mean
  // of the individual parameter estimates for each individual,
  // instantiate a temporary row vector to hold it.
  DoubleMatrix dvecBMeanOutTemp( nB, 1 );
  DoubleMatrix* pdvecBMeanOutTemp = &dvecBMeanOutTemp;
  
  // If this function is going to return the population covariance of the
  // individual parameter estimates for each individual, instantiate a
  // temporary row vector to hold it.  Otherwise, set the temporary
  // pointer to zero.
  DoubleMatrix dmatBCovOutTemp;
  DoubleMatrix* pdmatBCovOutTemp;
  if ( pdmatBCovOut )
  {
    dmatBCovOutTemp.resize( nB, nB );
    pdmatBCovOutTemp = &dmatBCovOutTemp;

  }
  else
  {
    pdmatBCovOutTemp = 0;
  }


  //------------------------------------------------------------
  // Prepare to perform one of the two-stage methods.
  //------------------------------------------------------------

  // Set the flag that indicates if the MAP Bayesian objective
  // function should be used when estimating the individuals'
  // parameters.
  bool withD;
  if ( method == MAP_BAYES_STANDARD_TWO_STAGE ||
       method == MAP_BAYES_ITERATIVE_TWO_STAGE ||
       method == MAP_BAYES_GLOBAL_TWO_STAGE )
  {
    withD = true;
  }
  else
  {
    withD = false;
  }

  DoubleMatrix dvecBIn_i    ( nB, 1 );
  DoubleMatrix dvecBOut_i   ( nB, 1 );
  DoubleMatrix dvecBOut_iSum( nB, 1 );


  //------------------------------------------------------------
  // Handle the case of the Standard Two-Stage (STS) method.
  //------------------------------------------------------------

  double* pdNull = 0;
  DoubleMatrix* pdmatNull = 0;

  int i;

  if ( method == STANDARD_TWO_STAGE ||
       method == MAP_BAYES_STANDARD_TWO_STAGE )
  {
    try
    {
    // [Remove]==========================================
    //
    /*
      MOVE EVERYTHING INSIDE THIS TRY BLOCK TO A SEPARATE FUNCTION: standardTwoStage( ... )
    */
    //
    // [Remove]==========================================

      //--------------------------------------------------------
      // Calculate the population mean.
      //--------------------------------------------------------

      const double* pdNData = dvecN.data();

      DoubleMatrix dvecY_i;

      int nY_i;
      int nYTotal = 0;

      // Initially set this equal to zero.
      dvecBOut_iSum.fill( 0.0 );

      // Calculate each individual's contribution to the population mean of
      // the individual parameter estimates.
      for ( i = 0; i < nInd; i++)
      {
        try
        {
	  // Set the current individual's index for the model.
          model.selectIndividual( i );

          // Get the number of data values for this individual.
          nY_i = static_cast<int>( pdNData[i] );

          // Get this individual's data values.
          dvecY_i = getSubblock( dvecY, nYTotal, 0, nY_i, 1 );
          nYTotal += nY_i;
    
          // Get this individual's initial parameter value.
          dvecBIn_i = getCol( dmatBIn, i );
    
          // Determine this individual's final parameter estimate.    
          mapOpt(
            model,
            dvecY_i,
            indOptInfo,
            dvecBLow,
            dvecBUp,
            dvecBIn_i,
            &dvecBOut_i,
            dvecBStep,
            pdNull,
            pdmatNull,
            pdmatNull,
            withD );
    
          // Add in this individual's parameter estimate.
          dvecBOut_iSum = add( dvecBOut_iSum, dvecBOut_i );
    
          // Set this individual's final parameter estimate in the
          // matrix of estimates for each individual, if necessary.
          if ( pdmatBOut )
          {
            replaceJth( dmatBOutTemp, i, dvecBOut_i);
          }
        }
        catch( SpkException& e )
        {         
          const int max = SpkError::maxMessageLen();
          char message[max];
          snprintf( message, max, "The Standard Two-Stage (STS) method failed during the calculation of the %s individual's contribution to the population mean.",
                    intToOrdinalString( i, ZERO_IS_FIRST_INT ).c_str() );
        
          throw e.push(
            SpkError::SPK_UNKNOWN_ERR, 
            message,
            __LINE__, 
            __FILE__ );
        }
        catch( const std::exception& stde )
        {
          const int max = SpkError::maxMessageLen();
          char message[max];
          snprintf( message, max, "The Standard Two-Stage (STS) method failed because a standard exception \nwas thrown during the calculation of the %s individual's contribution to the population mean.",
                    intToOrdinalString( i, ZERO_IS_FIRST_INT ).c_str() );
        
          throw SpkException(
            stde,
            message,
            __LINE__,
            __FILE__ );
        }
        catch( ... )
        {
          const int max = SpkError::maxMessageLen();
          char message[max];
          snprintf( message, max, "The Standard Two-Stage (STS) method failed because an unknown exception \nwas thrown during the calculation of the %s individual's contribution to the population mean.",
                    intToOrdinalString( i, ZERO_IS_FIRST_INT ).c_str() );
        
          throw SpkException(
            SpkError::SPK_UNKNOWN_ERR,
            message,
            __LINE__,
            __FILE__ );
        }
      }

      // Divide by the number of individuals to get the population mean of
      // the parameter estimates,
      //
      //                            nInd
      //                            ----
      //          (STS)       1     \    
      //     bMean       =  ------  /     bOut   .
      //                     nInd   ----      i
      //                            i = 1 
      //
      divByScalar( dvecBOut_iSum, nInd, dvecBMeanOutTemp );

    
      //--------------------------------------------------------
      // Calculate the population covariance.
      //--------------------------------------------------------

      if ( pdmatBCovOut )
      {
        DoubleMatrix dvecBOut_iMinusBMean            ( nB, 1 );
        DoubleMatrix dvecBOut_iMinusBMeanTrans       ( 1,  nB );
        DoubleMatrix dmatBOut_iMinusBMeanCrossProd   ( nB, nB );
        DoubleMatrix dmatBOut_iMinusBMeanCrossProdSum( nB, nB );
  
        // Initially set this equal to zero.
        dmatBOut_iMinusBMeanCrossProdSum.fill( 0.0 );
  
        // Calculate each individual's contribution to the population
        // covariance of the individual parameter estimates.
        for ( i = 0; i < nInd; i++)
        {
          // Get this individual's final parameter value.
          dvecBOut_i = getCol( dmatBOutTemp, i );
    
          // Calculate
          //
          //     bOut  -  bMean  
          //         i
          //
          // and its transpose.
          subtract( dvecBOut_i, dvecBMeanOutTemp, dvecBOut_iMinusBMean );
          transpose( dvecBOut_iMinusBMean, dvecBOut_iMinusBMeanTrans );

          // Calculate the cross product
          //
          //                                          T
          //     ( bOut  -  bMean ) ( bOut  -  bMean )   .
          //           i                  i
          //
          multiply( 
            dvecBOut_iMinusBMean,
            dvecBOut_iMinusBMeanTrans,
            dmatBOut_iMinusBMeanCrossProd );

          // Add in this individual's contribution.
          dmatBOut_iMinusBMeanCrossProdSum = add( 
            dmatBOut_iMinusBMeanCrossProdSum,
            dmatBOut_iMinusBMeanCrossProd );
        }
  

        // Divide by the number of individuals to get the population
        // covariance,
        //
        //                           nInd
        //                           ----
        //         (STS)       1     \                                           T
        //     bCov       =  ------  /      ( bOut  -  bMean ) ( bOut  -  bMean )   .
        //                    nInd   ----         i                  i       
        //                           i = 1 
        //
        divByScalar( dmatBOut_iMinusBMeanCrossProdSum, nInd, dmatBCovOutTemp );
      }


    // [Remove]==========================================
    //
    /*
      MOVE EVERYTHING IN THIS TRY BLOCK TO A SEPARATE FUNCTION: standardTwoStage( ... )
    */
    //
    // [Remove]==========================================

    }
    catch( SpkException& e )
    {         
      throw e.push(
        SpkError::SPK_UNKNOWN_ERR, 
        "The Standard Two-Stage (STS) method failed.",
        __LINE__, 
        __FILE__ );
    }
    catch( const std::exception& stde )
    {
      throw SpkException(
        stde,
        "A standard exception was thrown during the Standard Two-Stage (STS) method.",
        __LINE__,
        __FILE__ );
    }
    catch( ... )
    {
      throw SpkException(
        SpkError::SPK_UNKNOWN_ERR,
        "An unknown exception was thrown during the Standard Two-Stage (STS) method.",
        __LINE__,
        __FILE__ );
    }
  }

  
  //------------------------------------------------------------
  // Handle the case of the Iterative Two-Stage (ITS) method.
  //------------------------------------------------------------

  if ( method == ITERATIVE_TWO_STAGE ||
       method == MAP_BAYES_ITERATIVE_TWO_STAGE )
  {
    // [Remove]==========================================
    //
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The Iterative Two-Stage (ITS) method has not yet been implemented.",
        __LINE__,
        __FILE__ );
    //
    // [Remove]==========================================
  }


  //------------------------------------------------------------
  // Handle the case of the Global Two-Stage (GTS) method.
  //------------------------------------------------------------

  if ( method == GLOBAL_TWO_STAGE ||
       method == MAP_BAYES_GLOBAL_TWO_STAGE )
  {
    // [Remove]==========================================
    //
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The Global Two-Stage (GTS) method has not yet been implemented.",
        __LINE__,
        __FILE__ );
    //
    // [Remove]==========================================
  }


  //------------------------------------------------------------
  // Set the values to be returned.
  //------------------------------------------------------------

  // Set the matrix of individual parameter estimates for each
  // individual, if necessary.
  if ( pdmatBOut )
  {
    *pdmatBOut = dmatBOutTemp;
  }

  // Set the population mean of the individual parameter estimates
  // for each individual, if necessary.
  if ( pdvecBMeanOut )
  {
    *pdvecBMeanOut = dvecBMeanOutTemp;
  }
    
  // Set the population covariance of the individual parameter estimates
  // for each individual, if necessary.
  if ( pdmatBCovOut )
  {
    *pdmatBCovOut = dmatBCovOutTemp;
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Reset these individual optimizer flags to their original values.
  indOptInfo.setSaveStateAtEndOfOpt( oldIndSaveState );
  indOptInfo.setThrowExcepIfMaxIter( oldIndThrowExcep );

}

