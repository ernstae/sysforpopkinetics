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
  pharmacodynamic
  struct
  Excep
  Argenio
  Cov
  Schumitzky
  pharmacokinetic
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
              SpkModel<double>&       /model/,
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
For the Standard Two-Stage methods (STS), the population mean of
the individuals' parameter estimates is calculated as
$math%

                           nInd
                           ----
         (STS)       1     \    
    bMean       =  ------  /     bOut_i   
                    nInd   ----      
                           i = 1 

%$$
and the population covariance of the individuals' estimates is
calculated as
$math%

                          nInd
                          ----
        (STS)       1     \                                              T
    bCov       =  ------  /      ( bOut_i  -  bMean ) ( bOut_i  -  bMean )   .
                   nInd   ----          
                          i = 1 

%$$
For the Iterative and Global Two-Stage methods (ITS and GTS), the
population mean and the population covariance of the individuals' 
estimates are calculated using the algorithms described in Schumitzky
(1995).

$head Reference$$
A. Schumitzky, EM algorithms and two stage methods in pharmacokinetic population analysis.
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

$subhead popOptInfo.epsilon$$
This real number is used to specify the convergence criteria
for the iterative and global Two-Stage methods.
It must be greater than $math%0.0%$$.

$subhead popOptInfo.nMaxIter$$
This integer must be greater than or equal to zero.
It specifies the maximum number of iterations for the iterative and 
global Two-Stage methods to attempt before giving up on convergence.
If it is equal to zero, then the initial values for the population mean
and covariance are accepted as the final values.

$subhead popOptInfo.traceLevel$$
This integer scalar specifies the amount of tracing for the iterative 
and global Two-Stage methods.
If $math%level \ge 1%$$, trace values are directed to standard output 
(stdout).  

$subhead popOptInfo.nIterCompleted$$
This integer scalar holds the number of iteration that have been 
completed for the iterative and global Two-Stage methods..

$subhead popOptInfo.isTooManyIter$$
This flag indicates whether the too-many-iteration failure has occurred.  

$subhead popOptInfo.throwExcepIfMaxIter$$
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

$subhead indOptInfo.epsilon$$
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

$subhead indOptInfo.nMaxIter$$
This integer must be greater than or equal to zero.
It specifies the maximum number of 
iterations to attempt before giving up on convergence.
If it is equal to zero, then the initial
value for $math%b%$$ is accepted as the final value, and any requested output
values are evaluated at that final value.

$subhead indOptInfo.traceLevel$$
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

$subhead indOptInfo.nIterCompleted$$
This integer scalar holds the number of iteration that have been 
completed in the optimizer.

$subhead indOptInfo.isTooManyIter$$
This flag indicates whether the too-many-iteration failure has occurred.  

$subhead indOptInfo.saveStateAtEndOfOpt$$
This flag is not used for the individual level optimization.

$subhead indOptInfo.throwExcepIfMaxIter$$
This flag is not used for the individual level optimization.

$subhead indOptInfo.isWarmStartPossible$$
This flag is not used for the individual level optimization.

$subhead indOptInfo.isWarmStart$$
This flag is not used for the individual level optimization.

$subhead indOptInfo.stateInfo$$
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
#include "allTrue.h"
#include "divByScalar.h"
#include "getCol.h"
#include "getSubblock.h"
#include "indStatistics.h"
#include "intToOrdinalString.h"
#include "isLessThanOrEqualTo.h"
#include "inverse.h"
#include "mapOpt.h"
#include "mapObjDiff.h"
#include "matabs.h"
#include "multiply.h"
#include "namespace_population_analysis.h"
#include "replaceJth.h"
#include "SpkException.h"
#include "SpkValarray.h"
#include "subtract.h"
#include "transpose.h"
#include "twoStageMethod.h"
#include "WarningsManager.h"

// Standard library header files.
#include <cmath>
#include <string>
#include <iostream>
#include <iomanip>

using SPK_VA::valarray;


/*------------------------------------------------------------------------
 * Local class definitions
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{

  //**********************************************************************
  //
  // Class: TwoStageModel
  //
  //
  // This class behaves the same as the class passed in when it is
  // constructed except that it returns a stored version of the model
  // for the covariance of the individual parameters.
  //
  //**********************************************************************

  class TwoStageModel : public SpkModel<double>
  {
    //----------------------------------------------------------
    // Class members.
    //----------------------------------------------------------

  private:
    SpkModel<double>* pModel;
    int               nIndPar;
    valarray<double>  indParVarStored;


    //----------------------------------------------------------
    // Constructors.
    //----------------------------------------------------------

  public:
    TwoStageModel( SpkModel<double>* pModelIn, int nIndParIn )
      :
      pModel         ( pModelIn ),
      nIndPar        ( nIndParIn ),
      indParVarStored( nIndParIn * nIndParIn )
    {
    }


    //----------------------------------------------------------
    // State changing functions related to the individual parameter variance.
    //----------------------------------------------------------

    void setIndParVariance( const valarray<double>& indParVarStoredIn )
    {
      assert( indParVarStoredIn.size() == nIndPar * nIndPar );

      // Invalidate the stored model's cached value for the variance.
      pModel->invalidateIndParCovarianceCache();

      // Invalidate this model's cached value for the variance.
      invalidateIndParCovarianceCache();

      // Set the new value for the variance.
      indParVarStored = indParVarStoredIn;
    }


    //----------------------------------------------------------
    // State changing functions that just call the contained model.
    //----------------------------------------------------------

    void doSelectIndividual( int iIn )
    {
      pModel->selectIndividual( iIn );
    }

    void doSetPopPar( const valarray<double>& popParIn )
    {
      pModel->setPopPar( popParIn );
    }

    void doSetIndPar( const valarray<double>& indParIn )
    {
      pModel->setIndPar( indParIn );
    }


    //----------------------------------------------------------
    // Model evaluation functions that just call the contained model.
    //----------------------------------------------------------

    void doDataMean( valarray<double>& ret ) const
    {
      pModel->dataMean( ret );
    }

    bool doDataMean_indPar( valarray<double>& ret ) const
    {
      return pModel->dataMean_indPar( ret );
    }

    bool doDataMean_popPar( valarray<double>& ret ) const
    {
      return pModel->dataMean_popPar( ret );
    }

    void doDataVariance( valarray<double>& ret ) const
    {
      pModel->dataVariance( ret );
    }

    bool doDataVariance_indPar( valarray<double>& ret ) const
    {
      return pModel->dataVariance_indPar( ret );
    }

    bool doDataVariance_popPar( valarray<double>& ret ) const
    {
      return pModel->dataVariance_popPar( ret );
    }

    void doDataVarianceInv( valarray<double>& ret ) const
    {
      pModel->dataVarianceInv( ret );
    }

    bool doDataVarianceInv_indPar( valarray<double>& ret ) const
    {
      return pModel->dataVarianceInv_indPar( ret );
    }

    bool doDataVarianceInv_popPar( valarray<double>& ret ) const
    {
      return pModel->dataVarianceInv_popPar( ret );
    }


    //----------------------------------------------------------
    // Model evaluation functions related to the individual parameter variance.
    //----------------------------------------------------------

    void doIndParVariance( valarray<double>& ret ) const
    {
      ret.resize( nIndPar * nIndPar );

      ret = indParVarStored;
    }

    bool doIndParVariance_popPar( valarray<double>& ret ) const
    {
      throw SpkException(
        SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR, 
        "doIndParVariance_popPar() is not implemented",
        __LINE__,
        __FILE__ );
    }

    void doIndParVarianceInv( valarray<double>& ret ) const
    {
      ret.resize( nIndPar * nIndPar );

      ret = inverse( indParVarStored, nIndPar );
    }

    bool doIndParVarianceInv_popPar( valarray<double>& ret ) const
    {
      throw SpkException(
        SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR, 
        "doIndParVarianceInv_popPar() is not implemented",
        __LINE__,
        __FILE__ );
    }

  };

} // [End: unnamed namespace]


/*------------------------------------------------------------------------
 * Local function declarations
 *------------------------------------------------------------------------*/

namespace // [Begin: unnamed namespace]
{
  void standardTwoStage( TwoStageModel&       twoStageModel,
                         bool                 withD,
                         const DoubleMatrix&  dvecN,
                         const DoubleMatrix&  dvecY,
                         Optimizer&           popOptInfo,
                         Optimizer&           indOptInfo,
                         const DoubleMatrix&  dvecBMeanIn,
                         const DoubleMatrix&  dvecBLow,
                         const DoubleMatrix&  dvecBUp,
                         const DoubleMatrix&  dmatBIn,
                         DoubleMatrix*        pdmatBOut,
                         const DoubleMatrix&  dvecBStep,
                         DoubleMatrix*        pdvecBMeanOut,
                         DoubleMatrix*        pdmatBCovOut );

  void iterativeTwoStage( TwoStageModel&       twoStageModel,
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
                          DoubleMatrix*        pdmatBCovOut );

  void globalTwoStage( TwoStageModel&       twoStageModel,
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
                       DoubleMatrix*        pdmatBCovOut );

  void checkIndPar(
    const Optimizer&    indOptimizer,
    const DoubleMatrix& dvecBLow,
    const DoubleMatrix& dvecBUp,
    const DoubleMatrix& dmatBOut );

  void printTracingInfo( std::ostream&        outputStream,
                         int                  k,
                         const DoubleMatrix&  dvecBLow,
                         const DoubleMatrix&  dvecBUp,
                         const DoubleMatrix&  dmatBCurr,
                         const DoubleMatrix&  dmatBPrev );

  bool checkOptStatus( Optimizer&           popOptInfo,
                       const Optimizer&     indOptInfo,
                       std::ostream&        outputStream,
                       bool                 isWithinTol,
                       int                  k,
                       const DoubleMatrix&  dvecBLow,
                       const DoubleMatrix&  dvecBUp,
                       const DoubleMatrix&  dmatBCurr,
                       SpkError::ErrorCode  errorCode,
                       std::string&         stringMessage );

} // [End: unnamed namespace]


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void twoStageMethod( SpkModel<double>&    model,
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

  const int nInd = dvecN  .nr();
  const int nB   = dmatBIn.nr();


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

  // Since this function always needs to calculate the individual
  // parameter estimates for each individual instantiate a temporary
  // row vector to hold it.
  DoubleMatrix dmatBOutTemp;
  DoubleMatrix* pdmatBOutTemp;
  dmatBOutTemp.resize( nB, nInd );
  pdmatBOutTemp = &dmatBOutTemp;


  //------------------------------------------------------------
  // Prepare to perform the Two-Stage method.
  //------------------------------------------------------------

  // Construct the model that returns a stored version of the model
  // for the covariance of the individual parameters.
  TwoStageModel twoStageModel( &model, nB );

  // Set the method name string.
  string methodString;
  if ( method == STANDARD_TWO_STAGE || method == MAP_BAYES_STANDARD_TWO_STAGE )
  {
    methodString = "Standard Two-Stage (STS)";
  }
  else if ( method == ITERATIVE_TWO_STAGE || method == MAP_BAYES_ITERATIVE_TWO_STAGE )
  {
    methodString = "Iterative Two-Stage (ITS)";
  }
  else
  {
    methodString = "Global Two-Stage (GTS)";
  }

  // This flag is used to indicate if the Bayesian terms should be
  // included in the objective function during the Standard Two-Stage
  // (STS) method.
  bool withD;


  //------------------------------------------------------------
  // Perform the Two-Stage method.
  //------------------------------------------------------------

  string messageString;

  // Perform the requested Two-Stage method.
  try
  {
    if ( method == STANDARD_TWO_STAGE             || 
         method == MAP_BAYES_STANDARD_TWO_STAGE )
    {
      // Set this so that the Bayesian terms will not be included in
      // the individual level objective functions during the Standard
      // Two-Stage analysis.
      withD = false;

      // Set the mean value for the individual parameters that would
      // be used if the Bayesian terms were included in the individual
      // level objective functions.
      DoubleMatrix dvecBMeanIn( nB, 1 );
      dvecBMeanIn.fill( 0.0 );

      // Perform the Standard-Two Stage (STS) method.
      standardTwoStage( twoStageModel,
                        withD,
                        dvecN,
                        dvecY,
                        popOptInfo,
                        indOptInfo,
                        dvecBMeanIn,
                        dvecBLow,
                        dvecBUp,
                        dmatBIn,
                        pdmatBOutTemp,
                        dvecBStep,
                        pdvecBMeanOut,
                        pdmatBCovOut );
    }
    else if ( method == ITERATIVE_TWO_STAGE             ||
              method == MAP_BAYES_ITERATIVE_TWO_STAGE )
    {
      // Perform the Iterative-Two Stage (ITS) method.
      iterativeTwoStage( twoStageModel,
                         dvecN,
                         dvecY,
                         popOptInfo,
                         indOptInfo,
                         dvecBLow,
                         dvecBUp,
                         dmatBIn,
                         pdmatBOutTemp,
                         dvecBStep,
                         pdvecBMeanOut,
                         pdmatBCovOut );
    }
    else
    {
      // Perform the Global-Two Stage (GTS) method.
      globalTwoStage( twoStageModel,
                      dvecN,
                      dvecY,
                      popOptInfo,
                      indOptInfo,
                      dvecBLow,
                      dvecBUp,
                      dmatBIn,
                      pdmatBOutTemp,
                      dvecBStep,
                      pdvecBMeanOut,
                      pdmatBCovOut );
    }
  }
  catch( SpkException& e )
  {         
    messageString = "The " + methodString + " method failed.";

    throw e.push(
      SpkError::SPK_UNKNOWN_ERR, 
      messageString.c_str(),
      __LINE__, 
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    messageString = "A standard exception was thrown during the " + methodString + " method.", 

    throw SpkException(
      stde,
      messageString.c_str(),
      __LINE__,
      __FILE__ );
  }
  catch( ... )
  {
    messageString = "An unknown exception was thrown during the " + methodString + " method.", 

    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR,
      messageString.c_str(),
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Issue warning messages for parameters that are constrained.
  //------------------------------------------------------------

  // Check for individual level parameters that are constrained.
  checkIndPar( indOptInfo, dvecBLow, dvecBUp, dmatBOutTemp );


  //------------------------------------------------------------
  // Set the values to be returned.
  //------------------------------------------------------------

  // Set the matrix of individual parameter estimates for each
  // individual, if necessary.
  if ( pdmatBOut )
  {
    *pdmatBOut = dmatBOutTemp;
  }


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  // Reset these individual optimizer flags to their original values.
  indOptInfo.setSaveStateAtEndOfOpt( oldIndSaveState );
  indOptInfo.setThrowExcepIfMaxIter( oldIndThrowExcep );

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
 * Function: standardTwoStage
 *
 *
 * Performs the Standard Two-Stage (STS) method.
 *
 *************************************************************************/

void standardTwoStage( TwoStageModel&       twoStageModel,
                       bool                 withD,
                       const DoubleMatrix&  dvecN,
                       const DoubleMatrix&  dvecY,
                       Optimizer&           popOptInfo,
                       Optimizer&           indOptInfo,
                       const DoubleMatrix&  dvecBMeanIn,
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

  const int nInd = dvecN  .nr();
  const int nB   = dmatBIn.nr();


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
  // Prepare to perform the Standard Two-Stage (STS) method.
  //------------------------------------------------------------

  DoubleMatrix dvecBIn_i    ( nB, 1 );
  DoubleMatrix dvecBOut_i   ( nB, 1 );
  DoubleMatrix dvecBOut_iSum( nB, 1 );


  //------------------------------------------------------------
  // Perform the Standard Two-Stage (STS) method.
  //------------------------------------------------------------

  double* pdNull = 0;
  DoubleMatrix* pdmatNull = 0;
  bool isFO = false;

  int i;

  try
  {
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
    for ( i = 0; i < nInd; i++ )
    {
      try
      {
        // Set the current individual's index for the model.
        twoStageModel.selectIndividual( i );

        // Get the number of data values for this individual.
        nY_i = static_cast<int>( pdNData[i] );

        // Get this individual's data values.
        dvecY_i = getSubblock( dvecY, nYTotal, 0, nY_i, 1 );
        nYTotal += nY_i;

        // Get this individual's initial parameter value.
        dvecBIn_i = getCol( dmatBIn, i );

        // Determine this individual's final parameter estimate.    
        mapOpt(
          twoStageModel,
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
          withD,
          isFO,
          pdmatNull,
          &dvecBMeanIn );

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
        snprintf( message, max, "The Two-Stage method failed during the calculation of the %s individual's contribution to the population mean.",
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
        snprintf( message, max, "The Two-Stage method failed because a standard exception \nwas thrown during the calculation of the %s individual's contribution to the population mean.",
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
        snprintf( message, max, "The Two-Stage method failed because an unknown exception \nwas thrown during the calculation of the %s individual's contribution to the population mean.",
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
    // This expression is based on the "Standard Two Stage" section of
    // Schumitzky (1995).
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
      for ( i = 0; i < nInd; i++ )
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
      // This expression is based on the "Standard Two Stage" section of
      // Schumitzky (1995).
      divByScalar( dmatBOut_iMinusBMeanCrossProdSum, nInd, dmatBCovOutTemp );
    }
  }
  catch( SpkException& e )
  {         
    throw e.push(
      SpkError::SPK_UNKNOWN_ERR, 
      "The Two-Stage method failed.",
      __LINE__, 
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    throw SpkException(
      stde,
      "A standard exception was thrown during the Two-Stage method.",
      __LINE__,
      __FILE__ );
  }
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR,
      "An unknown exception was thrown during the Two-Stage method.",
      __LINE__,
      __FILE__ );
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

}


/*************************************************************************
 *
 * Function: iterativeTwoStage
 *
 *
 * Performs the Iterative Two-Stage (ITS) method.
 *
 *************************************************************************/

void iterativeTwoStage( TwoStageModel&       twoStageModel,
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

  const int nInd = dvecN  .nr();
  const int nB   = dmatBIn.nr();

  double popEpsilon  = popOptInfo.getEpsilon();
  int    popNMaxIter = popOptInfo.getNMaxIter();
  int    popLevel    = popOptInfo.getLevel();


  //------------------------------------------------------------
  // Prepare the objects to hold the output values.
  //------------------------------------------------------------

  // Instantiate a temporary matrix to hold the values for the
  // individual parameters calculated using the Standard Two-Stage
  // (STS) method.
  DoubleMatrix dmatBOutSTS;
  DoubleMatrix* pdmatBOutSTS;
  dmatBOutSTS.resize( nB, nInd );
  pdmatBOutSTS = &dmatBOutSTS;

  // Instantiate a temporary matrix to hold the values for the
  // population mean of the individual parameter estimates for each
  // individual calculated using the Standard Two-Stage (STS) method.
  DoubleMatrix dvecBMeanOutSTS;
  DoubleMatrix* pdvecBMeanOutSTS;
  dvecBMeanOutSTS.resize( nB, 1 );
  pdvecBMeanOutSTS = &dvecBMeanOutSTS;

  // Instantiate a temporary matrix to hold the values for the
  // population covariance of the individual parameter estimates for
  // each individual calculated using the Standard Two-Stage (STS)
  // method.
  DoubleMatrix dmatBCovOutSTS;
  DoubleMatrix* pdmatBCovOutSTS;
  dmatBCovOutSTS.resize( nB, nB );
  pdmatBCovOutSTS = &dmatBCovOutSTS;


  //------------------------------------------------------------
  // Calculate initial values for the individual parameter's mean and covariance.
  //------------------------------------------------------------

  // Set this so that the Bayesian terms will not be included in the
  // individual objective functions during the initial Standard
  // Two-Stage analysis.
  bool withD = false;

  // Set the initial value for all of the individuals' parameters.
  DoubleMatrix dmatBInSTS( dmatBIn );

  // Set the mean value for the individual parameters that would
  // be used if the Bayesian terms were included in the individual
  // level objective functions.
  DoubleMatrix dvecBMeanInSTS( nB, 1 );
  dvecBMeanInSTS.fill( 0.0 );

  // Do an initial Standard Two-Stage method to get the starting
  // values for the individual parameter's mean and covariance.
  try
  {
    // Perform the Standard-Two Stage (STS) method.
    standardTwoStage( twoStageModel,
                      withD,
                      dvecN,
                      dvecY,
                      popOptInfo,
                      indOptInfo,
                      dvecBMeanInSTS,
                      dvecBLow,
                      dvecBUp,
                      dmatBInSTS,
                      pdmatBOutSTS,
                      dvecBStep,
                      pdvecBMeanOutSTS,
                      pdmatBCovOutSTS );
  }
  catch( SpkException& e )
  {         
    throw e.push(
      SpkError::SPK_UNKNOWN_ERR, 
      "The calculation of the initial values for the Two-Stage method failed.",
      __LINE__, 
      __FILE__ );
  }
  catch( const std::exception& stde )
  {

    throw SpkException(
      stde,
      "A standard exception was thrown during the calculation of the initial values for the Two-Stage method.",
      __LINE__,
      __FILE__ );
  }
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR,
      "An unknown exception was thrown during the calculation of the initial values for the Two-Stage method.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Prepare to perform the Iterative Two-Stage (ITS) method.
  //------------------------------------------------------------

  DoubleMatrix dvecBOut_i   ( nB, 1 );
  DoubleMatrix dvecBOut_iSum( nB, 1 );

  DoubleMatrix dmatBCov_k       ( nB, nB );
  DoubleMatrix dmatBOutCov_i    ( nB, nB );
  DoubleMatrix dmatBOutCov_iSum ( nB, nB );
  DoubleMatrix dmatBOutCov_iMean( nB, nB );
  
  valarray<double> bOutCov_i( nB * nB );

  DoubleMatrix dmatMapObj_b_bOut( nB, nB );

  DoubleMatrix dvecY_i;

  // Set the initial value for the covariance of the individual
  // parameters equal to the value calculated using the Standard
  // Two-Stage (STS) method,
  //
  //                   (STS)
  //     bCov   =  bCov       ,
  //         1
  //
  // where the 1 subscript is for k = 1, i.e., the beginning of the
  // first iteration.
  dmatBCov_k = dmatBCovOutSTS;

  // Set this so that the Bayesian terms will be included in the
  // individual objective functions during the rest of the Standard
  // Two-Stage analysis that will be performed.
  withD = true;

  // Set this so that all of the individual parameter elements will be
  // used in the calculation of the covariances of the individuals'
  // parameter estimates.  
  valarray<bool> bMask( true, nB );

  // Set the formulation that will be used to approximate the
  // covariance of the individual parameter estimates.
  IndCovForm bStatisticsForm = R;

  // Initialize the convergence flag.
  bool isWithinTol;
  if ( popNMaxIter > 0 )
  {
    // Set the value for the case of one or more iterations.
    isWithinTol = false;
  }
  else
  {
    // If zero iterations have been requested, then accept the values
    // calculated during the initial Standard Two-Stage method above.
    isWithinTol = true;
  }

  // Send the output to standard cout.
  std::ostream& outputStream = std::cout;


  //------------------------------------------------------------
  // Perform the Iterative Two-Stage (ITS) method.
  //------------------------------------------------------------

  const double* pdBLowData    = dvecBLow.data();
  const double* pdBUpData     = dvecBUp.data();
  const double* pdBInSTSData  = dmatBInSTS.data();
  const double* pdBOutSTSData = dmatBOutSTS.data();
  const double* pdNData       = dvecN.data();

  int i;
  int j;
  int k;

  double* pdNull = 0;
  DoubleMatrix* pdmatNull = 0;
  valarray<double>* pVANull = 0;
  bool isFO = false;

  int nY_i;
  int nYTotal;

  k = 1;

  // Do some tracing if necessary.
  if ( popLevel > 0 && popNMaxIter > 0 )
  {
    outputStream << endl;
    outputStream << "Begin search for optimal parameter values." << endl;
    outputStream << endl;

    // In order to get the scaled change in the parameter values (dx)
    // to be zero, pass the tracing function the same value for the
    // current and previous set of individual parameters.
    printTracingInfo(
      outputStream,
      k,
      dvecBLow,
      dvecBUp,
      dmatBOutSTS,
      dmatBOutSTS );
  }

  // Perform the number of iterations that were requested.
  try
  {
    while ( !isWithinTol && ( k < popNMaxIter + 1 ) )
    {
      //----------------------------------------------------------
      // Do preparations related to the current iteration.
      //----------------------------------------------------------

      k++;


      //----------------------------------------------------------
      // Calculate the current set of individual parameter estimates.
      //----------------------------------------------------------

      // Set the SpkModel subclass's value for the covariance of the
      // individual parameters equal to the current value,
      //
      //     D  =  bCov       .
      //               k
      //
      twoStageModel.setIndParVariance( dmatBCov_k.toValarray() );

      // Set the current value for each of the individuals' parameters
      // and the mean of the individual parameters,
      //
      //              (STS)
      //     b   =  b        ,
      //      i      i
      //
      // and
      //
      //                    (STS)
      //     bMean  =  bMean      .
      //
      dmatBInSTS     = dmatBOutSTS;
      dvecBMeanInSTS = dvecBMeanOutSTS;

      // Do a Standard Two-Stage (STS) analysis using the value for the
      // covariance of the individual parameters that was just set.
      // 
      // Because the Bayesian terms are included in the individual
      // objective functions, this will calculate the set of MAP Bayesian
      // estimates referred to in equation (16) of Schumitzky (1995), i.e.,
      //
      //     bOut   =  arg max [ MapObj ( b ) ]  ,
      //         i                     i
      //
      // where
      //     
      //                  1                        1             T      -1
      //     MapObj (b) = - logdet[ 2 pi R (b) ] + - [y  - f (b)]  R (b)  [y  - f (b)]
      //           i      2               i        2   i    i       i       i    i
      //
      //                  1                        1            T  -1
      //                + - logdet[ 2 pi D ]     + - [bMean - b]  D  [bMean - b]  .
      //                  2                        2             
      //
      standardTwoStage( twoStageModel,
                        withD,
                        dvecN,
                        dvecY,
                        popOptInfo,
                        indOptInfo,
                        dvecBMeanInSTS,
                        dvecBLow,
                        dvecBUp,
                        dmatBInSTS,
                        pdmatBOutSTS,
                        dvecBStep,
                        pdvecBMeanOutSTS,
                        pdmatBCovOutSTS );

      pdBInSTSData  = dmatBInSTS.data();
      pdBOutSTSData = dmatBOutSTS.data();

  
      //--------------------------------------------------------
      // See if this function's convergence criterion has been met.
      //--------------------------------------------------------

      // Check all of the individuals' parameter values to see if
      //
      //     abs( b     -  b      )  <=  popEpsilon * ( bUp - bLow )  ,
      //           i,k      i,k-1
      //
      // where abs is the element-by-element absolute value function.
      isWithinTol = true;
      i = 0;
      while ( isWithinTol && i < nInd )
      {
        j = 0;
        while ( isWithinTol && j < nB )
        {
          if ( abs( pdBOutSTSData[j + i * nB] - pdBInSTSData[j + i * nB] ) > 
                 popEpsilon * ( pdBUpData[j] -  pdBLowData[j] ) )
          {
            isWithinTol = false;
          }
          j++;
        }
        i++;
      }

      // Do some tracing if necessary.
      if ( popLevel > 0 )
      {
        printTracingInfo(
          outputStream,
          k,
          dvecBLow,
          dvecBUp,
          dmatBOutSTS,
          dmatBInSTS );
      }

  
      //----------------------------------------------------------
      // Calculate the mean of all of the parameter estimates' covariances.
      //----------------------------------------------------------

      nYTotal = 0;

      // Initially set this equal to zero.
      dmatBOutCov_iSum.fill( 0.0 );

      // Calculate the sum of the covariances of the individual
      // parameter estimates,
      //
      //                     nInd
      //                     ----
      //                     \   
      //     bOutCov Sum  =  /      P     ,
      //            i        ----    i,k
      //                     i = 1
      //
      // where
      //
      //     P     =  cov[ bOut , bOut  ]
      //      i,k              i      i
      //
      //           =  bOutCov
      //                     i
      //
      // is an approximation for the covariance of each individual
      // parameter estimate.
      for ( i = 0; i < nInd; i++ )
      {
        try
        {
          // Set the current individual's index in the model.
          twoStageModel.selectIndividual( i );
      
          // Get the number of data values for this individual.
          nY_i = static_cast<int>( pdNData[i] );

          // Get this individual's data values.
          dvecY_i = getSubblock( dvecY, nYTotal, 0, nY_i, 1 );
          nYTotal += nY_i;

          // Get this individual's final parameter value.
          dvecBOut_i = getCol( dmatBOutSTS, i );

          // Set this individual's parameter value in the model.
          twoStageModel.setIndPar( dvecBOut_i.toValarray() );

          // Get the Hessian of this individual's objective function.
          mapObjDiff(
            twoStageModel,
            dvecY_i,
            dvecBStep,
            dvecBOut_i,
            pdmatNull,
            &dmatMapObj_b_bOut,
            withD,
            isFO,
            pdmatNull,
            &dvecBMeanInSTS );

          // Get the covariance of this individual's parameter estimate.
          indStatistics(
            twoStageModel,
            dvecY_i.toValarray(),
            dvecBOut_i.toValarray(),
            bMask,
            dmatMapObj_b_bOut.toValarray(),
            bStatisticsForm,
            &bOutCov_i,
            pVANull,                          
            pVANull,
            pVANull,
            pVANull,
            withD );
          dmatBOutCov_i.fromValarray( bOutCov_i, nB );
       
          // Add in this individual's contribution.
          dmatBOutCov_iSum = add( dmatBOutCov_iSum, dmatBOutCov_i );
        }
        catch( SpkException& e )
        {         
          const int max = SpkError::maxMessageLen();
          char message[max];
          snprintf( message, max, "The Two-Stage method failed during the calculation of the %s individual's \ncontribution to the sum of the covariances for the parameter estimates.",
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
          snprintf( message, max, "The Two-Stage method failed because a standard exception was thrown during \nthe calculation of the %s individual's contribution to the sum of the covariances for the parameter estimates.",
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
          snprintf( message, max, "The Two-Stage method failed because an unknown exception was thrown during \nthe calculation of the %s individual's contribution to the sum of the covariances for the parameter estimates.",
                    intToOrdinalString( i, ZERO_IS_FIRST_INT ).c_str() );
        
          throw SpkException(
            SpkError::SPK_UNKNOWN_ERR,
            message,
            __LINE__,
            __FILE__ );
        }
      }

      // Calculate the mean of the covariances of the individual
      // parameter estimates,
      //
      //                              nInd
      //                              ----
      //                        1     \   
      //     bOutCov Mean  =  ------  /     bOutCov   .
      //            i          nInd   ----         i
      //                              i = 1
      //
      // Note that this sum corresponds to the mean of the first term
      // from Equation (17b) of Schumitzky (1995).
      divByScalar( dmatBOutCov_iSum, nInd, dmatBOutCov_iMean );

  
      //----------------------------------------------------------
      // Calculate the updated version of the covariance of the parameters.
      //----------------------------------------------------------

      // Calculate the updated value for the model for the covariance of
      // the individual parameters.
      //
      // The updated value for the covariance of the individual
      // parameters that will be used for the next iteration (k + 1 )
      // is given by Equation (17b) of Schumitzky (1995),
      //
      //                       nInd
      //                       ----    -                                                 -
      //                 1     \      |                                                T  |
      //     bCov   =  ------  /      |  P     +  ( bOut  -  bMean ) ( bOut  -  bMean )   |
      //         k      nInd   ----   |   i,k           i                  i              |
      //                       i = 1   -                                                 -
      //
      //                       nInd                          nInd   
      //                       ----    -      -              ----    -                                        - 
      //                 1     \      |        |       1     \      |                                       T  |
      //            =  ------  /      |  P     |  +  ------  /      |  ( bOut  -  bMean ) ( bOut  -  bMean )   |
      //                nInd   ----   |   i,k  |      nInd   ----   |        i                  i              |
      //                       i = 1   -      -              i = 1   -                                        - 
      //
      //                                    (STS)
      //            =  bOutCov Mean  +  bCov      .
      //                      i
      //
      dmatBCov_k = add( dmatBOutCov_iMean, dmatBCovOutSTS );
    }
  }
  catch( SpkException& e )
  {
    throw e.push(
      SpkError::SPK_OPT_ERR, 
      "The iterations for the Two-Stage method failed.",
      __LINE__, 
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    throw SpkException(
      stde,
      "A standard exception was thrown during the iterations for the Two-Stage method.",
      __LINE__, 
      __FILE__ );
  }  
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR, 
      "An unknown exception was thrown during the iteration for the Two-Stage method.",
      __LINE__, 
      __FILE__ );
  }


  //------------------------------------------------------------
  // Check the status of the optimization.
  //------------------------------------------------------------

  bool ok;
  SpkError::ErrorCode errorCode;
  string stringMessage;

  // See if the optimization converged successfully and check to see
  // if any individual parameter values are at or near their bounds.
  ok = checkOptStatus(
    popOptInfo,
    indOptInfo,
    outputStream,
    isWithinTol,
    k,
    dvecBLow,
    dvecBUp,
    dmatBOutSTS,
    errorCode,
    stringMessage );

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
  popOptInfo.setDidOptFinishOk( true );


  //------------------------------------------------------------
  // Set the values to be returned.
  //------------------------------------------------------------

  // Set the matrix of individual parameter estimates for each
  // individual, if necessary.
  if ( pdmatBOut )
  {
    *pdmatBOut = dmatBOutSTS;
  }

  // Set the population mean of the individual parameter estimates
  // for each individual, if necessary.
  if ( pdvecBMeanOut )
  {
    *pdvecBMeanOut = dvecBMeanOutSTS;
  }
    
  // Set the population covariance of the individual parameter estimates
  // for each individual, if necessary.
  if ( pdmatBCovOut )
  {
    *pdmatBCovOut = dmatBCov_k;
  }

}


/*************************************************************************
 *
 * Function: globalTwoStage
 *
 *
 * Performs the Global Two-Stage (GTS) method.
 *
 *************************************************************************/

void globalTwoStage( TwoStageModel&       twoStageModel,
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

  const int nInd = dvecN  .nr();
  const int nB   = dmatBIn.nr();

  double popEpsilon  = popOptInfo.getEpsilon();
  int    popNMaxIter = popOptInfo.getNMaxIter();
  int    popLevel    = popOptInfo.getLevel();


  //------------------------------------------------------------
  // Prepare the objects to hold the output values.
  //------------------------------------------------------------

  // Instantiate a temporary matrix to hold the values for the
  // individual parameters calculated using the Standard Two-Stage
  // (STS) method.
  DoubleMatrix dmatBOutSTS;
  DoubleMatrix* pdmatBOutSTS;
  dmatBOutSTS.resize( nB, nInd );
  pdmatBOutSTS = &dmatBOutSTS;

  // Instantiate a temporary matrix to hold the values for the
  // population mean of the individual parameter estimates for each
  // individual calculated using the Standard Two-Stage (STS) method.
  DoubleMatrix dvecBMeanOutSTS;
  DoubleMatrix* pdvecBMeanOutSTS;
  dvecBMeanOutSTS.resize( nB, 1 );
  pdvecBMeanOutSTS = &dvecBMeanOutSTS;

  // Instantiate a temporary matrix to hold the values for the
  // population covariance of the individual parameter estimates for
  // each individual calculated using the Standard Two-Stage (STS)
  // method.
  DoubleMatrix dmatBCovOutSTS;
  DoubleMatrix* pdmatBCovOutSTS;
  dmatBCovOutSTS.resize( nB, nB );
  pdmatBCovOutSTS = &dmatBCovOutSTS;


  //------------------------------------------------------------
  // Calculate initial values for the individual parameter's mean and covariance.
  //------------------------------------------------------------

  // Set this so that the Bayesian terms will not be included in the
  // individual objective functions during the initial Standard
  // Two-Stage analysis.
  bool withD = false;

  // Set the initial value for all of the individuals' parameters.
  DoubleMatrix dmatBInSTS( dmatBIn );

  // Set the mean value for the individual parameters that would
  // be used if the Bayesian terms were included in the individual
  // level objective functions.
  DoubleMatrix dvecBMeanInSTS( nB, 1 );
  dvecBMeanInSTS.fill( 0.0 );

  // Do an initial Standard Two-Stage method to get the starting
  // values for the individual parameter's mean and covariance.
  try
  {
    // Perform the Standard-Two Stage (STS) method.
    standardTwoStage( twoStageModel,
                      withD,
                      dvecN,
                      dvecY,
                      popOptInfo,
                      indOptInfo,
                      dvecBMeanInSTS,
                      dvecBLow,
                      dvecBUp,
                      dmatBInSTS,
                      pdmatBOutSTS,
                      dvecBStep,
                      pdvecBMeanOutSTS,
                      pdmatBCovOutSTS );
  }
  catch( SpkException& e )
  {         
    throw e.push(
      SpkError::SPK_UNKNOWN_ERR, 
      "The calculation of the initial values for the Two-Stage method failed.",
      __LINE__, 
      __FILE__ );
  }
  catch( const std::exception& stde )
  {

    throw SpkException(
      stde,
      "A standard exception was thrown during the calculation of the initial values for the Two-Stage method.",
      __LINE__,
      __FILE__ );
  }
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR,
      "An unknown exception was thrown during the calculation of the initial values for the Two-Stage method.",
      __LINE__,
      __FILE__ );
  }


  //------------------------------------------------------------
  // Prepare to perform the Global Two-Stage (GTS) method.
  //------------------------------------------------------------

  DoubleMatrix dvecBOut_i              ( nB, 1 ); 
  DoubleMatrix dvecBMeanCurr           ( nB, 1 ); 
  DoubleMatrix dvecBOut_iMinusBMeanCurr( nB, 1 ); 

  DoubleMatrix dmatBCov_k                   ( nB, nB );
  DoubleMatrix dmatBCovInv_k                ( nB, nB );
  DoubleMatrix dmatBOutCovInv_i             ( nB, nB ); 
  DoubleMatrix dmatBCovInv_kPlusBOutCovInv_i( nB, nB ); 

  DoubleMatrix dvecP_i_kTimesBOutCovInv_iTimesBOut_iMinusBMeanCurr( nB, 1 );

  DoubleMatrix dmatP_i_k                 ( nB, nB ); 
  DoubleMatrix dmatP_i_kSum              ( nB, nB ); 
  DoubleMatrix dmatP_i_kMean             ( nB, nB ); 
  DoubleMatrix dmatP_i_kTimesBOutCovInv_i( nB, nB ); 

  DoubleMatrix dvecQ_i_k               ( nB, 1 ); 
  DoubleMatrix dvecQ_i_kSum            ( nB, 1 ); 
  DoubleMatrix dvecQ_i_kMinusBMean     ( nB, 1 ); 
  DoubleMatrix dvecQ_i_kMinusBMeanTrans( nB, 1 ); 

  DoubleMatrix dmatQ_i_kCov                   ( nB, nB ); 
  DoubleMatrix dmatQ_i_kMinusBMeanCrossProdSum( nB, nB ); 
  DoubleMatrix dmatQ_i_kMinusBMeanCrossProd   ( nB, nB ); 

  DoubleMatrix dmatQPrev( nB, nInd );
  DoubleMatrix dmatQCurr( nB, nInd );

  valarray<double> bOutCov_i   ( nB * nB );
  valarray<double> bOutCovInv_i( nB * nB );

  valarray<double> bOutCovInvAll( nInd * nB * nB );

  DoubleMatrix dmatMapObj_b_bOut( nB, nB );

  DoubleMatrix dvecY_i;

  // Set the initial values for the mean and covariance of the
  // individual parameters equal to the value calculated using the
  // Standard Two-Stage (STS) method,
  //
  //                     (STS)
  //     bMean   =  bMean       ,
  //          1
  //
  //                    (STS)
  //     bCov    =  bCov       ,
  //         1
  //
  // where the 1 subscript is for k = 1, i.e., the beginning of the
  // first iteration.
  dvecBMeanCurr = dvecBMeanOutSTS;
  dmatBCov_k    = dmatBCovOutSTS;

  // Set the current and previous values for the auxiliary mean
  // parameters equal to each individual's parameter value calculated
  // using the Standard Two-Stage (STS) method,
  //
  //      (Curr)         (STS)
  //     q        =  bOut       ,
  //      i,1            i
  //
  //      (Prev)         (STS)
  //     q        =  bOut       ,
  //      i,1            i
  //
  // where the 1 subscript is for k = 1, i.e., the beginning of the
  // first iteration.
  dmatQCurr = dmatBOutSTS;
  dmatQPrev = dmatQCurr;

  // Set this so that all of the individual parameter elements will be
  // used in the calculation of the covariances of the individuals'
  // parameter estimates.  
  valarray<bool> bMask( true, nB );

  // Set the formulation that will be used to approximate the
  // covariance of the individual parameter estimates.
  IndCovForm bStatisticsForm = R;

  // Initialize the convergence flag.
  bool isWithinTol;
  if ( popNMaxIter > 0 )
  {
    // Set the value for the case of one or more iterations.
    isWithinTol = false;
  }
  else
  {
    // If zero iterations have been requested, then accept the values
    // calculated during the initial Standard Two-Stage method above.
    isWithinTol = true;
  }

  // Send the output to standard cout.
  std::ostream& outputStream = std::cout;


  //------------------------------------------------------------
  // Calculate the covariances of the initial STS parameter estimates.
  //------------------------------------------------------------

  const double* pdNData = dvecN.data();

  int i;
  int j;
  int k;

  double* pdNull = 0;
  DoubleMatrix* pdmatNull = 0;
  valarray<double>* pVANull = 0;
  bool isFO = false;

  int nY_i;
  int nYTotal;

  nYTotal = 0;

  // Calculate the covariances of each of the individuals'
  // parameter estimates,
  //
  //     V   =  cov[ bOut , bOut  ]
  //      i              i      i
  //
  //         =  bOutCov     .
  //                     i
  //
  for ( i = 0; i < nInd; i++ )
  {
    try
    {
      // Set the current individual's index in the model.
      twoStageModel.selectIndividual( i );
  
      // Get the number of data values for this individual.
      nY_i = static_cast<int>( pdNData[i] );

      // Get this individual's data values.
      dvecY_i = getSubblock( dvecY, nYTotal, 0, nY_i, 1 );
      nYTotal += nY_i;

      // Get this individual's final parameter value.
      dvecBOut_i = getCol( dmatBOutSTS, i );

      // Set this individual's parameter value in the model.
      twoStageModel.setIndPar( dvecBOut_i.toValarray() );

      // Get the Hessian of this individual's objective function.
      mapObjDiff(
        twoStageModel,
        dvecY_i,
        dvecBStep,
        dvecBOut_i,
        pdmatNull,
        &dmatMapObj_b_bOut,
        withD,
        isFO,
        pdmatNull,
        &dvecBMeanInSTS );

      // Get the covariance of this individual's parameter estimate.
      indStatistics(
        twoStageModel,
        dvecY_i.toValarray(),
        dvecBOut_i.toValarray(),
        bMask,
        dmatMapObj_b_bOut.toValarray(),
        bStatisticsForm,
        &bOutCov_i,
        pVANull,                          
        pVANull,
        pVANull,
        pVANull,
        withD );

      // Calculate the inverse of the covariance of this individual's
      // parameter estimate.
      bOutCovInv_i = inverse( bOutCov_i, nB );

      // Save the inverse of the covariance of this individual's
      // parameter estimate.
      bOutCovInvAll[ slice( i * nB * nB, nB * nB, 1 ) ] = bOutCovInv_i;
    }
    catch( SpkException& e )
    {         
      const int max = SpkError::maxMessageLen();
      char message[max];
      snprintf( message, max, "The Two-Stage method failed during the calculation of the %s individual's \nparameter estimate covariance inverse.",
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
      snprintf( message, max, "The Two-Stage method failed because a standard exception was thrown during \nthe calculation of the %s individual's parameter estimate covariance inverse.",
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
      snprintf( message, max, "The Two-Stage method failed because an unknown exception was thrown during \nthe calculation of the %s individual's parameter estimate covariance inverse.",
                intToOrdinalString( i, ZERO_IS_FIRST_INT ).c_str() );
    
      throw SpkException(
        SpkError::SPK_UNKNOWN_ERR,
        message,
        __LINE__,
        __FILE__ );
    }
  }

  
  //------------------------------------------------------------
  // Perform the Global Two-Stage (GTS) method.
  //------------------------------------------------------------

  const double* pdBLowData  = dvecBLow.data();
  const double* pdBUpData   = dvecBUp.data();
  const double* pdQPrevData = dmatQPrev.data();
  const double* pdQCurrData = dmatQCurr.data();

  k = 1;

  // Do some tracing if necessary.
  if ( popLevel > 0 && popNMaxIter > 0 )
  {
    outputStream << endl;
    outputStream << "Begin search for optimal parameter values." << endl;
    outputStream << endl;

    printTracingInfo(
      outputStream,
      k,
      dvecBLow,
      dvecBUp,
      dmatQCurr,
      dmatQPrev );
  }

  // Perform the number of iterations that were requested.
  try
  {
    while ( !isWithinTol && ( k < popNMaxIter + 1 ) )
    {
      //----------------------------------------------------------
      // Do preparations related to the current iteration.
      //----------------------------------------------------------

      k++;

      // Calculate the inverse of the updated value for the model for
      // the covariance of the individual parameters,
      //
      //          -1
      //     bCov    .
      //         k
      //
      dmatBCovInv_k = inverse( dmatBCov_k );

      // Save the current set of auxiliary means.
      dmatQPrev = dmatQCurr;

      // Initially set these equal to zero.
      dmatP_i_kSum.fill( 0.0 );
      dvecQ_i_kSum.fill( 0.0 );


      //----------------------------------------------------------
      // Calculate the current values for each individual's auxiliary quantities.
      //----------------------------------------------------------

      // Calculate the following auxiliary mean and auxiliary
      // covariance that are defined in Equations (14a) and (14b) of
      // Schumitzky (1995):
      //
      //                                    -1
      //     q     =  bMean   +  P     *  V     *  ( bOut  -  bMean  )
      //      i,k          k      i,k      i             i         k   
      //
      // and
      //               -                 -
      //              |       -1     -1   |  -1
      //     P     =  |  bCov   +  V      |      ,
      //      i,k     |      k      i     |
      //               -                 -
      //
      // where bCov_k is the updated value for the model for the
      // covariance of the individual parameters and V_i is the
      // covariance of the i-th individual's parameter estimate,
      // bOut_i.
      for ( i = 0; i < nInd; i++ )
      {
        try
        {
          // Get the inverse of the covariance of this individual's
          // parameter estimate,
          //
          //       -1
          //     V    .
          //      i
          //
          bOutCovInv_i = bOutCovInvAll[ slice( i * nB * nB, nB * nB, 1 ) ];
          dmatBOutCovInv_i.fromValarray( bOutCovInv_i, nB );

          // Calculate this individual's auxiliary covariance,
          //
          //     P     .
          //      i,k
          //
          dmatBCovInv_kPlusBOutCovInv_i = add( dmatBCovInv_k, dmatBOutCovInv_i );
          dmatP_i_k = inverse( dmatBCovInv_kPlusBOutCovInv_i );

          // Add in this individual's contribution to the sum of
          // auxiliary covariancs.
          dmatP_i_kSum = add( dmatP_i_kSum, dmatP_i_k );

          // Get this individual's final parameter value.
          dvecBOut_i = getCol( dmatBOutSTS, i );

          // Calculate
          //
          //                -1
          //     P     *  V     *  ( bOut  -  bMean  )  .
          //      i,k      i             i         k   
          //
          dvecBOut_iMinusBMeanCurr = subtract( dvecBOut_i, dvecBMeanCurr );
          dmatP_i_kTimesBOutCovInv_i = multiply( dmatP_i_k, dmatBOutCovInv_i );
          dvecP_i_kTimesBOutCovInv_iTimesBOut_iMinusBMeanCurr = 
            multiply( dmatP_i_kTimesBOutCovInv_i, dvecBOut_iMinusBMeanCurr );

          // Calculate this individual's auxiliary mean,
          //
          //     q     .
          //      i,k
          //
          dvecQ_i_k = add( dvecBMeanCurr, dvecP_i_kTimesBOutCovInv_iTimesBOut_iMinusBMeanCurr );

          // Save this individual's auxiliary mean.
          replaceJth( dmatQCurr, i, dvecQ_i_k );

          // Add in this individual's contribution to the sum of
          // auxiliary means.
          dvecQ_i_kSum = add( dvecQ_i_kSum, dvecQ_i_k );
        }
        catch( SpkException& e )
        {         
          const int max = SpkError::maxMessageLen();
          char message[max];
          snprintf( message, max, "The Two-Stage method failed during the calculation of the %s individual's \ncontribution to the mean and covariance of the individual parameters.",
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
          snprintf( message, max, "The Two-Stage method failed because a standard exception was thrown during \nthe calculation of the %s individual's contribution to the mean and covariance of the individual parameters.",
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
          snprintf( message, max, "The Two-Stage method failed because an unknown exception was thrown during \nthe calculation of the %s individual's contribution  to the mean and covariance of the individual parameters.",
                    intToOrdinalString( i, ZERO_IS_FIRST_INT ).c_str() );
        
          throw SpkException(
            SpkError::SPK_UNKNOWN_ERR,
            message,
            __LINE__,
            __FILE__ );
        }
      }

      pdQPrevData = dmatQPrev.data();
      pdQCurrData = dmatQCurr.data();

  
      //--------------------------------------------------------
      // See if this function's convergence criterion has been met.
      //--------------------------------------------------------

      // Check all of the individuals' parameter values to see if
      //
      //     abs( q     -  q      )  <=  popEpsilon * ( bUp - bLow )  ,
      //           i,k      i,k-1
      //
      // where abs is the element-by-element absolute value function.
      isWithinTol = true;
      i = 0;
      while ( isWithinTol && i < nInd )
      {
        j = 0;
        while ( isWithinTol && j < nB )
        {
          if ( abs( pdQCurrData[j + i * nB] - pdQPrevData[j + i * nB] ) > 
                 popEpsilon * ( pdBUpData[j] -  pdBLowData[j] ) )
          {
            isWithinTol = false;
          }
          j++;
        }
        i++;
      }

      // Do some tracing if necessary.
      if ( popLevel > 0 )
      {
        printTracingInfo(
          outputStream,
          k,
          dvecBLow,
          dvecBUp,
          dmatQCurr,
          dmatQPrev );
      }

  
      //----------------------------------------------------------
      // Calculate the updated version of the mean of the parameters.
      //----------------------------------------------------------

      // Calculate the updated value for the mean of the individual
      // parameters that will be used for the next iteration (k + 1 )
      // using Equation (15a) of Schumitzky (1995),
      //
      //                        nInd
      //                        ---- 
      //                  1     \    
      //     bMean   =  ------  /      q     .
      //          k      nInd   ----    i,k
      //                        i = 1
      //
      divByScalar( dvecQ_i_kSum, nInd, dvecBMeanCurr );


      //----------------------------------------------------------
      // Calculate the updated version of the covariance of the parameters.
      //----------------------------------------------------------

      // Calculate the mean of the auxiliary covariances,
      //
      //                          nInd
      //                          ---- 
      //                    1     \    
      //     P   Mean  =  ------  /      P     .
      //      i,k          nInd   ----    i,k
      //                          i = 1
      //
      // Note that this sum corresponds to the mean of the first term
      // from Equation (15b) of Schumitzky (1995).
      divByScalar( dmatP_i_kSum, nInd, dmatP_i_kMean );

      // Initially set this equal to zero.
      dmatQ_i_kMinusBMeanCrossProdSum.fill( 0.0 );

      // Calculate the mean of the cross products of the differences
      // between the auxiliary means and the current mean of the
      // individual parameters,
      //
      //                       nInd
      //                       ----    -                                        -
      //                 1     \      |                                       T  |
      //     bCov   =  ------  /      |  ( q     -  bMean ) ( q     -  bMean )   |  .
      //         k      nInd   ----   |     i,k                i,k               |
      //                       i = 1   -                                        -
      //
      // Note that this sum corresponds to the mean of the second term
      // from Equation (15b) of Schumitzky (1995).
      for ( i = 0; i < nInd; i++ )
      {
        // Get this individual's auxiliary mean.
        dvecQ_i_k = getCol( dmatQCurr, i );

        // Calculate
        //
        //     q     -  bMean  
        //      i,k 
        //
        // and its transpose.
        subtract( dvecQ_i_k, dvecBMeanCurr, dvecQ_i_kMinusBMean );
        transpose( dvecQ_i_kMinusBMean, dvecQ_i_kMinusBMeanTrans );

        // Calculate the cross product
        //
        //                                          T
        //     ( q     -  bMean ) ( q     -  bMean )   .
        //        i,k                i,k 
        //
        multiply( 
          dvecQ_i_kMinusBMean,
          dvecQ_i_kMinusBMeanTrans,
          dmatQ_i_kMinusBMeanCrossProd );

        // Add in this individual's contribution.
        dmatQ_i_kMinusBMeanCrossProdSum = add( 
          dmatQ_i_kMinusBMeanCrossProdSum,
          dmatQ_i_kMinusBMeanCrossProd );
      }
      divByScalar( dmatQ_i_kMinusBMeanCrossProdSum, nInd, dmatQ_i_kCov );

      // Calculate the updated value for the model for the covariance of
      // the individual parameters.
      //
      // The updated value for the covariance of the individual
      // parameters that will be used for the next iteration (k + 1) is
      // given by Equation (15b) of Schumitzky (1995),
      //
      //                       nInd
      //                       ----    -                                                 -
      //                 1     \      |                                                T  |
      //     bCov   =  ------  /      |  P     +  ( q     -  bMean ) ( q     -  bMean )   |
      //         k      nInd   ----   |   i,k        i,k                i,k               |
      //                       i = 1   -                                                 -
      //
      //                       nInd                          nInd   
      //                       ----    -      -              ----    -                                        - 
      //                 1     \      |        |       1     \      |                                       T  |
      //            =  ------  /      |  P     |  +  ------  /      |  ( q     -  bMean ) ( q     -  bMean )   |
      //                nInd   ----   |   i,k  |      nInd   ----   |     i,k                i,k               |
      //                       i = 1   -      -              i = 1   -                                        - 
      //
      dmatBCov_k = add( dmatP_i_kMean, dmatQ_i_kCov );
    }
  }
  catch( SpkException& e )
  {
    throw e.push(
      SpkError::SPK_OPT_ERR, 
      "The iterations for the Two-Stage method failed.",
      __LINE__, 
      __FILE__ );
  }
  catch( const std::exception& stde )
  {
    throw SpkException(
      stde,
      "A standard exception was thrown during the iterations for the Two-Stage method.",
      __LINE__, 
      __FILE__ );
  }  
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR, 
      "An unknown exception was thrown during the iteration for the Two-Stage method.",
      __LINE__, 
      __FILE__ );
  }


  //------------------------------------------------------------
  // Check the status of the optimization.
  //------------------------------------------------------------

  bool ok;
  SpkError::ErrorCode errorCode;
  string stringMessage;

  // See if the optimization converged successfully and check to see
  // if any individual parameter values are at or near their bounds.
  ok = checkOptStatus(
    popOptInfo,
    indOptInfo,
    outputStream,
    isWithinTol,
    k,
    dvecBLow,
    dvecBUp,
    dmatQCurr,
    errorCode,
    stringMessage );

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
  popOptInfo.setDidOptFinishOk( true );


  //------------------------------------------------------------
  // Set the values to be returned.
  //------------------------------------------------------------

  // Set the matrix of individual parameter estimates for each
  // individual, if necessary.
  if ( pdmatBOut )
  {
    *pdmatBOut = dmatQCurr;
  }

  // Set the population mean of the individual parameter estimates
  // for each individual, if necessary.
  if ( pdvecBMeanOut )
  {
    *pdvecBMeanOut = dvecBMeanCurr;
  }
    
  // Set the population covariance of the individual parameter estimates
  // for each individual, if necessary.
  if ( pdmatBCovOut )
  {
    *pdmatBCovOut = dmatBCov_k;
  }

}


/*************************************************************************
 *
 * Function: checkIndPar
 *
 *
 * Checks the matrix of output individual parameters to see if any of
 * its elements is constrained by its corresponding lower or upper
 * limit but not by both.
 *
 *************************************************************************/

void checkIndPar(
  const Optimizer&    indOptimizer,
  const DoubleMatrix& dvecBLow,
  const DoubleMatrix& dvecBUp,
  const DoubleMatrix& dmatBOut )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  const double* pdBLowData = dvecBLow.data();
  const double* pdBUpData  = dvecBUp .data();
  const double* pdBOutData = dmatBOut.data();

  int nB   = dmatBOut.nr();
  int nInd = dmatBOut.nc();


  //------------------------------------------------------------
  // Check the parameters to see if any are constrained.
  //------------------------------------------------------------

  // Prepare a warning message that will only be issued if there
  // are constrained parameters.
  ostringstream warning;

  int i;
  int k;
  double bOut_i_k;
  double maxDistFromBound_k;

  int colWidth1 = 10 - 2;
  int colWidth2 = 9;
  int colWidth3 = 13 + 2;
  int colWidth4 = 12;
  string colSpacer = "  ";

  warning << "The following individual parameters are at or near their bounds." << endl;
  warning << endl;
  warning << "Individual  Parameter       Value            Bound"      << endl;
  warning << "----------  ---------  ---------------  ---------------" << endl;

  // Check each individual's final parameter value to see if they
  // are constrained by their lower or upper bound;
  bool isAnyBAtOrNearLimit = false;
  bool printIndex;
  for ( i = 0; i < nInd; i++ )
  {
    printIndex = true;

    for ( k = 0; k < nB; k++ )
    {
      // Don't give a warning if the value is constrained by both
      // of its bounds.
      if ( pdBLowData[k] != pdBUpData[k] )
      {
        // Set the maximum distance allowed from either bound.
        maxDistFromBound_k = 
          indOptimizer.getEpsilon() * ( pdBUpData[k] - pdBLowData[k] );
    
        bOut_i_k = pdBOutData[k + i * nB];
    
        // Give a warning if the value is within the maximum distance of
        // either bound.
        if ( bOut_i_k     - pdBLowData[k] <= maxDistFromBound_k ||
             pdBUpData[k] - bOut_i_k      <= maxDistFromBound_k )
        {
          isAnyBAtOrNearLimit = true;
    
          // Column 1.
          warning << setw( colWidth1 );
          if ( printIndex )
          {
            warning << i + 1;
          }
          else
          {
            warning << "";
          }
          warning << colSpacer;
    
          // Column 2.
          warning << setw( colWidth2 ) << k + 1 << colSpacer;
    
          // Column 3.
          warning << setw( colWidth3 ) << scientific 
                  << setprecision( 3 ) << bOut_i_k << colSpacer;
    
          // Column 4.
          warning << colSpacer << colSpacer << setw( colWidth4 );
          if ( bOut_i_k == pdBLowData[k] )
          {
            warning << "Lower (at)  ";
          }
          else if ( bOut_i_k == pdBUpData[k] )
          {
            warning << "Upper (at)  ";
          }
          else if ( pdBUpData[k] - pdBLowData[k] <= maxDistFromBound_k ) 
          {
            warning << "Both (near) ";
          }
          else if ( bOut_i_k - pdBLowData[k] <= maxDistFromBound_k )
          {
            warning << "Lower (near)";
          }
          else
          {
            warning << "Upper (near)";
          }
    
          warning << endl;
    
          printIndex = false;
        }
      }
    }
  }


  //------------------------------------------------------------
  // Issue a warning message if necessary.
  //------------------------------------------------------------

  // Only issue the warning message if at least one of the
  // values is constrained.
  if ( isAnyBAtOrNearLimit )
  {
    string warningStr = warning.str();
    WarningsManager::addWarning( warningStr, __LINE__, __FILE__);
  }
}


/*************************************************************************
 *
 * Function: printTracingInfo
 *
 *
 * Prints information related to the current iteration of the
 * optimization being performed.
 *
 *************************************************************************/

void printTracingInfo( std::ostream&        outputStream,
                       int                  k,
                       const DoubleMatrix&  dvecBLow,
                       const DoubleMatrix&  dvecBUp,
                       const DoubleMatrix&  dmatBCurr,
                       const DoubleMatrix&  dmatBPrev )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  const int nInd = dmatBCurr.nc();
  const int nB   = dvecBLow .nr();

  const double* pdBLowData  = dvecBLow.data();
  const double* pdBUpData   = dvecBUp.data();
  const double* pdBPrevData = dmatBPrev.data();
  const double* pdBCurrData = dmatBCurr.data();


  //------------------------------------------------------------
  // Print the tracing information.
  //------------------------------------------------------------

  int i;
  int j;

  outputStream << "k = " << k << endl;

  for ( i = 0; i < nInd; i++ )
  {
    outputStream << "Individual " << i + 1 << ", x = [";
    for ( j = 0; j < nB; j++ )
    {
      if ( pdBLowData[j] != pdBUpData[j] )
      {
        outputStream << ( pdBCurrData[j + i * nB] - pdBLowData[j] ) /
                          ( pdBUpData[j] -  pdBLowData[j] );
      }
      else
      {
        outputStream << 0.0;
      }
      if ( j < nB - 1 )
      {
        outputStream << ", ";
      }
    }
    outputStream << "], dx = [";
    for ( j = 0; j < nB; j++ )
    {
      if ( pdBLowData[j] != pdBUpData[j] )
      {
        outputStream << ( pdBCurrData[j + i * nB] - pdBPrevData[j + i * nB] ) /
                          ( pdBUpData[j] -  pdBLowData[j] );
      }
      else
      {
        outputStream << 0.0;
      }
      if ( j < nB - 1 )
      {
        outputStream << ", ";
      }
    }
    outputStream << "]" << endl;
  }

}


/*************************************************************************
 *
 * Function: checkOptStatus
 *
 *
 * Checks the status of the values calculated during the optimization.
 *
 * The return value for this function will be true if the optimization
 * converged successfully.
 *
 *************************************************************************/

bool checkOptStatus( Optimizer&           popOptInfo,
                     const Optimizer&     indOptInfo,
                     std::ostream&        outputStream,
                     bool                 isWithinTol,
                     int                  k,
                     const DoubleMatrix&  dvecBLow,
                     const DoubleMatrix&  dvecBUp,
                     const DoubleMatrix&  dmatBCurr,
                     SpkError::ErrorCode  errorCode,
                     std::string&         stringMessage )
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  using namespace std;

  const int nInd = dmatBCurr.nc();
  const int nB   = dvecBLow .nr();

  const double* pdBLowData  = dvecBLow .data();
  const double* pdBUpData   = dvecBUp  .data();
  const double* pdBCurrData = dmatBCurr.data();

  double popEpsilon  = popOptInfo.getEpsilon();
  int    popNMaxIter = popOptInfo.getNMaxIter();
  int    popLevel    = popOptInfo.getLevel();

  double indEpsilon = indOptInfo.getEpsilon();


  //------------------------------------------------------------
  // Check for parameter values that are at or near their bounds.
  //------------------------------------------------------------

  int i;
  int j;

  const string parAtOrNearBoundsMessage = 
    "Note that some parameter values are at or near their bounds.";

  double bOut_i_j;
  double maxDistFromBound_j;

  bool parAtOrNearBounds = false;

  // See if any of the free elements of the parameter is at or near
  // its bounds, i.e., within epsilon.
  if ( !isWithinTol )
  {
    for ( i = 0; i < nInd; i++ )
    {
      for ( j = 0; j < nB; j++ )
      {
        if ( pdBLowData[j] != pdBUpData[j] )
        {
          // Set the maximum distance allowed from either bound.
          maxDistFromBound_j = indEpsilon * ( pdBUpData[j] - pdBLowData[j] );
      
          bOut_i_j = pdBCurrData[j + i * nB];
      
          // Give a warning if the value is within the maximum distance of
          // either bound.
          if ( bOut_i_j     - pdBLowData[j] <= maxDistFromBound_j ||
               pdBUpData[j] - bOut_i_j      <= maxDistFromBound_j )
          {
            bool parAtOrNearBounds = true;
            break;
          }
        }
      }
    }
  }


  //------------------------------------------------------------
  // Check the status of the optimization.
  //------------------------------------------------------------

  bool ok = false;

  if ( isWithinTol )
  {
    //----------------------------------------------------------
    // This function's convergence criterion was satisfied.
    //----------------------------------------------------------

    if ( popLevel > 0 && popNMaxIter > 0 )
    {
      outputStream << endl;
      outputStream << "Optimal parameter values found." << endl;
      outputStream << endl;
    }

    popOptInfo.setIsTooManyIter( false );
    ok = true;
  }
  else if ( k == popNMaxIter + 1 )
  {
    //----------------------------------------------------------
    // The maximum number of iterations have been performed.
    //----------------------------------------------------------

    if ( popLevel > 0 && popNMaxIter > 0 )
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

    popOptInfo.setIsTooManyIter( true );
    if ( popOptInfo.getThrowExcepIfMaxIter() )
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

    if ( popLevel > 0 && popNMaxIter > 0 )
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

    popOptInfo.setIsTooManyIter( false );
    errorCode = SpkError::SPK_NOT_CONVERGED;
    stringMessage = "Unable to find optimal parameter values in twoStageMethod.";
    if ( parAtOrNearBounds )
    {
      stringMessage += "\n" + parAtOrNearBoundsMessage;
    }
    ok = false;
  }

  return ok;
}


} // [End: unnamed namespace]
