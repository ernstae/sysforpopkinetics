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
 * File: popResiduals.cpp
 *
 *
 * Calculates residuals for all of the individuals in the population.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: popResiduals
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin popResiduals$$

$spell
  Enumerator
  Model model
  valarray
  Obj
  enum
  Laplace
  const
  int
  covariance
  ind
  Spk
  subvector
  pred
  res
  resWtd
  Davidian
  Giltinan
  Raton
$$

$section Computing Residuals for All of the Individuals in the Population$$

$index popResiduals$$
$cindex \Computing Residuals \for \All \of \the \Individuals \in \the Population$$

$table
$bold Prototype:$$ $cend
$syntax/void popResiduals( SpkModel&                        /model/,
                   enum Objective                   /objective/,
                   const SPK_VA::valarray<int>&     /nMeasurementsAll/,
                   const SPK_VA::valarray<double>&  /measurementsAll/,
                   const SPK_VA::valarray<double>&  /popPar/,
                   const SPK_VA::valarray<double>&  /indParAll/,
                   SPK_VA::valarray<double>*        /pPopPredOut/,
                   SPK_VA::valarray<double>*        /pPopResOut/,
                   SPK_VA::valarray<double>*        /pPopResWtdOut/,
                   SPK_VA::valarray<double>*        /pPopIndParResOut/,
                   SPK_VA::valarray<double>*        /pPopIndParResWtdOut/ )
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

Calculates approximations for the predicted values, residuals, and/or
weighted residuals for all of the individuals in the population using
an additional approximation for the covariance of an individual's data
$math%VTilde_i%$$ as the weight.
The derivations for these approximations can be found in Sections (6.2) 
and (6.3) of Davidian and Giltinan (1998).
Also, calculates individual parameter residuals and weighted individual 
parameter residuals for all of the individuals in the population using 
the covariance of the individual parameters $math%D%$$ as the weight.
$pre

$$
The approximate predicted values $math%pred_i%$$, residuals $math%res_i%$$, 
and weighted residuals $math%resWtd_i%$$ for the $th i$$ 
individual are calculated as follows:
$math%

    pred   =  f (alp, b )  -  d  f (alp, b )  b   ,
        i      i       i       b  i       i    i 

%$$
$math%

    res    =  y   -  pred   ,
       i       i         i

%$$
and
$math%

                       -1/2
    resWtd   =  VTilde       *  res   ,
          i           i            i

%$$
where the term multiplying the residuals is the matrix square 
root of the inverse of the approximate covariance.
$pre

$$
The approximation used for the covariance is
$math%

                                                                      T
    VTilde   =  R (alp, b )  +  d  f (alp, b )  D(alp)  d  f (alp, b )   .
          i      i       i       b  i       i            b  i       i

%$$     
For the modified First Order objective functions, all of the 
individual parameters are set equal to zero in the approximation 
for the covariance, i.e., $math%b_i = 0%$$.
For the modified Laplace and modified Expected Hessian objective
functions, the individual parameters are set equal to the values
in the input argument $italic indParAll$$.
$pre

$$
The individual parameter residuals $math%bRes_i%$$ and the weighted
individual parameter residuals $math%bResWtd_i%$$ for the $th i$$ 
individual are calculated as follows:
$math%

    bRes   =  - b   ,
        i        i

%$$
and
$math%

                  -1/2
    bResWtd   =  D      * ( - b  )  .
           i                   i

%$$
(The above equations use
$xref/glossary/Population Notation/population notation/$$.)

$head Reference$$
Davidian M. and Giltinan D. M. (1998) $italic Nonlinear Models for 
Repeated Measurement Data$$, Chapman & Hall/CRC, Boca Raton, Florida.

$head Arguments$$
$syntax/
/model/
/$$
This function expects $italic model$$ to be a function of
all three parameters: $math%alp%$$, $math%b%$$ and $math%i%$$.
Refer to $xref/glossary/Model Functions Depend on i - alp - b/Model Functions 
Depend on i - alp - b/$$ for details.

$syntax/

/objective/
/$$
This enumerated type variable specifies which parametric population objective 
function will be minimized:  the modified Laplace, the modified 
Expected Hessian, or the modified First Order.
The permissible values for $italic objective$$ are defined in 
the $xref/Objective//Objective/$$ enumerated type definition.

$syntax/

/nMeasurementsAll/
/$$
The $code SPK_VA::valarray<int>$$ $italic nMeasurementsAll$$ corresponds to 
the array $math%N%$$.  The $th i$$ element of $math%N%$$
specifies the number of elements of $math%y%$$ that
correspond to the $th i$$ individual.
Note that the size of $italic nMeasurementsAll$$ specifies the number of 
individuals in the population, $math%M%$$.

$syntax/

/measurementsAll/
/$$
The $code SPK_VA::valarray<double>$$ $italic measurementsAll$$ contains the vector
$math%y%$$, which specifies the data for all the individuals.
The vector $math%y%$$ has
$math%
    N(1) + N(2) + ... + N(M)
%$$
elements where $math%M%$$ is the number of individuals.  
The data vector corresponding to the first individual is
$math%                                         
    y_1 = [ y(1) , y(2) , ... , y(N(1)) ]
%$$
Elements $math%y(N(1) + 1)%$$ through $math%y(N(1) + N(2))%$$ 
correspond to the second individual and so on.
(Note that $math%y_1%$$ refers to the first subvector of $math%y%$$ while
$math%y(1)%$$ refers to the first element of the valarray $math%y%$$.)

$syntax/

/popPar/
/$$
The $code SPK_VA::valarray<double>$$ $italic popPar$$ contains the vector 
$math%alp%$$, which specifies the values of the population parameters.  
Note that the size of $italic popPar$$ specifies the number of 
population parameters, $math%nAlp%$$.

$syntax/

/indParAll/
/$$
The $code SPK_VA::valarray<double>$$ $italic indParAll$$ contains the matrix 
$math%bAll%$$ in column major order.  
The $th i$$ column of $math%bAll%$$ specifies the values of the 
individual parameters for the $th i$$ individual, $math%b_i%$$.
The column dimension of $math%bAll%$$ is equal to $math%M%$$, 
and its row dimension is equal to the number of individual 
parameters, $math%nB%$$.  
Note that the size of $italic indParAll$$ divided by $math%M%$$ 
specifies $math%nB%$$.

$syntax/

/pPopPredOut/ 
/$$
If $italic pPopPredOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic pPopPredOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the length of the vector that contains all of the 
individuals' data values, $italic measurementsAll$$.  
If $italic pPopPredOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pPopPredOut$$ will contain the vectors of 
predicted values for all of the individuals $math%pred_i%$$ in 
the same order as the data values.  
Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pPopPredOut$$. 

$syntax/

/pPopResOut/ 
/$$
If $italic pPopResOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic pPopResOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the length of the vector that contains all of the 
individuals' data values, $italic measurementsAll$$.  
If $italic pPopResOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pPopResOut$$ will contain the vectors of residuals 
for all of the individuals $math%res_i%$$ in the same order as the data values.  
Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pPopResOut$$. 

$syntax/

/pPopResWtdOut/ 
/$$
If $italic pPopResWtdOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic pPopResWtdOut$$ 
must be declared in the function that calls this function, and its size must 
be equal to the length of the vector that contains all of the 
individuals' data values, $italic measurementsAll$$.  
If $italic pPopResWtdOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pPopResWtdOut$$ will contain the vectors of weighted
residuals for all of the individuals $math%resWtd_i%$$ in the 
same order as the data values.  
Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pPopResWtdOut$$. 

$syntax/

/pPopIndParResOut/ 
/$$

If $italic pPopIndParResOut$$ is not $code NULL$$, then the $code
SPK_VA::valarray<double>$$ object pointed to by $italic
pPopIndParResOut$$ must be declared in the function that calls this
function, and its size must be equal to $math%nB%$$ times the number
of individuals in the population $math%M%$$.
If $italic pPopIndParResOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pPopIndParResOut$$ will contain the vectors of 
individual parameter residuals for all of the individuals $math%bRes_i%$$.
Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pPopIndParResOut$$. 

$syntax/

/pPopIndParResWtdOut/ 
/$$

If $italic pPopIndParResWtdOut$$ is not $code NULL$$, then the $code
SPK_VA::valarray<double>$$ object pointed to by $italic
pPopIndParResWtdOut$$ must be declared in the function that calls this
function, and its size must be equal to $math%nB%$$ times the number
of individuals in the population $math%M%$$.
If $italic pPopIndParResWtdOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pPopIndParResWtdOut$$ will contain the vectors of 
weighted individual parameter residuals for all of the individuals 
$math%bResWtd_i%$$.
Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pPopIndParResWtdOut$$. 

$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include "add.h"
#include "intToOrdinalString.h"
#include "multiply.h"
#include "popResiduals.h"
#include "SpkException.h"
#include "SpkValarray.h"
#include "transpose.h"
#include "symmetrize.h"
#include "wres.h"

// Standard library header files.
#include <cmath>
#include <iostream>

using SPK_VA::valarray;
using namespace std;


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void popResiduals( SpkModel&                model,
                   enum Objective           objective,
                   const valarray<int>&     nMeasurementsAll,
                   const valarray<double>&  measurementsAll,
                   const valarray<double>&  popPar,
                   const valarray<double>&  indParAll,
                   valarray<double>*        pPopPredOut,
                   valarray<double>*        pPopResOut,
                   valarray<double>*        pPopResWtdOut,
                   valarray<double>*        pPopIndParResOut,
                   valarray<double>*        pPopIndParResWtdOut )
{
  //----------------------------------------------------------------
  // Preliminaries.
  //----------------------------------------------------------------

  using namespace std;

  // Return if there are no output values to calculate.
  if ( pPopPredOut         == 0 && 
       pPopResOut          == 0 && 
       pPopResWtdOut       == 0 && 
       pPopIndParResOut    == 0 && 
       pPopIndParResWtdOut == 0 )
  {
    return;
  }

  const int nInd = nMeasurementsAll.size();
  const int nY   = measurementsAll .size();
  const int nAlp = popPar          .size();
  const int nB   = indParAll       .size() / nInd;


  //----------------------------------------------------------------
  // Validate the inputs.
  //----------------------------------------------------------------

  int i;

  if ( nY != nMeasurementsAll.sum() )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR, 
      "The sum of the number of data values for each individual does not agree with the total number of data values.",
      __LINE__,
      __FILE__ );
  }

  for ( i = 0; i < nInd; i++ )
  {
    if ( nMeasurementsAll[i] < 0 )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR,  
        "The number of data values for one of the individuals is less than zero.",
        __LINE__,
        __FILE__ );
    }
  }

  if ( pPopPredOut )
  {
    if ( nY != pPopPredOut->size() )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The vector of predicted values has the wrong size.",
        __LINE__,
        __FILE__ );
    }
  }

  if ( pPopResOut )
  {
    if ( nY != pPopResOut->size() )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The vector of residuals has the wrong size.",
        __LINE__,
        __FILE__ );
    }
  }

  if ( pPopResWtdOut )
  {
    if ( nY != pPopResWtdOut->size() )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The vector of weighted residuals has the wrong size.",
        __LINE__,
        __FILE__ );
    }
  }

  if ( pPopIndParResOut )
  {
    if ( nB * nInd != pPopIndParResOut->size() )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The vector of individual parameter residuals has the wrong size.",
        __LINE__,
        __FILE__ );
    }
  }

  if ( pPopIndParResWtdOut )
  {
    if ( nB * nInd != pPopIndParResWtdOut->size() )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The vector of weighted individual parameter residuals has the wrong size.",
        __LINE__,
        __FILE__ );
    }
  }


  //----------------------------------------------------------------
  // Prepare the output values.
  //----------------------------------------------------------------

  // If this function is going to return the predicted values,
  // initialize the temporary array to hold them.  Otherwise, set the
  // temporary pointer to zero so that they will not be calculated.
  valarray<double> popPredTemp;
  valarray<double>* pPopPredTemp = &popPredTemp;
  if ( pPopPredOut )
  {
    popPredTemp.resize( nY );
  }
  else
  {
    pPopPredTemp = 0;
  }

  // If this function is going to return the residuals, initialize the
  // temporary arrays to hold them.  Otherwise, set the temporary
  // pointer to zero so that they will not be calculated.
  valarray<double> popResTemp;
  valarray<double>* pPopResTemp = &popResTemp;
  valarray<double> res_i;
  valarray<double>* pRes_i = &res_i;
  if ( pPopResOut )
  {
    popResTemp.resize( nY );
  }
  else
  {
    pPopResTemp = 0;
    pRes_i      = 0;
  }

  // If this function is going to return the weighted residuals,
  // initialize the temporary arrays to hold them.  Otherwise, set the
  // temporary pointer to zero so that they will not be calculated.
  valarray<double> popResWtdTemp;
  valarray<double>* pPopResWtdTemp = &popResWtdTemp;
  valarray<double> resWtd_i;
  valarray<double>* pResWtd_i = &resWtd_i;
  if ( pPopResWtdOut )
  {
    popResWtdTemp.resize( nY );
  }
  else
  {
    pPopResWtdTemp = 0;
    pResWtd_i      = 0;
  }

  // If this function is going to return the individual parameter
  // residuals, initialize the temporary arrays to hold them.
  // Otherwise, set the temporary pointers to zero so that they will
  // not be calculated.
  valarray<double> popIndParResTemp;
  valarray<double>* pPopIndParResTemp = &popIndParResTemp;
  valarray<double> indParRes_i;
  valarray<double>* pIndParRes_i = &indParRes_i;
  if ( pPopIndParResOut )
  {
    popIndParResTemp.resize( nB * nInd );
    indParRes_i     .resize( nB );
  }
  else
  {
    pPopIndParResTemp = 0;
    pIndParRes_i      = 0;
  }

  // If this function is going to return the weighted individual
  // parameter residuals, initialize the temporary arrays to hold
  // them.  Otherwise, set the temporary pointers to zero so that 
  // they will not be calculated.
  valarray<double> popIndParResWtdTemp;
  valarray<double>* pPopIndParResWtdTemp = &popIndParResWtdTemp;
  valarray<double> indParResWtd_i;
  valarray<double>* pIndParResWtd_i = &indParResWtd_i;
  if ( pPopIndParResWtdOut )
  {
    popIndParResWtdTemp.resize( nB * nInd );
    indParResWtd_i     .resize( nB );
  }
  else
  {
    pPopIndParResWtdTemp = 0;
    pIndParResWtd_i      = 0;
  }


  //----------------------------------------------------------------
  // Calculate the residuals for all of the individuals.
  //----------------------------------------------------------------

  // Set the current population parameter.
  model.setPopPar( popPar );

  // For the first order objectives, the individual parameters 
  // are all set equal to zero.
  valarray<double> b_i( nB );
  if ( objective == FIRST_ORDER || objective == NAIVE_FIRST_ORDER  )
  {
    b_i = 0.0;
  }

  // If necessary, evaluate
  //
  //     D(alp)  .
  //
  valarray<double> D( nB * nB );
  if ( pPopResWtdOut || pPopIndParResWtdOut )
  {
    model.indParVariance( D );
  }

  valarray<double> zeroes( nB );
  zeroes = 0.0;

  valarray<double> y_i;
  valarray<double> f_i;
  valarray<double> f_i_b;
  valarray<double> f_i_bTranspose;
  valarray<double> R_i;
  valarray<double> VTilde_i;
  valarray<double> pred_i;
  valarray<double> temp1;
  valarray<double> temp2;
  valarray<double> temp3;

  int nY_iTotal = 0;
  int nY_i;

  // Calculate the residuals and weighted residuals for all of the
  // individuals in the population.
  for ( i = 0; i < nInd; i++ )
  {
    try
    {
      //------------------------------------------------------------
      // Prepare the current individual.
      //------------------------------------------------------------
  
      // Set the current individual.
      model.selectIndividual( i );
  
      // Get the number of data values for this individual.
      nY_i = nMeasurementsAll[i];
  
      // Get this individual's parameter if the objective is not 
      // one of the first order objectives.
      if ( objective != FIRST_ORDER && objective != NAIVE_FIRST_ORDER  )
      {
        b_i = indParAll[ slice( i * nB, nB, 1 ) ];
      }
  
      // Set the current individual parameter.
      model.setIndPar( b_i );
  
  
      //------------------------------------------------------------
      // Evaluate the model functions.
      //------------------------------------------------------------
  
      // Evaluate
      //
      //     f (alp, b )  .
      //      i       i
      //
      model.dataMean( f_i );
  
      // Evaluate
      //
      //     d  f (alp, b )  .
      //      b  i       i
      //
      bool notAllZeroF_i_b = model.dataMean_indPar( f_i_b );
  
      // Evaluate
      //
      //     R (alp, b )  .
      //      i       i
      //
      model.dataVariance( R_i );
  
  
      //------------------------------------------------------------
      // Calculate the predicted values for this individual's data.
      //------------------------------------------------------------
  
      pred_i.resize( nY_i );
      temp1 .resize( nY_i );
  
      // Calculate the predicted value for this individual's data,
      //
      //     pred   =  f (alp, b )  -  d  f (alp, b )  b   .
      //         i      i       i       b  i       i    i 
      //
      // Note that this is the expected value for the first order
      // approximation for the model, i.e.,
      //
      //     E  [ f (alp, b ) ]  ~  f (alp, b )  -  d  f (alp, b )  b   .
      //      b    i       i         i       i       b  i       i    i
      //
      // This approximation is based on Eq. (6.19) and the discussion on
      // pp. 164-6 from Davidian M. and Giltinan D. M. (1998) "Nonlinear
      // Models for Repeated Measurement Data", Chapman & Hall/CRC, Boca
      // Raton, Florida.  When b_i equals zero as in the first order 
      // objective, this approximation is equivalent to Eq. (6.4) and is
      // based on the discussion on pp. 152-4 from the same book.
      temp1 = multiply( f_i_b, nB, b_i, 1 );
      pred_i = f_i - temp1;
  
  
      //------------------------------------------------------------
      // Calculate the covariance of this individual's data.
      //------------------------------------------------------------
  
      assert( R_i.size() == nY_i * nY_i );
  
      VTilde_i.resize( nY_i * nY_i );
  
      // Calculate
      //
      //     VTilde   =  R (alp, b )
      //           i      i       i
      //                                                 T
      //        +  d  f (alp, b )  D(alp)  d  f (alp, b )   .
      //            b  i       i            b  i       i
      //
      // This approximation is also based on Eq. (6.19) from 
      // Davidian and Giltinan (1998).
      if ( notAllZeroF_i_b )
      {
        assert( f_i_b.size() == nY_i * nB );
        assert( D.size()     == nB   * nB );
  
        f_i_bTranspose.resize( nB   * nY_i );
        temp1         .resize( nB   * nY_i );
        temp2         .resize( nY_i * nY_i );
        temp3         .resize( nY_i * nY_i );
  
        f_i_bTranspose = transpose( f_i_b, nB );
  
        temp1 = multiply( D,   nB, f_i_bTranspose, nY_i );
        temp2 = multiply( f_i_b, nB, temp1,        nY_i );
  
        temp3 = R_i + temp2;
  
        // Make sure that the calculated covariance is symmetric.
        symmetrize( temp3, nY_i, VTilde_i );
      }
      else
      {
        VTilde_i = R_i;
      }
  
  
      //------------------------------------------------------------
      // Calculate their residuals and/or weighted residuals.
      //------------------------------------------------------------
  
      // Get this invidividual's data vector.
      y_i.resize( nY_i );
      y_i = measurementsAll[ slice( nY_iTotal, nY_i, 1 ) ];
  
      // Prepare this individual's residuals, if necessary.
      if ( pPopResOut )
      {
        res_i.resize( nY_i );
      }
  
      // Prepare this individual's weighted residuals, if necessary.
      if ( pPopResWtdOut )
      {
        resWtd_i.resize( nY_i );
      }
  
      // Calculate this individual's residuals and weighted residuals.
      wres( y_i, pred_i, VTilde_i, pRes_i, pResWtd_i );
  
      // Set this individual's predicted values, if necessary.
      if ( pPopPredOut )
      {
        popPredTemp[ slice( nY_iTotal, nY_i, 1 ) ] = pred_i;
      }
      
      // Set this individual's residuals, if necessary.
      if ( pPopResOut )
      {
        popResTemp[ slice( nY_iTotal, nY_i, 1 ) ] = res_i;
      }
      
      // Set this individual's weighted residuals, if necessary.
      if ( pPopResWtdOut )
      {
        popResWtdTemp[ slice( nY_iTotal, nY_i, 1 ) ] = resWtd_i;
      }
  
      nY_iTotal += nY_i;
  
  
      //------------------------------------------------------------
      // Calculate their individual parameter residuals and weighted residuals.
      //------------------------------------------------------------
  
      // Calculate this individual's individual parameter residuals
      // and weighted individual parameter residuals.
      wres( zeroes, b_i, D, pIndParRes_i, pIndParResWtd_i );
  
      // Set this individual's individual parameter residuals, if
      // necessary.
      if ( pPopIndParResOut )
      {
        popIndParResTemp[ slice( i * nB, nB, 1 ) ] = indParRes_i;
      }
  
      // Set this individual's weighted individual parameter residuals,
      // if necessary.
      if ( pPopIndParResWtdOut )
      {
        popIndParResWtdTemp[ slice( i * nB, nB, 1 ) ] = indParResWtd_i;
      }

    }
    catch( SpkException& e )
    {         
      const int max = SpkError::maxMessageLen();
      char message[max];
      snprintf( message, max, "The population residuals calculation failed for the %s individual.",
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
      snprintf( message, max, "The population residuals calculation failed for the %s individual because \na standard exception was thrown.",
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
      snprintf( message, max, "The population residuals calculation failed for the %s individual because \an unknown exception was thrown.",
               intToOrdinalString( i, ZERO_IS_FIRST_INT ).c_str() );

      throw SpkException(
        SpkError::SPK_UNKNOWN_ERR,
        message,
        __LINE__,
        __FILE__ );
    }

  }


  //----------------------------------------------------------------
  // Set the output values.
  //----------------------------------------------------------------

  // Set the predicted values for all of the individuals, if necessary.
  if ( pPopPredOut )
  {
    *pPopPredOut = popPredTemp;
  }

  // Set the residuals for all of the individuals, if necessary.
  if ( pPopResOut )
  {
    *pPopResOut = popResTemp;
  }

  // Set the weighted residuals for all of the individuals, if necessary.
  if ( pPopResWtdOut )
  {
    *pPopResWtdOut = popResWtdTemp;
  }

  // Set the individual parameter residuals for all of the
  // individuals, if necessary.
  if ( pPopIndParResOut )
  {
    *pPopIndParResOut = popIndParResTemp;
  }

  // Set the weighted individual parameter residuals for all of the
  // individuals, if necessary.
  if ( pPopIndParResWtdOut )
  {
    *pPopIndParResWtdOut = popIndParResWtdTemp;
  }

}

