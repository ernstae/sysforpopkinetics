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
 * File: indResiduals.cpp
 *
 *
 * Calculates residuals and/or weighted residuals for an individual.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: indResiduals
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/
/*

$begin indResiduals$$

$spell
  Model model
  valarray
  const
  int
  covariance
  ind
  Spk
  pred
  res
  resWtd
$$

$section Calculating Predicted Values, Residuals, Weighted Residuals, and Weighted Individual Parameters for an Individual$$

$index indResiduals$$
$cindex /Calculating Predicted /Values\, Residuals\, Weighted /Residuals\, /and Weighted /Individual /Parameters /for /an Individual$$

$table
$bold Prototype:$$ $cend
$syntax/void indResiduals( SpkModel&                        /model/,
                   const SPK_VA::valarray<double>&  /measurements/,
                   const SPK_VA::valarray<double>&  /indPar/,
                   SPK_VA::valarray<double>*        /pIndPredOut/,
                   SPK_VA::valarray<double>*        /pIndResOut/,
                   SPK_VA::valarray<double>*        /pIndResWtdOut/,
                   SPK_VA::valarray<double>*        /pIndParWtdOut/,
                   bool                             /withD/ )
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

Calculates predicted values, residuals and/or weighted residuals for an 
individual using the covariance of the individual's data $math%R(b)%$$ as 
the weight.
Also, calculates weighted individual parameters for an individual
using the covariance of the individual parameters $math%D%$$ as the
weight.
$pre

$$
The predicted values $math%pred%$$, residuals $math%res%$$, and weighted 
residuals $math%resWtd%$$ for the individual are calculated as follows:
$math%

    pred  =  f(b)  ,

%$$
$math%

    res  =  y  -  pred  ,

%$$
and
$math%

                   -1/2
    resWtd  =  R(b)      *  res   ,

%$$
where the term multiplying the residuals is the matrix square 
root of the inverse of the covariance.
$pre

$$
The weighted individual parameters $math%bWtd%$$ for the individual 
are calculated as follows:
$math%

              -1/2
    bWtd  =  D      *  b   .

%$$
(The equations above use
$xref/glossary/Individual Notation/individual notation/$$.)

$head Arguments$$
$syntax/
/model/
/$$
This function expects $italic model$$ to be a function of $math%b%$$.
Refer to $xref/glossary/Model Functions Depend on only b/Model Functions Depend on only b/$$
for details.

$syntax/

/measurements/
/$$
The $code SPK_VA::valarray<double>$$ $italic measurements$$ contains 
the vector $math%y%$$, which specifies the data for the individual.
Note that the size of $italic measurements$$ specifies the number of 
data values, $math%nY%$$.

$syntax/

/indPar/
/$$
The $code SPK_VA::valarray<double>$$ $italic indPar$$ contains the vector 
$math%b%$$, which specifies the values of the individual parameters.  
Note that the size of $italic indPar$$ specifies the number of 
individual parameters, $math%nB%$$.

$syntax/

/pIndPredOut/ 
/$$

If $italic pIndPredOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic pIndPredOut$$ 
must be declared in the function that calls this function, and its size 
must be equal to $math%nY%$$.  
If $italic pIndPredOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pIndPredOut$$ will contain the vector of predicted
values for this individual's data $math%pred%$$ in the same order as the 
data values.  
Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pIndPredOut$$. 

$syntax/

/pIndResOut/ 
/$$

If $italic pIndResOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic pIndResOut$$ 
must be declared in the function that calls this function, and its size 
must be equal to $math%nY%$$.  
If $italic pIndResOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pIndResOut$$ will contain the vector of residuals 
for this individual $math%res%$$ in the same order as the data values.  
Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pIndResOut$$. 

$syntax/

/pIndResWtdOut/ 
/$$

If $italic pIndResWtdOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic pIndResWtdOut$$ 
must be declared in the function that calls this function, and its size 
must be equal to $math%nY%$$.  
If $italic pIndResWtdOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pIndResWtdOut$$ will contain the vector of weighted
residuals for this individual $math%resWtd%$$ in the same order as the 
data values.  
Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pIndResWtdOut$$. 

$syntax/

/pIndParWtdOut/ 
/$$

If $italic pIndParWtdOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic pIndParWtdOut$$ 
must be declared in the function that calls this function, and its size 
must be equal to $math%nB%$$.  
If $italic pIndParWtdOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pIndParWtdOut$$ will contain the vector of weighted
individual parameters for this individual $math%bWtd%$$.
Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pIndParWtdOut$$. 
$pre

$$
These weighted individual parameters may only be calculated if the
input argument $italic withD$$ is equal to $math%true%$$.

$syntax/

/withD/
/$$
If this flag is set to $math%true%$$, then the objective function that
was used to calculate $italic indPar$$ was the Map Bayesian objective,
i.e., the terms involving the matrix $math%D%$$ have been included in
$math%MapObj(b)%$$.
In this case, the weighted individual parameter $math%bWtd%$$ can be 
calculated.

$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include "indResiduals.h"
#include "SpkException.h"
#include "SpkValarray.h"
#include "wres.h"

// Standard library header files.
#include <iostream>

using SPK_VA::valarray;
using namespace std;


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void indResiduals( SpkModel&                model,
                   const valarray<double>&  measurements,
                   const valarray<double>&  indPar,
                   valarray<double>*        pIndPredOut,
                   valarray<double>*        pIndResOut,
                   valarray<double>*        pIndResWtdOut,
                   valarray<double>*        pIndParWtdOut,
                   bool                     withD )
{
  //----------------------------------------------------------------
  // Preliminaries.
  //----------------------------------------------------------------

  using namespace std;

  // Return if there are no output values to calculate.
  if ( pIndPredOut == 0 && pIndResOut == 0 && pIndResWtdOut == 0 && pIndParWtdOut == 0 )
  {
    return;
  }

  const int nY = measurements.size();
  const int nB = indPar      .size();


  //----------------------------------------------------------------
  // Validate the inputs.
  //----------------------------------------------------------------

  if ( pIndPredOut )
  {
    if ( nY != pIndPredOut->size() )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The vector of predicted values has the wrong size.",
        __LINE__,
        __FILE__ );
    }
  }

  if ( pIndResOut )
  {
    if ( nY != pIndResOut->size() )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The vector of residuals has the wrong size.",
        __LINE__,
        __FILE__ );
    }
  }

  if ( pIndResWtdOut )
  {
    if ( nY != pIndResWtdOut->size() )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The vector of weighted residuals has the wrong size.",
        __LINE__,
        __FILE__ );
    }
  }

  if ( pIndParWtdOut )
  {
    if ( nB != pIndParWtdOut->size() )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The vector of weighted individual parameters has the wrong size.",
        __LINE__,
        __FILE__ );
    }
  }

  if ( pIndParWtdOut && !withD )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR, 
      "The vector of weighted individual parameters can only be calculated for the Map Bayesian objective.",
      __LINE__,
      __FILE__ );
  }


  //----------------------------------------------------------------
  // Prepare the output values.
  //----------------------------------------------------------------

  // If this function is going to return the residuals, initialize the
  // temporary array to hold them.  Otherwise, set the temporary
  // pointer to zero so that they will not be calculated.
  valarray<double> indResTemp;
  valarray<double>* pIndResTemp = &indResTemp;
  if ( pIndResOut )
  {
    indResTemp.resize( nY );
  }
  else
  {
    pIndResTemp = 0;
  }

  // If this function is going to return the weighted residuals,
  // initialize the temporary array to hold them.  Otherwise, set the
  // temporary pointer to zero so that they will not be calculated.
  valarray<double> indResWtdTemp;
  valarray<double>* pIndResWtdTemp = &indResWtdTemp;
  if ( pIndResWtdOut )
  {
    indResWtdTemp.resize( nY );
  }
  else
  {
    pIndResWtdTemp = 0;
  }

  // It is not necessary to calculate the individual paramter
  // residuals, i.e., their difference from zero.
  valarray<double>* pIndParResTemp = 0;

  // If this function is going to return the weighted individual parameter,
  // initialize the temporary array to hold them.  Otherwise, set the
  // temporary pointer to zero so that they will not be calculated.
  valarray<double> indParWtdTemp;
  valarray<double>* pIndParWtdTemp = &indParWtdTemp;
  if ( pIndParWtdOut )
  {
    indParWtdTemp.resize( nB );
  }
  else
  {
    pIndParWtdTemp = 0;
  }


  //----------------------------------------------------------------
  // Prepare the model.
  //----------------------------------------------------------------

  // Set the current individual paramter.
  model.setIndPar( indPar );


  //----------------------------------------------------------------
  // Calculate the residuals and weighted residuals.
  //----------------------------------------------------------------

  valarray<double> f( nY );

  if ( pIndPredOut || pIndResOut ||  pIndResWtdOut )
  {
    valarray<double> R( nY * nY );

    // Evaluate
    //
    //     f(b)  .
    //
    model.dataMean( f );
    
    // Evaluate
    //
    //     R(b)  .
    //
    model.dataVariance( R );

    // Calculate this individual's residuals and/or weighted residuals.
    wres( measurements, f, R, pIndResTemp, pIndResWtdTemp );
  }


  //----------------------------------------------------------------
  // Calculate the weighted individual parameters.
  //----------------------------------------------------------------

  if ( pIndParWtdOut )
  {
    valarray<double> zeroes( nB );
    valarray<double> D     ( nB * nB );

    // Evaluate
    //
    //     D(alpha)  .
    //
    model.indParVariance( D );

    // Calculate the weighted individual parameters.
    wres( zeroes, indPar, D, pIndParResTemp, pIndParWtdTemp );
  }


  //----------------------------------------------------------------
  // Set the output values.
  //----------------------------------------------------------------

  // Set the predicted values for this individual, if necessary.
  if ( pIndPredOut )
  {
    *pIndPredOut = f;
  }

  // Set the residuals for this individual, if necessary.
  if ( pIndResOut )
  {
    *pIndResOut = indResTemp;
  }

  // Set the weighted residuals for this individual, if necessary.
  if ( pIndResWtdOut )
  {
    *pIndResWtdOut = indResWtdTemp;
  }

  // Set the weighted individual parameters for this individual, if
  // necessary.
  if ( pIndParWtdOut )
  {
    *pIndParWtdOut = indParWtdTemp;
  }

}

