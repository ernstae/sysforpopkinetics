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
 * Calculates residuals for an individual.
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

$section Computing Residuals for an Individual$$

$index indResiduals$$
$cindex \Computing Residuals \for \an Individual$$

$table
$bold Prototype:$$ $cend
$syntax/void indResiduals( SpkModel<double>&        /model/,
                   const SPK_VA::valarray<double>&  /measurements/,
                   const SPK_VA::valarray<double>&  /indPar/,
                   SPK_VA::valarray<double>*        /pIndPredOut/,
                   SPK_VA::valarray<double>*        /pIndResOut/,
                   SPK_VA::valarray<double>*        /pIndResWtdOut/,
                   SPK_VA::valarray<double>*        /pIndParResOut/,
                   SPK_VA::valarray<double>*        /pIndParResWtdOut/ )
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
Also, calculates individual parameter residuals and weighted individual 
parameter residuals for an individual using the covariance of the 
individual parameters $math%D%$$ as the weight.
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
The individual parameter residuals $math%bRes%$$ and the weighted
individual parameter residuals $math%bResWtd%$$ for the individual 
are calculated as follows: 
$math%

    bRes  =  - b  ,

%$$
and
$math%

                 -1/2
    bResWtd  =  D      * ( - b )  .

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
If weighted individual parameter residuals are going to be calculated,
then the function $tref SpkModel_indParVariance$$ must be defined 
for this model in order to calculate the covariance of the 
individual parameters $math%D%$$.

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

/pIndParResOut/ 
/$$

If $italic pIndParResOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic pIndParResOut$$ 
must be declared in the function that calls this function, and its size 
must be equal to $math%nB%$$.  
If $italic pIndParResOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pIndParResOut$$ will contain the vector of 
individual parameter residuals for this individual $math%bRes%$$.
Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pIndParResOut$$. 

$syntax/

/pIndParResWtdOut/ 
/$$

If $italic pIndParResWtdOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic pIndParResWtdOut$$ 
must be declared in the function that calls this function, and its size 
must be equal to $math%nB%$$.  
If $italic pIndParResWtdOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pIndParResWtdOut$$ will contain the vector of weighted
individual parameter residuals for this individual $math%bResWtd%$$.
Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pIndParResWtdOut$$. 
$pre

$$
These weighted individual parameter residuals may only be calculated 
if the function $tref SpkModel_indParVariance$$ is defined for the 
input argument $italic model$$.

$end
*/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include "identity.h"
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

void indResiduals( SpkModel<double>&        model,
                   const valarray<double>&  measurements,
                   const valarray<double>&  indPar,
                   valarray<double>*        pIndPredOut,
                   valarray<double>*        pIndResOut,
                   valarray<double>*        pIndResWtdOut,
                   valarray<double>*        pIndParResOut,
                   valarray<double>*        pIndParResWtdOut )
{
  //----------------------------------------------------------------
  // Preliminaries.
  //----------------------------------------------------------------

  using namespace std;

  // Return if there are no output values to calculate.
  if ( pIndPredOut      == 0 && 
       pIndResOut       == 0 && 
       pIndResWtdOut    == 0 && 
       pIndParResOut    == 0 && 
       pIndParResWtdOut == 0 )
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

  if ( pIndParResOut )
  {
    if ( nB != pIndParResOut->size() )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The vector of individual parameter residuals has the wrong size.",
        __LINE__,
        __FILE__ );
    }
  }

  if ( pIndParResWtdOut )
  {
    if ( nB != pIndParResWtdOut->size() )
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

  // If this function is going to return the individual parameter
  // residuals, initialize the temporary array to hold them.
  // Otherwise, set the temporary pointer to zero so that they will
  // not be calculated.
  valarray<double> indParResTemp;
  valarray<double>* pIndParResTemp = &indParResTemp;
  if ( pIndParResOut )
  {
    indParResTemp.resize( nB );
  }
  else
  {
    pIndParResTemp = 0;
  }

  // If this function is going to return the weighted individual
  // parameter residuals, initialize the temporary array to hold them.
  // Otherwise, set the temporary pointer to zero so that they will
  // not be calculated.
  valarray<double> indParResWtdTemp;
  valarray<double>* pIndParResWtdTemp = &indParResWtdTemp;
  if ( pIndParResWtdOut )
  {
    indParResWtdTemp.resize( nB );
  }
  else
  {
    pIndParResWtdTemp = 0;
  }


  //----------------------------------------------------------------
  // Prepare the model.
  //----------------------------------------------------------------

  // Set the current individual parameter.
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
  // Calculate the individual parameter residuals and weighted residuals.
  //----------------------------------------------------------------

  if ( pIndParResOut || pIndParResWtdOut )
  {
    valarray<double> D( nB * nB );

    // Evaluate
    //
    //     D(alpha)  .
    //
    if ( pIndParResWtdOut )
    {
      model.indParVariance( D );
    }
    else
    {
      // If the weighted individual parameter residuals are not going
      // to be calculated, then D is not needed.  So, just set it
      // equal to an identity matrix with the right dimensions.
      identity( nB, D );
    }

    valarray<double> zeroes( nB );
    zeroes = 0.0;

    // Calculate the individual parameter residuals and/or weighted
    // individual parameter residuals.
    wres( zeroes, indPar, D, pIndParResTemp, pIndParResWtdTemp );
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

  // Set the individual parameter residuals for this individual, if
  // necessary.
  if ( pIndParResOut )
  {
    *pIndParResOut = indParResTemp;
  }

  // Set the weighted individual parameter residuals for this
  // individual, if necessary.
  if ( pIndParResWtdOut )
  {
    *pIndParResWtdOut = indParResWtdTemp;
  }

}

