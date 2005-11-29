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
 * File: wres.cpp
 *
 *
 * Calculates residuals and weighted residuals.
 *
 * Author: Sachiko Honda
 *
 * Modified later by: Mitch Watrous
 *
 *************************************************************************/

/*************************************************************************
 *
 * Function: wres
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Function Specification
 *------------------------------------------------------------------------*/

/*
$begin wres$$
$spell
  cov
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
  iostream
  namespace
  std
  cout
  endl
  Spk
  subvector
  res
  wres
$$

$section Calculating Residuals and Weighted Residuals$$

$index wres$$
$index residual, weighted$$

$table
$bold Prototype:$$   $cend  
$syntax/void wres( const SPK_VA::valarray<double>& /y/,
                   const SPK_VA::valarray<double>& /f/,
                   const SPK_VA::valarray<double>& /cov/,
                   SPK_VA::valarray<double>*       /pResOut/,
                   SPK_VA::valarray<double>*       /pWresOut/ )/$$
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
Calculates residuals and weighted residuals using the covariance 
of the data $math%cov%$$ as the weight.
$pre

$$
The residuals $math%res%$$ and weighted residuals $math%wres%$$ 
are calculated as follows:
$math%

    res  =  y - f

%$$
and
$math%

                -1/2
    wres  =  cov     * res  ,

%$$

where the term multiplying the residuals is the matrix square 
root of the inverse of the covariance.

$head Arguments$$

$syntax/
&/y/
/$$
contains the actual measurement values to be fitted.
The size of $math%y%$$ is $math%n%$$, where $math%n >= 0%$$.
If $math%y%$$ were empty, the function returns immediately.

$syntax/

&/f/
/$$
contains the predicted values calculated by the model.
The size of $math%f%$$ must be equal to $math%n%$$.

$syntax/

&/cov/
/$$
represents a matrix in the column major order that contains
the covariance of the measurement data.  The matrix, therefore,
is assumed to have the positive definite property.
The size of $math%cov%$$ must be equal to $math%n^2%$$.

$syntax/

/pResOut/ 
/$$
If $italic pResOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic pResOut$$ 
must be declared in the function that calls this function, 
and its size must be equal to $italic n$$.  
If $italic pResOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pResOut$$ will contain the vector of residuals 
$math%res%$$ in the same order as the data values.  
Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pResOut$$. 

$syntax/

/pWresOut/ 
/$$
If $italic pWresOut$$ is not $code NULL$$, then the 
$code SPK_VA::valarray<double>$$ object pointed to by $italic pWresOut$$ 
must be declared in the function that calls this function, 
and its size must be equal to $italic n$$.  
If $italic pWresOut$$ is not $code NULL$$ and this function 
completed successfully, then the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pWresOut$$ will contain the vector of weighted
residuals $math%wres%$$ in the same order as the data values.  
Otherwise, this function will not attempt to change the 
contents of the $code SPK_VA::valarray<double>$$ object 
pointed to by $italic pWresOut$$. 

$head Example$$
If you compile, link, and run the following program:
$codep

    #include <iostream>
    #include <valarray>
    #include <spk/wres.h>

    using namespace std;
    void main()
    {
       int n = 2;

       //
       // y = [ 1.1  2.2 ]
       //
       double yIn[] = { 1.1, 2.2 };
       valarray<double> y( yIn, n );

       //
       // f = [ 1.0  2.0 ]
       //
       double fIn[] = { 1.0, 2.0 };
       valarray<double> f( fIn, n );

       //
       //       /            \
       //       |  1.0  0.0  |
       // cov = |            |
       //       |  0.0  4.0  |
       //       \            /
       //
       double covIn[] = { 1.0, 0.0, 0.0, 4.0 };
       valarray<double> cov( covIn, n*n );

       valarray<double> resOut(n);
       valarray<double> wresOut(n);

       wres( y, f, cov, &resOut, &wresOut );

       cout << "res  = " <<  resOut << endl;
       cout << "wres = " << wresOut << endl;
    }

$$
then it will display the following when it is run:
$codep

    res  = { 0.1, 0.2 }
    wres = { 0.1, 0.4 }
$$
$end
*/


/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include "inverse.h"
#include "multiply.h"
#include "transpose.h"
#include "SpkError.h"
#include "SpkException.h"
#include "SpkValarray.h"
#include "wres.h"

// LAPACK C interface headers.
#include "c2dsyev.h"

// Standard header files.
#include <cmath>

using SPK_VA::valarray;
using namespace std;


/*------------------------------------------------------------------------
 * Function definition
 *------------------------------------------------------------------------*/

void wres( const valarray<double>& y, 
           const valarray<double>& f,
           const valarray<double>& cov,
           valarray<double>*       pResOut,
           valarray<double>*       pWresOut )
{
  //----------------------------------------------------------------
  // Preliminaries.
  //----------------------------------------------------------------

  using namespace std;

  // Return if there are no output values to calculate.
  if( pResOut == 0 && pWresOut == 0 )
  {
    return;
  }

  int n = y.size();


  //----------------------------------------------------------------
  // Validate the inputs.
  //----------------------------------------------------------------

  if ( f.size() != n )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR, 
      "The number of model predicted values for the data does not match the number \nof data values.",
      __LINE__,
      __FILE__ );
  }

  if ( cov.size() != n * n )
  {
    throw SpkException(
      SpkError::SPK_USER_INPUT_ERR, 
      "The dimensions of the covariance of the data are not consistent with the number \nof data values.",
      __LINE__,
      __FILE__ );
  }

  if ( pResOut )
  {
    if ( n != pResOut->size() )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The vector of residuals has the wrong size.",
        __LINE__,
        __FILE__ );
    }
  }

  if ( pWresOut )
  {
    if ( n != pWresOut->size() )
    {
      throw SpkException(
        SpkError::SPK_USER_INPUT_ERR, 
        "The vector of weighted residuals has the wrong size.",
        __LINE__,
        __FILE__ );
    }
  }


  //----------------------------------------------------------------
  // Prepare the output values.
  //----------------------------------------------------------------

  // Return if there are no output values to calculate.
  if( n == 0 )
  {
    return;
  }

  // Instantiate the temporary array to hold the residuals.
  valarray<double> resTemp( n );

  // If this function is going to return the weighted residuals,
  // initialize the temporary array to hold them.
  valarray<double> wresTemp;
  if ( pWresOut )
  {
    wresTemp.resize( n );
  }


  //----------------------------------------------------------------
  // Calculate the square root of the covariance inverse.
  //----------------------------------------------------------------

  int i;

  valarray<double> covInvEigenVec( n * n );
  valarray<double> covInvSqrRoot ( n * n );

  // Calculate the matrix square root of the inverse of the covariance
  //
  //         -1/2
  //     cov       .
  //
  try
  {
    // Temporarily set the matrix of eigenvectors equal to
    // the inverse of the covariance.
    covInvEigenVec = inverse( cov, n );

    // Set the arguments for the C interface to the
    // to the Fortran symmetric eigenvector routine.
    int ijob   = int( 'V' ); // Calculate eigenvalues and eigen vectors.
    int iuplo  = int( 'U' ); // The upper triangle of the matrix is set.
    int lwork  = 3 * n + 1;  // Length of the work vector.
    int info;                // Return flag.

    valarray<double> covInvEigenVal( n );      // Eigenvalues in ascending order.
    valarray<double> work          ( lwork );  // Workspace.

    // Calculate the eigenvalues and eigenvectors of the 
    // inverse of the covariance.
    c2dsyev_(
      &ijob,
      &iuplo,
      &n,
      &covInvEigenVec[0],
      &n,
      &covInvEigenVal[0],
      &work[0],
      &lwork,
      &info);

    // Check that the eigenvalue calculation worked.
    if ( info )
    {
      throw SpkException(
        SpkError::SPK_UNKNOWN_ERR,  
        "The weighted residuals calculation failed while evaluating the eigenvalues \nof the covariance inverse.",
        __LINE__, 
        __FILE__ );
    }

    // Create a diagonal matrix with the square roots of the
    // eigenvalues along its diagonal.
    valarray<double> lambdaSqrRoot( 0.0, n * n );
    for ( i = 0; i < n; i++ )
    {
      lambdaSqrRoot[i + i * n] = sqrt( covInvEigenVal[i] );
    }

    // Get the transpose of the matrix of eigenvectors.
    valarray<double> covInvEigenVecTran( n * n );
    covInvEigenVecTran = transpose( covInvEigenVec, n );

    // Calculate the matrix square root of the covariance inverse
    //
    //         -1/2              1/2   T
    //     cov       =  Z  Lambda     Z   ,
    //
    // where Z is the matrix of eigenvectors and Lambda is the
    // diagonal matrix of eigenvalues.
    valarray<double> temp( n * n );
    temp          = multiply( covInvEigenVec, n, lambdaSqrRoot,      n );
    covInvSqrRoot = multiply( temp,           n, covInvEigenVecTran, n );
  }
  catch( ... )
  {
    throw SpkException(
      SpkError::SPK_UNKNOWN_ERR,  
      "The weighted residuals calculation failed while evaluating the square root \nof the covariance inverse.",
      __LINE__, 
      __FILE__ );
  }


  //----------------------------------------------------------------
  // Calculate the residuals and/or weighted residuals.
  //----------------------------------------------------------------

  // Calculate the residuals.
  resTemp = y - f;

  // Calculate the weighted residuals, 
  //
  //                 -1/2
  //     wres  =  cov     * res  .
  //
  if ( pWresOut )
  {
    wresTemp = multiply( covInvSqrRoot, n, resTemp, 1 );
  }


  //----------------------------------------------------------------
  // Set the output values.
  //----------------------------------------------------------------

  // Set the residuals, if necessary.
  if ( pResOut )
  {
    *pResOut = resTemp;
  }

  // Set the weighted residuals, if necessary.
  if ( pWresOut )
  {
    *pWresOut = wresTemp;
  }
}


/*************************************************************************
 *
 * Function: wres
 *
 *************************************************************************/

// This function is for backward compatability and can be removed
// if it is no longer needed.
void wres( const valarray<double>& y, 
           const valarray<double>& f,	      
           const valarray<double>& cov,	      
           valarray<double>&       resOut,   
           valarray<double>&       wresOut )
{
  wres( y, f, cov, &resOut, &wresOut );
}

