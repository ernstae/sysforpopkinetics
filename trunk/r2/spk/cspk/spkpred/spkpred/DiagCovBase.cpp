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
 *//**
 * @file: DiagCovBase.cpp
 *
 *
 * Implements DiagCovBase class.
 *//*
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "DiagCovBase.h"

// SPK library header files.
#include <spk/SpkValarray.h>
#include <spk/scalarToDouble.h>
#include <spk/scalarToDoubleArray.h>

// CppAD header files.
#include <CppAD/CppAD.h>

// Standard library header files.
#include <cassert>
#include <cmath>

using SPK_VA::valarray;


/*************************************************************************
 *
 * Function: DiagCovBase
 *
 *//**
 * Constructs a diagonal covariance matrix with nRowIn rows and columns.
 *//*
 *************************************************************************/

template<class Scalar>
DiagCovBase<Scalar>::DiagCovBase( int nRowIn )
  :
  Cov<Scalar>( nRowIn, nRowIn )
{
}

template<class Scalar>
DiagCovBase<Scalar>::DiagCovBase( int nRowIn,  const SPK_VA::valarray<bool>& minRepFixedIn )
  :
  Cov<Scalar>( nRowIn, nRowIn, minRepFixedIn )
{
}
/*************************************************************************
 *
 * Function: cov
 *
 *//**
 * Evaluates the covariance matrix at the current parameter value.
 *
 * In particular, this function sets covOut equal to
 * \f[
 *     \mbox{cov}(\mbox{par}) .
 * \f]
 *//*
 *************************************************************************/

template<class Scalar>
void DiagCovBase<Scalar>::cov( SPK_VA::valarray<Scalar>& covOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  covOut.resize( Cov<Scalar>::nRow * Cov<Scalar>::nRow );


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  if ( Cov<Scalar>::isCovCurrOk )
  {
    covOut = Cov<Scalar>::covCurr;
    Cov<Scalar>::usedCachedCov = true;

    return;
  }
  else
  {
    Cov<Scalar>::usedCachedCov = false;
  }


  //------------------------------------------------------------
  // Evaluate the covariance matrix.
  //------------------------------------------------------------

  // Create a matrix that has only zeroes.
  Cov<Scalar>::covCurr = 0.0;

  // Set the diagonal elements,
  //
  //    cov      ( par )  =  exp[ 2 par  ]  .
  //       (i, i)                      i   
  //
  int i;
  for ( i = 0; i < Cov<Scalar>::nPar; i++ )
  {
    Cov<Scalar>::covCurr[i + i * Cov<Scalar>::nRow] = exp( 2.0 * Cov<Scalar>::parCurr[i] );
  }    


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  Cov<Scalar>::isCovCurrOk = true;
  covOut = Cov<Scalar>::covCurr;
}


/*************************************************************************
 *
 * Function: cov_par
 *
 *//**
 * Evaluates the derivative of the covariance matrix at the current
 * parameter value.
 *
 * In particular, this function sets cov_parOut equal to
 * \f[
 *     \partial_{\mbox{par}} \; \mbox{cov}(\mbox{par}) =
 *       \partial_{\mbox{par}} \; \mbox{rvec} \left[ \mbox{cov}(\mbox{par}) \right] .
 * \f]
 *//*
 *************************************************************************/

template<class Scalar>
void DiagCovBase<Scalar>::cov_par( SPK_VA::valarray<double>& cov_parOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  cov_parOut.resize( Cov<Scalar>::nRow * Cov<Scalar>::nRow * Cov<Scalar>::nPar );


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  if ( Cov<Scalar>::isCov_parCurrOk )
  {
    cov_parOut = Cov<Scalar>::cov_parCurr;
    Cov<Scalar>::usedCachedCov_par = true;

    return;
  }
  else
  {
    Cov<Scalar>::usedCachedCov_par = false;
  }


  //------------------------------------------------------------
  // Evaluate the derivative of the covariance matrix.
  //------------------------------------------------------------

  // Create a double version of the current parameter.
  valarray<double> parCurrDouble( Cov<Scalar>::nPar );
  scalarToDoubleArray( Cov<Scalar>::parCurr, parCurrDouble );

  int nCov_parRow( Cov<Scalar>::nRow * Cov<Scalar>::nRow );

  // Create a matrix that has only zeroes.
  Cov<Scalar>::cov_parCurr = 0.0;

  // Set the nonzero elements of the derivative, i.e. the partial
  // derivatives of the diagonal elements of the covariance,
  //
  //     (i)     
  //    d     cov      ( par )  =  2 exp[ 2 par  ]  .
  //     par     (i, i)                        i
  //
  // Note that an rvec operation is performed on the elements of
  // the covariance before the partial derivatives are computed,
  //                                       -            -
  //                                      |              |
  //     d     cov( par )  =  d     rvec  |  cov( par )  |  .
  //      par                  par        |              |
  //                                       -            -
  int i;
  int row;
  for ( i = 0; i < Cov<Scalar>::nPar; i++ )
  {
    // Set the row in the rvec version of the covariance.
    row = i * Cov<Scalar>::nRow + i;

    Cov<Scalar>::cov_parCurr[row + i * nCov_parRow] = 2.0 * exp( 2.0 * parCurrDouble[i] );
  }    


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  Cov<Scalar>::isCov_parCurrOk = true;
  cov_parOut = Cov<Scalar>::cov_parCurr;
}


/*************************************************************************
 *
 * Function: inv
 *
 *//**
 * Evaluates the inverse of the covariance matrix at the current
 * parameter value.
 *
 * In particular, this function sets invOut equal to
 * \f[
 *     \mbox{cov}^{-1}(\mbox{par}) .
 * \f]
 *//*
 *************************************************************************/

template<class Scalar>
void DiagCovBase<Scalar>::inv( SPK_VA::valarray<Scalar>& invOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  invOut.resize( Cov<Scalar>::nRow * Cov<Scalar>::nRow );


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  if ( Cov<Scalar>::isInvCurrOk )
  {
    invOut = Cov<Scalar>::invCurr;
    Cov<Scalar>::usedCachedInv = true;

    return;
  }
  else
  {
    Cov<Scalar>::usedCachedInv = false;
  }


  //------------------------------------------------------------
  // Evaluate the inverse of the covariance matrix.
  //------------------------------------------------------------

  // Create a matrix that has only zeroes.
  Cov<Scalar>::invCurr = 0.0;

  // Set the diagonal elements,
  //
  //    inv      ( par )  =  exp[ - 2 par  ]  .
  //       (i, i)                        i
  //
  int i;
  for ( i = 0; i < Cov<Scalar>::nPar; i++ )
  {
    Cov<Scalar>::invCurr[i + i * Cov<Scalar>::nRow] = exp( -2.0 * Cov<Scalar>::parCurr[i] );
  }    


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  Cov<Scalar>::isInvCurrOk = true;
  invOut = Cov<Scalar>::invCurr;
}


/*************************************************************************
 *
 * Function: inv_par
 *
 *//**
 * Evaluates the derivative of the inverse of the covariance matrix
 * at the current parameter value.
 *
 * In particular, this function sets inv_parOut equal to
 * \f[
 *     \partial_{\mbox{par}} \; \mbox{cov}^{-1}(\mbox{par}) =
 *       \partial_{\mbox{par}} \; \mbox{rvec} \left[ \mbox{cov}^{-1}(\mbox{par}) \right] .
 * \f]
 *//*
 *************************************************************************/

template<class Scalar>
void DiagCovBase<Scalar>::inv_par( SPK_VA::valarray<double>& inv_parOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  inv_parOut.resize( Cov<Scalar>::nRow * Cov<Scalar>::nRow * Cov<Scalar>::nPar );


  //------------------------------------------------------------
  // Use the cached value if possible.
  //------------------------------------------------------------

  if ( Cov<Scalar>::isInv_parCurrOk )
  {
    inv_parOut = Cov<Scalar>::inv_parCurr;
    Cov<Scalar>::usedCachedInv_par = true;

    return;
  }
  else
  {
    Cov<Scalar>::usedCachedInv_par = false;
  }


  //------------------------------------------------------------
  // Evaluate the derivative of the inverse of the covariance matrix.
  //------------------------------------------------------------

  // Create a double version of the current parameter.
  valarray<double> parCurrDouble( Cov<Scalar>::nPar );
  scalarToDoubleArray( Cov<Scalar>::parCurr, parCurrDouble );

  int nInv_parRow( Cov<Scalar>::nRow * Cov<Scalar>::nRow );

  // Create a matrix that has only zeroes.
  Cov<Scalar>::inv_parCurr = 0.0;
  // Set the nonzero elements of the derivative, i.e. the partial
  // derivatives of the diagonal elements of the inverse of the 
  // the covariance,
  //
  //     (i)     
  //    d     inv      ( par )  =  -2 exp[ -2 par  ]  .
  //     par     (i, i)                          i
  //
  // Note that an rvec operation is performed on the elements of
  // the covariance before the partial derivatives are computed,
  //                                       -            -
  //                                      |              |
  //     d     inv( par )  =  d     rvec  |  inv( par )  |  .
  //      par                  par        |              |
  //                                       -            -
  int i;
  int row;
  for ( i = 0; i < Cov<Scalar>::nPar; i++ )
  {
    // Set the row in the rvec version of the inverse of the covariance.
    row = i * Cov<Scalar>::nRow + i;

    Cov<Scalar>::inv_parCurr[row + i * nInv_parRow] = -2.0 * exp( -2.0 * parCurrDouble[i] );
  }    


  //------------------------------------------------------------
  // Finish up.
  //------------------------------------------------------------

  Cov<Scalar>::isInv_parCurrOk = true;
  inv_parOut = Cov<Scalar>::inv_parCurr;
}


/*************************************************************************
 *
 * Function: getParLimits
 * * Modfied by David Salinger to allow for fixed components 05-11-21
 *//**
 * Determines the lower and upper limits for the covariance matrix parameters
 * at the current parameter value.  These limits are for use during the
 * optimization of objective functions that depend on these parameters.
 *
 * This function assumes that the current values for the covariance
 * parameters are approximately equal to the final or true values.
 *//*
 *************************************************************************/

template<class Scalar>
void DiagCovBase<Scalar>::getParLimits(
  SPK_VA::valarray<double>&  parLow,
  SPK_VA::valarray<double>&  parUp ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  parLow.resize( Cov<Scalar>::nPar );
  parUp .resize( Cov<Scalar>::nPar );


  //------------------------------------------------------------
  // Evaluate the current value for the covariance matrix. 
  //------------------------------------------------------------

  // If the covariance has not been evaluated at the current
  // value for the parameters, then evaluate it.
  if ( !Cov<Scalar>::isCovCurrOk )
  {
    valarray<Scalar> tempCov( Cov<Scalar>::nRow * Cov<Scalar>::nRow );
    cov( tempCov );
  }

  //------------------------------------------------------------
  // Set the limits for the parameters.
  //------------------------------------------------------------

  // Set the limits for optimization to constrain the diagonal
  // elements of the covariance matrix as follows,
  //
  //      1      (curr)                              (curr)
  //     ---  cov        <=   cov        <=  100  cov        .
  //     100     (i,i)           (i,i)               (i,i) 
  //
  // These limits for the covariance diagonal elements imply
  // these limits for its parameters,
  // If fixed[i], the parameter is set to the value .5 * log( cov[i,i] )
  // Otherwise    -                -                            -                -
  //             |                  |                          |                  |
  //      1      |   1      (curr)  |                   1      |          (curr)  |
  //     --- log |  ---  cov        |  <=   par    <=  --- log |  100  cov        |  .
  //      2      |  100     (i,i)   |          i        2      |          (i,i)   |
  //             |                  |                          |                  |   
  //              -                -                            -                -
  //
  // For fixed covariance elements, the upper and lower bounds are set to the par value.
  //
  int i;
  double parValue;
  double parLowValue;
  double parUpValue;
  for ( i = 0; i < Cov<Scalar>::nPar; i++ )
  { 
    scalarToDouble( 0.5 * log( Cov<Scalar>::covCurr[i + i * Cov<Scalar>::nRow] ), parValue );
    if( Cov<Scalar>::parFixed[i] )
    {  parLow[i] = parValue;
       parUp[i]  = parValue;
    }
    else 
    {   //parLow[i] = parValue - 0.5 * log( 100.0 );
        //parUp[i]  = parValue + 0.5 * log( 100.0 );
        scalarToDouble( 0.5 * log( Cov<Scalar>::covCurr[i + i * Cov<Scalar>::nRow] / 100.0 ), parLowValue );
        scalarToDouble( 0.5 * log( Cov<Scalar>::covCurr[i + i * Cov<Scalar>::nRow] * 100.0 ), parUpValue );
        parLow[i] = parLowValue;
        parUp[i]  = parUpValue;
    }
  }    

}


/*************************************************************************
 *
 * Function: calcPar
 *
 *//**
 * Sets parOut equal to the covariance matrix parameters that
 * correspond to the covariance matrix covIn.
 *//*
 *************************************************************************/

template<class Scalar>
void DiagCovBase<Scalar>::calcPar( 
  const SPK_VA::valarray<Scalar>& covIn,
  SPK_VA::valarray<Scalar>&       parOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  int nCovInRow = static_cast<int>( sqrt( static_cast<double>( covIn.size() ) ) );
  assert( covIn.size() == nCovInRow * nCovInRow );

  // Set the number of parameters for this covariance matrix.
  int nCovInPar = nCovInRow;

  parOut.resize( nCovInPar );


  //------------------------------------------------------------
  // Set the covariance matrix parameters.
  //------------------------------------------------------------

  // Set the parameter elements,
  //
  //              1
  //    par   =  ---  log[ cov      ( par ) ]  .
  //       i      2           (i, i)             
  //
  int i;
  for ( i = 0; i < nCovInPar; i++ )
  {
    parOut[i] = 0.5 * log( covIn[i + i * nCovInRow] );
  }    

}


/*************************************************************************
 *
 * Function: calcCovMinRep
 *
 *//**
 * Sets covMinRepOut equal to the minimal representation for the
 * covariance matrix covIn.
 *
 * The minimal representation is the set of diagonal elements.
 *//*
 *************************************************************************/

template<class Scalar>
void DiagCovBase<Scalar>::calcCovMinRep( 
  const SPK_VA::valarray<Scalar>& covIn,
  SPK_VA::valarray<Scalar>&       covMinRepOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  int nCovInRow = static_cast<int>( sqrt( static_cast<double>( covIn.size() ) ) );
  assert( covIn.size() == nCovInRow * nCovInRow );

  // Set the number of parameters for this covariance matrix.
  int nCovInPar = nCovInRow;

  covMinRepOut.resize( nCovInPar );


  //------------------------------------------------------------
  // Set the minimal representation for the covariance matrix.
  //------------------------------------------------------------

  // Extract the diagonal elements from the covariance matrix.
  int i;
  for ( i = 0; i < nCovInPar; i++ )
  {
    covMinRepOut[i] = covIn[i + i * nCovInRow];
  }    

}


/*************************************************************************
 *
 * Function: calcCovMinRepMask
 *
 *//**
 * Sets covMinRepMaskOut equal to the minimal representation ordered
 * mask that corresponds to the covariance parameter ordered mask
 * parMaskIn.
 *
 * The minimal representation is the set of diagonal elements.
 *//*
 *************************************************************************/

template<class Scalar>
void DiagCovBase<Scalar>::calcCovMinRepMask( 
  const SPK_VA::valarray<bool>& parMaskIn,
  SPK_VA::valarray<bool>&       covMinRepMaskOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Set the number of parameters for the covariance matrix.
  int nMaskInPar = parMaskIn.size();

  covMinRepMaskOut.resize( nMaskInPar );


  //------------------------------------------------------------
  // Set the mask for the covariance matrix minimal representation.
  //------------------------------------------------------------

  // Set the minimal representation ordered mask equal to the
  // covariance parameter ordered mask since the orders for their
  // elements are the same,
  //
  //    MinRep     =  cov      ( par ) 
  //          (i)        (i, i)        
  //                                        
  //               =  exp[ 2 par  ]  ,
  //                            i      
  //
  covMinRepMaskOut = parMaskIn;

}


/*************************************************************************
 *
 * Function: calcCovMinRep_par
 *
 *//**
 * Sets covMinRep_parOut equal to the derivative of the minimal
 * representation for the covariance matrix with derivative cov_parIn.
 *
 * The minimal representation is the set of diagonal elements.
 *//*
 *************************************************************************/

template<class Scalar>
void DiagCovBase<Scalar>::calcCovMinRep_par( 
  const SPK_VA::valarray<double>& cov_parIn,
  int                             nCov_parInCol,
  SPK_VA::valarray<double>&       covMinRep_parOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Set the number of parameters for the covariance matrix.
  int nCovInPar = nCov_parInCol;

  // Set the number of rows in the covariance matrix.
  int nCovInRow = nCovInPar;
  assert( cov_parIn.size() == nCovInRow * nCovInRow * nCovInPar );

  // Set the number of rows in the derivative of the covariance matrix
  // and its minimal representation.
  int nCov_parInRow        = nCovInRow * nCovInRow;
  int nCovMinRep_parOutRow = nCovInPar;

  covMinRep_parOut.resize( nCovInPar * nCovInPar );


  //------------------------------------------------------------
  // Set the derivative of the covariance matrix minimal representation.
  //------------------------------------------------------------

  int i;
  int k;

  // Extract the derivatives of the elements that are on the diagonal.
  for ( k = 0; k < nCovInPar; k++ )
  {
    for ( i = 0; i < nCovInRow; i++ )
    {
      covMinRep_parOut[( i                 ) + k * nCovMinRep_parOutRow] = 
        cov_parIn     [( i + i * nCovInRow ) + k * nCov_parInRow];
    }    
  }    

}


/*************************************************************************
 *
 * Function: expandCovMinRep
 *
 *//**
 * Sets covOut equal to the covariance matrix that corresponds
 * to the minimal representation for the covariance matrix that
 * is contained in covMinRepIn.
 *
 * The minimal representation is the set of diagonal elements.
 *//*
 *************************************************************************/

template<class Scalar>
void DiagCovBase<Scalar>::expandCovMinRep( 
  const SPK_VA::valarray<double>& covMinRepIn,
  SPK_VA::valarray<Scalar>&       covOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Set the number of parameters for this covariance matrix.
  int nCovInPar = covMinRepIn.size();

  // Set the number of rows in the covariance matrix.
  int nCovInRow = nCovInPar;

  covOut.resize( nCovInRow * nCovInRow );


  //------------------------------------------------------------
  // Expand the minimal representation for the covariance matrix.
  //------------------------------------------------------------

  // Set all of the covariance matrix elements equal to zero.
  covOut = 0.0;

  // Set the diagonal elements from the covariance matrix.
  int i;
  for ( i = 0; i < nCovInPar; i++ )
  {
    covOut[i + i * nCovInRow] = covMinRepIn[i];
  }    

}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

// Declare double versions of these functions.
template DiagCovBase<double>::DiagCovBase( int nRowIn );
template DiagCovBase<double>::DiagCovBase( int nRowIn,  const SPK_VA::valarray<bool>& minRepFixedIn );

template void DiagCovBase<double>::cov    ( SPK_VA::valarray<double>& covOut     ) const;
template void DiagCovBase<double>::cov_par( SPK_VA::valarray<double>& cov_parOut ) const;

template void DiagCovBase<double>::inv    ( SPK_VA::valarray<double>& invOut     ) const;
template void DiagCovBase<double>::inv_par( SPK_VA::valarray<double>& inv_parOut ) const;

template void DiagCovBase<double>::getParLimits(
    SPK_VA::valarray<double>&  parLow,
    SPK_VA::valarray<double>&  parUp ) const;

template void DiagCovBase<double>::calcPar( 
    const SPK_VA::valarray<double>& covIn,
    SPK_VA::valarray<double>&       parOut ) const;

template void DiagCovBase<double>::calcCovMinRep( 
    const SPK_VA::valarray<double>& covIn,
    SPK_VA::valarray<double>&       covMinRepOut ) const;

template void DiagCovBase<double>::calcCovMinRep_par( 
    const SPK_VA::valarray<double>& cov_parIn,
    int                             nCov_parInCol,
    SPK_VA::valarray<double>&       covMinRep_parOut ) const;

template void DiagCovBase<double>::calcCovMinRepMask( 
    const SPK_VA::valarray<bool>& parMaskIn,
    SPK_VA::valarray<bool>&       covMinRepMaskOut ) const;

template void DiagCovBase<double>::expandCovMinRep( 
    const SPK_VA::valarray<double>& covMinRepIn,
    SPK_VA::valarray<double>&       covOut ) const;


// Declare CppAD::AD<double> versions of these functions.
template DiagCovBase< CppAD::AD<double> >::DiagCovBase( int nRowIn );
template DiagCovBase< CppAD::AD<double> >::DiagCovBase( int nRowIn,  const SPK_VA::valarray<bool>& minRepFixedIn );

template void DiagCovBase< CppAD::AD<double> >::cov    ( SPK_VA::valarray< CppAD::AD<double> >& covOut     ) const;
template void DiagCovBase< CppAD::AD<double> >::cov_par( SPK_VA::valarray<double>& cov_parOut ) const;

template void DiagCovBase< CppAD::AD<double> >::inv    ( SPK_VA::valarray< CppAD::AD<double> >& invOut     ) const;
template void DiagCovBase< CppAD::AD<double> >::inv_par( SPK_VA::valarray<double>& inv_parOut ) const;

template void DiagCovBase< CppAD::AD<double> >::getParLimits(
    SPK_VA::valarray<double>&  parLow,
    SPK_VA::valarray<double>&  parUp ) const;

template void DiagCovBase< CppAD::AD<double> >::calcPar( 
    const SPK_VA::valarray< CppAD::AD<double> >& covIn,
    SPK_VA::valarray< CppAD::AD<double> >&       parOut ) const;

template void DiagCovBase< CppAD::AD<double> >::calcCovMinRep( 
    const SPK_VA::valarray< CppAD::AD<double> >& covIn,
    SPK_VA::valarray< CppAD::AD<double> >&       covMinRepOut ) const;

template void DiagCovBase< CppAD::AD<double> >::calcCovMinRep_par( 
    const SPK_VA::valarray<double>& cov_parIn,
    int                             nCov_parInCol,
    SPK_VA::valarray<double>&       covMinRep_parOut ) const;

template void DiagCovBase< CppAD::AD<double> >::calcCovMinRepMask( 
    const SPK_VA::valarray<bool>& parMaskIn,
    SPK_VA::valarray<bool>&       covMinRepMaskOut ) const;

template void DiagCovBase< CppAD::AD<double> >::expandCovMinRep( 
    const SPK_VA::valarray<double>& covMinRepIn,
    SPK_VA::valarray< CppAD::AD<double> >&       covOut ) const;

