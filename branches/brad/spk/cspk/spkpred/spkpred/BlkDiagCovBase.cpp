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
 * @file: BlkDiagCovBase.cpp
 *
 *
 * Implements BlkDiagCovBase class.
 *//*
 * Author: David Salinger / Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK Pred library header files.
#include "BlkDiagCovBase.h"
#include "DiagCov.h"
#include "FullCov.h"
#include "isEqual.h"

// SPK library header files.
#include <spk/SpkValarray.h>

// CppAD header files.
#include <CppAD/CppAD.h>

// Standard library header files.
#include <cassert>
#include <cmath>

using SPK_VA::valarray;
using SPK_VA::slice;

/*************************************************************************
 *
 * Function: BlkDiagCovBase
 *
 *//**
 * Constructs a block diagonal covariance matrix.  Each covariance block 
 * is either full or diagonal as specified in FullCov or DiagCov. 
 *//*
 *************************************************************************/

template<class Scalar>
BlkDiagCovBase<Scalar>::BlkDiagCovBase( int nRowIn,
                        const SPK_VA::valarray<bool>&  minRepFixedIn,
                        const SPK_VA::valarray<covStruct>&  blockStruct,
                        const SPK_VA::valarray<int>&   blockDims,
                        const SPK_VA::valarray<bool>&  blockSameAsPrev )
  :
  Cov<Scalar>( nRowIn, minRepFixedIn.size() ) 
{
  Cov<Scalar>::parFixed = minRepFixedIn;
  Cov<Scalar>::nBlocks =  blockDims.size();
  int start = 0;
  int nPar_i;
  valarray<bool> parFixed_i;
  for (int i = 0; i <Cov<Scalar>::nBlocks; i++) 
  {
    int nRow_i = blockDims[i];
    if ( blockStruct[i] == DIAGONAL )
    {
      nPar_i = nRow_i;
      parFixed_i.resize( nPar_i );
      parFixed_i = Cov<Scalar>::parFixed[ slice( start, nPar_i, 1 ) ];
      // Add a diagonal block to the vector of blocks
      Cov<Scalar>::block.push_back( new DiagCovBase<Scalar>( nRow_i, parFixed_i ) );
      start += nPar_i;
    }
    else if ( blockStruct[i] == FULL )
    {
      nPar_i = nRow_i * ( nRow_i + 1 )/2;
      parFixed_i.resize( nPar_i );
      parFixed_i = Cov<Scalar>::parFixed[ slice( start, nPar_i, 1 ) ];
      // Add a full block to the vector of blocks
      Cov<Scalar>::block.push_back( new FullCovBase<Scalar>( nRow_i, parFixed_i ) );
      start += nPar_i;
    }
    else
    {
      throw SpkException(
       SpkError::SPK_USER_INPUT_ERR, 
       "Unknown covariance matrix type requested for block.",
       __LINE__, 
       __FILE__ );
    }
  }
  
}

/*************************************************************************
 *
 * Function: ~BlkDiagCovBase
 *
 *//**
 * Destructor.
 */
/*************************************************************************/

template<class Scalar>
BlkDiagCovBase<Scalar>::~BlkDiagCovBase( void )
{ 
  for( int i = 0; i <Cov<Scalar>::nBlocks; i++ )
  {
    delete Cov<Scalar>::block[i];
  }
}
/*************************************************************************
 *
 * Function: setPar
 *
 *//**
 * Sets the current values for the minimal representation parameters
 * equal to parIn.
 *//*
 *************************************************************************/

template<class Scalar>
void BlkDiagCovBase<Scalar>::setPar( const SPK_VA::valarray<Scalar>& parIn )
{
  assert( parIn.size() == Cov<Scalar>::nPar );

  // Only reset the parameter value if it has changed.
  if ( !isEqual( parIn, Cov<Scalar>::parCurr ) )
  { 
    // If the parameter has changed, then any cached
    // values are no longer valid.
    Cov<Scalar>::invalidateCache();

    // Set the new value.
    Cov<Scalar>::parCurr = parIn;

    // loop through blocks to Set the new value in each block.
    valarray<Scalar> parIn_i;
    int start = 0;
    int nPar_i;
    for ( int i = 0; i <Cov<Scalar>::nBlocks; i++ )
    {
      nPar_i = Cov<Scalar>::block[i]->getNPar();
      parIn_i.resize( nPar_i );
      parIn_i = parIn[ slice( start, nPar_i, 1)];
      Cov<Scalar>::block[i]->setPar( parIn_i );
      start += nPar_i;
    }

  }
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
void BlkDiagCovBase<Scalar>::cov( SPK_VA::valarray<Scalar>& covOut ) const
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

  //  Loop through cov blocks to create rvec of combined cov
  //
  // 
  int start = 0;
  int start_i;
  int nRow_i;
  valarray<Scalar> covCurr_i;

  for ( int i = 0; i <Cov<Scalar>::nBlocks; i++ )
  {
    nRow_i = Cov<Scalar>::block[i]->getNRow();
    covCurr_i.resize( nRow_i * nRow_i );
    Cov<Scalar>::block[i]->cov( covCurr_i );
    start_i = 0;

    //loop through rows of current block
    for ( int j = 0; j < nRow_i; j++ )
    {
      assert( Cov<Scalar>::covCurr.size() >= start + nRow_i );
      Cov<Scalar>::covCurr[ slice( start, nRow_i, 1) ] = covCurr_i[ slice( start_i, nRow_i, 1) ];
      start_i += nRow_i;
      start  += Cov<Scalar>::nRow;
    }
    start  += nRow_i;
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
void BlkDiagCovBase<Scalar>::cov_par( SPK_VA::valarray<double>& cov_parOut ) const
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

  int nCov_parRow( Cov<Scalar>::nRow * Cov<Scalar>::nRow );

  // Create a matrix that has only zeroes.
  Cov<Scalar>::cov_parCurr = 0.0;

  //  Loop through cov_par blocks to create rvec of combined cov_par
  //
  // 
  int start = 0;
  int startTmp;
  int start_i;
  int nRow_i;
  int nPar_i;
  valarray<double> cov_parCurr_i;

  for ( int i = 0; i < Cov<Scalar>::nBlocks; i++ )
  {
    nRow_i = Cov<Scalar>::block[i]->getNRow();
    nPar_i  = Cov<Scalar>::block[i]->getNPar();
    cov_parCurr_i.resize( nRow_i * nRow_i * nPar_i );
    Cov<Scalar>::block[i]->cov_par( cov_parCurr_i );

    start_i = 0;
    //loop through columns of current block
    for ( int j = 0; j < nPar_i; j++ )
    {
      startTmp = start;
      for ( int k = 0; k < nRow_i; k++ )
      {
        assert( Cov<Scalar>::cov_parCurr.size() >= startTmp + nRow_i );
        Cov<Scalar>::cov_parCurr[ slice( startTmp, nRow_i, 1) ] = cov_parCurr_i[ slice( start_i, nRow_i, 1) ];
        start_i += nRow_i;
        startTmp += Cov<Scalar>::nRow;
      }
      start  += nCov_parRow;
    }
    start  += (Cov<Scalar>::nRow + 1) * nRow_i;
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
void BlkDiagCovBase<Scalar>::inv( SPK_VA::valarray<Scalar>& invOut ) const
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
 
  //  Loop through inv of cov  blocks to create rvec of combined inv
  //
  int start = 0;
  int start_i;
  int nRow_i;
  valarray<Scalar> invCurr_i;

  for ( int i = 0; i <Cov<Scalar>::nBlocks; i++ )
  {
    nRow_i = Cov<Scalar>::block[i]->getNRow();
    invCurr_i.resize( nRow_i * nRow_i );
    Cov<Scalar>::block[i]->inv( invCurr_i );
    start_i = 0;

    //loop through rows of current block
    for ( int j = 0; j < nRow_i; j++ )
    {
      assert( Cov<Scalar>::invCurr.size() >= start + nRow_i );
      Cov<Scalar>::invCurr[ slice( start, nRow_i, 1) ] = invCurr_i[ slice( start_i, nRow_i, 1) ];
      start_i += nRow_i;
      start  += Cov<Scalar>::nRow;
    }
    start  += nRow_i;
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
void BlkDiagCovBase<Scalar>::inv_par( SPK_VA::valarray<double>& inv_parOut ) const
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

  int nInv_parRow( Cov<Scalar>::nRow * Cov<Scalar>::nRow );

  // Create a matrix that has only zeroes.
  Cov<Scalar>::inv_parCurr = 0.0;
  
  //  Loop through inv_par blocks to create rvec of combined inv_par
  //
  // 
  int start = 0;
  int startTmp;
  int start_i;
  int nRow_i;
  int nPar_i;
  valarray<double> inv_parCurr_i;

  for ( int i = 0; i < Cov<Scalar>::nBlocks; i++ )
  {
    nRow_i = Cov<Scalar>::block[i]->getNRow();
    nPar_i  = Cov<Scalar>::block[i]->getNPar();
    inv_parCurr_i.resize( nRow_i * nRow_i * nPar_i );
    Cov<Scalar>::block[i]->inv_par( inv_parCurr_i );

    start_i = 0;
    //loop through rows of current block
    for ( int j = 0; j < nPar_i; j++ )
    {
      startTmp = start;
      for ( int k = 0; k < nRow_i; k++ )
      {
        assert( Cov<Scalar>::cov_parCurr.size() >= startTmp + nRow_i );
        Cov<Scalar>::inv_parCurr[ slice( startTmp, nRow_i, 1) ] = inv_parCurr_i[ slice( start_i, nRow_i, 1) ];
        start_i += nRow_i;
        startTmp += Cov<Scalar>::nRow;
      }
      start  += nInv_parRow;
    }
    start  += (Cov<Scalar>::nRow + 1) *nRow_i;
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
 * *
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
void BlkDiagCovBase<Scalar>::getParLimits(
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

  // loop through blocks to set/get parameter limits
  int nRow_i;
  int nPar_i;
  int start = 0;
  valarray<double> parLow_i;
  valarray<double> parUp_i;
  for ( int i = 0; i <Cov<Scalar>::nBlocks; i++ )
  {
    nPar_i = Cov<Scalar>::block[i]->getNPar();
    nRow_i = Cov<Scalar>::block[i]->getNRow();
    parLow_i.resize( nPar_i );
    parUp_i.resize ( nPar_i );
 
    //call getParLimits for block i
    Cov<Scalar>::block[i]->getParLimits( parLow_i, parUp_i );

    parLow[ slice( start, nPar_i, 1 ) ] = parLow_i;
    parUp[  slice( start, nPar_i, 1 ) ] = parUp_i;
    start += nPar_i;    
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
void BlkDiagCovBase<Scalar>::calcPar( 
  const SPK_VA::valarray<Scalar>& covIn,
  SPK_VA::valarray<Scalar>&       parOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.   <<
  //------------------------------------------------------------

  // Set the number of rows in the covariance matrix.
  int nCovInRow = static_cast<int>( sqrt( static_cast<double>( covIn.size() ) ) );
  assert( covIn.size() == nCovInRow * nCovInRow );

  // Set the number of parameters for this covariance matrix.
  int nCovInPar = Cov<Scalar>::nPar;  
  parOut.resize( nCovInPar );


  //------------------------------------------------------------
  // Set the covariance matrix parameters.
  //------------------------------------------------------------

  // loop through blocks to set/get parameter elements,
  int nRow_i;
  int nPar_i;
  int startPar = 0;
  int start = 0;
  int start_i;
  valarray<Scalar> par_i;
  valarray<Scalar> covIn_i;
  for ( int i = 0; i < Cov<Scalar>::nBlocks; i++ )
  {
    nPar_i = Cov<Scalar>::block[i]->getNPar();
    nRow_i = Cov<Scalar>::block[i]->getNRow();
    par_i.resize( nPar_i );
    covIn_i.resize( nRow_i * nRow_i );
    start_i = 0;

    //loop through rows in block to get rows
    for ( int j = 0; j < nRow_i; j++ )
    {
      covIn_i[ slice( start_i, nRow_i, 1) ] = covIn[ slice( start, nRow_i, 1) ];
      start_i += nRow_i;
      start  += Cov<Scalar>::nRow;
    }
    start  += nRow_i;
 
    //call calcPar for block i
    Cov<Scalar>::block[i]->calcPar( covIn_i, par_i );

    parOut[ slice( startPar, nPar_i, 1 ) ] = par_i;
    startPar += nPar_i;    
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
 * The minimal representation is the set minimal representations
 * from each constituant block.
 *//*
 *************************************************************************/

template<class Scalar>
void BlkDiagCovBase<Scalar>::calcCovMinRep( 
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
  int nCovInPar = Cov<Scalar>::nPar;

  covMinRepOut.resize( nCovInPar );


  //------------------------------------------------------------
  // Set the minimal representation for the covariance matrix.
  //------------------------------------------------------------

  // loop through blocks to calcCovMinRep for each block.
  int nRow_i;
  int nPar_i;
  int startPar = 0;
  int start = 0;
  int start_i;
  valarray<Scalar> minRep_i;
  valarray<Scalar> covIn_i;
  for ( int i = 0; i <Cov<Scalar>::nBlocks; i++ )
  {
    nPar_i = Cov<Scalar>::block[i]->getNPar();
    nRow_i = Cov<Scalar>::block[i]->getNRow();
    minRep_i.resize( nPar_i );
    covIn_i.resize( nRow_i * nRow_i );
    start_i = 0;

    //loop through rows in block to get rows
    for ( int j = 0; j < nRow_i; j++ )
    {
      covIn_i[ slice( start_i, nRow_i, 1) ] = covIn[ slice( start, nRow_i, 1) ];
      start_i += nRow_i;
      start  += Cov<Scalar>::nRow;
    }
    start  += nRow_i;
 
    //call calcPar for block i
    Cov<Scalar>::block[i]->calcCovMinRep( covIn_i, minRep_i );

    covMinRepOut[ slice( startPar, nPar_i, 1 ) ] = minRep_i;
    startPar += nPar_i;    
  }

}
 

/*************************************************************************
 *
 * Function: calcCovMinRep_par
 *
 *//**
 * Sets covMinRep_parOut equal to the derivative of the minimal
 * representation for the covariance matrix with derivative cov_parIn.
 *
 * The minimal representation is the set of minimal representations from 
 * each constituent block.
 *//*
 *************************************************************************/

template<class Scalar>
void BlkDiagCovBase<Scalar>::calcCovMinRep_par( 
  const SPK_VA::valarray<double>& cov_parIn,
  int                             nCov_parInCol,
  SPK_VA::valarray<double>&       covMinRep_parOut ) const
{
  //Note:  cov_parIn  not used in BLKDIAG case.  <this is wrong>!!!
  // Instead, cov_par is recomputed for each block - then calcCovMinRep_par
  // is called on each block.
  //
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Set the number of parameters for the covariance matrix.
  int nCovInPar  = nCov_parInCol;
  int nCov_parRow( Cov<Scalar>::nRow * Cov<Scalar>::nRow );

  covMinRep_parOut.resize( nCovInPar * nCovInPar );


  valarray<double> cov_parIn_i;
  valarray<double> covMinRep_parOut_i;
  int start  = 0;
  int start2 = 0;
  int startTmp;
  int start_i;
  int nPar_i;
  int nRow_i;

  for ( int i = 0; i <Cov<Scalar>::nBlocks; i++ )
  {
    nRow_i = Cov<Scalar>::block[i]->getNRow();
    nPar_i  = Cov<Scalar>::block[i]->getNPar();
    cov_parIn_i.resize( nRow_i * nRow_i* nPar_i );
    
    start_i = 0;
    //loop over columns of cov_parIn - parse into cov_parIn_i (for current block)
    for ( int j = 0; j < nPar_i; j++ )
    {
      startTmp = start;
      for ( int k = 0; k < nRow_i; k++ )
      {
        assert( cov_parIn.size() >= startTmp + nRow_i );
        cov_parIn_i[ slice( start_i, nRow_i, 1) ] = cov_parIn[ slice( startTmp, nRow_i, 1) ];
        start_i += nRow_i;
        startTmp += Cov<Scalar>::nRow;
      }    
      start  += nCov_parRow;
    }
    start += (Cov<Scalar>::nRow + 1) *nRow_i;

    // compute covMinRep_parOut_i for current block
    covMinRep_parOut_i.resize( nPar_i * nPar_i );
    Cov<Scalar>::block[i]->calcCovMinRep_par( cov_parIn_i, nPar_i, covMinRep_parOut_i );

    start_i = 0;
    //loop over columns of covMinRep_parOut_i  to produce covMinRep_parOut.
    for ( int k = 0; k < nPar_i; k++ )
    {
      covMinRep_parOut[ slice( start2, nPar_i, 1 ) ] = covMinRep_parOut_i[ slice( start_i, nPar_i, 1 ) ];
      start_i += nPar_i;
      start2 += nCovInPar;
    }
    start2 += nPar_i;                                     
  }  

}

//#######################Below under construction #################

/*************************************************************************
 *
 * Function: calcCovMinRepMask
 *
 *//**
 * Sets covMinRepMaskOut equal to the minimal representation ordered
 * mask that corresponds to the covariance parameter ordered mask
 * parMaskIn.
 *
 * The minimal representation is the elements from the lower triangle
 * of the covariance matrix stored in column major order.
 *//*
 *************************************************************************/

template<class Scalar>
void  BlkDiagCovBase<Scalar>::calcCovMinRepMask( 
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

  // loop through blocks to calcCovMinRep for each block.
  int nPar_i;
  int start = 0;
  valarray<bool> parMaskIn_i;
  valarray<bool> covMinRepMaskOut_i;

  for ( int i = 0; i <Cov<Scalar>::nBlocks; i++ )
  {
    nPar_i = Cov<Scalar>::block[i]->getNPar();
    parMaskIn_i.resize( nPar_i );
    covMinRepMaskOut_i.resize( nPar_i);
    parMaskIn_i = parMaskIn[ slice( start, nPar_i, 1) ];
    Cov<Scalar>::block[i]->calcCovMinRepMask( parMaskIn_i, covMinRepMaskOut_i);
    covMinRepMaskOut[ slice( start, nPar_i, 1) ] = covMinRepMaskOut_i;
    start += nPar_i;
  }

}

//#######################above under construction #################


/*************************************************************************
 *
 * Function: expandCovMinRep
 *
 *//**
 * Sets covOut equal to the covariance matrix that corresponds
 * to the minimal representation for the covariance matrix that
 * is contained in covMinRepIn.
 *
 * The minimal representation is the set minimal representations from each block.
 *//*
 *************************************************************************/

template<class Scalar>
void BlkDiagCovBase<Scalar>::expandCovMinRep( 
  const SPK_VA::valarray<double>& covMinRepIn,
  SPK_VA::valarray<Scalar>&       covOut ) const
{
  //------------------------------------------------------------
  // Preliminaries.
  //------------------------------------------------------------

  // Set the number of parameters for this covariance matrix.
  int nCovInPar = covMinRepIn.size();

  // Set the number of rows in the covariance matrix.
  int nCovInRow = Cov<Scalar>::nRow;

  covOut.resize( nCovInRow * nCovInRow );


  //------------------------------------------------------------
  // Expand the minimal representation for the covariance matrix.
  //------------------------------------------------------------

  // Set all of the covariance matrix elements equal to zero.
  covOut = 0.0;

    // Set the diagonal elements from the covariance matrix.
    //int i;
    //for ( i = 0; i < nCovInPar; i++ )
    //{
    //  covOut[i + i * nCovInRow] = covMinRepIn[i];
    //}    


  //  Loop through blocks - decompose MinRep into blocks, translate each to 
  //  Cov, recompose as rvec of combined cov
  //
  // 
  int start = 0;
  int start_i;
  int nRow_i;
  int nPar_i;
  int startPar = 0;
  valarray<double> minRepIn_i;
  valarray<Scalar> covCurr_i;

  for ( int i = 0; i <Cov<Scalar>::nBlocks; i++ )
  {
    //get min rep for current block
    nPar_i = Cov<Scalar>::block[i]->getNPar();
    minRepIn_i.resize( nPar_i );
    minRepIn_i = covMinRepIn[ slice ( startPar, nPar_i, 1 ) ];
    startPar += nPar_i;

    //expand min rep for current block
    nRow_i = Cov<Scalar>::block[i]->getNRow();
    covCurr_i.resize( nRow_i * nRow_i );
    Cov<Scalar>::block[i]->expandCovMinRep( minRepIn_i, covCurr_i );

    start_i = 0;
    //loop through rows of current block to create Cov
    for ( int j = 0; j < nRow_i; j++ )
    {
      covOut[ slice( start, nRow_i, 1) ] = covCurr_i[ slice( start_i, nRow_i, 1) ];
      start_i += nRow_i;
      start  += Cov<Scalar>::nRow;
    }
    start  += nRow_i;
   } 

}


/*------------------------------------------------------------------------
 * Template function instantiations.
 *------------------------------------------------------------------------*/

// Declare double versions of these functions.
template BlkDiagCovBase<double>::BlkDiagCovBase( int nRowIn, 
                  const SPK_VA::valarray<bool>&  minRepFixedIn,
                  const SPK_VA::valarray<covStruct>&  blockStruct,
                  const SPK_VA::valarray<int>&   blockDims,
                  const SPK_VA::valarray<bool>&  blockSameAsPrev );

template BlkDiagCovBase<double>::~BlkDiagCovBase( void );

template void BlkDiagCovBase<double>::setPar( const SPK_VA::valarray<double>& parIn );

template void BlkDiagCovBase<double>::cov    ( SPK_VA::valarray<double>& covOut     ) const;
template void BlkDiagCovBase<double>::cov_par( SPK_VA::valarray<double>& cov_parOut ) const;

template void BlkDiagCovBase<double>::inv    ( SPK_VA::valarray<double>& invOut     ) const;
template void BlkDiagCovBase<double>::inv_par( SPK_VA::valarray<double>& inv_parOut ) const;

template void BlkDiagCovBase<double>::getParLimits(
    SPK_VA::valarray<double>&  parLow,
    SPK_VA::valarray<double>&  parUp ) const;

template void BlkDiagCovBase<double>::calcPar( 
    const SPK_VA::valarray<double>& covIn,
    SPK_VA::valarray<double>&       parOut ) const;

template void BlkDiagCovBase<double>::calcCovMinRep( 
    const SPK_VA::valarray<double>& covIn,
    SPK_VA::valarray<double>&       covMinRepOut ) const;

template void BlkDiagCovBase<double>::calcCovMinRep_par( 
    const SPK_VA::valarray<double>& cov_parIn,
    int                             nCov_parInCol,
    SPK_VA::valarray<double>&       covMinRep_parOut ) const;

template void BlkDiagCovBase<double>::calcCovMinRepMask( 
    const SPK_VA::valarray<bool>& parMaskIn,
    SPK_VA::valarray<bool>&       covMinRepMaskOut ) const;

template void BlkDiagCovBase<double>::expandCovMinRep( 
    const SPK_VA::valarray<double>& covMinRepIn,
    SPK_VA::valarray<double>&       covOut ) const;


// Declare CppAD::AD<double> versions of these functions.t
template BlkDiagCovBase< CppAD::AD<double> >::BlkDiagCovBase( int nRowIn, 
                  const SPK_VA::valarray<bool>&  minRepFixedIn,
                  const SPK_VA::valarray<covStruct>&  blockStruct,
                  const SPK_VA::valarray<int>&   blockDims,
                  const SPK_VA::valarray<bool>&  blockSameAsPrev );

template BlkDiagCovBase< CppAD::AD<double> >::~BlkDiagCovBase( void );

template void BlkDiagCovBase< CppAD::AD<double> >::setPar( const SPK_VA::valarray< CppAD::AD<double> >& parIn );

template void BlkDiagCovBase< CppAD::AD<double> >::cov    ( SPK_VA::valarray< CppAD::AD<double> >& covOut     ) const;
template void BlkDiagCovBase< CppAD::AD<double> >::cov_par( SPK_VA::valarray<double>& cov_parOut ) const;

template void BlkDiagCovBase< CppAD::AD<double> >::inv    ( SPK_VA::valarray< CppAD::AD<double> >& invOut     ) const;
template void BlkDiagCovBase< CppAD::AD<double> >::inv_par( SPK_VA::valarray<double>& inv_parOut ) const;

template void BlkDiagCovBase< CppAD::AD<double> >::getParLimits(
    SPK_VA::valarray<double>&  parLow,
    SPK_VA::valarray<double>&  parUp ) const;

template void BlkDiagCovBase< CppAD::AD<double> >::calcPar( 
    const SPK_VA::valarray< CppAD::AD<double> >& covIn,
    SPK_VA::valarray< CppAD::AD<double> >&       parOut ) const;

template void BlkDiagCovBase< CppAD::AD<double> >::calcCovMinRep( 
    const SPK_VA::valarray< CppAD::AD<double> >& covIn,
    SPK_VA::valarray< CppAD::AD<double> >&       covMinRepOut ) const;

template void BlkDiagCovBase< CppAD::AD<double> >::calcCovMinRep_par( 
    const SPK_VA::valarray<double>& cov_parIn,
    int                             nCov_parInCol,
    SPK_VA::valarray<double>&       covMinRep_parOut ) const;

template void BlkDiagCovBase< CppAD::AD<double> >::calcCovMinRepMask( 
    const SPK_VA::valarray<bool>& parMaskIn,
    SPK_VA::valarray<bool>&       covMinRepMaskOut ) const;

template void BlkDiagCovBase< CppAD::AD<double> >::expandCovMinRep( 
    const SPK_VA::valarray<double>& covMinRepIn,
    SPK_VA::valarray< CppAD::AD<double> >&       covOut ) const;

