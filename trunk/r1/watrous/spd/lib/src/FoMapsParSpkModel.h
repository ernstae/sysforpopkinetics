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
 * File: FoMapsParSpkModel.h
 *
 *
 * Maps individual to population parameters, maps population to design
 * parameters, and evaluates first order approximations for the mean and 
 * covariance of the data based on the model functions for the SpdModel
 * that it contains.
 *
 * In particular, objects of this class evaluate the approximate
 * model functions 
 *
 *    fTilde_i(chi_i, alp, b_i = 0)  
 *
 * and
 *
 *    VTilde_i(chi_i, alp)  ,
 *
 * where
 *
 *    fTilde_i(chi_i, alp, b_i)  =  f_i(chi_i, alp, 0)  +  f_i_b(chi_i, alp, 0) * b_i  ,
 *
 *
 *    VTilde_i(chi_i, alp)  =  RTilde_i(chi_i, alp)
 *                                                                       T
 *        +  fTilde_i_b(chi_i, alp, 0)  D(alp)  fTilde_i_b(chi_i, alp, 0)  ,
 *
 *
 *    RTilde_i(chi_i, alp)  =  R_i(chi_i, alp, 0)  ,
 *
 * and
 *                                T
 *     chi_i  =  ( x_i, x_common )   
 *
 * is a vector that contains the design parameters for the current
 * individual i combined with the design parameters common to all 
 * individuals.
 *
 * Objects of this class also evaluate the derivatives of these 
 * functions with respect to chi_i, and they evaluate the derivatives 
 * with respect to the fixed population parameter vector alp.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef FOMAPSPARSPKMODEL_H
#define FOMAPSPARSPKMODEL_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPD include files.
#include "SpdModel.h"

// SPK include files.
#include "SpkModel.h"
#include "multiply.h"
#include "transpose.h"
#include "transposeDerivative.h"
#include "ABA_x.h"
#include "identity.h"
#include "AkronItimesC.h"
#include "IkronBtimesC.h"
#include "add.h"
#include "replaceSubblock.h"
#include "getSubblock.h"

// Standard include files.
#include <cmath>


/*------------------------------------------------------------------------
 * Class Declaration
 *------------------------------------------------------------------------*/

class FoMapsParSpkModel : public SpkModel
{
  //------------------------------------------------------------
  // Constructors.
  //------------------------------------------------------------

public:
  FoMapsParSpkModel( SpdModel* const pSpdModelIn )
    :
    pSpdModel ( pSpdModelIn ),
    nIndPar   ( pSpdModelIn->getNIndPar() ),
    desParCurr ( pSpdModelIn->getNDesPar() )
  {
    // The individual parameter state variable is always equal 
    // to zero for objects of this class.
    zeroIndPar();
  }

  // [Revisit - Population Parameter Step Size in FoMapsParSpkModel - Mitch]
  // Because lambda does not take the population parameter step
  // size as an argument, it must be made a member of the FoMapsParSpkModel
  // class.  In the future, when SPK is reorganized to accomodate
  // the optimal design system, then this step size should be
  // taken out of FoMapsParSpkModel and made an explicit argument.
  //
  FoMapsParSpkModel( 
    SpdModel* const                 pSpdModelIn,
    const SPK_VA::valarray<double>  popParStepIn )
    :
    pSpdModel  ( pSpdModelIn ),
    nIndPar    ( pSpdModelIn->getNIndPar() ),
    popParStep ( popParStepIn ),
    desParCurr ( pSpdModelIn->getNDesPar() )
  {
    // The individual parameter state variable is always equal 
    // to zero for objects of this class.
    zeroIndPar();
  }

private:
  void zeroIndPar() const
  {
    SPK_VA::valarray<double> indPar( nIndPar );
    indPar = 0.0;
    pSpdModel->setIndPar( indPar );
  }

private:
  // A default constructor should never be called for this class.
  // This default constructor is declared private and not defined
  // in FoMapsParSpkModel.cpp so it won't be called.
  FoMapsParSpkModel();


  //------------------------------------------------------------
  // Constant members.
  //------------------------------------------------------------

private:
  SpdModel* const pSpdModel;  // The population optimal design model.
  const int       nIndPar;    // Number of individual parameters.


  //------------------------------------------------------------
  // Nonconstant members.
  //------------------------------------------------------------

private:
  // [Revisit - Population Parameter Step Size in FoMapsParSpkModel - Mitch]
  // Because lambda does not take the population parameter step
  // size as an argument, it must be made a member of the FoMapsParSpkModel
  // class.  In the future, when SPK is reorganized to accomodate
  // the optimal design system, then this step size should be
  // taken out of FoMapsParSpkModel and made an explicit argument.
  SPK_VA::valarray<double> popParStep;
  
  // [Revisit - Current Design Parameter in FoMapsParSpkModel - Mitch]
  // Because mapObj does not take the current design parameter value
  // as an argument, it must be made a member of the FoMapsParSpkModel
  // class.  In the future, when SPK is reorganized to accomodate
  // the optimal design system, then this parameter should be
  // taken out of FoMapsParSpkModel and made an explicit argument.
  SPK_VA::valarray<double> desParCurr;


  //------------------------------------------------------------
  // State changing functions.
  //------------------------------------------------------------

private:
  void doSelectIndividual( int iBase0 )
  {
    pSpdModel->selectIndividual( iBase0 );
  }

  // Map the population parameters to the design parameters.
  void doSetPopPar( const SPK_VA::valarray<double>& in )
  {
    pSpdModel->setDesPar( in );
    assert( in.size() == pSpdModel->getNDesPar() );

    // [Revisit - Current Design Parameter in FoMapsParSpkModel - Mitch]
    // Because mapObj does not take the current design parameter value
    // as an argument, it must be made a member of the FoMapsParSpkModel
    // class.  In the future, when SPK is reorganized to accomodate
    // the optimal design system, then this parameter should be
    // taken out of FoMapsParSpkModel and made an explicit argument.
    desParCurr = in;
  }

  // Map the individual parameters to the population parameters.
  void doSetIndPar( const SPK_VA::valarray<double>& in )
  {
    pSpdModel->setPopPar( in );
  }


  //------------------------------------------------------------
  // Miscellaneous functions.
  //------------------------------------------------------------

public:
  // Returns a pointer the to SpdModel this class contains.
  SpdModel* const getSpdModel() const
  {
    return pSpdModel;
  }

  // [Revisit - Population Parameter Step Size in FoMapsParSpkModel - Mitch]
  // Because lambda does not take the population parameter step
  // size as an argument, it must be made a member of the FoMapsParSpkModel
  // class.  In the future, when SPK is reorganized to accomodate
  // the optimal design system, then this step size should be
  // taken out of FoMapsParSpkModel and made an explicit argument.
  void getPopParStep( SPK_VA::valarray<double>& ret ) const
  {
    ret.resize( popParStep.size() );
    ret = popParStep;
  }

  // [Revisit - Current Design Parameter in FoMapsParSpkModel - Mitch]
  // Because mapObj does not take the current design parameter value
  // as an argument, it must be made a member of the FoMapsParSpkModel
  // class.  In the future, when SPK is reorganized to accomodate
  // the optimal design system, then this parameter should be
  // taken out of FoMapsParSpkModel and made an explicit argument.
  void getDesPar( SPK_VA::valarray<double>& ret ) const
  {
    ret.resize( desParCurr.size() );
    ret = desParCurr;
  }


  //------------------------------------------------------------
  // Model evaluation functions.
  //------------------------------------------------------------

private:
  // Evaluates
  //
  //     fTilde_i(chi_i, alp, 0)  .
  //
  void doDataMean( SPK_VA::valarray<double>& ret ) const
  {
    // Evaluate
    //
    //     f_i(chi_i, alp, 0)  =  fTilde_i(chi_i, alp, 0)  .
    //
    pSpdModel->dataMean( ret );
  }

  // Evaluates
  //
  //    fTilde_i_alp(chi_i, alp, 0)  .
  //
  // Note that this function returns the derivative with respect
  // to the population parameter.  The reason for returning the 
  // population parameter derivative is that objects of this class
  // map individual to population parameters.
  bool doDataMean_indPar( SPK_VA::valarray<double>& ret ) const
  {
    // Evaluate
    //
    //     f_i_alp(chi_i, alp, 0)  =  fTilde_i_alp(chi_i, alp, 0)  .
    //
    return pSpdModel->dataMean_popPar( ret );
  }

  // Evaluates
  //
  //     fTilde_i_chi(chi_i, alp, 0)  .
  //
  // Note that this function returns the derivative with respect
  // to the design parameter subset, chi_i.  The reason for returning  
  // the design parameter subset derivative is that objects of this
  // class map population to design parameters.
  bool doDataMean_popPar( SPK_VA::valarray<double>& ret ) const
  {
    // Evaluate
    //
    //     f_i_chi(chi_i, alp, 0)  =  fTilde_i_chi(chi_i, alp, 0)  .
    //
    return pSpdModel->dataMean_desPar( ret );
  }

  // Evaluates
  //
  //     VTilde_i(chi_i, alp)  .
  //
  void doDataVariance( SPK_VA::valarray<double>& ret ) const
  {
    //------------------------------------------------------------
    // Preliminaries.
    //------------------------------------------------------------

    using SPK_VA::valarray;
    

    //------------------------------------------------------------
    // Evaluate the model functions.
    //------------------------------------------------------------

    // Evaluate
    //
    //     f_i_b(chi_i, alp, 0)  =  fTilde_i_b(chi_i, alp, 0)  .
    //
    valarray<double> f_b;
    bool notAllZeroF_b = pSpdModel->dataMean_indPar( f_b );

    // Evaluate
    //
    //     R_i(chi_i, alp, 0)  =  RTilde_i(chi_i, alp)  .
    //
    valarray<double> R;
    pSpdModel->dataVariance( R );

    // If necessary, evaluate
    //
    //     D(alp)  .
    //
    valarray<double> D;
    if ( notAllZeroF_b )
    {
      pSpdModel->indParVariance( D );
    }


    //------------------------------------------------------------
    // Calculate the approximate model function.
    //------------------------------------------------------------

    // Prepare the return value.
    int nY_i = static_cast<int>( sqrt( static_cast<double>( R.size() ) ) );
    ret.resize( nY_i * nY_i );

    // Calculate
    //
    //     VTilde_i(chi_i, alp)  =  RTilde_i(chi_i, alp)
    //                                                                       T
    //        +  fTilde_i_b(chi_i, alp, 0)  D(alp)  fTilde_i_b(chi_i, alp, 0)  .
    //
    if ( notAllZeroF_b )
    {
      assert( f_b.size() == nY_i * nIndPar );
      assert( D.size()   == nIndPar * nIndPar );

      valarray<double> f_bTranspose( nIndPar * nY_i );
      f_bTranspose = transpose( f_b, nIndPar );

      valarray<double> temp1( nIndPar * nY_i );
      valarray<double> temp2( nY_i * nY_i );

      temp1 = multiply( D,   nIndPar, f_bTranspose, nY_i );
      temp2 = multiply( f_b, nIndPar, temp1,        nY_i );

      ret = R + temp2;
    }
    else
    {
      ret = R;
    }
  }

  // Evaluates
  //
  //     VTilde_i_alp(chi_i, alp)  .
  //
  // Note that this function returns the derivative with respect
  // to the population parameter.  The reason for returning the 
  // population parameter derivative is that objects of this class
  // map individual to population parameters.
  bool doDataVariance_indPar( SPK_VA::valarray<double>& ret ) const
  {
    //------------------------------------------------------------
    // Preliminaries.
    //------------------------------------------------------------

    using SPK_VA::valarray;
    

    //------------------------------------------------------------
    // Evaluate the model functions.
    //------------------------------------------------------------

    // Evaluate
    //
    //     f_i_b_alp(chi_i, alp, 0)  =  fTilde_i_b_alp(chi_i, alp, 0)  .
    //
    // Note that this derivative is calculated before the first derivative
    // to allow caching of the first derivative by this derivative's code.
    valarray<double> f_b_alp;
    bool notAllZeroF_b_alp = pSpdModel->dataMean_indPar_popPar( f_b_alp );
    
    // Evaluate
    //
    //     f_i_b(chi_i, alp, 0)  =  fTilde_i_b(chi_i, alp, 0)  .
    //
    valarray<double> f_b;
    bool notAllZeroF_b = pSpdModel->dataMean_indPar( f_b );

    // Evaluate
    //
    //     R_i_alp(chi_i, alp, 0)  =  RTilde_i_alp(chi_i, alp)  .
    //
    valarray<double> R_alp;
    bool notAllZeroR_alp = pSpdModel->dataVariance_popPar( R_alp );

    // If necessary, evaluate
    //
    //     D_alp(alp)  .
    //
    // Note that this derivative is calculated before the function 
    // to allow caching of the function by the derivative code.
    valarray<double> D_alp;
    bool notAllZeroD_alp;
    if ( notAllZeroF_b && notAllZeroF_b_alp )
    {
      notAllZeroD_alp = pSpdModel->indParVariance_popPar( D_alp );
    }

    // If necessary, evaluate
    //
    //     D(alp)  .
    //
    valarray<double> D;
    if ( notAllZeroF_b && ( notAllZeroF_b_alp || notAllZeroD_alp ) )
    {
      pSpdModel->indParVariance( D );
    }


    //------------------------------------------------------------
    // Calculate the approximate model function.
    //------------------------------------------------------------

    // Prepare the return value.
    ret.resize( R_alp.size() );

    bool notAllZero;

    // Calculate
    //
    //     VTilde_i_alp(chi_i, alp)  =  RTilde_i_alp(chi_i, alp)
    //
    //                  -                                                               -
    //                 |                                                              T  |
    //        +  d     |  fTilde_i_b(chi_i, alp, 0)  D(alp)  fTilde_i_b(chi_i, alp, 0)   |  .
    //            alp  |                                                                 |
    //                  -                                                               -
    //
    if ( notAllZeroF_b && ( notAllZeroF_b_alp || notAllZeroD_alp ) )
    {
      notAllZero = true;

      int nY_i    = f_b.size() / nIndPar;
      int nPopPar = D_alp.size() / ( nIndPar * nIndPar );

      assert( f_b_alp.size() == nY_i * nIndPar * nPopPar );
      assert( R_alp.size()   == nY_i * nY_i * nPopPar );
      assert( D.size()       == nIndPar * nIndPar );

      valarray<double> f_bTranspose( nIndPar * nY_i );
      valarray<double> f_bTranspose_alp( nIndPar * nY_i * nPopPar );

      f_bTranspose = transpose( f_b, nIndPar );
      f_bTranspose_alp = transposeDerivative( f_b_alp, nIndPar, nY_i, nPopPar );

      ret = R_alp + ABA_x( f_bTranspose, nY_i, D, nIndPar, f_bTranspose_alp, D_alp, nPopPar );
    }
    else
    {
      if ( notAllZeroR_alp )
      {
        notAllZero = true;
        ret        = R_alp;
      }
      else
      {
        assert( ret.size() > 0 );

        notAllZero = false;
        ret        = 0.0;
      }
    }


    //------------------------------------------------------------
    // Finish up.
    //------------------------------------------------------------

    return notAllZero;
  }

  // Evaluates
  //
  //     VTilde_i_chi(chi_i, alp)  .
  //
  // Note that this function returns the derivative with respect
  // to the design parameter subset, chi_i.  The reason for returning  
  // the design parameter subset derivative is that objects of this
  // class map population to design parameters.
  bool doDataVariance_popPar( SPK_VA::valarray<double>& ret ) const
  {
    //------------------------------------------------------------
    // Preliminaries.
    //------------------------------------------------------------

    using SPK_VA::valarray;
    

    //------------------------------------------------------------
    // Evaluate the model functions.
    //------------------------------------------------------------

    // Evaluate
    //
    //     f_i_b_chi(chi_i, alp, 0)  =  fTilde_i_b_chi(chi_i, alp, 0)  .
    //
    // Note that this derivative is calculated before the first derivative
    // to allow caching of the first derivative by this derivative's code.
    valarray<double> f_b_chi;
    bool notAllZeroF_b_chi = pSpdModel->dataMean_indPar_desPar( f_b_chi );
    
    // If necessary, evaluate
    //
    //     f_i_b(chi_i, alp, 0)  =  fTilde_i_b(chi_i, alp, 0)  .
    //
    valarray<double> f_b;
    bool notAllZeroF_b;
    if ( notAllZeroF_b_chi )
    {
      notAllZeroF_b = pSpdModel->dataMean_indPar( f_b );
    }

    // Evaluate
    //
    //     R_i_chi(chi_i, alp, 0)  =  RTilde_i_chi(chi_i, alp)  .
    //
    valarray<double> R_chi;
    bool notAllZeroR_chi = pSpdModel->dataVariance_desPar( R_chi );

    // If necessary, evaluate
    //
    //     D(alp)  .
    //
    valarray<double> D;
    if ( notAllZeroF_b && notAllZeroF_b_chi )
    {
      pSpdModel->indParVariance( D );
    }


    //------------------------------------------------------------
    // Calculate the approximate model function.
    //------------------------------------------------------------

    // Prepare the return value.
    ret.resize( R_chi.size() );

    bool notAllZero;

    // Calculate
    //
    //     VTilde_i_chi(chi_i, alp)  =  RTilde_i_chi(chi_i, alp)
    //
    //                  -                                                               -
    //                 |                                                              T  |
    //        +  d     |  fTilde_i_b(chi_i, alp, 0)  D(alp)  fTilde_i_b(chi_i, alp, 0)   |  .
    //            chi  |                                                                 |
    //                  -                                                               -
    //
    if ( notAllZeroF_b && notAllZeroF_b_chi )
    {
      notAllZero = true;

      int nY_i   = f_b.size() / nIndPar;
      int nChi_i = f_b_chi.size() / ( nIndPar * nY_i );

      assert( R_chi.size() == nY_i * nY_i * nChi_i );
      assert( D.size()     == nIndPar * nIndPar );

      valarray<double> f_bTranspose( nIndPar * nY_i );
      valarray<double> f_bTranspose_chi( nIndPar * nY_i * nChi_i );

      f_bTranspose = transpose( f_b, nIndPar );
      f_bTranspose_chi = transposeDerivative( f_b_chi, nIndPar, nY_i, nChi_i );

      // In order to calculate
      //
      //            -                                                               -
      //           |                                                              T  |
      //     d     |  fTilde_i_b(chi_i, alp, 0)  D(alp)  fTilde_i_b(chi_i, alp, 0)   |  ,
      //      chi  |                                                                 |
      //            -                                                               -
      //
      // let
      //
      //     A(x)  =  fTilde_i_b(chi_i, alp, 0)  ,
      //
      //
      //     B(x)  =  D( alp )  ,
      //
      //                                       T
      //     C(x)  =  fTilde_i_b(chi_i, alp, 0)   ,
      //
      // and
      //
      //     x     = chi_i  .
      //
      // Then, use Corollary 7 from B. M. Bell, "Approximating The 
      // Marginal Likelihood Estimate For Models With Random Parameters", 
      // Applied Mathematics and Computation, 119 (2001), pp. 57-73:
      //
      //     d  [ A(x) B(x) C(x) ]  =  [ ( A(x) B(x) )  kron  I_d ]  C_x(x)
      //      x
      //
      //                                                 T
      //                            +  [ A(x)  kron  C(x)  ]  B_x(x)
      //
      //
      //                                                  T     T
      //                            +  [ I_a  kron  ( C(x)  B(x)  ) ]  A_x(x)
      //
      // where kron is the Kronecker product operator, I_a is an identity
      // matrix with the same number of rows as A, and I_d is an identity
      // matrix with the same number of columns as C.
      //
      // In this case, B(x) does not actually depend on x, i.e., D(alp) 
      // does not depend on chi_i, so the second term in the above expression
      // is equal to zero.  Also, since C is just the transpose of A, 
      // I_a and I_d are the same size.  Finally, since B is symmetric,
      //
      //                       T     T
      //     A(x) B(x)  =  C(x)  B(x)  .
      //
      valarray<double> I_a( nY_i * nY_i );
      identity( nY_i, I_a );

      valarray<double> temp1( nY_i * nIndPar );
      valarray<double> temp2( nY_i * nY_i * nChi_i );

      temp1 = multiply( f_b, nIndPar, D, nIndPar );

      temp2 = 
	AkronItimesC( temp1,             nIndPar, 
		      I_a,               nY_i, 
		      f_bTranspose_chi,  nChi_i )
        +
	IkronBtimesC( I_a,               nY_i,
		      temp1,             nIndPar, 
		      f_b_chi,           nChi_i );
 
      ret = R_chi + temp2;
    }
    else
    {
      if ( notAllZeroR_chi )
      {
        notAllZero = true;
        ret        = R_chi;
      }
      else
      {
        assert( ret.size() > 0 );

        notAllZero = false;
        ret        = 0.0;
      }
    }


    //------------------------------------------------------------
    // Finish up.
    //------------------------------------------------------------

    return notAllZero;
  }

  // This function uses the default versions provided by SpkModel
  // and is not defined by this class.
  //
  // [Revisit - Unspecified Inherited Virtual Functions - Mitch]
  // Do the inherited virtual functions from SpkModel that are not
  // defined in this class need to be specified in this class?
  /*
  virtual void doDataVarianceInv( SPK_VA::valarray<double>& ret ) const;
  */

  // Evaluates
  //                            -1
  //     [  VTilde_i(chi_i, alp)    ]_alp  .
  //
  // Note that this function returns the derivative with respect
  // to the population parameter.  The reason for returning the 
  // population parameter derivative is that objects of this class
  // map individual to population parameters.
  bool doDataVarianceInv_indPar( SPK_VA::valarray<double>& ret ) const
  {
    return pSpdModel->dataVarianceInv_popPar( ret );
  }

  // Evaluates
  //                            -1
  //     [  VTilde_i(chi_i, alp)    ]_chi  .
  //
  // Note that this function returns the derivative with respect
  // to the design parameter subset, chi_i.  The reason for returning  
  // the design parameter subset derivative is that objects of this
  // class map population to design parameters.
  bool doDataVarianceInv_popPar( SPK_VA::valarray<double>& ret ) const
  {
    return pSpdModel->dataVarianceInv_desPar( ret );
  }

  // Evaluates
  //
  //     D(alp)  .
  //
  // Note that the mapping of individual to population parameters
  // and population to design parameters does not apply to D(alp).
  void doIndParVariance( SPK_VA::valarray<double>& ret ) const
  {
    pSpdModel->indParVariance( ret );
  }

  // Evaluates
  //
  //     D_alp(alp)  .
  //
  // Note that the mapping of individual to population parameters
  // and population to design parameters does not apply to D(alp).
  bool doIndParVariance_popPar( SPK_VA::valarray<double>& ret ) const
  {
    return pSpdModel->indParVariance_popPar( ret );
  }

  // These functions use the default versions provided by SpkModel
  // and are not defined by this class.
  //
  // [Revisit - Unspecified Inherited Virtual Functions - Mitch]
  // Do the inherited virtual functions from SpkModel that are not
  // defined in this class need to be specified in this class?
  /*
  virtual void doIndParVarianceInv( SPK_VA::valarray<double>& ret ) const;
  virtual bool doIndParVarianceInv_popPar( SPK_VA::valarray<double>& ret ) const;
  */

};

#endif
