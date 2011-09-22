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
 * File: SpdModel.h
 *
 *
 * This is the base class for population optimal design models.
 *
 * Most of its functions are copies of analagous functions from
 * SpkModel with minor modifications.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef SPDMODEL_H
#define SPDMODEL_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK include files.
#include "SpkModel.h"


/*------------------------------------------------------------------------
 * Class Declaration
 *------------------------------------------------------------------------*/

class SpdModel : public SpkModel
{
  //------------------------------------------------------------
  // Constructors and destructors.
  //------------------------------------------------------------

protected:
  // These are protected (rather than public) because this is an 
  // abstract class and they should not be called directly.
  // These are protected (rather than private) because subclasses
  // of this class may need to call these functions.
  SpdModel( const SPK_VA::valarray<int>&  nIndDesParIn,
            int                           nCommonDesParIn,
            int                           nIndParIn )
    :
    nInd              ( nIndDesParIn.size() ),
    nDesPar           ( nIndDesParIn.sum() + nCommonDesParIn ),
    nCommonDesPar     ( nCommonDesParIn ),
    nIndPar           ( nIndParIn ),
    nIndDesPar        ( nIndDesParIn )
    {
      assert( nInd          >= 0 );
      assert( nDesPar       >= 0 );
      assert( nCommonDesPar >= 0 );
    }

  SpdModel( const SpdModel &right );
  SpdModel& operator=(const SpdModel &right);

public:
    // This destructor is public so that it can be called directly.   
    // The destructor must be virtual for an abstract class.
    virtual ~SpdModel() {}


  //------------------------------------------------------------
  // State changing functions.
  //------------------------------------------------------------

  // These functions are inherited from SpkModel and are not 
  // defined by this class.
  //
  // [Revisit - Unspecified Inherited Virtual Functions - Mitch]
  // Do the inherited virtual functions from SpkModel that are not
  // defined in this class need to be specified in this class?
  /*
private:
  virtual void doSelectIndividual ( int base0 );

  virtual void doSetPopPar( const SPK_VA::valarray<double>& inVA );
  virtual void doSetIndPar( const SPK_VA::valarray<double>& inVA ) = 0;
  */

public:
  void setDesPar( const SPK_VA::valarray<double>& inVA );

private:
  // This function is not pure virtual because it is not one of the
  // functions required for SPK to estimate individual parameters.
  virtual void doSetDesPar( const SPK_VA::valarray<double>& inVA );


  //------------------------------------------------------------
  // Model evaluation functions.
  //------------------------------------------------------------

  // These functions are inherited from SpkModel and are not 
  // defined by this class.
  //
  // [Revisit - Unspecified Inherited Virtual Functions - Mitch]
  // Do the inherited virtual functions from SpkModel that are not
  // defined in this class need to be specified in this class?
  /*
private:
  virtual void doDataMean( SPK_VA::valarray<double>& ret ) const = 0;
  virtual bool doDataMean_indPar( SPK_VA::valarray<double>& ret ) const = 0;
  virtual bool doDataMean_popPar( SPK_VA::valarray<double>& ret ) const;

  virtual void doDataVariance( SPK_VA::valarray<double>& ret ) const = 0;
  virtual bool doDataVariance_indPar( SPK_VA::valarray<double>& ret ) const = 0;
  virtual bool doDataVariance_popPar( SPK_VA::valarray<double>& ret ) const;

  virtual void doDataVarianceInv( SPK_VA::valarray<double>& ret ) const;
  virtual bool doDataVarianceInv_indPar( SPK_VA::valarray<double>& ret ) const;
  virtual bool doDataVarianceInv_popPar( SPK_VA::valarray<double>& ret ) const;

  virtual void doIndParVariance( SPK_VA::valarray<double>& ret ) const;
  virtual bool doIndParVariance_popPar( SPK_VA::valarray<double>& ret ) const;
  virtual void doIndParVarianceInv( SPK_VA::valarray<double>& ret ) const;
  virtual bool doIndParVarianceInv_popPar( SPK_VA::valarray<double>& ret ) const;
  */

public:
  bool dataMean_desPar( SPK_VA::valarray<double>& ret ) const;
  bool dataMean_indPar_desPar( SPK_VA::valarray<double>& ret ) const;
  bool dataMean_indPar_popPar( SPK_VA::valarray<double>& ret ) const;

  bool dataVariance_desPar( SPK_VA::valarray<double>& ret ) const;

  bool dataVarianceInv_desPar( SPK_VA::valarray<double>& ret ) const;

  void popParPrior( double& ret ) const;
  bool popParPrior_popPar( SPK_VA::valarray<double>& ret ) const;

private:
  // These functions are not pure virtual because they are not one of the
  // functions required for SPK to estimate individual parameters.
  virtual bool doDataMean_desPar( SPK_VA::valarray<double>& ret ) const;
  virtual bool doDataMean_indPar_desPar( SPK_VA::valarray<double>& ret ) const;
  virtual bool doDataMean_indPar_popPar( SPK_VA::valarray<double>& ret ) const;

  // This function is not pure virtual because it is not one of the
  // functions required for SPK to estimate individual parameters.
  virtual bool doDataVariance_desPar( SPK_VA::valarray<double>& ret ) const;

  // This function is not pure virtual because it is not one of the
  // functions required for SPK to estimate individual parameters.
  virtual bool doDataVarianceInv_desPar( SPK_VA::valarray<double>& ret ) const;

  // These functions are not pure virtual because they are not one of the
  // functions required for SPK to estimate individual parameters.
  virtual void doPopParPrior( double& ret ) const;
  virtual bool doPopParPrior_popPar( SPK_VA::valarray<double>& ret ) const;

  //------------------------------------------------------------
  // Miscellaneous functions and members.
  //------------------------------------------------------------

public:
  int getNInd() const { return nInd; }
  int getNDesPar() const { return nDesPar; }
  int getNCommonDesPar() const { return nCommonDesPar; }
  int getNIndPar() const { return nIndPar; }

  int getNIndDesPar( int iCurr ) const { return nIndDesPar[ iCurr ]; }

private:
  const int nInd;                 // Number of individuals.
  const int nDesPar;              // Number of design parameters.
  const int nCommonDesPar;        // Number of common design parameters.
  const int nIndPar;              // Number of individual parameters.

  SPK_VA::valarray<int> nIndDesPar; // Number of design parameters
                                    // for each individual.


  //------------------------------------------------------------
  // Disallowed, implicitly generated member functions.
  //------------------------------------------------------------

private:
  // Because many of the data members are constant and need to be
  // initialized when the SpdModel object is constructed, a
  // default constructor, i.e., a constructor with no arguments,
  // is not appropriate and should never be called. This default 
  // constructor is therefore declared private here so that it won't 
  // be called from outside the class, and it is purposely not 
  // defined in SpdModel.cpp so that a link error will occur if
  // it's called by member or friend functions.
  // (See Item 27 from "Effective C++", Second Edition, Scott 
  // Meyers, Addison-Wesley, 1998.)
  SpdModel();

};

#endif
