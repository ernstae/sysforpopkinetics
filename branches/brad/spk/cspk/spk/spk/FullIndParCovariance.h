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
 * Class: FullIndParCovariance
 *
 *
 * Objects of this class are full covariance matrices that are used to
 * perform mathematical operations with the random population
 * parameter covariance D(alp) and that contain pointers to the
 * SpkModel that contains them.
 *
 *************************************************************************/
#ifndef FULLINDPARCOV_H
#define FULLINDPARCOV_H

#include "SpkValarray.h"
#include "SpkModel.h"
#include "FullCovariance.h"

class FullIndParCovariance : public FullCovariance 
{
  //------------------------------------------------------------
  // Constructors, destructors and assignment operators.
  //------------------------------------------------------------

public:
  FullIndParCovariance();

private:
  // These are not supported for this temporary class, i.e., they
  // are not defined in the .cpp file and thus cannot be called.
  FullIndParCovariance( const FullIndParCovariance& );
  FullIndParCovariance& operator=(const FullIndParCovariance& );

public:
  virtual ~FullIndParCovariance();

  //------------------------------------------------------------
  // Mathematical functions.
  //------------------------------------------------------------

private:
  inline void doCov       ( SPK_VA::valarray<double>& ret ) const;
  inline bool doCov_popPar( SPK_VA::valarray<double>& ret ) const;
  inline bool doCov_indPar( SPK_VA::valarray<double>& ret ) const;

  inline void doInv       ( SPK_VA::valarray<double>& ret ) const;
  inline bool doInv_popPar( SPK_VA::valarray<double>& ret ) const;
  inline bool doInv_indPar( SPK_VA::valarray<double>& ret ) const;

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  // BACKWARD COMPATABLE BACKWARD COMPATABLE BACKWARD COMPATABLE
  // 
  // [ Revisit - Backward Compatable Section ]
  // The whole remaining block of code here down 
  // is deprecated and should be deleted once all
  // of the old Covariance remnants are gone.
  //
  // BACKWARD COMPATABLE BACKWARD COMPATABLE BACKWARD COMPATABLE
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //------------------------------------------------------------
  // SpkModel class backward compatability functions and data.
  //------------------------------------------------------------

public:
  SpkModel<double>* pModel;

  void setModel( SpkModel<double>* pModelIn );

};

#endif
