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
 * Class: FullDataCovariance
 *
 *
 * Objects of this class are full covariance matrices that are used 
 * to perform mathematical operations with the individual data 
 * covariances Ri(alp, b) and that contain pointers to the SpkModel 
 * that contain them.
 *
 *************************************************************************/
#ifndef FULLDATACOV_H
#define FULLDATACOV_H

#include "SpkValarray.h"
#include "FullCovariance.h"

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// [Revisit - Backward Compatable SpkModel Code - Mitch]
// This class is temporary and should be deleted once all of 
// the old SpkModel remnants are gone.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class SpkModel;

class FullDataCovariance : public FullCovariance 
{
  //------------------------------------------------------------
  // Constructors, destructors and assignment operators.
  //------------------------------------------------------------

public:
  FullDataCovariance();

private:
  // These are not supported for this temporary class, i.e., they
  // are not defined in the .cpp file and thus cannot be called.
  FullDataCovariance( const FullDataCovariance& );
  FullDataCovariance& operator=( const FullDataCovariance& );

public:
  virtual ~FullDataCovariance();

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
  SpkModel* pModel;

  void setModel( SpkModel* pModelIn );
};

#endif

