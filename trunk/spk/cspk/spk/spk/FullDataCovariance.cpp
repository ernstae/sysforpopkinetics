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
 * File: FullDataCovariance.cpp
 *
 *
 * Define FullDataCovariance class derived from
 * FullCovariance and DataCovariance classes.
 *
 *
 * Author: Mitch Watrous, Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Class: FullDataCovariance
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

/*
$begin FullDataCovariance$$
$spell 
   covariance
   covariances
   int
   const
   std
   ind
   cov
$$

$section FullDataCovariance$$

$index FullDataCovariance$$
$index covariance, full covariance within individual parameters$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$
This is a subclass of FullCovariance class which is derived from Covariance class as shown below:
$math%

                ------------------------
               |       Covariance       |    
                ------------------------
                       (abstract)
                /|\                  /|\           
                 |                    |
                 |                    |
     ------------------------   ------------------------ 
    |     FullCovariance     | |     DiagCovariance     |
     ------------------------  -------------------------
            (abstract)                 (abstract)
                /|\                        /|\         
                 |                          |
                 |                          |
    -------------------------   -------------------------
   |    FullDataCovariance   | |    DiagDataCovariance   | 
    -------------------------   -------------------------
            (concrete)                 (concrete)
                         
%$$

This class encapsulates math operations and state setting functions unique to
the full covariance between measurements.

$end
*/

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// [Revisit - Backward Compatable SpkModel Code - Mitch]
// This class is temporary and should be deleted once all of 
// the old SpkModel remnants are gone.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#include "SpkValarray.h"
#include "FullDataCovariance.h"
#include "SpkModel.h"
#include "DoubleMatrix.h"
#include "SpkException.h"

using SPK_VA::valarray;

FullDataCovariance::~FullDataCovariance()
{
}

FullDataCovariance::FullDataCovariance()
{
}
void FullDataCovariance::setModel( SpkModel<double>* pModelIn ) 
{ 
  pModel = pModelIn; 
}

inline void FullDataCovariance::doCov( valarray<double>& ret ) const 
{  
  pModel->dataVariance(ret); 
}
inline bool FullDataCovariance::doCov_popPar( valarray<double>& ret ) const 
{  
  return pModel->dataVariance_popPar(ret); 
}
inline bool FullDataCovariance::doCov_indPar( valarray<double>& ret ) const 
{  
  return pModel->dataVariance_indPar(ret); 
}

inline void FullDataCovariance::doInv( valarray<double>& ret ) const 
{  
  pModel->dataVarianceInv(ret); 
}
inline bool FullDataCovariance::doInv_popPar( valarray<double>& ret ) const 
{  
  return pModel->dataVarianceInv_popPar(ret); 
}
inline bool FullDataCovariance::doInv_indPar( valarray<double>& ret ) const 
{  
  return pModel->dataVarianceInv_indPar(ret); 
}
