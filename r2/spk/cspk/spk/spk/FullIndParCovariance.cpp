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
 * File: FullIndParCovariance.cpp
 *
 *
 * Define FullIndParCovariance class derived from
 * FullCovariance class.
 *
 *
 * Author: Mitch Watrous, Sachiko Honda
 *
 *************************************************************************/
/*************************************************************************
 *
 * Class: FullIndParCovariance
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

/*
$begin FullIndParCovariance$$
$spell 
   covariance
   covariances
   int
   const
   std
   ind
   cov
$$

$section FullIndParCovariance$$

$index FullIndParCovariance$$
$index covariance, full covariance within measurements$$

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
   |   FullIndParCovariance  | |   DiagIndParCovariance  | 
    -------------------------   -------------------------
            (concrete)                 (concrete)
                         
%$$

This class encapsulates math operations and state setting functions unique to
the full covariance between individual parameters.

$end
*/

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// [Revisit - Backward Compatable SpkModel Code - Mitch]
// This class is temporary and should be deleted once all of 
// the old SpkModel remnants are gone.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#include "SpkValarray.h"
#include "FullIndParCovariance.h"
#include "DoubleMatrix.h"
#include "allZero.h"
#include "SpkException.h"
#include "SpkModel.h"

using SPK_VA::valarray;

FullIndParCovariance::~FullIndParCovariance()
{
}
FullIndParCovariance::FullIndParCovariance()
{
}
void FullIndParCovariance::setModel( SpkModel<double>* pModelIn ) 
{ 
  pModel = pModelIn; 
}
inline void FullIndParCovariance::doCov( valarray<double>& ret ) const 
{  
    pModel->indParVariance(ret); 
}
inline bool FullIndParCovariance::doCov_popPar( valarray<double>& ret ) const 
{  
  return pModel->indParVariance_popPar(ret); 
}
inline bool FullIndParCovariance::doCov_indPar( valarray<double>& ret ) const 
{  
  throw SpkException(
    SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR, 
    "FullIndParCovariance::doCov_indPar",
    __LINE__,
    __FILE__);
}

inline void FullIndParCovariance::doInv( valarray<double>& ret ) const 
{  
  pModel->indParVarianceInv(ret); 
}
inline bool FullIndParCovariance::doInv_popPar( valarray<double>& ret ) const 
{  
  return pModel->indParVarianceInv_popPar(ret); 
}
inline bool FullIndParCovariance::doInv_indPar( valarray<double>& ret ) const 
{  
  throw SpkException(
    SpkError::SPK_MODEL_NOT_IMPLEMENTED_ERR, 
    "FullIndParCovariance::doInv_indPar",
    __LINE__,
    __FILE__);
}
