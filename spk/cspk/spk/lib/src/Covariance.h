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
 * File: Covariance.h
 *
 *
 * This is the very top abstract base class for all covariance matrix classes.
 *
 *
 * It defines the functions and data members that manage the status of 
 * the cache, but these functios do not actually perform the caching.
 * This class declaration also contains virtual functions to 
 * the mathematical functions that must be defined in its subclasses.
 *
 *
 * Author: Mitch Watrous
 * Author: Sachiko Honda
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Top Page of Omhelp Specification
 *------------------------------------------------------------------------*/
/*
$begin Covariance$$
$spell
  covariance
  logdet
  instantiated
  ind
$$

$section Covariance Class$$

$cindex Covariance Class$$
$cindex Base \Class \for \Covariance \Matrix \Classes$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$

This is the very top level abstract base class for all covariance matrix classes.
This class defines the functions and data members that manage the status of
the cache, but these functions do not actually perform the caching.  
This class declaration also contains virtual functions to the mathematical
functions that must be defined in its subclasses.

$pre

$$
Objects of this class are used to calculate and cache the following
quantities for a covariance matrix:
$table  
(1.) the covariance matrix, $rend
(2.) the inverse of the covariance matrix, $rend
(3.) the log determinant of the covariance matrix, and $rend
(4.) the weighted sum of squares of an arbitrary vector. $rend
$tend
In addition, objects of this class are used to evaluate the 
derivatives of these quantities with respect to the population
and the individual parameters.
$pre

$$
The following diagram shows the Covariance class hierarchy:
$pre
              --------------------------
             |         Covariance       | ( This exists for caching mechanism )
              --------------------------
                       (Abstract)
              /|\                   /|\        
               |                     | 
      math     |                     |       states
   -----------------------    -------------------------
  |     FullCovariance    |  |  Data/IndParCovariance  | (This contains pointers to functions declared in the left hand side)
   -----------------------    -------------------------
           (Abstract)            (Abstract)
              /|\                   /|\
               |                     |
               |                     |
              --------------------------
          ex.|    FullDataCovariance    |  
              --------------------------
                      (Abstract)
                         /|\
                          |
                          |
              --------------------------
          ex.|  UserFullDataCovariance  |
              --------------------------
                      (concrete)
$$

This class defines the functions and data members that manage the status of
the cache, but these functions do not actually perform the caching.  
This class declaration also contains virtual functions to the mathematical
functions that must be defined in its subclasses.

$head Note$$

This class declares pure virtual functions that calculate the
above mathematical quantities and their derivatives.  
These functions must be defined in concrete subclasses of this class 
before any $code Covariance$$ objects may be instantiated.

$contents/
  Covariance.cpp/$$

$end
*/

#ifndef COVARIANCE_H
#define COVARIANCE_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
// Standard include files.
#pragma warning( disable : 4786 )
#include <vector>

#include "SpkValarray.h"

/*------------------------------------------------------------------------
 * Class declaration
 *------------------------------------------------------------------------*/

class Covariance
{
  //------------------------------------------------------------
  // Constructors, destructors, and assignment operators.
  //------------------------------------------------------------

protected:
  // These are protected (rather than public) because this is an 
  // abstract class and they should not be called directly.
  // These are protected (rather than private) because subclasses
  // of this class may need to call these functions.
  Covariance();
  Covariance( const Covariance &right );
  Covariance& operator=(const Covariance &right);

public:
  // This destructor is public so that it can be called directly.   
  // The destructor must be virtual for an abstract class.
  virtual ~Covariance();

  //------------------------------------------------------------
  // Mathematical functions.
  //------------------------------------------------------------

public:
  void cov       ( SPK_VA::valarray<double>& ret ) const;
  void cov_popPar( SPK_VA::valarray<double>& ret ) const;
  void cov_indPar( SPK_VA::valarray<double>& ret ) const;

  void inv       ( SPK_VA::valarray<double>& ret ) const;
  void inv_popPar( SPK_VA::valarray<double>& ret ) const;
  void inv_indPar( SPK_VA::valarray<double>& ret ) const;

private:
  virtual void doCov       ( SPK_VA::valarray<double>& ret ) const = 0;
  virtual bool doCov_popPar( SPK_VA::valarray<double>& ret ) const = 0;
  virtual bool doCov_indPar( SPK_VA::valarray<double>& ret ) const = 0;

  virtual void doInv       ( SPK_VA::valarray<double>& ret ) const = 0;
  virtual bool doInv_popPar( SPK_VA::valarray<double>& ret ) const = 0;
  virtual bool doInv_indPar( SPK_VA::valarray<double>& ret ) const = 0;

public:
  double logdet() const;
  double weightedSumOfSquares( const SPK_VA::valarray<double>& z ) const;

private:
  virtual double doLogdet() const = 0;
  virtual double doWeightedSumOfSquares( const SPK_VA::valarray<double>& z ) const = 0;

  //------------------------------------------------------------
  // Debugging tools
  //------------------------------------------------------------
private:
  mutable unsigned long timesCacheUpdated;

protected:
  void incrementUpdateCounter() const;

public:
  int getTimesCacheUpdated() const;

  //------------------------------------------------------------
  // Information specifying the parameter subsets that affect the covariance.
  //------------------------------------------------------------

private:

  //------------------------------------------------------------
  // State information.
  //------------------------------------------------------------

private:
  int index;                    // Current value of the individual index. 

  SPK_VA::valarray<double> covPopPar;   // Current value of the subset of the population 
                                // parameters that affect this covariance matrix.

  SPK_VA::valarray<double> covIndPar;   // Current value of the subset of the individual 
                                // parameters that affect this covariance matrix.

  //------------------------------------------------------------
  // Cache management members and functions.
  //------------------------------------------------------------

private:
  mutable bool isCacheValid;    // Status of any cached quantities in 
                                // the sub-classes of this class.

protected:
  void setCacheStatusValid()   const;
  void setCacheStatusInvalid() const;

  bool isCacheStatusValid()    const;
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  //------------------------------------------------------------
  // State changing functions.
  //------------------------------------------------------------

public:
  void selectCovIndividual( int indexNew );

  void setCovPopPar( const SPK_VA::valarray<double>& popParNew );
  void setCovIndPar( const SPK_VA::valarray<double>& indParNew );
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

};


#endif
