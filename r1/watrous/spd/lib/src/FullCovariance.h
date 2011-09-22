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
 * File: FullCovariance.h
 *
 *
 * This is an abstract subclass of Covariance from which subclasses for 
 * full covariance matrices may be derived.
 *
 * Objects of this class are used to calculate and cache various 
 * mathematical quantities involving full covariance matrices.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Top Page of Omhelp Specification
 *------------------------------------------------------------------------*/

// [Revisit - Omhelp Specifications in Header Files - Mitch] In order that
// the member functions for this class that are defined in FullCovariance.cpp
// can be displayed as subsections of a "FullCovariance Class" section, this 
// part of the Omhelp specification has been moved to this header file.

/*
$begin FullCovariance$$
$spell
  calc
  cholesky
  covariance
  logdet
  instantiated
  implementer
  factorization
$$

$section FullCovariance Class$$

$cindex FullCovariance Class$$

$center
$italic
$include shortCopyright.txt$$
$$
$$
$pre
$$
$head Description$$

This is a subclass of $xref/Covariance//Covariance/$$ 
that serves interfaces for mathematical operations.
It provides default implementations for some of the interfaces that
are designed to handle non-sparse matrices.
$pre

$$
The intended use of this class is for the client to define a yet-another
level of abstract class derived from this class and one of the Covariance classes
on the right hand side of the diagram shown below.
$escape #$$
$math%

              --------------------------
             |         Covariance       |    
              --------------------------
                       (Abstract)
              /|\                   /|\        
               |                     | 
      math     |                     |       states
   -----------------------    -------------------------
  |     FullCovariance    |  |  Data/IndParCovariance  |
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
%$$
$escape \$$

Objects of this class are used to calculate and cache the following 
quantities for a covariance matrix:
$table
(1.) the covariance, $rend
(2.) the inverse of the covariance, $rend
(3.) the log determinant, and  $rend
(4.) the weighted sum of squares of an arbitrary vector. $rend
$tend
In addition, objects of this class are used to evaluate the 
derivatives of these quantities with respect to the population
and the individual parameters.

$head Note$$

(1.) This class defines virtual functions that calculate the
above mathematical quantities and their derivatives and that were
declared in the $code Covariance$$ base class.  
These functions may be redefined in subclasses of this class 
in order to take advantage of more efficient algorithms or methods 
known to the implementer of the subclass.
If these functions are redefined, their new implementations 
must update the cached values before they do any computations. 
See $mref/FullCovariance_RedefiningMathFunctions/$$ for more.
$pre

$$
(2.) This class declares pure virtual functions that get the 
current value of the covariance matrix and its derivatives.  
These functions must be defined in concrete subclasses of this class 
before any $code FullCovariance$$ objects may be instantiated.
$pre

$$
(3.) This class defines a virtual function 
$xref/FullCovariance_calcCholesky//calcCholesky/$$ that calculates 
the Cholesky factorization of the full covariance matrix.
This function may be redefined in subclasses of this class 
in order to take advantage of more efficient implementations 
known to the implementer of the subclass.

$contents/
  FullCovariance.cpp/$$


$end
*/

#ifndef FULLCOVARIANCE_H
#define FULLCOVARIANCE_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/
//
// These pragma are to disable "too long literals" warnings generated
// as a result of using STL component, std::vector.
//
#pragma warning( disable : 4786 )  
#include <vector>

// SPK include files.
#include "Covariance.h"
#include "SpkValarray.h"

class FullCovariance : public Covariance
{
  //------------------------------------------------------------
  // Constructors, destructors, and assignment operators.
  //------------------------------------------------------------

protected:
  // These are protected (rather than public) because this is an 
  // abstract class and they should not be called directly.
  // These are protected (rather than private) because subclasses
  // of this class may need to call these functions.
  FullCovariance();
  FullCovariance( const FullCovariance &right );
  FullCovariance& operator=(const FullCovariance &right);

public:
  // This destructor is public so that it can be called directly.   
  // The destructor must be virtual for an abstract class.
  virtual ~FullCovariance();

  //------------------------------------------------------------
  // Mathematical functions.
  //------------------------------------------------------------
private:
  // provided with default implementations
  virtual double doLogdet() const;
  virtual double doWeightedSumOfSquares( const SPK_VA::valarray<double>& z ) const;
  virtual void   calcCholesky( SPK_VA::valarray<double>& chol ) const;
  
  // no default
  virtual void doCov       ( SPK_VA::valarray<double>& ret ) const = 0;
  virtual bool doCov_popPar( SPK_VA::valarray<double>& ret ) const = 0;
  virtual bool doCov_indPar( SPK_VA::valarray<double>& ret ) const = 0;

  // no default
  virtual void doInv       ( SPK_VA::valarray<double>& ret ) const = 0;
  virtual bool doInv_popPar( SPK_VA::valarray<double>& ret ) const = 0;
  virtual bool doInv_indPar( SPK_VA::valarray<double>& ret ) const = 0;

private:
  // concrete
  double calcLogdetFromCholesky( const SPK_VA::valarray<double>& chol ) const;

  //--------------------------------------------------------------
  // Cached information.
  //--------------------------------------------------------------

private:
  mutable SPK_VA::valarray<double>  choleskyCached;   // The lower triangular Cholesky factor.
  mutable double            logdetCached;         // The logdet of the matrix.


  //--------------------------------------------------------------
  // Cache related functions.
  //--------------------------------------------------------------

private:
  void updateCache() const;

};



#endif
