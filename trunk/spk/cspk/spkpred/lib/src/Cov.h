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
 * File: Cov.h
 *
 *
 * This is the abstract base class for covariance matrix classes.
 *
 * Covariance matrices must be positive definite and symmetric.
 *
 * Author: Mitch Watrous
 *
 *************************************************************************/

#ifndef COV_H
#define COV_H

/*------------------------------------------------------------------------
 * Include files
 *------------------------------------------------------------------------*/

// SPK library header files.
#include <spk/SpkValarray.h>


/*------------------------------------------------------------------------
 * Class declaration
 *------------------------------------------------------------------------*/

class Cov
{
  //------------------------------------------------------------
  // Constructors and destructors.
  //------------------------------------------------------------

public:
  Cov( int nRowIn, int nParIn );

  // The destructor must be virtual for an abstract class.
  virtual ~Cov() {}


  //------------------------------------------------------------
  // Constant members.
  //------------------------------------------------------------

public:
  const int nRow;
  const int nPar;


  //------------------------------------------------------------
  // State information.
  //------------------------------------------------------------

protected:
  SPK_VA::valarray<double> parCurr;


  //------------------------------------------------------------
  // State changing functions.
  //------------------------------------------------------------

public:
  void setPar( const SPK_VA::valarray<double>& parIn );
  void setCov( const SPK_VA::valarray<double>& covIn );


  //------------------------------------------------------------
  // Cache management members and functions.
  //------------------------------------------------------------

private:
  void invalidateCache() const;

protected:
  mutable SPK_VA::valarray<double> covCurr;
  mutable SPK_VA::valarray<double> cov_parCurr;
  mutable SPK_VA::valarray<double> invCurr;
  mutable SPK_VA::valarray<double> inv_parCurr;

  mutable bool isCovCurrOk;
  mutable bool isCov_parCurrOk;
  mutable bool isInvCurrOk;
  mutable bool isInv_parCurrOk;

  mutable bool usedCachedCov;
  mutable bool usedCachedCov_par;
  mutable bool usedCachedInv;
  mutable bool usedCachedInv_par;

public:
  bool getUsedCachedCov()     const;
  bool getUsedCachedCov_par() const;
  bool getUsedCachedInv()     const;
  bool getUsedCachedInv_par() const;


  //------------------------------------------------------------
  // Mathematical functions.
  //------------------------------------------------------------

public:
  virtual void cov    ( SPK_VA::valarray<double>& covOut     ) const = 0;
  virtual void cov_par( SPK_VA::valarray<double>& cov_parOut ) const = 0;

  virtual void inv    ( SPK_VA::valarray<double>& invOut     ) const = 0;
  virtual void inv_par( SPK_VA::valarray<double>& inv_parOut ) const = 0;

  virtual void getParLimits(
    SPK_VA::valarray<double>&  parLow,
    SPK_VA::valarray<double>&  parUp ) const = 0;

  virtual void calcPar( 
    const SPK_VA::valarray<double>& covIn,
    SPK_VA::valarray<double>&       parOut ) const = 0;

  virtual void calcCovMinRep( 
    const SPK_VA::valarray<double>& covIn,
    SPK_VA::valarray<double>&       covMinRepOut ) const = 0;

  virtual void expandCovMinRep( 
    const SPK_VA::valarray<double>& covMinRepIn,
    SPK_VA::valarray<double>&       covOut ) const = 0;


  //------------------------------------------------------------
  // Miscellaneous member functions.
  //------------------------------------------------------------

public:
  int getNRow() const;
  int getNPar() const;


  //------------------------------------------------------------
  // Disallowed, implicitly generated member functions.
  //------------------------------------------------------------

private:
  // These functions should not be called, so they are not
  // implemented in Cov.cpp.
  Cov( const Cov& right );
  Cov& operator=( const Cov& right );
};


#endif

