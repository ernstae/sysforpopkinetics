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
 * File: Optimizer.h
 *
 *
 * A wrapper for optimizer substitution and control.
 *
 * Author: Jiaji Du
 *
 *************************************************************************/

/*************************************************************************
 *
 * Class: Optimizer
 *
 *************************************************************************/

/*------------------------------------------------------------------------
 * Class Specification
 *------------------------------------------------------------------------*/

#ifndef OPTIMIZER_H
#define OPTIMIZER_H

#include <iostream> 

//
// Structure: StateInfo
//
struct StateInfo
{
  // Note: the state information maintained by this class 
  // is specific to the optimizer QuasiNewton01Box.
  int      n;
  int      b;
  double   r;
  double   f;
  double*  x;   // Length n.
  double*  g;   // Length n.
  double*  h;   // Length n * n.
};

//
// Class: Optimizer
//
class Optimizer
{
public:
  // Default constructor
  Optimizer();

  // Copy constructor
  Optimizer( const Optimizer& right ); 
    
  // Constructor
  Optimizer( double Epsilon, int NMaxIter, int Level );

  // Destructor
  ~Optimizer();

    // Assignment operator
  Optimizer& operator=( const Optimizer& right ); 

  // Getters
  inline double getEpsilon() const { return epsilon; }
  inline int    getNMaxIter() const { return nMaxIter; }
  inline int    getLevel() const { return level; }
  inline int    getNIterCompleted() const { return nIterCompleted; }
  inline bool   getIsTooManyIter() const { return isTooManyIter; }
  inline bool   getSaveStateAtEndOfOpt() { return saveStateAtEndOfOpt; }
  inline bool   getThrowExcepIfMaxIter() { return throwExcepIfMaxIter; }
  inline bool   getIsWarmStart() const { return isWarmStart; }
  inline StateInfo  getStateInfo() const { return stateInfo; }

    // Setters
  inline void setEpsilon( double e ) { epsilon = e; }
  inline void setNMaxIter( int n ) { nMaxIter = n; }
  inline void setLevel( int l ) { level = l; }
  inline void setNIterCompleted( int n ) { nIterCompleted = n; }
  inline void setIsTooManyIter( bool b ) { isTooManyIter = b; }
  inline void setSaveStateAtEndOfOpt( bool s ) { saveStateAtEndOfOpt = s; }
  inline void setThrowExcepIfMaxIter( bool t ) { throwExcepIfMaxIter = t; }
  void setIsWarmStart( bool w );
  void setStateInfo( const StateInfo& s );
  void deleteStateInfo();

  // Allocate memory for returning state info for warm start
    void setupWarmStart( int n );

  // Stream operators
  friend std::ostream& operator<<(std::ostream&, const Optimizer&);
  friend std::istream& operator>>(std::istream&, Optimizer&);

private:

  double    epsilon;
  int       nMaxIter;
  int       level;
  int       nIterCompleted;
  bool      isTooManyIter;
  bool      saveStateAtEndOfOpt;
  bool      throwExcepIfMaxIter;
  bool      isWarmStart;

  StateInfo stateInfo;
};
            
#endif
