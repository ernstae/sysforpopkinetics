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
 * Modified later by: Sachiko Honda
 * Modified later by: Mitch Watrous
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
#include <string> 

//
// Class: Optimizer
//
class Optimizer
{
  //------------------------------------------------------------
  // Constructors, destructors, and assignment operators.
  //------------------------------------------------------------

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


  //------------------------------------------------------------
  // Getters.
  //------------------------------------------------------------

public:
  inline double getEpsilon() const { return epsilon; }
  inline int    getNMaxIter() const { return nMaxIter; }
  inline int    getLevel() const { return level; }
  inline int    getNIterCompleted() const { return nIterCompleted; }
  inline bool   getIsTooManyIter() const { return isTooManyIter; }
  inline bool   getSaveStateAtEndOfOpt() { return saveStateAtEndOfOpt; }
  inline bool   getThrowExcepIfMaxIter() { return throwExcepIfMaxIter; }
  inline bool   getIsWarmStartPossible() const { return isWarmStartPossible; }
  inline bool   getIsWarmStart() const { return isWarmStart; }
  inline bool   getDidOptFinishOk() const { return didOptFinishOk; }
  inline bool   getIsBeginOfIterStateInfo() const { return isBeginOfIterStateInfo; }


  //------------------------------------------------------------
  // Setters.
  //------------------------------------------------------------

public:
  inline void setEpsilon( double e ) { epsilon = e; }
  inline void setNMaxIter( int n ) { nMaxIter = n; }
  inline void setLevel( int l ) { level = l; }
  inline void setNIterCompleted( int n ) { nIterCompleted = n; }
  inline void setIsTooManyIter( bool b ) { isTooManyIter = b; }
  inline void setSaveStateAtEndOfOpt( bool b ) { saveStateAtEndOfOpt = b; }
  inline void setThrowExcepIfMaxIter( bool b ) { throwExcepIfMaxIter = b; }
  inline void setIsWarmStartPossible( bool b ) { isWarmStartPossible = b; }
  inline void setIsWarmStart( bool b ) { isWarmStart = b; }
  inline void setDidOptFinishOk( bool b) { didOptFinishOk = b; }
  inline void setIsBeginOfIterStateInfo( bool b) { isBeginOfIterStateInfo = b; }


  //------------------------------------------------------------
  // Optimizer control variables and flags.
  //------------------------------------------------------------

private:
  double    epsilon;
  int       nMaxIter;
  int       level;
  int       nIterCompleted;

  bool      isTooManyIter;
  bool      saveStateAtEndOfOpt;
  bool      throwExcepIfMaxIter;
  bool      isWarmStartPossible;
  bool      isWarmStart;
  bool      didOptFinishOk;
  bool      isBeginOfIterStateInfo;


  //------------------------------------------------------------
  // Optimizer state related functions.
  //------------------------------------------------------------

public:
  // Get state information for warm start
  void Optimizer::getStateInfo(
    int            nIn,
    size_t&        bOut,
    double&        rOut,
    double&        fOut,
    double* const  xOut,
    double* const  gOut,
    double* const  hOut );

  // Set state information for warm start
  void setStateInfo(
    int                  nIn,
    size_t               bIn,
    double               rIn,
    double               fIn,
    const double* const  xIn,
    const double* const  gIn,
    const double* const  hIn,
    int                  mIn,
    const double* const  lowIn,
    const double* const  upIn,
    const int*    const  posIn );

private:
  // The state information used by the optimizer QuasiNewton01Box.
  struct StateInfo
  {
    // State information in the [0,1] coordinates.
    int      n;
    size_t   b;
    double   r;
    double   f;
    double*  x;   // Length n.
    double*  g;   // Length n.
    double*  h;   // Length n * n.

    // Original coordinate information for converting values back.
    int      m;
    double*  low;     // Length m.
    double*  up;      // Length m.
    int*     pos;     // Length n.

  } stateInfo;


  //------------------------------------------------------------
  // Error message related functions.
  //------------------------------------------------------------

public:
  bool isThereErrorInfo() const;

  void getErrorInfo( 
    const std::string  headerStr,
    std::string&       messageStr,
    unsigned int       lineNumber,
    const char*        fileName );


  //------------------------------------------------------------
  // Stream operators
  //------------------------------------------------------------

public:
  friend std::ostream& operator<<(std::ostream&, const Optimizer&);
  friend std::istream& operator>>(std::istream&, Optimizer&);

};
            
#endif
