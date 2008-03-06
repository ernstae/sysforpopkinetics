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
 * A wrapper for optimizer control.
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

#include "QuasiNewtonAnyBoxObj.h"
#include "SpkValarray.h"
#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <fstream> 
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
  Optimizer(
    double             epsilonIn,
    int                nMaxIterIn,
    int                levelIn,
    const std::string  restartFileNameIn  = "",
    bool               readRestartInfoIn  = false,
    bool               writeRestartInfoIn = false );

  Optimizer();
  Optimizer( const Optimizer& original );
  Optimizer& operator=( const Optimizer& right );

  ~Optimizer();


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
  // Optimizer state related variables and functions.
  //------------------------------------------------------------

public:
  void getObj( double& objOut ) const;

  void getPar( SPK_VA::valarray<double>& parOut ) const;

  void getStateInfo(
    int                  nIn,
    size_t&              bOut,
    double&              rOut,
    double&              fOut,
    double* const        xOut,
    double* const        gOut,
    double* const        hOut,
    int                  mIn,
    const double* const  lowIn,
    const double* const  upIn,
    const int*    const  posIn,
    int&                 acceptStepCountOut );

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
    const int*    const  posIn,
    int                  acceptStepCountIn );

private:
  void initStateInfo( int nIn, int mIn );
  void freeStateInfo();

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

    // Acceptance criteria information.
    int      acceptStepCount;

  } stateInfo;


  //------------------------------------------------------------
  // Restart file related variables and functions.
  //------------------------------------------------------------

public:
  void setObjFunc( QuasiNewtonAnyBoxObj* pObjFuncIn );

  void readRestartInfoFromFile();

private:
  bool readRestartInfo;
  bool writeRestartInfo;

  std::string         restartFileName;
  std::fstream        tempRestartFileStream;

  QuasiNewtonAnyBoxObj* pObjFunc;

  void writeRestartInfoToFile();


  //------------------------------------------------------------
  // XML related variables and functions.
  //------------------------------------------------------------

private:
  xercesc::XercesDOMParser* pParser;
  xercesc::DOMElement*      pCurrElement;
  bool                      freeXMLMemory;
  int                       xmlLevel;
  const int                 xmlTab;

  template<class ValType> void getAttributeValue( 
    const XMLCh* const  pAttributeName,
    ValType&            valueOut );

  template<class ValType> void getValue(
    const XMLCh* const  pSubelementTag,
    ValType&            valueOut );

  template<class ValType> void getArray(
    const XMLCh* const  pSubelementTag,
    int                 nValue,
    ValType* const      arrayOut );

public:
  template<class ValType> void getValue(
    const char* const  pSubelementTag,
    ValType&           valueOut );

  template<class ValType> void getArray(
    const char* const  pSubelementTag,
    int                nValue,
    ValType* const     arrayOut );

private:
  void writeXMLVersion();

  template<class ValType> void writeAttribute( 
    const char* const  pAttributeName,
    ValType            valueIn );

public:
  template<class ValType> void writeValue(
    const char* const  pSubelementTag,
    ValType            valueIn );

  template<class ValType> void writeArray(
    const char* const    pSubelementTag,
    int                  nValue,
    const ValType* const arrayIn );

private:
  void writeStartTag( 
    const char* const pTag,
    bool              writeClosingBracket = true,
    bool              newLineAfter        = true );

  void writeStartTagFinalBracket( bool newLineAfter = true );

  void writeEndTag(
    const char* const pTag,
    bool              printSpacesBefore = true );


  //------------------------------------------------------------
  // Pointers to XML strings.
  //------------------------------------------------------------

private:
  XMLCh* pPop_restart_infoTag;
  XMLCh* pIsTooManyIterAttribute;
  XMLCh* pSaveStateAtEndOfOptAttribute;
  XMLCh* pThrowExcepIfMaxIterAttribute;
  XMLCh* pIsWarmStartPossibleAttribute;
  XMLCh* pIsWarmStartAttribute;
  XMLCh* pDidOptFinishOkAttribute;
  XMLCh* pIsBeginOfIterStateInfoAttribute;
  XMLCh* pEpsilonTag;
  XMLCh* pNIterCompletedTag;
  XMLCh* pStateInfo_nTag;
  XMLCh* pStateInfo_bTag;
  XMLCh* pStateInfo_rTag;
  XMLCh* pStateInfo_fTag;
  XMLCh* pStateInfo_xTag;
  XMLCh* pStateInfo_gTag;
  XMLCh* pStateInfo_hTag;
  XMLCh* pStateInfo_mTag;
  XMLCh* pStateInfo_lowTag;
  XMLCh* pStateInfo_upTag;
  XMLCh* pStateInfo_posTag;
  XMLCh* pStateInfo_acceptStepCountTag;
  XMLCh* pYesStr;
  XMLCh* pValueTag;
  XMLCh* pLengthAttribute;


  //------------------------------------------------------------
  // Error message related functions.
  //------------------------------------------------------------

public:
  bool isThereErrorInfo() const;

  void getErrorInfo( 
    const std::string  headerStr,
    std::string&       messageStr,
    unsigned int       lineNumber,
    const char* const  fileName ) const;


  //------------------------------------------------------------
  // Stream operators
  //------------------------------------------------------------

public:
  friend std::ostream& operator<<(std::ostream&, const Optimizer&);
  friend std::istream& operator>>(std::istream&, Optimizer&);

};
            
#endif
