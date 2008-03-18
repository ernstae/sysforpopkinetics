/**
 * @file NonmemTranslator.cpp
 * Define NonmemTranslator constructors, destructor and others.
 *
 */
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
#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"
#include "explang.h"
#include "countStrInLhs.h"
#include "../series.h"

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

//==================================================================================
// Names of files that are generated by SPK Compiler or at runtime.
//==================================================================================
const char* NonmemTranslator::fMakefile                    ( "Makefile.SPK" );
const char* NonmemTranslator::fIndData_h                   ( "IndData.h" );
const char* NonmemTranslator::fDataSet_h                   ( "DataSet.h" );
const char* NonmemTranslator::fPredEqn_fortran             ( "predEqn.fortran" );
const char* NonmemTranslator::fPredEqn_cpp                 ( "predEqn.cpp" );
const char* NonmemTranslator::fPred_h                      ( "Pred.h" );
const char* NonmemTranslator::fDiffEqn_fortran             ( "diffEqn.fortran" );
const char* NonmemTranslator::fDiffEqn_cpp                 ( "diffEqn.cpp" );
const char* NonmemTranslator::fOdePred_h                   ( "OdePred.h" );
const char* NonmemTranslator::fIdentPred_h                 ( "IdentPred.h" );
const char* NonmemTranslator::fPkEqn_fortran               ( "pkEqn.fortran" );
const char* NonmemTranslator::fPkEqn_cpp                   ( "pkEqn.cpp" );
const char* NonmemTranslator::fErrorEqn_fortran            ( "errorEqn.fortran" );
const char* NonmemTranslator::fErrorEqn_cpp                ( "errorEqn.cpp" );   
const char* NonmemTranslator::fNonmemPars_h                ( "NonmemPars.h" );
const char* NonmemTranslator::fMontePars_h                 ( "MontePars.h" );
const char* NonmemTranslator::fFitDriver_cpp               ( "fitDriver.cpp" );
const char* NonmemTranslator::fIndDriver_cpp               ( "indDriver.cpp" );
const char* NonmemTranslator::fMonteDriver_cpp             ( "monteDriver.cpp" );
const char* NonmemTranslator::fSpkRuntimeLongError_tmp     ( "scratch.tmp" );
const char* NonmemTranslator::fResult_xml                  ( "result.xml" );
const char* NonmemTranslator::fCheckpoint_xml              ( "checkpoint.xml" );

NonmemTranslator::NonmemTranslator( DOMDocument* sourceIn, DOMDocument* dataIn )
  : ClientTranslator                    ( sourceIn, dataIn ),
    myDescription                       ( NULL ),
    myModelSpec                         ( PRED ),
    myTrans                             ( TRANS1 ),
    myIsEstimate                        ( true ),
    myIsSimulate                        ( false ),
    myIsIdent                           ( false ),
    myIsNonparam                        ( false ),
    myNonparamGridMeasurePointPerSideIn ( 0 ),
    myNonparamRandomMeasurePointIn      ( 0 ),
    myIsMonte                           ( false ),
    myIsStat                            ( false ),
    mySubproblemsN                      ( 1 ),
    myIntegMethod                       ( PLAIN ),
    myIntegNumberEvals                  ( 1 ), // this is a vector
    myIntegNEvals                       ( 1 ),
    myIsPosthoc                         ( true ),
    myIsRestart                         ( false ),
    myIndWriteCheckpoint                ( true ),
    myPopWriteCheckpoint                ( true ),
    myThetaLen                          ( 0 ),
    myOmegaDim                          ( 0 ),                 //valarray
    myOmegaOrder                        ( 0 ),                 //valarray
    myOmegaStruct                       ( Symbol::TRIANGLE ),  //valarray
    myOmegaSameAsPrev                   ( false ),             //valarray
    mySigmaDim                          ( 0 ),                 //valarray
    mySigmaOrder                        ( 0 ),                 //valarray
    mySigmaStruct                       ( Symbol::TRIANGLE ),  //valarray
    mySigmaSameAsPrev                   ( false ),             //valarray
    myEtaLen                            ( 0 ),
    myEpsLen                            ( 0 ),
    mySigDigits                         ( 3 ),
    myPopMitr                           ( 100 ),
    myIndMitr                           ( 100 ),
    myPopEpsilon                        ( pow    ( 10.0, -(mySigDigits+1.0) ) ),
    myIndEpsilon                        ( pow    ( 10.0, -(mySigDigits+1.0) ) ),
    myPopTraceLevel                     ( 1 ),
    myIndTraceLevel                     ( 1 ),
    mySeed                              ( 0 ),
    myCovForm                           ( "RSR" ), // default for population level
    myIsStderr                          ( true ),
    myIsCorrelation                     ( true ),
    myIsCov                             ( true ),
    myIsInvCov                          ( true ),
    myIsConfidence                      ( true ),
    myIsCoefficient                     ( true ),
    myRecordNums                        ( 1 ),
    myCompModel                         ( NULL ),
    myIsMissingCmt                      ( true ),
    myIsMissingPcmt                     ( true ),
    myIsMissingRate                     ( true )
{
  table = ClientTranslator::getSymbolTable();

  myPopEpsilon = pow( 10.0, -(mySigDigits+1.0) );
  myIndEpsilon = pow( 10.0, -(mySigDigits+1.0) );

  // Clean up reminents from a previous run.
  remove( fMakefile );
  remove( fIndData_h );
  remove( fDataSet_h );
  remove( fPredEqn_fortran );
  remove( fPredEqn_cpp );
  remove( fPred_h );
  remove( fDiffEqn_fortran );
  remove( fDiffEqn_cpp );
  remove( fOdePred_h );
  remove( fIdentPred_h );
  remove( fPkEqn_fortran );
  remove( fPkEqn_cpp );
  remove( fErrorEqn_fortran );
  remove( fErrorEqn_cpp );
  remove( fNonmemPars_h );
  remove( fMontePars_h );
  remove( fFitDriver_cpp );
  remove( fIndDriver_cpp );
  remove( fMonteDriver_cpp );
  remove( fSpkRuntimeLongError_tmp );
  remove( fResult_xml );
}
NonmemTranslator::NonmemTranslator()
{
}
NonmemTranslator::~NonmemTranslator()
{
  if( myDescription )
    delete [] myDescription;
  if( myCompModel )
    delete myCompModel;
}
NonmemTranslator::NonmemTranslator( const NonmemTranslator& )
{
}
NonmemTranslator& NonmemTranslator::operator=( const NonmemTranslator& )
{
}










