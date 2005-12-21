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
//========================================
// The global variables used by
// yyparse() and yylex() (or equivalent).
//----------------------------------------
extern int           gSpkExpErrors;
extern char*         gSpkExpErrorMessages;
extern int           gSpkExpLines;
extern SymbolTable * gSpkExpSymbolTable;
extern FILE *        gSpkExpOutput;
extern bool          gSpkIsTInRhs;
extern FILE *        nm_in;
extern int           NM_ACCEPT;
extern int           NM_ABORT;

extern "C"{
  int nm_parse(void);
  int nm_error( const char* );
};
//========================================

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
const char* NonmemTranslator::fPkEqn_fortran               ( "pkEqn.fortran" );
const char* NonmemTranslator::fPkEqn_cpp                   ( "pkEqn.cpp" );
const char* NonmemTranslator::fErrorEqn_fortran            ( "errorEqn.fortran" );
const char* NonmemTranslator::fErrorEqn_cpp                ( "errorEqn.cpp" );   
const char* NonmemTranslator::fNonmemPars_h                ( "NonmemPars.h" );
const char* NonmemTranslator::fMontePars_h                 ( "MontePars.h" );
const char* NonmemTranslator::fFitDriver_cpp               ( "fitDriver.cpp" );
const char* NonmemTranslator::fMonteDriver_cpp             ( "monteDriver.cpp" );
const char* NonmemTranslator::fSpkRuntimeLongError_tmp     ( "scratch.tmp" );
const char* NonmemTranslator::fResult_xml                  ( "result.xml" );
const char* NonmemTranslator::fCheckpoint_xml              ( "checkpoint.xml" );

NonmemTranslator::NonmemTranslator( DOMDocument* sourceIn, DOMDocument* dataIn )
  : ClientTranslator        ( sourceIn, dataIn ),
    myDescription           ( NULL ),
    myModelSpec             ( PRED ),
    myTrans                 ( TRANS1 ),
    myIsEstimate            ( true ),
    myIsSimulate            ( false ),
    myIsMonte               ( false ),
    myIsStat                ( false ),
    mySubproblemsN          ( 1 ),
    myIntegMethod           ( PLAIN ),
    myIntegNumberEvals      ( 1 ), // this is a vector
    myIntegNEvals           ( 1 ),
    myIsPosthoc             ( true ),
    myIsRestart             ( false ),
    myIndWriteCheckpoint    ( true ),
    myPopWriteCheckpoint    ( true ),
    myThetaLen              ( 0 ),
    myOmegaDim              ( 0 ),
    myOmegaOrder            ( 0 ),
    myOmegaStruct           ( Symbol::TRIANGLE ),
    mySigmaDim              ( 0 ),
    mySigmaOrder            ( 0 ),
    mySigmaStruct           ( Symbol::TRIANGLE ),
    myEtaLen                ( 0 ),
    myEpsLen                ( 0 ),
    mySigDigits             ( 3 ),
    myPopMitr               ( 100 ),
    myIndMitr               ( 100 ),
    myPopEpsilon            ( pow    ( 10.0, -(mySigDigits+1.0) ) ),
    myIndEpsilon            ( pow    ( 10.0, -(mySigDigits+1.0) ) ),
    myPopTraceLevel         ( 1 ),
    myIndTraceLevel         ( 1 ),
    mySeed                  ( 0 ),
    myCovForm               ( "RSR" ), // default for population level
    myIsStderr              ( true ),
    myIsCorrelation         ( true ),
    myIsCov                 ( true ),
    myIsInvCov              ( true ),
    myIsConfidence          ( true ),
    myIsCoefficient         ( true ),
    myRecordNums            ( 1 ),
    myCompModel             ( NULL ),
    myIsMissingCmt          ( true ),
    myIsMissingPcmt         ( true ),
    myIsMissingRate         ( true )
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
  remove( fPkEqn_fortran );
  remove( fPkEqn_cpp );
  remove( fErrorEqn_fortran );
  remove( fErrorEqn_cpp );
  remove( fNonmemPars_h );
  remove( fMontePars_h );
  remove( fFitDriver_cpp );
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
  /*
  //XMLString::release( &X_SPKDATA );
  XMLString::release( &X_VERSION );
  XMLString::release( &X_POINTONE );
  XMLString::release( &X_TABLE );
  XMLString::release( &X_COLUMNS );
  XMLString::release( &X_ROWS );
  XMLString::release( &X_DESCRIPTION );
  XMLString::release( &X_ROW );
  XMLString::release( &X_POSITION );
  XMLString::release( &X_VALUE );
  XMLString::release( &X_TYPE );
  XMLString::release( &X_NUMERIC );
  XMLString::release( &X_ID );
  XMLString::release( &X_MDV );
  XMLString::release( &X_EVID );
  XMLString::release( &X_AMT );
  XMLString::release( &X_DROP );
  XMLString::release( &X_SKIP );

  //  XMLString::release( &X_DESCRIPTION );
  XMLString::release( &X_YES );
  XMLString::release( &X_NO );
  XMLString::release( &X_FIXED );
  XMLString::release( &X_IN );
  XMLString::release( &X_LOW );
  XMLString::release( &X_UP );
  XMLString::release( &X_DIAGONAL );
  XMLString::release( &X_BLOCK );
  //  XMLString::release( &X_VALUE );
  XMLString::release( &X_STRUCT );
  XMLString::release( &X_DIMENSION );
  XMLString::release( &X_LABEL );
  XMLString::release( &X_COV_R );
  XMLString::release( &X_COV_RSR );
  XMLString::release( &X_COV_S );
  XMLString::release( &X_COV_H );
  XMLString::release( &X_COV_HSH );
  XMLString::release( &X_IS_ERR_OUT );
  XMLString::release( &X_IS_CORR_OUT );
  XMLString::release( &X_IS_COV_OUT );
  XMLString::release( &X_IS_INV_COV_OUT );
  XMLString::release( &X_IS_COEF_OUT );
  XMLString::release( &X_IS_CONF_OUT );
  
  XMLString::release( &X_NONMEM );
  XMLString::release( &X_POP_ANALYSIS );
  XMLString::release( &X_IND_ANALYSIS );
  XMLString::release( &X_CONSTRAINT );
  XMLString::release( &X_MODEL );
  XMLString::release( &X_ADVAN );
  XMLString::release( &X_TRANS );
  XMLString::release( &X_PRED );
  XMLString::release( &X_COMP_MODEL );
  XMLString::release( &X_COMPARTMENT );
  XMLString::release( &X_DIFFEQN );
  XMLString::release( &X_PK );
  XMLString::release( &X_ERROR );
  XMLString::release( &X_MONTE_CARLO );
  XMLString::release( &X_PRESENTATION );
  //  XMLString::release( &X_TABLE );
  XMLString::release( &X_SCATTERPLOT );
  XMLString::release( &X_COLUMN );
  XMLString::release( &X_X );
  XMLString::release( &X_Y );
  XMLString::release( &X_SPLIT );
  XMLString::release( &X_APPROXIMATION );
  XMLString::release( &X_FO );
  XMLString::release( &X_FOCE );
  XMLString::release( &X_LAPLACE );
  XMLString::release( &X_ANALYTIC );
  XMLString::release( &X_GRID );
  XMLString::release( &X_PLAIN );
  XMLString::release( &X_MISER );
  XMLString::release( &X_VEGAS );
  XMLString::release( &X_NUMBEREVAL );
  XMLString::release( &X_POP_SIZE );
  XMLString::release( &X_IS_ESTIMATION );
  XMLString::release( &X_IS_ETA_OUT );
  XMLString::release( &X_IS_RESTART );
  XMLString::release( &X_DATA_LABELS );
  XMLString::release( &X_FILENAME );
  XMLString::release( &X_NAME );
  XMLString::release( &X_SYNONYM );
  XMLString::release( &X_THETA );
  XMLString::release( &X_LENGTH );
  XMLString::release( &X_OMEGA );
  XMLString::release( &X_SIGMA );
  XMLString::release( &X_SIMULATION );
  XMLString::release( &X_SEED );
  XMLString::release( &X_POP_STAT );
  XMLString::release( &X_COVARIANCE_FORM );
  XMLString::release( &X_MITR );
  XMLString::release( &X_IND_STAT );
  XMLString::release( &X_SIG_DIGITS );
  XMLString::release( &X_SUBPROBLEMS );

  // SpkReportML attribute
  XMLString::release( &X_ELAPSEDTIME );
  XMLString::release( &X_NEMBER_EVAL );
  XMLString::release( &X_SUBPROBLEM );
 
  // SpkReportML tags
  XMLString::release( &X_ERROR_LIST );
  XMLString::release( &X_WARNING_LIST );
  XMLString::release( &X_POP_ANALYSIS_RESULT );
  XMLString::release( &X_IND_ANALYSIS_RESULT );
  XMLString::release( &X_OPT_TRACE_OUT );
  XMLString::release( &X_POP_MONTE_RESULT );
  XMLString::release( &X_MESSAGE );
  XMLString::release( &X_FILE_NAME );
  XMLString::release( &X_LINE_NUMBER );
  XMLString::release( &X_WARNING );
  XMLString::release( &X_POP_OPT_RESULT );
  XMLString::release( &X_IND_OPT_RESULT );
  XMLString::release( &X_POP_STAT_RESULT );
  XMLString::release( &X_IND_STAT_RESULT );
  XMLString::release( &X_POP_OBJ_OUT );
  XMLString::release( &X_IND_OBJ_OUT );
  XMLString::release( &X_THETA_IN );
  XMLString::release( &X_THETA_OUT );
  XMLString::release( &X_OMEGA_IN );
  XMLString::release( &X_OMEGA_OUT );
  XMLString::release( &X_SIGMA_IN );
  XMLString::release( &X_SIGMA_OUT );
  XMLString::release( &X_POP_STDERROR_OUT );
  XMLString::release( &X_POP_COVARIANCE_OUT );
  XMLString::release( &X_POP_INVERSE_COVARIANCE_OUT );
  XMLString::release( &X_POP_CORRELATION_OUT );
  XMLString::release( &X_POP_COEFFICIENT_OUT );
  XMLString::release( &X_POP_CONFIDENCE_OUT );
  XMLString::release( &X_IND_STDERROR_OUT );
  XMLString::release( &X_IND_COVARIANCE_OUT );
  XMLString::release( &X_IND_INVERSE_COVARIANCE_OUT );
  XMLString::release( &X_IND_CORRELATION_OUT );
  XMLString::release( &X_IND_COEFFICIENT_OUT );
  XMLString::release( &X_IND_CONFIDENCE_OUT );
  XMLString::release( &X_PRESENTATION_DATA );

  XMLString::release( &X_NCOMPARTMENTS );
  XMLString::release( &X_NPARAMETERS );
  XMLString::release( &X_NEQUILIBRIMS );
  XMLString::release( &X_INITIAL_OFF );
  XMLString::release( &X_NO_OFF );
  XMLString::release( &X_NO_DOSE );
  XMLString::release( &X_EQUILIBRIM );
  XMLString::release( &X_EXCLUDE );
  XMLString::release( &X_DEFAULT_OBSERVATION );
  XMLString::release( &X_DEFAULT_DOSE );
  XMLString::release( &X_TOLERANCE );
  */
}
NonmemTranslator::NonmemTranslator( const NonmemTranslator& )
{
}
NonmemTranslator& NonmemTranslator::operator=( const NonmemTranslator& )
{
}











