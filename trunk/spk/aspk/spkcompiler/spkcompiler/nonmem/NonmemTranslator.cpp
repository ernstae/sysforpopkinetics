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
#include "../upper.h"
#include "../lower.h"
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
// XML tag names and attribute names in C string
//==================================================================================
const char* NonmemTranslator::C_SPKDATA    ( "spkdata" );
const char* NonmemTranslator::C_VERSION    ( "version" );
const char* NonmemTranslator::C_POINTONE   ( "0.1" );
const char* NonmemTranslator::C_TABLE      ( "table" );
const char* NonmemTranslator::C_COLUMNS    ( "columns" );
const char* NonmemTranslator::C_ROWS       ( "rows" );
const char* NonmemTranslator::C_DESCRIPTION( "description" );
const char* NonmemTranslator::C_ROW        ( "row" );
const char* NonmemTranslator::C_POSITION   ( "position" );
const char* NonmemTranslator::C_VALUE      ( "value" );
const char* NonmemTranslator::C_TYPE       ( "type" );
const char* NonmemTranslator::C_NUMERIC    ( "numeric" );
const char* NonmemTranslator::C_ID         ( "ID" );
const char* NonmemTranslator::C_MDV        ( "MDV" );
const char* NonmemTranslator::C_EVID       ( "EVID" );
const char* NonmemTranslator::C_AMT        ( "AMT" );

const char* NonmemTranslator::C_YES                        ( "yes" );
const char* NonmemTranslator::C_NO                         ( "no" );
const char* NonmemTranslator::C_FIXED                      ( "fixed" );
const char* NonmemTranslator::C_IN                         ( "in" );
const char* NonmemTranslator::C_LOW                        ( "low" );
const char* NonmemTranslator::C_UP                         ( "up" );
const char* NonmemTranslator::C_DIAGONAL                   ( "diagonal" );
const char* NonmemTranslator::C_BLOCK                      ( "block" );
const char* NonmemTranslator::C_STRUCT                     ( "struct" );
const char* NonmemTranslator::C_DIMENSION                  ( "dimension" );
const char* NonmemTranslator::C_LABEL                      ( "label" );
const char* NonmemTranslator::C_LABELS                     ( "labels" );
const char* NonmemTranslator::C_COV_R                      ( "r" );
const char* NonmemTranslator::C_COV_RSR                    ( "rsr" );
const char* NonmemTranslator::C_COV_S                      ( "s" );
const char* NonmemTranslator::C_COV_H                      ( "h" );
const char* NonmemTranslator::C_COV_HSH                    ( "hsh" );
const char* NonmemTranslator::C_NONMEM                     ( "nonmem" );
const char* NonmemTranslator::C_POP_ANALYSIS               ( "pop_analysis" );
const char* NonmemTranslator::C_IND_ANALYSIS               ( "ind_analysis" );
const char* NonmemTranslator::C_CONSTRAINT                 ( "constraint" );
const char* NonmemTranslator::C_MODEL                      ( "model" );
const char* NonmemTranslator::C_ADVAN                      ( "advan" );
const char* NonmemTranslator::C_TRANS                      ( "trans" );
const char* NonmemTranslator::C_PRED                       ( "pred" );
const char* NonmemTranslator::C_COMP_MODEL                 ( "comp_model" );
const char* NonmemTranslator::C_COMPARTMENT                ( "compartment" );
const char* NonmemTranslator::C_DIFFEQN                    ( "diffeqn" );
const char* NonmemTranslator::C_PK                         ( "pk" );
const char* NonmemTranslator::C_ERROR                      ( "error" );
const char* NonmemTranslator::C_PRESENTATION               ( "presentation" );
//const char* NonmemTranslator::C_TABLE                      ( "table" );
const char* NonmemTranslator::C_SCATTERPLOT                ( "scatterplot" );
const char* NonmemTranslator::C_COLUMN                     ( "column" );
const char* NonmemTranslator::C_X                          ( "x" );
const char* NonmemTranslator::C_Y                          ( "y" );
const char* NonmemTranslator::C_SPLIT                      ( "split" );
const char* NonmemTranslator::C_APPROXIMATION              ( "approximation" );
const char* NonmemTranslator::C_FO                         ( "fo" );
const char* NonmemTranslator::C_FOCE                       ( "foce" );
const char* NonmemTranslator::C_LAPLACE                    ( "laplace" );
const char* NonmemTranslator::C_MONTE_CARLO                ( "monte_carlo" );
const char* NonmemTranslator::C_METHOD                     ( "method" );
const char* NonmemTranslator::C_ANALYTIC                   ( "analytic" );
const char* NonmemTranslator::C_GRID                       ( "grid" );
const char* NonmemTranslator::C_MISER                      ( "miser" );
const char* NonmemTranslator::C_PLAIN                      ( "plain" );
const char* NonmemTranslator::C_VEGAS                      ( "vegas" );
const char* NonmemTranslator::C_NUMBEREVAL                 ( "number_eval" );
const char* NonmemTranslator::C_POP_SIZE                   ( "pop_size" );
const char* NonmemTranslator::C_IS_ESTIMATION              ( "is_estimation" );
const char* NonmemTranslator::C_IS_ETA_OUT                 ( "is_eta_out" );
const char* NonmemTranslator::C_IS_RESTART                 ( "is_restart" );
const char* NonmemTranslator::C_DATA_LABELS                ( "data_labels" );
const char* NonmemTranslator::C_FILENAME                   ( "filename" );
const char* NonmemTranslator::C_NAME                       ( "name" );
const char* NonmemTranslator::C_SYNONYM                    ( "synonym" );
const char* NonmemTranslator::C_THETA                      ( "theta" );
const char* NonmemTranslator::C_LENGTH                     ( "length" );
const char* NonmemTranslator::C_OMEGA                      ( "omega" );
const char* NonmemTranslator::C_SIGMA                      ( "sigma" );
const char* NonmemTranslator::C_SIMULATION                 ( "simulation" );
const char* NonmemTranslator::C_SEED                       ( "seed" );
const char* NonmemTranslator::C_SUBPROBLEMS                ( "subproblems" );
const char* NonmemTranslator::C_POP_STAT                   ( "pop_stat" );
const char* NonmemTranslator::C_COVARIANCE_FORM            ( "covariance_form" );
const char* NonmemTranslator::C_MITR                       ( "mitr" );
const char* NonmemTranslator::C_IND_STAT                   ( "ind_stat" );
const char* NonmemTranslator::C_SIG_DIGITS                 ( "sig_digits" );
const char* NonmemTranslator::C_IS_STDERROR_OUT            ( "is_stderror_out" );
const char* NonmemTranslator::C_IS_CORRELATION_OUT         ( "is_correlation_out" );
const char* NonmemTranslator::C_IS_COVARIANCE_OUT          ( "is_covariance_out" );
const char* NonmemTranslator::C_IS_INVERSE_COVARIANCE_OUT  ( "is_inverse_covariance_out" );
const char* NonmemTranslator::C_IS_COEFFICIENT_OUT         ( "is_coefficient_out" );
const char* NonmemTranslator::C_IS_CONFIDENCE_OUT          ( "is_confidence_out" );
const char* NonmemTranslator::C_SPKREPORT                  ( "spkreport" );
const char* NonmemTranslator::C_ELAPSEDTIME                ( "elapsedtime" );
const char* NonmemTranslator::C_NUMBER_EVAL                ( "number_eval" );
const char* NonmemTranslator::C_SUBPROBLEM                 ( "subproblem" );
const char* NonmemTranslator::C_ERROR_LIST                 ( "error_list" );
const char* NonmemTranslator::C_WARNING_LIST               ( "warning_list" );
const char* NonmemTranslator::C_POP_ANALYSIS_RESULT        ( "pop_analysis_result" );
const char* NonmemTranslator::C_IND_ANALYSIS_RESULT        ( "ind_analysis_result" );
const char* NonmemTranslator::C_OPT_TRACE_OUT              ( "opt_trace_out" );
const char* NonmemTranslator::C_POP_MONTE_RESULT           ( "pop_monte_result" );
const char* NonmemTranslator::C_MESSAGE                    ( "message" );
const char* NonmemTranslator::C_FILE_NAME                  ( "file_name" );
const char* NonmemTranslator::C_LINE_NUMBER                ( "line_number" );
const char* NonmemTranslator::C_WARNING                    ( "warning" );
const char* NonmemTranslator::C_POP_OPT_RESULT             ( "pop_opt_result" );
const char* NonmemTranslator::C_POP_STAT_RESULT            ( "pop_stat_result" );
const char* NonmemTranslator::C_IND_OPT_RESULT             ( "ind_opt_result" );
const char* NonmemTranslator::C_IND_STAT_RESULT            ( "ind_stat_result" );
const char* NonmemTranslator::C_POP_OBJ_OUT                ( "pop_obj_out" );
const char* NonmemTranslator::C_IND_OBJ_OUT                ( "ind_obj_out" );
const char* NonmemTranslator::C_THETA_IN                   ( "theta_in" );
const char* NonmemTranslator::C_THETA_OUT                  ( "theta_out" );
const char* NonmemTranslator::C_OMEGA_IN                   ( "omega_in" );
const char* NonmemTranslator::C_OMEGA_OUT                  ( "omega_out" );
const char* NonmemTranslator::C_SIGMA_IN                   ( "sigma_in" );
const char* NonmemTranslator::C_SIGMA_OUT                  ( "sigma_out" );
const char* NonmemTranslator::C_POP_STDERROR_OUT           ( "pop_stderror_out" );
const char* NonmemTranslator::C_POP_COVARIANCE_OUT         ( "pop_convariance_out" );
const char* NonmemTranslator::C_POP_INVERSE_COVARIANCE_OUT ( "pop_inverse_covariance_out" );
const char* NonmemTranslator::C_POP_CORRELATION_OUT        ( "pop_correlation_out" );
const char* NonmemTranslator::C_POP_COEFFICIENT_OUT        ( "pop_coefficient_out" );
const char* NonmemTranslator::C_POP_CONFIDENCE_OUT         ( "pop_confidence_out" );
const char* NonmemTranslator::C_IND_STDERROR_OUT           ( "ind_stderror_out" );
const char* NonmemTranslator::C_IND_COVARIANCE_OUT         ( "ind_covariance_out" );
const char* NonmemTranslator::C_IND_INVERSE_COVARIANCE_OUT ( "ind_inverse_covariance_out" );
const char* NonmemTranslator::C_IND_CORRELATION_OUT        ( "ind_correlation_out" );
const char* NonmemTranslator::C_IND_COEFFICIENT_OUT        ( "ind_coefficient_out" );
const char* NonmemTranslator::C_IND_CONFIDENCE_OUT         ( "ind_confidence_out" );
const char* NonmemTranslator::C_PRESENTATION_DATA          ( "presentation_data" );
const char* NonmemTranslator::C_NCOMPARTMENTS              ( "ncompartments" );
const char* NonmemTranslator::C_NPARAMETERS                ( "nparameters" );
const char* NonmemTranslator::C_NEQUILIBRIMS               ( "nequilibrims" );
const char* NonmemTranslator::C_INITIAL_OFF                ( "initial_off" );
const char* NonmemTranslator::C_NO_OFF                     ( "no_off" );
const char* NonmemTranslator::C_NO_DOSE                    ( "no_dose" );
const char* NonmemTranslator::C_EQUILIBRIM                 ( "equilibrim" );
const char* NonmemTranslator::C_EXCLUDE                    ( "exclude" );
const char* NonmemTranslator::C_DEFAULT_OBSERVATION        ( "default_observation" );
const char* NonmemTranslator::C_DEFAULT_DOSE               ( "default_dose" );
const char* NonmemTranslator::C_TOLERANCE                  ( "tolerance" );

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

  DefaultStr.THETA             = "THETA";
  DefaultStr.ETA               = "ETA";
  DefaultStr.EPS               = "EPS";
  DefaultStr.OMEGA             = "OMEGA";
  DefaultStr.SIGMA             = "SIGMA";
  DefaultStr.PRED              = "PRED";
  DefaultStr.RES               = "RES";
  DefaultStr.WRES              = "WRES";
  DefaultStr.ETARES            = "ETARES";
  DefaultStr.WETARES           = "WETARES";
  DefaultStr.IPRED             = "IPRED";
  DefaultStr.IRES              = "IRES";
  DefaultStr.IWRES             = "IWRES";
  DefaultStr.IETARES           = "IETARES";
  DefaultStr.IWETARES          = "IWETARES";
  DefaultStr.PPRED             = "PPRED";
  DefaultStr.PRES              = "PRES";
  DefaultStr.PWRES             = "PWRES";
  DefaultStr.PETARES           = "PETARES";
  DefaultStr.PWETARES          = "PWETARES";
  DefaultStr.CPRED             = "CPRED";
  DefaultStr.CRES              = "CRES";
  DefaultStr.CWRES             = "CWRES";
  DefaultStr.CETARES           = "CETARES";
  DefaultStr.CWETARES          = "CWETARES";
  DefaultStr.DV                = "DV";
  DefaultStr.ORGDV             = "ORGDV";
  DefaultStr.MDV               = "MDV";
  DefaultStr.ID                = "ID";
  DefaultStr.F                 = "F";
  DefaultStr.Y                 = "Y";
  DefaultStr.T                 = "T";
  DefaultStr.P                 = "P";
  DefaultStr.A                 = "A";
  DefaultStr.EVID              = "EVID";
  DefaultStr.DADT              = "DADT";
  DefaultStr.AMT               = "AMT";
  DefaultStr.CMT               = "CMT";
  DefaultStr.PCMT              = "PCMT";
  DefaultStr.R                 = "R";
  DefaultStr.D                 = "D";
  DefaultStr.ALAG              = "ALAG";
  DefaultStr.FO                = "FO"; // ef-oh
  DefaultStr.F0                = "F0"; // ef-zero
  DefaultStr.S0                = "S0"; // es-zero
  DefaultStr.RATE              = "RATE";
  DefaultStr.TIME              = "TIME";
  DefaultStr.TSCALE            = "TSCALE";
  DefaultStr.S                 = "S";

  UserStr.THETA                = DefaultStr.THETA;
  UserStr.ETA                  = DefaultStr.ETA;
  UserStr.EPS                  = DefaultStr.EPS;
  UserStr.OMEGA                = DefaultStr.OMEGA;
  UserStr.SIGMA                = DefaultStr.SIGMA;
  UserStr.PRED                 = DefaultStr.PRED;
  UserStr.RES                  = DefaultStr.RES;
  UserStr.WRES                 = DefaultStr.WRES;
  UserStr.ETARES               = DefaultStr.ETARES;
  UserStr.WETARES              = DefaultStr.WETARES;
  UserStr.IPRED                = DefaultStr.IPRED;
  UserStr.IRES                 = DefaultStr.IRES;
  UserStr.IWRES                = DefaultStr.IWRES;
  UserStr.IETARES              = DefaultStr.IETARES;
  UserStr.IWETARES             = DefaultStr.IWETARES;
  UserStr.PPRED                = DefaultStr.PPRED;
  UserStr.PRES                 = DefaultStr.PRES;
  UserStr.PWRES                = DefaultStr.PWRES;
  UserStr.PETARES              = DefaultStr.PETARES;
  UserStr.PWETARES             = DefaultStr.PWETARES;
  UserStr.CPRED                = DefaultStr.CPRED;
  UserStr.CRES                 = DefaultStr.CRES;
  UserStr.CWRES                = DefaultStr.CWRES;
  UserStr.CETARES              = DefaultStr.CETARES;
  UserStr.CWETARES             = DefaultStr.CWETARES;
  UserStr.DV                   = DefaultStr.DV;
  UserStr.ORGDV                = DefaultStr.ORGDV;
  UserStr.MDV                  = DefaultStr.MDV;
  UserStr.ID                   = DefaultStr.ID;
  UserStr.F                    = DefaultStr.F;
  UserStr.Y                    = DefaultStr.Y;
  UserStr.T                    = DefaultStr.T;
  UserStr.P                    = DefaultStr.P;
  UserStr.A                    = DefaultStr.A;
  UserStr.DADT                 = DefaultStr.DADT;
  UserStr.EVID                 = DefaultStr.EVID;
  UserStr.DADT                 = DefaultStr.DADT;
  UserStr.AMT                  = DefaultStr.AMT;
  UserStr.CMT                  = DefaultStr.CMT;
  UserStr.PCMT                 = DefaultStr.PCMT;
  UserStr.R                    = DefaultStr.R;
  UserStr.D                    = DefaultStr.D;
  UserStr.ALAG                 = DefaultStr.ALAG;
  UserStr.FO                   = DefaultStr.FO; // ef-oh
  UserStr.F0                   = DefaultStr.F0; // ef-zero
  UserStr.S0                   = DefaultStr.S0; // es-zero
  UserStr.RATE                 = DefaultStr.RATE;
  UserStr.TIME                 = DefaultStr.TIME;
  UserStr.TSCALE               = DefaultStr.TSCALE;
  UserStr.S                    = DefaultStr.S;

  // These are used as insensitive search keys to find the values of
  // NONMEM-predefined variables in the symbol table or to be extracted
  // as C++ variable names when cases are supposed to be insensitive.
  KeyStr.THETA                 = SymbolTable::key( DefaultStr.THETA );
  KeyStr.ETA                   = SymbolTable::key( DefaultStr.ETA );
  KeyStr.EPS                   = SymbolTable::key( DefaultStr.EPS );
  KeyStr.OMEGA                 = SymbolTable::key( DefaultStr.OMEGA );
  KeyStr.SIGMA                 = SymbolTable::key( DefaultStr.SIGMA );
  KeyStr.PRED                  = SymbolTable::key( DefaultStr.PRED );
  KeyStr.RES                   = SymbolTable::key( DefaultStr.RES );
  KeyStr.WRES                  = SymbolTable::key( DefaultStr.WRES );
  KeyStr.ETARES                = SymbolTable::key( DefaultStr.ETARES );
  KeyStr.WETARES               = SymbolTable::key( DefaultStr.WETARES );
  KeyStr.IPRED                 = SymbolTable::key( DefaultStr.IPRED );
  KeyStr.IRES                  = SymbolTable::key( DefaultStr.IRES );
  KeyStr.IWRES                 = SymbolTable::key( DefaultStr.IWRES );
  KeyStr.IETARES               = SymbolTable::key( DefaultStr.IETARES );
  KeyStr.IWETARES              = SymbolTable::key( DefaultStr.IWETARES );
  KeyStr.PPRED                 = SymbolTable::key( DefaultStr.PPRED );
  KeyStr.PRES                  = SymbolTable::key( DefaultStr.PRES );
  KeyStr.PWRES                 = SymbolTable::key( DefaultStr.PWRES );
  KeyStr.PETARES               = SymbolTable::key( DefaultStr.PETARES );
  KeyStr.PWETARES              = SymbolTable::key( DefaultStr.PWETARES );
  KeyStr.CPRED                 = SymbolTable::key( DefaultStr.CPRED );
  KeyStr.CRES                  = SymbolTable::key( DefaultStr.CRES );
  KeyStr.CWRES                 = SymbolTable::key( DefaultStr.CWRES );
  KeyStr.CETARES               = SymbolTable::key( DefaultStr.CETARES );
  KeyStr.CWETARES              = SymbolTable::key( DefaultStr.CWETARES );
  KeyStr.DV                    = SymbolTable::key( DefaultStr.DV );
  KeyStr.ORGDV                 = SymbolTable::key( DefaultStr.ORGDV );
  KeyStr.MDV                   = SymbolTable::key( DefaultStr.MDV );
  KeyStr.ID                    = SymbolTable::key( DefaultStr.ID );
  KeyStr.F                     = SymbolTable::key( DefaultStr.F );
  KeyStr.Y                     = SymbolTable::key( DefaultStr.Y );
  KeyStr.T                     = SymbolTable::key( DefaultStr.T );
  KeyStr.P                     = SymbolTable::key( DefaultStr.P );
  KeyStr.A                     = SymbolTable::key( DefaultStr.A );
  KeyStr.DADT                  = SymbolTable::key( DefaultStr.DADT );
  KeyStr.EVID                  = SymbolTable::key( DefaultStr.EVID );
  KeyStr.DADT                  = SymbolTable::key( DefaultStr.DADT );
  KeyStr.AMT                   = SymbolTable::key( DefaultStr.AMT );
  KeyStr.CMT                   = SymbolTable::key( DefaultStr.CMT );
  KeyStr.PCMT                  = SymbolTable::key( DefaultStr.PCMT );
  KeyStr.R                     = SymbolTable::key( DefaultStr.R );
  KeyStr.D                     = SymbolTable::key( DefaultStr.D );
  KeyStr.ALAG                  = SymbolTable::key( DefaultStr.ALAG );
  KeyStr.FO                    = SymbolTable::key( DefaultStr.FO ); // ef-oh
  KeyStr.F0                    = SymbolTable::key( DefaultStr.F0 ); // ef-zero
  KeyStr.S0                    = SymbolTable::key( DefaultStr.S0 ); // es-zero
  KeyStr.RATE                  = SymbolTable::key( DefaultStr.RATE );
  KeyStr.TIME                  = SymbolTable::key( DefaultStr.TIME );
  KeyStr.TSCALE                = SymbolTable::key( DefaultStr.TSCALE );
  KeyStr.S                     = SymbolTable::key( DefaultStr.S );

  // SpkDataML tags & attributes;
    X_SPKDATA    = XMLString::transcode( C_SPKDATA );
    X_VERSION    = XMLString::transcode( C_VERSION );
    X_POINTONE   = XMLString::transcode( C_POINTONE );
    X_TABLE      = XMLString::transcode( C_TABLE );
    X_COLUMNS    = XMLString::transcode( C_COLUMNS );
    X_ROWS       = XMLString::transcode( C_ROWS );
    X_DESCRIPTION= XMLString::transcode( C_DESCRIPTION );
    X_ROW        = XMLString::transcode( C_ROW );
    X_POSITION   = XMLString::transcode( C_POSITION );
    X_VALUE      = XMLString::transcode( C_VALUE );
    X_TYPE       = XMLString::transcode( C_TYPE );
    X_NUMERIC    = XMLString::transcode( C_NUMERIC );
    X_ID         = XMLString::transcode( C_ID );
    X_MDV        = XMLString::transcode( C_MDV );
    X_EVID       = XMLString::transcode( C_EVID );
    X_AMT        = XMLString::transcode( C_AMT );

  // SpkSourceML tags
    //  X_DESCRIPTION                = XMLString::transcode( C_DESCRIPTION );
  X_IN                         = XMLString::transcode( C_IN );
  X_NONMEM                     = XMLString::transcode( C_NONMEM );
  X_POP_ANALYSIS               = XMLString::transcode( C_POP_ANALYSIS );
  X_IND_ANALYSIS               = XMLString::transcode( C_IND_ANALYSIS );
  X_CONSTRAINT                 = XMLString::transcode( C_CONSTRAINT );
  X_MONTE_CARLO                = XMLString::transcode( C_MONTE_CARLO );
  X_MODEL                      = XMLString::transcode( C_MODEL );
  X_ADVAN                      = XMLString::transcode( C_ADVAN );
  X_TRANS                      = XMLString::transcode( C_TRANS );
  X_PRED                       = XMLString::transcode( C_PRED );
  X_COMP_MODEL                 = XMLString::transcode( C_COMP_MODEL );
  X_COMPARTMENT                = XMLString::transcode( C_COMPARTMENT );
  X_DIFFEQN                    = XMLString::transcode( C_DIFFEQN );
  X_PK                         = XMLString::transcode( C_PK );
  X_ERROR                      = XMLString::transcode( C_ERROR );
  X_PRESENTATION               = XMLString::transcode( C_PRESENTATION );
  //  X_TABLE                      = XMLString::transcode( C_TABLE );
  X_SCATTERPLOT                = XMLString::transcode( C_SCATTERPLOT );
  X_COLUMN                     = XMLString::transcode( C_COLUMN );
  X_LOW                        = XMLString::transcode( C_LOW );
  X_UP                         = XMLString::transcode( C_UP );
  X_LABEL                      = XMLString::transcode( C_LABEL );
  X_LABELS                     = XMLString::transcode( C_LABELS );
  X_X                          = XMLString::transcode( C_X );
  X_Y                          = XMLString::transcode( C_Y );
  X_SPLIT                      = XMLString::transcode( C_SPLIT );
  X_THETA                      = XMLString::transcode( C_THETA );
  X_OMEGA                      = XMLString::transcode( C_OMEGA );
  X_SIGMA                      = XMLString::transcode( C_SIGMA );
  X_SIMULATION                 = XMLString::transcode( C_SIMULATION );
  X_POP_STAT                   = XMLString::transcode( C_POP_STAT );
  X_IND_STAT                   = XMLString::transcode( C_IND_STAT );
  X_NCOMPARTMENTS              = XMLString::transcode( C_NCOMPARTMENTS );
  X_NPARAMETERS                = XMLString::transcode( C_NPARAMETERS );
  X_NEQUILIBRIMS               = XMLString::transcode( C_NEQUILIBRIMS );
  X_INITIAL_OFF                = XMLString::transcode( C_INITIAL_OFF );
  X_NO_OFF                     = XMLString::transcode( C_NO_OFF );
  X_NO_DOSE                    = XMLString::transcode( C_NO_DOSE );
  X_EQUILIBRIM                 = XMLString::transcode( C_EQUILIBRIM );
  X_EXCLUDE                    = XMLString::transcode( C_EXCLUDE );
  X_DEFAULT_OBSERVATION        = XMLString::transcode( C_DEFAULT_OBSERVATION );
  X_DEFAULT_DOSE               = XMLString::transcode( C_DEFAULT_DOSE );
  X_TOLERANCE                  = XMLString::transcode( C_TOLERANCE );

  // SpkSourceML attributes
  X_FIXED                      = XMLString::transcode( C_FIXED );
  //  X_VALUE                      = XMLString::transcode( C_VALUE );
  X_STRUCT                     = XMLString::transcode( C_STRUCT );
  X_DIMENSION                  = XMLString::transcode( C_DIMENSION );
  X_IS_ERR_OUT                 = XMLString::transcode( C_IS_STDERROR_OUT );
  X_IS_CORR_OUT                = XMLString::transcode( C_IS_CORRELATION_OUT );
  X_IS_COV_OUT                 = XMLString::transcode( C_IS_COVARIANCE_OUT );
  X_IS_INV_COV_OUT             = XMLString::transcode( C_IS_INVERSE_COVARIANCE_OUT );
  X_IS_COEF_OUT                = XMLString::transcode( C_IS_COEFFICIENT_OUT );
  X_IS_CONF_OUT                = XMLString::transcode( C_IS_CONFIDENCE_OUT );
  X_APPROXIMATION              = XMLString::transcode( C_APPROXIMATION );
  X_METHOD                     = XMLString::transcode( C_METHOD );
  X_NUMBEREVAL                 = XMLString::transcode( C_NUMBEREVAL );
  X_POP_SIZE                   = XMLString::transcode( C_POP_SIZE  );
  X_IS_ESTIMATION              = XMLString::transcode( C_IS_ESTIMATION );
  X_IS_ETA_OUT                 = XMLString::transcode( C_IS_ETA_OUT );
  X_IS_RESTART                 = XMLString::transcode( C_IS_RESTART );
  X_DATA_LABELS                = XMLString::transcode( C_DATA_LABELS );
  X_FILENAME                   = XMLString::transcode( C_FILENAME );
  X_NAME                       = XMLString::transcode( C_NAME );
  X_SYNONYM                    = XMLString::transcode( C_SYNONYM );
  X_LENGTH                     = XMLString::transcode( C_LENGTH );
  X_SEED                       = XMLString::transcode( C_SEED );
  X_SUBPROBLEMS                = XMLString::transcode( C_SUBPROBLEMS );
  X_COVARIANCE_FORM            = XMLString::transcode( C_COVARIANCE_FORM );
  X_MITR                       = XMLString::transcode( C_MITR );
  X_SIG_DIGITS                 = XMLString::transcode( C_SIG_DIGITS );

  // SpkSourceML attribute values
  X_YES                        = XMLString::transcode( C_YES );
  X_NO                         = XMLString::transcode( C_NO );
  X_DIAGONAL                   = XMLString::transcode( C_DIAGONAL );
  X_BLOCK                      = XMLString::transcode( C_BLOCK );
  X_COV_R                      = XMLString::transcode( C_COV_R );
  X_COV_RSR                    = XMLString::transcode( C_COV_RSR );
  X_COV_S                      = XMLString::transcode( C_COV_S );
  X_COV_H                      = XMLString::transcode( C_COV_H );
  X_COV_HSH                    = XMLString::transcode( C_COV_HSH );
  X_FO                         = XMLString::transcode( C_FO );
  X_FOCE                       = XMLString::transcode( C_FOCE );
  X_LAPLACE                    = XMLString::transcode( C_LAPLACE );
  X_ANALYTIC                   = XMLString::transcode( C_ANALYTIC );
  X_GRID                       = XMLString::transcode( C_GRID );
  X_PLAIN                      = XMLString::transcode( C_PLAIN );
  X_MISER                      = XMLString::transcode( C_MISER );
  X_VEGAS                      = XMLString::transcode( C_VEGAS );

  // SpkReportML attribute
  X_SPKREPORT                  = XMLString::transcode( C_SPKREPORT );
  X_ELAPSEDTIME                = XMLString::transcode( C_ELAPSEDTIME );
  X_NEMBER_EVAL                = XMLString::transcode( C_NUMBER_EVAL );
  X_SUBPROBLEM                 = XMLString::transcode( C_SUBPROBLEM );
 
  // SpkReportML tags
  X_ERROR_LIST                 = XMLString::transcode( C_ERROR_LIST );
  X_WARNING_LIST               = XMLString::transcode( C_WARNING_LIST );
  X_POP_ANALYSIS_RESULT        = XMLString::transcode( C_POP_ANALYSIS_RESULT );
  X_IND_ANALYSIS_RESULT        = XMLString::transcode( C_IND_ANALYSIS_RESULT );
  X_OPT_TRACE_OUT              = XMLString::transcode( C_OPT_TRACE_OUT );
  X_POP_MONTE_RESULT           = XMLString::transcode( C_POP_MONTE_RESULT );
  X_MESSAGE                    = XMLString::transcode( C_MESSAGE );
  X_FILE_NAME                  = XMLString::transcode( C_FILE_NAME );
  X_LINE_NUMBER                = XMLString::transcode( C_LINE_NUMBER );
  X_WARNING                    = XMLString::transcode( C_WARNING );
  X_POP_OPT_RESULT             = XMLString::transcode( C_POP_OPT_RESULT );
  X_IND_OPT_RESULT             = XMLString::transcode( C_IND_OPT_RESULT );
  X_POP_STAT_RESULT            = XMLString::transcode( C_POP_STAT_RESULT );
  X_IND_STAT_RESULT            = XMLString::transcode( C_IND_STAT_RESULT );
  X_POP_OBJ_OUT                = XMLString::transcode( C_POP_OBJ_OUT );
  X_IND_OBJ_OUT                = XMLString::transcode( C_IND_OBJ_OUT );
  X_THETA_IN                   = XMLString::transcode( C_THETA_IN );
  X_THETA_OUT                  = XMLString::transcode( C_THETA_OUT );
  X_OMEGA_IN                   = XMLString::transcode( C_OMEGA_IN );
  X_OMEGA_OUT                  = XMLString::transcode( C_OMEGA_OUT );
  X_SIGMA_IN                   = XMLString::transcode( C_SIGMA_IN );
  X_SIGMA_OUT                  = XMLString::transcode( C_SIGMA_OUT );
  X_POP_STDERROR_OUT           = XMLString::transcode( C_POP_STDERROR_OUT );
  X_POP_COVARIANCE_OUT         = XMLString::transcode( C_POP_COVARIANCE_OUT );
  X_POP_INVERSE_COVARIANCE_OUT = XMLString::transcode( C_POP_INVERSE_COVARIANCE_OUT );
  X_POP_CORRELATION_OUT        = XMLString::transcode( C_POP_CORRELATION_OUT );
  X_POP_COEFFICIENT_OUT        = XMLString::transcode( C_POP_COEFFICIENT_OUT );
  X_POP_CONFIDENCE_OUT         = XMLString::transcode( C_POP_CONFIDENCE_OUT );
  X_IND_STDERROR_OUT           = XMLString::transcode( C_IND_STDERROR_OUT );
  X_IND_COVARIANCE_OUT         = XMLString::transcode( C_IND_COVARIANCE_OUT );
  X_IND_INVERSE_COVARIANCE_OUT = XMLString::transcode( C_IND_INVERSE_COVARIANCE_OUT );
  X_IND_CORRELATION_OUT        = XMLString::transcode( C_IND_CORRELATION_OUT );
  X_IND_COEFFICIENT_OUT        = XMLString::transcode( C_IND_COEFFICIENT_OUT );
  X_IND_CONFIDENCE_OUT         = XMLString::transcode( C_IND_CONFIDENCE_OUT );
  X_PRESENTATION_DATA          = XMLString::transcode( C_PRESENTATION_DATA );

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

  XMLString::release( &X_SPKDATA );
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
}
NonmemTranslator::NonmemTranslator( const NonmemTranslator& )
{
}
NonmemTranslator& NonmemTranslator::operator=( const NonmemTranslator& )
{
}











