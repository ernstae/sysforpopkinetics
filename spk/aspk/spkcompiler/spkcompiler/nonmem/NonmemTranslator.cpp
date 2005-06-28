#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"
#include "explang.h"
#include "../series.h"

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

//========================================

//========================================
// The global variables used by
// yyparse() and yylex() (or equivalent).
//----------------------------------------
extern int           gSpkExpErrors;
extern char*         gSpkExpErrorMessages;
extern int           gSpkExpLines;
extern SymbolTable * gSpkExpSymbolTable;
extern FILE *        gSpkExpOutput;
extern FILE *        nm_in;
extern int           NM_ACCEPT;
extern int           NM_ABORT;

extern "C"{
  int nm_parse(void);
  int nm_error( const char* );
};
//========================================

const char* NonmemTranslator::C_DESCRIPTION                ( "description" );
const char* NonmemTranslator::C_YES                        ( "yes" );
const char* NonmemTranslator::C_NO                         ( "no" );
const char* NonmemTranslator::C_FIXED                      ( "fixed" );
const char* NonmemTranslator::C_IN                         ( "in" );
const char* NonmemTranslator::C_LOW                        ( "low" );
const char* NonmemTranslator::C_UP                         ( "up" );
const char* NonmemTranslator::C_DIAGONAL                   ( "diagonal" );
const char* NonmemTranslator::C_BLOCK                      ( "block" );
const char* NonmemTranslator::C_VALUE                      ( "value" );
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
const char* NonmemTranslator::C_DIFFEQN                    ( "diffeqn" );
const char* NonmemTranslator::C_PK                         ( "pk" );
const char* NonmemTranslator::C_ERROR                      ( "error" );
const char* NonmemTranslator::C_PRESENTATION               ( "presentation" );
const char* NonmemTranslator::C_TABLE                      ( "table" );
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


NonmemTranslator::NonmemTranslator( DOMDocument* sourceIn, DOMDocument* dataIn )
  : ClientTranslator    ( sourceIn, dataIn ),
    fMakefile           ( "Makefile.SPK" ),
    fIndData_h          ( "IndData.h" ),
    fDataSet_h          ( "DataSet.h" ),
    fPredEqn_fortran    ( "predEqn.fortran" ),
    fPredEqn_cpp        ( "predEqn.cpp" ),
    fPred_h             ( "Pred.h" ),
    fDiffEqn_fortran    ( "diffEqn.fortran" ),
    fDiffEqn_cpp        ( "diffEqn.cpp" ),
    fODEPred_h          ( "Pred.h" ), // Use the same as for PRED so that Makefile doesn't have to be adjusted.
    fPkEqn_fortran      ( "pkEqn.fortran" ),
    fPkEqn_cpp          ( "pkEqn.cpp" ),
    fErrorEqn_fortran   ( "errorEqn.fortran" ),
    fErrorEqn_cpp       ( "errorEqn.cpp" ),   
    fNonmemPars_h       ( "NonmemPars.h" ),
    fMontePars_h        ( "MontePars.h" ),
    fFitDriver_cpp      ( "fitDriver.cpp" ),
    fMonteDriver_cpp    ( "monteDriver.cpp" ),
    fSpkRuntimeLongError_tmp( "scratch.tmp" ),
    fResult_xml         ( "result.xml" ),
    fCheckpoint_xml     ( "checkpoint.xml" ),
    myDescription       ( NULL ),
    myModelSpec         ( PRED ),
    myTrans             ( TRANS1 ),
    myIsEstimate        ( true ),
    myIsSimulate        ( false ),
    myIsMonte           ( false ),
    myIsStat            ( false ),
    mySubproblemsN      ( 1 ),
    myIntegMethod       ( PLAIN ),
    myIntegNumberEvals  ( 1 ), // this is a vector
    myIntegNEvals       ( 1 ),
    myIsPosthoc         ( true ),
    myIsRestart         ( false ),
    myIndWriteCheckpoint( true ),
    myPopWriteCheckpoint( true ),
    myThetaLen          ( 0 ),
    myOmegaDim          ( 0 ),
    myOmegaOrder        ( 0 ),
    myOmegaStruct       ( Symbol::TRIANGLE ),
    mySigmaDim          ( 0 ),
    mySigmaOrder        ( 0 ),
    mySigmaStruct       ( Symbol::TRIANGLE ),
    myEtaLen            ( 0 ),
    myEpsLen            ( 0 ),
    mySigDigits         ( 3 ),
    myPopMitr           ( 100 ),
    myIndMitr           ( 100 ),
    myPopEpsilon        ( pow( 10.0, -(mySigDigits+1.0) ) ),
    myIndEpsilon        ( pow( 10.0, -(mySigDigits+1.0) ) ),
    myPopTraceLevel     ( 1 ),
    myIndTraceLevel     ( 1 ),
    mySeed              ( 0 ),
    myCovForm           ( "RSR" ), // default for population level
    myIsStderr          ( true ),
    myIsCorrelation     ( true ),
    myIsCov             ( true ),
    myIsInvCov          ( true ),
    myIsConfidence      ( true ),
    myIsCoefficient     ( true ),
    myRecordNums        ( 1 )

{
  table = ClientTranslator::getSymbolTable();

  DefaultStr.THETA    = "THETA";
  DefaultStr.ETA      = "ETA";
  DefaultStr.EPS      = "EPS";
  DefaultStr.OMEGA    = "OMEGA";
  DefaultStr.SIGMA    = "SIGMA";
  DefaultStr.PRED     = "PRED";
  DefaultStr.RES      = "RES";
  DefaultStr.WRES     = "WRES";
  DefaultStr.ETARES   = "ETARES";
  DefaultStr.WETARES  = "WETARES";
  DefaultStr.IPRED    = "IPRED";
  DefaultStr.IRES     = "IRES";
  DefaultStr.IWRES    = "IWRES";
  DefaultStr.IETARES  = "IETARES";
  DefaultStr.IWETARES = "IWETARES";
  DefaultStr.PPRED    = "PPRED";
  DefaultStr.PRES     = "PRES";
  DefaultStr.PWRES    = "PWRES";
  DefaultStr.PETARES  = "PETARES";
  DefaultStr.PWETARES = "PWETARES";
  DefaultStr.CPRED    = "CPRED";
  DefaultStr.CRES     = "CRES";
  DefaultStr.CWRES    = "CWRES";
  DefaultStr.CETARES  = "CETARES";
  DefaultStr.CWETARES = "CWETARES";
  DefaultStr.DV       = "DV";
  DefaultStr.ORGDV    = "ORGDV";
  DefaultStr.MDV      = "MDV";
  DefaultStr.ID       = "ID";
  DefaultStr.F        = "F";
  DefaultStr.Y        = "Y";
  DefaultStr.T        = "T";
  DefaultStr.P        = "P";
  DefaultStr.A        = "A";
  DefaultStr.DADT     = "DADT";

  UserStr.THETA       = DefaultStr.THETA;
  UserStr.ETA         = DefaultStr.ETA;
  UserStr.EPS         = DefaultStr.EPS;
  UserStr.OMEGA       = DefaultStr.OMEGA;
  UserStr.SIGMA       = DefaultStr.SIGMA;
  UserStr.PRED        = DefaultStr.PRED;
  UserStr.RES         = DefaultStr.RES;
  UserStr.WRES        = DefaultStr.WRES;
  UserStr.ETARES      = DefaultStr.ETARES;
  UserStr.WETARES     = DefaultStr.WETARES;
  UserStr.IPRED       = DefaultStr.IPRED;
  UserStr.IRES        = DefaultStr.IRES;
  UserStr.IWRES       = DefaultStr.IWRES;
  UserStr.IETARES     = DefaultStr.IETARES;
  UserStr.IWETARES    = DefaultStr.IWETARES;
  UserStr.PPRED       = DefaultStr.PPRED;
  UserStr.PRES        = DefaultStr.PRES;
  UserStr.PWRES       = DefaultStr.PWRES;
  UserStr.PETARES     = DefaultStr.PETARES;
  UserStr.PWETARES    = DefaultStr.PWETARES;
  UserStr.CPRED       = DefaultStr.CPRED;
  UserStr.CRES        = DefaultStr.CRES;
  UserStr.CWRES       = DefaultStr.CWRES;
  UserStr.CETARES     = DefaultStr.CETARES;
  UserStr.CWETARES    = DefaultStr.CWETARES;
  UserStr.DV          = DefaultStr.DV;
  UserStr.ORGDV       = DefaultStr.ORGDV;
  UserStr.MDV         = DefaultStr.MDV;
  UserStr.ID          = DefaultStr.ID;
  UserStr.F           = DefaultStr.F;
  UserStr.Y           = DefaultStr.Y;
  UserStr.T           = DefaultStr.T;
  UserStr.P           = DefaultStr.P;
  UserStr.A           = DefaultStr.A;
  UserStr.DADT        = DefaultStr.DADT;

  // These are used as insensitive search keys to find the values of
  // NONMEM-predefined variables in the symbol table or to be extracted
  // as C++ variable names when cases are supposed to be insensitive.
  KeyStr.THETA        = SymbolTable::key( DefaultStr.THETA );
  KeyStr.ETA          = SymbolTable::key( DefaultStr.ETA );
  KeyStr.EPS          = SymbolTable::key( DefaultStr.EPS );
  KeyStr.OMEGA        = SymbolTable::key( DefaultStr.OMEGA );
  KeyStr.SIGMA        = SymbolTable::key( DefaultStr.SIGMA );
  KeyStr.PRED         = SymbolTable::key( DefaultStr.PRED );
  KeyStr.RES          = SymbolTable::key( DefaultStr.RES );
  KeyStr.WRES         = SymbolTable::key( DefaultStr.WRES );
  KeyStr.ETARES       = SymbolTable::key( DefaultStr.ETARES );
  KeyStr.WETARES      = SymbolTable::key( DefaultStr.WETARES );
  KeyStr.IPRED        = SymbolTable::key( DefaultStr.IPRED );
  KeyStr.IRES         = SymbolTable::key( DefaultStr.IRES );
  KeyStr.IWRES        = SymbolTable::key( DefaultStr.IWRES );
  KeyStr.IETARES      = SymbolTable::key( DefaultStr.IETARES );
  KeyStr.IWETARES     = SymbolTable::key( DefaultStr.IWETARES );
  KeyStr.PPRED        = SymbolTable::key( DefaultStr.PPRED );
  KeyStr.PRES         = SymbolTable::key( DefaultStr.PRES );
  KeyStr.PWRES        = SymbolTable::key( DefaultStr.PWRES );
  KeyStr.PETARES      = SymbolTable::key( DefaultStr.PETARES );
  KeyStr.PWETARES     = SymbolTable::key( DefaultStr.PWETARES );
  KeyStr.CPRED        = SymbolTable::key( DefaultStr.CPRED );
  KeyStr.CRES         = SymbolTable::key( DefaultStr.CRES );
  KeyStr.CWRES        = SymbolTable::key( DefaultStr.CWRES );
  KeyStr.CETARES      = SymbolTable::key( DefaultStr.CETARES );
  KeyStr.CWETARES     = SymbolTable::key( DefaultStr.CWETARES );
  KeyStr.DV           = SymbolTable::key( DefaultStr.DV );
  KeyStr.ORGDV        = SymbolTable::key( DefaultStr.ORGDV );
  KeyStr.MDV          = SymbolTable::key( DefaultStr.MDV );
  KeyStr.ID           = SymbolTable::key( DefaultStr.ID );
  KeyStr.F            = SymbolTable::key( DefaultStr.F );
  KeyStr.Y            = SymbolTable::key( DefaultStr.Y );
  KeyStr.T            = SymbolTable::key( DefaultStr.T );
  KeyStr.P            = SymbolTable::key( DefaultStr.P );
  KeyStr.A            = SymbolTable::key( DefaultStr.A );
  KeyStr.DADT         = SymbolTable::key( DefaultStr.DADT );

  // TAG names
  X_DESCRIPTION       = XMLString::transcode( C_DESCRIPTION );
  X_IN                = XMLString::transcode( C_IN );
  X_NONMEM            = XMLString::transcode( C_NONMEM );
  X_POP_ANALYSIS      = XMLString::transcode( C_POP_ANALYSIS );
  X_IND_ANALYSIS      = XMLString::transcode( C_IND_ANALYSIS );
  X_CONSTRAINT        = XMLString::transcode( C_CONSTRAINT );
  X_MONTE_CARLO       = XMLString::transcode( C_MONTE_CARLO );
  X_MODEL             = XMLString::transcode( C_MODEL );
  X_ADVAN             = XMLString::transcode( C_ADVAN );
  X_TRANS             = XMLString::transcode( C_TRANS );
  X_PRED              = XMLString::transcode( C_PRED );
  X_COMP_MODEL        = XMLString::transcode( C_COMP_MODEL );
  X_DIFFEQN           = XMLString::transcode( C_DIFFEQN );
  X_PK                = XMLString::transcode( C_PK );
  X_ERROR             = XMLString::transcode( C_ERROR );
  X_PRESENTATION      = XMLString::transcode( C_PRESENTATION );
  X_TABLE             = XMLString::transcode( C_TABLE );
  X_SCATTERPLOT       = XMLString::transcode( C_SCATTERPLOT );
  X_COLUMN            = XMLString::transcode( C_COLUMN );
  X_LOW               = XMLString::transcode( C_LOW );
  X_UP                = XMLString::transcode( C_UP );
  X_LABEL             = XMLString::transcode( C_LABEL );
  X_LABELS            = XMLString::transcode( C_LABELS );
  X_X                 = XMLString::transcode( C_X );
  X_Y                 = XMLString::transcode( C_Y );
  X_SPLIT             = XMLString::transcode( C_SPLIT );
  X_THETA             = XMLString::transcode( C_THETA );
  X_OMEGA             = XMLString::transcode( C_OMEGA );
  X_SIGMA             = XMLString::transcode( C_SIGMA );
  X_SIMULATION        = XMLString::transcode( C_SIMULATION );
  X_POP_STAT          = XMLString::transcode( C_POP_STAT );
  X_IND_STAT          = XMLString::transcode( C_IND_STAT );

  // Attribute names 
  X_FIXED             = XMLString::transcode( C_FIXED );
  X_VALUE             = XMLString::transcode( C_VALUE );
  X_STRUCT            = XMLString::transcode( C_STRUCT );
  X_DIMENSION         = XMLString::transcode( C_DIMENSION );
  X_IS_ERR_OUT        = XMLString::transcode( C_IS_STDERROR_OUT );
  X_IS_CORR_OUT       = XMLString::transcode( C_IS_CORRELATION_OUT );
  X_IS_COV_OUT        = XMLString::transcode( C_IS_COVARIANCE_OUT );
  X_IS_INV_COV_OUT    = XMLString::transcode( C_IS_INVERSE_COVARIANCE_OUT );
  X_IS_COEF_OUT       = XMLString::transcode( C_IS_COEFFICIENT_OUT );
  X_IS_CONF_OUT       = XMLString::transcode( C_IS_CONFIDENCE_OUT );
  X_APPROXIMATION     = XMLString::transcode( C_APPROXIMATION );
  X_METHOD            = XMLString::transcode( C_METHOD );
  X_NUMBEREVAL        = XMLString::transcode( C_NUMBEREVAL );
  X_POP_SIZE          = XMLString::transcode( C_POP_SIZE  );
  X_IS_ESTIMATION     = XMLString::transcode( C_IS_ESTIMATION );
  X_IS_ETA_OUT        = XMLString::transcode( C_IS_ETA_OUT );
  X_IS_RESTART        = XMLString::transcode( C_IS_RESTART );
  X_DATA_LABELS       = XMLString::transcode( C_DATA_LABELS );
  X_FILENAME          = XMLString::transcode( C_FILENAME );
  X_NAME              = XMLString::transcode( C_NAME );
  X_SYNONYM           = XMLString::transcode( C_SYNONYM );
  X_LENGTH            = XMLString::transcode( C_LENGTH );
  X_SEED              = XMLString::transcode( C_SEED );
  X_SUBPROBLEMS       = XMLString::transcode( C_SUBPROBLEMS );
  X_COVARIANCE_FORM   = XMLString::transcode( C_COVARIANCE_FORM );
  X_MITR              = XMLString::transcode( C_MITR );
  X_SIG_DIGITS        = XMLString::transcode( C_SIG_DIGITS );

  // Attribute values
  X_YES               = XMLString::transcode( C_YES );
  X_NO                = XMLString::transcode( C_NO );
  X_DIAGONAL          = XMLString::transcode( C_DIAGONAL );
  X_BLOCK             = XMLString::transcode( C_BLOCK );
  X_COV_R             = XMLString::transcode( C_COV_R );
  X_COV_RSR           = XMLString::transcode( C_COV_RSR );
  X_COV_S             = XMLString::transcode( C_COV_S );
  X_COV_H             = XMLString::transcode( C_COV_H );
  X_COV_HSH           = XMLString::transcode( C_COV_HSH );
  X_FO                = XMLString::transcode( C_FO );
  X_FOCE              = XMLString::transcode( C_FOCE );
  X_LAPLACE           = XMLString::transcode( C_LAPLACE );
  X_ANALYTIC          = XMLString::transcode( C_ANALYTIC );
  X_GRID              = XMLString::transcode( C_GRID );
  X_PLAIN             = XMLString::transcode( C_PLAIN );
  X_MISER             = XMLString::transcode( C_MISER );

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
  remove( fODEPred_h );
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
  delete [] myDescription;
  XMLString::release( &X_YES );
  XMLString::release( &X_NO );
  XMLString::release( &X_FIXED );
  XMLString::release( &X_IN );
  XMLString::release( &X_LOW );
  XMLString::release( &X_UP );
  XMLString::release( &X_DIAGONAL );
  XMLString::release( &X_BLOCK );
  XMLString::release( &X_VALUE );
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
  XMLString::release( &X_DIFFEQN );
  XMLString::release( &X_PK );
  XMLString::release( &X_ERROR );
  XMLString::release( &X_MONTE_CARLO );
  XMLString::release( &X_PRESENTATION );
  XMLString::release( &X_TABLE );
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
}
NonmemTranslator::NonmemTranslator( const NonmemTranslator& )
{
}
NonmemTranslator& NonmemTranslator::operator=( const NonmemTranslator& )
{
}

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  detAnalysisType()
//  Determines the type of analysis and the number of subjects.
//
//  Pre-conditions
// 
//  * source points to a valid XMLDocument
//
//  Post-conditions
//
//  * ourTarget is set to either POP or IND
//  * ourPopSize is set to the number of subjects
//
/////////////////////////////////////////////////////////////////////////////////////////////
int NonmemTranslator::detAnalysisType()
{
  DOMElement  * spksource      = source->getDocumentElement();
  DOMNodeList * pop_analysises = spksource->getElementsByTagName( X_POP_ANALYSIS );
  DOMNodeList * ind_analysises = spksource->getElementsByTagName( X_IND_ANALYSIS );
  DOMElement  * analysis;
  if( pop_analysises->getLength() > 0 )
    {
      if( ind_analysises->getLength() > 0 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, 
		   "<%s> and <%s> elements cannot be found together in a sourceML document.", 
		   C_POP_ANALYSIS, C_IND_ANALYSIS );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	  throw e;
	}
      ourTarget = POP;
      analysis = dynamic_cast<DOMElement*>( pop_analysises->item(0) );
      assert( analysis != NULL );

      //
      // Finding out the population size
      //
      if( !analysis->hasAttribute( X_POP_SIZE ) )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Missing <%s::%s> attribute specification.", C_POP_ANALYSIS, C_POP_SIZE );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	  throw e;
	}
      const XMLCh * xml_pop_size = analysis->getAttribute( X_POP_SIZE );
      if( !XMLString::textToBin( xml_pop_size, ourPopSize ) )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, 
		   "Invalid <%s::%s> attribute value: \"%s\"", C_POP_ANALYSIS, C_POP_SIZE,
		   XMLString::transcode(xml_pop_size) );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	  throw e;
	}
      return ourPopSize;
    }
  else //( ind_analysises->getLength() > 0 )
    {
      ourTarget = IND;
      ourPopSize = 1;
      return ourPopSize;
    }

  return ourPopSize;
}

/////////////////////////////////////////////////////////////////////////////////////////////
//
//  parseSource()
//  Analizes source.xml further in details.
//
//  Pre-conditions
//
//  * ourTarget is set to either POP or IND
//  * ourPopSize is set to the number of subjects.
// 
//  Post-conditions
//
/////////////////////////////////////////////////////////////////////////////////////////////
void NonmemTranslator::parseSource()
{
  //---------------------------------------------------------------------------------------
  // <nonmem>
  //---------------------------------------------------------------------------------------
  DOMElement  * spksouce = source->getDocumentElement();
  DOMNodeList * nonmems  = spksouce->getElementsByTagName( X_NONMEM );
  if( !nonmems->getLength() > 0 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s> element.", C_NONMEM );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement * nonmem = dynamic_cast<DOMElement*>( nonmems->item(0) );

  //---------------------------------------------------------------------------------------
  // <constraint>
  //---------------------------------------------------------------------------------------
  DOMNodeList * constraints = nonmem->getElementsByTagName( X_CONSTRAINT );
  if( constraints->getLength() != 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen()];
      sprintf( mess, "Missing <%s> element.", C_CONSTRAINT );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement * constraint = dynamic_cast<DOMElement*>( constraints->item(0) );
  if(  !constraint->hasChildNodes() )
    {
      char mess[ SpkCompilerError::maxMessageLen()];
      sprintf( mess, "Missing <%s> or <%s> element.", 
	       C_POP_ANALYSIS, 
	       C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  DOMElement * analysis;
  bool isAnalysisDone = false;
  if( ourTarget == POP )
    {
      DOMNodeList * pop_analysises = constraint->getElementsByTagName( X_POP_ANALYSIS );
      analysis = dynamic_cast<DOMElement*>( pop_analysises->item(0) );
      parsePopAnalysis( analysis );
    }
  else //if( ourTarget == IND )
    {
      DOMNodeList * ind_analysises = constraint->getElementsByTagName( X_IND_ANALYSIS );
      analysis = dynamic_cast<DOMElement*>( ind_analysises->item(0) );
      parseIndAnalysis( analysis );
    }
  isAnalysisDone = true;

  //---------------------------------------------------------------------------------------
  // <model>
  // NOTE: <pred> and the combination of {<pk>, <diffeqn>, <error>} are allowed.
  //       Default to <pred>.
  //---------------------------------------------------------------------------------------
  DOMNodeList * models = nonmem->getElementsByTagName( X_MODEL );
  if( models->getLength() != 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s> element.", C_MODEL );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  DOMElement  * model = dynamic_cast<DOMElement*>( models->item(0) );

  bool myIsModelDone = false;
  unsigned int advan, trans;

  //
  // All ADVANs fit to the compartmental modeling framework.
  //
  if( model->hasAttribute( X_ADVAN ) )
    {
      XMLString::textToBin( model->getAttribute( X_ADVAN ), advan );
      assert( advan > 0 );

      assert( myTrans == TRANS1 );  // default TRANS value
      if( model->hasAttribute( X_TRANS ) )
	{
	  XMLString::textToBin( model->getAttribute( X_TRANS ), trans );
	  myTrans = static_cast<TRANS>( trans );
	}
      myModelSpec = static_cast<MODEL_SPEC>( advan );
      parseAdvan( myModelSpec, myTrans, model );
    }
  else
    {
      DOMNodeList * preds   = model->getElementsByTagName( X_PRED );
      if( preds->getLength() < 1 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Missing <pred>!" );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
				  mess, __LINE__, __FILE__ );
	  throw e;
	}
      myModelSpec = PRED;
      DOMElement  * pred = dynamic_cast<DOMElement*>( preds->item(0) );
      parsePred( pred ); 
    }
  
  myIsModelDone = true;
 
  //---------------------------------------------------------------------------------------
  // <monte_carlo>
  //---------------------------------------------------------------------------------------
  DOMNodeList * monte_carlos = nonmem->getElementsByTagName( X_MONTE_CARLO );
  DOMElement * monte_carlo;
  if( monte_carlos->getLength() > 0 )
    {
      myIsMonte = true;


      //
      // REVISIT - Sachiko - 09/09/2004
      // estimation and MC are mutually exclusive.  Currently (as of 9/9/04)
      // MDA allows both to be true, so for now set myIsEstimate=false.
      //
      if( myIsEstimate )
	myIsEstimate = false;
      //
      // if( myIsEstimate )
      // {
      //    char mess[ SpkCompilerError::maxMessageLen() ];
      //    sprintf( mess, "The parameter estimation and the post-interation requests are mutually exclusive." );
      //    SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
      //        mess, __LINE__, __FILE__ );
      //    throw e;
      // }

      if( ourTarget != POP )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Integral methods are only valid for the population analysis results.");
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
				  mess, __LINE__, __FILE__ );
	  throw e;
	}
      monte_carlo = dynamic_cast<DOMElement*>( monte_carlos->item(0) );
      parseMonte( monte_carlo );
    }

  //---------------------------------------------------------------------------------------
  //<presentation>
  //---------------------------------------------------------------------------------------
  //PRED parsing and <xxx_analysis> parsing must have been completed so
  //that the symbol table contains entries for the user defined
  //variables and THETA/OMEGA/SIGMA/ETA.
  if( !myIsModelDone )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "<%s> must be parsed before parsing <%s>.", 
	       C_PRED, C_PRESENTATION);
      SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  if( !isAnalysisDone )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "<%s> must be parsed before parsing <%s>", 
	       (ourTarget==POP? C_POP_ANALYSIS : C_IND_ANALYSIS ), 
	       C_PRESENTATION );
      SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  
  DOMNodeList * presentations = nonmem->getElementsByTagName( X_PRESENTATION );
  if( presentations->getLength() > 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Multiple <%s> elements are found.", C_PRESENTATION ); 
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  DOMElement * presentation = dynamic_cast<DOMElement*>( presentations->item(0) );

  myRecordNums.resize( ourPopSize );
  Symbol * id = table->findi( KeyStr.ID );
  if( id == NULL || id == Symbol::empty() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "\"%s\" data item seems missing from the data set.", 
	       DefaultStr.ID.c_str() ); 
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  // Determine the number of data records for each subject.
  for( int i=0; i<ourPopSize; i++ )
    {
      myRecordNums[i] = id->initial[i].size();
    }

  // ORGDV is a place for the original data set to be kept
  // if a new data set is simulated.
  Symbol * s = table->insertUserVar( DefaultStr.ORGDV );
 
  // Keep the user-typed Nonmem Keyword strings
  Symbol * p;
  if( (p = table->findi( KeyStr.ID )) != Symbol::empty() )
    UserStr.ID = p->name;
  else
    UserStr.ID = DefaultStr.ID;

  if( (p = table->findi( KeyStr.THETA )) != Symbol::empty() )
    UserStr.THETA = p->name;
  else
    UserStr.THETA = DefaultStr.THETA;

  if( (p = table->findi( KeyStr.OMEGA )) != Symbol::empty() )
    UserStr.OMEGA = p->name;
  else
    UserStr.OMEGA = DefaultStr.OMEGA;

  if( (p = table->findi( KeyStr.SIGMA )) != Symbol::empty() )
    UserStr.SIGMA = p->name;
  else
    UserStr.SIGMA = DefaultStr.SIGMA;

  if( (p = table->findi( KeyStr.ETA )) != Symbol::empty() )
    UserStr.ETA = p->name;
  else
    UserStr.ETA = DefaultStr.ETA;

  if( (p = table->findi( KeyStr.EPS )) != Symbol::empty() )
    UserStr.EPS = p->name;
  else
    UserStr.EPS = DefaultStr.EPS;

  // PRED
  if( (p = table->findi( KeyStr.PRED )) != Symbol::empty() )
    UserStr.PRED = p->name;
  else
    {
      table->insertUserVar( DefaultStr.PRED );
      UserStr.PRED = DefaultStr.PRED;
    }

  // RES
  if( (p = table->findi( KeyStr.RES )) != Symbol::empty() )
    UserStr.RES = p->name;
  else
    {
      table->insertUserVar( DefaultStr.RES );
      UserStr.RES = DefaultStr.RES;
    }

  // WRES
  if( (p = table->findi( KeyStr.WRES )) != Symbol::empty() )
    UserStr.WRES = p->name;
  else
    {
      table->insertUserVar( DefaultStr.WRES );
      UserStr.WRES = DefaultStr.WRES;
    }

  // IPRED
  if( (p = table->findi( KeyStr.IPRED )) != Symbol::empty() )
    UserStr.IPRED = p->name;
  else
    {
      table->insertUserVar( DefaultStr.IPRED );
      UserStr.IPRED = DefaultStr.IPRED;
    }

  // IRES
  if( (p = table->findi( KeyStr.IRES )) != Symbol::empty() )
    UserStr.IRES = p->name;
  else
    {
      table->insertUserVar( DefaultStr.IRES );
      UserStr.IRES = DefaultStr.IRES;
    }

  // IWRES
  if( (p = table->findi( KeyStr.IWRES )) != Symbol::empty() )
    UserStr.IWRES = p->name;
  else
    {
      table->insertUserVar( DefaultStr.IWRES );
      UserStr.IWRES = DefaultStr.IWRES;
    }

  if( ourTarget == POP )
    {
      // ETARES
      if( (p = table->findi( KeyStr.ETARES )) != Symbol::empty() )
	UserStr.ETARES = p->name;
      else
	{
	  table->insertNMVector( DefaultStr.ETARES, myEtaLen );
	  UserStr.ETARES = DefaultStr.ETARES;
	}

      // WETARES
      if( (p = table->findi( KeyStr.WETARES )) != Symbol::empty() )
	UserStr.WETARES = p->name;
      else
	{
	  table->insertNMVector( DefaultStr.WETARES, myEtaLen );
	  UserStr.WETARES = DefaultStr.WETARES;
	}

      // IETARES
      if( (p = table->findi( KeyStr.IETARES )) != Symbol::empty() )
	UserStr.IETARES = p->name;
      else
	{
	  table->insertNMVector( DefaultStr.IETARES, myEtaLen );
	  UserStr.IETARES = DefaultStr.IETARES;
	}

      // IWETARES
      if( (p = table->findi( KeyStr.IWETARES )) != Symbol::empty() )
	UserStr.IWETARES = p->name;
      else
	{
	  table->insertNMVector( DefaultStr.IWETARES, myEtaLen );
	  UserStr.IWETARES = DefaultStr.IWETARES;
	}
      // PPRED
      if( (p = table->findi( KeyStr.PPRED )) != Symbol::empty() )
	UserStr.PPRED = p->name;
      else
	{
	  table->insertUserVar( DefaultStr.PPRED );
	  UserStr.PPRED = DefaultStr.PPRED;
	}
      
      // PRES
      if( (p = table->findi( KeyStr.PRES )) != Symbol::empty() )
	UserStr.PRES = p->name;
      else
	{
	  table->insertUserVar( DefaultStr.PRES );
	  UserStr.PRES = DefaultStr.PRES;
	}
      
      // PWRES
      if( (p = table->findi( KeyStr.PWRES )) != Symbol::empty() )
	UserStr.PWRES = p->name;
      else
	{
	  table->insertUserVar( DefaultStr.PWRES );
	  UserStr.PWRES = DefaultStr.PWRES;
	}

      // PETARES
      if( (p = table->findi( KeyStr.PETARES )) != Symbol::empty() )
	UserStr.PETARES = p->name;
      else
	{
	  table->insertNMVector( DefaultStr.PETARES, myEtaLen );
	  UserStr.PETARES = DefaultStr.PETARES;
	}

      // PWETARES
      if( (p = table->findi( KeyStr.PWETARES )) != Symbol::empty() )
	UserStr.PWETARES = p->name;
      else
	{
	  table->insertNMVector( DefaultStr.PWETARES, myEtaLen );
	  UserStr.PWETARES = DefaultStr.PWETARES;
	}

      // CPRED
      if( (p = table->findi( KeyStr.CPRED )) != Symbol::empty() )
	UserStr.CPRED = p->name;
      else
	{
	  table->insertUserVar( DefaultStr.CPRED );
	  UserStr.CPRED = DefaultStr.CPRED;
	}
      
      // CRES
      if( (p = table->findi( KeyStr.CRES )) != Symbol::empty() )
	UserStr.CRES = p->name;
      else
	{
	  table->insertUserVar( DefaultStr.CRES );
	  UserStr.CRES = DefaultStr.CRES;
	}
      
      // CWRES
      if( (p = table->findi( KeyStr.CWRES )) != Symbol::empty() )
	UserStr.CWRES = p->name;
      else
	{
	  table->insertUserVar( DefaultStr.CWRES );
	  UserStr.CWRES = DefaultStr.CWRES;
	}

      // CETARES
      if( (p = table->findi( KeyStr.CETARES )) != Symbol::empty() )
	UserStr.CETARES = p->name;
      else
	{
	  table->insertNMVector( DefaultStr.CETARES, myEtaLen );
	  UserStr.CETARES = DefaultStr.CETARES;
	}

      // CWETARES
      if( (p = table->findi( KeyStr.CWETARES )) != Symbol::empty() )
	UserStr.CWETARES = p->name;
      else
	{
	  table->insertNMVector( DefaultStr.CWETARES, myEtaLen );
	  UserStr.CWETARES = DefaultStr.CWETARES;
	}
    }

  // MDV
  if( (p = table->findi( KeyStr.MDV )) != Symbol::empty() )
    UserStr.MDV = p->name;
  else
    {
      Symbol * s = table->insertLabel( DefaultStr.MDV, "", myRecordNums );
      for( int i=0; i<ourPopSize; i++ )
	s->initial[i] = "0";
      UserStr.MDV = DefaultStr.MDV;
    }
  if( (p = table->findi( KeyStr.ORGDV )) != Symbol::empty() )
    UserStr.ORGDV = p->name;
  else
    {
      UserStr.ORGDV = DefaultStr.ORGDV; 
    }

  if( (p = table->findi( KeyStr.DV )) != Symbol::empty() )
    UserStr.DV = p->name;
  else
    UserStr.DV = DefaultStr.DV;

  if( (p = table->findi( KeyStr.F )) != Symbol::empty() )
    UserStr.F = p->name;
  else
    UserStr.F = DefaultStr.F;
  
  if( (p = table->findi( KeyStr.Y )) != Symbol::empty() )
    UserStr.Y = p->name;
  else
    UserStr.Y = DefaultStr.Y;
  
  //
  // Generate the headers and definition files for IndData class and
  // DataSet class.
  //
  // The symbol table (ie. the order of data labels in the list) must not change
  // in between the following two routines.
  //
  generateDataSet();
  generateIndData();

  if( myModelSpec == PRED )
    generatePred( fPredEqn_cpp );
  else //( myModelSpec == PRED )
    {
      //    generateODEPred( fPkEqn_cpp, fDiffEqn_cpp, fErrorEqn_cpp );
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Invalid model specification!." );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess,
			      __LINE__, __FILE__ );
      throw e;

    }

  generateNonmemParsNamespace();
  if( myIsMonte )
    generateMonteParsNamespace();
  
  if( myIsEstimate || myIsSimulate )
    {
      if( ourTarget == POP )
	generatePopDriver();
      else 
	generateIndDriver();
    }
  generateMakefile();
}
//=============================================================================
//
// parse <monte_carlo>
//
// Pre-conditions - (Nothing really)
//
// Post-condtions -  The following variables are set to valid values:
//                     myIntegMethod
//                     myIntegNEvals
//                     myIntegNumberEvals (vector)
//
//=============================================================================
void NonmemTranslator::parseMonte( DOMElement* monte_carlo )
{
  assert( monte_carlo != NULL );
  if( monte_carlo->hasAttribute( X_METHOD ) )
    {
      const XMLCh* x_temp = monte_carlo->getAttribute( X_METHOD );
      if( XMLString::equals( x_temp, X_ANALYTIC ) )
	myIntegMethod = ANALYTIC;
      else if( XMLString::equals( x_temp, X_GRID ) )
	myIntegMethod = GRID;
      else if( XMLString::equals( x_temp, X_MISER ) )
	myIntegMethod = MISER;
      else //if( XMLString::equals( x_temp, X_PLAIN ) )
	myIntegMethod = PLAIN;
    }
  else
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s::%s> attribute.", C_MONTE_CARLO, C_METHOD );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess,
			      __LINE__, __FILE__ );
      throw e;
    }
  DOMNodeList * number_evals = monte_carlo->getElementsByTagName( X_NUMBEREVAL );
  if( number_evals->getLength() < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s> element!", C_NUMBEREVAL );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
			      mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement  * number_eval  = dynamic_cast<DOMElement*>( number_evals->item(0) );
  DOMNodeList * value_list = number_eval->getElementsByTagName( X_VALUE );
  myIntegNEvals = value_list->getLength();
  if( myIntegNEvals < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s> element!",
	       C_VALUE );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
			      mess, __LINE__, __FILE__ );
      throw e;
    }

  // Only when the integration method is GRID, the number of function evaluations is
  // something other then 1.  Actually it is equal to the order of OMEGA (ie. the length of ETA).
  if( myIntegMethod == GRID )
    {
      if( myIntegNEvals != myEtaLen )
        {
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "The number <%s> elements must be equal to the length of ETA (%d) for grid and miser approximation!", 
		   C_VALUE, myEtaLen );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
				  mess, __LINE__, __FILE__ );
	  throw e;
        }
    }
  else // plain, miser, analytic
    {
      // For these methods, ignore what the user says.
      // They take only one and the first occurence of <number_eval>.
      myIntegNEvals = 1;
    }
  myIntegNumberEvals.resize( myIntegNEvals );
  for( int i=0; i<myIntegNEvals; i++ )
    {
      DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(i) );
      const XMLCh * x_value = value->getFirstChild()->getNodeValue();
      unsigned int temp_value = 0;
      XMLString::textToBin( x_value, temp_value );
      myIntegNumberEvals[i] = temp_value;
    }
}

//=============================================================================
//
// Create a Makefile for either an SPK Optimization request or
// a Monte Carlo integration request.
//
// Pre-condtions   - myIsMonte is set to true if the post-integration is
//                   going to be performed.  False otherwise.
// 
//                 - The current working directory is writable.
//
// Post-conditions - A file, Makefile.SPK, is saved in the current 
//                   working directory.  The make file defines targets
//                   that are either to build an optimization/simulation driver
//                   or a post-integration driver.
//
//=============================================================================
void NonmemTranslator::generateMakefile() const
{
  ofstream oMakefile( fMakefile );
  if( !oMakefile.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Failed to create %s file.", fMakefile ); 
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  oMakefile << "PROD_DIR  = spkprod" << endl;
  oMakefile << "TEST_DIR  = spktest" << endl;
  oMakefile << endl;                                   

  oMakefile << "CPP_FLAGS = -g" << endl;
  oMakefile << endl;                                        

  oMakefile << "LIBS      = -lspk -lspkopt -lspkpred";
  oMakefile << (myIsMonte? " -lgsl" : "" ) << " -latlas_lapack -lcblas -latlas -lpthread -lm -lxerces-c" << endl;
  oMakefile << endl;

  oMakefile << "COMMON_INCLUDE = \\" << endl;
  oMakefile << "\tPred.h \\" << endl;
  oMakefile << "\tDataSet.h \\" << endl;
  oMakefile << "\tIndData.h \\" << endl;
  oMakefile << "\tNonmemPars.h \\" << endl;
  oMakefile << endl;                                   

  if( !myIsMonte )
    {
      oMakefile << "prod : fitDriver.cpp $(COMMON_INCLUDE)" << endl;
      oMakefile << "\tg++ $(CPP_FLAGS) fitDriver.cpp -o driver ";
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;
      
      oMakefile << "test : fitDriver.cpp $(COMMON_INCLUDE)" << endl;
      oMakefile << "\tg++ $(CPP_FLAGS) fitDriver.cpp -o driver ";
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;

      oMakefile << "clean : " << endl;
      oMakefile << "\trm -f $(COMMON_INCLUDE) \\" << endl;
      oMakefile << "\tspkDriver.cpp \\" << endl;
      oMakefile << "\tdriver \\" << endl;
      oMakefile << "\tsoftware_error \\" << endl;
      oMakefile << "\tresult.xml \\" << endl;
      oMakefile << "\tpredEqn.cpp \\" << endl;
      oMakefile << "\tspk_error.tmp \\" << endl;
      oMakefile << "*.o" << endl;    
    }
  else
    {
      oMakefile << "MONTE_SRC = \\" << endl;
      oMakefile << "\tmonteDriver.cpp \\" << endl; 
      oMakefile << "\tAnalyticIntegral.cpp \\" << endl;
      oMakefile << "\tGridIntegral.cpp \\" << endl;
      oMakefile << "\tMontePopObj.cpp \\" << endl;
      oMakefile << "\tMapBay.cpp \\" << endl;
      oMakefile << "\tMapMonte.cpp \\" << endl;
      oMakefile << "\tGsl2SpkError.cpp" << endl;
      oMakefile << endl;

      oMakefile << "MONTE_INCLUDE = \\" << endl;
      oMakefile << "\tAnalyticIntegral.h \\" << endl;
      oMakefile << "\tGridIntegral.h \\" << endl;
      oMakefile << "\tMontePopObj.h \\" << endl;
      oMakefile << "\tMapBay.h \\" << endl;
      oMakefile << "\tMapMonte.h \\" << endl;
      oMakefile << "\tGsl2SpkError.h" << endl;
      oMakefile << endl;

      oMakefile << "prod : " << endl;
      oMakefile << "\tmake -f Makefile.SPK monte_clean" << endl;
      oMakefile << "\tcp /usr/local/src/$(PROD_DIR)/ml/* ." << endl;
      oMakefile << "\tg++ $(CPP_FLAGS) $(MONTE_SRC) -o monteDriver ";
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;
      
      oMakefile << "test : " << endl;
      oMakefile << "\tmake -f Makefile.SPK monte_clean" << endl;
      oMakefile << "\tcp /usr/local/src/$(TEST_DIR)/ml/* . " << endl;
      oMakefile << "\tg++ $(CPP_FLAGS) $(MONTE_SRC) -o monteDriver ";
      oMakefile << "-L/usr/local/lib ";
      oMakefile << "-L/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;
  
      oMakefile << "monte_clean : " << endl;
      oMakefile << "\trm -f $(MONTE_SRC) $(MONTE_INCLUDE)" << endl;
      oMakefile << endl;

      oMakefile << "clean : " << endl;
      oMakefile << "\trm -f $(COMMON_INCLUDE) \\" << endl;
      oMakefile << "\t$(MONTE_SRC) \\" << endl;
      oMakefile << "\t$(MONTE_INCLUDE) \\" << endl;
      oMakefile << "\tdriver \\" << endl;
      oMakefile << "\tsoftware_error \\" << endl;
      oMakefile << "\tresult.xml \\" << endl;
      oMakefile << "\tpredEqn.cpp \\" << endl;
      oMakefile << "\tspk_error.tmp \\" << endl;
      oMakefile << "\t*.o" << endl;
    }
  oMakefile.close();

  return;
}

void NonmemTranslator::parsePopAnalysis( DOMElement* pop_analysis )
{
  
  //---------------------------------------------------------------------------------------
  // <pop_analysis>: Attributes required when "is_estimation=yes".
  //---------------------------------------------------------------------------------------
  // * approximation = {fo, foce, laplace}
  // * pop_size
  // * is_estimation = {yes, no}
  //
  // Finding out if parameter estimation is requested.
  //
  if( !pop_analysis->hasAttribute( X_IS_ESTIMATION ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s::%s> attribute.", C_IS_ESTIMATION, C_POP_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
      throw e;
    }
  const XMLCh * xml_is_estimation = pop_analysis->getAttribute( X_IS_ESTIMATION );
  myIsEstimate = ( XMLString::equals( xml_is_estimation, X_YES )? true : false );


  if( myIsEstimate )
    {
      //
      // Finding out the approximation method
      //
      if( !pop_analysis->hasAttribute( X_APPROXIMATION ) )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Missing <%s::%s> attribute.", C_POP_ANALYSIS, C_APPROXIMATION );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	  throw e;
	}
      const XMLCh * xml_approx = pop_analysis->getAttribute( X_APPROXIMATION );
      
      if( XMLString::equals( xml_approx, X_FO ) )
	ourApproximation = FO;
      else if( XMLString::equals( xml_approx, X_FOCE ) )
	ourApproximation = FOCE;
      else if( XMLString::equals( xml_approx, X_LAPLACE ) )
	ourApproximation = LAPLACE;  
      else
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Invalid <%s::%s> attribute value: \"%s\".", 
		   C_POP_ANALYSIS, 
		   C_APPROXIMATION, 
		   XMLString::transcode(xml_approx) );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	  throw e;
	}
    }

  myIndTraceLevel = 0;
  myPopTraceLevel = 1;

  //---------------------------------------------------------------------------------------
  // Optional attributes
  //---------------------------------------------------------------------------------------
  // * is_eta_out = {yes, "no"}
  // * is_restart = {yes, "no"}
  if( myIsEstimate )
    {
      const XMLCh * xml_is_eta_out;
      if( pop_analysis->hasAttribute( X_IS_ETA_OUT ) )
	{
	  xml_is_eta_out = pop_analysis->getAttribute( X_IS_ETA_OUT );
	  myIsPosthoc = ( XMLString::equals( xml_is_eta_out, X_YES )? true : false );
	}
      
      const XMLCh * xml_is_restart;
      if( pop_analysis->hasAttribute( X_IS_RESTART ) )
	{
	  xml_is_restart = pop_analysis->getAttribute( X_IS_RESTART );
	  myIsRestart = ( XMLString::equals( xml_is_restart, X_YES )? true : false );
	}
      if( pop_analysis->hasAttribute( X_MITR ) )
	{
	  const XMLCh* xml_mitr = pop_analysis->getAttribute( X_MITR );
	  if( !XMLString::textToBin( xml_mitr, myPopMitr ) )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      sprintf( mess, "Invalid <%s::%s> attribute value: \"%s\".", 
		       C_POP_ANALYSIS, C_MITR, XMLString::transcode(xml_mitr) );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	      throw e;
	    }
	}
      /*
      else{
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "Missing <%s::%s> attribute.", 
		 C_POP_ANALYSIS, C_MITR );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, 
				__LINE__, __FILE__ );
	throw e;
      }
      */
      const XMLCh* xml_sig_digits;
      if( pop_analysis->hasAttribute( X_SIG_DIGITS ) )
	{
	  xml_sig_digits = pop_analysis->getAttribute( X_SIG_DIGITS );
	  if( XMLString::stringLen( xml_sig_digits ) > 0 )
	    {
	      if( !XMLString::textToBin( xml_sig_digits, mySigDigits ) )
		{
		  char mess[ SpkCompilerError::maxMessageLen() ];
		  sprintf( mess, "Invalid <%s::%s> attribute value: \"%s\".", 
			   C_POP_ANALYSIS, 
			   C_SIG_DIGITS, 
			   XMLString::transcode( xml_sig_digits ) );
		  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
		  throw e;
              
		}
	      if( !( mySigDigits > 0 && mySigDigits < 9 ) )
		{
		  char mess[ SpkCompilerError::maxMessageLen() ];
		  sprintf( mess, "Invalid <%s::%s> attribute value: \"%s\".  Valid range: (1-8)", 
			   C_POP_ANALYSIS, C_SIG_DIGITS, XMLString::transcode( xml_sig_digits )  );
		  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
		  throw e;
		}
	      myIndEpsilon = pow( 10.0, -(mySigDigits + 1.0) );
	      myPopEpsilon = myIndEpsilon;
	    }
	}
    }

  //---------------------------------------------------------------------------------------
  // Required elements
  //---------------------------------------------------------------------------------------
  // <data_labels>
  // <theta>
  // <omega>+
  // <sigma>+
  DOMNodeList * data_labels_list = pop_analysis->getElementsByTagName( X_DATA_LABELS );
  if( data_labels_list->getLength() > 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Multiple <%s> elements found in the sourceML document.", 
	       C_DATA_LABELS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
      throw e;
    }
  if( data_labels_list->getLength() < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s> element.", 
	       C_DATA_LABELS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
      throw e;
    }

  DOMElement * data_labels = dynamic_cast<DOMElement*>( data_labels_list->item(0) );
  {
    DOMNodeList * labels = data_labels->getElementsByTagName( X_LABEL );
    int nLabels = labels->getLength();
    if( nLabels < 1 ) 
      {

        char mess[ SpkCompilerError::maxMessageLen() ];
        sprintf( mess, "Missing <%s> element.",  C_LABEL );
        SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
        throw e;
      }
    for( int i=0; i<nLabels; i++ )
      {
	DOMElement * xml_label = dynamic_cast<DOMElement*>( labels->item(i) );
	// <label> is an empty element

	// required attribute
	// * name
	if( !xml_label->hasAttribute( X_NAME ) )
	  {
            char mess[ SpkCompilerError::maxMessageLen() ];
            sprintf( mess, "Missing <%s::%s> attribute for the %d-th <%s>.", C_LABEL, C_NAME,
		     i, C_LABEL );
            SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
            throw e;
	  }
	const XMLCh* xml_name = xml_label->getAttribute( X_NAME );
	char * c_name = XMLString::transcode( xml_name );
	Symbol * name = table->find( c_name );

	if( name == Symbol::empty() )
	  {
            char mess[ SpkCompilerError::maxMessageLen() ];
            sprintf( mess, "\"%s\" is not registered in the symbol table.", c_name );
            SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
            throw e;
	  }
	delete c_name;

	// optional attribute
	// * synonym
	if( xml_label->hasAttribute( X_SYNONYM ) )
	  {
	    const XMLCh* xml_synonym = xml_label->getAttribute( X_SYNONYM );
	    char * c_synonym = XMLString::transcode( xml_synonym );
	    // register the synonym to the symbol table
	    name->synonym = string( c_synonym );
	    delete c_synonym;
	  }
      }
  }

  char valueDefault[] = "0.0";

  DOMNodeList * theta_list = pop_analysis->getElementsByTagName( X_THETA );
  if( theta_list->getLength() > 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Multiple <%s> elements found in the sourceML document.", C_THETA );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  if( theta_list->getLength() < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s> element.", C_THETA );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  DOMElement * theta = dynamic_cast<DOMElement*>( theta_list->item(0) );
  if( !theta->hasAttribute( X_LENGTH ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s::%s> attribute .", C_THETA, C_LENGTH );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const XMLCh* xml_theta_len = theta->getAttribute( X_LENGTH );
  myThetaLen = 0;
  if( !XMLString::textToBin( xml_theta_len, myThetaLen ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess,
	       "Invalid <%s::%s> attribute value: %s", 
	       C_THETA, C_LENGTH, XMLString::transcode( xml_theta_len ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  Symbol * sym_theta = table->insertNMVector( DefaultStr.THETA, myThetaLen );
  {
    //<in>
    DOMNodeList * theta_in_list = theta->getElementsByTagName( X_IN );
    if( theta_in_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess,
		 "Multiple <%s> child elements found under <%s>.", 
		 C_IN, C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( theta_in_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess,
		 "Missing <%s> child under <%s>.", 
		 C_IN, C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * theta_in = dynamic_cast<DOMElement*>( theta_in_list->item(0) );

    DOMNodeList * value_list = theta_in->getElementsByTagName( X_VALUE );
    if( myThetaLen != value_list->getLength() )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess,
		 "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		 C_VALUE, C_THETA, C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    for( int i=0; i<myThetaLen; i++ )
      {
	char str_val[128];
        bool isFixed = false;
	DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(i) );
	if( value->hasAttribute( X_FIXED ) )
	  {
	    const XMLCh* xml_fixed = value->getAttribute( X_FIXED );
	    isFixed = (XMLString::equals( xml_fixed, X_YES )? true : false );
	  }
	const XMLCh* xml_val = value->getFirstChild()->getNodeValue();
	if( XMLString::stringLen( xml_val ) == 0 )
	  strcpy( str_val, valueDefault );
	else
	  {
	    char * tmp_c_val = XMLString::transcode( xml_val );
	    strcpy( str_val, tmp_c_val );
	    delete tmp_c_val;
	  }
	sym_theta->initial[0][i] = str_val;
	sym_theta->fixed[0][i]   = isFixed;
      }
    //<low>
    DOMNodeList * theta_low_list = theta->getElementsByTagName( X_LOW );
    if( theta_low_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess,
		 "Multiple <%s> elements found under <%s>.", 
		 C_LOW, C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( theta_low_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess,
		 "Missing <%s> element under <%s>.", 
		 C_LOW, C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * theta_low = dynamic_cast<DOMElement*>( theta_low_list->item(0) );
    value_list = theta_low->getElementsByTagName( X_VALUE );
    if( myThetaLen != value_list->getLength() )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess,
		 "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		 C_VALUE, C_THETA, C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    for( int i=0; i<myThetaLen; i++ )
      {
	char str_val[128];
	DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh* xml_val = value->getFirstChild()->getNodeValue();
	if( XMLString::stringLen( xml_val ) == 0 )
	  strcpy( str_val, valueDefault );
	else
	  {
	    char * tmp_c_val = XMLString::transcode( xml_val );
	    strcpy( str_val, tmp_c_val );
	    delete tmp_c_val;
	  }
	if( sym_theta->fixed[0][i] )
	  sym_theta->lower[0][i] = sym_theta->initial[0][i];
        else
	  sym_theta->lower[0][i] = str_val;
      }

    //<up>
    DOMNodeList * theta_up_list = theta->getElementsByTagName( X_UP );
    if( theta_up_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess,
		 "Multiple <%s> elements found under <%s>.", 
		 C_UP, C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( theta_up_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess,
		 "Missing <%s> element under <%s>.", 
		 C_UP, C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * theta_up = dynamic_cast<DOMElement*>( theta_up_list->item(0) );
    value_list = theta_up->getElementsByTagName( X_VALUE );
    if( myThetaLen != value_list->getLength() )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess,
		 "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		 C_VALUE, C_THETA, C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    for( int i=0; i<myThetaLen; i++ )
      {
	char str_val[128];
	const XMLCh* xml_val = value_list->item(i)->getFirstChild()->getNodeValue();
	if( XMLString::stringLen( xml_val ) == 0 )
	  strcpy( str_val, valueDefault );
	else
	  {
	    char * tmp_c_val = XMLString::transcode( xml_val );
	    strcpy( str_val, tmp_c_val );
	    delete tmp_c_val;
	  }
        if( sym_theta->fixed[0][i] )
	  sym_theta->upper[0][i] = sym_theta->initial[0][i];
        else
	  sym_theta->upper[0][i] = str_val;
      }

    // step values
    for( int i=0; i<myThetaLen; i++ )
      {
	if( sym_theta->fixed[0][i] )
	  sym_theta->step[0][i] = "0.0";
	else
	  {
	    double tmp_dbl = fabs( atof( sym_theta->upper[0][i].c_str() ) 
				   - atof( sym_theta->lower[0][i].c_str() ) ) / 1000.0;
	    char tmp_char[256];
	    sprintf( tmp_char, "%f", tmp_dbl );
	    sym_theta->step[0][i] = string( tmp_char );
	  }
      }
  }

  DOMNodeList * omega_list = pop_analysis->getElementsByTagName( X_OMEGA );
  int nOmegaSpecs = omega_list->getLength();
  if( nOmegaSpecs > 1 )
    {
      // v0.1 supports only one (full) Omega specification
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Multiple <%s> elements found.",
	       C_OMEGA );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }  
  if( nOmegaSpecs < 1 )
    {
      // v0.1 supports only one (full) Omega specification
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s> element.",
	       C_OMEGA );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement * omega = dynamic_cast<DOMElement*>( omega_list->item(0) );
  if( !omega->hasAttribute( X_DIMENSION ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s::%s> attribute.",
	       C_OMEGA, C_DIMENSION );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const XMLCh* xml_omega_dim = omega->getAttribute( X_DIMENSION );
  if( !XMLString::textToBin( xml_omega_dim, myOmegaDim ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, 
	       "Invalid <%s::%s> attribute value: \"%s\".", 
	       C_OMEGA, C_DIMENSION, XMLString::transcode( xml_omega_dim ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  if( !omega->hasAttribute( X_STRUCT ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s::%s> attribute.",
	       C_OMEGA, C_STRUCT );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const XMLCh* xml_omega_struct = omega->getAttribute( X_STRUCT );
  if( XMLString::equals( xml_omega_struct, X_DIAGONAL ) )
    {
      myOmegaStruct = Symbol::DIAGONAL;
      myOmegaOrder = myOmegaDim;
    }
  else if( XMLString::equals( xml_omega_struct, X_BLOCK ) )
    {
      myOmegaStruct = Symbol::TRIANGLE;
      myOmegaOrder = series( 1, 1, myOmegaDim );
    }
  else
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, 
	       "Invalid <%s::%s> attribute value: \"%s\".", 
	       C_OMEGA, C_STRUCT, XMLString::transcode( xml_omega_struct ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  Symbol * sym_omega = table->insertNMMatrix( DefaultStr.OMEGA, myOmegaStruct, myOmegaDim );
  {
    //<in>
    DOMNodeList * omega_in_list = omega->getElementsByTagName( X_IN );
    if( omega_in_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "Multiple <%s> elements found under <%s>.",
		 C_OMEGA, C_IN );
	SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( omega_in_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "Missing <%s> element under <%s>.",
		 C_OMEGA, C_IN );
	SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * omega_in = dynamic_cast<DOMElement*>( omega_in_list->item(0) );

    //
    // Omega specification contains the minimal representation of the matrix.
    //
    //     /                 \
    //     |  a11  a12  a13  |
    // A = |  a21  a22  a23  |
    //     |  a31  a32  a33  |
    //     \                 /
    //
    // For full, the list contains the LOWER half in the row major order.
    // For diagonal, only the diagonal elements.
    //
    // If A is full, the user-given list will contain elements in the following order:
    // A' = { a11, a21, a22, a31, a32, a33 }
    //
    // xxxPredModel's constructor expects the list containing elements of
    // the UPPER half in the row major order.
    //
    // Thus, A has to be reorganized and stored in an internal array in the following order:
    // A" = { a11, a21, a31, a22, a32, a33 }
    //
    DOMNodeList * value_list = omega_in->getElementsByTagName( X_VALUE );
    if( myOmegaOrder != value_list->getLength() )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess,
		 "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		 C_VALUE, C_OMEGA, C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( myOmegaStruct == Symbol::TRIANGLE )
      {
	// First construct a full n by n matrix.
	valarray<string> omega_in_full ( myOmegaDim * myOmegaDim );
	valarray<bool> omega_fix_full( myOmegaDim * myOmegaDim );
	for( int i=0, cnt=0; i<myOmegaDim; i++ )
	  {
	    for( int j=0; j<=i; j++, cnt++ )
	      {
		char str_val[128];
		bool isFixed = false;
		DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(cnt) );
		if( value->hasAttribute( X_FIXED ) )
		  {
		    const XMLCh* xml_fixed = value->getAttribute( X_FIXED );
		    isFixed = (XMLString::equals( xml_fixed, X_YES )? true : false );
		  }
		const XMLCh* xml_val = value->getFirstChild()->getNodeValue();

		if( XMLString::stringLen( xml_val ) == 0 )
		  strcpy( str_val, valueDefault );
		else
		  {
		    char * tmp_c_val = XMLString::transcode( xml_val );
		    strcpy( str_val, tmp_c_val );
		    delete tmp_c_val;
		  }
		//omega_in_full[ j + i*dim ] = a[cnt]; // filling a lower triangle element
		omega_in_full [ i + j*myOmegaDim ] = str_val; // filling a upper triangle element
		omega_fix_full[ i + j*myOmegaDim ] = isFixed;
	      }
	  }
	// Then, extract only the upper half in the row major order.
	for( int i=0, cnt=0; i<myOmegaDim; i++ )
	  {
	    for( int j=i; j<myOmegaDim; j++, cnt++ )
	      {
		sym_omega->initial[0][cnt] = omega_in_full [ j + i * myOmegaDim ];
		sym_omega->fixed  [0][cnt] = omega_fix_full[ j + i * myOmegaDim ];
	      }
	  }
      }
    else // diagonal case
      {
	for( int i=0; i<myOmegaDim; i++ )
	  {
	    char str_val[128];
	    bool isFixed = false;
	    DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(i) );
	    if( value->hasAttribute( X_FIXED ) )
	      {
		const XMLCh* xml_fixed = value->getAttribute( X_FIXED );
		isFixed = (XMLString::equals( xml_fixed, X_YES )? true : false );
	      }
	    const XMLCh* xml_val = value->getFirstChild()->getNodeValue();

	    if( XMLString::stringLen( xml_val ) == 0 )
	      strcpy( str_val, valueDefault );
	    else
	      {
		char * tmp_c_val = XMLString::transcode( xml_val );
		strcpy( str_val, tmp_c_val );
		delete tmp_c_val;
	      }
	    sym_omega->initial[0][i] = str_val;
	    sym_omega->fixed[0][i]   = isFixed;
	  }
      }

  }

  DOMNodeList * sigma_list = pop_analysis->getElementsByTagName( X_SIGMA );
  int nSigmaSpecs = sigma_list->getLength();
  if( nSigmaSpecs > 1 )
    { 
      // v0.1 supports only one (full) Sigma specification
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Multiple <%s> elements found.",
	       C_SIGMA );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  if( nSigmaSpecs < 1 )
    { 
      // v0.1 supports only one (full) Sigma specification
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s> element.",
	       C_SIGMA );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement * sigma = dynamic_cast<DOMElement*>( sigma_list->item(0) );
  if( !sigma->hasAttribute( X_DIMENSION ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s::%s> attribute.",
	       C_SIGMA, C_DIMENSION );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const XMLCh* xml_sigma_dim = sigma->getAttribute( X_DIMENSION );
  if( !XMLString::textToBin( xml_sigma_dim, mySigmaDim ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, 
	       "Invalid <%s::%s> attribute value: \"%s\".", 
	       C_OMEGA, C_DIMENSION, XMLString::transcode( xml_sigma_dim ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  if( !sigma->hasAttribute( X_STRUCT ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s::%s> attribute.",
	       C_SIGMA, C_STRUCT );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const XMLCh* xml_sigma_struct = sigma->getAttribute( X_STRUCT );
  if( XMLString::equals( xml_sigma_struct, X_DIAGONAL ) )
    {
      mySigmaStruct = Symbol::DIAGONAL;
      mySigmaOrder = mySigmaDim;
    }
  else if( XMLString::equals( xml_sigma_struct, X_BLOCK ) )
    {
      mySigmaStruct = Symbol::TRIANGLE;
      mySigmaOrder = series( 1, 1, mySigmaDim );
    }
  else
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, 
	       "Invalid <%s::%s> attribute value: \"%s\".", 
	       C_SIGMA, C_STRUCT, XMLString::transcode( xml_sigma_struct ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  Symbol * sym_sigma = table->insertNMMatrix( DefaultStr.SIGMA, mySigmaStruct, mySigmaDim ); 
  {
    //<in>
    DOMNodeList * sigma_in_list = sigma->getElementsByTagName( X_IN );
    if( sigma_in_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "Multiple <%s> elements found under <%s>.",
		 C_SIGMA, C_IN );
	SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( sigma_in_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "Missing <%s> element under <%s>.",
		 C_SIGMA, C_IN );
	SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * sigma_in = dynamic_cast<DOMElement*>( sigma_in_list->item(0) );

    //
    // Sigma specification contains the minimal representation of the matrix.
    //
    //     /                 \
    //     |  a11  a12  a13  |
    // A = |  a21  a22  a23  |
    //     |  a31  a32  a33  |
    //     \                 /
    //
    // For full, the list contains the LOWER half in the row major order.
    // For diagonal, only the diagonal elements.
    //
    // If A is full, the user-given list will contain elements in the following order:
    // A' = { a11, a21, a22, a31, a32, a33 }
    //
    // xxxPredModel's constructor expects the list containing elements of
    // the UPPER half in the row major order.
    //
    // Thus, A has to be reorganized and stored in an internal array in the following order:
    // A" = { a11, a21, a31, a22, a32, a33 }
    //
    DOMNodeList * value_list = sigma_in->getElementsByTagName( X_VALUE );
    if( mySigmaOrder != value_list->getLength() )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess,
		 "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		 C_VALUE, C_SIGMA, C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( mySigmaStruct == Symbol::TRIANGLE )
      {
	// First construct a full n by n matrix.
	valarray<string> sigma_in_full ( mySigmaDim * mySigmaDim );
	valarray<bool>   sigma_fix_full( mySigmaDim * mySigmaDim );
	for( int i=0, cnt=0; i<mySigmaDim; i++ )
	  {
	    for( int j=0; j<=i; j++, cnt++ )
	      {
		char str_val[128];
		bool isFixed = false;
		DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(cnt) );
		if( value->hasAttribute( X_FIXED ) )
		  {
		    const XMLCh* xml_fixed = value->getAttribute( X_FIXED );
		    isFixed = (XMLString::equals( xml_fixed, X_YES )? true : false );
		  }
		const XMLCh* xml_val = value->getFirstChild()->getNodeValue();

		if( XMLString::stringLen( xml_val ) == 0 )
		  strcpy( str_val, valueDefault );
		else
		  {
		    char * tmp_c_val = XMLString::transcode( xml_val );
		    strcpy( str_val, tmp_c_val );
		    delete tmp_c_val;
		  }
		//sigma_in_full[ j + i*dim ] = str_val; // filling a lower triangle element
		sigma_in_full [ i + j*mySigmaDim ] = str_val; // filling a upper triangle element
		sigma_fix_full[ i + j*mySigmaDim ] = isFixed;
	      }
	  }
	// Then, extract only the upper half in the row major order.
	for( int i=0, cnt=0; i<mySigmaDim; i++ )
	  {
	    for( int j=i; j<mySigmaDim; j++, cnt++ )
	      {
		sym_sigma->initial[0][cnt] = sigma_in_full [ j + i * mySigmaDim ];
		sym_sigma->fixed  [0][cnt] = sigma_fix_full[ j + i * mySigmaDim ];
	      }
	  }
      }
    else // diagonal case
      {
	for( int i=0; i<mySigmaDim; i++ )
	  {
	    char str_val[128];
	    bool isFixed = false;
	    DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(i) );
	    if( value->hasAttribute( X_FIXED ) )
	      {
		const XMLCh* xml_fixed = value->getAttribute( X_FIXED );
		isFixed = (XMLString::equals( xml_fixed, X_YES )? true : false );
	      }
	    const XMLCh* xml_val = value->getFirstChild()->getNodeValue();

	    if( XMLString::stringLen( xml_val ) == 0 )
	      strcpy( str_val, valueDefault );
	    else
	      {
		char * tmp_c_val = XMLString::transcode( xml_val );
		strcpy( str_val, tmp_c_val );
		delete tmp_c_val;
	      }
	    sym_sigma->initial[0][i] = str_val;
	    sym_sigma->fixed[0][i]   = isFixed;
	  }
      }
  }
  
  //---------------------------------------------------------------------------------------
  // eta
  // NOTE: eta is not given by the user.  
  // eta's initial estimate is set to 0.0 automatically.
  //
  //---------------------------------------------------------------------------------------
  myEtaLen = myOmegaDim;
  char etaDefault[] = "0.0";
  Symbol * sym_eta = table->insertNMVector( DefaultStr.ETA, myEtaLen );
  for( int i=0; i<myEtaLen; i++ ) sym_eta->initial[0][i] = etaDefault;
  sym_eta->fixed[0] = false;

  //---------------------------------------------------------------------------------------
  // Sigma 
  // Sigma is the covariance of EPS: thus, 
  // the order of Sigma is the length of EPS vector.
  //---------------------------------------------------------------------------------------
  myEpsLen = mySigmaDim;
  char epsDefault[] = "0.0";
  Symbol * sym_eps = table->insertNMVector( DefaultStr.EPS, myEpsLen );
  for( int i=0; i<myEpsLen; i++ ) sym_eps->initial[0][i] = epsDefault;
  sym_eta->fixed[0] = false;

  //---------------------------------------------------------------------------------------
  // (Optional) Statistics elements
  //---------------------------------------------------------------------------------------
  // <description>
  // <simulation>
  // <pop_stat>
  DOMNodeList * descriptions = pop_analysis->getElementsByTagName( X_DESCRIPTION );
  myDescription = new char[ 128 ];
  strcpy( myDescription, "--- This file is generated by SPK Compiler ---" );
  if( descriptions->getLength() > 0 )
    {
      const XMLCh* description = descriptions->item(0)->getFirstChild()->getNodeValue();
      if( XMLString::stringLen( description ) > 0 )
	{
	  delete [] myDescription;
	  myDescription = XMLString::transcode( description );
	}
    }

  myIsSimulate = false;
  mySeed       = 0;
  DOMNodeList * simulations = pop_analysis->getElementsByTagName( X_SIMULATION );
  if( simulations->getLength() > 0 )
    {
      if( simulations->getLength() > 1 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Multiple <%s> elements found.",
		   C_SIMULATION );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      myIsSimulate = true;
      DOMElement* simulation = dynamic_cast<DOMElement*>( simulations->item(0) );
      if( !simulation->hasAttribute( X_SEED ) )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Missing <%s::%s> attribute.",
		   C_SIMULATION, C_SEED );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      const XMLCh* xml_seed = simulation->getAttribute( X_SEED );
      if( !XMLString::textToBin( xml_seed, mySeed ) )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Invalid <%s::%s> attribute value: \"%s\".", 
		   C_SIMULATION, C_SEED, XMLString::transcode(xml_seed) );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}

      if( simulation->hasAttribute( X_SUBPROBLEMS ) )
	{
	  const XMLCh* xml_subproblems = simulation->getAttribute( X_SUBPROBLEMS );
	  if( !XMLString::textToBin( xml_subproblems, mySubproblemsN ) )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      sprintf( mess, "Invalid <%s::%s> attribute value: \"%s\".", 
		       C_SIMULATION, C_SUBPROBLEMS, XMLString::transcode(xml_subproblems) );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	      throw e;
	    }
	}
    }

  DOMNodeList * pop_stat_list = pop_analysis->getElementsByTagName( X_POP_STAT );

  // Statistics computation can be done only when the parameter estimation
  // is requested.
  if( pop_stat_list->getLength() > 0 && myIsEstimate )
    {
      if( pop_stat_list->getLength() > 1 )
	{
          char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Multiple <%s> elements found.", 
	           C_POP_STAT );
          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
          throw e;
	}
      DOMElement * pop_stat = dynamic_cast<DOMElement*>( pop_stat_list->item(0) );
      if( !pop_stat->hasAttribute( X_COVARIANCE_FORM ) && myIsStat )
	{
          char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Missing <%s::%s> attribute.", 
	           C_POP_STAT, C_COVARIANCE_FORM );
          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
          throw e;
	}
      const XMLCh* cov_form = pop_stat->getAttribute( X_COVARIANCE_FORM ); // r, rsr, s
      if( XMLString::equals( cov_form, X_COV_S ) )
	myCovForm = "S";
      else if( XMLString::equals( cov_form, X_COV_RSR ) )
	myCovForm = "RSR";
      else if( XMLString::equals( cov_form, X_COV_R ) )
	myCovForm = "R";
      else
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Invalid <%s::%s> attribute value: \"%s\".", 
		   C_POP_STAT, C_COVARIANCE_FORM, XMLString::transcode( cov_form )  );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}

      if( pop_stat->hasAttribute( X_IS_ERR_OUT ) )
	{
	  const XMLCh* xml_stderr = pop_stat->getAttribute( X_IS_ERR_OUT );
	  myIsStderr = (XMLString::equals( xml_stderr, X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( X_IS_CORR_OUT ) )
	{
	  const XMLCh* xml_correlation = pop_stat->getAttribute( X_IS_CORR_OUT );
	  myIsCorrelation = (XMLString::equals( xml_correlation, X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( X_IS_COV_OUT ) )
	{
	  const XMLCh* xml_cov = pop_stat->getAttribute( X_IS_COV_OUT );
	  myIsCov = (XMLString::equals( xml_cov, X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( X_IS_INV_COV_OUT ) )
	{
	  const XMLCh* xml_inv_cov = pop_stat->getAttribute( X_IS_INV_COV_OUT );
	  myIsInvCov = (XMLString::equals( xml_inv_cov, X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( X_IS_CONF_OUT ) )
	{
	  const XMLCh* xml_conf = pop_stat->getAttribute( X_IS_CONF_OUT );
	  myIsConfidence = (XMLString::equals( xml_conf, X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( X_IS_COEF_OUT ) )
	{
	  const XMLCh* xml_coef = pop_stat->getAttribute( X_IS_COEF_OUT );
	  myIsCoefficient = (XMLString::equals( xml_coef, X_YES )? true : false );
	}
    }
  else
    {
      myIsStat = false;
      myIsStderr = false;
      myIsCorrelation = false;
      myIsCov = false;
      myIsInvCov = false;
      myIsConfidence = false;
      myIsCoefficient = false;
    }

  myIsStat = myIsStderr 
    || myIsCorrelation 
    || myIsCov 
    || myIsInvCov 
    || myIsConfidence 
    || myIsCoefficient;

  return;
}
void NonmemTranslator::parseIndAnalysis( DOMElement* ind_analysis )
{
  //---------------------------------------------------------------------------------------
  // Parse <simulate> if exists.  There's a chance in which only data simulation
  // is requested but not estimation.
  //---------------------------------------------------------------------------------------
  myIsSimulate = false;
  mySeed = 0;
  DOMNodeList * simulations = ind_analysis->getElementsByTagName( X_SIMULATION );
  if( simulations->getLength() > 0 )
    {
      if( simulations->getLength() > 1 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Multiple <%s> elements found.",
		   C_SIMULATION );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      if( simulations->getLength() < 1 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Missing <%s> element.",
		   C_SIMULATION );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      myIsSimulate = true;
      DOMElement* simulation = dynamic_cast<DOMElement*>( simulations->item(0) );
      if( !simulation->hasAttribute( X_SEED ) )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Missing <%s::%s> attribute.",
		   C_SIMULATION, C_SEED );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      const XMLCh* xml_seed = simulation->getAttribute( X_SEED );
      if( !XMLString::textToBin( xml_seed, mySeed ) )
	{
          char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Invalid <%s::%s> attribute value: \"%s\".", 
		   C_SIMULATION, C_SEED, XMLString::transcode( xml_seed ) );
          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
          throw e;
	}
   
      if( simulation->hasAttribute( X_SUBPROBLEMS ) )
	{
	  const XMLCh* xml_subproblems = simulation->getAttribute( X_SUBPROBLEMS );
	  if( !XMLString::textToBin( xml_subproblems, mySubproblemsN ) )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      sprintf( mess, "Invalid <%s::%s> attribute value: \"%s\".", 
		       C_SIMULATION, C_SUBPROBLEMS, XMLString::transcode(xml_subproblems) );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	      throw e;
	    }
	}
    }

  //---------------------------------------------------------------------------------------
  // <pop_analysis> Required attributes
  //---------------------------------------------------------------------------------------
  // * is_estimation = {yes, no}
  if( !ind_analysis->hasAttribute( X_IS_ESTIMATION ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s::%s> attribute.", C_IND_ANALYSIS, C_IS_ESTIMATION );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  const XMLCh * xml_is_estimation = ind_analysis->getAttribute( X_IS_ESTIMATION );
  if( XMLString::equals( xml_is_estimation, X_YES ) )
    myIsEstimate = true;
  else if ( XMLString::equals( xml_is_estimation, X_NO ) )
    myIsEstimate = false;
  else
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, 
	       "Invalid <%s::%s> attribute value: \"%s\".", 
	       C_IND_ANALYSIS, 
	       C_IS_ESTIMATION, 
	       XMLString::transcode( xml_is_estimation ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }


  myIndTraceLevel = 1;
  myPopTraceLevel = 1;

  //---------------------------------------------------------------------------------------
  // Optional attributes
  //---------------------------------------------------------------------------------------
  // * mitr   --- required when is_estimation == "yes"
  // * is_restart = {yes, "no"}
  // * sig_digits = 3
  myIndMitr   = 0;
  mySigDigits = 3;

  const XMLCh * xml_is_restart;
  if( ind_analysis->hasAttribute( X_SIG_DIGITS ) )
    {
      xml_is_restart = ind_analysis->getAttribute( X_IS_RESTART );
      myIsRestart = ( XMLString::equals( xml_is_restart, X_YES )? true : false );
    }
  if( myIsEstimate )
    {
      const XMLCh* xml_mitr;
      if( ind_analysis->hasAttribute( X_MITR ) )
	{
	  xml_mitr = ind_analysis->getAttribute( X_MITR );
	  if( !XMLString::textToBin( xml_mitr, myIndMitr ) )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      sprintf( mess, "Invalid <%s::%s> attribute value: \"%s\".", 
		       C_IND_ANALYSIS, C_MITR, XMLString::transcode(xml_mitr) );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	      throw e;
	    }
	}
      const XMLCh* xml_sig_digits;
      if( ind_analysis->hasAttribute( X_SIG_DIGITS ) )
	{
	  xml_sig_digits = ind_analysis->getAttribute( X_SIG_DIGITS );
	  if( !XMLString::textToBin( xml_sig_digits, mySigDigits ) )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      sprintf( mess, "Invalid <%s::%s> attribute value: \"%s\".", 
		       C_IND_ANALYSIS, C_SIG_DIGITS, XMLString::transcode(xml_sig_digits) );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	      throw e;
	    }
	  if( !( mySigDigits > 0 && mySigDigits < 9 ) )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      sprintf( mess, "Invalid <%s::%s> attribute value: \"%s\".  Valid values: (1-8).", 
		       C_IND_ANALYSIS, C_SIG_DIGITS, mySigDigits );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	      throw e;
	    }
	  myIndEpsilon = pow( 10.0, -(mySigDigits + 1.0) );
	}
    }
  
  //---------------------------------------------------------------------------------------
  // Required elements
  //---------------------------------------------------------------------------------------
  // <data_labels>
  // <theta>
  // <omega>+
  DOMNodeList * data_labels_list = ind_analysis->getElementsByTagName( X_DATA_LABELS );
  if( data_labels_list->getLength() > 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Multiple <%s> elements found under <%s>.",
	       C_DATA_LABELS, C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  if( data_labels_list->getLength() < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s> element under <%s>.",
	       C_DATA_LABELS, C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement * data_labels = dynamic_cast<DOMElement*>( data_labels_list->item(0) );
  {

    DOMNodeList * labels = data_labels->getElementsByTagName( X_LABEL );
    int nLabels = labels->getLength();
    if( nLabels < 1 )
      {
        char mess[ SpkCompilerError::maxMessageLen() ];
        sprintf( mess, "Missing <%s> element under <%s>.",
                 C_LABEL, C_DATA_LABELS );
        SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
        throw e;
      }
    for( int i=0; i<nLabels; i++ )
      {
	DOMElement * xml_label = dynamic_cast<DOMElement*>( labels->item(i) );
	// <label> is an empty element

	// required attribute
	// * name
	if( !xml_label->hasAttribute( X_NAME ) )
	  {
            char mess[ SpkCompilerError::maxMessageLen() ];
            sprintf( mess, "Missing <%s::%s> attribute.",
                     C_LABEL, C_NAME );
            SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
            throw e;
	  }
	const XMLCh* xml_name = xml_label->getAttribute( X_NAME );
	char * c_name = XMLString::transcode( xml_name );

	// optional attribute
	// * synonym
	const XMLCh* xml_synonym;
	char * c_synonym = NULL;
	if( xml_label->hasAttribute( X_SYNONYM ) )
	  {
	    xml_synonym = xml_label->getAttribute( X_SYNONYM );
	    c_synonym = XMLString::transcode( xml_synonym );
	  }

	Symbol * name = table->findi( c_name );

	// "name" may not be one of the official data item labels.
	// For example, "DV" may be used as an official data label
	// in the data set (ie. dataML) but used as an alias
	// to "CP" so that "CP" appears as the item title in
	// the display table/scatterplot.
	// Check if this <label> has label::synonym attribute.
	// If it does, check if it exists in the symbol table.
	if( name == Symbol::empty() )
	  {
	    if( c_synonym == NULL )
	      {
                char mess[ SpkCompilerError::maxMessageLen() ];
                sprintf( mess, "\"%s\" is not found in the symbol table as either the name or the alias.", c_name );
                SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, 
                                        __LINE__, __FILE__ );
                throw e;
	      }
	    Symbol * synonym = table->findi( c_synonym );
	    if( synonym == Symbol::empty() )
	      {
                char mess[ SpkCompilerError::maxMessageLen() ];
                sprintf( mess, "\"%s\" is not found in the symbol table as either the name or the alias.", c_name );
                SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, 
                                        __LINE__, __FILE__ );
                throw e;
	      }
	    synonym->synonym = c_name;
	  }

	else if( c_synonym != NULL )
	  {
	    // register the synonym to the symbol table
	    name->synonym = c_synonym;
	  }
	delete c_name;
	delete c_synonym;
      }
  }

  // THETA
  DOMNodeList * theta_list = ind_analysis->getElementsByTagName( X_THETA );
  if( theta_list->getLength() > 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Multiple <%s> elements found under <%s>.",
	       C_THETA, C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  if( theta_list->getLength() < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s> element under <%s>.",
	       C_THETA, C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement * theta = dynamic_cast<DOMElement*>( theta_list->item(0) );
  if( !theta->hasAttribute( X_LENGTH ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s::%s> attribute.",
	       C_THETA, C_LENGTH );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  const XMLCh* xml_theta_len = theta->getAttribute( X_LENGTH );
  myThetaLen = 0;
  if( !XMLString::textToBin( xml_theta_len, myThetaLen ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Invalid <%s::%s> attribute value: \"%s\"", 
	       C_THETA, C_LENGTH, XMLString::transcode( xml_theta_len ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  Symbol * sym_theta = table->insertNMVector( DefaultStr.THETA, myThetaLen );
  {
    //<in>
    DOMNodeList * theta_in_list = theta->getElementsByTagName( X_IN );
    if( theta_in_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "Multiple <%s> element found under <%s>.",
		 C_THETA, C_IN );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( theta_in_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "Missing <%s> element under <%s>.",
		 C_THETA, C_IN );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * theta_in = dynamic_cast<DOMElement*>( theta_in_list->item(0) );

    DOMNodeList * value_list = theta_in->getElementsByTagName( X_VALUE );
    if( myThetaLen != value_list->getLength() )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "The number of <%s> elements under <%s> does not match with the <%s::%s> attribute value.",
		 C_VALUE, C_IN, C_THETA, C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    for( int i=0; i<myThetaLen; i++ )
      {
	char str_val[128];
        bool isFixed = false;
	DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(i) );
        const XMLCh* xml_fixed = value->getAttribute( X_FIXED );
	if( XMLString::stringLen( xml_fixed ) != 0 )
	  {
	    isFixed = (XMLString::equals( xml_fixed, X_YES )? true : false );
	  }
	const XMLCh* xml_val = value->getFirstChild()->getNodeValue();
	if( XMLString::stringLen( xml_val ) == 0 )
	  strcpy( str_val, "0.0" );
	else
	  {
	    char * tmp_c_val = XMLString::transcode( xml_val );
	    strcpy( str_val, tmp_c_val );
	    delete tmp_c_val;
	  }
	sym_theta->initial[0][i] = str_val;
	sym_theta->fixed[0][i]   = isFixed;
      }
    //<low>
    DOMNodeList * theta_low_list = theta->getElementsByTagName( X_LOW );
    if( theta_low_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "Multiple <%s> elements found under <%s>.",
		 C_LOW, C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( theta_low_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "Missing <%s> element  under <%s>.",
		 C_LOW, C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * theta_low = dynamic_cast<DOMElement*>( theta_low_list->item(0) );
    value_list = theta_low->getElementsByTagName( X_VALUE );
    if( myThetaLen != value_list->getLength() )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "The number of <%s> elements under <%s> tag does not match with the <%s::%s> attribute value.",
		 C_VALUE, C_LOW, C_THETA, C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    for( int i=0; i<myThetaLen; i++ )
      {
	char str_val[128];
	DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(i) );
	const XMLCh* xml_val = value->getFirstChild()->getNodeValue();
	if( XMLString::stringLen( xml_val ) == 0 )
	  strcpy( str_val, "0.0" );
	else
	  {
	    char * tmp_c_val = XMLString::transcode( xml_val );
	    strcpy( str_val, tmp_c_val );
	    delete tmp_c_val;
	  }
	sym_theta->lower[0][i] = str_val;
      }

    //<up>
    DOMNodeList * theta_up_list = theta->getElementsByTagName( X_UP );
    if( theta_up_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "Multiple <%s> elements found under <%s>.",
		 C_UP, C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( theta_up_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "Missing <%s> element under <%s>.",
		 C_UP, C_THETA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * theta_up = dynamic_cast<DOMElement*>( theta_up_list->item(0) );
    value_list = theta_up->getElementsByTagName( X_VALUE );
    if( myThetaLen != value_list->getLength() )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "The number of <%s> elements under <%s> tag does not match with the <%s::%s> attribute value.",
		 C_VALUE, C_UP, C_THETA, C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    for( int i=0; i<myThetaLen; i++ )
      {
	char str_val[128];
	const XMLCh* xml_val = value_list->item(i)->getFirstChild()->getNodeValue();
	if( XMLString::stringLen( xml_val ) == 0 )
	  strcpy( str_val, "0.0" );
	else
	  {
	    char * tmp_c_val = XMLString::transcode( xml_val );
	    strcpy( str_val, tmp_c_val );
	    delete tmp_c_val;
	  }
	sym_theta->upper[0][i] = str_val;
      }

    // step values
    for( int i=0; i<myThetaLen; i++ )
      {
	double tmp_dbl = fabs( ( atof( sym_theta->upper[0][i].c_str() ) - atof( sym_theta->lower[0][i].c_str() ) ) ) / 1000.0;
	char tmp_char[256];
	sprintf( tmp_char, "%f", tmp_dbl );
	sym_theta->step[0][i] = string( tmp_char );
      }
  }

  // OMEGA
  DOMNodeList * omega_list = ind_analysis->getElementsByTagName( X_OMEGA );
  int nOmegaSpecs = omega_list->getLength();
  if( nOmegaSpecs > 1 )
    {
      // v0.1 supports only one Omega specification
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Multiple <%s> elements found under <%s>.",
	       C_OMEGA, C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  if( nOmegaSpecs < 1 )
    {
      // v0.1 supports only one Omega specification
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s> elements under <%s>.",
	       C_OMEGA, C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement * omega = dynamic_cast<DOMElement*>( omega_list->item(0) );
  if( !omega->hasAttribute( X_DIMENSION ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s::%s> attribute.",
	       C_OMEGA, C_DIMENSION );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const XMLCh* xml_omega_dim = omega->getAttribute( X_DIMENSION );
  if( !XMLString::textToBin( xml_omega_dim, myOmegaDim ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Invalid <%s::%s> attribute value: \"%s\".", 
	       C_OMEGA, C_DIMENSION, XMLString::transcode(xml_omega_dim) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  if( !omega->hasAttribute( X_STRUCT ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s::%s> attribute.",
	       C_OMEGA, C_STRUCT );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  // In Individual analysis, Omega is diagonal only.
  const XMLCh* xml_omega_struct = omega->getAttribute( X_STRUCT );
  if( !XMLString::equals( xml_omega_struct, X_DIAGONAL ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "For the individual analysis, Omega can be only diagonal.  %s is invalid.",
	       XMLString::transcode( xml_omega_struct ) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  myOmegaStruct = Symbol::DIAGONAL;
  myOmegaOrder = myOmegaDim;

  Symbol * sym_omega = table->insertNMMatrix( DefaultStr.OMEGA, myOmegaStruct, myOmegaDim );
  {
    //<in>
    DOMNodeList * omega_in_list = omega->getElementsByTagName( X_IN );
    if( omega_in_list->getLength() > 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "Multiple <%s> elements found under <%s>.",
		 C_IN, C_OMEGA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( omega_in_list->getLength() < 1 )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess, "Missing <%s> element under <%s>.",
		 C_IN, C_OMEGA );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    DOMElement * omega_in = dynamic_cast<DOMElement*>( omega_in_list->item(0) );
    //
    // Omega specification contains the minimal representation of the matrix.
    //
    //     /                 \
    //     |  a11  a12  a13  |
    // A = |  a21  a22  a23  |
    //     |  a31  a32  a33  |
    //     \                 /
    //
    // For full, the list contains the LOWER half in the row major order.
    // For diagonal, only the diagonal elements.
    //
    // If A is full, the user-given list will contain elements in the following order:
    // A' = { a11, a21, a22, a31, a32, a33 }
    //
    // xxxPredModel's constructor expects the list containing elements of
    // the UPPER half in the row major order.
    //
    // Thus, A has to be reorganized and stored in an internal array in the following order:
    // A" = { a11, a21, a31, a22, a32, a33 }
    //
    char valueDefault[] = "0.0";
    DOMNodeList * value_list = omega_in->getElementsByTagName( X_VALUE );
    if( myOmegaOrder != value_list->getLength() )
      {
	char mess[ SpkCompilerError::maxMessageLen() ];
	sprintf( mess,
		 "The number of <%s> elements does not match with the <%s::%s> attribute value.", 
		 C_VALUE, C_OMEGA, C_LENGTH );
	SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	throw e;
      }
    if( myOmegaStruct == Symbol::TRIANGLE )
      {
	// First construct a full n by n matrix.
	valarray<string> omega_in_full ( myOmegaDim * myOmegaDim );
	valarray<bool> omega_fix_full( myOmegaDim * myOmegaDim );
	for( int i=0, cnt=0; i<myOmegaDim; i++ )
	  {
	    for( int j=0; j<=i; j++, cnt++ )
	      {
		char str_val[128];
		bool isFixed = false;
		DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(cnt) );
		if( value->hasAttribute( X_FIXED ) )
		  {
		    const XMLCh* xml_fixed = value->getAttribute( X_FIXED );
		    isFixed = (XMLString::equals( xml_fixed, X_YES )? true : false );
		  }
		const XMLCh* xml_val = value->getFirstChild()->getNodeValue();

		if( XMLString::stringLen( xml_val ) == 0 )
		  strcpy( str_val, valueDefault );
		else
		  {
		    char * tmp_c_val = XMLString::transcode( xml_val );
		    strcpy( str_val, tmp_c_val );
		    delete tmp_c_val;
		  }
		//omega_in_full[ j + i*dim ] = a[cnt]; // filling a lower triangle element
		omega_in_full [ i + j*myOmegaDim ] = str_val; // filling a upper triangle element
		omega_fix_full[ i + j*myOmegaDim ] = isFixed;
	      }
	  }
	// Then, extract only the upper half in the row major order.
	for( int i=0, cnt=0; i<myOmegaDim; i++ )
	  {
	    for( int j=i; j<myOmegaDim; j++, cnt++ )
	      {
		sym_omega->initial[0][cnt] = omega_in_full [ j + i * myOmegaDim ];
		sym_omega->fixed  [0][cnt] = omega_fix_full[ j + i * myOmegaDim ];
	      }
	  }
      }
    else // diagonal case
      {
	for( int i=0; i<myOmegaDim; i++ )
	  {
	    char str_val[128];
	    bool isFixed = false;
	    DOMElement * value = dynamic_cast<DOMElement*>( value_list->item(i) );
	    if( value->hasAttribute( X_FIXED ) )
	      {
		const XMLCh* xml_fixed = value->getAttribute( X_FIXED );
		isFixed = (XMLString::equals( xml_fixed, X_YES )? true : false );
	      }
	    const XMLCh* xml_val = value->getFirstChild()->getNodeValue();

	    if( XMLString::stringLen( xml_val ) == 0 )
	      strcpy( str_val, valueDefault );
	    else
	      {
		char * tmp_c_val = XMLString::transcode( xml_val );
		strcpy( str_val, tmp_c_val );
		delete tmp_c_val;
	      }
	    sym_omega->initial[0][i] = str_val;
	    sym_omega->fixed[0][i]   = isFixed;
	  }
      }
  }

  // ETA
  // Eta plays the same role as EPS as in the population analysis.
  // Variance of data?
  char etaDefault[] = "0.0";
  //  myEtaLen = myOmegaOrder;
  myEtaLen = myOmegaDim;
  Symbol * sym_eta = table->insertNMVector( DefaultStr.ETA, myEtaLen );
  for( int i=0; i<myEtaLen; i++ ) sym_eta->initial[0][i] = etaDefault;
  sym_eta->fixed[0] = false;
  
  //---------------------------------------------------------------------------------------
  // Optional elements
  //---------------------------------------------------------------------------------------
  // <description>  --- ignore!
  // <ind_stat>
  // <pop_stat>
  DOMNodeList * descriptions = ind_analysis->getElementsByTagName( X_DESCRIPTION );
  myDescription = new char[ 128 ];
  strcpy( myDescription, "--- This file is generated by SPK Compiler ---" );

  if( descriptions->getLength() > 0 )
    {
      const XMLCh* description = descriptions->item(0)->getFirstChild()->getNodeValue();
      if( XMLString::stringLen( description ) > 0 )
	{
	  delete [] myDescription;
	  myDescription = XMLString::transcode( description );
	}
    }

  DOMNodeList * ind_stat_list = ind_analysis->getElementsByTagName( X_IND_STAT );

  // Statistics computation can be done only when the parameter estimation
  // is requested.
  if( ind_stat_list->getLength() > 0 && myIsEstimate )
    {
      if( ind_stat_list->getLength() > 1 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Multiple <%s> elements found.",
		   C_IND_STAT );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      if( ind_stat_list->getLength() < 1 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Missing <%s> element.",
		   C_IND_STAT );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      DOMElement * ind_stat = dynamic_cast<DOMElement*>( ind_stat_list->item(0) );
      const XMLCh* xml_stderr = ind_stat->getAttribute( X_IS_ERR_OUT );
      const XMLCh* cov_form = X_COV_H;
      if( ind_stat->hasAttribute( X_COVARIANCE_FORM ) )
          cov_form = ind_stat->getAttribute( X_COVARIANCE_FORM );
      if( XMLString::equals( cov_form, X_COV_S ) )
	myCovForm = "S";
      else if( XMLString::equals( cov_form, X_COV_RSR ) )
	myCovForm = "RSR";
      else if( XMLString::equals( cov_form, X_COV_R ) )
	myCovForm = "R";
      else if( XMLString::equals( cov_form, X_COV_HSH ) )
	myCovForm = "HSH";
      else if( XMLString::equals( cov_form, X_COV_H ) )
	myCovForm = "H";
      else
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Invalid <%s::%s> attribute value: \"%s\".", 
		   C_IND_STAT, C_COVARIANCE_FORM, XMLString::transcode( cov_form )  );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
	  throw e;
	}
      if( XMLString::stringLen( xml_stderr ) > 0 )
	{
	  myIsStderr = (XMLString::equals( xml_stderr, X_YES )? true : false );
	}
      const XMLCh* xml_correlation = ind_stat->getAttribute( X_IS_CORR_OUT );
      if( XMLString::stringLen( xml_correlation ) > 0 )
	{
	  myIsCorrelation = (XMLString::equals( xml_correlation, X_YES )? true : false );
	}
      const XMLCh* xml_cov = ind_stat->getAttribute( X_IS_COV_OUT );
      if( XMLString::stringLen( xml_cov ) > 0 )
	{
	  myIsCov = (XMLString::equals( xml_cov, X_YES )? true : false );
	}
      const XMLCh* xml_inv_cov = ind_stat->getAttribute( X_IS_INV_COV_OUT );
      if( XMLString::stringLen( xml_inv_cov ) > 0 )
	{
	  myIsInvCov = (XMLString::equals( xml_inv_cov, X_YES )? true : false );
	}
      const XMLCh* xml_conf = ind_stat->getAttribute( X_IS_CONF_OUT );
      if( XMLString::stringLen( xml_conf ) > 0 )
	{
	  myIsConfidence = (XMLString::equals( xml_conf, X_YES )? true : false );
	}
      const XMLCh* xml_coef = ind_stat->getAttribute( X_IS_COEF_OUT );
      if( XMLString::stringLen( xml_coef ) > 0 )
	{
	  myIsCoefficient = (XMLString::equals( xml_coef, X_YES )? true : false );
	}
    }
  else
    {
      myIsStat = false;
      myIsStderr = false;
      myIsCorrelation = false;
      myIsCov = false;
      myIsInvCov = false;
      myIsConfidence = false;
      myIsCoefficient = false;
    }

  myIsStat = myIsStderr 
    || myIsCorrelation 
    || myIsCov 
    || myIsInvCov 
    || myIsConfidence 
    || myIsCoefficient;

  return;
}
void NonmemTranslator::parsePred( DOMElement * pred )
{
  char * c_equations = NULL;
  const XMLCh* xml_pred_def = pred->getFirstChild()->getNodeValue();
  int size = XMLString::stringLen( xml_pred_def );

  if( size > 0 )
    c_equations = XMLString::transcode( xml_pred_def );

  nm_in = fopen( fPredEqn_fortran, "w" );
  fprintf( nm_in, "%s", c_equations );
  fclose( nm_in );
  delete c_equations;

  nm_in = fopen( fPredEqn_fortran, "r" );
  gSpkExpOutput = fopen( fPredEqn_cpp, "w" );
  gSpkExpSymbolTable = table;
  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpErrorMessages = new char[ SpkCompilerError::maxMessageLen()-50 ];
  strcpy( gSpkExpErrorMessages, "" );

  nm_parse();

  if( gSpkExpErrors > 0 )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      sprintf( m, "Syntax error(s) found in PRED definition.\n%s", 
	       gSpkExpErrorMessages );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, 
			      m, __LINE__, __FILE__ );
      throw e;
    }

  fclose( nm_in );
  fclose( gSpkExpOutput );
  remove( fPredEqn_fortran );
}
int NonmemTranslator::countStrInLhs( const char* str, const char * equations )
{
  // Look only on LHS of equations
  int n = 0;
  char * dup = strdup( equations );
  char * line = NULL;
  char * lhs  = NULL;
  line = strtok( dup, "=" );
  while( line != NULL )
  {
     if( strstr( line, str ) != NULL )
        ++n;
     line = strtok( NULL, "\n" );
     line = strtok( NULL, "=" );
  }
  delete [] dup;
  return n;

}

//****************************************************************
// Following NONMEM specification, parse modules in the
// order of:
//    1. COMP_MODEL ($MODEL)
//    1. PK         ($PK)
//    2. DIFFEQN    ($DES)
//    3. ERROR      ($ERROR)
//****************************************************************
void NonmemTranslator::parseAdvan(
				  enum MODEL_SPEC   advan,
				  enum TRANS        trans,
				  const DOMElement* model )
{
  DOMNodeList * comp_models = model->getElementsByTagName( X_COMP_MODEL );
  DOMNodeList * pks         = model->getElementsByTagName( X_PK );
  DOMNodeList * errors      = model->getElementsByTagName( X_ERROR );
  DOMNodeList * diffeqns    = model->getElementsByTagName( X_DIFFEQN );
  DOMElement  * comp_model  = NULL;
  DOMElement  * diffeqn     = NULL;
  DOMElement  * pk          = NULL;
  DOMElement  * error       = NULL;

  //
  // Get <pk> and <error> which are needed for any modeling framework besides $PRED.
  //
  // REVISIT Sachiko 05/12/2005
  // There are times when $PK is not specified.  
  // That's when the LINK is spefied for each compartment in $MODEL.
  // For now, let's ignore that style of specification and
  // mandate $PK.
  //
  if( pks->getLength() < 1 || errors->getLength() < 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <pk> and/or <error>!" );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
			      mess, __LINE__, __FILE__ );
      throw e;
    }
  
  pk      = dynamic_cast<DOMElement*>( pks->item(0) );
  error   = dynamic_cast<DOMElement*>( errors->item(0) );
  
  // ADVAN 5-9 needs the compartmental model definiton, <comp_model>.
  if( myModelSpec >= ADVAN5 && myModelSpec <= ADVAN9 )
    {
      if( comp_models->getLength() < 1 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "<comp_model> must be specified when ADVAN 5-9 is used!" );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
				  mess, __LINE__, __FILE__ );
	      throw e;
	}
      comp_model = dynamic_cast<DOMElement*>( comp_models->item(0) );

      // further, ADVAN 6, 8 and 9 needs the differential equations, <diffeqn>.
      if( myModelSpec == 6 || myModelSpec == 8 || myModelSpec == 9 )
	{
	  if( diffeqns->getLength() < 1 )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      sprintf( mess, "<diffeqn> must be specified when ADVAN 6, 8 or 9 is used!" );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
				      mess, __LINE__, __FILE__ );
	      throw e;
	    }
	  diffeqn = dynamic_cast<DOMElement*>( diffeqns->item(0) );
	}
    }
  // Other ADVANs don't need more than <pk> and <error>
  else
    {
      // nothing to do.
    }

  switch( advan )
    {
    case ADVAN6: // nonlinear ODE model.
      parseAdvan6( trans, comp_model, pk, diffeqn, error );
      break;
    default:
      break;
    }
}
void NonmemTranslator::parseAdvan6( enum TRANS trans,
		    const xercesc::DOMElement* comp_model,
		    const xercesc::DOMElement* pk,
		    const xercesc::DOMElement* diffeqn,
		    const xercesc::DOMElement* error )
{
  parseCompModel( comp_model );
  parsePK( pk );
  parseDiffEqn( diffeqn );
  parseError( error );
}
void NonmemTranslator::parseCompModel( const DOMElement* comp_model )
{
}
void NonmemTranslator::parsePK( const DOMElement* pk )
{
  //============================
  // <pk>
  //============================
  char * c_pk_def = NULL;
  const XMLCh* xml_pk_def = pk->getFirstChild()->getNodeValue();
  int pk_size = XMLString::stringLen( xml_pk_def );

  if( pk_size > 0 )
    c_pk_def = XMLString::transcode( xml_pk_def );

  int nPkParams = countStrInLhs( UserStr.P.c_str(), c_pk_def );

  UserStr.P    = DefaultStr.P;
  table->insertNMVector( UserStr.P, nPkParams );

  nm_in = fopen( fPkEqn_fortran, "w" );
  fprintf( nm_in, "%s", c_pk_def );
  fclose( nm_in );
  delete c_pk_def;

  nm_in = fopen( fPkEqn_fortran, "r" );
  gSpkExpOutput = fopen( fPkEqn_cpp, "w" );
  gSpkExpSymbolTable = table;
  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpErrorMessages = new char[ SpkCompilerError::maxMessageLen()-50 ];
  strcpy( gSpkExpErrorMessages, "" );

  nm_parse();

  fclose( nm_in );
  fclose( gSpkExpOutput );
  if( gSpkExpErrors > 0 )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      sprintf( m, "Syntax error(s) found in <pk> definition.\n%s", 
	       gSpkExpErrorMessages );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, 
			      m, __LINE__, __FILE__ );
      throw e;
    }
  remove( fPkEqn_fortran );
}
void NonmemTranslator::parseDiffEqn( const DOMElement* diffeqn )
{    
  //============================
  // <diffeqn>
  //============================
  //--------------------------------------------------------------
  // Figure out the # of compartments/differential equations
  // The value is equal to the number of occurences of DADT
  // left-hand quantities in <diffeqn> block.
  //--------------------------------------------------------------
  char * c_des_def = NULL;
  const XMLCh* xml_des_def = diffeqn->getFirstChild()->getNodeValue();
  int des_size = XMLString::stringLen( xml_des_def );

  if( des_size > 0 )
    c_des_def = XMLString::transcode( xml_des_def );

  int nComps = countStrInLhs( UserStr.DADT.c_str(), c_des_def );

  UserStr.DADT = DefaultStr.DADT;  // this is a vector
  UserStr.A    = DefaultStr.A;
  UserStr.T    = DefaultStr.T;
  table->insertNMVector( UserStr.DADT, nComps );
  table->insertNMVector( UserStr.A, nComps );
  table->insertUserVar ( UserStr.T );
  
  nm_in = fopen( fDiffEqn_fortran, "w" );
  fprintf( nm_in, "%s", c_des_def );
  fclose( nm_in );
  delete c_des_def;

  nm_in = fopen( fDiffEqn_fortran, "r" );
  gSpkExpOutput = fopen( fDiffEqn_cpp, "w" );
  gSpkExpSymbolTable = table;
  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpErrorMessages = new char[ SpkCompilerError::maxMessageLen()-50 ];
  strcpy( gSpkExpErrorMessages, "" );

  nm_parse();

  fclose( nm_in );
  fclose( gSpkExpOutput );
  if( gSpkExpErrors > 0 )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      sprintf( m, "Syntax error(s) found in <diffeqn> definition.\n%s", 
	       gSpkExpErrorMessages );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, 
			      m, __LINE__, __FILE__ );
      throw e;
    }
  remove( fDiffEqn_fortran );
}
void NonmemTranslator::parseError( const DOMElement* error )
{
  //============================
  // <error> block
  //============================
  char * c_error_def = NULL;
  const XMLCh* xml_error_def = error->getFirstChild()->getNodeValue();
  int error_size = XMLString::stringLen( xml_error_def );

  if( error_size > 0 )
    c_error_def = XMLString::transcode( xml_error_def );

  nm_in = fopen( fErrorEqn_fortran, "w" );
  fprintf( nm_in, "%s", c_error_def );
  fclose( nm_in );
  delete c_error_def;

  nm_in = fopen( fErrorEqn_fortran, "r" );
  gSpkExpOutput = fopen( fErrorEqn_cpp, "w" );
  gSpkExpSymbolTable = table;
  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkExpErrorMessages = new char[ SpkCompilerError::maxMessageLen()-50 ];
  strcpy( gSpkExpErrorMessages, "" );

  nm_parse();

  fclose( nm_in );
  fclose( gSpkExpOutput );
  if( gSpkExpErrors > 0 )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      sprintf( m, "Syntax error(s) found in <error> definition.\n%s", 
	       gSpkExpErrorMessages );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, 
			      m, __LINE__, __FILE__ );
      throw e;
    }
  remove( fErrorEqn_fortran );
}
//=========================================================================================
// 
// Generate IndData.h, a file declaring and defining IndData template class,
// a class that represents a data set for a single individual.
//
// Pre-conditions  - The symbol table contains the NONMEM keywords and user defined variable
//                   names needed by the user-provided model.
//
//                 - The symbol table contains entries for the data labels and aliases.
//                   The data labels have to be retrievable by calling 
//                   SymbolTable::getlabels().  
//          
//                 - The vector returned by SymbolTable::getLables() must contain
//                   the data labels in the order in which they define the data items 
//                   (ie. columns) in the data set.
//
//                 - The current working directory is writable.
//
// Post-conditions - A file, IndData.h, is saved in the current working directory.
//=========================================================================================
void NonmemTranslator::generateIndData( ) const
{
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Preliminaries
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //
  // The only the "ID" data items have type of string
  // All others have double precision type.
  // When generating C++ source code, thus, the ID
  // data items have to be recognized and treated
  // differently.  We keep a pointer to the Symbol
  // object that holds "ID" data items handy for
  // frequent references.
  //
  const Symbol * pID = table->findi( KeyStr.ID );

  //
  // DV has to be declared non-constant if simulated data replaces it.
  //
  const Symbol * pDV = table->findi( KeyStr.DV );
  const Symbol * pORGDV = table->findi( KeyStr.ORGDV );

  //
  // The order in which the label strings appear is significant.
  // So, get a constant pointer to the list so that I cannot 
  // mess it up.
  //
  const vector<string> * labels = table->getLabels();
  vector<string>::const_iterator pLabel;

  //
  // Just a sanity check: there has be one and only one "ID" entry in the label list.
  //
#ifndef NDEBUG
  int cnt=0;
  for( pLabel = labels->begin(); pLabel != labels->end(); pLabel++ )
    {
      if( *pLabel == pID->name )
	++cnt;
    }
  if( cnt != 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "There may be one and only one \"%s\" label.", pID->name.c_str() );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
#endif

  // 
  // A SymbolTable object uses a std::map object as the actual
  // table.  The usual way of retrieving the entries in the internal
  // table is though member functions provided by SymbolTable class.
  // Another way, which is used here, is to access the internal
  // table directly, which is faster and convenient.
  //
  const map<const string, Symbol> * const internalTable = table->getTable();
  map<const string, Symbol>::const_iterator pInternalTable;

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Write into IndData.h
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //
  // Declare and define IndData template class.
  // For name binding reason, the declaration and the definition
  // are both stored in a single file: IndData.h
  //
  // Variable names strictly preserve the names defined/typed by the user.
  //
  ofstream oIndData_h( fIndData_h );
  if( !oIndData_h.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Failed to create a file, %s.", fIndData_h );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  
  //---------------------------------------------------------------------------------------
  //
  // Print out some description for this file.
  //
  //---------------------------------------------------------------------------------------
  oIndData_h << "/* " << myDescription << "*/" << endl;

  //---------------------------------------------------------------------------------------
  // 
  // Header include statements.
  //
  //---------------------------------------------------------------------------------------
  oIndData_h << "#ifndef INDDATA_H" << endl;
  oIndData_h << "#define INDDATA_H" << endl;
  oIndData_h << endl;

  oIndData_h << "#include <vector>" << endl;
  oIndData_h << "#include <map>" << endl;
  oIndData_h << "#include <spk/SpkValarray.h>" << endl;
  oIndData_h << "#include <spk/cholesky.h>" << endl;
  oIndData_h << "#include <spk/multiply.h>" << endl;
  oIndData_h << "#include <CppAD/CppAD.h>" << endl;
  oIndData_h << endl;
  
  //---------------------------------------------------------------------------------------
  //
  // Declaration of IndData class
  //
  // The template argument (ie. "ValueType" in this case)
  // must be something guaranteed that the user
  // do not use for one of their user-defined variables.
  //
  // The specification for SpkSourceML where the 
  // definition of PRED or other models appears
  // restricts user use of variable names beginning
  // with "spk_", let's take advantage of it here.
  //
  //---------------------------------------------------------------------------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "class IndData{" << endl;
  
  //----------------------------------------
  // Public member declarations.
  //----------------------------------------
  oIndData_h << "public:" << endl;

  // -----------
  // Constructor
  // -----------
  // A constructor that takes the number of measurements
  // for this particular individual's data set 
  // and the data item values (from the data file).
  // The prototype varies depending on different data sets.
  // The following example shows the contructor prototype
  // for a data set which has three data items: ID, d1 and d2.
  // The data labels d1 and d2 have aliases "d1_alias" and 
  // "d2_alias", respectively.
  // 
  // IndData( int nRecordsIn,
  //          const vector<char*>         &IDIn,
  //          const vector<spk_ValueType> &d1In,  // data item 1
  //          const vector<spk_ValueType> &d2In   // data item 2 )
  // : n(nRecordsIn), d1(d1In), d1_alias(d1In), d2(d2In), d2_alias(d2In)
  // {...}
  // 
  oIndData_h << "   IndData( int nRecordsIn";
  pLabel = labels->begin();
  for( ; pLabel != labels->end(); pLabel++ )
    {
      bool isID = ( *pLabel == pID->name );
      oIndData_h << "," << endl;
	  
      //
      // If the label is of "ID", then, the data type is char*.
      // Otherwise, all others have double precision.
      //
      oIndData_h << '\t' << "   const std::vector<" << (isID? "char*":"spk_ValueType") << ">";
      oIndData_h << " & " << *pLabel << "In";
    }
  oIndData_h << ");" << endl;
  oIndData_h << endl;

  // 
  // Declare member variables whose names resemble the data labels and their
  // corresponding synonyms if they have.  They are all have double
  // precision except for the ID data item which has char* type.
  //
  for( pInternalTable=internalTable->begin(); pInternalTable != internalTable->end(); pInternalTable++ )
    {
      const string varName         = pInternalTable->second.name;
      const string varAlias        = pInternalTable->second.synonym;
      const string keyVarName      = SymbolTable::key( varName );
      const string keyVarAlias     = SymbolTable::key( varAlias );
      enum Symbol::SymbolType type = pInternalTable->second.symbol_type;

      // Handling data labels.
      if( type == Symbol::DATALABEL )
	{
	  bool isID    = ( varName == pID->name?    true : false );
          bool isDV    = ( varName == pDV->name?    true : false );
          bool isORGDV = ( varName == pORGDV->name? true : false );

	  // If data simulation is requested, DV values are replaced by simulated
	  // measurements and the original DV values are moved/stored in ORGDV.
	  // Thus, in case of data simulation, DV (and ORGDV) has to be writable.
	  // Otherwise, DV is read-only.
	  oIndData_h << "   std::vector<";
	  if( isID )
	    {
	      oIndData_h << "char *";
	    }
	  else
	    {
	      oIndData_h << "spk_ValueType";
	    }
	  oIndData_h << "> " << varName << ";" << endl;

	  // If the current symbol has an alias, declare the alias as well.
	  if( varAlias != "" )
	    {
	      oIndData_h << "   std::vector<";
	      if( isID )
		{
		  oIndData_h << "char *";
		}
	      else
		{
		  oIndData_h << "spk_ValueType";
		}
	      oIndData_h << "> " << varAlias << ";" << endl;
	    }
	}
      // Handling NONMEM pred variables.
      //
      // The NONMEM pred variables are vectors whose elements
      // would be replaced by computed values.  So they have to be
      // declared writable.
      else if( type == Symbol::NONMEMDEF )
	{
	  if( keyVarName == KeyStr.THETA 
	      || keyVarName == KeyStr.ETA 
	      || keyVarName == KeyStr.EPS 
	      || keyVarName == KeyStr.ETARES
	      || keyVarName == KeyStr.WETARES
	      || keyVarName == KeyStr.IETARES
	      || keyVarName == KeyStr.IWETARES
	      || keyVarName == KeyStr.PETARES
	      || keyVarName == KeyStr.PWETARES
	      || keyVarName == KeyStr.CETARES
	      || keyVarName == KeyStr.CWETARES
	      || keyVarName == KeyStr.DADT
	      || keyVarName == KeyStr.P
	      || keyVarName == KeyStr.A )
	    oIndData_h << "   std::vector<std::vector<spk_ValueType> > " << varName << ";" << endl;

	  // The values of Omega and Sigma matrices are
	  // rather expressed as ETA and EPS, respectively.
	  // So, these matrices don't need place-holders.
	  if( keyVarName == KeyStr.OMEGA 
              || keyVarName == KeyStr.SIGMA )
	    {}
	}
      // Handling all others (ie. the user defined variables)
      // 
      // User defined variables store values computed every time the user model
      // is evaluated.  Thus, these have to be declared writable.
      else
	{
	  oIndData_h << "   std::vector<spk_ValueType> " << varName << ";" << endl;
	}
    }

  oIndData_h << endl;
  
  // ----------
  // Destructor
  // ----------
  oIndData_h << "   ~IndData();" << endl;

  //----------------------------------------
  // Public member declarations
  //----------------------------------------
  oIndData_h << "   int getNRecords() const;" << endl;
  oIndData_h << "   const SPK_VA::valarray<double> getMeasurements() const;"          << endl;
  oIndData_h << "   int getRecordIndex( int measurementIndex ) const;"                << endl;
  oIndData_h << "   int getMeasurementIndex( int recordIndex ) const;"                << endl;
  oIndData_h << "   void replaceMeasurements( const SPK_VA::valarray<double>& yyi );" << endl;
  oIndData_h << "   void replacePred   ( const SPK_VA::valarray<double>& predIn );"   << endl;
  oIndData_h << "   void replaceRes    ( const SPK_VA::valarray<double>& ResIn );"    << endl;
  oIndData_h << "   void replaceWRes   ( const SPK_VA::valarray<double>& WresIn );"   << endl;
  oIndData_h << "   void replacePPred  ( const SPK_VA::valarray<double>& pPredIn );"  << endl;
  oIndData_h << "   void replacePRes   ( const SPK_VA::valarray<double>& pResIn );"   << endl;
  oIndData_h << "   void replacePWRes  ( const SPK_VA::valarray<double>& pWResIn );"  << endl;
  oIndData_h << "   void replaceIPred  ( const SPK_VA::valarray<double>& iPredIn );"  << endl;
  oIndData_h << "   void replaceIRes   ( const SPK_VA::valarray<double>& iResIn );"   << endl;
  oIndData_h << "   void replaceIWRes  ( const SPK_VA::valarray<double>& iWresIn );"  << endl;
  oIndData_h << "   void replaceCPred  ( const SPK_VA::valarray<double>& cPredIn );"  << endl;
  oIndData_h << "   void replaceCRes   ( const SPK_VA::valarray<double>& cResIn );"   << endl;
  oIndData_h << "   void replaceCWRes  ( const SPK_VA::valarray<double>& cWresIn );"  << endl;
  if( ourTarget == POP )
  {
     oIndData_h << "   void replaceEta     ( const SPK_VA::valarray<double>& etaIn );"      << endl;
     oIndData_h << "   void replaceEtaRes  ( const SPK_VA::valarray<double>& EtaresIn );"  << endl;
     oIndData_h << "   void replaceWEtaRes ( const SPK_VA::valarray<double>& WetaresIn );" << endl;
     oIndData_h << "   void replaceIEtaRes ( const SPK_VA::valarray<double>& iEtaresIn );"  << endl;
     oIndData_h << "   void replaceIWEtaRes( const SPK_VA::valarray<double>& iWetaresIn );" << endl;
     oIndData_h << "   void replacePEtaRes ( const SPK_VA::valarray<double>& pEtaResIn );"  << endl;
     oIndData_h << "   void replacePWEtaRes( const SPK_VA::valarray<double>& pWEtaResIn );" << endl;
     oIndData_h << "   void replaceCEtaRes ( const SPK_VA::valarray<double>& cEtaResIn );"  << endl;
     oIndData_h << "   void replaceCWEtaRes( const SPK_VA::valarray<double>& cWEtaResIn );" << endl;
  }
  oIndData_h << endl;

  //----------------------------------------
  // Protected member declarations.
  //----------------------------------------
  oIndData_h << "protected:" << endl;
  oIndData_h << "   IndData();" << endl;
  oIndData_h << "   IndData( const IndData& );" << endl;
  oIndData_h << "   IndData& operator=( const IndData& );" << endl;
  oIndData_h << endl;
  oIndData_h << "   int nY; // #of measurements (DVs where MDV=0)." << endl;
  oIndData_h << "   SPK_VA::valarray<double> measurements;" << endl;

  //----------------------------------------
  // Private member declarations.
  //----------------------------------------
  oIndData_h << "private:" << endl;
  oIndData_h << "   const int nRecords; // the number of data records." << endl;
  oIndData_h << "   void assign( double&, const CppAD::AD<double>& ) const;" << endl;
  oIndData_h << "   void assign( double&, double ) const;" << endl;
  oIndData_h << "   /////////////////////////////////////////////////////////" << endl;
  oIndData_h << "   //      original                     y"                    << endl;
  oIndData_h << "   //  -------------------      -------------------"          << endl;
  oIndData_h << "   //   j    i   MDV   DV         j'  j   i   DV"             << endl;
  oIndData_h << "   //  -------------------      -------------------"          << endl;
  oIndData_h << "   //   0    0    0    0.1        0   0   0   0.1"            << endl;
  oIndData_h << "   //   1    0    1               1   2   0   0.2"            << endl;
  oIndData_h << "   //   2    0    0    0.2        2   4   0   0.3"            << endl;
  oIndData_h << "   //   3    0    1"                                          << endl;
  oIndData_h << "   //   4    0    0    0.3"                                   << endl;
  oIndData_h << "   //"                                                        << endl;
  oIndData_h << "   //"                                                        << endl;
  oIndData_h << "   //   jTojPrime            jPrimeToj"                       << endl;
  oIndData_h << "   //  -----------          -----------"                      << endl;
  oIndData_h << "   //    j    j'              j'   j"                         << endl;
  oIndData_h << "   //  -----------          -----------"                      << endl;
  oIndData_h << "   //    0    0               0    0"                         << endl;
  oIndData_h << "   //    1   -1*              1    2"                         << endl;
  oIndData_h << "   //    2    1               2    4"                         << endl;
  oIndData_h << "   //    3   -1*"                                             << endl;
  oIndData_h << "   //    4    2"                                              << endl;
  oIndData_h << "   //"                                                        << endl;
  oIndData_h << "   //  * (-1) points to no j', i.e. MDV=1"                    << endl;
  oIndData_h << "   /////////////////////////////////////////////////////////" << endl;
  oIndData_h << "   std::vector<int> jTojPrime;" << endl;
  oIndData_h << "   std::vector<int> jPrimeToj;" << endl;
  oIndData_h << "};" << endl;


  //---------------------------------------------------------------------------------------
  //
  // Definition of IndData class
  //
  //---------------------------------------------------------------------------------------

  // -----------
  // Constructor
  // -----------
  //
  // Prototype:
  // Pay extra attention to the order of arguments.
  // The declaration portiona was done using the vector of labels
  // returned by SymbolTable::getLabels().  Use the
  // same exact vector to ensure the order consistency.
  //
  string synonym;
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "IndData<spk_ValueType>::IndData( int nRecordsIn";
  pLabel = labels->begin();
  for( ; pLabel != labels->end(); pLabel++ )
    {
      bool isID = ( *pLabel == pID->name );
      oIndData_h << "," << endl;

      //
      // If the label string is of "ID", then the data type is char*.
      // Othewise, double.
      //
      oIndData_h << "const std::vector<" << (isID? "char*":"spk_ValueType") << "> ";
      oIndData_h << "& " << *pLabel << "In";
    }
  oIndData_h << ")" << endl;
  oIndData_h << ": nRecords( nRecordsIn ), // # of records (including MDV=0)" << endl;
  oIndData_h << "  nY( 0 )   // # of measurements" << endl;

  //
  // Constructor initialization:
  // Assign the argument values to the internal valarray variables.
  // Also assign the same values to equivalent (synonym) variables
  // if the variable has a synonym defined.
  // NOTE: Yes, I know if the values were pointed by pointers
  // the primary variable and alias can share the same objects.
  // But, since they are of type of, essentially, double, 
  // copying would not be a big deal; as a matter of fact, 
  // it may be actually faster than accessing via pointers.
  //
  pLabel = labels->begin();
  for( ; pLabel != labels->end(); pLabel++ )
    {
      const string keyLabel = SymbolTable::key( *pLabel );
      oIndData_h << "," << endl;
      oIndData_h << *pLabel;
      oIndData_h << "( " << *pLabel << "In" << " )";

      //
      // If the label has a synonym, apply the same value to the synonym.
      //
      if( ( synonym = table->findi( keyLabel/**pLabel*/ )->synonym ) != "" )
	{
	  oIndData_h << "," << endl;
	  oIndData_h << synonym;
	  oIndData_h << "( " << *pLabel << "In" << " )";
	}
    }

  //
  // Constructor body:
  // Initialize the sizes of the user defined variables that
  // appear in the model definition.
  // We don't know the values yet, so just assign the size,
  // which is the same as the number of data records for a subject.
  //
  // These arrays will be internally (ie. PRED routine) used to store 
  // intermediate values.  The intermediate values are
  // returned to the user for tabular display or plot display.
  // They need corresponding shadow placeholders so that
  // if an iteration fails, the system can return the previously
  // successfully computed values.
  //
  pInternalTable = internalTable->begin();
  for( ; pInternalTable != internalTable->end(); pInternalTable++ )
    {
      const string label    = pInternalTable->second.name;
      const string keyLabel = SymbolTable::key( label );
      if( keyLabel == KeyStr.OMEGA || keyLabel == KeyStr.SIGMA )
	{
	  continue;
	}

      //
      // Initialize sizes of place holders for computed values.
      //
      if( find( labels->begin(), labels->end(), pInternalTable->second.name ) 
	  == labels->end() )
	oIndData_h << "," << endl << label << "( nRecordsIn )";
    }
  oIndData_h << endl;

  oIndData_h << "{" << endl;
  oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      if( " << UserStr.MDV << "[i] != 1 )" << endl;
  oIndData_h << "          ++nY;" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "   measurements.resize( nY ); " << endl;
  oIndData_h << endl;

  oIndData_h << "   //" << endl;
  oIndData_h << "   // Initialize scalar variables" << endl;
  oIndData_h << "   //" << endl;
  pInternalTable = internalTable->begin();
  for( ; pInternalTable != internalTable->end(); pInternalTable++ )
    {
      const string label    = pInternalTable->second.name;
      const string keyLabel = SymbolTable::key( label );
      if( keyLabel == KeyStr.OMEGA 
	  || keyLabel == KeyStr.SIGMA
	  || keyLabel == KeyStr.THETA
	  || keyLabel == KeyStr.ETA
	  || keyLabel == KeyStr.ETARES
	  || keyLabel == KeyStr.WETARES
	  || keyLabel == KeyStr.IETARES
	  || keyLabel == KeyStr.IWETARES
	  || keyLabel == KeyStr.PETARES
	  || keyLabel == KeyStr.PWETARES
	  || keyLabel == KeyStr.CETARES
	  || keyLabel == KeyStr.CWETARES
	  || keyLabel == KeyStr.EPS
	  || keyLabel == KeyStr.DADT
	  || keyLabel == KeyStr.A
	  || keyLabel == KeyStr.P
	  )
	{
	  continue;
	}

      //
      // Initialize place holders for computed values.
      //
      if( find( labels->begin(), labels->end(), pInternalTable->second.name ) 
	  == labels->end() )
	oIndData_h << "fill( " << label << ".begin(), " << label << ".end(), -99999 );" << endl;
    }
  oIndData_h << endl;
  oIndData_h << "copy( " << UserStr.DV << ".begin(), " << UserStr.DV << ".end(), " << UserStr.ORGDV << ".begin() );" << endl;
  oIndData_h << endl;

  oIndData_h << "   //" << endl;
  oIndData_h << "   // Resize and initialize vector variables" << endl;
  oIndData_h << "   //" << endl;
  oIndData_h << "   jTojPrime.resize( nRecords );" << endl;
  oIndData_h << "   jPrimeToj.resize( nY );" << endl;
  oIndData_h << "   for( int j=0, jPrime=0; j<nRecords; j++ )" << endl;
  oIndData_h << "   {" << endl;
  if( myThetaLen > 0 )
    {
      oIndData_h << "      " << UserStr.THETA << "[j].resize( " << myThetaLen << " );" << endl;
      oIndData_h << "      " << "fill( " << UserStr.THETA << "[j].begin(), " << UserStr.THETA << "[j].end(), -99999 );" << endl;
    }
  if( myEtaLen > 0 )
    {
      oIndData_h << "      " << UserStr.ETA     << "[j].resize( " << myEtaLen << " );" << endl;
      oIndData_h << "      " << "fill( " << UserStr.ETA << "[j].begin(), " << UserStr.ETA << "[j].end(), -99999 );" << endl;
      if( ourTarget == POP )
	{
	  oIndData_h << "      " << UserStr.ETARES   << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << UserStr.WETARES  << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << UserStr.IETARES  << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << UserStr.IWETARES << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << UserStr.PETARES  << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << UserStr.PWETARES << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << UserStr.CETARES  << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << UserStr.CWETARES << "[j].resize( " << myEtaLen << " );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.ETARES   << "[j].begin(), " << UserStr.ETARES   << "[j].end(), -99999 );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.WETARES  << "[j].begin(), " << UserStr.WETARES  << "[j].end(), -99999 );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.IETARES  << "[j].begin(), " << UserStr.IETARES  << "[j].end(), -99999 );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.IWETARES << "[j].begin(), " << UserStr.IWETARES << "[j].end(), -99999 );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.PETARES  << "[j].begin(), " << UserStr.PETARES  << "[j].end(), -99999 );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.PWETARES << "[j].begin(), " << UserStr.PWETARES << "[j].end(), -99999 );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.CETARES  << "[j].begin(), " << UserStr.CETARES  << "[j].end(), -99999 );" << endl;
	  oIndData_h << "      " << "fill( " << UserStr.CWETARES << "[j].begin(), " << UserStr.CWETARES << "[j].end(), -99999 );" << endl;
	}
    }
  if( myEpsLen > 0 )
    {
      oIndData_h << "      " << UserStr.EPS   << "[j].resize( " << myEpsLen << " );" << endl;
      oIndData_h << "      " << "fill( " << UserStr.EPS   << "[j].begin(), " << UserStr.EPS << "[j].end(), -99999 );" << endl;
    }

  if( myModelSpec != PRED )
    {
      assert( table->findi( KeyStr.DADT ) != Symbol::empty() );
      assert( table->findi( KeyStr.A    ) != Symbol::empty() );
      assert( table->findi( KeyStr.P    ) != Symbol::empty() );

      int nComps  = table->findi( KeyStr.DADT )->initial[0].size();
      oIndData_h << "      " << UserStr.DADT << "[j].resize( " << nComps  << " );" << endl;
      oIndData_h << "      " << UserStr.A    << "[j].resize( " << nComps  << " );" << endl;

      int nParams = table->findi( KeyStr.P    )->initial[0].size(); 
      oIndData_h << "      " << UserStr.P    << "[j].resize( " << nParams << " );" << endl;

      oIndData_h << "      " << "fill( " << UserStr.DADT << "[j].begin(), " << UserStr.DADT << "[j].end(), -99999 );" << endl;
      oIndData_h << "      " << "fill( " << UserStr.A    << "[j].begin(), " << UserStr.P    << "[j].end(), -99999 );" << endl;
      oIndData_h << "      " << "fill( " << UserStr.P    << "[j].begin(), " << UserStr.A    << "[j].end(), -99999 );" << endl;

    }

  oIndData_h << "        if( " << UserStr.MDV << "[j] != 1 )" << endl;
  oIndData_h << "        {" << endl;
  oIndData_h << "           assign( measurements[jPrime], " << UserStr.DV << "[j] );" << endl;
  oIndData_h << "           jPrimeToj[jPrime] = j;" << endl;
  oIndData_h << "           jTojPrime[j] = jPrime;" << endl;
  oIndData_h << "           jPrime++;" << endl;
  oIndData_h << "        }" << endl;
  oIndData_h << "        else" << endl;
  oIndData_h << "        {" << endl;
  oIndData_h << "           jTojPrime[j] = -1;" << endl;
  oIndData_h << "        }" << endl;

  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;

  oIndData_h << endl;

  // ----------
  // Destructor
  // ---------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "IndData<spk_ValueType>::~IndData(){}" << endl;

  // -------------------
  // Default constructor
  // (protected)
  // -------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "IndData<spk_ValueType>::IndData(){}" << endl;

  // ----------------
  // Copy constructor
  // (protected)
  // ---------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "IndData<spk_ValueType>::IndData( const IndData<spk_ValueType>& ){}" << endl;

  // -------------------
  // Assignment operator
  // (protected)
  // -------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "IndData<spk_ValueType>& IndData<spk_ValueType>::operator=( const IndData<spk_ValueType>& ){}" << endl;

  //------------------
  // getNRecords()
  //------------------
  oIndData_h << "// return the number of data records (include MDV=1)" << endl;
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "int IndData<spk_ValueType>::getNRecords() const " << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   return nRecords;" << endl;
  oIndData_h << "}" << endl;

  //----------------------------------------
  // getMeasurementIndex( int j' )
  //----------------------------------------
  oIndData_h << "// Return an index to y (measurements/DVs) vector, j, such that " << endl;
  oIndData_h << "// the value of j-th element in y corresponds to the DV value of " << endl;
  oIndData_h << "// the j'-th record in the data set .  " << endl;
  oIndData_h << "// If the j'-th record does not have a DV value, the returned value is -1." << endl;
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "int IndData<spk_ValueType>::getMeasurementIndex( int recordIndex ) const" << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   return jTojPrime[recordIndex];" << endl;
  oIndData_h << "}" << endl;

  //----------------------------------------
  // getRecordIndex( int j )
  //----------------------------------------
  oIndData_h << "// Return the index, j', to a record in the dataset to which the value of " << endl;
  oIndData_h << "// the j-th element of y (measurements/DVs) vector belongs." << endl;
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "int IndData<spk_ValueType>::getRecordIndex( int measurementIndex ) const" << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   return jPrimeToj[measurementIndex];" << endl;
  oIndData_h << "}" << endl;

  // -----------------
  // getMeasurements()
  // -----------------
  oIndData_h << "// Return SPK's y" << endl;
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "const SPK_VA::valarray<double> IndData<spk_ValueType>::getMeasurements() const" << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   return measurements;" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // ---------------------
  // replaceMeasurements()
  // ---------------------
  // Replace (the internally kept) y with the given y'.
  //
  oIndData_h << "// Replace y with the given y'." << endl;
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceMeasurements( const SPK_VA::valarray<double>& yyi )" << endl;
  oIndData_h << "{" << endl;
  bool hasAlias = ( pDV->synonym != "" );
  oIndData_h << "   for( int i=0, k=0; i<nRecords; i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      if( " << UserStr.MDV << "[i] != 1 )" << endl;
  oIndData_h << "      {" << endl;
  oIndData_h << "         " << UserStr.ORGDV << "[i] = " << UserStr.DV << "[i];" << endl;
  oIndData_h << "         " << UserStr.DV << "[i] = yyi[k];" << endl;
  if( hasAlias )
    oIndData_h << "         " << pDV->synonym << "[i] = yyi[k];" << endl;
  oIndData_h << "         k++;" << endl;
  oIndData_h << "      }" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
 
  // --------------------
  // replacePred()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replacePred( const SPK_VA::valarray<double>& predIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( predIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.PRED << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.PRED << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = predIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replaceRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceRes( const SPK_VA::valarray<double>& ResIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( ResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.RES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.RES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = ResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replaceWRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceWRes( const SPK_VA::valarray<double>& WResIn )"    << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( WResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.WRES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.WRES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = WResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replaceIPred()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceIPred( const SPK_VA::valarray<double>& iPredIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( iPredIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.IPRED << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.IPRED << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = iPredIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replaceIRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceIRes( const SPK_VA::valarray<double>& iResIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( iResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.IRES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.IRES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = iResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replaceIWRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceIWRes( const SPK_VA::valarray<double>& iWResIn )"    << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( iWResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.IWRES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.IWRES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = iWResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replacePPred()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replacePPred( const SPK_VA::valarray<double>& pPredIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( pPredIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.PPRED << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.PPRED << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = pPredIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replacePRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replacePRes( const SPK_VA::valarray<double>& pResIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( pResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.PRES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.PRES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = pResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replacePWRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replacePWRes( const SPK_VA::valarray<double>& pWResIn )"    << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( pWResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.PWRES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.PWRES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = pWResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

   // --------------------
  // replaceCPred()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceCPred( const SPK_VA::valarray<double>& cPredIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( cPredIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.CPRED << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.CPRED << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = cPredIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replaceCRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceCRes( const SPK_VA::valarray<double>& cResIn )"     << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( cResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.CRES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.CRES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = cResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  // --------------------
  // replaceCWRes()
  // --------------------
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::replaceCWRes( const SPK_VA::valarray<double>& cWResIn )"    << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   assert( cWResIn.size() == nRecords );" << endl;
  oIndData_h << "   typename std::vector<spk_ValueType>::iterator itr = " << UserStr.CWRES << ".begin();" << endl;
  oIndData_h << "   for( int i=0; itr != " << UserStr.CWRES << ".end(); itr++, i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      *itr = cWResIn[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

 if( ourTarget == POP )
    {
      // --------------------
      // replaceEta()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replaceEta( const SPK_VA::valarray<double>& etaIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( etaIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.ETA << "[i][j] = etaIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;
      
      // --------------------
      // replaceEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replaceEtaRes( const SPK_VA::valarray<double>& EtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( EtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.ETARES << "[i][j] = EtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;
      
      // --------------------
      // replaceWEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replaceWEtaRes( const SPK_VA::valarray<double>& WEtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( WEtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.WETARES << "[i][j] = WEtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;

      // --------------------
      // replaceIEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replaceIEtaRes( const SPK_VA::valarray<double>& iEtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( iEtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.IETARES << "[i][j] = iEtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;
      
      // --------------------
      // replaceIWEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replaceIWEtaRes( const SPK_VA::valarray<double>& iWEtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( iWEtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.IWETARES << "[i][j] = iWEtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;
 
      // --------------------
      // replacePEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replacePEtaRes( const SPK_VA::valarray<double>& pEtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( pEtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.PETARES << "[i][j] = pEtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;
      
      // --------------------
      // replacePWEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replacePWEtaRes( const SPK_VA::valarray<double>& pWEtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( pWEtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.PWETARES << "[i][j] = pWEtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;

      // --------------------
      // replaceCEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replaceCEtaRes( const SPK_VA::valarray<double>& cEtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( cEtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.CETARES << "[i][j] = cEtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;
      
      // --------------------
      // replaceCWEtaRes()
      // --------------------
      oIndData_h << "template <class spk_ValueType>" << endl;
      oIndData_h << "void IndData<spk_ValueType>::replaceCWEtaRes( const SPK_VA::valarray<double>& cWEtaResIn )"    << endl;
      oIndData_h << "{" << endl;
      oIndData_h << "   const int nEta = " << myEtaLen << ";" << endl;
      oIndData_h << "   assert( cWEtaResIn.size() == nEta );" << endl;
      oIndData_h << "   for( int i=0; i<nRecords; i++ )" << endl;
      oIndData_h << "   {" << endl;
      oIndData_h << "      for( int j=0; j<nEta; j++ )" << endl;
      oIndData_h << "      {" << endl;
      oIndData_h << "         " << UserStr.CWETARES << "[i][j] = cWEtaResIn[j];" << endl;
      oIndData_h << "      }" << endl;
      oIndData_h << "   }" << endl;
      oIndData_h << "}" << endl;
      oIndData_h << endl;
    }

  // -------------------
  // assignToDbl()
  // -------------------
  // This is to make an assignment operation, a = b, transparent for situations where a is double and b is CppAD<double>
  // and a an b are both double.
  //
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::assign( double & d, const CppAD::AD<double>& ad ) const" << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   d = CppAD::Value( ad );" << endl;
  oIndData_h << "   return;" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;
  oIndData_h << "template <class spk_ValueType>" << endl;
  oIndData_h << "void IndData<spk_ValueType>::assign( double & left, double right ) const" << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   left = right;" << endl;
  oIndData_h << "   return;" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  oIndData_h << "#endif" << endl;

  oIndData_h.close();
}
//=========================================================================================
//
// Generate DataSet.h, a file declaring and defining DataSet template class,
// a class that represents an entire population data set.
//
// Pre-conditions  - The symbol table contains the NONMEM keywords and user defined variable
//                   names needed by the user-provided model.
//
//                 - The symbol table contains entries for the data labels and aliases.
//                   The data labels have to be retrievable by calling 
//                   SymbolTable::getlabels().  
//          
//                 - The vector returned by SymbolTable::getLables() must contain
//                   the data labels in the order in which they define the data items 
//                   (ie. columns) in the data set.
//
//                 - The current working directory is writable.
//
// post-conditions - DataSet.h is saved in the current working directory.
//
//=========================================================================================
void NonmemTranslator::generateDataSet( ) const
{
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Preliminaries
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  // 
  // A SymbolTable object uses a std::map object as the actual
  // table.  The usual way of retrieving the entries in the internal
  // table is though member functions provided by SymbolTable class.
  // Another way, which is used here, is to access the internal
  // table directly, which is faster and convenient.
  //
  const map<const string, Symbol> * const t = table->getTable();

  //
  // The order in which the label strings appear is significant.
  // So, get a constant pointer to the list so that I cannot 
  // mess it up.
  //
  const vector<string> *labels = table->getLabels();
  vector<string>::const_iterator pLabel;
  const int nLabels = labels->size();

  //
  // Reference to an Symbol object that represents "ID" data label.
  // Keep it for repetitive use.
  //
  const Symbol * const pID = table->findi( KeyStr.ID );

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Write into DataSet.h
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //
  // Declare and define DataSet template class.
  // For name binding reason, the declaration and the definition
  // are both stored in a single file: DataSet.h
  // 
  // The only legal constructor is the default constructor.
  // The constructor initializes the array of IndData objects,
  // each contains the entire data set for a subject.
  //
  // The order in which the arguments to the IndData 
  // constructor appear is significant.  The order must
  // match with the IndData constructor's interface.
  // It relizes on the order of strings stored in the list 
  // returned by "SymbolTable::getLabels()".
  // Thus, in between the time when the DataSet constructor
  // is defined and the time when the IndData constructor
  // is declared/defined, the SymbolTable object
  // may NOT be modified.
  //
  ofstream oDataSet_h( fDataSet_h );
  if( !oDataSet_h.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Failed to create a file, %s.", fDataSet_h );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  //---------------------------------------------------------------------------------------
  //
  // Print out some description for this file.
  //
  //---------------------------------------------------------------------------------------
  oDataSet_h << "// " << myDescription << endl;

  //---------------------------------------------------------------------------------------
  // 
  // Header include statements.
  //
  //---------------------------------------------------------------------------------------
  oDataSet_h << "#ifndef DATASET_H" << endl;
  oDataSet_h << "#define DATASET_H" << endl;

  oDataSet_h << "#include <vector>" << endl;
  oDataSet_h << "#include <spk/SpkValarray.h>" << endl;
  oDataSet_h << "#include \"IndData.h\"" << endl;
  oDataSet_h << endl;

  //---------------------------------------------------------------------------------------
  //
  // Declaration of DataSet class
  //
  // The template argument (ie. "ValueType" in this case)
  // must be something guaranteed that the user
  // do not use for one of their user-defined variables.
  //
  // The specification for SpkSourceML where the 
  // definition of PRED or other models appears
  // restricts user use of variable names beginning
  // with "spk_", let's take advantage of it here.
  //
  //---------------------------------------------------------------------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "class DataSet" << endl;
  oDataSet_h << "{" << endl;
      
  //----------------------------------------
  // Public member declarations.
  //----------------------------------------
  oDataSet_h << "public:" << endl;

  // -------------------
  // Default constructor
  // -------------------
  // The default constructor initializes the entire data set
  // internally.
  //
  oDataSet_h << "   DataSet();" << endl;

  // ----------
  // Destructor
  // ----------
  oDataSet_h << "   ~DataSet();" << endl;
  oDataSet_h << endl;

  // ------------
  // The data set
  // ------------
  oDataSet_h << "   std::vector<IndData<spk_ValueType>*> data;" << endl;

  // -----------------------
  // Public member functions
  // -----------------------
  oDataSet_h << "   const SPK_VA::valarray<double> getAllMeasurements() const;" << endl;
  oDataSet_h << "   int getMeasurementIndex( int recordIndex ) const;" << endl;
  oDataSet_h << "   int getRecordIndex( int measurementIndex ) const;" << endl;
  oDataSet_h << "   int getPopSize() const;" << endl;
  oDataSet_h << "   const SPK_VA::valarray<int> getN() const;" << endl;
  oDataSet_h << "   void replaceAllMeasurements( const SPK_VA::valarray<double> & yy );"    << endl;
  oDataSet_h << "   void replaceEta     ( const SPK_VA::valarray<double>& EtaIn );"      << endl;
  oDataSet_h << "   void replacePred    ( const SPK_VA::valarray<double>& PredIn );"        << endl;
  oDataSet_h << "   void replaceRes     ( const SPK_VA::valarray<double>& ResIn );"         << endl;
  oDataSet_h << "   void replaceWRes    ( const SPK_VA::valarray<double>& WResIn );"        << endl;
  oDataSet_h << "   void replaceEtaRes  ( const SPK_VA::valarray<double>& EtaResIn );"      << endl;
  oDataSet_h << "   void replaceWEtaRes ( const SPK_VA::valarray<double>& WEtaResIn );"     << endl;
  oDataSet_h << "   void replaceIPred   ( const SPK_VA::valarray<double>& iPredIn );"       << endl;
  oDataSet_h << "   void replaceIRes    ( const SPK_VA::valarray<double>& iResIn );"        << endl;
  oDataSet_h << "   void replaceIWRes   ( const SPK_VA::valarray<double>& iWResIn );"       << endl;
  oDataSet_h << "   void replaceIEtaRes ( const SPK_VA::valarray<double>& iEtaResIn );"     << endl;
  oDataSet_h << "   void replaceIWEtaRes( const SPK_VA::valarray<double>& iWEtaResIn );"    << endl;
  oDataSet_h << "   void replacePPred   ( const SPK_VA::valarray<double>& pPredIn );"       << endl;
  oDataSet_h << "   void replacePRes    ( const SPK_VA::valarray<double>& pResAllIn );"     << endl;
  oDataSet_h << "   void replacePWRes   ( const SPK_VA::valarray<double>& pWResAllIn );"    << endl;
  oDataSet_h << "   void replacePEtaRes ( const SPK_VA::valarray<double>& pEtaResAllIn );"  << endl;
  oDataSet_h << "   void replacePWEtaRes( const SPK_VA::valarray<double>& pWEtaResAllIn );" << endl;
  oDataSet_h << "   void replaceCPred   ( const SPK_VA::valarray<double>& cPredIn );"       << endl;
  oDataSet_h << "   void replaceCRes    ( const SPK_VA::valarray<double>& cResAllIn );"     << endl;
  oDataSet_h << "   void replaceCWRes   ( const SPK_VA::valarray<double>& cWResAllIn );"    << endl;
  oDataSet_h << "   void replaceCEtaRes ( const SPK_VA::valarray<double>& cEtaResAllIn );"  << endl;
  oDataSet_h << "   void replaceCWEtaRes( const SPK_VA::valarray<double>& cWEtaResAllIn );" << endl;
  oDataSet_h << endl;
  oDataSet_h << "   friend std::ostream& operator<< <spk_ValueType>( std::ostream& o, const DataSet<spk_ValueType>& A );" << endl;
  oDataSet_h << endl;
 
  //----------------------------------------
  // Protected member declarations.
  //----------------------------------------
  oDataSet_h << "protected:" << endl;
  oDataSet_h << "   DataSet( const DataSet& );" << endl;
  oDataSet_h << "   DataSet& operator=( const DataSet& );" << endl;
  oDataSet_h << endl;

  //----------------------------------------
  // Private member declarations.
  //----------------------------------------
  oDataSet_h << "private:" << endl;
  oDataSet_h << "   SPK_VA::valarray<double> measurements; // a long vector containg all measurements" << endl;
  oDataSet_h << "   SPK_VA::valarray<int> N; // a vector containing the # of measurements for each individual." << endl;
  oDataSet_h << "   const int popSize;" << endl;
  oDataSet_h << "   /////////////////////////////////////////////////////////" << endl;
  oDataSet_h << "   //      original                     y"                    << endl;
  oDataSet_h << "   //  -------------------      -------------------"          << endl;
  oDataSet_h << "   //   j    i   MDV   DV         j'  j   i   DV"             << endl;
  oDataSet_h << "   //  -------------------      -------------------"          << endl;
  oDataSet_h << "   //   0    0    0    0.1        0   0   0   0.1"            << endl;
  oDataSet_h << "   //   1    0    1               1   2   0   0.2"            << endl;
  oDataSet_h << "   //   2    0    0    0.2        2   4   0   0.3"            << endl;
  oDataSet_h << "   //   3    0    1               3   5   1   0.01"           << endl;
  oDataSet_h << "   //   4    0    0    0.3        4   7   1   0.02"           << endl;
  oDataSet_h << "   //   5    1    0    0.1        5   9   1   0.03"           << endl;
  oDataSet_h << "   //   6    1    1"                                          << endl;
  oDataSet_h << "   //   7    1    0    0.2"                                   << endl;
  oDataSet_h << "   //   8    1    1"                                          << endl;
  oDataSet_h << "   //   9    1    0    0.3"                                   << endl;
  oDataSet_h << "   //"                                                        << endl;
  oDataSet_h << "   //"                                                        << endl;
  oDataSet_h << "   //   jTojPrime            jPrimeToj"                       << endl;
  oDataSet_h << "   //  -----------          -----------"                      << endl;
  oDataSet_h << "   //    j    j'              j'   j"                         << endl;
  oDataSet_h << "   //  -----------          -----------"                      << endl;
  oDataSet_h << "   //    0    0               0    0"                         << endl;
  oDataSet_h << "   //    1   -1*              1    2"                         << endl;
  oDataSet_h << "   //    2    1               2    4"                         << endl;
  oDataSet_h << "   //    3   -1*              3    5"                         << endl;
  oDataSet_h << "   //    4    2               4    7"                         << endl;
  oDataSet_h << "   //    5    3               5    9"                         << endl;
  oDataSet_h << "   //    6   -1*"                                             << endl;
  oDataSet_h << "   //    7    4"                                              << endl;
  oDataSet_h << "   //    8   -1*"                                             << endl;
  oDataSet_h << "   //    9    5"                                              << endl;
  oDataSet_h << "   //"                                                        << endl;
  oDataSet_h << "   //  * (-1) points to no j', i.e. MDV=1"                    << endl;
  oDataSet_h << "   /////////////////////////////////////////////////////////" << endl;
  oDataSet_h << "   std::vector<int> jTojPrime;" << endl;
  oDataSet_h << "   std::vector<int> jPrimeToj;" << endl;

  oDataSet_h << "};" << endl;
  oDataSet_h << endl;

  //---------------------------------------------------------------------------------------
  //
  // Definition of DataSet class
  //
  //---------------------------------------------------------------------------------------
       
  // -------------------
  // Default constructor
  // -------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "DataSet<spk_ValueType>::DataSet()" << endl;

  //
  // Constructor intialization
  //
  oDataSet_h << ": popSize( " << ourPopSize << " )," << endl;
  oDataSet_h << "  data( " << ourPopSize << " )," << endl;
  oDataSet_h << "  N( " << ourPopSize << " )" << endl;

  //
  // Constructor body
  //
  oDataSet_h << "{" << endl;

  // Initialize the entire data set.
  for( int who=0, sofar=0, nRecords=0; who < ourPopSize; who++, sofar+=nRecords )
    {
      char c_who[256];
      sprintf( c_who, "%d", who );
      int nRecords = pID->initial[who].size();
      const string id = pID->initial[who][0];

      //
      // The order in which the labels appear must be consistent
      // with the order in the constructor declaration.
      // By using the iterator in both places, here and up there,
      // they shall match.  However, this should be tested in
      // the corresponding unit tests.
      //
      oDataSet_h << "   //------------------------------------" << endl;
      oDataSet_h << "   // Subject <" << id << "> " << endl;
      oDataSet_h << "   // # of sampling points = " << nRecords << endl;
      oDataSet_h << "   //------------------------------------" << endl;
      oDataSet_h << "   N[" << who << "] = " << nRecords << ";" << endl;

      //
      // Initialize C arrays with data values.
      // The C arrays are passed to the valarray's constructor.
      //
      pLabel = labels->begin();
      for( int i=0; pLabel != labels->end(), i<nLabels; i++, pLabel++ )
	{
	  const Symbol * s = table->findi( *pLabel );
	  bool isID = (*pLabel == pID->name);
	  string carray_name = s->name + "_" + c_who + "_c";
	  string vector_name = s->name + "_" + c_who;

	  oDataSet_h << (isID? "char*":"spk_ValueType") << " " << carray_name << "[] = { ";
	  for( int j=0; j<nRecords; j++ )
	    {
	      if( j > 0 )
		oDataSet_h << ", ";
	      if( *pLabel == pID->name )
		oDataSet_h << "\"" << s->initial[who][j] << "\"";
	      else
		oDataSet_h << s->initial[who][j];
	    }
	  oDataSet_h << " };" << endl;
	  oDataSet_h << "   std::vector<" << (isID? "char*":"spk_ValueType") << "> ";
	  oDataSet_h << vector_name;
	  oDataSet_h << "( " << nRecords << " );" << endl;
	  oDataSet_h << "   copy( " << carray_name << ", " << carray_name << "+" << nRecords;
	  oDataSet_h << ", " << vector_name << ".begin() );" << endl;
	}

      //
      // Create an IndData object.  The order in which the arguments
      // are passed to the IndData constructor must be strictly
      // compliant to the order in which the label strings are stored
      // in the list returned by SymbolTable::getLabels().
      //
      oDataSet_h << "   data[" << who << "] = new IndData<spk_ValueType>";
      oDataSet_h << "( " << nRecords << ", ";
      pLabel = labels->begin();
      for( int i=0; pLabel != labels->end(), i<nLabels; i++, pLabel++ )
	{
	  if( i>0 )
	    oDataSet_h << ", ";
	  const Symbol * s = table->findi( *pLabel );
	  string array_name = s->name + "_" + c_who;
	  oDataSet_h << array_name;
	}

      oDataSet_h << " );" << endl;
      oDataSet_h << endl; 
    }
  oDataSet_h << endl;

  // 
  // Extracts measurements (ie. SPK's y) from the entire data set and keep it in "measurements".
  //
  oDataSet_h << "   int nRecords = 0;" << endl;
  oDataSet_h << "   for( int i=0; i<popSize; i++ )" << endl;
  oDataSet_h << "      nRecords += data[i]->getNRecords();" << endl;
  oDataSet_h << "   " << endl;
  oDataSet_h << "   int nY = N.sum();  // # of DVs" << endl;
  oDataSet_h << "   measurements.resize( nY ); " << endl;
  oDataSet_h << "   jPrimeToj.resize( nY );" << endl;
  oDataSet_h << "   jTojPrime.resize( nRecords );" << endl;
  oDataSet_h << "   for( int i=0, m=0, j=0, jPrime=0; i<popSize; i++ )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      int nYi = data[i]->getMeasurements().size();" << endl;
  oDataSet_h << "      measurements[ SPK_VA::slice( m, nYi, 1 ) ] = data[i]->getMeasurements();" << endl;
  oDataSet_h << "      m+=nYi;" << endl;
  oDataSet_h << "      int n = data[i]->getNRecords();" << endl;
  oDataSet_h << "      for( int k=0; k<n; k++, j++ )" << endl;
  oDataSet_h << "      {" << endl;
  oDataSet_h << "         if( data[i]->" << UserStr.MDV << "[k] != 1 )" << endl;
  oDataSet_h << "         {" << endl;
  oDataSet_h << "            jPrimeToj[jPrime] = j;" << endl;
  oDataSet_h << "            jTojPrime[j] = jPrime;" << endl;
  oDataSet_h << "            jPrime++;" << endl;
  oDataSet_h << "         }" << endl;
  oDataSet_h << "      }" << endl;
  oDataSet_h << "   }" << endl;

  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ----------
  // Destructor
  // ----------
  // Free the memory allocated for the data set.
  //
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "DataSet<spk_ValueType>::~DataSet()" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int nPop = data.size();" << endl;
  oDataSet_h << "   for( int i=0; i<nPop; i++ )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      delete data[i];" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ----------------
  // Copy constructor
  // (protected)
  // ----------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "DataSet<spk_ValueType>::DataSet( const DataSet<spk_ValueType>& )" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // -------------------
  // Assignment operator
  // (protected)
  // -------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "DataSet<spk_ValueType>& DataSet<spk_ValueType>::operator=( const DataSet<spk_ValueType>& )" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------------
  // getMeasurementIndex( int j )
  // ------------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "int DataSet<spk_ValueType>::getMeasurementIndex( int recordIndex ) const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return jTojPrime[ recordIndex ];" << endl;
  oDataSet_h << "}" << endl;

  // ------------------------------
  // getRecordIndex( int jPrime )
  // ------------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "int DataSet<spk_ValueType>::getRecordIndex( int measurementIndex ) const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return jPrimeToj[ measurementIndex ];" << endl;
  oDataSet_h << "}" << endl;

  // --------------------
  // getAllMeasurements()
  // --------------------
  // Returns SPK's y.
  //
  oDataSet_h << "// Returns SPK's y." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "const SPK_VA::valarray<double> DataSet<spk_ValueType>::getAllMeasurements() const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return measurements;" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------
  // getPopSize()
  // ------------
  // Return the size of population.
  //
  oDataSet_h << "// Returns the population size." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "int DataSet<spk_ValueType>::getPopSize() const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return popSize;" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------
  // getN()
  // ------
  // Return SPK's N (ie. N[i] is the number of measurements of the i-th individual.
  //
  oDataSet_h << "// Return SPK's N (ie. N[i] is the number of measurements of the i-th individual." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "const SPK_VA::valarray<int> DataSet<spk_ValueType>::getN() const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return N;" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceAllMeasurements()
  // ------------------------
  // Replace the currently kept y with the given y'.
  //
  oDataSet_h << "// Replace the currently kept y with the given y'." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceAllMeasurements( const SPK_VA::valarray<double> & yy )" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n= data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=N[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceMeasurements( yy[ SPK_VA::slice(k, N[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "   measurements = yy;" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;


  // ------------------------
  // replacePred()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replacePred( const SPK_VA::valarray<double>& PredIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=N[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replacePred( PredIn[ SPK_VA::slice(k, N[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceRes( const SPK_VA::valarray<double>& ResIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=N[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceRes( ResIn[ SPK_VA::slice(k, N[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceWRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceWRes( const SPK_VA::valarray<double>& WResIn )"    << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=N[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceWRes( WResIn[ SPK_VA::slice(k, N[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceIPred()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceIPred( const SPK_VA::valarray<double>& iPredIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=N[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceIPred( iPredIn[ SPK_VA::slice(k, N[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceIRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceIRes( const SPK_VA::valarray<double>& iResIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=N[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceIRes( iResIn[ SPK_VA::slice(k, N[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceIWRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceIWRes( const SPK_VA::valarray<double>& iWResIn )"    << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=N[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceIWRes( iWResIn[ SPK_VA::slice(k, N[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replacePPred()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replacePPred( const SPK_VA::valarray<double>& pPredIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=N[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replacePPred( pPredIn[ SPK_VA::slice(k, N[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replacePRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replacePRes( const SPK_VA::valarray<double>& pResIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=N[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replacePRes( pResIn[ SPK_VA::slice(k, N[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replacePWRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replacePWRes( const SPK_VA::valarray<double>& pWResIn )"    << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=N[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replacePWRes( pWResIn[ SPK_VA::slice(k, N[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceCPred()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceCPred( const SPK_VA::valarray<double>& cPredIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=N[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceCPred( cPredIn[ SPK_VA::slice(k, N[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceCRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceCRes( const SPK_VA::valarray<double>& cResIn )"     << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=N[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceCRes( cResIn[ SPK_VA::slice(k, N[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  // ------------------------
  // replaceCWRes()
  // ------------------------
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "void DataSet<spk_ValueType>::replaceCWRes( const SPK_VA::valarray<double>& cWResIn )"    << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0, k=0; i<n; k+=N[i++] )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->replaceCWRes( cWResIn[ SPK_VA::slice(k, N[i], 1) ] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  if( ourTarget == POP )
    {
      // ------------------------
      // replaceEta()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replaceEta( const SPK_VA::valarray<double>& etaIn )"  << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( etaIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replaceEta( etaIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replaceEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replaceEtaRes( const SPK_VA::valarray<double>& EtaResIn )"  << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( EtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replaceEtaRes( EtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replaceWEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replaceWEtaRes( const SPK_VA::valarray<double>& WEtaResIn )" << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( WEtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replaceWEtaRes( WEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replaceIEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replaceIEtaRes( const SPK_VA::valarray<double>& iEtaResIn )"  << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( iEtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replaceIEtaRes( iEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replaceIWEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replaceIWEtaRes( const SPK_VA::valarray<double>& iWEtaResIn )" << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( iWEtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replaceIWEtaRes( iWEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replacePEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replacePEtaRes( const SPK_VA::valarray<double>& pEtaResIn )"  << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( pEtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replacePEtaRes( pEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replacePWEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replacePWEtaRes( const SPK_VA::valarray<double>& pWEtaResIn )" << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( pWEtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replacePWEtaRes( pWEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replaceCEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replaceCEtaRes( const SPK_VA::valarray<double>& cEtaResIn )"  << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( cEtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replaceCEtaRes( cEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;

      // ------------------------
      // replaceCWEtaRes()
      // ------------------------
      oDataSet_h << "template <class spk_ValueType>" << endl;
      oDataSet_h << "void DataSet<spk_ValueType>::replaceCWEtaRes( const SPK_VA::valarray<double>& cWEtaResIn )" << endl;
      oDataSet_h << "{" << endl;
      oDataSet_h << "   const int n = data.size();" << endl;
      oDataSet_h << "   const int nEta = " << myEtaLen << "; // the length of eta" << endl;
      oDataSet_h << "   assert( cWEtaResIn.size() == n * nEta );" << endl;
      oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
      oDataSet_h << "   {" << endl;
      oDataSet_h << "      data[i]->replaceCWEtaRes( cWEtaResIn[ SPK_VA::slice(i*nEta, nEta, 1) ] );" << endl;
      oDataSet_h << "   }" << endl;
      oDataSet_h << "}" << endl;
      oDataSet_h << endl;
    }

  // ---------
  // Extractor
  // ---------
  oDataSet_h << "// Extracts the contents of this class object in the SpkResultML::presentation_data form." << endl;
  oDataSet_h << "template <class spk_ValueType>" << endl;
  oDataSet_h << "std::ostream& operator<<( std::ostream& o, const DataSet<spk_ValueType>& A )" << endl;
  oDataSet_h << "{" << endl;

  if( pID == Symbol::empty() )
    {
      char mess [ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "\"ID\" is not defined." );
      SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  // Compute the number of items that are supposed to be printed out in <presentation_data>.
  // This includes User defined scalar variables, NONMEM (scalar/vector/matrix)
  // and RES/WRES.
  // 
  // THETA/ETA/OMEGA/(EPS)/(SIGMA) are vectors or matrices.
  // <presentation_data> prints out elements of these objects one by one.
  // That is, if THETA had a length of 2, it prints out THETA(1) and THETA(2) individually.
  // The names such as "THETA" is already in the symbol table, so when we get the size of
  // SymbolTable object, it returns the number that already contains the count for "THETA".
  // So, we increment the country by (2-1)=1. 
  const int nItems = t->size();
  int nColumns = nItems 
               + myThetaLen-1
               + myEtaLen-1 
               + (ourTarget==POP? (myEpsLen - 1) : 0 ) // for EPS
               + (ourTarget==POP? (myEtaLen - 1) : 0 ) // for ETARES
               + (ourTarget==POP? (myEtaLen - 1) : 0 ) // for WETARES
               + (ourTarget==POP? (myEtaLen - 1) : 0 ) // for IETARES
               + (ourTarget==POP? (myEtaLen - 1) : 0 ) // for IWETARES
               + (ourTarget==POP? (myEtaLen - 1) : 0 ) // for PETARES
               + (ourTarget==POP? (myEtaLen - 1) : 0 ) // for PWETARES
               + (ourTarget==POP? (myEtaLen - 1) : 0 ) // for CETARES
               + (ourTarget==POP? (myEtaLen - 1) : 0 ) // for CWETARES
               - (table->findi(KeyStr.OMEGA)   == Symbol::empty()? 0 : 1 )
               - (table->findi(KeyStr.SIGMA)   == Symbol::empty()? 0 : 1 );
  
  map<const string, Symbol>::const_iterator pEntry = t->begin();
  const vector<string>::const_iterator pLabelBegin = table->getLabels()->begin();
  const vector<string>::const_iterator pLabelEnd   = table->getLabels()->end();
  vector<string> whatGoesIn;  // will hold those labels in the order that actually go into the data section.
  vector<string>::const_iterator pWhatGoesIn;
  string keyWhatGoesIn;

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Open <presentation_data>
  //
  oDataSet_h << "   o << \"<\" << \"presentation_data\" << \" rows=\\\"\" << A.N.sum() << \"\\\" \";" << endl;
  oDataSet_h << "   o << \"columns=\\\"" << nColumns << "\\\">\" << endl;" << endl;

  //-----------------------------------------------------------------------------
  // Begin printing out labels.
  //
  oDataSet_h << "   o << \"<data_labels>\" << endl;" << endl;
  
  // Put ID first in the sequence
  whatGoesIn.push_back( pID->name );
  oDataSet_h << "   o << \"<label name=\\\"" << pID->name << "\\\"/>\" << endl;" << endl;
  
  // ...aaand, following ID is, all the left hand side quantities in the model definition.
  // cntColumns is initialized to 1 because the ID column is already printed out.
  int cntColumns = 1;
  for( cntColumns=1,  pEntry = t->begin(); pEntry!=t->end(); pEntry++ )
    {
      if( pEntry->first != KeyStr.ID )
	{
	  // Skip Omega and Sigma.
          // These values are not computed by Pred::eval().
	  if(    pEntry->first != KeyStr.OMEGA 
	      && pEntry->first != KeyStr.SIGMA )
	    {
	      whatGoesIn.push_back( pEntry->second.name );
	      
	      // theta: This is a vector.  So, all element values have to be printed out individually.
	      if( pEntry->first == KeyStr.THETA )
		{
		  for( int cntTheta=0; cntTheta<myThetaLen; cntTheta++ )
		    {
		      oDataSet_h << "   o << \"<label name=\\\"";
		      oDataSet_h << pEntry->second.name << "(" << cntTheta+1 << ")";
		      oDataSet_h << "\\\"/>\" << endl;" << endl;		      
		      cntColumns++;
		    }
		}
	      // eta, etares, wetares: These are vectors of length myEtaLen.
              // So, all elements values have to be printed out individually.
	      else if( pEntry->first == KeyStr.ETA
		       || pEntry->first == KeyStr.ETARES 
		       || pEntry->first == KeyStr.WETARES
		       || pEntry->first == KeyStr.IETARES 
		       || pEntry->first == KeyStr.IWETARES
		       || pEntry->first == KeyStr.PETARES 
		       || pEntry->first == KeyStr.PWETARES
		       || pEntry->first == KeyStr.CETARES 
		       || pEntry->first == KeyStr.CWETARES )
		{
		  for( int cntEta=0; cntEta<myEtaLen; cntEta++ )
		    {
		      oDataSet_h << "   o << \"<label name=\\\"";
		      oDataSet_h << pEntry->second.name << "(" << cntEta+1 << ")";
		      oDataSet_h << "\\\"/>\" << endl;" << endl;		      
		      cntColumns++;
		    }
		}
	      // eps: This is a vector of length myEpsLen.
              // So, all elements values have to be printed out individually.
	      else if( pEntry->first == KeyStr.EPS )
		{
		  for( int cntEps=0; cntEps<myEpsLen; cntEps++ )
		    {
		      oDataSet_h << "   o << \"<label name=\\\"";
		      oDataSet_h << pEntry->second.name << "(" << cntEps+1 << ")";
		      oDataSet_h << "\\\"/>\" << endl;" << endl;		      
		      cntColumns++;
		    }
		}
	      // scalar variables (user-defined variables & NONMEM reserved variables).
	      else
		{
		  oDataSet_h << "   o << \"<label name=\\\"";
		  oDataSet_h << pEntry->second.name;
		  oDataSet_h << "\\\"/>\" << endl;" << endl;
		  cntColumns++;
		}
	    }
	}
    }

  // Sanity check; is the actual number of labels (ie. columns) match the value given by the user earlier?

  if( cntColumns != nColumns )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "The number of data items (%d) does not match the number of labels (%d).",
	       cntColumns, nColumns );
      SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  oDataSet_h << "   o << \"</data_labels>\" << endl;" << endl;
  oDataSet_h << endl;
  //
  // End of labels.
  //-----------------------------------------------------------------------------
  
  //-----------------------------------------------------------------------------
  // Begin printing out computed values.
  // 
  oDataSet_h << "   for( int i=0, position=1; i<A.getPopSize(); i++ )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      for( int j=0; j<A.N[i]; j++, position++ )" << endl;
  oDataSet_h << "      {" << endl;
  oDataSet_h << "         o << \"<row position=\\\"\" << position << \"\\\">\" << endl;" << endl;

  for( cntColumns=0, pWhatGoesIn = whatGoesIn.begin(); pWhatGoesIn!=whatGoesIn.end(); pWhatGoesIn++ )
    {
      keyWhatGoesIn = SymbolTable::key( *pWhatGoesIn );
      if( keyWhatGoesIn == KeyStr.THETA )
	{
	  for( int cntTheta=0; cntTheta<myThetaLen; cntTheta++ )
	    {
	      oDataSet_h << "         o << \"<value ref=\\\"";
	      oDataSet_h << *pWhatGoesIn << "(" << cntTheta+1 << ")" << "\\\"" << ">\" << ";
	      oDataSet_h << "A.data[i]->" << *pWhatGoesIn << "[j][" << cntTheta << "]";
	      oDataSet_h << " << \"</value>\" << endl;" << endl;
	      cntColumns++;
	    }
	}
      else if( keyWhatGoesIn == KeyStr.ETA
	       || keyWhatGoesIn == KeyStr.ETARES
	       || keyWhatGoesIn == KeyStr.WETARES
	       || keyWhatGoesIn == KeyStr.IETARES
	       || keyWhatGoesIn == KeyStr.IWETARES
	       || keyWhatGoesIn == KeyStr.PETARES
	       || keyWhatGoesIn == KeyStr.PWETARES
	       || keyWhatGoesIn == KeyStr.CETARES
	       || keyWhatGoesIn == KeyStr.CWETARES )
	{
	  for( int cntEta=0; cntEta<myEtaLen; cntEta++ )
	    {
	      oDataSet_h << "         o << \"<value ref=\\\"";
	      oDataSet_h << *pWhatGoesIn << "(" << cntEta+1 << ")"<< "\\\"" << ">\" << ";
	      oDataSet_h << "A.data[i]->" << *pWhatGoesIn << "[j][" << cntEta << "]";
	      oDataSet_h << " << \"</value>\" << endl;" << endl;
	      cntColumns++;
	    }
	}
      else if( keyWhatGoesIn == KeyStr.EPS && ourTarget == POP )
	{
	  for( int cntEps=0; cntEps<myEpsLen; cntEps++ )
	    {
	      oDataSet_h << "         o << \"<value ref=\\\"";
	      oDataSet_h << *pWhatGoesIn << "(" << cntEps+1 << ")"<< "\\\"" << ">\" << ";
	      oDataSet_h << "A.data[i]->" << *pWhatGoesIn << "[j][" << cntEps << "]";
	      oDataSet_h << " << \"</value>\" << endl;" << endl;
	      cntColumns++;
	    }
	}
      else if( keyWhatGoesIn == KeyStr.OMEGA || keyWhatGoesIn == KeyStr.SIGMA )
	{
	  // ignore
	}
      else
	{
	  oDataSet_h << "         o << \"<value ref=\\\"" << *pWhatGoesIn << "\\\"" << ">\" << ";
	  oDataSet_h << "A.data[i]->" << *pWhatGoesIn << "[j]";
	  oDataSet_h << " << \"</value>\" << endl;" << endl;
	  cntColumns++;
	}
    }
  if( cntColumns != nColumns )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "The number of data items (%d), does not the number of labels (%d).",
	       cntColumns, nColumns );
      SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  oDataSet_h << "         o << \"</row>\" << endl;" << endl;
  oDataSet_h << "      }" << endl;
  oDataSet_h << "   }" << endl;
  //
  // End of computed values.
  //-----------------------------------------------------------------------------
  
  oDataSet_h << "   o << \"</\" << \"presentation_data\" << \">\";" << endl;
  //
  // Close <presentation_data>
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  oDataSet_h << "}" << endl;

  oDataSet_h << "#endif" << endl;
  oDataSet_h.close();
}
//=========================================================================================
//
// Generate Pred.h, a file declaring and defining Pred template class.
//
// Pre-conditions  - The symbol table contains the NONMEM keywords and user defined variable
//                   names needed by the user-provided model.
//
//                 - The symbol table contains entries for the data labels and aliases.
//                   The data labels have to be retrievable by calling 
//                   SymbolTable::getlabels().  
//          
//                 - The vector returned by SymbolTable::getLables() must contain
//                   the data labels in the order in which they define the data items 
//                   (ie. columns) in the data set.
//
//                 - The current working directory is writable.
//
// Post-conditions - Pred.h is saved in the current working directory.
//
//=========================================================================================
void NonmemTranslator::generatePred( const char* fPredEqn_cpp ) const
{
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Preliminaries
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  const vector<string> * const labels = table->getLabels();
  const int nLabels = labels->size();
  vector<string>::const_iterator pLabel;

  // 
  // A SymbolTable object uses a std::map object as the actual
  // table.  The usual way of retrieving the entries in the internal
  // table is though member functions provided by SymbolTable class.
  // Another way, which is used here, is to access the internal
  // table directly, which is faster and convenient.
  //
  const map<const string, Symbol> * const t = table->getTable();
  map<const string, Symbol>::const_iterator pT;

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Write into Pred.h
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //
  // Declare and define Pred template class.
  // For name binding reason, the declaration and the definition
  // are both stored in a single file: Pred.h
  // 
  ofstream oPred_h( fPred_h );
  if( !oPred_h.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Failed to create %s file.", fPred_h );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  //---------------------------------------------------------------------------------------
  //
  // Print out some description for this file.
  //
  //---------------------------------------------------------------------------------------
  oPred_h << "// " << myDescription << endl;

  //---------------------------------------------------------------------------------------
  // 
  // Header include statements.
  //
  //---------------------------------------------------------------------------------------
  oPred_h << "#ifndef PRED_H" << endl;
  oPred_h << "#define PRED_H" << endl;
  oPred_h << endl;

  oPred_h << "#include <vector>" << endl;
  oPred_h << "#include <string>" << endl;
  oPred_h << "#include <spkpred/PredBase.h>" << endl;
  oPred_h << "#include <CppAD/CppAD.h>" << endl;
  oPred_h << "#include \"DataSet.h\"" << endl;
  oPred_h << endl;
  
  
  //---------------------------------------------------------------------------------------
  //
  // Declaration of Pred class
  //
  // The template argument (ie. "ValueType" in this case)
  // must be something guaranteed that the user
  // do not use for one of their user-defined variables.
  //
  // The specification for SpkSourceML where the 
  // definition of PRED or other models appears
  // restricts user use of variable names beginning
  // with "spk_", let's take advantage of it here.
  //
  //---------------------------------------------------------------------------------------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "class Pred : public PredBase<spk_ValueType>" << endl;
  oPred_h << "{" << endl;
      
  //----------------------------------------
  // Public member declarations.
  //----------------------------------------
  oPred_h << "public:" << endl;

  // -----------
  // Constructor
  // -----------
  // This constructor takes a pointer to the DataSet (the set of
  // all individuals' data).
  //
  oPred_h << "   Pred( const DataSet<spk_ValueType>* dataIn );" << endl;

  // ----------
  // Destructor
  // ----------
  oPred_h << "   ~Pred();" << endl;

  // -----------------------
  // Public member functions
  // -----------------------
  oPred_h << "   int getNObservs( int ) const;" << endl;

  oPred_h << "   bool eval( int spk_thetaOffset, int spk_thetaLen," << endl;
  oPred_h << "              int spk_etaOffset,   int spk_etaLen," << endl;
  oPred_h << "              int spk_epsOffset,   int spk_epsLen," << endl;
  oPred_h << "              int spk_fOffset,     int spk_fLen," << endl;
  oPred_h << "              int spk_yOffset,     int spk_yLen," << endl;
  oPred_h << "              int spk_i," << endl;
  oPred_h << "              int spk_j," << endl;
  oPred_h << "              const std::vector<spk_ValueType>& spk_indepVar," << endl;
  oPred_h << "              std::vector<spk_ValueType>& spk_depVar );" << endl;
  oPred_h << endl;

  //----------------------------------------
  // Protected member declarations.
  //----------------------------------------
  oPred_h << "protected:" << endl;
  oPred_h << "   Pred();" << endl;
  oPred_h << "   Pred( const Pred& );" << endl;
  oPred_h << "   Pred & operator=( const Pred& );" << endl;

  //----------------------------------------
  // Private member declarations.
  //----------------------------------------
  oPred_h << "private:" << endl;
  oPred_h << "   const int nIndividuals;" << endl;
  oPred_h << "   const DataSet<spk_ValueType> *perm;" << endl;
  oPred_h << "   DataSet<spk_ValueType> temp;" << endl;
  oPred_h << "   mutable bool isIterationCompleted;" << endl;
  oPred_h << endl;

  // Taking care of the data items (from the data file).
  // Only the "ID" data item values are of type string,
  // otherwise all numeric, spk_ValueType.
  pLabel = labels->begin();
  for( int i=0; i<nLabels, pLabel != labels->end(); i++, pLabel++ )
    {
      bool isID = (SymbolTable::key( *pLabel ) == KeyStr.ID );

      const Symbol* s = table->findi( *pLabel );
      oPred_h << "mutable " << ( isID? "std::string" : "spk_ValueType" );
      oPred_h << " " << s->name << ";" << endl;
      if( !s->synonym.empty() )
	{
	  oPred_h << "mutable " << ( isID? "std::string" : "spk_ValueType" );
	  oPred_h << " " << s->synonym << ";" << endl;
	}
    }

  // Taking care of the user defined scalar variables.
  // The entries in the symbol table include everything,
  // the NONMEM required items such as THETA and EPS
  // and the data item labels as well as the user defined
  // scalar variable names.  The data item variables
  // are taken care in the previous step, so now
  // just pull out the user defined scalar variables.
  // The NONMEM variables are given to
  // Pred::eval() every time the iteration advances.
  for( pT = t->begin(); pT != t->end(); pT++ )
    {
      const string label    = pT->second.name;
      const string keyLabel = SymbolTable::key( label );

      // Ignore if the label is of the NONMEM required variable names.
      if( keyLabel != KeyStr.THETA 
	  && keyLabel != KeyStr.ETA 
	  && keyLabel != KeyStr.EPS 
	  //	  && keyLabel != KeyStr.PRED
	  && keyLabel != KeyStr.SIGMA
	  && keyLabel != KeyStr.OMEGA
	  && keyLabel != KeyStr.RES
	  && keyLabel != KeyStr.WRES
	  && keyLabel != KeyStr.ETARES
	  && keyLabel != KeyStr.WETARES
          && keyLabel != KeyStr.IPRED
	  && keyLabel != KeyStr.IRES
	  && keyLabel != KeyStr.IWRES
	  && keyLabel != KeyStr.IETARES
	  && keyLabel != KeyStr.IWETARES 
	  && keyLabel != KeyStr.PPRED
	  && keyLabel != KeyStr.PRES
	  && keyLabel != KeyStr.PWRES
	  && keyLabel != KeyStr.PETARES
	  && keyLabel != KeyStr.PWETARES
	  && keyLabel != KeyStr.CPRED
	  && keyLabel != KeyStr.CRES
	  && keyLabel != KeyStr.CWRES
	  && keyLabel != KeyStr.CETARES
	  && keyLabel != KeyStr.CWETARES
	  && keyLabel != KeyStr.ORGDV )
	{
	  // Ignore if the label is of the data item's.
	  if( find( labels->begin(), labels->end(), label ) 
	      == labels->end() )
	    {
	      oPred_h << "mutable spk_ValueType " << label;
	      oPred_h << ";" << endl;
	    }
	}
    }

  oPred_h << "};" << endl;
  oPred_h << endl;

  //---------------------------------------------------------------------------------------
  //
  // Definition of DataSet class
  //
  //---------------------------------------------------------------------------------------

  // -----------
  // Constructor
  // -----------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "Pred<spk_ValueType>::Pred( const DataSet<spk_ValueType>* dataIn )" << endl;
  oPred_h << ": perm( dataIn )," << endl;
  oPred_h << "  nIndividuals( " << ourPopSize << " )," << endl;
  oPred_h << "  isIterationCompleted( true )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;
  oPred_h << endl;

  // ----------
  // Destructor
  // ----------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "Pred<spk_ValueType>::~Pred()" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;
  oPred_h << endl;

  // -------------
  // getNObservs()
  // -------------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "int Pred<spk_ValueType>::getNObservs( int spk_i ) const" << endl;
  oPred_h << "{" << endl;
  oPred_h << "  return perm->data[spk_i]->" << UserStr.ID << ".size();" << endl;
  oPred_h << "}" << endl;
  oPred_h << endl;

  // ------
  // eval()
  // ------
  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "bool Pred<spk_ValueType>::eval( int spk_thetaOffset, int spk_thetaLen," << endl;
  oPred_h << "                        int spk_etaOffset,   int spk_etaLen," << endl;
  oPred_h << "                        int spk_epsOffset,   int spk_epsLen," << endl;
  oPred_h << "                        int spk_fOffset,     int spk_fLen," << endl;
  oPred_h << "                        int spk_yOffset,     int spk_yLen," << endl;
  oPred_h << "                        int spk_i," << endl;
  oPred_h << "                        int spk_j," << endl;
  oPred_h << "                        const std::vector<spk_ValueType>& spk_indepVar," << endl;
  oPred_h << "                        std::vector<spk_ValueType>& spk_depVar )" << endl;
  oPred_h << "{" << endl;

  oPred_h << "  assert( spk_thetaLen == " << myThetaLen << " );" << endl;
  oPred_h << "  assert( spk_etaLen   == " << myEtaLen << " );" << endl;
  oPred_h << "  assert( spk_epsLen   == " << myEpsLen << " );" << endl;
  oPred_h << endl;

  ///////////////////////////////////////////////////////////////////////////////////
  // Assign the current data (i,j) to appropriate variables
  // so that the user's (originally-fortran) code can easily
  // access them.
  // ex.  cp = perm->data[spk_i]->cp
  // ...given that the user's PRED code has a reference to something
  // ...like "aaa = cp * 10.0".
  //
  for( pLabel = labels->begin(); pLabel != labels->end(); pLabel++ )
    {
      const Symbol *s = table->findi( *pLabel );
      // label
      oPred_h << "   " << s->name;
      oPred_h << " = perm->data[spk_i]->";
      oPred_h << s->name << "[spk_j];" << endl;
      // synonym
      if( !s->synonym.empty() )
	{
	  oPred_h << "   " << s->synonym;
	  oPred_h << " = perm->data[spk_i]->";
	  oPred_h << s->synonym << "[spk_j];" << endl;
	}
    }
  for( int i=0; i<myThetaLen; i++ )
    {
      oPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.THETA << i+1;
      oPred_h << " = spk_indepVar.begin() + spk_thetaOffset + " << i << ";" << endl;
    }
  for( int i=0; i<myEtaLen; i++ )
    {
      oPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.ETA << i+1;
      oPred_h << " = spk_indepVar.begin() + spk_etaOffset + " << i << ";" << endl;
    }

  // EPS is only apparent in the population analysis.
  // The size of EPS vector is the order of SIGMA which is only apparent in
  // the population analysis.  So, if this is the individual level,
  // "myEpsLen" has been set to zero; thus the following loop loops zero times.
  for( int i=0; i<myEpsLen; i++ )
    {
      oPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.EPS << i+1;
      oPred_h << " = spk_indepVar.begin() + spk_epsOffset + " << i << ";" << endl;
    }
  oPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.THETA;
  oPred_h << " = spk_indepVar.begin() + spk_thetaOffset;" << endl;
  oPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.ETA;
  oPred_h << " = spk_indepVar.begin() + spk_etaOffset;" << endl;
  if( ourTarget == POP )
    {
      oPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.EPS;
      oPred_h << " = spk_indepVar.begin() + spk_epsOffset;" << endl;
    }

  oPred_h << "   spk_ValueType " << UserStr.F << " = 0.0;" << endl;

  oPred_h << "   spk_ValueType " << UserStr.Y << " = 0.0;" << endl;
  //
  ///////////////////////////////////////////////////////////////////////////////////
      
  oPred_h << "//=========================================" << endl;
  oPred_h << "// Begin User Code                         " << endl;
  oPred_h << "//-----------------------------------------" << endl;
  char ch;
  ifstream iPredEqn( fPredEqn_cpp );
  if( !iPredEqn.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Failed to open %s file.", fPredEqn_cpp );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  while( iPredEqn.get(ch) )
    oPred_h.put(ch);
  iPredEqn.close();
  remove( fPredEqn_cpp );
  oPred_h << "//-----------------------------------------" << endl;
  oPred_h << "// End User Code                           " << endl;
  oPred_h << "//=========================================" << endl;
      
  ///////////////////////////////////////////////////////////////////////////////////
  // Store the current values in temporary storage
  // : the user defined variable values and the NONMEM required variable values.
  oPred_h << UserStr.PRED << " = " << UserStr.F << ";" << endl;

  for( pT = t->begin(); pT != t->end(); pT++ )
    {
      // THETA, ETA, EPS are given Pred::eval() as vectors by the caller.
      // So, we have to treat these guys a bit different from the user variables
      // which are scalar values.
      const string label    = pT->second.name;
      const string keyLabel = SymbolTable::key( label );
      if( keyLabel == KeyStr.THETA )
	{
	  oPred_h << "   copy( " << label << ", " << label << "+spk_thetaLen, ";
          oPred_h << "temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	}
      else if( keyLabel == KeyStr.ETA )
	{
	  oPred_h << "   copy( " << label << ", " << label << "+spk_etaLen, ";
          oPred_h << "temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	}
      else if( keyLabel == KeyStr.EPS )
	{
	  oPred_h << "   copy( " << label << ", " << label << "+spk_epsLen, ";
          oPred_h << "temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	}
      else if( keyLabel == KeyStr.OMEGA 
	       || keyLabel == KeyStr.SIGMA
	       //	       || keyLabel == KeyStr.PRED
	       || keyLabel == KeyStr.RES 
	       || keyLabel == KeyStr.WRES
	       || keyLabel == KeyStr.ETARES
	       || keyLabel == KeyStr.WETARES
	       || keyLabel == KeyStr.IPRED
	       || keyLabel == KeyStr.IRES 
	       || keyLabel == KeyStr.IWRES
	       || keyLabel == KeyStr.IETARES
	       || keyLabel == KeyStr.IWETARES
	       || keyLabel == KeyStr.PPRED
	       || keyLabel == KeyStr.PRES 
	       || keyLabel == KeyStr.PWRES
	       || keyLabel == KeyStr.PETARES
	       || keyLabel == KeyStr.PWETARES
	       || keyLabel == KeyStr.CPRED
	       || keyLabel == KeyStr.CRES 
	       || keyLabel == KeyStr.CWRES
	       || keyLabel == KeyStr.CETARES
	       || keyLabel == KeyStr.CWETARES
	       || keyLabel == KeyStr.ORGDV )
	{
	  // ignore.  These values are only computed outside at the final estimate.
	}
      else
	{
	  if( find( labels->begin(), labels->end(), label ) 
	      == labels->end() )
	    {
	      oPred_h << "   temp.data[ spk_i ]->" << label;
	      oPred_h << "[ spk_j ]";
	      oPred_h << " = " << label << ";" << endl;
	    }
	}
    }   
  oPred_h << endl;

  // Saving/moving computed values to ensure a complete set of values
  // is available even when a failure occurs.
  //
  oPred_h << "   if( spk_i == " << ourPopSize << "-1 && spk_j == perm->data[spk_i]->";
  oPred_h << UserStr.ID << ".size()-1 )" << endl;
  oPred_h << "   {" << endl;
  oPred_h << "     // This means, SPK advanced in iteration." << endl;
  oPred_h << "     // Move temporary storage to permanent storage." << endl;
  oPred_h << "     isIterationCompleted = true;" << endl;
  oPred_h << "     for( int i=0; i < nIndividuals; i++ )" << endl;
  oPred_h << "     {" << endl;
  // User defined variables temp(current) => permanent
  // The user defined scalar variables
  for( pT = t->begin(); pT != t->end(); pT++ )
    {
      const string label     = pT->second.name;
      const string keyLabel = SymbolTable::key( label );
      if( keyLabel == KeyStr.OMEGA 
          || keyLabel == KeyStr.SIGMA 
	  //          || keyLabel == KeyStr.PRED 
          || keyLabel == KeyStr.RES 
          || keyLabel == KeyStr.WRES 
	  || keyLabel == KeyStr.ETARES
	  || keyLabel == KeyStr.WETARES
          || keyLabel == KeyStr.IPRED 
          || keyLabel == KeyStr.IRES 
          || keyLabel == KeyStr.IWRES 
	  || keyLabel == KeyStr.IETARES
	  || keyLabel == KeyStr.IWETARES
          || keyLabel == KeyStr.PPRED 
          || keyLabel == KeyStr.PRES 
          || keyLabel == KeyStr.PWRES 
	  || keyLabel == KeyStr.PETARES
	  || keyLabel == KeyStr.PWETARES
          || keyLabel == KeyStr.CPRED 
          || keyLabel == KeyStr.CRES 
          || keyLabel == KeyStr.CWRES 
	  || keyLabel == KeyStr.CETARES
	  || keyLabel == KeyStr.CWETARES
	  || keyLabel == KeyStr.ORGDV )
	continue;

      if( find( labels->begin(), labels->end(), label ) == labels->end() )
	{
	  oPred_h << "       perm->data[ i ]->" << label;
	  oPred_h << " = temp.data[ i ]->";
	  oPred_h << label << ";" << endl;
	}
    }      
  oPred_h << "     }" << endl;
  oPred_h << "   }" << endl;
  oPred_h << "   else" << endl;
  oPred_h << "   {" << endl;
  oPred_h << "     isIterationCompleted = false;" << endl;
  oPred_h << "   }" << endl;
  oPred_h << endl;

  ///////////////////////////////////////////////////////////////////////////////////

  // Set the output values
  oPred_h << "   spk_depVar[ spk_fOffset+spk_j ] = " << UserStr.F << ";" << endl;
  oPred_h << "   spk_depVar[ spk_yOffset+spk_j ] = " << UserStr.Y << ";" << endl;

  // Pred::eval() returns true if MDV(i,j)=0, 
  // where MDV=0 is interpreted that the statement "Missing Dependent Variable" is false.
  // In ver 0.1, it is assumed that MDV=true for all data records, 
  // so return true unconditionally.
  oPred_h << "   if( perm->data[ spk_i ]->" << UserStr.MDV << "[ spk_j ] == 0 )" << endl;
  oPred_h << "      return true;" << endl;
  oPred_h << "   else return false;" << endl;

  oPred_h << "}" << endl;

  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "Pred<spk_ValueType>::Pred()" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "Pred<spk_ValueType>::Pred( const Pred<spk_ValueType>& )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "template <class spk_ValueType>" << endl;
  oPred_h << "Pred<spk_ValueType> & Pred<spk_ValueType>::operator=( const Pred<spk_ValueType>& )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "#endif" << endl;
  oPred_h.close();
}
void NonmemTranslator::generateMonteParsNamespace() const
{
  //---------------------------------------------------------------------------------------
  // Generate the MontePars namespace if Monte is requested.
  //---------------------------------------------------------------------------------------
  if( !myIsMonte )
    {
      return;
    }
  ofstream oMontePars( fMontePars_h );
  if( !oMontePars.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Failed to create %s file.", fMontePars_h );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  oMontePars << "//==============================================================================" << endl;
  oMontePars << "// " << endl;
  oMontePars << "// " << myDescription << endl;
  oMontePars << "// " << endl;
  oMontePars << "// The namespace MontePars exports the values needed by monteDriver.cpp." << endl;
  oMontePars << "// " << endl;
  oMontePars << "// The user requested the " << (ourTarget==POP? "population":"individual");
  oMontePars << " analysis." << endl;
  oMontePars << "// " << endl;
  oMontePars << "//==============================================================================" << endl;

  oMontePars << "#ifndef MONTEPARS_H" << endl;
  oMontePars << "#define MONTEPARS_H" << endl;
  oMontePars << endl;

  oMontePars << "#include <spk/SpkValarray.h>" << endl;
  oMontePars << endl;

  oMontePars << "namespace MontePars{" << endl;
  oMontePars << "   enum METHOD { analytic, grid, plain, miser, monte };" << endl;
  oMontePars << "   const enum METHOD method = ";
  if( myIntegMethod == GRID )
    oMontePars << "grid;" << endl;
  else if( myIntegMethod == MISER )
    oMontePars << "miser;" << endl;
  else if( myIntegMethod == ANALYTIC )
    oMontePars << "analytic;" << endl;
  else //if( myIntegMethod == PLAIN )
    oMontePars << "plain;" << endl;

  oMontePars << "   const int nEval = " << myIntegNEvals << ";" << endl;
  oMontePars << "   const int c_numberEval[ nEval ] = { ";
  for( int i=0; i<myIntegNEvals; i++ )
    {
      if( i > 0 )
        oMontePars << ", ";
      oMontePars << myIntegNumberEvals[i];
    }
  oMontePars << " };" << endl;
  oMontePars << "   const SPK_VA::valarray<int> numberEval( c_numberEval, nEval );" << endl;
  oMontePars << "};" << endl;

  oMontePars << endl;

  oMontePars << "#endif" << endl;
}
/*
void NonmemTranslator::generateODEPred( const char* fPkEqn_cpp, 
                                        const char* fDiffEqn_cpp, 
					const char* fErrorEqn_cpp ) const
{
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Preliminaries
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //
  // labels is an array of data labels.  The order in which the labels are stored
  // must not be disturbed.
  //
  const vector<string> * const labels = table->getLabels();
  const int nLabels = labels->size();
  vector<string>::const_iterator pLabel;

  // 
  // A SymbolTable object uses a std::map object as the actual
  // table.  The usual way of retrieving the entries in the internal
  // table is though member functions provided by SymbolTable class.
  // Another way, which is used here, is to access the internal
  // table directly, which is faster and convenient.
  //
  const map<const string, Symbol> * const t = table->getTable();
  map<const string, Symbol>::const_iterator pT;

  char ch;

  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  //
  // Write into ODEPred.h
  //
  //+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //
  // Declare and define Pred template class.
  // For name binding reason, the declaration and the definition
  // are both stored in a single file: Pred.h
  // 
  ofstream oODEPred_h( fODEPred_h );
  if( !oODEPred_h.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Failed to create %s file.", fODEPred_h );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  //---------------------------------------------------------------------------------------
  //
  // Print out some description for this file.
  //
  //---------------------------------------------------------------------------------------
  oODEPred_h << "// " << myDescription << endl;

  //---------------------------------------------------------------------------------------
  // 
  // Header include statements.
  //
  //---------------------------------------------------------------------------------------
  oODEPred_h << "#ifndef ODEPRED_H" << endl;
  oODEPred_h << "#define ODEPRED_H" << endl;
  oODEPred_h << endl;

  oODEPred_h << "#include <vector>" << endl;
  oODEPred_h << "#include <string>" << endl;
  oODEPred_h << "#include <spkpred/ODEPredBase.h>" << endl;
  oODEPred_h << "#include <CppAD/CppAD.h>" << endl;
  oODEPred_h << "#include \"DataSet.h\"" << endl;
  oODEPred_h << endl;
  
  
  //---------------------------------------------------------------------------------------
  //
  // Declaration of ODEPred class
  //
  // The template argument (ie. "ValueType" in this case)
  // must be something guaranteed that the user
  // do not use for one of their user-defined variables.
  //
  // The specification for SpkSourceML where the 
  // definition of PRED or other models appears
  // restricts user use of variable names beginning
  // with "spk_", let's take advantage of it here.
  //
  //---------------------------------------------------------------------------------------
  oODEPred_h << "template <class spk_ValueType>" << endl;
  oODEPred_h << "class ODEPred : public ODEPredBase<spk_ValueType>" << endl;
  oODEPred_h << "{" << endl;
      
  //----------------------------------------
  // Public member declarations.
  //----------------------------------------
  oODEPred_h << "public:" << endl;

  // -----------
  // Constructor
  // -----------
  // This constructor takes a pointer to the DataSet (the set of
  // all individuals' data).
  //
  oODEPred_h << "   ODEPred( const DataSet<spk_ValueType>* dataIn );" << endl;

  // ----------
  // Destructor
  // ----------
  oODEPred_h << "   ~ODEPred();" << endl;

  // -----------------------
  // Public member functions
  // -----------------------
  oODEPred_h << "   int getNObservs( int ) const;" << endl;

  oODEPred_h << "   bool eval     ( int spk_thetaOffset, int spk_thetaLen,"                << endl;
  oODEPred_h << "                   int spk_etaOffset,   int spk_etaLen,"                  << endl;
  oODEPred_h << "                   int spk_epsOffset,   int spk_epsLen,"                  << endl;
  oODEPred_h << "                   int spk_fOffset,     int spk_fLen,"                    << endl;
  oODEPred_h << "                   int spk_yOffset,     int spk_yLen,"                    << endl;
  oODEPred_h << "                   int spk_i,"                                            << endl;
  oODEPred_h << "                   int spk_j,"                                            << endl;
  oODEPred_h << "                   const std::vector<spk_ValueType>& spk_indepVar,"       << endl;
  oODEPred_h << "                   std::vector<spk_ValueType>& spk_depVar );"             << endl;
  oODEPred_h << endl;

  oODEPred_h << "   bool evalError( int spk_thetaOffset, int spk_thetaLen,"                << endl;
  oODEPred_h << "                   int spk_etaOffset,   int spk_etaLen,"                  << endl;
  oODEPred_h << "                   int spk_epsOffset,   int spk_epsLen,"                  << endl;
  oODEPred_h << "                   int spk_fOffset,     int spk_fLen,"                    << endl;
  oODEPred_h << "                   int spk_yOffset,     int spk_yLen,"                    << endl;
  oODEPred_h << "                   int spk_i,"                                            << endl;
  oODEPred_h << "                   int spk_j,"                                            << endl;
  oODEPred_h << "                   const std::vector<spk_ValueType>& spk_indepVar,"       << endl;
  oODEPred_h << "                   std::vector<spk_ValueType>& spk_depVar );"         << endl;
  oODEPred_h << endl;

  oODEPred_h << "   bool evalPK   ( int thetaOffset, int thetaLen,"                        << endl;
  oODEPred_h << "                   int etaOffset,   int etaLen,"                          << endl;
  oODEPred_h << "                   int spk_i,"                                            << endl;
  oODEPred_h << "                   int spk_j,"                                            << endl;
  oODEPred_h << "                   const std::vector<spk_ValueType>& spk_indepVar );" << endl;
  oODEPred_h << endl;

  oODEPred_h << "   bool evalODE  ( int thetaOffset, int thetaLen,"                        << endl;
  oODEPred_h << "                   int spk_i,"                                            << endl;
  oODEPred_h << "                   int spk_j,"                                            << endl;
  oODEPred_h << "                   const std::vector<spk_ValueType>& spk_indepVar );" << endl;
  oODEPred_h << endl;

  //----------------------------------------
  // Protected member declarations.
  //----------------------------------------
  oODEPred_h << "protected:" << endl;
  oODEPred_h << "   ODEPred();" << endl;
  oODEPred_h << "   ODEPred( const ODEPred& );" << endl;
  oODEPred_h << "   ODEPred & operator=( const ODEPred& );" << endl;

  //----------------------------------------
  // Private member declarations.
  //----------------------------------------
  oODEPred_h << "private:" << endl;
  oODEPred_h << "   const int nIndividuals;" << endl;
  oODEPred_h << "   const DataSet<spk_ValueType> *perm;" << endl;
  oODEPred_h << "   DataSet<spk_ValueType> temp;" << endl;
  oODEPred_h << "   mutable bool isIterationCompleted;" << endl;
  oODEPred_h << endl;

  // Taking care of the data items (from the data file).
  // Only the "ID" data item values are of type string,
  // otherwise all numeric, spk_ValueType.
  pLabel = labels->begin();
  for( int i=0; i<nLabels, pLabel != labels->end(); i++, pLabel++ )
    {
      bool isID = (SymbolTable::key( *pLabel ) == KeyStr.ID );

      const Symbol* s = table->findi( *pLabel );
      oODEPred_h << "   mutable " << ( isID? "std::string" : "spk_ValueType" );
      oODEPred_h << " " << s->name << ";" << endl;
      if( !s->synonym.empty() )
	{
	  oODEPred_h << "   mutable " << ( isID? "std::string" : "spk_ValueType" );
	  oODEPred_h << " " << s->synonym << ";" << endl;
	}
    }

  // Taking care of the user defined scalar variables.
  // The entries in the symbol table include everything,
  // the NONMEM required items such as THETA and EPS
  // and the data item labels as well as the user defined
  // scalar variable names.  The data item variables
  // are taken care in the previous step, so now
  // just pull out the user defined scalar variables.
  // The NONMEM variables are given to
  // ODEPred::eval() every time the iteration advances.
  for( pT = t->begin(); pT != t->end(); pT++ )
    {
      const string label    = pT->second.name;
      const string keyLabel = SymbolTable::key( label );
      // Ignore if the label is of the data item's.
      if( find( labels->begin(), labels->end(), label ) 
	      == labels->end() )
	{
	  // Ignore if the label is of the NONMEM required variable names.
	  // They have to be declared in the body of PRED() because
	  // they (theta, eta, eps) have to be "const" double array.
	  if( keyLabel != KeyStr.THETA 
	      && keyLabel != KeyStr.ETA 
	      && keyLabel != KeyStr.EPS )
              //&& keyLabel != KeyStr.PRED )
	    {
	      if( keyLabel == KeyStr.DADT 
		  || keyLabel == KeyStr.A
		  || keyLabel == KeyStr.P )
		{
		  oODEPred_h << "   typename std::vector<spk_ValueType>::iterator " << label;
		  oODEPred_h << ";" << endl;
		}
	      else
		{
		  oODEPred_h << "   mutable spk_ValueType " << label;
		  oODEPred_h << ";" << endl;
		}
	    }
	}
    }

  oODEPred_h << "};" << endl;
  oODEPred_h << endl;

  //---------------------------------------------------------------------------------------
  //
  // Definition of DataSet class
  //
  //---------------------------------------------------------------------------------------

  // -----------
  // Constructor
  // -----------
  oODEPred_h << "template <class spk_ValueType>" << endl;
  oODEPred_h << "ODEPred<spk_ValueType>::ODEPred( const DataSet<spk_ValueType>* dataIn )" << endl;
  oODEPred_h << ": perm( dataIn )," << endl;
  oODEPred_h << "  nIndividuals( " << ourPopSize << " )," << endl;
  oODEPred_h << "  isIterationCompleted( true )" << endl;
  oODEPred_h << "{" << endl;
  oODEPred_h << "}" << endl;
  oODEPred_h << endl;

  // ----------
  // Destructor
  // ----------
  oODEPred_h << "template <class spk_ValueType>" << endl;
  oODEPred_h << "ODEPred<spk_ValueType>::~ODEPred()" << endl;
  oODEPred_h << "{" << endl;
  oODEPred_h << "}" << endl;
  oODEPred_h << endl;

  // -------------
  // getNObservs()
  // -------------
  oODEPred_h << "template <class spk_ValueType>" << endl;
  oODEPred_h << "int ODEPred<spk_ValueType>::getNObservs( int spk_i ) const" << endl;
  oODEPred_h << "{" << endl;
  oODEPred_h << "  return perm->data[spk_i]->" << UserStr.ID << ".size();" << endl;
  oODEPred_h << "}" << endl;
  oODEPred_h << endl;

  // -------
  // eval()
  // -------
  oODEPred_h << "template <class spk_ValueType>" << endl;
  oODEPred_h << "bool " << endl;
  oODEPred_h << "ODEPred<spk_ValueType>::eval    ( int spk_thetaOffset, int spk_thetaLen,"          << endl;
  oODEPred_h << "                                  int spk_etaOffset,   int spk_etaLen,"            << endl;
  oODEPred_h << "                                  int spk_epsOffset,   int spk_epsLen,"            << endl;
  oODEPred_h << "                                  int spk_fOffset,     int spk_fLen,"              << endl;
  oODEPred_h << "                                  int spk_yOffset,     int spk_yLen,"              << endl;
  oODEPred_h << "                                  int spk_i,"                                      << endl;
  oODEPred_h << "                                  int spk_j,"                                      << endl;
  oODEPred_h << "                                  const std::vector<spk_ValueType>& spk_indepVar," << endl;
  oODEPred_h << "                                  std::vector<spk_ValueType>& spk_depVar )"        << endl;
  oODEPred_h << "{" << endl;
  oODEPred_h << "   assert( spk_thetaLen == " << myThetaLen << " );" << endl;
  oODEPred_h << "   assert( spk_etaLen   == " << myEtaLen << " );"   << endl;
  oODEPred_h << "   assert( spk_epsLen   == " << myEpsLen << " );"   << endl;
  oODEPred_h << endl;

  ///////////////////////////////////////////////////////////////////////////////////
  // Assign the current data (i,j) to appropriate variables
  // so that the user's (originally-fortran) code can easily
  // access them.
  // ex.  cp = perm->data[spk_i]->cp
  // ...given that the user's PRED code has a reference to something
  // ...like "aaa = cp * 10.0".
  //
  for( pT = t->begin(); pT != t->end(); pT++ )
    {
      // THETA, ETA, EPS are given ODEPred::eval() as vectors by the caller.
      // So, we have to treat these guys a bit different from the user variables
      // which are scalar values.
      const string label    = pT->second.name;
      const string keyLabel = SymbolTable::key( label );
      if( keyLabel == KeyStr.OMEGA 
	       || keyLabel == KeyStr.SIGMA 
               || keyLabel == KeyStr.PRED
	       || keyLabel == KeyStr.RES
	       || keyLabel == KeyStr.WRES 
	       || keyLabel == KeyStr.ETARES 
	       || keyLabel == KeyStr.WETARES
	       || keyLabel == KeyStr.IPRED
	       || keyLabel == KeyStr.IRES
	       || keyLabel == KeyStr.IWRES 
	       || keyLabel == KeyStr.IETARES 
	       || keyLabel == KeyStr.IWETARES
	       || keyLabel == KeyStr.PPRED
	       || keyLabel == KeyStr.PRES
	       || keyLabel == KeyStr.PWRES 
	       || keyLabel == KeyStr.PETARES 
	       || keyLabel == KeyStr.PWETARES
	       || keyLabel == KeyStr.CPRED
	       || keyLabel == KeyStr.CRES
	       || keyLabel == KeyStr.CWRES 
	       || keyLabel == KeyStr.CETARES 
	       || keyLabel == KeyStr.CWETARES
	       || keyLabel == KeyStr.ORGDV
	       || keyLabel == KeyStr.THETA
	       || keyLabel == KeyStr.ETA
	       || keyLabel == KeyStr.EPS )
	{
	}
      else if( keyLabel == KeyStr.DADT 
	       || keyLabel == KeyStr.A
	       || keyLabel == KeyStr.P )
	{
	  oODEPred_h << "   " << label << " = perm->data[spk_i]->" << label << "[spk_j].begin();" << endl;
	}
      else
	{
	  oODEPred_h << "   " << label << " = perm->data[spk_i]->" << label << "[spk_j];" << endl;
	}
    }

  for( int i=0; i<myThetaLen; i++ )
    {
      oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.THETA << i+1;
      oODEPred_h << " = spk_indepVar.begin() + spk_thetaOffset + " << i << ";" << endl;
    }
  for( int i=0; i<myEtaLen; i++ )
    {
      oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.ETA << i+1;
      oODEPred_h << " = spk_indepVar.begin() + spk_etaOffset + " << i << ";" << endl;
    }

  // EPS is only applicable to the population analysis.
  // The size of EPS vector is the order of SIGMA which is only apparent in
  // the population analysis.  So, if this is the individual level,
  // "myEpsLen" has been set to zero; thus the following loop loops zero times.
  for( int i=0; i<myEpsLen; i++ )
    {
      oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.EPS << i+1;
      oODEPred_h << " = spk_indepVar.begin() + spk_epsOffset + " << i << ";" << endl;
    }
  oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.THETA;
  oODEPred_h << " = spk_indepVar.begin() + spk_thetaOffset;" << endl;
  oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.ETA;
  oODEPred_h << " = spk_indepVar.begin() + spk_etaOffset;" << endl;
  if( ourTarget == POP )
    {
      oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.EPS;
      oODEPred_h << " = spk_indepVar.begin() + spk_epsOffset;" << endl;
    }

  oODEPred_h << "   spk_ValueType "                            << UserStr.F << " = 0.0;"  << endl;
  oODEPred_h << "   evalPK(   spk_thetaOffset, spk_thetaLen,"  << endl;
  oODEPred_h << "             spk_etaOffset,   spk_etaLen,"    << endl;
  oODEPred_h << "             spk_i, spk_j,"                   << endl;
  oODEPred_h << "             spk_indepVar );"                 << endl;
  oODEPred_h << "   advanceODE(spk_i, spk_j, F);"              << endl;
  oODEPred_h << "   spk_depVar[ spk_fOffset+spk_j ] = "        << UserStr.F << ";" << endl;
  oODEPred_h << "   evalError(spk_thetaOffset, spk_thetaLen,"  << endl;
  oODEPred_h << "             spk_etaOffset,   spk_etaLen,  "  << endl;
  oODEPred_h << "             spk_epsOffset,   spk_epsLen,  "  << endl;
  oODEPred_h << "             spk_fOffset,     spk_fLen,    "  << endl;
  oODEPred_h << "             spk_yOffset,     spk_yLen,    "  << endl;
  oODEPred_h << "             spk_i, "                         << endl;
  oODEPred_h << "             spk_j, "                         << endl;
  oODEPred_h << "             spk_indepVar,    spk_depVar ); " << endl;

  ///////////////////////////////////////////////////////////////////////////////////
  // Store the current values in temporary storage
  // : the user defined variable values and the NONMEM required variable values.
  //  oODEPred_h << "   " << UserStr.PRED << " = " << UserStr.F << ";" << endl;

  for( pT = t->begin(); pT != t->end(); pT++ )
    {
      // THETA, ETA, EPS are given ODEPred::eval() as vectors by the caller.
      // So, we have to treat these guys a bit different from the user variables
      // which are scalar values.
      const string label    = pT->second.name;
      const string keyLabel = SymbolTable::key( label );
      if( keyLabel == KeyStr.THETA )
	{
	  oODEPred_h << "   copy( " << label << ", " << label << "+spk_thetaLen, ";
          oODEPred_h << "   temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	}
      else if( keyLabel == KeyStr.ETA )
	{
	  oODEPred_h << "   copy( " << label << ", " << label << "+spk_etaLen, ";
          oODEPred_h << "   temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	}
      else if( keyLabel == KeyStr.EPS )
	{
	  oODEPred_h << "   copy( " << label << ", " << label << "+spk_epsLen, ";
          oODEPred_h << "   temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	}
      else if( keyLabel == KeyStr.OMEGA 
	       || keyLabel == KeyStr.SIGMA
               || keyLabel == KeyStr.PRED 
	       || keyLabel == KeyStr.RES
	       || keyLabel == KeyStr.WRES 
	       || keyLabel == KeyStr.ETARES 
	       || keyLabel == KeyStr.WETARES
	       || keyLabel == KeyStr.IPRED 
	       || keyLabel == KeyStr.IRES
	       || keyLabel == KeyStr.IWRES 
	       || keyLabel == KeyStr.IETARES 
	       || keyLabel == KeyStr.IWETARES
	       || keyLabel == KeyStr.PPRED 
	       || keyLabel == KeyStr.PRES
	       || keyLabel == KeyStr.PWRES 
	       || keyLabel == KeyStr.PETARES 
	       || keyLabel == KeyStr.PWETARES
	       || keyLabel == KeyStr.CPRED 
	       || keyLabel == KeyStr.CRES
	       || keyLabel == KeyStr.CWRES 
	       || keyLabel == KeyStr.CETARES 
	       || keyLabel == KeyStr.CWETARES
	       || keyLabel == KeyStr.ORGDV )
	{
	  // ignore.  These values are only computed outside at the final estimate.
	}
      else if( keyLabel == KeyStr.DADT || keyLabel == KeyStr.A || keyLabel == KeyStr.P )
	{
	  oODEPred_h << "   copy( " << label << ", " << label << "+";
	  oODEPred_h << "temp.data[ spk_i ]->" << label << "[ spk_j ].size(), ";
          oODEPred_h << "temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	}
      else
	{
	  if( find( labels->begin(), labels->end(), label ) 
	      == labels->end() )
	    {
	      oODEPred_h << "   temp.data[ spk_i ]->" << label;
	      oODEPred_h << "[ spk_j ]";
	      oODEPred_h << " = " << label << ";" << endl;
	    }
	}
    }   
  oODEPred_h << endl;

  // Saving/moving computed values to ensure a complete set of values
  // is available even when a failure occurs.
  //
  oODEPred_h << "   if( spk_i == " << ourPopSize << "-1 && spk_j == perm->data[spk_i]->";
  oODEPred_h << UserStr.ID << ".size()-1 )" << endl;
  oODEPred_h << "   {" << endl;
  oODEPred_h << "     // This means, SPK advanced in iteration." << endl;
  oODEPred_h << "     // Move temporary storage to permanent storage." << endl;
  oODEPred_h << "     isIterationCompleted = true;" << endl;
  oODEPred_h << "     for( int i=0; i < nIndividuals; i++ )" << endl;
  oODEPred_h << "     {" << endl;
  // User defined variables temp(current) => permanent
  // The user defined scalar variables
  for( pT = t->begin(); pT != t->end(); pT++ )
    {
      const string label    = pT->second.name;
      const string keyLabel = SymbolTable::key( label );
      if( keyLabel == KeyStr.OMEGA 
          || keyLabel == KeyStr.SIGMA
          || keyLabel == KeyStr.PRED 
          || keyLabel == KeyStr.RES
          || keyLabel == KeyStr.WRES 
	  || keyLabel == KeyStr.ETARES
	  || keyLabel == KeyStr.WETARES
          || keyLabel == KeyStr.IPRED 
          || keyLabel == KeyStr.IRES
          || keyLabel == KeyStr.IWRES 
	  || keyLabel == KeyStr.IETARES
	  || keyLabel == KeyStr.IWETARES
          || keyLabel == KeyStr.PPRED 
          || keyLabel == KeyStr.PRES
          || keyLabel == KeyStr.PWRES 
	  || keyLabel == KeyStr.PETARES
	  || keyLabel == KeyStr.PWETARES
          || keyLabel == KeyStr.CPRED 
          || keyLabel == KeyStr.CRES
          || keyLabel == KeyStr.CWRES 
	  || keyLabel == KeyStr.CETARES
	  || keyLabel == KeyStr.CWETARES
	  || keyLabel == KeyStr.ORGDV )
	continue;

      if( find( labels->begin(), labels->end(), label ) == labels->end() )
	{
	  oODEPred_h << "       perm->data[ i ]->" << label;
	  oODEPred_h << " = temp.data[ i ]->";
	  oODEPred_h << label << ";" << endl;
	}
    }      
  oODEPred_h << "     }" << endl;
  oODEPred_h << "   }" << endl;
  oODEPred_h << "   else" << endl;
  oODEPred_h << "   {" << endl;
  oODEPred_h << "     isIterationCompleted = false;" << endl;
  oODEPred_h << "   }" << endl;
  oODEPred_h << endl;

  ///////////////////////////////////////////////////////////////////////////////////
  // ODEPred::eval() returns true if MDV(i,j)=0, 
  // where MDV=0 is interpreted that the statement "Missing Dependent Variable" is false.
  // In ver 0.1, it is assumed that MDV=true for all data records, 
  // so return true unconditionally.
  oODEPred_h << "   if( perm->data[ spk_i ]->" << UserStr.MDV << "[ spk_j ] == 0 )" << endl;
  oODEPred_h << "      return true;" << endl;
  oODEPred_h << "   else return false;" << endl;

  oODEPred_h << "}" << endl;

  // --------
  // evalPK()
  // --------
  oODEPred_h << "template <class spk_ValueType>" << endl;
  oODEPred_h << "bool " << endl;
  oODEPred_h << "ODEPred<spk_ValueType>::evalPK   ( int spk_thetaOffset, int spk_thetaLen,"           << endl;
  oODEPred_h << "                                   int spk_etaOffset,   int spk_etaLen,"             << endl;
  oODEPred_h << "                                   int spk_i,"                                       << endl;
  oODEPred_h << "                                   int spk_j,"                                       << endl;
  oODEPred_h << "                                   const std::vector<spk_ValueType>& spk_indepVar )" << endl;
  oODEPred_h << "{" << endl;

  oODEPred_h << "   assert( spk_thetaLen == " << myThetaLen << " );" << endl;
  oODEPred_h << "   assert( spk_etaLen   == " << myEtaLen << " );"   << endl;
  oODEPred_h << endl;

  ///////////////////////////////////////////////////////////////////////////////////
  // Assign the current data (i,j) to appropriate variables
  // so that the user's (originally-fortran) code can easily
  // access them.
  // ex.  cp = perm->data[spk_i]->cp
  // ...given that the user's PRED code has a reference to something
  // ...like "aaa = cp * 10.0".
  //
  for( pLabel = labels->begin(); pLabel != labels->end(); pLabel++ )
    {
      const Symbol *s = table->findi( *pLabel );
      // label
      oODEPred_h << "   " << s->name;
      oODEPred_h << " = perm->data[spk_i]->";
      oODEPred_h << s->name << "[spk_j];" << endl;
      // synonym
      if( !s->synonym.empty() )
	{
	  oODEPred_h << "   " << s->synonym;
	  oODEPred_h << " = perm->data[spk_i]->";
	  oODEPred_h << s->synonym << "[spk_j];" << endl;
	}
    }
  for( int i=0; i<myThetaLen; i++ )
    {
      oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.THETA << i+1;
      oODEPred_h << " = spk_indepVar.begin() + spk_thetaOffset + " << i << ";" << endl;
    }
  for( int i=0; i<myEtaLen; i++ )
    {
      oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.ETA << i+1;
      oODEPred_h << " = spk_indepVar.begin() + spk_etaOffset + " << i << ";" << endl;
    }

  oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.THETA;
  oODEPred_h << " = spk_indepVar.begin() + spk_thetaOffset;" << endl;
  oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.ETA;
  oODEPred_h << " = spk_indepVar.begin() + spk_etaOffset;" << endl;
  //
  ///////////////////////////////////////////////////////////////////////////////////
      
  oODEPred_h << "//=========================================" << endl;
  oODEPred_h << "// Begin User Code for $PK                 " << endl;
  oODEPred_h << "//-----------------------------------------" << endl;
  ifstream iPkEqn( fPkEqn_cpp );
  if( !iPkEqn.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Failed to open %s file.", fPkEqn_cpp );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  while( iPkEqn.get(ch) )
    oODEPred_h.put(ch);
  iPkEqn.close();
  remove( fPkEqn_cpp );
  oODEPred_h << "//-----------------------------------------" << endl;
  oODEPred_h << "// End User Code for $PK                   " << endl;
  oODEPred_h << "//=========================================" << endl;
      
  oODEPred_h << "}" << endl;
  oODEPred_h << endl;
  ///////////////////////////////////////////////////////////////////////////////////
  
  
  // ---------
  // evalODE()
  // ---------
  oODEPred_h << "template <class spk_ValueType>" << endl;
  oODEPred_h << "bool " << endl;
  oODEPred_h << "ODEPred<spk_ValueType>::evalODE  ( int spk_thetaOffset, int spk_thetaLen,"           << endl;
  oODEPred_h << "                                   int spk_i, int spk_j,"                            << endl;
  oODEPred_h << "                                   const std::vector<spk_ValueType>& spk_indepVar )" << endl;
  oODEPred_h << "{" << endl;

  oODEPred_h << "   assert( spk_thetaLen == " << myThetaLen << " );" << endl;
  oODEPred_h << endl;

  ///////////////////////////////////////////////////////////////////////////////////
  // Assign the current data (i,j) to appropriate variables
  // so that the user's (originally-fortran) code can easily
  // access them.
  // ex.  cp = perm->data[spk_i]->cp
  // ...given that the user's PRED code has a reference to something
  // ...like "aaa = cp * 10.0".
  //
  for( pLabel = labels->begin(); pLabel != labels->end(); pLabel++ )
    {
      const Symbol *s = table->findi( *pLabel );
      // label
      oODEPred_h << "   " << s->name;
      oODEPred_h << " = perm->data[spk_i]->";
      oODEPred_h << s->name << "[spk_j];" << endl;
      // synonym
      if( !s->synonym.empty() )
	{
	  oODEPred_h << "   " << s->synonym;
	  oODEPred_h << " = perm->data[spk_i]->";
	  oODEPred_h << s->synonym << "[spk_j];" << endl;
	}
    }
  for( int i=0; i<myThetaLen; i++ )
    {
      oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.THETA << i+1;
      oODEPred_h << " = spk_indepVar.begin() + spk_thetaOffset + " << i << ";" << endl;
    }
  oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.THETA;
  oODEPred_h << " = spk_indepVar.begin() + spk_thetaOffset;" << endl;
  //
  ///////////////////////////////////////////////////////////////////////////////////
      
  oODEPred_h << "//=========================================" << endl;
  oODEPred_h << "// Begin User Code for $DES                " << endl;
  oODEPred_h << "//-----------------------------------------" << endl;
  ifstream iDiffEqn( fDiffEqn_cpp );
  if( !iDiffEqn.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Failed to open %s file.", fDiffEqn_cpp );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  while( iDiffEqn.get(ch) )
    oODEPred_h.put(ch);
  iDiffEqn.close();
  remove( fDiffEqn_cpp );
  oODEPred_h << "//-----------------------------------------" << endl;
  oODEPred_h << "// End User Code for $DES                  " << endl;
  oODEPred_h << "//=========================================" << endl;
      
  oODEPred_h << endl;
  oODEPred_h << "}" << endl;
  oODEPred_h << endl;
  ///////////////////////////////////////////////////////////////////////////////////

  // -----------
  // evalError()
  // -----------
  oODEPred_h << "template <class spk_ValueType>" << endl;
  oODEPred_h << "bool " << endl;
  oODEPred_h << "ODEPred<spk_ValueType>::evalError( int spk_thetaOffset, int spk_thetaLen,"           << endl;
  oODEPred_h << "                                   int spk_etaOffset,   int spk_etaLen,"             << endl;
  oODEPred_h << "                                   int spk_epsOffset,   int spk_epsLen,"             << endl;
  oODEPred_h << "                                   int spk_fOffset,     int spk_fLen,"               << endl;
  oODEPred_h << "                                   int spk_yOffset,     int spk_yLen,"               << endl;
  oODEPred_h << "                                   int spk_i,"                                       << endl;
  oODEPred_h << "                                   int spk_j,"                                       << endl;
  oODEPred_h << "                                   const std::vector<spk_ValueType>& spk_indepVar,"  << endl;
  oODEPred_h << "                                   std::vector<spk_ValueType>& spk_depVar )"         << endl;
  oODEPred_h << "{" << endl;

  oODEPred_h << "   assert( spk_thetaLen == " << myThetaLen << " );" << endl;
  oODEPred_h << "   assert( spk_etaLen   == " << myEtaLen << " );"   << endl;
  oODEPred_h << "   assert( spk_epsLen   == " << myEpsLen << " );"   << endl;
  oODEPred_h << endl;

  ///////////////////////////////////////////////////////////////////////////////////
  // Assign the current data (i,j) to appropriate variables
  // so that the user's (originally-fortran) code can easily
  // access them.
  // ex.  cp = perm->data[spk_i]->cp
  // ...given that the user's PRED code has a reference to something
  // ...like "aaa = cp * 10.0".
  //
  for( pLabel = labels->begin(); pLabel != labels->end(); pLabel++ )
    {
      const Symbol *s = table->findi( *pLabel );
      // label
      oODEPred_h << "   " << s->name;
      oODEPred_h << " = perm->data[spk_i]->";
      oODEPred_h << s->name << "[spk_j];" << endl;
      // synonym
      if( !s->synonym.empty() )
	{
	  oODEPred_h << "   " << s->synonym;
	  oODEPred_h << " = perm->data[spk_i]->";
	  oODEPred_h << s->synonym << "[spk_j];" << endl;
	}
    }
  for( int i=0; i<myThetaLen; i++ )
    {
      oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.THETA << i+1;
      oODEPred_h << " = spk_indepVar.begin() + spk_thetaOffset + " << i << ";" << endl;
    }
  for( int i=0; i<myEtaLen; i++ )
    {
      oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.ETA << i+1;
      oODEPred_h << " = spk_indepVar.begin() + spk_etaOffset + " << i << ";" << endl;
    }

  // EPS is only applicable to the population analysis.
  // The size of EPS vector is the order of SIGMA which is only apparent in
  // the population analysis.  So, if this is the individual level,
  // "myEpsLen" has been set to zero; thus the following loop loops zero times.
  for( int i=0; i<myEpsLen; i++ )
    {
      oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.EPS << i+1;
      oODEPred_h << " = spk_indepVar.begin() + spk_epsOffset + " << i << ";" << endl;
    }
  oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.THETA;
  oODEPred_h << " = spk_indepVar.begin() + spk_thetaOffset;" << endl;
  oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.ETA;
  oODEPred_h << " = spk_indepVar.begin() + spk_etaOffset;" << endl;
  if( ourTarget == POP )
    {
      oODEPred_h << "   typename std::vector<spk_ValueType>::const_iterator " << UserStr.EPS;
      oODEPred_h << " = spk_indepVar.begin() + spk_epsOffset;" << endl;
    }

  oODEPred_h << "   spk_ValueType " << UserStr.Y << " = 0.0;" << endl;
  //
  ///////////////////////////////////////////////////////////////////////////////////
      
  oODEPred_h << "//=========================================" << endl;
  oODEPred_h << "// Begin User Code for $ERROR              " << endl;
  oODEPred_h << "//-----------------------------------------" << endl;
  ifstream iErrorEqn( fErrorEqn_cpp );
  if( !iErrorEqn.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Failed to open %s file.", fErrorEqn_cpp );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  while( iErrorEqn.get(ch) )
    oODEPred_h.put(ch);
  iErrorEqn.close();
  remove( fErrorEqn_cpp );
  oODEPred_h << "//-----------------------------------------" << endl;
  oODEPred_h << "// End User Code for $ERROR                " << endl;
  oODEPred_h << "//=========================================" << endl;   
  oODEPred_h << "   spk_depVar[ spk_yOffset+spk_j ] = " << UserStr.Y << ";" << endl;

  oODEPred_h << "}" << endl;

  oODEPred_h << "template <class spk_ValueType>" << endl;
  oODEPred_h << "ODEPred<spk_ValueType>::ODEPred()" << endl;
  oODEPred_h << "{" << endl;
  oODEPred_h << "}" << endl;

  oODEPred_h << "template <class spk_ValueType>" << endl;
  oODEPred_h << "ODEPred<spk_ValueType>::ODEPred( const ODEPred<spk_ValueType>& )" << endl;
  oODEPred_h << "{" << endl;
  oODEPred_h << "}" << endl;

  oODEPred_h << "template <class spk_ValueType>" << endl;
  oODEPred_h << "ODEPred<spk_ValueType> & ODEPred<spk_ValueType>::operator=( const ODEPred<spk_ValueType>& )" << endl;
  oODEPred_h << "{" << endl;
  oODEPred_h << "}" << endl;

  oODEPred_h << "#endif" << endl;
  oODEPred_h.close();
}
*/
void NonmemTranslator::generateNonmemParsNamespace() const
{
  //---------------------------------------------------------------------------------------
  // Generate the NonmemPars namespace.
  //---------------------------------------------------------------------------------------
  const Symbol* pTheta = table->findi(KeyStr.THETA);
  const Symbol* pOmega = table->findi(KeyStr.OMEGA);
  const Symbol* pSigma = table->findi(KeyStr.SIGMA);
  const Symbol* pEta   = table->findi(KeyStr.ETA);
  ofstream oNonmemPars( fNonmemPars_h );
  if( !oNonmemPars.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Failed to create %s file.", fNonmemPars_h );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
 
  oNonmemPars << "//=============================================================" << endl;
  oNonmemPars << "// " << endl;
  oNonmemPars << "// " << myDescription << endl;
  oNonmemPars << "// " << endl;
  oNonmemPars << "// The namespace NonmemPars exports the values " << endl;
  oNonmemPars << "// given by the user or values drived from the user-given values." << endl;
  oNonmemPars << "// " << endl;
  oNonmemPars << "// The user requested the " << (ourTarget==POP? "population":"individual") << " analysis." << endl;
  oNonmemPars << "// This means that this namespace would contain materials related to " << endl;
  if( ourTarget==POP )
    {
      oNonmemPars << "// all of THETA, OMEGA, ETA, SIGMA and EPS." << endl;
    }
  else
    {
      oNonmemPars << "// only THETA, OMEGA and ETA." << endl;
    }
  oNonmemPars << "// It also contains the input value(s) necessary to simulate a data set " << endl;
  oNonmemPars << "// when requested." << endl;
  oNonmemPars << "// " << endl;
  oNonmemPars << "//=============================================================" << endl;

  oNonmemPars << "#ifndef NONMEMPARS_H" << endl;
  oNonmemPars << "#define NONMEMPARS_H" << endl;
  oNonmemPars << endl;

  oNonmemPars << "#include <valarray>" << endl;
  if( ourTarget == POP )
    oNonmemPars << "#include <spkpred/PopPredModel.h>" << endl;
  else
    oNonmemPars << "#include <spkpred/IndPredModel.h>" << endl;
  oNonmemPars << endl;
  

  oNonmemPars << "namespace NonmemPars{" << endl;

  oNonmemPars << "using namespace std;" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // THETA" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // The length of THETA vector." << endl;
  oNonmemPars << "   const int nTheta = " << myThetaLen << ";" << endl;
  oNonmemPars << endl;
  oNonmemPars << "   // A C-arrary containing the upper boundary values for THETA." << endl;
  oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
  oNonmemPars << "   double c_thetaUp[nTheta] = { ";
  for( int j=0; j<myThetaLen; j++ )
    {
      if( j>0 )
        oNonmemPars << ", ";
      oNonmemPars << pTheta->upper[0][j];
    }
  oNonmemPars << "   };" << endl;
  oNonmemPars << "   const valarray<double> thetaUp ( c_thetaUp,  " << myThetaLen << " );" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   // A C-arrary containing the lower boundary values for THETA." << endl;
  oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
  oNonmemPars << "   double c_thetaLow[nTheta] = { ";
  for( int j=0; j<myThetaLen; j++ )
    {
      if( j>0 )
	oNonmemPars << ", ";
      oNonmemPars << pTheta->lower[0][j];
    }
  oNonmemPars << "   };" << endl;
  oNonmemPars << "   const valarray<double> thetaLow( c_thetaLow, " << myThetaLen << " );" << endl;

  oNonmemPars << "   // A C-arrary containing the initial estimates for THETA." << endl;
  oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
  oNonmemPars << "   double c_thetaIn[nTheta] = { ";
  for( int j=0; j<myThetaLen; j++ )
    {
      if( j>0 )
	oNonmemPars << ", ";
      oNonmemPars << pTheta->initial[0][j];
    }
  oNonmemPars << " };" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   // A C-arrary containing the fixation flags for THETA." << endl;
  oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
  oNonmemPars << "   bool c_thetaFixed[nTheta] = { ";
  for( int j=0; j<myThetaLen; j++ )
    {
      if( j>0 )
	oNonmemPars << ", ";
      oNonmemPars << pTheta->fixed[0][j];
    }
  oNonmemPars << " };" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   const valarray<bool> thetaFixed( c_thetaFixed, " << myThetaLen << " );" << endl;
  oNonmemPars << "   // A valarray object that *will* contain the initial values for THETA." << endl;
  oNonmemPars << "   // The object value may be replaced if a new data set is simulated." << endl;
  oNonmemPars << "   valarray<double> thetaIn ( c_thetaIn, nTheta );" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // ETA" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // The length of ETA vector, which determines the dimension of OMEGA covariance." << endl;
  oNonmemPars << "   const int nEta = " << myEtaLen << ";" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   // A C-arrary containing the initial estimates for ETA." << endl;
  oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
  oNonmemPars << "   double c_etaIn[nEta] = { ";
  for( int i=0; i<myEtaLen; i++ )
    {
      if( i > 0 )
	oNonmemPars << ", ";
      oNonmemPars << pEta->initial[0][i];
    }
  oNonmemPars << " };" << endl;
  oNonmemPars << "   const valarray<double> etaIn( c_etaIn, nEta );" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // OMEGA" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;  
  oNonmemPars << "   // The structure of OMEGA matrix." << endl;
  oNonmemPars << "   // \"FULL\" indicates that possibly all elements of the symmetric matrix may be non-zero." << endl;
  oNonmemPars << "   // \"DIAGONAL\" indicates that only the diagonal elements are non-zero and the rest are all zero." << endl;
     
  oNonmemPars << "   const enum " << (ourTarget==POP? "Pop":"Ind") << "PredModel::covStruct omegaStruct = ";
  oNonmemPars << (ourTarget==POP? "Pop":"Ind") << "PredModel::";
  oNonmemPars << (myOmegaStruct == Symbol::TRIANGLE? "FULL" : "DIAGONAL" ) << ";" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   // The dimension of OMEGA matrix is detemined by the length of ETA vector." << endl;
  oNonmemPars << "   const int omegaDim = nEta;" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   // The order of OMEGA matrix." << endl;
  oNonmemPars << "   // If the matrix is full, the value is equal to the number of " << endl;
  oNonmemPars << "   // elements in a half triangle (diagonal elements included)." << endl;
  oNonmemPars << "   // If the matrix is diagonal, it is equal to the dimension of the symmetric matrix." << endl;
  oNonmemPars << "   const int omegaOrder = " << (myOmegaStruct==Symbol::DIAGONAL? "omegaDim" : "omegaDim * (omegaDim+1) / 2" ) << ";" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   // A C-arrary containing the initial estimates for OMEGA." << endl;
  oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
  oNonmemPars << "   double c_omegaIn[ omegaOrder ] = { "; 
  for( int j=0; j<myOmegaOrder; j++ )
    {
      if( j>0 )
	oNonmemPars << ", ";
      oNonmemPars << pOmega->initial[0][j];
    }
  oNonmemPars << " };" << endl;
  oNonmemPars << "   const valarray<double> omegaIn( c_omegaIn, omegaOrder );" << endl;
  oNonmemPars << endl;

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // EPS" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;  
  oNonmemPars << "   // The length of EPS vector, which determines the dimension of SIGMA." << endl;
  if( ourTarget == POP )
    {
      oNonmemPars << "   const int nEps = " << myEpsLen << ";" << endl;
    }
  else
    {
      oNonmemPars << "// NOTE:" << endl;
      oNonmemPars << "// EPS related variable(s) do not appear in this namespace" << endl;
      oNonmemPars << "// because you requested the single individual analysis." << endl;
      oNonmemPars << "// const int nEps;" << endl;
    }
  oNonmemPars << endl;

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // SIGMA" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;  
  if( ourTarget == POP )
    {
      oNonmemPars << "   // The structure of SIGMA matrix." << endl;
      oNonmemPars << "   // \"FULL\" indicates that possibly all elements of the symmetric matrix may be non-zero." << endl;
      oNonmemPars << "   // \"DIAGONAL\" indicates that only the diagonal elements are non-zero and the rest are all zero." << endl;
      oNonmemPars << "   const enum PopPredModel::covStruct sigmaStruct = ";
      oNonmemPars << "PopPredModel::" << (mySigmaStruct == Symbol::TRIANGLE? "FULL" : "DIAGONAL" ) << ";" << endl;
      oNonmemPars << endl;

      oNonmemPars << "   // The dimension of SIGMA matrix is detemined by the length of EPS vector." << endl;
      oNonmemPars << "   const int sigmaDim = nEps;" << endl;
      oNonmemPars << endl;

      oNonmemPars << "   // The order of SIGMA matrix." << endl;
      oNonmemPars << "   // If the matrix is full, the value is equal to the number of " << endl;
      oNonmemPars << "   // elements in a half triangle (diagonal elements included)." << endl;
      oNonmemPars << "   // If the matrix is diagonal, it is equal to the dimension of the symmetric matrix." << endl;
      oNonmemPars << "   const int sigmaOrder = " << (mySigmaStruct==Symbol::DIAGONAL? "sigmaDim;" : "sigmaDim * ( sigmaDim + 1 ) / 2;") << endl;

      oNonmemPars << "   // A C-arrary containing the initial estimates for SIGMA." << endl;
      oNonmemPars << "   // This array is used to initializes a valarray object that follows." << endl;
      oNonmemPars << "   double c_sigmaIn[ sigmaOrder ] = { ";
      for( int j=0; j<mySigmaOrder; j++ )
	{
	  if( j>0 )
	    oNonmemPars << ", ";
	  oNonmemPars << pSigma->initial[0][j];
	}
      oNonmemPars << " };" << endl;
      oNonmemPars << "   const valarray<double> sigmaIn( c_sigmaIn, sigmaOrder );" << endl;
    }
  else
    {
      oNonmemPars << "// NOTE:" << endl;
      oNonmemPars << "// SIGMA related variables do not appear in this namespace" << endl;
      oNonmemPars << "// because you requested the single individual analysis." << endl;
      oNonmemPars << "// const enum PopPredModel::covStruct sigmaStruct;" << endl;
      oNonmemPars << "// const valarray<double> sigmaIn;" << endl;
    }
  oNonmemPars << endl;

  oNonmemPars << "   //-------------------------------------------" << endl;
  oNonmemPars << "   // Data Simulation" << endl;
  oNonmemPars << "   //-------------------------------------------" << endl;  
  if( myIsSimulate )
    {
      oNonmemPars << "// The seed for data simulation." << endl;
      oNonmemPars << "const int seed = " << mySeed << ";" << endl;
    }
  else
    {
      oNonmemPars << "// No simulation is requested." << endl;
      oNonmemPars << "const int seed = -1;" << endl;      
    }
  oNonmemPars << endl;

  oNonmemPars << "};" << endl;
  oNonmemPars << "#endif" << endl;
  oNonmemPars.close();
}
void NonmemTranslator::generateIndDriver( ) const
{
  //---------------------------------------------------------------------------------------
  // Generate the SPK driver
  //---------------------------------------------------------------------------------------
  ofstream oIndDriver ( fFitDriver_cpp );
  if( !oIndDriver.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Failed to create %s file.", fFitDriver_cpp );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const Symbol* pTheta = table->findi(KeyStr.THETA);
  const Symbol* pEta   = table->findi(KeyStr.ETA);
  const Symbol* pOmega = table->findi(KeyStr.OMEGA);

  oIndDriver << "// " << myDescription              << endl;
  oIndDriver << "#include <iostream>"               << endl;
  oIndDriver << "#include <fstream>"                << endl;
  oIndDriver << "#include <sys/time.h>"             << endl;
  oIndDriver << "#include <vector>"                 << endl;
  oIndDriver << endl;

  oIndDriver << "#include <spk/SpkValarray.h>"      << endl;
  oIndDriver << "#include <spk/SpkException.h>"     << endl;
  oIndDriver << "#include <spk/WarningsManager.h>"  << endl;
  oIndDriver << "#include <CppAD/CppAD.h>"          << endl;
  oIndDriver << endl;

  oIndDriver << "// For parameter estimation"       << endl;
  oIndDriver << "#include <spk/fitIndividual.h>"    << endl;
  oIndDriver << "#include <spk/Optimizer.h>"        << endl;
  oIndDriver << endl;

  oIndDriver << "// For statistics"                 << endl;
  oIndDriver << "#include <spk/inverse.h>"          << endl;
  oIndDriver << "#include <spk/indStatistics.h>"    << endl;
  oIndDriver << "#include <spk/derParStatistics.h>" << endl;
  oIndDriver << "#include <spk/multiply.h>"         << endl;
  oIndDriver << "#include <spk/cholesky.h>"         << endl;
  oIndDriver << "#include <spk/indResiduals.h>"     << endl;
  oIndDriver << endl;

  oIndDriver << "// Helper" << endl;
  oIndDriver << "#include <spk/printInMatrix.h>"    << endl;
  oIndDriver << endl;

  oIndDriver << "// For data simulation"            << endl;
  oIndDriver << "#include <spk/simulate.h>"         << endl;
  oIndDriver << endl;

  oIndDriver << "// SPK Compiler generated headers/classes" << endl;
  oIndDriver << "#include \"IndData.h\""            << endl;
  oIndDriver << "#include \"DataSet.h\""            << endl;
  oIndDriver << "#include \"NonmemPars.h\""         << endl;
  oIndDriver << endl;

  oIndDriver << "//   NONMEM PRED SPECIFIC"         << endl;
  oIndDriver << "#include \"Pred.h\""               << endl;
  oIndDriver << "#include <spkpred/IndPredModel.h>" << endl;
  oIndDriver << endl;

  oIndDriver << "using SPK_VA::valarray;" << endl;
  oIndDriver << "using namespace std;"    <<endl;
  oIndDriver << endl;

  oIndDriver << "enum RETURN_CODE { SUCCESS=0,"             << endl;
  oIndDriver << "                   CONVERGENCE_FAILURE=1," << endl;
  oIndDriver << "                   FILE_ACCESS_FAILURE=2," << endl;
  oIndDriver << "                   MONTE_FAILURE=3,"       << endl;
  oIndDriver << "                   STAT_FAILURE=4,"        << endl;
  oIndDriver << "                   SIMULATION_FAILURE=5," << endl;
  oIndDriver << "                   OTHER_FAILURE };"       << endl;
  oIndDriver << endl;
  oIndDriver << "int main( int argc, const char argv[] )" << endl;
  oIndDriver << "{" << endl;

  oIndDriver << "/*******************************************************************/" << endl;
  oIndDriver << "/*                                                                 */" << endl;
  oIndDriver << "/*   Variable declarations and definitions                         */" << endl;
  oIndDriver << "/*                                                                 */" << endl;
  oIndDriver << "/*******************************************************************/" << endl;
  oIndDriver << "enum RETURN_CODE ret = SUCCESS;" << endl;
  oIndDriver << endl;

  oIndDriver << "SpkException errors;" << endl;
  oIndDriver << "ofstream oLongError;" << endl;
  oIndDriver << endl;

  oIndDriver << "const int nY = " << myRecordNums[0] << ";" << endl;
  oIndDriver << "DataSet< CppAD::AD<double> > set;" << endl;
  oIndDriver << "valarray<double> y( nY );" << endl;
  oIndDriver << endl;
  
  oIndDriver << "const bool isSimRequested     = " << ( myIsSimulate? "true":"false" ) << ";" << endl;
  oIndDriver << "bool haveCompleteData         = !isSimRequested;" << endl;
  oIndDriver << endl;

  oIndDriver << "const bool isOptRequested     = " << ( myIsEstimate? "true":"false" ) << ";" << endl;
  oIndDriver << "bool isOptSuccess             = !isOptRequested;" << endl;
  oIndDriver << endl;

  oIndDriver << "const bool isStatRequested    = " << ( myIsStat? "true":"false" ) << ";"     << endl;
  oIndDriver << "IndCovForm covForm            = " << myCovForm << ";" << endl;
  oIndDriver << "bool isStatSuccess            = !isStatRequested;" << endl;
  oIndDriver << endl;
  oIndDriver << endl;

  oIndDriver << "const bool isRestartRequested = " << ( myIsRestart? "true":"false" ) << ";"     << endl;
  oIndDriver << endl;

  oIndDriver << "const int nRepeats            = " << mySubproblemsN << ";" << endl;
  oIndDriver << endl;

  oIndDriver << "const bool withD              = false;" << endl;
  oIndDriver << endl;

  oIndDriver << "valarray<double> thetaStep( NonmemPars::nTheta );" << endl;
  oIndDriver << "valarray<double> thetaIn  ( NonmemPars::thetaIn );" << endl;
  oIndDriver << "valarray<double> omegaIn  ( NonmemPars::omegaIn );" << endl;
  oIndDriver << "valarray<double> thetaOut ( NonmemPars::nTheta );" << endl;
  oIndDriver << "valarray<double> omegaOut ( NonmemPars::omegaOrder );" << endl;
  oIndDriver << endl;

  oIndDriver << "//////////////////////////////////////////////////////////////////////" << endl;
  oIndDriver << "//   Model Initialization" << endl;
  oIndDriver << "Pred<CppAD::AD<double> > mPred(&set);"        << endl;
  oIndDriver << "IndPredModel model( mPred, "                  << endl;
  oIndDriver << "                    NonmemPars::nTheta, "     << endl;
  oIndDriver << "                    NonmemPars::thetaLow, "   << endl;
  oIndDriver << "                    NonmemPars::thetaUp, "    << endl;
  oIndDriver << "                    NonmemPars::thetaIn, "    << endl;
  oIndDriver << "                    NonmemPars::nEta, "       << endl;
  oIndDriver << "                    NonmemPars::omegaStruct," << endl;
  oIndDriver << "                    NonmemPars::omegaIn );"   << endl;
  oIndDriver << "//" << endl;
  oIndDriver << "//////////////////////////////////////////////////////////////////////" << endl;
  oIndDriver << endl;
  oIndDriver << "//////////////////////////////////////////////////////////////////////" << endl;
  oIndDriver << "//   DataSet and Model for disposal in order to save the last values"   << endl;
  oIndDriver << "//   from parameter estimation."                                        << endl;
  oIndDriver << "DataSet< CppAD::AD<double> > dataForDisposal;"                          << endl;
  oIndDriver << "Pred<CppAD::AD<double> > predForDisposal(&dataForDisposal);"            << endl;
  oIndDriver << "IndPredModel modelForDisposal( predForDisposal, "                       << endl;
  oIndDriver << "                    NonmemPars::nTheta, "     << endl;
  oIndDriver << "                    NonmemPars::thetaLow, "   << endl;
  oIndDriver << "                    NonmemPars::thetaUp, "    << endl;
  oIndDriver << "                    NonmemPars::thetaIn, "    << endl;
  oIndDriver << "                    NonmemPars::nEta, "       << endl;
  oIndDriver << "                    NonmemPars::omegaStruct," << endl;
  oIndDriver << "                    NonmemPars::omegaIn );"   << endl;
  oIndDriver << "//" << endl;
  oIndDriver << "//////////////////////////////////////////////////////////////////////" << endl;
  oIndDriver << endl;

  oIndDriver << "const int nB = model.getNIndPar();" << endl;
  oIndDriver << "valarray<double> bIn  ( nB );"      << endl;
  oIndDriver << "valarray<double> bStep( nB );"      << endl;
  oIndDriver << "valarray<double> bLow ( nB );"      << endl;
  oIndDriver << "valarray<double> bUp  ( nB );"      << endl;
  oIndDriver << "valarray<double> bOut ( nB );"      << endl;
  oIndDriver << "valarray<bool>   bMask( nB );"      << endl;
  oIndDriver << "double           bObjOut;"                       << endl;
  oIndDriver << "valarray<double> bObj_bOut( nB );"               << endl;
  oIndDriver << "valarray<double> bObj_b_bOut( nB * nB );"        << endl;
  oIndDriver << endl;
  
  oIndDriver << "timeval optBegin, optEnd;" << endl;
  oIndDriver << "double optTimeSec = 0.0;" << endl;
  oIndDriver << endl;

  oIndDriver << "const double indEps             = "   << myIndEpsilon    << ";" << endl;
  oIndDriver << "const int    indMitr            = "   << myIndMitr       << ";" << endl;
  oIndDriver << "const int    indTrace           = "   << myIndTraceLevel << ";" << endl;
  oIndDriver << "const string indCheckpointFile  = \"" << fCheckpoint_xml << "\";"  << endl;
  oIndDriver << "bool         indWriteCheckpoint = "   << (myIndWriteCheckpoint? "true" : "false") << ";" << endl;
  oIndDriver << "ifstream     iCheckpoint( indCheckpointFile.c_str() );"  << endl;
  oIndDriver << "// Error if the user asked to continue but no checkpoint.xml is found " << endl;
  oIndDriver << "// in the current directory." << endl;
  oIndDriver << "if( isRestartRequested && !iCheckpoint.good() )" << endl;
  oIndDriver << "{" << endl;
  oIndDriver << "   char m[ SpkError::maxMessageLen()];" << endl;
  oIndDriver << "   sprintf( m, \"Warm start is requested but no checkpoint file found.\" );" << endl;
  oIndDriver << "   SpkError e( SpkError::SPK_STD_ERR, m, __LINE__, __FILE__);" << endl;
  oIndDriver << "   errors.push( e );" << endl;
  oIndDriver << "   ret = FILE_ACCESS_FAILURE;" << endl;
  oIndDriver << "}" << endl;
  oIndDriver << "// Flag to read the checkpoint file if that exists even " << endl;
  oIndDriver << "// if the user didn't ask a continuation." << endl;
  oIndDriver << "bool         indReadCheckpoint  = iCheckpoint.good();"   << endl;
  oIndDriver << "iCheckpoint.close();"                                    << endl;
  oIndDriver << "Optimizer    indOpt( indEps, "                           << endl;
  oIndDriver << "                     indMitr, "                          << endl;
  oIndDriver << "                     indTrace, "                         << endl;
  oIndDriver << "                     indCheckpointFile, "                << endl;
  oIndDriver << "                     indReadCheckpoint,"                 << endl;
  oIndDriver << "                     indWriteCheckpoint );"              << endl;
  oIndDriver << endl;

  oIndDriver << "model.getIndPar       ( bIn );"       << endl;
  oIndDriver << "model.getIndParLimits ( bLow, bUp );" << endl;
  oIndDriver << "model.getIndParStep   ( bStep );"     << endl;
  oIndDriver << endl;

  oIndDriver << "timeval statBegin, statEnd;"                         << endl;
  oIndDriver << "double statTimeSec = 0.0;"                           << endl;
  oIndDriver << "const int nDegOfFreedom = nY - nB;"                  << endl;
  oIndDriver << "valarray<double> bCov( nB * nB );"                   << endl;
  oIndDriver << "valarray<double> stdPar( nB );"                      << endl;
  oIndDriver << "valarray<double> stdPar_b( nB * nB );"               << endl;
  oIndDriver << "bool isCovOut         = " << ( myIsCov?         "true" : "false" ) << ";" << endl;    
  oIndDriver << "bool isInvCovOut      = " << ( myIsInvCov?      "true" : "false" ) << ";" << endl;    
  oIndDriver << "bool isStdErrOut      = " << ( myIsStderr?      "true" : "false" ) << ";" << endl;    
  oIndDriver << "bool isCorrelationOut = " << ( myIsCorrelation? "true" : "false" ) << ";" << endl;    
  oIndDriver << "bool isConfidenceOut  = " << ( myIsConfidence?  "true" : "false" ) << ";" << endl;    
  oIndDriver << "bool isCoefficientOut = " << ( myIsCoefficient? "true" : "false" ) << ";" << endl;    
  oIndDriver << "valarray<double> stdParCovOut( nB * nB );"         << endl;
  oIndDriver << "valarray<double> stdParSEOut( nB );"               << endl;
  oIndDriver << "valarray<double> stdParCorrelationOut( nB * nB );" << endl;
  oIndDriver << "valarray<double> stdParCoefficientOut( nB );"      << endl;
  oIndDriver << "valarray<double> stdParConfidenceOut( 2 * nB );"   << endl;
  oIndDriver << "valarray<double> stdParInvCovOut( nB * nB );"      << endl;
	  
  oIndDriver << "valarray<double> f_bOut( nY * nB );"      << endl;
  oIndDriver << "valarray<double> R_bOut( nY * nY * nB );" << endl;
  oIndDriver << "valarray<double> RInvOut( nY * nY );"     << endl;
  oIndDriver << endl;

  oIndDriver << "valarray<double> iPredOut     ( nY );"      << endl;
  oIndDriver << "valarray<double> iResOut      ( nY );"      << endl;
  oIndDriver << "valarray<double> iResWtdOut   ( nY );"    << endl;
  oIndDriver << endl;

  oIndDriver << "ofstream oResults;" << endl;
  oIndDriver << "string warningsOut;" << endl;
  oIndDriver << "int seed = NonmemPars::seed;" << endl;
  oIndDriver << "int iSub = 0;" << endl;
  oIndDriver << endl;

  oIndDriver << "if( ret != SUCCESS )" << endl;
  oIndDriver << "  goto REPORT_GEN;" << endl;
  oIndDriver << endl;

  oIndDriver << "bOut     = bIn;" << endl;
  oIndDriver << "thetaOut = thetaIn;" << endl;
  oIndDriver << "omegaOut = omegaIn;" << endl;
  oIndDriver << "remove( \"result.xml\" );" << endl;
  oIndDriver << "for( iSub=0; iSub<nRepeats; iSub++, seed=rand() )" << endl;
  oIndDriver << "{" << endl;
  oIndDriver << "   bIn = bOut;" << endl;
  oIndDriver << "   thetaIn = thetaOut;" << endl;
  oIndDriver << "   omegaIn = omegaOut;" << endl;
  oIndDriver << "   /*******************************************************************/" << endl;
  oIndDriver << "   /*                                                                 */" << endl;
  oIndDriver << "   /*   Data Initialization                                           */" << endl;
  oIndDriver << "   /*                                                                 */" << endl;
  oIndDriver << "   /*******************************************************************/" << endl;
  oIndDriver << "   if( isSimRequested )" << endl;
  oIndDriver << "   {" << endl;
  oIndDriver << "      valarray<double> yOut( nY );" << endl;
  oIndDriver << "      try" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         simulate( model, nY, bIn, yOut, NonmemPars::seed );" << endl;
  oIndDriver << "         set.replaceAllMeasurements( yOut );" << endl;
  oIndDriver << "         y = yOut;" << endl;
  oIndDriver << "         haveCompleteData = true;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      catch( SpkException& e )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         char mess[ SpkError::maxMessageLen() ];" << endl;
  oIndDriver << "         sprintf( mess, \"Failed in data simulation.\\n\" );" << endl;
  oIndDriver << "         e.push( SpkError::SPK_SIMULATION_ERR, mess, __LINE__, __FILE__ );" << endl;
  oIndDriver << "         errors.cat( e );" << endl;
  oIndDriver << "         haveCompleteData = false;" << endl;
  oIndDriver << "         ret = SIMULATION_FAILURE;" << endl;
  oIndDriver << "         goto REPORT_GEN;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      catch( ... )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         char message[] =\"Unknown exception: failed in data simulation!!!\";" << endl;
  oIndDriver << "         SpkError e( SpkError::SPK_UNKNOWN_ERR, message, __LINE__, __FILE__ );" << endl;
  oIndDriver << "         errors.push( e );" << endl;
  oIndDriver << "         haveCompleteData = false;" << endl;
  oIndDriver << "         ret = SIMULATION_FAILURE;" << endl;
  oIndDriver << "         goto REPORT_GEN;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "   }" << endl;
  oIndDriver << "   else" << endl;
  oIndDriver << "   {" << endl;
  oIndDriver << "      y = set.getAllMeasurements();" << endl;
  oIndDriver << "      haveCompleteData = true;" << endl;
  oIndDriver << "   }" << endl;
  oIndDriver << endl;
  
  oIndDriver << "OPTIMIZATION:" << endl;
  oIndDriver << "   /*******************************************************************/" << endl;
  oIndDriver << "   /*                                                                 */" << endl;
  oIndDriver << "   /*   Parameter Estimation                                          */" << endl;
  oIndDriver << "   /*                                                                 */" << endl;
  oIndDriver << "   /*******************************************************************/" << endl;
  oIndDriver << "   if( isOptRequested && haveCompleteData )" << endl;
  oIndDriver << "   {" << endl;
  oIndDriver << "      gettimeofday( &optBegin, NULL );" << endl;
  oIndDriver << "      try" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         fitIndividual( model," << endl;
  oIndDriver << "                        y," << endl;
  oIndDriver << "                        indOpt," << endl;
  oIndDriver << "                        bLow," << endl;
  oIndDriver << "                        bUp," << endl;
  oIndDriver << "                        bIn," << endl;
  oIndDriver << "                        bStep," << endl;
  oIndDriver << "                       &bOut," << endl;
  oIndDriver << "                       &bObjOut," << endl;
  oIndDriver << "                       &bObj_bOut," << endl;
  oIndDriver << "                       &bObj_b_bOut," << endl;
  oIndDriver << "                        withD );" << endl;
  oIndDriver << "         isOptSuccess = true;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      catch( SpkException& e )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         char mess[ SpkError::maxMessageLen() ];" << endl;
  oIndDriver << "         sprintf( mess, \"Failed in population parameter estimation.\\n\" );" << endl;
  oIndDriver << "         e.push( SpkError::SPK_OPT_ERR, mess, __LINE__, __FILE__ );" << endl;
  oIndDriver << "         errors.cat( e );" << endl;
  oIndDriver << "         isOptSuccess = false;" << endl;
  oIndDriver << "         ret = CONVERGENCE_FAILURE;" << endl;
  oIndDriver << "         goto REPORT_GEN;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      catch( ... )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         char message[] = \"Unknown exception: failed in parameter estimation!!!\";" << endl;
  oIndDriver << "         SpkError e( SpkError::SPK_UNKNOWN_ERR, message, __LINE__, __FILE__ );" << endl;
  oIndDriver << "         errors.push( e );" << endl;
  oIndDriver << "         isOptSuccess = false;" << endl;
  oIndDriver << "         ret = CONVERGENCE_FAILURE;" << endl;
  oIndDriver << "         goto REPORT_GEN;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << endl;
  oIndDriver << "      // Get the latest value of theta and Omega." << endl;
  oIndDriver << "      // These values may be garbage if optimization had failed." << endl;
  oIndDriver << "      model.getTheta( thetaOut );" << endl;
  oIndDriver << "      model.getOmega( omegaOut );" << endl;
  oIndDriver << "      if( !isOptSuccess )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         string optErrHeader;" << endl;
  oIndDriver << "         string optErrMessage;" << endl;
  oIndDriver << "         // If individual level estimation failed, then get any details as to why." << endl;
  oIndDriver << "         if( indOpt.isThereErrorInfo() )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            oLongError.open( \"" << fSpkRuntimeLongError_tmp << "\" );" << endl;
  oIndDriver << "            if( !oLongError.good() )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               char m[ SpkError::maxMessageLen() ];" << endl;
  oIndDriver << "               sprintf( m, \"Failed to create a temporary file, %s, for writing.\", " << endl;
  oIndDriver << "                        \"" << fSpkRuntimeLongError_tmp << "\" );" << endl;
  oIndDriver << "               SpkError e( SpkError::SPK_STD_ERR, m, __LINE__, __FILE__ );" << endl;
  oIndDriver << "               errors.push( e );" << endl;
  oIndDriver << "               ret = FILE_ACCESS_FAILURE;" << endl;
  oIndDriver << "               goto REPORT_GEN;" << endl;
  oIndDriver << "            }" << endl;      
  oIndDriver << "            optErrHeader  = \"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \\n\";" << endl;
  oIndDriver << "            optErrHeader += \"Individual level optimization failure details. \\n\";" << endl;
  oIndDriver << "            optErrHeader += \"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \\n\";" << endl;
  oIndDriver << "            optErrHeader += \"\\n\";" << endl;
  oIndDriver << "            indOpt.getErrorInfo(" << endl;
  oIndDriver << "               optErrHeader," << endl;
  oIndDriver << "               optErrMessage," << endl;
  oIndDriver << "               __LINE__," << endl;
  oIndDriver << "               __FILE__ );" << endl;
  oIndDriver << "            oLongError << optErrMessage << endl;" << endl;
  oIndDriver << "            oLongError.close();" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << endl;
  oIndDriver << endl;
  oIndDriver << "      gettimeofday( &optEnd, NULL );" << endl;
  oIndDriver << "      optTimeSec = difftime( optEnd.tv_sec, optBegin.tv_sec );" << endl;
  oIndDriver << endl;
  oIndDriver << "      //////////////////////////////////////////////////////////////////////" << endl;
  oIndDriver << "      //   NONMEM Specific" << endl;
  oIndDriver << "      if( isOptRequested && isOptSuccess )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         indResiduals( modelForDisposal, y, bOut, "    << endl;
  oIndDriver << "                      &iPredOut, "        << endl;
  oIndDriver << "                      &iResOut, "         << endl;
  oIndDriver << "                      &iResWtdOut, "      << endl;
  oIndDriver << "                       NULL, "            << endl;
  oIndDriver << "                       NULL );"           << endl;
  oIndDriver << "         set.replaceIPred( iPredOut );"   << endl;
  oIndDriver << "         set.replaceIRes ( iResOut );"    << endl;
  oIndDriver << "         set.replaceIWRes( iResWtdOut );" << endl;
  oIndDriver << "         set.replacePred ( iPredOut );"   << endl;
  oIndDriver << "         set.replaceRes  ( iResOut );"    << endl;
  oIndDriver << "         set.replaceWRes ( iResWtdOut );" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      //" << endl;
  oIndDriver << "      //////////////////////////////////////////////////////////////////////" << endl;    
  oIndDriver << "   }" << endl;

  // Statistics can be only computed when the parameter estimation has been done.
  oIndDriver << "STATISTICS:" << endl;
  oIndDriver << "   /*******************************************************************/" << endl;
  oIndDriver << "   /*                                                                 */" << endl;
  oIndDriver << "   /*   Statistics                                                    */" << endl;
  oIndDriver << "   /*                                                                 */" << endl;
  oIndDriver << "   /*******************************************************************/" << endl;
  oIndDriver << "   if( isStatRequested && isOptRequested && haveCompleteData && isOptSuccess )" << endl;
  oIndDriver << "   {" << endl;
  // indStatistics
  oIndDriver << "      gettimeofday( &statBegin, NULL );"     << endl;
  oIndDriver << "      try" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         for( int i=0; i<nB; i++ )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            bMask[i] =!( bLow[i]==bUp[i] || bOut[i]==bLow[i] || bOut[i]==bUp[i] );" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "         model.getStandardPar( stdPar );"          << endl;
  oIndDriver << "         model.getStandardPar_indPar( stdPar_b );" << endl;
  oIndDriver << "         indStatistics(    modelForDisposal,"      << endl;
  oIndDriver << "                           y,"             << endl;
  oIndDriver << "                           bOut, "         << endl;
  oIndDriver << "                           bMask,"         << endl;
  oIndDriver << "                           bObj_b_bOut,"   << endl;
  oIndDriver << "                           covForm,"       << endl;
  oIndDriver << "                          &bCov,"          << endl;
  oIndDriver << "                           NULL,"          << endl;
  oIndDriver << "                           NULL,"          << endl;
  oIndDriver << "                           NULL,"          << endl;
  oIndDriver << "                           NULL,"          << endl;
  oIndDriver << "                           withD );"       << endl;
  oIndDriver << "         derParStatistics( bMask,"         << endl;
  oIndDriver << "                           bCov,"          << endl;
  oIndDriver << "                           stdPar,"        << endl;
  oIndDriver << "                           stdPar_b,"      << endl;
  oIndDriver << "                           nDegOfFreedom," << endl;
  oIndDriver << "                          (isCovOut || isInvCovOut? &stdParCovOut        : NULL)," << endl;
  oIndDriver << "                          (isCovOut || isInvCovOut? &stdParInvCovOut     : NULL)," << endl;
  oIndDriver << "                          (isStdErrOut?             &stdParSEOut         : NULL)," << endl;
  oIndDriver << "                          (isCorrelationOut?        &stdParCorrelationOut: NULL)," << endl;
  oIndDriver << "                          (isCoefficientOut?        &stdParCoefficientOut: NULL)," << endl;
  oIndDriver << "                          (isConfidenceOut?         &stdParConfidenceOut : NULL) " << endl;
  oIndDriver << "                         );" << endl;
  oIndDriver << "         isStatSuccess = true;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      catch( SpkException& e )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         char mess[ SpkError::maxMessageLen() ];" << endl;
  oIndDriver << "         sprintf( mess, \"Failed to compute statistics value(s).\\n\" );" << endl;
  oIndDriver << "         e.push( SpkError::SPK_STATISTICS_ERR, mess, __LINE__, __FILE__ );" << endl;
  oIndDriver << "         errors.cat( e );" << endl;
  oIndDriver << "         isStatSuccess = false;" << endl;
  oIndDriver << "         ret = STAT_FAILURE;" << endl;
  oIndDriver << "         goto REPORT_GEN;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      catch( ... )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         char message[] = \"Unknown exception: failed in statistics calculation!!!\";" << endl;
  oIndDriver << "         SpkError e( SpkError::SPK_UNKNOWN_ERR, message, __LINE__, __FILE__ );" << endl;
  oIndDriver << "         errors.push( e );" << endl;
  oIndDriver << "         isStatSuccess = false;" << endl;
  oIndDriver << "         ret = STAT_FAILURE;" << endl;
  oIndDriver << "         goto REPORT_GEN;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << endl;

  oIndDriver << "      gettimeofday( &statEnd, NULL );" << endl;
  oIndDriver << "      statTimeSec = difftime( statEnd.tv_sec, statBegin.tv_sec );" << endl;
  oIndDriver << "   }" << endl;
  oIndDriver << endl;

  oIndDriver << "REPORT_GEN:" << endl;
  oIndDriver << "   /*******************************************************************/" << endl;
  oIndDriver << "   /*                                                                 */" << endl;
  oIndDriver << "   /*   ReportML Document                                             */" << endl;
  oIndDriver << "   /*                                                                 */" << endl;
  oIndDriver << "   /*******************************************************************/" << endl;
  oIndDriver << "   oResults.open( \"" << fResult_xml << "\", ios_base::app );" << endl;
  oIndDriver << "   if( !oResults.good() )" << endl;
  oIndDriver << "   {" << endl;
  oIndDriver << "      fprintf( stderr, \"Failed to open a file, %s, for writing output!!!\"," << endl;
  oIndDriver << "\"" << fResult_xml << "\" );" << endl;
  oIndDriver << "      ret = FILE_ACCESS_FAILURE;" << endl;
  oIndDriver << "      oResults << \"</spkreport>\" << endl;" << endl;
  oIndDriver << "      oResults.close();" << endl;
  oIndDriver << "      goto END;" << endl;
  oIndDriver << "   }" << endl;

  //  oIndDriver << "   if( iSub == 0 )" << endl;
  oIndDriver << "   {" << endl;
  oIndDriver << "      oResults << \"<?xml version=\\\"1.0\\\"?>\" << endl;" << endl;
  oIndDriver << "      oResults << \"<spkreport>\" << endl;" << endl;
  oIndDriver << "   }" << endl;
  //  oIndDriver << endl;

  // Print out <error_list> even when it is empty.
  oIndDriver << "   oResults << \"<error_list length=\\\"\" << errors.size() << \"\\\">\" << endl;" << endl;
  oIndDriver << "   if( !(haveCompleteData && isOptSuccess && isStatSuccess) )" << endl;
  oIndDriver << "   {" << endl;
  oIndDriver << "      // Print out a long error message if exists." << endl;
  oIndDriver << "      char buf[ 128 ];" << endl;
  oIndDriver << "      ifstream iLongError( \"" << fSpkRuntimeLongError_tmp << "\" );" << endl;
  oIndDriver << "      while( iLongError.getline(buf, 128) )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         oResults << buf << endl;" << endl;   // Write a long error to the SpkReportML document.
  oIndDriver << "      }" << endl;
  oIndDriver << endl;
  oIndDriver << "      // Print out ordinary-length error messages" << endl;
  oIndDriver << "      oResults << errors << endl;" << endl;
  oIndDriver << "      iLongError.close();" << endl;
  oIndDriver << "   }" << endl;
  oIndDriver << "   oResults << \"</error_list>\" << endl;" << endl;
  oIndDriver << "   remove( \"" << fSpkRuntimeLongError_tmp << "\" );" << endl;
  oIndDriver << endl;

  // Print out <warning_list> even when it is empty.
  oIndDriver << "   WarningsManager::getAllWarnings( warningsOut );" << endl;
  oIndDriver << "   oResults << warningsOut;" << endl;
  oIndDriver << endl;

  oIndDriver << "   if( ret != SUCCESS )" << endl;
  oIndDriver << "   {" << endl;
  oIndDriver << "      oResults << \"</spkreport>\" << endl;" << endl;
  oIndDriver << "      oResults.close();" << endl;
  oIndDriver << "      goto END;" << endl;
  oIndDriver << "   }" << endl;
  oIndDriver << endl;

  oIndDriver << "   if( isSimRequested )" << endl;
  oIndDriver << "      oResults << \"<simulation seed=\\\"\" << seed << \"\\\" subproblem=\\\"\" << iSub+1 << \"\\\"/>\" << endl;" << endl;
  oIndDriver << endl;

  oIndDriver << "   if( isOptRequested )" << endl;
  oIndDriver << "   {" << endl;
  oIndDriver << "      oResults << \"<ind_analysis_result>\" << endl;" << endl;
  oIndDriver << endl;
  oIndDriver << "      oResults << \"<ind_opt_result elapsedtime=\\\"\" << optTimeSec << \"\\\">\" << endl;" << endl;
  oIndDriver << "      oResults << \"<ind_obj_out>\" << endl;" << endl;
  oIndDriver << "      oResults << \"<value>\" << bObjOut << \"</value>\" << endl;" << endl;
  oIndDriver << "      oResults << \"</ind_obj_out>\" << endl;" << endl;
  oIndDriver << endl;
  
  oIndDriver << "      //////////////////////////////////////////////////////////////////////" << endl;
  oIndDriver << "      //    NONMEM Specific" << endl;
  /*
  // theta (b)
  oIndDriver << "      oResults << \"<theta length=\\\"\" << NonmemPars::nTheta << \"\\\">\" << endl;" << endl;
  oIndDriver << "      oResults << \"<in>\" << endl;" << endl;
  oIndDriver << "      for( int i=0; i<NonmemPars::nTheta; i++ )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         oResults << \"<value>\" << thetaIn[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      oResults << \"</in>\" << endl;" << endl;
  oIndDriver << "      oResults << \"<out>\" << endl;" << endl;
  oIndDriver << "      for( int i=0; i<NonmemPars::nTheta; i++ )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         oResults << \"<value>\" << thetaOut[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      oResults << \"</out>\" << endl;" << endl;
  oIndDriver << "      oResults << \"</theta>\" << endl;" << endl;

  // omega 
  oIndDriver << "      oResults << \"<omega dimension=\" << \"\\\"\" << NonmemPars::omegaDim << \"\\\"\";" << endl;
  oIndDriver << "      oResults << \" struct=\" << \"\\\"\";" << endl;
  oIndDriver << "      if( NonmemPars::omegaStruct==IndPredModel::DIAGONAL )" << endl;
  oIndDriver << "         oResults << \"diagonal\";" << endl;
  oIndDriver << "      else" << endl;
  oIndDriver << "         oResults << \"block\";" << endl;
  oIndDriver << "      oResults << \"\\\"\" << \">\" << endl;" << endl;
  oIndDriver << "      oResults << \"<in>\" << endl;" << endl;
  oIndDriver << "      if( NonmemPars::omegaStruct==IndPredModel::DIAGONAL )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         for( int i=0; i<NonmemPars::omegaDim; i++ )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            oResults << \"<value>\" << NonmemPars::omegaIn[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      else // full" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         valarray<double> omegaFullTemp( (NonmemPars::omegaDim * (NonmemPars::omegaDim+1)) / 2 );" << endl;
  oIndDriver << "         for( int j=0, cnt=0; j<NonmemPars::omegaDim; j++ )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            for( int i=j; i<NonmemPars::omegaDim; i++, cnt++ ) // lower only" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               omegaFullTemp[ i + j * NonmemPars::omegaDim ] = NonmemPars::omegaIn[cnt];" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "         for( int j=0, cnt=0; j<NonmemPars::omegaDim; j++ )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            for( int i=0; i<=j; i++, cnt++ )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               oResults << \"<value>\" << omegaFullTemp[ j+i*NonmemPars::omegaDim ];" << endl;
  oIndDriver << "               oResults << \"</value>\" << endl;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      oResults << \"</in>\" << endl;" << endl;
  oIndDriver << "      oResults << \"<out>\" << endl;" << endl;
  oIndDriver << "      if( NonmemPars::omegaStruct==IndPredModel::DIAGONAL )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         for( int i=0; i<NonmemPars::omegaDim; i++ )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            oResults << \"<value>\" << omegaOut[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      else // full" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         valarray<double> omegaFullTemp( (NonmemPars::omegaDim * (NonmemPars::omegaDim+1)) / 2 );" << endl;
  oIndDriver << "         for( int j=0, cnt=0; j<NonmemPars::omegaDim; j++ )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            for( int i=j; i<NonmemPars::omegaDim; i++, cnt++ ) // lower only" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               omegaFullTemp[ i + j * NonmemPars::omegaDim ] = omegaOut[cnt];" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "         for( int j=0, cnt=0; j<NonmemPars::omegaDim; j++ )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            for( int i=0; i<=j; i++, cnt++ )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               oResults << \"<value>\" << omegaFullTemp[ j+i*NonmemPars::omegaDim ];" << endl;
  oIndDriver << "               oResults << \"</value>\" << endl;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      oResults << \"</out>\" << endl;" << endl;
  oIndDriver << "      oResults << \"</omega>\" << endl;" << endl;
  */
  // theta in
  oIndDriver << "      oResults << \"<theta_in length=\\\"\" << NonmemPars::nTheta << \"\\\">\" << endl;" << endl;
  oIndDriver << "      for( int i=0; i<NonmemPars::nTheta; i++ )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         oResults << \"<value>\" << thetaIn[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      oResults << \"</theta_in>\" << endl;" << endl;
  // theta out
  oIndDriver << "      oResults << \"<theta_out length=\\\"\" << NonmemPars::nTheta << \"\\\">\" << endl;" << endl;
  oIndDriver << "      for( int i=0; i<NonmemPars::nTheta; i++ )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         oResults << \"<value>\" << thetaOut[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      oResults << \"</theta_out>\" << endl;" << endl;
  // omega in
  oIndDriver << "      oResults << \"<omega_in dimension=\" << \"\\\"\" << NonmemPars::omegaDim << \"\\\"\";" << endl;
  oIndDriver << "      oResults << \" struct=\" << \"\\\"\";" << endl;
  oIndDriver << "      if( NonmemPars::omegaStruct==IndPredModel::DIAGONAL )" << endl;
  oIndDriver << "         oResults << \"diagonal\";" << endl;
  oIndDriver << "      else" << endl;
  oIndDriver << "         oResults << \"block\";" << endl;
  oIndDriver << "      oResults << \"\\\"\" << \">\" << endl;" << endl;
  oIndDriver << "      if( NonmemPars::omegaStruct==IndPredModel::DIAGONAL )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         for( int i=0; i<NonmemPars::omegaDim; i++ )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            oResults << \"<value>\" << omegaIn[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      else // full" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         valarray<double> omegaFullTemp( (NonmemPars::omegaDim * (NonmemPars::omegaDim+1)) / 2 );" << endl;
  oIndDriver << "         for( int j=0, cnt=0; j<NonmemPars::omegaDim; j++ )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            for( int i=j; i<NonmemPars::omegaDim; i++, cnt++ ) // lower only" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               omegaFullTemp[ i + j * NonmemPars::omegaDim ] = omegaIn[cnt];" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "         for( int j=0, cnt=0; j<NonmemPars::omegaDim; j++ )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            for( int i=0; i<=j; i++, cnt++ )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               oResults << \"<value>\" << omegaFullTemp[ j+i*NonmemPars::omegaDim ];" << endl;
  oIndDriver << "               oResults << \"</value>\" << endl;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      oResults << \"</omega_in>\" << endl;" << endl;
  // omega out
  oIndDriver << "      oResults << \"<omega_out dimension=\" << \"\\\"\" << NonmemPars::omegaDim << \"\\\"\";" << endl;
  oIndDriver << "      oResults << \" struct=\" << \"\\\"\";" << endl;
  oIndDriver << "      if( NonmemPars::omegaStruct==IndPredModel::DIAGONAL )" << endl;
  oIndDriver << "         oResults << \"diagonal\";" << endl;
  oIndDriver << "      else" << endl;
  oIndDriver << "         oResults << \"block\";" << endl;
  oIndDriver << "      oResults << \"\\\"\" << \">\" << endl;" << endl;
  oIndDriver << "      if( NonmemPars::omegaStruct==IndPredModel::DIAGONAL )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         for( int i=0; i<NonmemPars::omegaDim; i++ )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            oResults << \"<value>\" << omegaOut[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      else // full" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         valarray<double> omegaFullTemp( (NonmemPars::omegaDim * (NonmemPars::omegaDim+1)) / 2 );" << endl;
  oIndDriver << "         for( int j=0, cnt=0; j<NonmemPars::omegaDim; j++ )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            for( int i=j; i<NonmemPars::omegaDim; i++, cnt++ ) // lower only" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               omegaFullTemp[ i + j * NonmemPars::omegaDim ] = omegaOut[cnt];" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "         for( int j=0, cnt=0; j<NonmemPars::omegaDim; j++ )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            for( int i=0; i<=j; i++, cnt++ )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               oResults << \"<value>\" << omegaFullTemp[ j+i*NonmemPars::omegaDim ];" << endl;
  oIndDriver << "               oResults << \"</value>\" << endl;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "         }" << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      oResults << \"</omega_out>\" << endl;" << endl;

  oIndDriver << "      //" << endl;
  oIndDriver << "      //////////////////////////////////////////////////////////////////////" << endl;
  oIndDriver << endl;
  oIndDriver << "      oResults << \"</ind_opt_result>\" << endl;" << endl;
  oIndDriver << endl;

  oIndDriver << "      if( isCovOut " << endl;
  oIndDriver << "          || isInvCovOut " << endl;
  oIndDriver << "          || isStdErrOut " << endl;
  oIndDriver << "          || isCorrelationOut " << endl;
  oIndDriver << "          || isConfidenceOut " << endl;
  oIndDriver << "          || isCoefficientOut )" << endl;
  oIndDriver << "      {" << endl;
  oIndDriver << "         oResults << \"<ind_stat_result elapsedtime=\\\"\" << statTimeSec << \"\\\">\" << endl;" << endl;
  oIndDriver << "         if( isCovOut )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            oResults << \"<ind_covariance_out struct=\\\"block\\\" dimension=\\\"\" << nB << \"\\\">\" << endl;" << endl;
  oIndDriver << "            for( int i=0; i<nB; i++ )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               for( int j=0; j<=i; j++ )" << endl;
  oIndDriver << "               {" << endl;
  oIndDriver << "                  oResults << \"   <value>\" << stdParCovOut[i+j*nB] << \"</value>\" << endl;" << endl;
  oIndDriver << "               }" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            oResults << \"</ind_covariance_out>\" << endl;" << endl;
  oIndDriver << "         }" << endl;

  oIndDriver << "         if( isInvCovOut )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            oResults << \"<ind_inverse_covariance_out struct=\\\"block\\\" dimension=\\\"\" << nB << \"\\\">\" << endl;" << endl;
  oIndDriver << "            for( int i=0; i<nB; i++ )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               for( int j=0; j<=i; j++ )" << endl;
  oIndDriver << "               {" << endl;
  oIndDriver << "                  oResults << \"   <value>\" << stdParInvCovOut[i+j*nB] << \"</value>\" << endl;" << endl;
  oIndDriver << "               }" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            oResults << \"</ind_inverse_covariance_out>\" << endl;" << endl;
  oIndDriver << "         }" << endl;

  oIndDriver << "         if( isStdErrOut )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            oResults << \"<ind_stderror_out length=\\\"\" << nB << \"\\\">\" << endl;" << endl;
  oIndDriver << "            for( int i=0; i<nB; i++ )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               oResults << \"   <value>\" << stdParSEOut[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            oResults << \"</ind_stderror_out>\" << endl;" << endl;
  oIndDriver << "         }" << endl;

  oIndDriver << "         if( isCorrelationOut )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            oResults << \"<ind_correlation_out struct=\\\"block\\\" dimension=\\\"\" << nB << \"\\\">\" << endl;" << endl;
  oIndDriver << "            for( int i=0; i<nB; i++ )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               for( int j=0; j<=i; j++ )" << endl;
  oIndDriver << "               {" << endl;
  oIndDriver << "                  oResults << \"   <value>\" << stdParCorrelationOut[i+j*nB] << \"</value>\" << endl;" << endl;
  oIndDriver << "               }" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            oResults << \"</ind_correlation_out>\" << endl;" << endl;
  oIndDriver << "         }" << endl;

  oIndDriver << "         if( isCoefficientOut )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            oResults << \"<ind_coefficient_out length=\\\"\" << nB << \"\\\">\" << endl;" << endl;
  oIndDriver << "            for( int i=0; i<nB; i++ )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               oResults << \"   <value>\" << stdParCoefficientOut[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            oResults << \"</ind_coefficient_out>\" << endl;" << endl;
  oIndDriver << "         }" << endl;

  oIndDriver << "         if( isConfidenceOut )" << endl;
  oIndDriver << "         {" << endl;
  oIndDriver << "            oResults << \"<ind_confidence_out length=\\\"\" << nB*2 << \"\\\">\" << endl;" << endl;
  oIndDriver << "            for( int i=0; i<nB*2; i++ )" << endl;
  oIndDriver << "            {" << endl;
  oIndDriver << "               oResults << \"   <value>\" << stdParConfidenceOut[i] << \"</value>\" << endl;" << endl;
  oIndDriver << "            }" << endl;
  oIndDriver << "            oResults << \"</ind_confidence_out>\" << endl;" << endl;
  oIndDriver << "         }" << endl;

  oIndDriver << "         oResults << \"</ind_stat_result>\" << endl;" << endl;
  oIndDriver << endl;
  oIndDriver << "      }" << endl;
  oIndDriver << "      oResults << \"</ind_analysis_result>\" << endl;" << endl;
  oIndDriver << "      if( iSub == nRepeats-1 )" << endl;
  oIndDriver << "         oResults << set << endl;" << endl;
  oIndDriver << "      oResults << \"</spkreport>\" << endl;" << endl;
  oIndDriver << "      oResults.close();" << endl;
  oIndDriver << "   }" << endl;
  oIndDriver << "}" << endl;
  oIndDriver << "END:" << endl;
  //  oIndDriver << "   oResults << \"</spkreport>\" << endl;" << endl;
  //  oIndDriver << "   oResults.close();" << endl;
  oIndDriver << endl;
  oIndDriver << "   cout << \"exit code = \" << ret << endl;" << endl;
  oIndDriver << "   return ret;" << endl;
  oIndDriver << "}" << endl;
  oIndDriver.close();
}

void NonmemTranslator::generatePopDriver() const
{
  //==================================================================
  // Generate the driver
  //==================================================================
  ofstream oPopDriver ( fFitDriver_cpp );
  if( !oPopDriver.good() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Failed to create %s file.",
	       fFitDriver_cpp );
      SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  const Symbol* pTheta = table->findi(KeyStr.THETA);
  const Symbol* pOmega = table->findi(KeyStr.OMEGA);
  const Symbol* pSigma = table->findi(KeyStr.SIGMA);
  const Symbol* pEta   = table->findi(KeyStr.ETA);
  
  oPopDriver << "// " << myDescription << endl;

  oPopDriver << "#include <iostream>"                   << endl;
  oPopDriver << "#include <fstream>"                    << endl;
  oPopDriver << "#include <sys/time.h>"                 << endl;
  oPopDriver << endl;

  oPopDriver << "#include <spk/SpkValarray.h>"          << endl;
  oPopDriver << "#include <spk/SpkException.h>"         << endl;
  oPopDriver << "#include <spk/WarningsManager.h>"      << endl;
  oPopDriver << "#include <CppAD/CppAD.h>"              << endl;
  oPopDriver << endl;
 
  oPopDriver << "// For parameter esitimate " << endl;
  oPopDriver << "#include <spk/fitPopulation.h>"    << endl;
  oPopDriver << "#include <spk/Optimizer.h>"        << endl;
  oPopDriver << endl;

  oPopDriver << "// For statistics" << endl;
  oPopDriver << "#include <spk/derParStatistics.h>" << endl;
  oPopDriver << "#include <spk/popStatistics.h>"    << endl;
  oPopDriver << "#include <spk/inverse.h>"          << endl;
  oPopDriver << "#include <spk/lTilde.h>"           << endl;
  oPopDriver << "#include <spk/NaiveFoModel.h>"     << endl;
  oPopDriver << "#include <spk/multiply.h>"   << endl;
  oPopDriver << "#include <spk/cholesky.h>"   << endl;
  oPopDriver << "#include <spk/indResiduals.h>" << endl;
  oPopDriver << "#include <spk/popResiduals.h>" << endl;
  oPopDriver << endl;

  oPopDriver << "// For data simulation" << endl;
  oPopDriver << "#include <spk/simulate.h>" << endl;
  oPopDriver << endl;

  oPopDriver << "// SPK Compiler generated headers/classes" << endl;
  oPopDriver << "#include \"IndData.h\""      << endl;
  oPopDriver << "#include \"DataSet.h\""      << endl;
  oPopDriver << endl;

  oPopDriver << "//   NONMEM specific"   << endl;
  oPopDriver << "#include \"Pred.h\"" << endl;
  oPopDriver << "#include <spkpred/PopPredModel.h>" << endl;
  oPopDriver << "#include \"NonmemPars.h\""   << endl;
  oPopDriver << endl;

  oPopDriver << "using SPK_VA::valarray;" << endl;
  oPopDriver << "using namespace std;" << endl;
  oPopDriver << endl;
  oPopDriver << "enum RETURN_CODE { SUCCESS=0,"             << endl;
  oPopDriver << "                   CONVERGENCE_FAILURE=1," << endl;
  oPopDriver << "                   FILE_ACCESS_FAILURE=2," << endl;
  oPopDriver << "                   MONTE_FAILURE=3,"       << endl;
  oPopDriver << "                   STAT_FAILURE=4,"        << endl;
  oPopDriver << "                   SIMULATION_FAILURE=5,"  << endl;
  oPopDriver << "                   OTHER_FAILURE };"       << endl;
  oPopDriver << endl;

  oPopDriver << "int main( int argc, const char argv[] )" << endl;
  oPopDriver << "{" << endl;
  oPopDriver << "/*******************************************************************/" << endl;
  oPopDriver << "/*                                                                 */" << endl;
  oPopDriver << "/*   Variable declarations and definitions                         */" << endl;
  oPopDriver << "/*                                                                 */" << endl;
  oPopDriver << "/*******************************************************************/" << endl;
  oPopDriver << "enum RETURN_CODE ret = SUCCESS;" << endl;
  oPopDriver << endl;

  oPopDriver << "SpkException errors;" << endl;
  oPopDriver << "ofstream oLongError;" << endl;
  oPopDriver << endl;

  oPopDriver << "const bool isSimRequested     = " << (myIsSimulate? "true":"false") << ";" << endl;
  oPopDriver << "bool haveCompleteData         = !isSimRequested;" << endl;
  oPopDriver << endl;

  oPopDriver << "const bool isOptRequested     = " << (myIsEstimate? "true":"false") << ";" << endl;
  oPopDriver << "bool isOptSuccess             = !isOptRequested;" << endl;
  oPopDriver << "Objective objective           = ";
  if( ourApproximation == FO )
    oPopDriver << "FIRST_ORDER;" << endl;
  else if( ourApproximation == FOCE )
    oPopDriver << "EXPECTED_HESSIAN;" << endl;
  else
    oPopDriver << "MODIFIED_LAPLACE;" << endl;
  oPopDriver << endl;

  oPopDriver << "const bool isStatRequested    = " << (myIsStat? "true":"false") << ";" << endl;
  oPopDriver << "enum PopCovForm covForm       = " << myCovForm << ";" << endl;
  oPopDriver << "bool isStatSuccess            = !isStatRequested;" << endl;
  oPopDriver << endl;

  oPopDriver << "const bool isRestartRequested = " << (myIsRestart? "true":"false") << ";" << endl;
  oPopDriver << endl;

  oPopDriver << "const int nRepeats            = " << mySubproblemsN << ";" << endl;
  oPopDriver << endl;

  oPopDriver << "const bool isPostHoc          = " << (myIsPosthoc? "true" : "false") << ";" << endl;
  oPopDriver << endl;

  oPopDriver << "DataSet< CppAD::AD<double> > set;" << endl;
  oPopDriver << "const int           nPop      = set.getPopSize();" << endl;
  oPopDriver << "const valarray<int> N         = set.getN();" << endl;
  oPopDriver << "const int           nY        = N.sum();" << endl;
  oPopDriver << "valarray<double>    y( nY );" << endl;
  oPopDriver << endl;

  oPopDriver << "valarray<double> thetaIn  ( NonmemPars::thetaIn );" << endl;
  oPopDriver << "valarray<double> omegaIn  ( NonmemPars::omegaIn );" << endl;
  oPopDriver << "valarray<double> sigmaIn  ( NonmemPars::sigmaIn );" << endl;
  oPopDriver << "valarray<double> thetaOut ( NonmemPars::nTheta );" << endl;
  oPopDriver << "valarray<double> omegaOut ( NonmemPars::omegaOrder );" << endl;
  oPopDriver << "valarray<double> sigmaOut ( NonmemPars::sigmaOrder );" << endl;
  oPopDriver << endl;

  oPopDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oPopDriver << "//   Model initialization" << endl;
  oPopDriver << "Pred< CppAD::AD<double> > mPred(&set);" << endl;

  oPopDriver << "PopPredModel model( mPred,"                   << endl;
  oPopDriver << "                    NonmemPars::nTheta,"      << endl;
  oPopDriver << "                    NonmemPars::thetaLow,"    << endl;
  oPopDriver << "                    NonmemPars::thetaUp,"     << endl;
  oPopDriver << "                    NonmemPars::thetaIn,"     << endl;
  oPopDriver << "                    NonmemPars::nEta,"        << endl;
  oPopDriver << "                    NonmemPars::etaIn,"       << endl;
  oPopDriver << "                    NonmemPars::nEps,"        << endl;
  oPopDriver << "                    NonmemPars::omegaStruct," << endl;
  oPopDriver << "                    NonmemPars::omegaIn,"     << endl;
  oPopDriver << "                    NonmemPars::sigmaStruct," << endl;
  oPopDriver << "                    NonmemPars::sigmaIn );"   << endl;
  oPopDriver << "//" << endl;
  oPopDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oPopDriver << endl;
  oPopDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oPopDriver << "//   DataSet and Model for disposal; this DataSet instance is used " << endl;
  oPopDriver << "//   for computations other than parameter estimation, in order to" << endl;
  oPopDriver << "//   save the values at the end of estimation." << endl;
  oPopDriver << "DataSet< CppAD::AD<double> > dataForDisposal;" << endl;
  oPopDriver << "Pred< CppAD::AD<double> > predForDisposal(&dataForDisposal);" << endl;

  oPopDriver << "PopPredModel modelForDisposal( predForDisposal,"                   << endl;
  oPopDriver << "                    NonmemPars::nTheta,"      << endl;
  oPopDriver << "                    NonmemPars::thetaLow,"    << endl;
  oPopDriver << "                    NonmemPars::thetaUp,"     << endl;
  oPopDriver << "                    NonmemPars::thetaIn,"     << endl;
  oPopDriver << "                    NonmemPars::nEta,"        << endl;
  oPopDriver << "                    NonmemPars::etaIn,"       << endl;
  oPopDriver << "                    NonmemPars::nEps,"        << endl;
  oPopDriver << "                    NonmemPars::omegaStruct," << endl;
  oPopDriver << "                    NonmemPars::omegaIn,"     << endl;
  oPopDriver << "                    NonmemPars::sigmaStruct," << endl;
  oPopDriver << "                    NonmemPars::sigmaIn );"   << endl;
  oPopDriver << "//" << endl;
  oPopDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oPopDriver << endl;

  oPopDriver << "const int nAlp = model.getNPopPar();" << endl;
  oPopDriver << "const int nB   = model.getNIndPar();" << endl;
  oPopDriver << endl;
   
  oPopDriver << "valarray<double> alpIn  ( nAlp );" << endl;
  oPopDriver << "valarray<double> alpUp  ( nAlp );" << endl;
  oPopDriver << "valarray<double> alpLow ( nAlp );" << endl;
  oPopDriver << "valarray<double> alpStep( nAlp );" << endl;
  oPopDriver << "valarray<double> alpOut ( nAlp );" << endl;
  oPopDriver << "valarray<bool>   alpMask( nAlp );" << endl;
  oPopDriver << endl;
  
  oPopDriver << "double           alpObjOut;" << endl;
  oPopDriver << "valarray<double> alpObj_alpOut    ( nAlp );" << endl;
  oPopDriver << "valarray<double> alpObj_alp_alpOut( nAlp * nAlp );" << endl;
  oPopDriver << endl;

  oPopDriver << "model.getPopPar         ( alpIn );" << endl;
  oPopDriver << "model.getPopParLimits   ( alpLow, alpUp );" << endl;
  oPopDriver << "model.getPopParStep     ( alpStep );" << endl;

  oPopDriver << endl;

  oPopDriver << "valarray<double> bIn  ( nB * nPop );" << endl;
  oPopDriver << "valarray<double> biIn ( nB );" << endl;
  oPopDriver << "valarray<double> bUp  ( nB );" << endl;
  oPopDriver << "valarray<double> bLow ( nB );" << endl;
  oPopDriver << "valarray<double> bStep( nB );" << endl;
  oPopDriver << "valarray<double> bOut ( nB * nPop );" << endl;
  oPopDriver << "for( int i=0; i<nPop; i++ )" << endl;
  oPopDriver << "{" << endl;
  oPopDriver << "   model.selectIndividual( i ); " << endl;
  oPopDriver << "   model.getIndPar( biIn );" << endl;
  oPopDriver << "   bIn[ slice(i*nB, nB, 1) ] = biIn;" << endl;
  oPopDriver << "}" << endl;
  oPopDriver << "model.getIndParLimits ( bLow, bUp );" << endl;
  oPopDriver << "model.getIndParStep   ( bStep );" << endl;
  oPopDriver << endl;

  oPopDriver << "timeval optBegin, optEnd;" << endl;
  oPopDriver << "double  optTimeSec = 0.0;" << endl;
  oPopDriver << "const   double popEps             = "   << myPopEpsilon    << ";" << endl;
  oPopDriver << "const   int    popMitr            = "   << myPopMitr       << ";" << endl;
  oPopDriver << "const   int    popTrace           = "   << myPopTraceLevel << ";" << endl;
  oPopDriver << "const   string popCheckpointFile  = \"" << fCheckpoint_xml << "\";" << endl;
  oPopDriver << "bool           popWriteCheckpoint = "   << (myPopWriteCheckpoint? "true":"false") << ";" << endl;
  oPopDriver << "ifstream       iCheckpoint( popCheckpointFile.c_str() );"  << endl;
  oPopDriver << "// Error if the user asked to continue but no checkpoint.xml is found " << endl;
  oPopDriver << "// in the current directory." << endl;
  oPopDriver << "if( isRestartRequested && !iCheckpoint.good() )" << endl;
  oPopDriver << "{" << endl;
  oPopDriver << "   char m[ SpkError::maxMessageLen()];" << endl;
  oPopDriver << "   sprintf( m, \"Warm start is request but no checkpoint file found.\" );" << endl;
  oPopDriver << "   SpkError e( SpkError::SPK_STD_ERR, m, __LINE__, __FILE__);" << endl;
  oPopDriver << "   errors.push( e );" << endl;
  oPopDriver << "   ret = FILE_ACCESS_FAILURE;" << endl;
  oPopDriver << "}" << endl;
  oPopDriver << "// Flag to read the checkpoint file if that exists even " << endl;
  oPopDriver << "// if the user didn't ask a continuation." << endl;
  oPopDriver << "bool           popReadCheckpoint  = iCheckpoint.good();"   << endl;
  oPopDriver << "iCheckpoint.close();"                                    << endl;
  oPopDriver << "Optimizer      popOpt( popEps, "                           << endl;
  oPopDriver << "                       popMitr, "                          << endl;
  oPopDriver << "                       popTrace, "                         << endl;
  oPopDriver << "                       popCheckpointFile, "                << endl;
  oPopDriver << "                       popReadCheckpoint,"                 << endl;
  oPopDriver << "                       popWriteCheckpoint );"              << endl;
  oPopDriver << endl;
  oPopDriver << "const double   indEps   = " << myIndEpsilon    << ";" << endl;
  oPopDriver << "const int      indMitr  = " << myIndMitr       << ";" << endl;
  oPopDriver << "const int      indTrace = " << myIndTraceLevel << ";" << endl;
  oPopDriver << "Optimizer      indOpt( indEps, indMitr, indTrace );"  << endl;
  oPopDriver << endl;
 
  oPopDriver << "timeval statBegin, statEnd;"                               << endl;
  oPopDriver << "double statTimeSec = 0.0;"                                 << endl;
  oPopDriver << "valarray<double> alpCov( nAlp * nAlp );"                   << endl;
  oPopDriver << "valarray<double> stdPar( nAlp );"                          << endl;
  oPopDriver << "valarray<double> stdPar_alp( nAlp * nAlp );"               << endl;
  oPopDriver << "const int nDegOfFreedom = nY - nAlp;"                      << endl;
  oPopDriver << "bool isCovOut         = " << ( myIsCov?    "true" : "false" ) << ";"      << endl;
  oPopDriver << "bool isInvCovOut      = " << ( myIsInvCov? "true" : "false" ) << ";"      << endl;
  oPopDriver << "bool isStdErrOut      = " << ( myIsStderr? "true" : "false" ) << ";"      << endl;
  oPopDriver << "bool isCorrelationOut = " << ( myIsCorrelation? "true" : "false" ) << ";" << endl;
  oPopDriver << "bool isCoefficientOut = " << ( myIsCoefficient? "true" : "false" ) << ";" << endl;
  oPopDriver << "bool isConfidenceOut  = " << ( myIsConfidence?  "true" : "false" ) << ";" << endl;
  oPopDriver << "valarray<double> stdParCovOut        ( nAlp * nAlp );"     << endl;
  oPopDriver << "valarray<double> stdParSEOut         ( nAlp );"            << endl;
  oPopDriver << "valarray<double> stdParCorrelationOut( nAlp * nAlp );"     << endl;
  oPopDriver << "valarray<double> stdParCoefficientOut( nAlp );"            << endl;
  oPopDriver << "valarray<double> stdParConfidenceOut ( 2 * nAlp );"        << endl;
  oPopDriver << "valarray<double> stdParInvCovOut     ( nAlp * nAlp );"     << endl;
  oPopDriver << endl;

  oPopDriver << "valarray<double> iPredOut     ( nY );"      << endl;
  oPopDriver << "valarray<double> iResOut      ( nY );"      << endl;
  oPopDriver << "valarray<double> iResWtdOut   ( nY );"      << endl;
  oPopDriver << "valarray<double> iParResOut   ( nB*nPop );" << endl;
  oPopDriver << "valarray<double> iParResWtdOut( nB*nPop );" << endl;
  oPopDriver << endl;
  oPopDriver << "valarray<double> pPredOut     ( nY );"      << endl;
  oPopDriver << "valarray<double> pResOut      ( nY );"      << endl;
  oPopDriver << "valarray<double> pResWtdOut   ( nY );"      << endl;
  oPopDriver << "valarray<double> pParResOut   ( nB*nPop );" << endl;
  oPopDriver << "valarray<double> pParResWtdOut( nB*nPop );" << endl;
  oPopDriver << endl;
  oPopDriver << "valarray<double> cPredOut     ( nY );"      << endl;
  oPopDriver << "valarray<double> cResOut      ( nY );"      << endl;
  oPopDriver << "valarray<double> cResWtdOut   ( nY );"      << endl;
  oPopDriver << "valarray<double> cParResOut   ( nB*nPop );" << endl;
  oPopDriver << "valarray<double> cParResWtdOut( nB*nPop );" << endl;
  oPopDriver << endl;

  oPopDriver << "ofstream oResults;" << endl;
  oPopDriver << "string warningsOut;" << endl;
  oPopDriver << "int seed = NonmemPars::seed; " << endl;
  oPopDriver << "int iSub = 0;" << endl;
  oPopDriver << endl;

  // do data simulation first to replace DV data in IndData objects
  oPopDriver << "if( ret != SUCCESS )" << endl;
  oPopDriver << "  goto REPORT_GEN;" << endl;
  oPopDriver << endl;

  oPopDriver << "alpOut   = alpIn;" << endl;
  oPopDriver << "bOut     = bIn;" << endl;
  oPopDriver << "thetaOut = thetaIn;" << endl;
  oPopDriver << "omegaOut = omegaIn;" << endl;
  oPopDriver << "sigmaOut = sigmaIn;" << endl;
  oPopDriver << "remove( \"result.xml\" );" << endl;
  oPopDriver << "for( iSub=0; iSub<nRepeats; iSub++, seed = rand() )" << endl;
  oPopDriver << "{" << endl;
  oPopDriver << "   alpIn   = alpOut;" << endl;
  oPopDriver << "   bIn     = bOut;" << endl;
  oPopDriver << "   thetaIn = thetaOut;" << endl;
  oPopDriver << "   omegaIn = omegaOut;" << endl;
  oPopDriver << "   sigmaIn = sigmaOut;" << endl;
  oPopDriver << "   /*******************************************************************/" << endl;
  oPopDriver << "   /*                                                                 */" << endl;
  oPopDriver << "   /*   Data Initialization                                           */" << endl;
  oPopDriver << "   /*                                                                 */" << endl;
  oPopDriver << "   /*******************************************************************/" << endl;
  oPopDriver << "   if( isSimRequested )" << endl;
  oPopDriver << "   {" << endl;
  oPopDriver << "      valarray<double> yOut( nY );" << endl;
  oPopDriver << "      try" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         simulate( model, alpIn, N, bLow, bUp, yOut, bOut, NonmemPars::seed );" << endl;
  oPopDriver << "         bIn = bOut;" << endl;
  oPopDriver << "         set.replaceAllMeasurements( yOut );" << endl;
  oPopDriver << "         y   = yOut;" << endl;
  oPopDriver << "         haveCompleteData = true;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      catch( SpkException& e )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         char mess[ SpkError::maxMessageLen() ];" << endl;
  oPopDriver << "         sprintf( mess, \"Failed in data simulation.\\n\" );" << endl;
  oPopDriver << "         e.push( SpkError::SPK_SIMULATION_ERR, mess, __LINE__, __FILE__ );" << endl;
  oPopDriver << "         errors.cat( e );" << endl;
  oPopDriver << "         haveCompleteData = false;" << endl;
  oPopDriver << "         ret = SIMULATION_FAILURE;" << endl;
  oPopDriver << "         goto REPORT_GEN;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      catch( ... )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         char message[] =\"Unknown exception: failed in data simulation!!!\";" << endl;
  oPopDriver << "         SpkError e( SpkError::SPK_UNKNOWN_ERR, message, __LINE__, __FILE__ );" << endl;
  oPopDriver << "         errors.push( e );" << endl;
  oPopDriver << "         haveCompleteData = false;" << endl;
  oPopDriver << "         ret = SIMULATION_FAILURE;" << endl;
  oPopDriver << "         goto REPORT_GEN;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "   }" << endl;
  oPopDriver << "   else" << endl;
  oPopDriver << "   {" << endl;
  oPopDriver << "      y = set.getAllMeasurements();" << endl;
  oPopDriver << "      haveCompleteData = true;" << endl;
  oPopDriver << endl;
  oPopDriver << "   }" << endl;
  oPopDriver << endl;
 
  oPopDriver << "OPTIMIZATION:" << endl;
  oPopDriver << "   /*******************************************************************/" << endl;
  oPopDriver << "   /*                                                                 */" << endl;
  oPopDriver << "   /*   Parameter Estimation                                          */" << endl;
  oPopDriver << "   /*                                                                 */" << endl;
  oPopDriver << "   /*******************************************************************/" << endl;
  oPopDriver << "   if( isOptRequested && haveCompleteData )" << endl;
  oPopDriver << "   {" << endl;
  oPopDriver << "      gettimeofday( &optBegin, NULL );" << endl;
  oPopDriver << "      try" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         fitPopulation( model,"         << endl;
  oPopDriver << "                        objective, "    << endl;
  oPopDriver << "                        N,"             << endl;
  oPopDriver << "                        y,"             << endl;
  oPopDriver << "                        popOpt,"        << endl;
  oPopDriver << "                        alpLow,"        << endl;
  oPopDriver << "                        alpUp,"         << endl;
  oPopDriver << "                        alpIn,"         << endl;
  oPopDriver << "                        alpStep,"       << endl;
  oPopDriver << "                       &alpOut,"        << endl;
  oPopDriver << "                        indOpt,"        << endl;
  oPopDriver << "                        bLow,"          << endl;
  oPopDriver << "                        bUp,"           << endl;
  oPopDriver << "                        bIn,"           << endl;
  oPopDriver << "                        bStep,"         << endl;
  oPopDriver << "                       &bOut,"          << endl;
  oPopDriver << "                       &alpObjOut,"     << endl;
  oPopDriver << "                       &alpObj_alpOut," << endl;
  oPopDriver << "                       &alpObj_alp_alpOut );" << endl;
  oPopDriver << "         isOptSuccess = true;"          << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      catch( SpkException& e )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         char mess[ SpkError::maxMessageLen() ];" << endl;
  oPopDriver << "         sprintf( mess, \"Failed in population parameter estimation.\\n\" );" << endl;
  oPopDriver << "         e.push( SpkError::SPK_OPT_ERR, mess, __LINE__, __FILE__ );" << endl;
  oPopDriver << "         errors.cat( e );" << endl;
  oPopDriver << "         isOptSuccess = false;" << endl;
  oPopDriver << "         ret = CONVERGENCE_FAILURE;" << endl;
  oPopDriver << "         goto REPORT_GEN;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      catch( ... )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         char message[] = \"Unknown exception: failed in parameter estimation!!!\";" << endl;
  oPopDriver << "         SpkError e( SpkError::SPK_UNKNOWN_ERR, message, __LINE__, __FILE__ );" << endl;
  oPopDriver << "         errors.push( e );" << endl;
  oPopDriver << "         isOptSuccess = false;" << endl;
  oPopDriver << "         ret = CONVERGENCE_FAILURE;" << endl;
  oPopDriver << "         goto REPORT_GEN;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      gettimeofday( &optEnd, NULL );" << endl;
  oPopDriver << "      optTimeSec = difftime( optEnd.tv_sec, optBegin.tv_sec );" << endl;
  oPopDriver << endl;
  oPopDriver << "      // Get the latest values of theta, Omega and Sigma." << endl;
  oPopDriver << "      // These values may be garbage if optimization had failed." << endl;
  oPopDriver << "      model.getTheta( thetaOut );" << endl;
  oPopDriver << "      model.getOmega( omegaOut );" << endl;
  oPopDriver << "      model.getSigma( sigmaOut );" << endl;
  oPopDriver << "      set.replaceEta( bOut );" << endl;
  oPopDriver << endl;
  oPopDriver << "      if( !isOptSuccess )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         string optErrHeader;" << endl;
  oPopDriver << "         string optErrMessage;" << endl;
  oPopDriver << "         oLongError.open( \"" << fSpkRuntimeLongError_tmp << "\" );" << endl;
  oPopDriver << "         if( !oLongError.good() )" << endl;
  oPopDriver << "         {" << endl;
  oPopDriver << "            char m[ SpkError::maxMessageLen() ];" << endl;
  oPopDriver << "            sprintf( m, \"Failed to create a temporary file, %s, for writing.\", " << endl;
  oPopDriver << "                     \"" << fSpkRuntimeLongError_tmp << "\" );" << endl;
  oPopDriver << "            SpkError e( SpkError::SPK_STD_ERR, m, __LINE__, __FILE__ );" << endl;
  oPopDriver << "            errors.push( e );" << endl;
  oPopDriver << "            ret = FILE_ACCESS_FAILURE;" << endl;
  oPopDriver << "            goto REPORT_GEN;" << endl;
  oPopDriver << "         }" << endl;
  oPopDriver << "         // If individual level estimation failed, then get any details as to why." << endl;
  oPopDriver << "         if( indOpt.isThereErrorInfo() )" << endl;
  oPopDriver << "         {" << endl;
  oPopDriver << "            optErrHeader  = \"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \\n\";" << endl;
  oPopDriver << "            optErrHeader += \"Individual level optimization failure details. \\n\";" << endl;
  oPopDriver << "            optErrHeader += \"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \\n\";" << endl;
  oPopDriver << "            optErrHeader += \"\\n\";" << endl;
  oPopDriver << "            indOpt.getErrorInfo(" << endl;
  oPopDriver << "                                 optErrHeader," << endl;
  oPopDriver << "                                 optErrMessage," << endl;
  oPopDriver << "                                 __LINE__," << endl;
  oPopDriver << "                                 __FILE__ );" << endl;
  oPopDriver << "            oLongError << optErrMessage << endl;" << endl;
  oPopDriver << "         }" << endl;
  oPopDriver << "         // If population level estimation failed, then get any details as to why." << endl;
  oPopDriver << "         if( popOpt.isThereErrorInfo() )" << endl;
  oPopDriver << "         {" << endl;
  oPopDriver << "            optErrHeader  = \"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \\n\";" << endl;
  oPopDriver << "            optErrHeader += \"Population level optimization failure details. \\n\";" << endl;
  oPopDriver << "            optErrHeader += \"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \\n\";" << endl;
  oPopDriver << "            optErrHeader += \"\\n\";" << endl;
  oPopDriver << "            popOpt.getErrorInfo(" << endl;
  oPopDriver << "                                 optErrHeader," << endl;
  oPopDriver << "                                 optErrMessage," << endl;
  oPopDriver << "                                 __LINE__," << endl;
  oPopDriver << "                                 __FILE__ );" << endl;
  oPopDriver << "            oLongError << optErrMessage << endl;" << endl;
  oPopDriver << "            oLongError.close();" << endl;
  oPopDriver << "         }" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << endl;

  oPopDriver << "      ///////////////////////////////////////////////////////////////////" << endl;
  oPopDriver << "      //   NONMEM Specific" << endl;
  oPopDriver << "      if( isOptRequested && isOptSuccess )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         Objective objForDisposal = FIRST_ORDER;" << endl;
  oPopDriver << "         valarray<double> yi;"                    << endl;
  oPopDriver << "         valarray<double> bi( nB );"              << endl;
  oPopDriver << "         valarray<double> iiPredOut;"             << endl;
  oPopDriver << "         valarray<double> iiResOut; "             << endl;
  oPopDriver << "         valarray<double> iiResWtdOut;"           << endl;
  oPopDriver << "         valarray<double> iiParResOut( nB );"     << endl;
  oPopDriver << "         valarray<double> iiParResWtdOut( nB );"  << endl;
  oPopDriver << "         modelForDisposal.setPopPar( alpOut );"   << endl;
  oPopDriver << "         for( int i=0, k=0; i<nPop; k+=N[i++] )"  << endl;
  oPopDriver << "         {" << endl;
  oPopDriver << "            yi.resize( N[i] );"                   << endl;
  oPopDriver << "            iiPredOut.resize( N[i] );"            << endl;
  oPopDriver << "            iiResOut.resize( N[i] );"             << endl;
  oPopDriver << "            iiResWtdOut.resize( N[i] );"          << endl;
  oPopDriver << "            yi = y[ slice( k, N[i], 1 ) ]; "      << endl;
  oPopDriver << "            bi = bOut[ slice( i*nB, nB, 1 ) ];"   << endl;
  oPopDriver << "            modelForDisposal.selectIndividual( i );" << endl;
  oPopDriver << "            modelForDisposal.setIndPar( bi );"       << endl;
  oPopDriver << "            indResiduals( modelForDisposal,"         << endl;
  oPopDriver << "                          yi, "                   << endl;
  oPopDriver << "                          bi,"                    << endl;
  oPopDriver << "                         &iiPredOut,"             << endl;
  oPopDriver << "                         &iiResOut,"              << endl;
  oPopDriver << "                         &iiResWtdOut, "          << endl;
  oPopDriver << "                         &iiParResOut, "          << endl;
  oPopDriver << "                         &iiParResWtdOut );"      << endl;
  oPopDriver << "            iPredOut     [ slice( k, N[i], 1 ) ]  = iiPredOut;"      << endl;
  oPopDriver << "            iResOut      [ slice( k, N[i], 1 ) ]  = iiResOut;"       << endl;
  oPopDriver << "            iResWtdOut   [ slice( k, N[i], 1 ) ]  = iiResWtdOut;"    << endl;
  oPopDriver << "            iParResOut   [ slice( nB*i, nB, 1 ) ] = iiParResOut;"    << endl;
  oPopDriver << "            iParResWtdOut[ slice( nB*i, nB, 1 ) ] = iiParResWtdOut;" << endl;
  oPopDriver << "         }" << endl;
  oPopDriver << "         set.replaceIPred   ( iPredOut );"        << endl;
  oPopDriver << "         set.replaceIRes    ( iResOut );"         << endl;
  oPopDriver << "         set.replaceIWRes   ( iResWtdOut );"      << endl;
  oPopDriver << "         set.replaceIEtaRes ( iParResOut );"      << endl;
  oPopDriver << "         set.replaceIWEtaRes( iParResWtdOut );"   << endl;
  oPopDriver << endl;
  oPopDriver << "         objForDisposal = FIRST_ORDER;"           << endl;
  oPopDriver << "         popResiduals( modelForDisposal, "        << endl;
  oPopDriver << "                       objForDisposal, "          << endl;
  oPopDriver << "                       N,"                        << endl;
  oPopDriver << "                       y,"                        << endl;
  oPopDriver << "                       alpOut,"                   << endl;
  oPopDriver << "                       bOut,"                     << endl;
  oPopDriver << "                      &pPredOut, "                << endl;
  oPopDriver << "                      &pResOut, "                 << endl;
  oPopDriver << "                      &pResWtdOut, "              << endl;
  oPopDriver << "                      &pParResOut, "              << endl;
  oPopDriver << "                      &pParResWtdOut );"          << endl;
  oPopDriver << "         set.replacePPred   ( pPredOut );"        << endl;
  oPopDriver << "         set.replacePRes    ( pResOut );"         << endl;
  oPopDriver << "         set.replacePWRes   ( pResWtdOut );"      << endl;
  oPopDriver << "         set.replacePEtaRes ( pParResOut );"      << endl;
  oPopDriver << "         set.replacePWEtaRes( pParResWtdOut );"   << endl;
  oPopDriver << endl;
  oPopDriver << "         objForDisposal = EXPECTED_HESSIAN;"      << endl;
  oPopDriver << "         popResiduals( modelForDisposal, "        << endl;
  oPopDriver << "                       objForDisposal, "          << endl;
  oPopDriver << "                       N,"                        << endl;
  oPopDriver << "                       y,"                        << endl;
  oPopDriver << "                       alpOut,"                   << endl;
  oPopDriver << "                       bOut,"                     << endl;
  oPopDriver << "                      &cPredOut, "                << endl;
  oPopDriver << "                      &cResOut, "                 << endl;
  oPopDriver << "                      &cResWtdOut, "              << endl;
  oPopDriver << "                      &cParResOut, "              << endl;
  oPopDriver << "                      &cParResWtdOut );"          << endl;
  oPopDriver << "         set.replaceCPred   ( cPredOut );"        << endl;
  oPopDriver << "         set.replaceCRes    ( cResOut );"         << endl;
  oPopDriver << "         set.replaceCWRes   ( cResWtdOut );"      << endl;
  oPopDriver << "         set.replaceCEtaRes ( cParResOut );"      << endl;
  oPopDriver << "         set.replaceCWEtaRes( cParResWtdOut );"   << endl;
  oPopDriver << endl;
  oPopDriver << "         if( isPostHoc )" << endl;
  oPopDriver << "         {" << endl;
  oPopDriver << "            set.replacePred    ( iPredOut );"        << endl;
  oPopDriver << "            set.replaceRes     ( iResOut );"         << endl;
  oPopDriver << "            set.replaceWRes    ( iResWtdOut );"      << endl;
  oPopDriver << "            set.replaceEtaRes  ( iParResOut );"      << endl;
  oPopDriver << "            set.replaceWEtaRes ( iParResWtdOut );"   << endl;
  oPopDriver << "         }" << endl;
  oPopDriver << "         else" << endl;
  oPopDriver << "         {" << endl;
  oPopDriver << "            if( objective == FIRST_ORDER )" << endl;
  oPopDriver << "            {" << endl;
  oPopDriver << "               set.replacePred    ( pPredOut );"        << endl;
  oPopDriver << "               set.replaceRes     ( pResOut );"         << endl;
  oPopDriver << "               set.replaceWRes    ( pResWtdOut );"      << endl;
  oPopDriver << "               set.replaceEtaRes  ( pParResOut );"      << endl;
  oPopDriver << "               set.replaceWEtaRes ( pParResWtdOut );"   << endl;
  oPopDriver << "            }" << endl;
  oPopDriver << "            else // conditional" << endl;
  oPopDriver << "            {" << endl;
  oPopDriver << "               set.replacePred    ( cPredOut );"        << endl;
  oPopDriver << "               set.replaceRes     ( cResOut );"         << endl;
  oPopDriver << "               set.replaceWRes    ( cResWtdOut );"      << endl;
  oPopDriver << "               set.replaceEtaRes  ( cParResOut );"      << endl;
  oPopDriver << "               set.replaceWEtaRes ( cParResWtdOut );"   << endl;
  oPopDriver << "            }" << endl;
  oPopDriver << "         }" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "     //" << endl;
  oPopDriver << "     //////////////////////////////////////////////////////////////////////" << endl;    
  oPopDriver << "   }" << endl;
  oPopDriver << endl;
 
  // Statistics can be only computed when the parameter estimation has been done.
  oPopDriver << "STATISTICS:" << endl;
  oPopDriver << "   /*******************************************************************/" << endl;
  oPopDriver << "   /*                                                                 */" << endl;
  oPopDriver << "   /*   Statistics                                                    */" << endl;
  oPopDriver << "   /*                                                                 */" << endl;
  oPopDriver << "   /*******************************************************************/" << endl;
	  
  oPopDriver << "   if( isStatRequested && isOptRequested && haveCompleteData && isOptSuccess )" << endl;
  oPopDriver << "   {" << endl;
  oPopDriver << "      gettimeofday( &statBegin, NULL );" << endl;
  oPopDriver << "      try" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         for( int i=0; i<nAlp; i++ )" << endl;
  oPopDriver << "         {" << endl;
  oPopDriver << "            alpMask[i] = !( alpLow[i]==alpUp[i] || alpOut[i]==alpLow[i] || alpOut[i]==alpUp[i] );" << endl;
  oPopDriver << "         }" << endl;
  oPopDriver << "         model.getStandardPar( stdPar );" << endl;
  oPopDriver << "         model.getStandardPar_popPar( stdPar_alp );" << endl;
  oPopDriver << "         popStatistics(    modelForDisposal, "                  << endl;
  oPopDriver << "                           objective,"               << endl;
  oPopDriver << "                           N,"                       << endl;
  oPopDriver << "                           y,"                       << endl;
  oPopDriver << "                           alpOut, "                 << endl;
  oPopDriver << "                           alpMask,"                 << endl;
  oPopDriver << "                           alpObj_alp_alpOut, "      << endl;
  oPopDriver << "                           bOut,"                    << endl;
  oPopDriver << "                           bLow,"                    << endl;
  oPopDriver << "                           bUp,"                     << endl;
  oPopDriver << "                           bStep,"                   << endl;
  oPopDriver << "                           covForm,"                 << endl;
  oPopDriver << "                          &alpCov, "                 << endl;
  oPopDriver << "                           NULL, "                   << endl;
  oPopDriver << "                           NULL, "                   << endl;
  oPopDriver << "                           NULL, "                   << endl;
  oPopDriver << "                           NULL );"                  << endl;
  oPopDriver << endl;
  oPopDriver << "         derParStatistics( alpMask,"                 << endl;
  oPopDriver << "                           alpCov,"                  << endl;
  oPopDriver << "                           stdPar,"                  << endl;
  oPopDriver << "                           stdPar_alp,"              << endl;
  oPopDriver << "                           nDegOfFreedom,"           << endl;
  oPopDriver << "                          (isCovOut || isInvCovOut? &stdParCovOut        : NULL)," << endl;
  oPopDriver << "                          (isCovOut || isInvCovOut? &stdParInvCovOut     : NULL)," << endl;
  oPopDriver << "                          (isStdErrOut?             &stdParSEOut         : NULL)," << endl;
  oPopDriver << "                          (isCorrelationOut?        &stdParCorrelationOut: NULL)," << endl;
  oPopDriver << "                          (isCoefficientOut?        &stdParCoefficientOut: NULL)," << endl;
  oPopDriver << "                          (isConfidenceOut?         &stdParConfidenceOut : NULL) " << endl;
  oPopDriver << "                         );" << endl;
  oPopDriver << "         isStatSuccess = true;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      catch( SpkException& e )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         char mess[ SpkError::maxMessageLen() ];" << endl;
  oPopDriver << "         sprintf( mess, \"Failed to compute statistics value(s).\\n\" );" << endl;
  oPopDriver << "         e.push( SpkError::SPK_STATISTICS_ERR, mess, __LINE__, __FILE__ );" << endl;
  oPopDriver << "         errors.cat( e );" << endl;
  oPopDriver << "         isStatSuccess = false;" << endl;
  oPopDriver << "         ret = STAT_FAILURE;" << endl;
  oPopDriver << "         goto REPORT_GEN;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      catch( ... )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         char message[] = \"Unknown exception: failed in statistics calculation!!!\";" << endl;
  oPopDriver << "         SpkError e( SpkError::SPK_UNKNOWN_ERR, message, __LINE__, __FILE__ );" << endl;
  oPopDriver << "         errors.push( e );" << endl;
  oPopDriver << "         isStatSuccess = false;" << endl;
  oPopDriver << "         ret = STAT_FAILURE;" << endl;
  oPopDriver << "         goto REPORT_GEN;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      gettimeofday( &statEnd, NULL );" << endl;
  oPopDriver << "      statTimeSec = difftime( statEnd.tv_sec, statBegin.tv_sec );" << endl;
  oPopDriver << "   }" << endl;
  oPopDriver << endl;

  oPopDriver << "REPORT_GEN:" << endl;
  oPopDriver << "   /*******************************************************************/" << endl;
  oPopDriver << "   /*                                                                 */" << endl;
  oPopDriver << "   /*   ReportML Document                                             */" << endl;
  oPopDriver << "   /*                                                                 */" << endl;
  oPopDriver << "   /*******************************************************************/" << endl;
  oPopDriver << "   oResults.open( \"" << fResult_xml << "\", ios_base::app );" << endl;
  oPopDriver << "   if( !oResults.good() )" << endl;
  oPopDriver << "   {" << endl;
  oPopDriver << "      fprintf( stderr, \"Failed to open a file, %s, for writing output!!!\", \"";
  oPopDriver << fResult_xml << "\" );" << endl;
  oPopDriver << "      ret = FILE_ACCESS_FAILURE;" << endl;
  oPopDriver << "      oResults << \"</spkreport>\" << endl;" << endl;
  oPopDriver << "      oResults.close();" << endl;
  oPopDriver << "      goto END;" << endl;
  oPopDriver << "   }" << endl;

  //  oPopDriver << "   if( iSub == 0 )" << endl;
  oPopDriver << "   {" << endl;
  oPopDriver << "      oResults << \"<?xml version=\\\"1.0\\\"?>\" << endl;" << endl;
  oPopDriver << "      oResults << \"<spkreport>\" << endl;" << endl;
  oPopDriver << "   }" << endl;
  oPopDriver << endl;

  // Print out <error_list> even when it is empty.
  oPopDriver << "   oResults << \"<error_list length=\\\"\" << errors.size() << \"\\\">\" << endl;" << endl;
  oPopDriver << "   if( !(haveCompleteData && isOptSuccess && isStatSuccess) )" << endl;
  oPopDriver << "   {" << endl;
  oPopDriver << "      // Print out a long error message if exists." << endl;
  oPopDriver << "      char buf[ 128 ];" << endl;
  oPopDriver << "      ifstream iLongError( \"" << fSpkRuntimeLongError_tmp << "\" );" << endl;
  oPopDriver << "      while( iLongError.getline(buf, 128) )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         oResults << buf << endl;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << endl;
  oPopDriver << "      // Print out ordinary-length error messages" << endl;
  oPopDriver << "      oResults << errors << endl;" << endl;
  oPopDriver << "      iLongError.close();" << endl;
  oPopDriver << "   }" << endl;
  oPopDriver << "   oResults << \"</error_list>\" << endl;" << endl;
  oPopDriver << "   remove( \"" << fSpkRuntimeLongError_tmp << "\" );" << endl;
  oPopDriver << endl;

  // Print out <warning_list> even when it is empty.
  oPopDriver << "   WarningsManager::getAllWarnings( warningsOut );" << endl;
  oPopDriver << "   oResults << warningsOut;" << endl;
  oPopDriver << endl;

  oPopDriver << "   if( ret != SUCCESS )" << endl;
  oPopDriver << "   {" << endl;
  oPopDriver << "      oResults << \"</spkreport>\" << endl;" << endl;
  oPopDriver << "      oResults.close();" << endl;
  oPopDriver << "      goto END;" << endl;
  oPopDriver << "   }" << endl;
  oPopDriver << endl;

  oPopDriver << "   if( isSimRequested )" << endl;
  oPopDriver << "      oResults << \"<simulation seed=\\\"\" << seed << \"\\\" subproblem=\\\"\" << iSub+1 << \"\\\"/>\" << endl;" << endl;
  oPopDriver << endl;

  oPopDriver << "   if( isOptRequested )" << endl;
  oPopDriver << "   {" << endl;
  oPopDriver << "      oResults << \"<pop_analysis_result>\" << endl;" << endl;
  oPopDriver << endl;

  oPopDriver << "      oResults << \"<pop_opt_result elapsedtime=\\\"\" << optTimeSec << \"\\\">\" << endl;" << endl;
  oPopDriver << "      oResults << \"<pop_obj_out>\" << endl;" << endl;
  oPopDriver << "      oResults << \"<value>\" << alpObjOut << \"</value>\" << endl;" << endl;
  oPopDriver << "      oResults << \"</pop_obj_out>\" << endl;" << endl;
  oPopDriver << endl;
  
  oPopDriver << "      ///////////////////////////////////////////////////////////////////" << endl;
  oPopDriver << "      //   NONMEM Specific" << endl;
  /*
  // theta <in> and <out>
  oPopDriver << "      oResults << \"<theta length=\\\"\" << NonmemPars::nTheta << \"\\\">\" << endl;" << endl;
  oPopDriver << "      oResults << \"<in>\" << endl;" << endl;
  oPopDriver << "      for( int i=0; i<NonmemPars::nTheta; i++ )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         oResults << \"<value>\" << thetaIn[i] << \"</value>\" << endl;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      oResults << \"</in>\" << endl;" << endl;
  oPopDriver << "      oResults << \"<out>\" << endl;" << endl;
  oPopDriver << "      for( int i=0; i<NonmemPars::nTheta; i++ )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         oResults << \"<value>\" << thetaOut[i] << \"</value>\" << endl;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      oResults << \"</out>\" << endl;" << endl;
  oPopDriver << "      oResults << \"</theta>\" << endl;" << endl;

  // Omega <in> and <out>
  oPopDriver << "      oResults << \"<omega dimension=\" << \"\\\"\" << NonmemPars::omegaDim << \"\\\"\";" << endl;
  oPopDriver << "      oResults << \" struct=\" << \"\\\"\";" << endl;
  oPopDriver << "      if( NonmemPars::omegaStruct==PopPredModel::DIAGONAL )" << endl;
  oPopDriver << "         oResults << \"diagonal\";" << endl;
  oPopDriver << "      else" << endl;
  oPopDriver << "         oResults << \"block\";" << endl;
  oPopDriver << "      oResults << \"\\\"\" << \">\" << endl;" << endl;

  oPopDriver << "      oResults << \"<in>\" << endl;" << endl;
  oPopDriver << "      for( int i=0; i<NonmemPars::omegaOrder; i++ )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         oResults << \"<value>\" << omegaIn[i] << \"</value>\" << endl;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      oResults << \"</in>\" << endl;" << endl;
  oPopDriver << "      oResults << \"<out>\" << endl;" << endl;
  oPopDriver << "      for( int i=0; i<NonmemPars::omegaOrder; i++ )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         oResults << \"<value>\" << omegaOut[i] << \"</value>\" << endl;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      oResults << \"</out>\" << endl;" << endl;
  oPopDriver << "      oResults << \"</omega>\" << endl;" << endl;

  // Sigma <in> and <out>
  oPopDriver << "      oResults << \"<sigma dimension=\" << \"\\\"\" << NonmemPars::sigmaDim << \"\\\"\";" << endl;
  oPopDriver << "      oResults << \" struct=\" << \"\\\"\";" << endl;
  oPopDriver << "      if( NonmemPars::sigmaStruct==PopPredModel::DIAGONAL )" << endl;
  oPopDriver << "         oResults << \"diagonal\";" << endl;
  oPopDriver << "      else" << endl;
  oPopDriver << "         oResults << \"block\";" << endl;
  oPopDriver << "      oResults << \"\\\"\" << \">\" << endl;" << endl;
  oPopDriver << "      oResults << \"<in>\" << endl;" << endl;
  oPopDriver << "      for( int i=0; i<NonmemPars::sigmaOrder; i++ )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         oResults << \"<value>\" << sigmaIn[i] << \"</value>\" << endl;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      oResults << \"</in>\" << endl;" << endl;
  oPopDriver << "      oResults << \"<out>\" << endl;" << endl;
  oPopDriver << "      for( int i=0; i<NonmemPars::sigmaOrder; i++ )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         oResults << \"<value>\" << sigmaOut[i] << \"</value>\" << endl;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      oResults << \"</out>\" << endl;" << endl;
  oPopDriver << "      oResults << \"</sigma>\" << endl;" << endl;
  */

  // theta in
  oPopDriver << "      oResults << \"<theta_in length=\\\"\" << NonmemPars::nTheta << \"\\\">\" << endl;" << endl;
  oPopDriver << "      for( int i=0; i<NonmemPars::nTheta; i++ )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         oResults << \"<value>\" << thetaIn[i] << \"</value>\" << endl;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      oResults << \"</theta_in>\" << endl;" << endl;
  // theta out
  oPopDriver << "      oResults << \"<theta_out length=\\\"\" << NonmemPars::nTheta << \"\\\">\" << endl;" << endl;
  oPopDriver << "      for( int i=0; i<NonmemPars::nTheta; i++ )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         oResults << \"<value>\" << thetaOut[i] << \"</value>\" << endl;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      oResults << \"</theta_out>\" << endl;" << endl;

  // sigam in
  oPopDriver << "      oResults << \"<sigma_in dimension=\" << \"\\\"\" << NonmemPars::sigmaDim << \"\\\"\";" << endl;
  oPopDriver << "      oResults << \" struct=\" << \"\\\"\";" << endl;
  oPopDriver << "      if( NonmemPars::sigmaStruct==PopPredModel::DIAGONAL )" << endl;
  oPopDriver << "         oResults << \"diagonal\";" << endl;
  oPopDriver << "      else" << endl;
  oPopDriver << "         oResults << \"block\";" << endl;
  oPopDriver << "      oResults << \"\\\"\" << \">\" << endl;" << endl;
  oPopDriver << "      for( int i=0; i<NonmemPars::sigmaOrder; i++ )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         oResults << \"<value>\" << sigmaIn[i] << \"</value>\" << endl;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      oResults << \"</sigma_in>\" << endl;" << endl;
  // sigma out
  oPopDriver << "      oResults << \"<sigma_out dimension=\" << \"\\\"\" << NonmemPars::sigmaDim << \"\\\"\";" << endl;
  oPopDriver << "      oResults << \" struct=\" << \"\\\"\";" << endl;
  oPopDriver << "      if( NonmemPars::sigmaStruct==PopPredModel::DIAGONAL )" << endl;
  oPopDriver << "         oResults << \"diagonal\";" << endl;
  oPopDriver << "      else" << endl;
  oPopDriver << "         oResults << \"block\";" << endl;
  oPopDriver << "      oResults << \"\\\"\" << \">\" << endl;" << endl;
  oPopDriver << "      for( int i=0; i<NonmemPars::sigmaOrder; i++ )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         oResults << \"<value>\" << sigmaOut[i] << \"</value>\" << endl;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      oResults << \"</sigma_out>\" << endl;" << endl;

  // omega in
  oPopDriver << "      oResults << \"<omega_in dimension=\" << \"\\\"\" << NonmemPars::omegaDim << \"\\\"\";" << endl;
  oPopDriver << "      oResults << \" struct=\" << \"\\\"\";" << endl;
  oPopDriver << "      if( NonmemPars::omegaStruct==PopPredModel::DIAGONAL )" << endl;
  oPopDriver << "         oResults << \"diagonal\";" << endl;
  oPopDriver << "      else" << endl;
  oPopDriver << "         oResults << \"block\";" << endl;
  oPopDriver << "      oResults << \"\\\"\" << \">\" << endl;" << endl;
  oPopDriver << "      for( int i=0; i<NonmemPars::omegaOrder; i++ )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         oResults << \"<value>\" << omegaIn[i] << \"</value>\" << endl;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      oResults << \"</omega_in>\" << endl;" << endl;
  // omega out
  oPopDriver << "      oResults << \"<omega_out dimension=\" << \"\\\"\" << NonmemPars::omegaDim << \"\\\"\";" << endl;
  oPopDriver << "      oResults << \" struct=\" << \"\\\"\";" << endl;
  oPopDriver << "      if( NonmemPars::omegaStruct==PopPredModel::DIAGONAL )" << endl;
  oPopDriver << "         oResults << \"diagonal\";" << endl;
  oPopDriver << "      else" << endl;
  oPopDriver << "         oResults << \"block\";" << endl;
  oPopDriver << "      oResults << \"\\\"\" << \">\" << endl;" << endl;
  oPopDriver << "      for( int i=0; i<NonmemPars::omegaOrder; i++ )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         oResults << \"<value>\" << omegaOut[i] << \"</value>\" << endl;" << endl;
  oPopDriver << "      }" << endl;
  oPopDriver << "      oResults << \"</omega_out>\" << endl;" << endl;
  oPopDriver << "      //" << endl;

  oPopDriver << "      ///////////////////////////////////////////////////////////////////" << endl;
  oPopDriver << "      oResults << \"</pop_opt_result>\" << endl;" << endl;
  oPopDriver << endl;
  
  oPopDriver << "      if( isCovOut || isInvCovOut || isStdErrOut || isCorrelationOut || isConfidenceOut || isCoefficientOut )" << endl;
  oPopDriver << "      {" << endl;
  oPopDriver << "         oResults << \"<pop_stat_result elapsedtime=\\\"\" << statTimeSec << \"\\\">\" << endl;" << endl;
  oPopDriver << "         if( isCovOut )" << endl;
  oPopDriver << "         {" << endl;
  oPopDriver << "            oResults << \"<pop_covariance_out struct=\\\"block\\\" dimension=\\\"\" << nAlp << \"\\\">\" << endl;" << endl;
  oPopDriver << "            for( int i=0; i<nAlp; i++ )" << endl;
  oPopDriver << "            {" << endl;
  oPopDriver << "               for( int j=0; j<=i; j++ )" << endl;
  oPopDriver << "               {" << endl;
  oPopDriver << "                  oResults << \"   <value>\" << stdParCovOut[i+j*nAlp] << \"</value>\" << endl;" << endl;
  oPopDriver << "               }" << endl;
  oPopDriver << "            }" << endl;
  oPopDriver << "            oResults << \"</pop_covariance_out>\" << endl;" << endl;
  oPopDriver << "         }" << endl;
  
  oPopDriver << "         if( isInvCovOut )" << endl;
  oPopDriver << "         {" << endl;
  oPopDriver << "            oResults << \"<pop_inverse_covariance_out struct=\\\"block\\\" dimension=\\\"\" << nAlp << \"\\\">\" << endl;" << endl;
  oPopDriver << "            for( int i=0; i<nAlp; i++ )" << endl;
  oPopDriver << "            {" << endl;
  oPopDriver << "               for( int j=0; j<=i; j++ )" << endl;
  oPopDriver << "               {" << endl;
  oPopDriver << "                  oResults << \"   <value>\" << stdParInvCovOut[i+j*nAlp] << \"</value>\" << endl;" << endl;
  oPopDriver << "               }" << endl;
  oPopDriver << "            }" << endl;
  oPopDriver << "            oResults << \"</pop_inverse_covariance_out>\" << endl;" << endl;
  oPopDriver << "         }" << endl;
  
  oPopDriver << "         if( isStdErrOut )" << endl;
  oPopDriver << "         {" << endl;
  oPopDriver << "            oResults << \"<pop_stderror_out length=\\\"\" << nAlp << \"\\\">\" << endl;" << endl;
  oPopDriver << "            for( int i=0; i<nAlp; i++ )" << endl;
  oPopDriver << "            {" << endl;
  oPopDriver << "               oResults << \"   <value>\" << stdParSEOut[i] << \"</value>\" << endl;" << endl;
  oPopDriver << "            }" << endl;	      
  oPopDriver << "            oResults << \"</pop_stderror_out>\" << endl;" << endl;
  oPopDriver << "         }" << endl;
  
  oPopDriver << "         if( isCorrelationOut )" << endl;
  oPopDriver << "         {" << endl;
  oPopDriver << "            oResults << \"<pop_correlation_out struct=\\\"block\\\" dimension=\\\"\" << nAlp << \"\\\">\" << endl;" << endl;
  oPopDriver << "            for( int i=0; i<nAlp; i++ )" << endl;
  oPopDriver << "            {" << endl;
  oPopDriver << "               for( int j=0; j<=i; j++ )" << endl;
  oPopDriver << "               {" << endl;
  oPopDriver << "                  oResults << \"   <value>\" << stdParCorrelationOut[i+j*nAlp] << \"</value>\" << endl;" << endl;
  oPopDriver << "               }" << endl;
  oPopDriver << "            }" << endl;   
  oPopDriver << "            oResults << \"</pop_correlation_out>\" << endl;" << endl;
  oPopDriver << "         }" << endl;
  
  oPopDriver << "         if( isCoefficientOut )" << endl;
  oPopDriver << "         {" << endl;
  oPopDriver << "            oResults << \"<pop_coefficient_out length=\\\"\" << nAlp << \"\\\">\" << endl;" << endl;
  oPopDriver << "            for( int i=0; i<nAlp; i++ )" << endl;
  oPopDriver << "            {" << endl;
  oPopDriver << "               oResults << \"   <value>\" << stdParCoefficientOut[i] << \"</value>\" << endl;" << endl;
  oPopDriver << "            }" << endl;	      
  oPopDriver << "            oResults << \"</pop_coefficient_out>\" << endl;" << endl;
  oPopDriver << "         }" << endl;
  
  oPopDriver << "         if( isConfidenceOut )" << endl;
  oPopDriver << "         {" << endl;
  oPopDriver << "            oResults << \"<pop_confidence_out length=\\\"\" << nAlp*2 << \"\\\">\" << endl;" << endl;
  oPopDriver << "            for( int i=0; i<nAlp*2; i++ )" << endl;
  oPopDriver << "            {" << endl;
  oPopDriver << "               oResults << \"   <value>\" << stdParConfidenceOut[i] << \"</value>\" << endl;" << endl;
  oPopDriver << "            }" << endl;    
  oPopDriver << "            oResults << \"</pop_confidence_out>\" << endl;" << endl;
  oPopDriver << "         }" << endl;
  
  oPopDriver << "         oResults << \"</pop_stat_result>\" << endl;" << endl;
  oPopDriver << endl;
  oPopDriver << "      }" << endl;
  
  oPopDriver << "      oResults << \"</pop_analysis_result>\" << endl;" << endl;
  oPopDriver << "      if( iSub == nRepeats-1 )" << endl;
  oPopDriver << "         oResults << set << endl;" << endl;
  oPopDriver << "      oResults << \"</spkreport>\" << endl;" << endl;
  oPopDriver << "      oResults.close();" << endl;
  oPopDriver << "   }" << endl;
  oPopDriver << endl;

  // Print out <presentation_data> if this is the last subproblem.
  oPopDriver << "}" << endl;
  oPopDriver << endl;
  oPopDriver << "END:" << endl;
 
  //  oPopDriver << "   oResults << \"</spkreport>\" << endl;" << endl;
  //  oPopDriver << "   oResults.close();" << endl;
  oPopDriver << "   cout << \"exit code = \" << ret << endl;" << endl;
  oPopDriver << "   return ret;" << endl;
  oPopDriver << "}" << endl;
  oPopDriver.close();
}
