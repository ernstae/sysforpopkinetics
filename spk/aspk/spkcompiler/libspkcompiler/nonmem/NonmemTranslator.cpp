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
extern int           gSpkExpLines;
extern SymbolTable * gSpkExpSymbolTable;
extern FILE *        gSpkExpOutput;
extern FILE *        nm_in;
extern int           NM_ACCEPT;
extern int           NM_ABORT;

extern "C"{
     int nm_parse(void);
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
const char* NonmemTranslator::C_NONMEM                     ( "nonmem" );
const char* NonmemTranslator::C_POP_ANALYSIS               ( "pop_analysis" );
const char* NonmemTranslator::C_IND_ANALYSIS               ( "ind_analysis" );
const char* NonmemTranslator::C_CONSTRAINT                 ( "constraint" );
const char* NonmemTranslator::C_MODEL                      ( "model" );
const char* NonmemTranslator::C_PRED                       ( "pred" );
const char* NonmemTranslator::C_PRESENTATION               ( "presentation" );
const char* NonmemTranslator::C_TABLE                      ( "table" );
const char* NonmemTranslator::C_SCATTERPLOT                ( "scatterplot" );
const char* NonmemTranslator::C_COLUMN                     ( "column" );
const char* NonmemTranslator::C_X                          ( "x" );
const char* NonmemTranslator::C_Y                          ( "y" );
const char* NonmemTranslator::C_BY                         ( "by" );
const char* NonmemTranslator::C_APPROXIMATION              ( "approximation" );
const char* NonmemTranslator::C_FO                         ( "fo" );
const char* NonmemTranslator::C_FOCE                       ( "foce" );
const char* NonmemTranslator::C_LAPLACE                    ( "laplace" );
const char* NonmemTranslator::C_MONTE_CARLO                ( "monte_carlo" );
const char* NonmemTranslator::C_METHOD                     ( "method" );
const char* NonmemTranslator::C_MONTE                      ( "monte" );
const char* NonmemTranslator::C_NUMBEREVAL                 ( "number_eval" );
const char* NonmemTranslator::C_ANALYTIC                   ( "analytic" );
const char* NonmemTranslator::C_GRID                       ( "grid" );
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
const char* NonmemTranslator::C_ONLYSIMULATION             ( "onlysimulation" );
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
    fNonmemPars_h       ( "NonmemPars.h" ),
    fMontePars_h        ( "MontePars.h" ),
    fFitDriver_cpp      ( "fitDriver.cpp" ),
    fMonteDriver_cpp    ( "monteDriver.cpp" ),
    fSpkRuntimeError_tmp( "spk_error.tmp" ),
    fResult_xml         ( "result.xml" ),

    myDescription       ( NULL ),
    myTarget            ( POP ),
    myModelSpec         ( PRED ),
    myIsEstimate        ( true ),
    myIsSimulate        ( false ),
    myIsMonte           ( false ),
    myIsStat            ( false ),
    myIsOnlySimulation  ( false ),
    mySubproblemsN      ( 1 ),
    myApproximation     ( FO ),
    myMonteMethod       ( MONTE ),
    myMonteNumberEval   ( 1000 ),
    myPopSize           ( 1 ),
    myIsEtaOut          ( false ),
    myIsRestart         ( true ),
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
    myCovForm           ( "R" ),
    myIsStderr          ( true ),
    myIsCorrelation     ( true ),
    myIsCov             ( true ),
    myIsInvCov          ( true ),
    myIsConfidence      ( true ),
    myIsCoefficient     ( true ),
    myRecordNums        ( 1 )

{
  table = ClientTranslator::getSymbolTable();

  DefaultStr.THETA = "THETA";
  DefaultStr.ETA   = "ETA";
  DefaultStr.EPS   = "EPS";
  DefaultStr.OMEGA = "OMEGA";
  DefaultStr.SIGMA = "SIGMA";
  DefaultStr.RES   = "RES";
  DefaultStr.WRES  = "WRES";
  DefaultStr.PRED  = "PRED";
  DefaultStr.DV    = "DV";
  DefaultStr.MDV   = "MDV";
  DefaultStr.ID    = "ID";
  DefaultStr.SIMDV = "SIMDV";
  DefaultStr.F     = "F";
  DefaultStr.Y     = "Y";

  UserStr.THETA = DefaultStr.THETA;
  UserStr.ETA   = DefaultStr.ETA;
  UserStr.EPS   = DefaultStr.EPS;
  UserStr.OMEGA = DefaultStr.OMEGA;
  UserStr.SIGMA = DefaultStr.SIGMA;
  UserStr.RES   = DefaultStr.RES;
  UserStr.WRES  = DefaultStr.WRES;
  UserStr.PRED  = DefaultStr.PRED;
  UserStr.DV    = DefaultStr.DV;
  UserStr.MDV   = DefaultStr.MDV;
  UserStr.ID    = DefaultStr.ID;
  UserStr.SIMDV = DefaultStr.SIMDV;
  UserStr.F     = DefaultStr.F;
  UserStr.Y     = DefaultStr.Y;

  // These are used as insensitive search keys to find the values of
  // NONMEM-predefined variables in the symbol table or to be extracted
  // as C++ variable names when cases are supposed to be insensitive.
  KeyStr.THETA = SymbolTable::key( DefaultStr.THETA );
  KeyStr.ETA   = SymbolTable::key( DefaultStr.ETA );
  KeyStr.EPS   = SymbolTable::key( DefaultStr.EPS );
  KeyStr.OMEGA = SymbolTable::key( DefaultStr.OMEGA );
  KeyStr.SIGMA = SymbolTable::key( DefaultStr.SIGMA );
  KeyStr.RES   = SymbolTable::key( DefaultStr.RES );
  KeyStr.WRES  = SymbolTable::key( DefaultStr.WRES );
  KeyStr.PRED  = SymbolTable::key( DefaultStr.PRED );
  KeyStr.DV    = SymbolTable::key( DefaultStr.DV );
  KeyStr.MDV   = SymbolTable::key( DefaultStr.MDV );
  KeyStr.ID    = SymbolTable::key( DefaultStr.ID );
  KeyStr.SIMDV = SymbolTable::key( DefaultStr.SIMDV );
  KeyStr.F     = SymbolTable::key( DefaultStr.F );
  KeyStr.Y     = SymbolTable::key( DefaultStr.Y );

  // TAG names
  X_DESCRIPTION    = XMLString::transcode( C_DESCRIPTION );
  X_IN             = XMLString::transcode( C_IN );
  X_NONMEM         = XMLString::transcode( C_NONMEM );
  X_POP_ANALYSIS   = XMLString::transcode( C_POP_ANALYSIS );
  X_IND_ANALYSIS   = XMLString::transcode( C_IND_ANALYSIS );
  X_CONSTRAINT     = XMLString::transcode( C_CONSTRAINT );
  X_MONTE_CARLO    = XMLString::transcode( C_MONTE_CARLO );
  X_MODEL          = XMLString::transcode( C_MODEL );
  X_PRED           = XMLString::transcode( C_PRED );
  X_PRESENTATION   = XMLString::transcode( C_PRESENTATION );
  X_TABLE          = XMLString::transcode( C_TABLE );
  X_SCATTERPLOT    = XMLString::transcode( C_SCATTERPLOT );
  X_COLUMN         = XMLString::transcode( C_COLUMN );
  X_LOW            = XMLString::transcode( C_LOW );
  X_UP             = XMLString::transcode( C_UP );
  X_LABEL          = XMLString::transcode( C_LABEL );
  X_LABELS         = XMLString::transcode( C_LABELS );
  X_X              = XMLString::transcode( C_X );
  X_Y              = XMLString::transcode( C_Y );
  X_BY             = XMLString::transcode( C_BY );
  X_THETA          = XMLString::transcode( C_THETA );
  X_OMEGA          = XMLString::transcode( C_OMEGA );
  X_SIGMA          = XMLString::transcode( C_SIGMA );
  X_SIMULATION     = XMLString::transcode( C_SIMULATION );
  X_POP_STAT       = XMLString::transcode( C_POP_STAT );
  X_IND_STAT       = XMLString::transcode( C_IND_STAT );

  // Attribute names 
  X_FIXED          = XMLString::transcode( C_FIXED );
  X_VALUE          = XMLString::transcode( C_VALUE );
  X_STRUCT         = XMLString::transcode( C_STRUCT );
  X_DIMENSION      = XMLString::transcode( C_DIMENSION );
  X_IS_ERR_OUT     = XMLString::transcode( C_IS_STDERROR_OUT );
  X_IS_CORR_OUT    = XMLString::transcode( C_IS_CORRELATION_OUT );
  X_IS_COV_OUT     = XMLString::transcode( C_IS_COVARIANCE_OUT );
  X_IS_INV_COV_OUT = XMLString::transcode( C_IS_INVERSE_COVARIANCE_OUT );
  X_IS_COEF_OUT    = XMLString::transcode( C_IS_COEFFICIENT_OUT );
  X_IS_CONF_OUT    = XMLString::transcode( C_IS_CONFIDENCE_OUT );
  X_APPROXIMATION  = XMLString::transcode( C_APPROXIMATION );
  X_METHOD         = XMLString::transcode( C_METHOD );
  X_NUMBEREVAL     = XMLString::transcode( C_NUMBEREVAL );
  X_POP_SIZE       = XMLString::transcode( C_POP_SIZE  );
  X_IS_ESTIMATION  = XMLString::transcode( C_IS_ESTIMATION );
  X_IS_ETA_OUT     = XMLString::transcode( C_IS_ETA_OUT );
  X_IS_RESTART     = XMLString::transcode( C_IS_RESTART );
  X_DATA_LABELS    = XMLString::transcode( C_DATA_LABELS );
  X_FILENAME       = XMLString::transcode( C_FILENAME );
  X_NAME           = XMLString::transcode( C_NAME );
  X_SYNONYM        = XMLString::transcode( C_SYNONYM );
  X_LENGTH         = XMLString::transcode( C_LENGTH );
  X_ONLYSIMULATION = XMLString::transcode( C_ONLYSIMULATION );
  X_SEED           = XMLString::transcode( C_SEED );
  X_SUBPROBLEMS    = XMLString::transcode( C_SUBPROBLEMS );
  X_COVARIANCE_FORM= XMLString::transcode( C_COVARIANCE_FORM );
  X_MITR           = XMLString::transcode( C_MITR );
  X_SIG_DIGITS     = XMLString::transcode( C_SIG_DIGITS );

  // Attribute values
  X_YES            = XMLString::transcode( C_YES );
  X_NO             = XMLString::transcode( C_NO );
  X_DIAGONAL       = XMLString::transcode( C_DIAGONAL );
  X_BLOCK          = XMLString::transcode( C_BLOCK );
  X_COV_R          = XMLString::transcode( C_COV_R );
  X_COV_RSR        = XMLString::transcode( C_COV_RSR );
  X_COV_S          = XMLString::transcode( C_COV_S );
  X_FO             = XMLString::transcode( C_FO );
  X_FOCE           = XMLString::transcode( C_FOCE );
  X_LAPLACE        = XMLString::transcode( C_LAPLACE );
  X_MONTE          = XMLString::transcode( C_MONTE );
  X_ANALYTIC       = XMLString::transcode( C_ANALYTIC );
  X_GRID           = XMLString::transcode( C_GRID );

  myPopEpsilon = pow( 10.0, -(mySigDigits+1.0) );
  myIndEpsilon = pow( 10.0, -(mySigDigits+1.0) );

  // Clean up reminents from a previous run.
  remove( fMakefile );
  remove( fIndData_h );
  remove( fDataSet_h );
  remove( fPredEqn_fortran );
  remove( fPredEqn_cpp );
  remove( fPred_h );
  remove( fNonmemPars_h );
  remove( fMontePars_h );
  remove( fFitDriver_cpp );
  remove( fMonteDriver_cpp );
  remove( fSpkRuntimeError_tmp );
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
  XMLString::release( &X_PRED );
  XMLString::release( &X_MONTE_CARLO );
  XMLString::release( &X_PRESENTATION );
  XMLString::release( &X_TABLE );
  XMLString::release( &X_SCATTERPLOT );
  XMLString::release( &X_COLUMN );
  XMLString::release( &X_X );
  XMLString::release( &X_Y );
  XMLString::release( &X_BY );
  XMLString::release( &X_APPROXIMATION );
  XMLString::release( &X_FO );
  XMLString::release( &X_FOCE );
  XMLString::release( &X_LAPLACE );
  XMLString::release( &X_ANALYTIC );
  XMLString::release( &X_GRID );
  XMLString::release( &X_MONTE );
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
  XMLString::release( &X_ONLYSIMULATION );
  XMLString::release( &X_SUBPROBLEMS );
}
NonmemTranslator::NonmemTranslator( const NonmemTranslator& )
{
}
NonmemTranslator& NonmemTranslator::operator=( const NonmemTranslator& )
{
}
void NonmemTranslator::parseSource()
{
  if( table->getLabels()->size() <= 1 )
  {
     char mess[ SpkCompilerError::maxMessageLen()];
     sprintf( mess, "Missing \"%s\" tag.", C_LABELS );
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }

  DOMElement * spksouce = source->getDocumentElement();
  DOMNodeList * nonmems = spksouce->getElementsByTagName( X_NONMEM );
  if( nonmems->getLength() != 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing \"%s\" tag.", C_NONMEM );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  DOMElement * nonmem = dynamic_cast<DOMElement*>( nonmems->item(0) );

  //------------------------------------------------------
  // <constraint>
  //------------------------------------------------------
  DOMNodeList * constraints = nonmem->getElementsByTagName( X_CONSTRAINT );
  if( constraints->getLength() != 1 )
  {
     char mess[ SpkCompilerError::maxMessageLen()];
     sprintf( mess, "Missing \"%s\" tag.", C_CONSTRAINT );
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  DOMElement * constraint = dynamic_cast<DOMElement*>( constraints->item(0) );
  if(  !constraint->hasChildNodes() )
  {
     char mess[ SpkCompilerError::maxMessageLen()];
     sprintf( mess, "\"%s\" must have a child, either \"%s\" or \"%s\".", C_CONSTRAINT, C_POP_ANALYSIS, C_IND_ANALYSIS );
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }

  DOMNodeList * pop_analysises = constraint->getElementsByTagName( X_POP_ANALYSIS );
  DOMNodeList * ind_analysises = constraint->getElementsByTagName( X_IND_ANALYSIS );
  DOMElement * analysis;
  bool isAnalysisDone = false;
  if( pop_analysises->getLength() == 1 )
    {
      myTarget = POP;
      analysis = dynamic_cast<DOMElement*>( pop_analysises->item(0) );
      parsePopAnalysis( analysis );
      isAnalysisDone = true;
    }
  else if( ind_analysises->getLength() == 1 )
    {
      myTarget = IND;
      analysis = dynamic_cast<DOMElement*>( ind_analysises->item(0) );
      myPopSize = 1;
      parseIndAnalysis( analysis );
      isAnalysisDone = true;
    }
  else
    {
      // illegal
      char mess[ SpkCompilerError::maxMessageLen()];
      sprintf( mess, "\"%s\" must have a child, either \"%s\" or \"%s\".", C_CONSTRAINT, C_POP_ANALYSIS, C_IND_ANALYSIS );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  //------------------------------------------------------
  // <model>
  // NOTE: only <pred> is allowed under <model> for v0.1.
  //------------------------------------------------------
  DOMNodeList * models = nonmem->getElementsByTagName( X_MODEL );
  if( models->getLength() != 1 )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Missing \"%s\" tag.", C_MODEL );
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }

  DOMElement * model = dynamic_cast<DOMElement*>( models->item(0) );
  DOMNodeList * preds = model->getElementsByTagName( X_PRED );
  if(preds->getLength() != 1 )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Missing \"%s\" tag.", C_PRED );
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  DOMElement * pred = dynamic_cast<DOMElement*>( preds->item(0) );
  myModelSpec = PRED;

  bool isPredDone = false;
  parsePred( pred );
  isPredDone = true;
 
  //------------------------------------------------------
  // <monte_carlo>
  //------------------------------------------------------
  DOMNodeList * monte_carlos = nonmem->getElementsByTagName( X_MONTE_CARLO );
  if( monte_carlos->getLength() > 0 )
  {
    myIsMonte = true;
    DOMElement * monte_carlo = dynamic_cast<DOMElement*>( monte_carlos->item(0) );

      if( monte_carlo->hasAttribute( X_METHOD ) )
      {
	const XMLCh* x_temp = monte_carlo->getAttribute( X_METHOD );
	if( XMLString::equals( x_temp, X_ANALYTIC ) )
	  myMonteMethod = ANALYTIC;
	else if( XMLString::equals( x_temp, X_GRID ) )
	  myMonteMethod = GRID;
	else
	  myMonteMethod = MONTE;
      }
      if( monte_carlo->hasAttribute( X_NUMBEREVAL) )
	{
	  const XMLCh* x_num = monte_carlo->getAttribute( X_NUMBEREVAL );
	  if( ! XMLString::textToBin( x_num, myMonteNumberEval ) )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      sprintf( mess, 
		       "Invalid %s attribute value in \"%s\" tag: %s", C_NUMBEREVAL, C_MONTE_CARLO,
		       XMLString::transcode(x_num) );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	      throw e;
	    }
	}
  }
  //------------------------------------------------------
  //<presentation>
  //------------------------------------------------------ 
  //PRED parsing and <xxx_analysis> parsing must have been completed so
  //that the symbol table contains entries for the user defined
  //variables and THETA/OMEGA/SIGMA/ETA.
  if( !isPredDone )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Before parsing \"%s\", \"%s\" must be parsed.", 
              C_PRESENTATION, C_PRED );
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  if( !isAnalysisDone )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Before parsing \"%s\", \"%s\" must be parsed.", C_PRESENTATION, 
              (myTarget==POP? C_POP_ANALYSIS : C_IND_ANALYSIS ) );
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  
  DOMNodeList * presentations = nonmem->getElementsByTagName( X_PRESENTATION );
  if( presentations->getLength() > 1 )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "One or none \"%s\" is allowed in a sourceML document.", C_PRESENTATION ); 
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }

  DOMElement * presentation = dynamic_cast<DOMElement*>( presentations->item(0) );

  myRecordNums.resize( myPopSize );
  Symbol * id = table->findi( KeyStr.ID );
  if( id == NULL || id == Symbol::empty() )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "The data set is missing \"%s\" item or did you forget to label it?", 
             DefaultStr.ID.c_str() ); 
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  for( int i=0; i<myPopSize; i++ )
    {
      myRecordNums[i] = id->initial[i].size();
    }

  if( myIsSimulate )
    {
      Symbol * p = table->insertUserVar(DefaultStr.SIMDV);
    }

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

  if( (p = table->findi( KeyStr.PRED )) != Symbol::empty() )
     UserStr.PRED = p->name;
  else
  {
     table->insertUserVar( DefaultStr.PRED );
     UserStr.PRED = DefaultStr.PRED;
  }

  if( (p = table->findi( KeyStr.RES )) != Symbol::empty() )
     UserStr.RES = p->name;
  else
  {
     table->insertUserVar( DefaultStr.RES );
     UserStr.RES = DefaultStr.RES;
  }

  if( (p = table->findi( KeyStr.WRES )) != Symbol::empty() )
     UserStr.WRES = p->name;
  else
  {
     table->insertUserVar( DefaultStr.WRES );
     UserStr.WRES = DefaultStr.WRES;
  }

  if( (p = table->findi( KeyStr.MDV )) != Symbol::empty() )
     UserStr.MDV = p->name;
  else
  {
     Symbol * s = table->insertLabel( DefaultStr.MDV, "", myRecordNums );
     for( int i=0; i<myPopSize; i++ )
	s->initial[i] = "0";
     UserStr.MDV = DefaultStr.MDV;
  }

  if( (p = table->findi( KeyStr.DV )) != Symbol::empty() )
     UserStr.DV = p->name;
  else
     UserStr.DV = DefaultStr.DV;
  
  if( (p = table->findi( KeyStr.SIMDV )) != Symbol::empty() )
     UserStr.SIMDV = p->name;
  else
     UserStr.SIMDV = DefaultStr.SIMDV;
  
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
  generatePred( fPredEqn_cpp );
  generateNonmemParsNamespace();
  generateMonteParsNamespace();
  if( myTarget == POP )
    generatePopDriver();
  else
    generateIndDriver();
  generateMakefile();
}

//=============================================================================
//
// SPK Optimization request and Monte Carlo integration request are
// mutually exclusive.
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
  oMakefile << (myIsMonte? " -lgsl" : "" ) << " -latlas_lapack -lcblas -latlas -lpthread -lm" << endl;
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
      oMakefile << "-L/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;
      
      oMakefile << "test : fitDriver.cpp $(COMMON_INCLUDE)" << endl;
      oMakefile << "\tg++ $(CPP_FLAGS) fitDriver.cpp -o driver ";
      oMakefile << "-L/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR) ";
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
      oMakefile << "MONTE_DIR = /usr/local/src/spktest/ml" << endl;
      oMakefile << endl;

      oMakefile << "MONTE_SRC = \\" << endl;
      oMakefile << "\tmonteDriver.cpp \\" << endl; 
      oMakefile << "\tAnalyticIntegral.cpp \\" << endl;
      oMakefile << "\tGridIntegral.cpp \\" << endl;
      oMakefile << "\tMontePopObj.cpp \\" << endl;
      oMakefile << "\tMapBay.cpp \\" << endl;
      oMakefile << "\tMapMonte.cpp" << endl;
      oMakefile << endl;

      oMakefile << "MONTE_INCLUDE = \\" << endl;
      oMakefile << "\tAnalyticIntegral.h \\" << endl;
      oMakefile << "\tGridIntegral.h \\" << endl;
      oMakefile << "\tMontePopObj.h \\" << endl;
      oMakefile << "\tMapBay.h \\" << endl;
      oMakefile << "\tMapMonte.h \\" << endl;
      oMakefile << endl;

      oMakefile << "prod : driver $(MONTE_SRC) $(MONTE_INCLUDE) $(COMMON_INCLUDE)" << endl;
      oMakefile << "\tg++ $(CPP_FLAGS) $(MONTE_SRC) -o monteDriver ";
      oMakefile << "-L/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "-I/usr/local/include/$(PROD_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(PROD_DIR) ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;
      
      oMakefile << "test : driver $(MONTE_SRC) $(MONTE_INCLUDE) $(COMMON_INCLUDE)" << endl;
      oMakefile << "\tg++ $(CPP_FLAGS) $(MONTE_SRC) -o monteDriver ";
      oMakefile << "-L/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "-I/usr/local/include/$(TEST_DIR) ";
      oMakefile << "-Wl,--rpath -Wl,/usr/local/lib/$(TEST_DIR) ";
      oMakefile << "$(LIBS)" << endl;
      oMakefile << endl;
  
      oMakefile << "driver : " << endl;
      oMakefile << "\tcp $(MONTE_DIR)/$@ ." << endl;
      oMakefile << endl;

      oMakefile << "monteDriver.cpp : " << endl;
      oMakefile << "\tcp $(MONTE_DIR)/$@ ." << endl;
      oMakefile << endl;

      oMakefile << "AnalyticIntegral.cpp :" << endl;
      oMakefile << "\tcp $(MONTE_DIR)/$@ ." << endl;
      oMakefile << "AnalyticIntegral.h :" << endl;
      oMakefile << "\tcp $(MONTE_DIR)/$@ ." << endl;
      oMakefile << endl;

      oMakefile << "GridIntegral.cpp :" << endl;
      oMakefile << "\tcp $(MONTE_DIR)/$@ ." << endl;
      oMakefile << "GridIntegral.h :" << endl;
      oMakefile << "\tcp $(MONTE_DIR)/$@ ." << endl;
      oMakefile << endl;

      oMakefile << "MontePopObj.cpp :" << endl;
      oMakefile << "\tcp $(MONTE_DIR)/$@ ." << endl;
      oMakefile << "MontePopObj.h : " << endl;
      oMakefile << "\tcp $(MONTE_DIR)/$@ ." << endl;
      oMakefile << endl;

      oMakefile << "MapBay.cpp :" << endl;
      oMakefile << "\tcp $(MONTE_DIR)/$@ ." << endl;
      oMakefile << "MapBay.h : " << endl;
      oMakefile << "\tcp $(MONTE_DIR)/$@ ." << endl;
      oMakefile << endl;

      oMakefile << "MapMonte.cpp : " << endl;
      oMakefile << "\tcp $(MONTE_DIR)/$@ ." << endl;
      oMakefile << "MapMonte.h : " << endl;
      oMakefile << "\tcp $(MONTE_DIR)/$@ ." << endl;
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
      oMakefile << "*.o" << endl;
    }
  oMakefile.close();

  return;
}

void NonmemTranslator::parsePopAnalysis( DOMElement* pop_analysis )
{
  
  //================================================================================
  // <pop_analysis>: Attributes required when "is_estimation=yes".
  //================================================================================
  // * approximation = {fo, foce, laplace}
  // * pop_size
  // * is_estimation = {yes, no}
  //
  // Finding out if parameter estimation is requested.
  //
  if( !pop_analysis->hasAttribute( X_IS_ESTIMATION ) )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Missing \"%s\" attribute in \"%s\" tag.", C_POP_ANALYSIS, C_IS_ESTIMATION );
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
	  sprintf( mess, "%s attribute is missing in \"%s\" tag.", C_APPROXIMATION, C_POP_ANALYSIS );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	  throw e;
	}
      const XMLCh * xml_approx = pop_analysis->getAttribute( X_APPROXIMATION );
      
      if( XMLString::equals( xml_approx, X_FO ) )
	myApproximation = FO;
      else if( XMLString::equals( xml_approx, X_FOCE ) )
	myApproximation = FOCE;
      else if( XMLString::equals( xml_approx, X_LAPLACE ) )
	myApproximation = LAPLACE;  
      else
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Invalid \"%s\" attribute value: %s.", C_APPROXIMATION, XMLString::transcode(xml_approx) );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	  throw e;
	}
      //
      // Finding out the population size
      //
      if( !pop_analysis->hasAttribute( X_POP_SIZE ) )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Missing \"%s\" attribute in \"%s\" tag.", C_POP_ANALYSIS, C_POP_SIZE );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
	  throw e;
	}
      const XMLCh * xml_pop_size = pop_analysis->getAttribute( X_POP_SIZE );
      if( !XMLString::textToBin( xml_pop_size, myPopSize ) )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, 
		   "Invalid %s attribute value in \"%s\" tag: %s", C_POP_SIZE, C_POP_ANALYSIS,
		   XMLString::transcode(xml_pop_size) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
      throw e;
	}
    }

  myIndTraceLevel = 0;
  myPopTraceLevel = 1;

  //================================================================================
  // Optional attributes
  //================================================================================
  // * is_eta_out = {yes, "no"}
  // * is_restart = {"yes", no}
  myIsEtaOut = false;
  const XMLCh * xml_is_eta_out;
  if( pop_analysis->hasAttribute( X_IS_ETA_OUT ) )
    {
      xml_is_eta_out = pop_analysis->getAttribute( X_IS_ETA_OUT );
      myIsEtaOut = ( XMLString::equals( xml_is_eta_out, X_YES )? true : false );
    }

  if( myIsEstimate )
    {
      myIsRestart = true;
      const XMLCh * xml_is_restart;
      if( pop_analysis->hasAttribute( X_IS_RESTART ) )
	{
	  xml_is_restart = pop_analysis->getAttribute( X_IS_RESTART );
	  myIsRestart = ( XMLString::equals( xml_is_restart, X_YES )? true : false );
	}
      const XMLCh* xml_sig_digits;
      if( pop_analysis->hasAttribute( X_SIG_DIGITS ) )
	{
	  xml_sig_digits = pop_analysis->getAttribute( X_SIG_DIGITS );
	  if( XMLString::stringLen( xml_sig_digits ) > 0 )
	    {
	      if( !XMLString::textToBin( xml_sig_digits, mySigDigits ) )
              {
                 char mess[ SpkCompilerError::maxMessageLen() ];
                 sprintf( mess, "Invalid %s attribute value in \"%s\" tag?: \"%s\"", 
                          C_POP_ANALYSIS, C_SIG_DIGITS, XMLString::transcode( xml_sig_digits ) );
                 SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
                 throw e;
              
              }
	      if( !( mySigDigits > 0 && mySigDigits < 9 ) )
              {
                 char mess[ SpkCompilerError::maxMessageLen() ];
                 sprintf( mess, "Invalid %s attribute value, \"%s\", in \"%s\" tag.  Valid (1-8)", 
                          C_SIG_DIGITS, XMLString::transcode( xml_sig_digits ), C_POP_ANALYSIS );
                 SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__);
                 throw e;
              }
	      myIndEpsilon = pow( 10.0, -(mySigDigits + 1.0) );
	      myPopEpsilon = myIndEpsilon;
	    }
	}
    }

  //================================================================================
  // Required elements
  //================================================================================
  // <data_labels>
  // <theta>
  // <omega>+
  // <sigma>+
  DOMNodeList * data_labels_list = pop_analysis->getElementsByTagName( X_DATA_LABELS );
  if( data_labels_list->getLength() != 1 )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "There must be one and only one \"%s\" tag in the sourceML document.", 
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
        sprintf( mess, "There must be at least one \"%s\" tag.",  C_LABEL );
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
            sprintf( mess, "%s attribute is required in \"%s\" tag: missing in %d-th %s", C_NAME, C_LABEL,
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
            sprintf( mess, "%s is not registered in the symbol table.", c_name );
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
  if( theta_list->getLength() != 1 )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "There may be one and only one \"%s\" tag in the sourceML document.", C_THETA );
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  DOMElement * theta = dynamic_cast<DOMElement*>( theta_list->item(0) );
  if( !theta->hasAttribute( X_LENGTH ) )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Missing %s attribute in \"%s\" tag in the sourceML document.", C_LENGTH, C_THETA );
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  const XMLCh* xml_theta_len = theta->getAttribute( X_LENGTH );
  myThetaLen = 0;
  if( !XMLString::textToBin( xml_theta_len, myThetaLen ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess,
	       "Invalid %s attribute value for \"%s\" tag? : %s\n", 
	       XMLString::transcode( xml_theta_len ), C_THETA );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  Symbol * sym_theta = table->insertNMVector( DefaultStr.THETA, myThetaLen );
  {
    //<in>
    DOMNodeList * theta_in_list = theta->getElementsByTagName( X_IN );
    if( theta_in_list->getLength() != 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess,
	       "There may be one and only \"%s\" sug-tag for \"%s\" tag in the sourceML document.", 
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
	       "The number of \"%s\" subtags does not match with the %s attribute specification for \"%s\" tag.", 
               C_VALUE, C_LENGTH, C_THETA );
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
    if( theta_low_list->getLength() != 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess,
	       "There may be one and only \"%s\" sug-tag for \"%s\" tag in the sourceML document.", 
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
	       "The number of \"%s\" subtags does not match with the %s attribute specification for \"%s\" tag.", 
               C_VALUE, C_LENGTH, C_THETA );
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
	sym_theta->lower[0][i] = str_val;
      }

    //<up>
    DOMNodeList * theta_up_list = theta->getElementsByTagName( X_UP );
    if( theta_up_list->getLength() != 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess,
	       "There may be one and only \"%s\" sug-tag for \"%s\" tag in the sourceML document.", 
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
	       "The number of \"%s\" subtags does not match with the %s attribute specification for \"%s\" tag.", 
               C_VALUE, C_LENGTH, C_THETA );
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
	sym_theta->upper[0][i] = str_val;
      }

    // step values
    for( int i=0; i<myThetaLen; i++ )
    {
      double tmp_dbl = fabs( atof( sym_theta->upper[0][i].c_str() ) 
			     - atof( sym_theta->lower[0][i].c_str() ) ) / 1000.0;
      char tmp_char[256];
      sprintf( tmp_char, "%f", tmp_dbl );
      sym_theta->step[0][i] = string( tmp_char );
    }
  }

  DOMNodeList * omega_list = pop_analysis->getElementsByTagName( X_OMEGA );
  int nOmegaSpecs = omega_list->getLength();
  if( nOmegaSpecs != 1 )
  {
     // v0.1 supports only one (full) Omega specification
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "There is one and only one \"%s\" tag in the sourceML document.",
              C_OMEGA );
     SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  DOMElement * omega = dynamic_cast<DOMElement*>( omega_list->item(0) );
  if( !omega->hasAttribute( X_DIMENSION ) )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Missing %s attribute in \"%s\" tag.",
              C_DIMENSION, C_OMEGA );
     SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  const XMLCh* xml_omega_dim = omega->getAttribute( X_DIMENSION );
  if( !XMLString::textToBin( xml_omega_dim, myOmegaDim ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, 
	       "Invalid %s attribute value, %s, in \"%s\" tag.", 
	       C_DIMENSION, XMLString::transcode( xml_omega_dim ), C_OMEGA );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  if( !omega->hasAttribute( X_STRUCT ) )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Missing %s attribute in \"%s\" tag.",
              C_STRUCT, C_OMEGA );
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
	       "Invalid %s attribute value, %s, in \"%s\" tag.", 
	       C_STRUCT, XMLString::transcode( xml_omega_struct ), C_OMEGA );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  Symbol * sym_omega = table->insertNMMatrix( DefaultStr.OMEGA, myOmegaStruct, myOmegaDim );
  {
    //<in>
    DOMNodeList * omega_in_list = omega->getElementsByTagName( X_IN );
    if( omega_in_list->getLength() != 1 )
    {
       char mess[ SpkCompilerError::maxMessageLen() ];
       sprintf( mess, "There is one and only one \"%s\" subtag in \"%s\" tag in the sourceML document.",
                C_IN, C_OMEGA );
       SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
       throw e;
    }
    DOMElement * omega_in = dynamic_cast<DOMElement*>( omega_in_list->item(0) );

    DOMNodeList * value_list = omega_in->getElementsByTagName( X_VALUE );
    if( myOmegaOrder != value_list->getLength() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess,
	       "The number of \"%s\" subtags does not match with the %s attribute specification for \"%s\" tag.", 
               C_VALUE, C_LENGTH, C_OMEGA );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
    for( int i=0; i<myOmegaOrder; i++ )
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

  DOMNodeList * sigma_list = pop_analysis->getElementsByTagName( X_SIGMA );
  int nSigmaSpecs = sigma_list->getLength();
  if( nSigmaSpecs != 1 )
  { 
     // v0.1 supports only one (full) Sigma specification
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "There is one and only one \"%s\" tag in the sourceML document.",
              C_SIGMA );
     SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  DOMElement * sigma = dynamic_cast<DOMElement*>( sigma_list->item(0) );
  if( !sigma->hasAttribute( X_DIMENSION ) )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Missing %s attribute in \"%s\" tag.",
              C_DIMENSION, C_SIGMA );
     SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  const XMLCh* xml_sigma_dim = sigma->getAttribute( X_DIMENSION );
  if( !XMLString::textToBin( xml_sigma_dim, mySigmaDim ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, 
	       "Invalid %s attribute value, %s, in \"%s\" tag.", 
	       C_DIMENSION, XMLString::transcode( xml_sigma_dim ), C_OMEGA );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  if( !sigma->hasAttribute( X_STRUCT ) )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Missing %s attribute in \"%s\" tag.",
              C_STRUCT, C_SIGMA );
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
	       "Invalid %s attribute value, %s, in \"%s\" tag.", 
	       C_STRUCT, XMLString::transcode( xml_sigma_struct ), C_SIGMA );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  Symbol * sym_sigma = table->insertNMMatrix( DefaultStr.SIGMA, mySigmaStruct, mySigmaDim ); 
  {
    //<in>
    DOMNodeList * sigma_in_list = sigma->getElementsByTagName( X_IN );
    if( sigma_in_list->getLength() != 1 )
    {
       char mess[ SpkCompilerError::maxMessageLen() ];
       sprintf( mess, "There is one and only one \"%s\" subtag in \"%s\" tag in the sourceML document.",
                C_IN, C_SIGMA );
       SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
       throw e;
    }
    DOMElement * sigma_in = dynamic_cast<DOMElement*>( sigma_in_list->item(0) );

    DOMNodeList * value_list = sigma_in->getElementsByTagName( X_VALUE );
    if( mySigmaOrder != value_list->getLength() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess,
	       "The number of \"%s\" subtags does not match with the %s attribute specification for \"%s\" tag.", 
               C_VALUE, C_LENGTH, C_SIGMA );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
    for( int i=0; i<mySigmaOrder; i++ )
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
  
  //----------------------------------------------------------
  // eta
  // NOTE: eta is not given by the user.  
  // eta's initial estimate is set to 0.0 automatically.
  //
  //-----------------------------------------------------------
  myEtaLen = myOmegaDim;
  char etaDefault[] = "0.0";
  Symbol * sym_eta = table->insertNMVector( DefaultStr.ETA, myEtaLen );
  for( int i=0; i<myEtaLen; i++ ) sym_eta->initial[0][i] = etaDefault;
  sym_eta->fixed[0] = false;

  //----------------------------------------------------------
  // Sigma 
  // Sigma is the covariance of EPS: thus, 
  // the order of Sigma is the length of EPS vector.
  myEpsLen = mySigmaDim;
  char epsDefault[] = "0.0";
  Symbol * sym_eps = table->insertNMVector( DefaultStr.EPS, myEpsLen );
  for( int i=0; i<myEpsLen; i++ ) sym_eps->initial[0][i] = epsDefault;
  sym_eta->fixed[0] = false;

  //================================================================================
  // (Optional) Statistics elements
  //================================================================================
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
         sprintf( mess, "There may not be more than one \"%s\" tag in the sourceML document.  You got %d.",
                  C_SIMULATION, simulations->getLength() );
         SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
         throw e;
      }
      myIsSimulate = true;
      DOMElement* simulation = dynamic_cast<DOMElement*>( simulations->item(0) );
      if( !simulation->hasAttribute( X_SEED ) )
      {
         char mess[ SpkCompilerError::maxMessageLen() ];
         sprintf( mess, "Missing %s attribute in \"%s\" tag.",
                  C_SEED, C_SIMULATION );
         SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
         throw e;
      }
      const XMLCh* xml_seed = simulation->getAttribute( X_SEED );
      if( !XMLString::textToBin( xml_seed, mySeed ) )
	{
           char mess[ SpkCompilerError::maxMessageLen() ];
	   sprintf( mess, "Invalid %s attribute value?  You gave me \"%s\".", 
	            C_SEED, XMLString::transcode(xml_seed) );
           SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
           throw e;
	}
      if( simulation->hasAttribute( X_ONLYSIMULATION ) )
      {
         const XMLCh* xml_only_simulation = simulation->getAttribute( X_ONLYSIMULATION );
         if( XMLString::equals( xml_only_simulation, X_YES ) )
	   {
	     myIsOnlySimulation = true;
	   }
         else
           myIsOnlySimulation = false;
      }
      if( simulation->hasAttribute( X_SUBPROBLEMS ) )
      {
         const XMLCh* xml_subproblems = simulation->getAttribute( X_SUBPROBLEMS );
         if( !XMLString::textToBin( xml_subproblems, mySubproblemsN ) )
         {
           char mess[ SpkCompilerError::maxMessageLen() ];
	   sprintf( mess, "Invalid %s attribute value?  You gave me \"%s\".", 
	            C_SUBPROBLEMS, XMLString::transcode(xml_subproblems) );
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
	  sprintf( mess, "There may be at most one \"%s\" tag in the sourceML document.", 
	           C_POP_STAT );
          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
          throw e;
      }
      DOMElement * pop_stat = dynamic_cast<DOMElement*>( pop_stat_list->item(0) );
      if( !pop_stat->hasAttribute( X_COVARIANCE_FORM ) && myIsStat )
      {
          char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Missing %s attribute in \"%s\" tag.", 
	           C_COVARIANCE_FORM, C_POP_STAT );
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
	   sprintf( mess, "Invalid %s attribute value?  You gave me \"%s\".", 
	            C_COVARIANCE_FORM, XMLString::transcode( cov_form)  );
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
  if( myIsOnlySimulation && myIsEstimate )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "%s::%s and %s::%s are exclusive to each other.", 
	       C_SIMULATION, C_ONLYSIMULATION, C_POP_ANALYSIS, C_IS_ESTIMATION, C_ONLYSIMULATION  );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
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
  //================================================================================
  // Parse <simulate> if exists.  There's a chance in which only data simulation
  // is requested but not estimation.
  //================================================================================
  myIsSimulate = false;
  mySeed = 0;
  DOMNodeList * simulations = ind_analysis->getElementsByTagName( X_SIMULATION );
  if( simulations->getLength() > 0 )
    {
      if( simulations->getLength() != 1 )
      {
         char mess[ SpkCompilerError::maxMessageLen() ];
         sprintf( mess, "At most one \"%s\" tag may appear in the sourceML document.",
                  C_SIMULATION );
         SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
         throw e;
      }
      myIsSimulate = true;
      DOMElement* simulation = dynamic_cast<DOMElement*>( simulations->item(0) );
      if( !simulation->hasAttribute( X_SEED ) )
      {
         char mess[ SpkCompilerError::maxMessageLen() ];
         sprintf( mess, "Missing %s attribute in \"%s\" tag.",
                  C_SEED, C_SIMULATION );
         SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
         throw e;
      }
      const XMLCh* xml_seed = simulation->getAttribute( X_SEED );
      if( !XMLString::textToBin( xml_seed, mySeed ) )
	{
          char mess[ SpkCompilerError::maxMessageLen() ];
	  sprintf( mess, "Invalid %s attribute value?  You gave me \"%s\".", 
		   C_SEED, XMLString::transcode( xml_seed ) );
          SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
          throw e;
	}
   
      if( simulation->hasAttribute( X_ONLYSIMULATION ) )
      {
         const XMLCh* xml_only_simulation = simulation->getAttribute( X_ONLYSIMULATION );
         if( XMLString::equals( xml_only_simulation, X_YES ) )
	   {
	     myIsOnlySimulation = true;
	   }
         else
           myIsOnlySimulation = false;
      }
      if( simulation->hasAttribute( X_SUBPROBLEMS ) )
      {
         const XMLCh* xml_subproblems = simulation->getAttribute( X_SUBPROBLEMS );
         if( !XMLString::textToBin( xml_subproblems, mySubproblemsN ) )
         {
           char mess[ SpkCompilerError::maxMessageLen() ];
	   sprintf( mess, "Invalid %s attribute value?  You gave me \"%s\".", 
	            C_SUBPROBLEMS, XMLString::transcode(xml_subproblems) );
           SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
           throw e;
         }
      }
    }

  //================================================================================
  // <pop_analysis> Required attributes
  //================================================================================
  // * is_estimation = {yes, no}
  if( !ind_analysis->hasAttribute( X_IS_ESTIMATION ) )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Missing %s attribute in \"%s\" tag.", C_IS_ESTIMATION, C_IND_ANALYSIS );
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
     fprintf( stderr, "Warning: Invalid %s attribute value, %s, in \"%s\" tag.  Applied \"%s\".",
              C_IS_ESTIMATION, xml_is_estimation, C_IND_ANALYSIS, C_YES );
     myIsEstimate = true;
  }


  myIndTraceLevel = 1;
  myPopTraceLevel = 1;

  //================================================================================
  // Optional attributes
  //================================================================================
  // * mitr   --- required when is_estimation == "yes"
  // * is_restart = {"yes", no}
  // * sig_digits = 3
  myIsRestart = true;
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
	    sprintf( mess, "Invalid %s attribute value?  You gave me \"%s\".", 
	             C_MITR, XMLString::transcode(xml_mitr) );
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
	    sprintf( mess, "Invalid %s attribute value?  You gave me \"%s\".", 
	             C_SIG_DIGITS, XMLString::transcode(xml_sig_digits) );
            SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
            throw e;
          }
	  if( !( mySigDigits > 0 && mySigDigits < 9 ) )
          {
            char mess[ SpkCompilerError::maxMessageLen() ];
	    sprintf( mess, "Invalid %s attribute value?  Valid values (1-8).  You gave me \"%d\".", 
	             C_SIG_DIGITS, mySigDigits );
            SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
            throw e;
          }
	  myIndEpsilon = pow( 10.0, -(mySigDigits + 1.0) );
	}
    }
  
  //================================================================================
  // Required elements
  //================================================================================
  // <data_labels>
  // <theta>
  // <omega>+
  DOMNodeList * data_labels_list = ind_analysis->getElementsByTagName( X_DATA_LABELS );
  if( data_labels_list->getLength() != 1 )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "There may be one and only one \"%s\" tag under \"%s\" tag.",
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
        sprintf( mess, "There must be at least one \"%s\" tag under \"%s\" tag.",
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
            sprintf( mess, "Missing %s attribute in \"%s\" tag.",
                     C_NAME, C_LABEL );
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
                sprintf( mess, "%s is not found in the symbol table as either the name or the alias.", c_name );
                SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, 
                                        __LINE__, __FILE__ );
                throw e;
             }
	     Symbol * synonym = table->findi( c_synonym );
	     if( synonym == Symbol::empty() )
             {
                char mess[ SpkCompilerError::maxMessageLen() ];
                sprintf( mess, "%s is not found in the symbol table as either the name or the alias.", c_name );
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
  if( theta_list->getLength() != 1 )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "There may be one and only one \"%s\" tag under \"%s\" tag.",
              C_THETA, C_IND_ANALYSIS );
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  DOMElement * theta = dynamic_cast<DOMElement*>( theta_list->item(0) );
  if( !theta->hasAttribute( X_LENGTH ) )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Missing %s attribute in \"%s\" tag.",
              C_LENGTH, C_THETA );
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }

  const XMLCh* xml_theta_len = theta->getAttribute( X_LENGTH );
  myThetaLen = 0;
  if( !XMLString::textToBin( xml_theta_len, myThetaLen ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Invalid %s attribute value, %s, in \"%s\" tag?", 
	       C_LENGTH, XMLString::transcode( xml_theta_len ), C_THETA );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  Symbol * sym_theta = table->insertNMVector( DefaultStr.THETA, myThetaLen );
  {
    //<in>
    DOMNodeList * theta_in_list = theta->getElementsByTagName( X_IN );
    if( theta_in_list->getLength() != 1 )
    {
       char mess[ SpkCompilerError::maxMessageLen() ];
       sprintf( mess, "There may be one and only one \"%s\" tag under \"%s\" tag.",
                C_IN, C_THETA );
       SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
       throw e;
    }
    DOMElement * theta_in = dynamic_cast<DOMElement*>( theta_in_list->item(0) );

    DOMNodeList * value_list = theta_in->getElementsByTagName( X_VALUE );
    if( myThetaLen != value_list->getLength() )
    {
       char mess[ SpkCompilerError::maxMessageLen() ];
       sprintf( mess, "The number of \"%s\"s under \"%s\" tag does not match with the \"%s\"'s %s attribute specification.",
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
    if( theta_low_list->getLength() != 1 )
    {
       char mess[ SpkCompilerError::maxMessageLen() ];
       sprintf( mess, "There may be one and only one \"%s\" tag under \"%s\" tag.",
                C_LOW, C_THETA );
       SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
       throw e;
    }
    DOMElement * theta_low = dynamic_cast<DOMElement*>( theta_low_list->item(0) );
    value_list = theta_low->getElementsByTagName( X_VALUE );
    if( myThetaLen != value_list->getLength() )
    {
       char mess[ SpkCompilerError::maxMessageLen() ];
       sprintf( mess, "The number of \"%s\"s under \"%s\" tag does not match with the \"%s\"'s %s attribute specification.",
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
    if( theta_up_list->getLength() != 1 )
    {
       char mess[ SpkCompilerError::maxMessageLen() ];
       sprintf( mess, "There may be one and only one \"%s\" tag under \"%s\" tag.",
                C_UP, C_THETA );
       SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
       throw e;
    }
    DOMElement * theta_up = dynamic_cast<DOMElement*>( theta_up_list->item(0) );
    value_list = theta_up->getElementsByTagName( X_VALUE );
    if( myThetaLen != value_list->getLength() )
    {
       char mess[ SpkCompilerError::maxMessageLen() ];
       sprintf( mess, "The number of \"%s\"s under \"%s\" tag does not match with the \"%s\"'s %s attribute specification.",
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
  if( nOmegaSpecs != 1 )
  {
     // v0.1 supports only one Omega specification
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "There may be one and only one \"%s\" tag under \"%s\" tag.",
              C_OMEGA, C_IND_ANALYSIS );
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  DOMElement * omega = dynamic_cast<DOMElement*>( omega_list->item(0) );
  if( !omega->hasAttribute( X_DIMENSION ) )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Missing %s attribute in \"%s\" tag.",
              C_DIMENSION, C_OMEGA );
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  const XMLCh* xml_omega_dim = omega->getAttribute( X_DIMENSION );
  if( !XMLString::textToBin( xml_omega_dim, myOmegaDim ) )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Invalid %s attribute value?  You gave me %s.", 
	       C_DIMENSION, XMLString::transcode(xml_omega_dim) );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  if( !omega->hasAttribute( X_STRUCT ) )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Missing %s attribute in \"%s\" tag.",
              C_STRUCT, C_OMEGA );
     SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  // In Individual analysis, Omega is diagonal only.
  const XMLCh* xml_omega_struct = omega->getAttribute( X_STRUCT );
  if( !XMLString::equals( xml_omega_struct, X_DIAGONAL ) )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "For Individual analysis, Omega can be only diagonal.  %s is invalid.",
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
    if( omega_in_list->getLength() != 1 )
    {
       char mess[ SpkCompilerError::maxMessageLen() ];
       sprintf( mess, "There may be one and only one \"%s\" tag under \"%s\" tag.",
                C_IN, C_OMEGA );
       SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
       throw e;
    }
    DOMElement * omega_in = dynamic_cast<DOMElement*>( omega_in_list->item(0) );

    DOMNodeList * value_list = omega_in->getElementsByTagName( X_VALUE );
    if( myOmegaOrder != value_list->getLength() )
    {
       char mess[ SpkCompilerError::maxMessageLen() ];
       sprintf( mess, "The number of \"%s\"s under \"%s\" does not match with the %s attribute value specified in \"%s\".",
                C_VALUE, C_IN, C_LENGTH, C_OMEGA );
       SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
       throw e;
    }
    for( int i=0; i<myOmegaOrder; i++ )
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
	sym_omega->initial[0][i] = str_val;
	sym_omega->fixed[0][i]   = isFixed;
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
  
  //================================================================================
  // Optional elements
  //================================================================================
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
      if( ind_stat_list->getLength() != 1 )
      {
         char mess[ SpkCompilerError::maxMessageLen() ];
         sprintf( mess, "There may be one and only one \"%s\" tag in the sourceML document.",
                  C_IND_STAT );
         SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
         throw e;
      }
      DOMElement * ind_stat = dynamic_cast<DOMElement*>( ind_stat_list->item(0) );
      const XMLCh* xml_stderr = ind_stat->getAttribute( X_IS_ERR_OUT );
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


  if( myIsOnlySimulation && myIsEstimate )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "%s::%s and %s::%s are exclusive to each other.", 
	       C_SIMULATION, C_ONLYSIMULATION, C_POP_ANALYSIS, C_IS_ESTIMATION, C_ONLYSIMULATION  );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
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

  // If this detects any syntax error, throws an exception.
  nm_parse();

  fclose( nm_in );
  fclose( gSpkExpOutput );
  remove( fPredEqn_fortran );
}
//=========================================================================================
// Generate the declaration and the definition files for each
// IndData class and IndDataSet class.
//=========================================================================================
void NonmemTranslator::generateIndData( ) const
{
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
  // The order in which the label strings appear is crutial.
  // So, get a constant pointer to the list and the iterator
  // for throughout use.
  //
  const vector<string> * labels = table->getLabels();
#ifndef NDEBUG
  int cnt=0;
  for( vector<string>::const_iterator itr=labels->begin(); itr != labels->end(); itr++ )
    {
      if( *itr == pID->name )
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
  vector<string>::const_iterator pLabel;

  // 
  // rawTable points to the actual std::map object that
  // maps the label strings and its associated data values.
  //
  const map<const string, Symbol> * const rawTable = table->getTable();
  map<const string, Symbol>::const_iterator pRawTable;

  //
  // Declare and define IndData template class.
  // For name binding reason, the declaration and the definition
  // are both stored in a single file: IndData.h
  //
  // Variable names strictly preserve the names defined/typed by
  // the user.
  //
  ofstream oIndData_h( fIndData_h );
  if( !oIndData_h.good() )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Failed to create %s file.", fIndData_h );
     SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  
  oIndData_h << "/* " << myDescription << "*/" << endl;

  oIndData_h << "#ifndef INDDATA_H" << endl;
  oIndData_h << "#define INDDATA_H" << endl;
  oIndData_h << "#include <vector>" << endl;
  oIndData_h << "#include <map>" << endl;
  oIndData_h << "#include <spk/SpkValarray.h>" << endl;
  oIndData_h << "#include <spk/cholesky.h>" << endl;
  oIndData_h << "#include <spk/multiply.h>" << endl;
  oIndData_h << "#include <CppAD/CppAD.h>" << endl;
  oIndData_h << endl;
  
  //-----------------------------------------------
  // Declaration
  //-----------------------------------------------
  oIndData_h << "template <class ValueType>" << endl;
  oIndData_h << "class IndData{" << endl;
  
  //
  // Public member declaration.
  //
  //
  // A constructor that takes the number of measurements
  // for this particular set and the data item values
  // (from the data file) are given as arguments.
  // 
  // IndData( int nIn,
  //          const vector<char*> IDIn,
  //          const vector<double> d1In,  // data item 1
  //          const vector<double> d2In,  // data item 2
  //          ...,
  //        )
  // : n(nIn), d1(d1In), d1_alias(d1In), d2(d2In), d2_alias(d2In)...
  // {...}
  // 
  oIndData_h << "public:" << endl;
  
  //
  // Constructor declaration.
  // The constructor takes a list of valarray objects as arguments.
  // The arguments are for the variables whose names
  // are defined as *the data labels* in the NONMEM term.
  //
  oIndData_h << "IndData( int nIn";
  pLabel = labels->begin();
  for( ; pLabel != labels->end(); pLabel++ )
    {
      bool isID = ( *pLabel == pID->name );
      oIndData_h << "," << endl;
	  
      //
      // If the label is of "ID", then, the data type is char*.
      // Otherwise, all others have double precision.
      //
      oIndData_h << '\t' << "const std::vector<" << (isID? "char*":"ValueType") << ">";
      oIndData_h << " & " << *pLabel << "In";
    }
  oIndData_h << ");" << endl;
  oIndData_h << endl;

  // 
  // Declare the data item labels (from the data file) and their
  // corresponding synonyms if they have.  They are all have double
  // precision except for the ID data item which has char* type.
  //
  for( pRawTable=rawTable->begin(); pRawTable != rawTable->end(); pRawTable++ )
    {
      const string varName         = pRawTable->second.name;
      const string varAlias        = pRawTable->second.synonym;
      const string keyVarName      = SymbolTable::key( varName );
      const string keyVarAlias     = SymbolTable::key( varAlias );
      enum Symbol::SymbolType type = pRawTable->second.symbol_type;

      // The data labels are constant.
      if( type == Symbol::DATALABEL )
	{
	  bool isID = ( varName==pID->name? true : false );
          oIndData_h << "const std::vector<" << (isID? "char *" : "ValueType") << ">";
	  oIndData_h << " " << varName << ";" << endl;
	  if( varAlias != "" )
            {
	      isID = ( varAlias == pID->name? true : false );
	      oIndData_h << "const std::vector<" << (isID? "char" : "ValueType") << ">";
	      oIndData_h << " " << varAlias << ";" << endl;
            }
	}
      // The NONMEM pred variables are writable.
      else if( type == Symbol::NONMEMDEF )
	{
	  if( keyVarName == KeyStr.THETA 
	      || keyVarName == KeyStr.ETA 
	      || keyVarName == KeyStr.EPS )
	    oIndData_h << "std::vector< std::vector<ValueType> > " << varName << ";" << endl;
	  if( keyVarName == KeyStr.OMEGA 
              || keyVarName == KeyStr.SIGMA )
	    {}

	}
      else // All others, ie. the user (pred) defined variables, are writable.
	{
	  oIndData_h << "std::vector<ValueType> " << varName << ";" << endl;
	}
    }

  //
  // Member functions (public).
  //
  oIndData_h << endl;
  oIndData_h << "~IndData();" << endl;
  oIndData_h << "const SPK_VA::valarray<double> getMeasurements() const;" << endl;
  oIndData_h << "void compResiduals();" << endl;
  oIndData_h << "void compWeightedResiduals( const SPK_VA::valarray<double>& R );" << endl;
  oIndData_h << endl;

  // 
  // Protected member declarations.
  //
  // const int n: #of measurements in this set (ie. individual).
  //
  oIndData_h << "protected:" << endl;
  oIndData_h << "IndData();" << endl;
  oIndData_h << "IndData( const IndData& );" << endl;
  oIndData_h << "IndData& operator=( const IndData& );" << endl;
  oIndData_h << endl;
  oIndData_h << "int nY; // #of measurements (DVs where MDV=0)." << endl;
  oIndData_h << "SPK_VA::valarray<double> measurements;" << endl;

  //
  // Private member declarations.
  //
  // const int n: #of measurements in this set (ie. individual).
  //
  oIndData_h << "private:" << endl;
  oIndData_h << "const int n; // the number of data records." << endl;
  oIndData_h << "void assignToDbl( double&, const CppAD::AD<double>& ) const;" << endl;
  oIndData_h << "void assignToDbl( double&, double ) const;" << endl;
  oIndData_h << "};" << endl;


  //-----------------------------------------------
  // Definition
  //-----------------------------------------------
  //
  // Definition of the constructor that takes a list of
  // valarray objects as arguments.
  // The order must be consistant with the declaration.
  //
  string synonym;
  oIndData_h << "template <class ValueType>" << endl;
  oIndData_h << "IndData<ValueType>::IndData( int nIn";
  pLabel = labels->begin();
  for( ; pLabel != labels->end(); pLabel++ )
    {
      bool isID = ( *pLabel == pID->name );
      oIndData_h << "," << endl;

      //
      // If the label string is of "ID", then the data type is char*.
      // Othewise, double.
      //
      oIndData_h << "const std::vector<" << (isID? "char*":"ValueType") << "> ";
      oIndData_h << "& " << *pLabel << "In";
    }
  oIndData_h << ")" << endl;
  oIndData_h << ": n( nIn ), " << endl;
  oIndData_h << "  nY( 0 ) " << endl;

  //
  // The constructor initialization.
  // Assign the argument values to the internal valarray variables.
  // Also assign the same values to equivalent (synonym) variables
  // if the variable has a synonym defined.
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
  // The constructor body.
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
  pRawTable = rawTable->begin();
  for( ; pRawTable != rawTable->end(); pRawTable++ )
    {
      const string label    = pRawTable->second.name;
      const string keyLabel = SymbolTable::key( label );
      if( keyLabel == KeyStr.OMEGA || keyLabel == KeyStr.SIGMA )
	{
	  continue;
	}

      //
      // Initialize sizes of place holders for computed values.
      //
      if( find( labels->begin(), labels->end(), pRawTable->second.name ) 
	  == labels->end() )
	oIndData_h << "," << endl << label << "( nIn )";
    }
  oIndData_h << endl;

  oIndData_h << "{" << endl;
  //oIndData_h << "   nY = std::count_if( " << UserStr.MDV << ".begin(), ";
  //oIndData_h << UserStr.MDV << ".end(), logical_true<ValueType>() );" << endl;
  oIndData_h << "   for( int i=0; i<n; i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      if( " << UserStr.MDV << "[i] != 1 )" << endl;
  oIndData_h << "          ++nY;" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << endl;
  oIndData_h << "   measurements.resize( nY ); " << endl;
  oIndData_h << "   for( int i=0, j=0; i<n; i++ )" << endl;
  oIndData_h << "   {" << endl;
  if( myThetaLen > 0 )
    oIndData_h << "      " << UserStr.THETA << "[i].resize( " << myThetaLen << " );" << endl;
  if( myEtaLen > 0 )
    oIndData_h << "      " << UserStr.ETA   << "[i].resize( " << myEtaLen << " );" << endl;
  if( myEpsLen > 0 )
    oIndData_h << "      " << UserStr.EPS   << "[i].resize( " << myEpsLen << " );" << endl;

  oIndData_h << "        if( " << UserStr.MDV << "[i] != 1 )" << endl;
  oIndData_h << "        {" << endl;
  oIndData_h << "           assignToDbl( measurements[j], " << UserStr.DV << "[i] );" << endl;
  oIndData_h << "           j++;" << endl;
  oIndData_h << "        }" << endl;

  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;

  oIndData_h << endl;
  oIndData_h << "template <class ValueType>" << endl;
  oIndData_h << "IndData<ValueType>::~IndData(){}" << endl;

  oIndData_h << "template <class ValueType>" << endl;
  oIndData_h << "IndData<ValueType>::IndData(){}" << endl;

  oIndData_h << "template <class ValueType>" << endl;
  oIndData_h << "IndData<ValueType>::IndData( const IndData<ValueType>& ){}" << endl;

  oIndData_h << "template <class ValueType>" << endl;
  oIndData_h << "IndData<ValueType>& IndData<ValueType>::operator=( const IndData<ValueType>& ){}" << endl;

  oIndData_h << "template <class ValueType>" << endl;
  oIndData_h << "const SPK_VA::valarray<double> IndData<ValueType>::getMeasurements() const" << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   return measurements;" << endl;
  oIndData_h << "}" << endl;

  oIndData_h << "template <class ValueType>" << endl;
  oIndData_h << "void IndData<ValueType>::assignToDbl( double & d, const CppAD::AD<double>& ad ) const" << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   d = CppAD::Value( ad );" << endl;
  oIndData_h << "   return;" << endl;
  oIndData_h << "}" << endl;

  oIndData_h << "template <class ValueType>" << endl;
  oIndData_h << "void IndData<ValueType>::assignToDbl( double & left, double right  ) const" << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   left = right;" << endl;
  oIndData_h << "   return;" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  oIndData_h << "template <class ValueType>" << endl;
  oIndData_h << "void IndData<ValueType>::compResiduals()" << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   for( int i=0; i<n; i++ )" << endl;
  oIndData_h << "   {" << endl;
  oIndData_h << "      " << UserStr.RES << "[i] =" << UserStr.DV << "[i] - " << UserStr.PRED << "[i];" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;
  oIndData_h << endl;

  oIndData_h << "// It is unfortunately that this function is dependent on CppAD. " << endl;
  oIndData_h << "// The type of template argument must have CppAD::Value() operator." << endl;
  oIndData_h << "template <class ValueType>" << endl;
  oIndData_h << "void IndData<ValueType>::compWeightedResiduals( const SPK_VA::valarray<double>& Ri )" << endl;
  oIndData_h << "{" << endl;
  oIndData_h << "   using SPK_VA::valarray;" << endl;
  oIndData_h << "   using std::vector;" << endl;
  oIndData_h << "   assert( Ri.size() == n * n );" << endl;
  oIndData_h << "   compResiduals();" << endl;
  oIndData_h << "   valarray<double> r( n );" << endl;
  oIndData_h << "   for( int i=0; i<n; i++ )" << endl;
  oIndData_h << "      r[i] = CppAD::Value( " << UserStr.RES << "[i] );" << endl;
  oIndData_h << "   valarray<double> C( 0.0, n * n );" << endl;
  oIndData_h << "   C = cholesky( Ri, n );" << endl;
  oIndData_h << "   valarray<double> w = multiply( C, n, r, 1 );" << endl;
  oIndData_h << "   vector< CppAD::AD<double> > Cr(n);" << endl;
  oIndData_h << "   for( int i=0; i<n; i++ )" << endl;
  oIndData_h << "      " << UserStr.WRES << "[i] = w[i];" << endl;
  oIndData_h << "   return;" << endl;

  oIndData_h << "}" << endl;

  oIndData_h << "#endif" << endl;

  oIndData_h.close();
}
void NonmemTranslator::generateDataSet( ) const
{
  const map<const string, Symbol> * t = table->getTable();
  const vector<string> *labels = table->getLabels();
  vector<string>::const_iterator pLabel;
  int nLabels = labels->size();
  const Symbol * pID = table->findi( KeyStr.ID );

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
  // constructor appear is critical.  The order must
  // match with the IndData constructor's interface.
  // It relizes on the order of strings stored in the list 
  // returned by "SymbolTable::getLabels()".
  // Thus, in between the time when the DataSet constructor
  // is defined and the time when the IndData constructor
  // is declared/defined, the SymbolTable object
  // may NOT be modified.
  //  const Symbol* pID = table->findi(KeyStr.ID);
  //
  ofstream oDataSet_h( fDataSet_h );
  if( !oDataSet_h.good() )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Failed to create %s file.", fDataSet_h );
     SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }

  oDataSet_h << "// " << myDescription << endl;

  oDataSet_h << "#ifndef DATASET_H" << endl;
  oDataSet_h << "#define DATASET_H" << endl;

  oDataSet_h << "#include <vector>" << endl;
  oDataSet_h << "#include <spk/SpkValarray.h>" << endl;
  oDataSet_h << "#include \"IndData.h\"" << endl;
  oDataSet_h << endl;

  //-----------------------------------------------
  // Declaration
  //-----------------------------------------------
  oDataSet_h << "template <class ValueType>" << endl;
  oDataSet_h << "class DataSet" << endl;
  oDataSet_h << "{" << endl;
      
  //
  // public member declarations
  //
  // The default constructor initializes the entire data set
  // internally.
  //
  // vector<IndData<ValueType>*> data: The entire data set.
  // const int popSize:      : The number of individuals in the population.
  oDataSet_h << "public:" << endl;
  oDataSet_h << "DataSet();" << endl;
  oDataSet_h << "~DataSet();" << endl;
  oDataSet_h << endl;

  oDataSet_h << "std::vector<IndData<ValueType>*> data;" << endl;
  oDataSet_h << "const int popSize;" << endl;
  oDataSet_h << "const SPK_VA::valarray<double> getAllMeasurements() const;" << endl;
  oDataSet_h << "void compAllResiduals();" << endl;
  oDataSet_h << "void compAllWeightedResiduals( std::vector< SPK_VA::valarray<double> >& R );" << endl;
  oDataSet_h << endl;
 
  //
  // protected member declarations
  //
  // The copy constructor and the assigment operator are 
  // prohibited in use.
  //
  oDataSet_h << "protected:" << endl;
  oDataSet_h << "DataSet( const DataSet& );" << endl;
  oDataSet_h << "DataSet& operator=( const DataSet& );" << endl;
  oDataSet_h << endl;

  oDataSet_h << "private:" << endl;
  oDataSet_h << "SPK_VA::valarray<double> measurements; // a long vector containg all measurements" << endl;
  oDataSet_h << "SPK_VA::valarray<int> N; // a vector containing the # of measurements for each individual." << endl;

  oDataSet_h << "};" << endl;


  //-----------------------------------------------
  // Definition
  //-----------------------------------------------
       
  //
  // The constructor
  //
  // Initialize the class member variables.
  //
  oDataSet_h << "template <class ValueType>" << endl;
  oDataSet_h << "DataSet<ValueType>::DataSet()" << endl;
  oDataSet_h << ": popSize( " << myPopSize << " )," << endl;
  oDataSet_h << "  data( " << myPopSize << " )," << endl;
  oDataSet_h << "  N( " << myPopSize << " )" << endl;
  oDataSet_h << "{" << endl;

  // Initialize the entire data set.
  for( int who=0, sofar=0, nRecords=0; who < myPopSize; who++, sofar+=nRecords )
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
      oDataSet_h << "//------------------------------------" << endl;
      oDataSet_h << "// Subject <" << id << "> " << endl;
      oDataSet_h << "// # of sampling points = " << nRecords << endl;
      oDataSet_h << "//------------------------------------" << endl;
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
	  string carray_name   = s->name + "_" + c_who + "_c";
	  string vector_name = s->name + "_" + c_who;

	  oDataSet_h << (isID? "char*":"ValueType") << " " << carray_name << "[] = { ";
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
	  oDataSet_h << "std::vector<" << (isID? "char*":"ValueType") << "> ";
	  oDataSet_h << vector_name;
	  oDataSet_h << "( " << nRecords << " );" << endl;
	  oDataSet_h << "copy( " << carray_name << ", " << carray_name << "+" << nRecords;
	  oDataSet_h << ", " << vector_name << ".begin() );" << endl;
	}

      //
      // Create an IndData object.  The order in which the arguments
      // are passed to the IndData constructor must be strictly
      // compliant to the order in which the label strings are stored
      // in the list returned by SymbolTable::getLabels().
      //
      oDataSet_h << "data[" << who << "] = new IndData<ValueType>";
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

  oDataSet_h << "   int nY = N.sum();" << endl;
  oDataSet_h << "   measurements.resize( nY ); " << endl;
  oDataSet_h << "   for( int i=0, m=0; i<popSize; i++ )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      int nYi = data[i]->getMeasurements().size();" << endl;
  oDataSet_h << "      measurements[ SPK_VA::slice( m, nYi, 1 ) ] = data[i]->getMeasurements();" << endl;
  oDataSet_h << "      m+=nYi;" << endl;
  oDataSet_h << "   }" << endl;

  oDataSet_h << "}" << endl;

  // The destructor
  // Free memory allocated for the entire data set.
  oDataSet_h << "template <class ValueType>" << endl;
  oDataSet_h << "DataSet<ValueType>::~DataSet()" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      delete data[i];" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;

  oDataSet_h << "template <class ValueType>" << endl;
  oDataSet_h << "DataSet<ValueType>::DataSet( const DataSet<ValueType>& ){}" << endl;

  oDataSet_h << "template <class ValueType>" << endl;
  oDataSet_h << "DataSet<ValueType>& DataSet<ValueType>::operator=( const DataSet<ValueType>& ){}" << endl;
  oDataSet_h << "template <class ValueType>" << endl;
  oDataSet_h << "const SPK_VA::valarray<double> DataSet<ValueType>::getAllMeasurements() const" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   return measurements;" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  oDataSet_h << "template <class ValueType>" << endl;
  oDataSet_h << "void DataSet<ValueType>::compAllResiduals()" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->compResiduals();" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  oDataSet_h << "template <class ValueType>" << endl;
  oDataSet_h << "void DataSet<ValueType>::compAllWeightedResiduals( std::vector< SPK_VA::valarray<double> >& R )" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      data[i]->compWeightedResiduals( R[i] );" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;
  oDataSet_h << endl;

  oDataSet_h << "#endif" << endl;
  oDataSet_h.close();
}
void NonmemTranslator::generatePred( const char* fPredEqn_cpp ) const
{
  // The vector, labels, contains the data (from the data file) item labels
  // and the variable names defined within the user's pred block.
  const vector<string> * labels = table->getLabels();
  const int nLabels = labels->size();
  vector<string>::const_iterator pLabel;

  // The map, rawTable, points to the actuall symbol table which
  // contains all entries including the data (from the data file),
  // the NONMEM required entries (such as THETA, EPS, etc.) and
  // the user defined variable names.
  const map<const string, Symbol> * rawTable = table->getTable();
  map<const string, Symbol>::const_iterator pRawTable;

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

  // macros and header includes
  oPred_h << "// " << myDescription << endl;

  oPred_h << "#ifndef PRED_H" << endl;
  oPred_h << "#define PRED_H" << endl;
  oPred_h << endl;

  oPred_h << "#include <vector>" << endl;
  oPred_h << "#include <string>" << endl;
  oPred_h << "#include <spkpred/PredBase.h>" << endl;
  oPred_h << "#include <CppAD/CppAD.h>" << endl;
  oPred_h << "#include \"DataSet.h\"" << endl;
  oPred_h << endl;
  
  oPred_h << "const CppAD::AD<double> pow( const CppAD::AD<double>& x, int n )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "   CppAD::AD<double> y = 1.0;" << endl;
  oPred_h << "   if( n > 0 )" << endl;
  oPred_h << "   {" << endl;
  oPred_h << "      for( int i=0; i<n; i++ )" << endl;
  oPred_h << "      {" << endl;
  oPred_h << "         y *= x;" << endl;
  oPred_h << "      }" << endl;
  oPred_h << "   }" << endl;
  oPred_h << "   else if( n < 0 )" << endl;
  oPred_h << "   {" << endl;
  oPred_h << "      for( int i=n; i<0; i++ )" << endl;
  oPred_h << "      {" << endl;
  oPred_h << "         y /= x;" << endl;
  oPred_h << "      }" << endl;
  oPred_h << "   }" << endl;
  oPred_h << "   return y;" << endl;
  oPred_h << "}" << endl;
  oPred_h << "const CppAD::AD<double> pow( int x, const CppAD::AD<double>& n )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "   return pow( static_cast< CppAD::AD<double> >( x ), n );" << endl;
  oPred_h << "}" << endl;
  oPred_h << "const CppAD::AD<double> pow( const CppAD::AD<double>& x, double n )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "   return pow( x, CppAD::AD<double>( n ) );" << endl;
  oPred_h << "}" << endl;
  oPred_h << "const CppAD::AD<double> pow( double x, const CppAD::AD<double>& n )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "   return pow( CppAD::AD<double>( x ), n );" << endl;
  oPred_h << "}" << endl;

  oPred_h << endl;
  
  
  //----------------------------------------------
  // Declaration
  //----------------------------------------------
  oPred_h << "template <class ValueType>" << endl;
  oPred_h << "class Pred : public PredBase<ValueType>" << endl;
  oPred_h << "{" << endl;
      
  //
  // public interfaces
  //
  oPred_h << "public:" << endl;

  // The legal constructor.
  // This constructor takes a pointer to the DataSet (the set of
  // all individuals' data).
  oPred_h << "Pred( const DataSet<ValueType>* dataIn );" << endl;

  // The destructor.
  oPred_h << "~Pred();" << endl;

  // Function that retuns the number of the i-th individual's measurments
  oPred_h << "int getNObservs( int ) const;" << endl;

  // eval(): evaluates PRED.
  oPred_h << "bool eval( int spk_thetaOffset, int spk_thetaLen," << endl;
  oPred_h << "           int spk_etaOffset,   int spk_etaLen," << endl;
  oPred_h << "           int spk_epsOffset,   int spk_epsLen," << endl;
  oPred_h << "           int spk_fOffset,     int spk_fLen," << endl;
  oPred_h << "           int spk_yOffset,     int spk_yLen," << endl;
  oPred_h << "           int spk_i," << endl;
  oPred_h << "           int spk_j," << endl;
  oPred_h << "           const std::vector<ValueType>& spk_indepVar," << endl;
  oPred_h << "           std::vector<ValueType>& spk_depVar );" << endl;

  oPred_h << endl;

  //
  // Protected member declarations.
  //
  // Illegal constructors and member functions.
  //
  oPred_h << "protected:" << endl;
  oPred_h << "Pred();" << endl;
  oPred_h << "Pred( const Pred& );" << endl;
  oPred_h << "Pred & operator=( const Pred& );" << endl;

  // 
  // Private member delarations
  //
  // const DataSet<ValueType> *perm: A pointer to the read-only data set.
  // DataSet<ValueType> temp:The temporary storage for current values
  // mutable string id:      A place holder for the current ID value
  // mutable T data_item1:   A place holder for a data item, data_item1
  // mutable T data_item2:   A place holder fot a data item, data_item2
  // ...
  // mutable T user_var1:    A place holder for a user defined variable, user_var1
  // mutable T user_var2:    A place holder for a user defined variable, user_var2
  // ...
  // mutable T NONMEM_var1:  A place holder for a NONMEM required variable
  // mutable T NONMEM_var1:  A place holder for a NONMEM required variable
  // ...
  oPred_h << "private:" << endl;
  oPred_h << "const int nIndividuals;" << endl;
  oPred_h << "const DataSet<ValueType> *perm;" << endl;
  oPred_h << "DataSet<ValueType> temp;" << endl;
  oPred_h << "mutable bool isIterationCompleted;" << endl;

  // Taking care of the data items (from the data file).
  // Only the "ID" data item values are of type string,
  // otherwise all numeric, T.
  pLabel = labels->begin();
  for( int i=0; i<nLabels, pLabel != labels->end(); i++, pLabel++ )
    {
      bool isID = (SymbolTable::key( *pLabel ) == KeyStr.ID );

      const Symbol* s = table->findi( *pLabel );
      oPred_h << "mutable " << ( isID? "std::string" : "ValueType" );
      oPred_h << " " << s->name << ";" << endl;
      if( !s->synonym.empty() )
	{
	  oPred_h << "mutable " << ( isID? "std::string" : "ValueType" );
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
  for( pRawTable = rawTable->begin(); pRawTable != rawTable->end(); pRawTable++ )
    {
      const string label    = pRawTable->second.name;
      const string keyLabel = SymbolTable::key( label );

      // Ignore if the label is of the NONMEM required variable names.
      // They have to be declared in the body of PRED() because
      // they (theta, eta, eps) have to be "const" double array.
      if( keyLabel != KeyStr.THETA 
	  && keyLabel != KeyStr.ETA 
	  && keyLabel != KeyStr.EPS 
	  && keyLabel != KeyStr.SIGMA
	  && keyLabel != KeyStr.OMEGA )
	{
	  // Ignore if the label is of the data item's.
	  if( find( labels->begin(), labels->end(), label ) 
	      == labels->end() )
	    {
	      oPred_h << "mutable ValueType " << label;
	      oPred_h << ";" << endl;
	    }
	}
    }

  // footer
  oPred_h << "};" << endl;

  //----------------------------------------------
  // Definition
  //----------------------------------------------
  oPred_h << "template <class ValueType>" << endl;
  oPred_h << "Pred<ValueType>::Pred( const DataSet<ValueType>* dataIn )" << endl;
  oPred_h << ": perm( dataIn )," << endl;
  oPred_h << "  nIndividuals( " << myPopSize << " )," << endl;
  oPred_h << "  isIterationCompleted( true )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "template <class ValueType>" << endl;
  oPred_h << "Pred<ValueType>::~Pred()" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "template <class ValueType>" << endl;
  oPred_h << "int Pred<ValueType>::getNObservs( int spk_i ) const" << endl;
  oPred_h << "{" << endl;
  oPred_h << "  return perm->data[spk_i]->" << UserStr.ID << ".size();" << endl;
  oPred_h << "}" << endl;


  oPred_h << "template <class ValueType>" << endl;
  oPred_h << "bool Pred<ValueType>::eval( int spk_thetaOffset, int spk_thetaLen," << endl;
  oPred_h << "                        int spk_etaOffset,   int spk_etaLen," << endl;
  oPred_h << "                        int spk_epsOffset,   int spk_epsLen," << endl;
  oPred_h << "                        int spk_fOffset,     int spk_fLen," << endl;
  oPred_h << "                        int spk_yOffset,     int spk_yLen," << endl;
  oPred_h << "                        int spk_i," << endl;
  oPred_h << "                        int spk_j," << endl;
  oPred_h << "                        const std::vector<ValueType>& spk_indepVar," << endl;
  oPred_h << "                        std::vector<ValueType>& spk_depVar )" << endl;
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
      oPred_h << s->name;
      oPred_h << " = perm->data[spk_i]->";
      oPred_h << s->name << "[spk_j];" << endl;
      // synonym
      if( !s->synonym.empty() )
	{
	  oPred_h << s->synonym;
	  oPred_h << " = perm->data[spk_i]->";
	  oPred_h << s->synonym << "[spk_j];" << endl;
	}
    }
  for( int i=0; i<myThetaLen; i++ )
    {
      oPred_h << "typename std::vector<ValueType>::const_iterator " << UserStr.THETA << i+1;
      oPred_h << " = spk_indepVar.begin() + spk_thetaOffset + " << i << ";" << endl;
    }
  for( int i=0; i<myEtaLen; i++ )
    {
      oPred_h << "typename std::vector<ValueType>::const_iterator " << UserStr.ETA << i+1;
      oPred_h << " = spk_indepVar.begin() + spk_etaOffset + " << i << ";" << endl;
    }

  // EPS is only apparent in the population analysis.
  // The size of EPS vector is the order of SIGMA which is only apparent in
  // the population analysis.  So, if this is the individual level,
  // "myEpsLen" has been set to zero; thus the following loop loops zero times.
  for( int i=0; i<myEpsLen; i++ )
    {
      oPred_h << "typename std::vector<ValueType>::const_iterator " << UserStr.EPS << i+1;
      oPred_h << " = spk_indepVar.begin() + spk_epsOffset + " << i << ";" << endl;
    }
  oPred_h << "typename std::vector<ValueType>::const_iterator " << UserStr.THETA;
  oPred_h << " = spk_indepVar.begin() + spk_thetaOffset;" << endl;
  oPred_h << "typename std::vector<ValueType>::const_iterator " << UserStr.ETA;
  oPred_h << " = spk_indepVar.begin() + spk_etaOffset;" << endl;
  if( myTarget == POP )
    {
      oPred_h << "typename std::vector<ValueType>::const_iterator " << UserStr.EPS;
      oPred_h << " = spk_indepVar.begin() + spk_epsOffset;" << endl;
    }

  oPred_h << "ValueType " << UserStr.F << " = 0.0;" << endl;

  oPred_h << "ValueType " << UserStr.Y << " = 0.0;" << endl;
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

  for( pRawTable = rawTable->begin(); pRawTable != rawTable->end(); pRawTable++ )
    {
      // THETA, ETA, EPS are given Pred::eval() as vectors by the caller.
      // So, we have to treat these guys a bit different from the user variables
      // which are scalar values.
      const string label    = pRawTable->second.name;
      const string keyLabel = SymbolTable::key( label );
      if( keyLabel == KeyStr.THETA )
	{
	  oPred_h << "copy( " << label << ", " << label << "+spk_thetaLen, ";
          oPred_h << "temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	}
      else if( keyLabel == KeyStr.ETA )
	{
	  oPred_h << "copy( " << label << ", " << label << "+spk_etaLen, ";
          oPred_h << "temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	}
      else if( keyLabel == KeyStr.EPS )
	{
	  oPred_h << "copy( " << label << ", " << label << "+spk_epsLen, ";
          oPred_h << "temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	}
      else if( keyLabel == KeyStr.OMEGA || keyLabel == KeyStr.SIGMA )
	{
	  // ignore.  these don't get used within PRED.
	}
      else if( keyLabel == KeyStr.WRES || keyLabel == KeyStr.RES )
	{
	  // ignore.  These values are only computed outside at the final estimate.
	}
      else
	{
	  if( find( labels->begin(), labels->end(), label ) 
	      == labels->end() )
	    {
	      oPred_h << "temp.data[ spk_i ]->" << label;
	      oPred_h << "[ spk_j ]";
	      oPred_h << " = " << label << ";" << endl;
	    }
	}
    }   
  oPred_h << endl;

  // Saving/moving computed values to ensure a complete set of values
  // is available even when a failure occurs.
  //
  oPred_h << "if( spk_i == " << myPopSize << "-1 && spk_j == perm->data[spk_i]->";
  oPred_h << UserStr.ID << ".size()-1 )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "  // This means, SPK advanced in iteration." << endl;
  oPred_h << "  // Move temporary storage to permanent storage." << endl;
  oPred_h << "  isIterationCompleted = true;" << endl;
  oPred_h << "  for( int i=0; i < nIndividuals; i++ )" << endl;
  oPred_h << "  {" << endl;
  // User defined variables temp(current) => permanent
  // The user defined scalar variables
  for( pRawTable = rawTable->begin(); pRawTable != rawTable->end(); pRawTable++ )
    {
      const string label     = pRawTable->second.name;
      const string keyLabel = SymbolTable::key( label );
      if( keyLabel == KeyStr.OMEGA || keyLabel == KeyStr.SIGMA || keyLabel == KeyStr.WRES || keyLabel == KeyStr.RES )
	continue;

      if( find( labels->begin(), labels->end(), label ) == labels->end() )
	{
	  oPred_h << "    perm->data[ i ]->" << label;
	  oPred_h << " = temp.data[ i ]->";
	  oPred_h << label << ";" << endl;
	}
    }      
  oPred_h << "  }" << endl;
  oPred_h << "}" << endl;
  oPred_h << "else" << endl;
  oPred_h << "{" << endl;
  oPred_h << "  isIterationCompleted = false;" << endl;
  oPred_h << "}" << endl;
  oPred_h << endl;

  ///////////////////////////////////////////////////////////////////////////////////

  // Set the output values
  oPred_h << "spk_depVar[ spk_fOffset+spk_j ] = " << UserStr.F << ";" << endl;
  oPred_h << "spk_depVar[ spk_yOffset+spk_j ] = " << UserStr.Y << ";" << endl;

  // Pred::eval() returns true if MDV(i,j) is 0, which means DV is NOT missing.
  // In this iteration, it is assumed that MDV=true for all, so return true.
  oPred_h << "if( perm->data[ spk_i ]->" << UserStr.MDV << "[ spk_j ] == 0 )" << endl;
  oPred_h << "   return true;" << endl;
  oPred_h << "else return false;" << endl;

  oPred_h << "}" << endl;

  oPred_h << "template <class ValueType>" << endl;
  oPred_h << "Pred<ValueType>::Pred()" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "template <class ValueType>" << endl;
  oPred_h << "Pred<ValueType>::Pred( const Pred<ValueType>& )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "template <class ValueType>" << endl;
  oPred_h << "Pred<ValueType> & Pred<ValueType>::operator=( const Pred<ValueType>& )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "#endif" << endl;
  oPred_h.close();
}
void NonmemTranslator::generateMonteParsNamespace() const
{
  //==================================================================
  // Generate the MontePars namespace if Monte is requested.
  //==================================================================
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
  oMontePars << "// The user requested the " << (myTarget==POP? "population":"individual") << " analysis." << endl;
  oMontePars << "// " << endl;
  oMontePars << "//==============================================================================" << endl;

  oMontePars << "#ifndef MONTEPARS_H" << endl;
  oMontePars << "#define MONTEPARS_H" << endl;
  oMontePars << endl;

  oMontePars << "namespace MontePars{" << endl;
  oMontePars << "   enum METHOD { monte, analytic, grid };" << endl;
  oMontePars << "   const enum METHOD method = ";
  if( myMonteMethod == GRID )
    oMontePars << "grid;";
  else if( myMonteMethod == ANALYTIC )
    oMontePars << "analytic;" << endl;
  else //( myMonteMethod == MONTE )
    oMontePars << "monte;" << endl;

  oMontePars << "   const int numberEval = " << myMonteNumberEval << ";" << endl;
  oMontePars << "};" << endl;

  oMontePars << endl;

  oMontePars << "#endif" << endl;
}
void NonmemTranslator::generateNonmemParsNamespace() const
{
  //==================================================================
  // Generate the NonmemPars namespace.
  //==================================================================
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
  oNonmemPars << "// The user requested the " << (myTarget==POP? "population":"individual") << " analysis." << endl;
  oNonmemPars << "// This means that this namespace would contain materials related to " << endl;
  if( myTarget==POP )
    {
      oNonmemPars << "// all of THETA, OMEGA, ETA, SIGMA and EPS." << endl;
    }
  else
    {
      oNonmemPars << "// only THETA, OMEGA and ETA." << endl;
    }
  if( myIsSimulate )
    {
      oNonmemPars << "// It also contains the input value(s) necessary to simulate a data set " << endl;
      oNonmemPars << "// since it is also requested." << endl;
    }
  oNonmemPars << "// " << endl;
  oNonmemPars << "//=============================================================" << endl;

  oNonmemPars << "#ifndef NONMEMPARS_H" << endl;
  oNonmemPars << "#define NONMEMPARS_H" << endl;
  oNonmemPars << endl;

  oNonmemPars << "#include <valarray>" << endl;
  if( myTarget == POP )
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

  if( myTarget == POP && myIsSimulate )
    {
      oNonmemPars << "   // A valarray object that *will* contain the initial values for THETA." << endl;
      oNonmemPars << "   // The object is intentionally non-constant because you will have to simulate the values " << endl;
      oNonmemPars << "   // and place them  in this placeholder." << endl;
      oNonmemPars << "   valarray<double> thetaIn ( c_thetaIn, nTheta );" << endl;
    }
  else
    {
      oNonmemPars << "   // A valarray object containing the initial values for THETA." << endl;
      oNonmemPars << "   const valarray<double> thetaIn ( c_thetaIn, nTheta );" << endl;
    }
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
     
  oNonmemPars << "   const enum " << (myTarget==POP? "Pop":"Ind") << "PredModel::covStruct omegaStruct = ";
  oNonmemPars << (myTarget==POP? "Pop":"Ind") << "PredModel::";
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
  if( myTarget == POP )
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
  if( myTarget == POP )
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
  //==================================================================
  // Generate the SPK driver
  assert( !(myIsOnlySimulation && myIsEstimate) );
  //==================================================================
  ofstream oDriver ( fFitDriver_cpp );
  if( !oDriver.good() )
  {
     char mess[ SpkCompilerError::maxMessageLen() ];
     sprintf( mess, "Failed to create %s file.", fFitDriver_cpp );
     SpkCompilerException e( SpkCompilerError::ASPK_STD_ERR, mess, __LINE__, __FILE__ );
     throw e;
  }
  const Symbol* pTheta = table->findi(KeyStr.THETA);
  const Symbol* pEta   = table->findi(KeyStr.ETA);
  const Symbol* pOmega = table->findi(KeyStr.OMEGA);

  oDriver << "// " << myDescription << endl;
  oDriver << "#include <iostream>" << endl;
  oDriver << "#include <fstream>" << endl;
  oDriver << "#include <sys/time.h>" << endl;
  oDriver << "#include <vector>" << endl;
  oDriver << endl;

  oDriver << "#include <spk/SpkValarray.h>" << endl;
  oDriver << "#include <spk/SpkException.h>" << endl;
  oDriver << "#include <spk/WarningsManager.h>" << endl;
  oDriver << "#include <CppAD/CppAD.h>" << endl;
  if( myIsEstimate )
  {
     oDriver << "#include <spk/fitIndividual.h>" << endl;
     oDriver << "#include <spk/Optimizer.h>" << endl;
  }
  if( myIsStat )
    {
      oDriver << "#include <spk/inverse.h>" << endl;
      oDriver << "#include <spk/indStatistics.h>" << endl;
      oDriver << "#include <spk/derParStatistics.h>" << endl;
      oDriver << "#include <spk/printInMatrix.h>" << endl;
    }
  if( myIsSimulate )
     oDriver << "#include <spk/simulate.h>" << endl;
  oDriver << "#include \"IndData.h\"" << endl;
  oDriver << "#include \"DataSet.h\"" << endl;
  oDriver << "#include \"NonmemPars.h\"" << endl;
  oDriver << endl;
  oDriver << "#include <spk/multiply.h>" << endl;
  oDriver << "#include <spk/cholesky.h>" << endl;

  oDriver << "///////////////////////////////////////////////////////////////////////////////////" << endl;
  oDriver << "//   NONMEM PRED SPECIFIC" << endl;
  oDriver << "#include \"Pred.h\"" << endl;
  oDriver << "#include <spkpred/IndPredModel.h>" << endl;
  oDriver << "//" << endl;
  oDriver << "///////////////////////////////////////////////////////////////////////////////////" << endl;
  oDriver << endl;

  oDriver << "using SPK_VA::valarray;" << endl;
  oDriver << "using namespace std;" <<endl;
  oDriver << endl;

  oDriver << "enum RETURN_CODE { SUCCESS=0, CONVERGENCE_FAILURE=1, FILE_ACCESS_FAILURE=2, OTHER_FAILURE };" << endl;
  oDriver << endl;

  oDriver << "namespace{" << endl;
  oDriver << "// Compute the residuals for i-th individual." << endl;
  oDriver << "const vector<CppAD::AD<double> > wres( int n," << endl;
  oDriver << "                                       const valarray<double> & Ri," << endl;
  oDriver << "                                       const vector  < CppAD::AD<double> > & residual )" << endl;
  oDriver << "{" << endl;
  oDriver << "   assert( Ri.size() == n * n );" << endl;
  oDriver << "   assert( residual.size() == n );" << endl;
  oDriver << "   valarray<double> r( n );" << endl;
  oDriver << "   for( int i=0; i<n; i++ ) r[i] = CppAD::Value( residual[i] );" << endl;
  oDriver << "   valarray<double> C( 0.0, n * n );" << endl;
  oDriver << "   C = cholesky( Ri, n );" << endl;
  oDriver << "   valarray<double> w = multiply( C, n, r, 1 );" << endl;
  oDriver << "   vector< CppAD::AD<double> > Cr(n);" << endl;
  oDriver << "   for( int i=0; i<n; i++ ) Cr[i] = w[i];" << endl;
  oDriver << "   return Cr;" << endl;
  oDriver << "}" << endl;
  oDriver << endl;
  oDriver << "};" << endl;

  oDriver << "int main( int argc, const char argv[] )" << endl;
  oDriver << "{" << endl;

  oDriver << "/*******************************************************************/" << endl;
  oDriver << "/*                                                                 */" << endl;
  oDriver << "/*   Set up                                                        */" << endl;
  oDriver << "/*                                                                 */" << endl;
  oDriver << "/*******************************************************************/" << endl;
  oDriver << "ofstream oRuntimeError( \"" << fSpkRuntimeError_tmp << "\" );" << endl;
  oDriver << "if( !oRuntimeError.good() )" << endl;
  oDriver << "  {" << endl;
  oDriver << "      fprintf( stderr, \"%s:%d: Failed to create a temporary file, %s.\", ";
  oDriver << " __FILE__, __LINE__, \"" << fSpkRuntimeError_tmp << "\" );" << endl;
  oDriver << "      return FILE_ACCESS_FAILURE;" << endl;
  oDriver << "  }" << endl;
  oDriver << endl;

  oDriver << "const int nY = " << myRecordNums[0] << ";" << endl;
  oDriver << "DataSet< CppAD::AD<double> > set;" << endl;
  oDriver << endl;
  
  oDriver << "const bool isSimOnly       = " << ( myIsOnlySimulation? "true":"false" ) << ";" << endl;
  oDriver << "const bool isSimRequested  = " << ( myIsSimulate? "true":"false" ) << ";" << endl;
  oDriver << "bool haveCompleteData      = " << ( myIsSimulate? "false":"true" ) << ";" << endl;
  oDriver << endl;

  oDriver << "const bool isOptRequested  = " << ( myIsEstimate? "true":"false" ) << ";" << endl;
  oDriver << "bool isOptSuccess          = " << ( myIsEstimate? "false":"true" ) << ";" << endl;
  oDriver << endl;

  oDriver << "const bool isStatRequested = " << ( myIsStat? "true":"false" ) << ";" << endl;
  oDriver << "bool isStatSuccess         = " << ( myIsStat? "false":"true" ) << ";" << endl;
  oDriver << endl;

  oDriver << "const int nRepeats         = " << mySubproblemsN << ";" << endl;
  oDriver << endl;

  oDriver << "//////////////////////////////////////////////////////////////////////" << endl;
  oDriver << "//   NONMEM Specific" << endl;
  oDriver << endl;

  if( myIsEstimate )
    {
      oDriver << "valarray<double> thetaStep( NonmemPars::nTheta );" << endl;
    }
  oDriver << endl;
  oDriver << "valarray<double> thetaOut( NonmemPars::nTheta );" << endl;
  oDriver << endl;
  
  // Omega
  oDriver << "valarray<double> omegaOut( NonmemPars::omegaOrder );" << endl;
  oDriver << endl;
  oDriver << "//" << endl;
  oDriver << "//////////////////////////////////////////////////////////////" << endl;
  oDriver << endl;

  oDriver << "//////////////////////////////////////////////////////////////////////" << endl;
  oDriver << "//   NONMEM PRED Specific" << endl;
  oDriver << "Pred<CppAD::AD<double> > mPred(&set);" << endl;
  oDriver << "IndPredModel model( mPred, " << endl;
  oDriver << "                    NonmemPars::nTheta, " << endl;
  oDriver << "                    NonmemPars::thetaLow, " << endl;
  oDriver << "                    NonmemPars::thetaUp, " << endl;
  oDriver << "                    NonmemPars::thetaIn, " << endl;
  oDriver << "                    NonmemPars::nEta, " << endl;
  oDriver << "                    NonmemPars::omegaStruct," << endl;
  oDriver << "                    NonmemPars::omegaIn );" << endl;
  oDriver << "//" << endl;
  oDriver << "//////////////////////////////////////////////////////////////////////" << endl;
  oDriver << endl;

  oDriver << "const int nB = model.getNIndPar();" << endl;
  oDriver << "valarray<double> bIn  ( nB );" << endl;
  oDriver << "valarray<double> bStep( nB );" << endl;
  oDriver << "valarray<double> bLow ( nB );" << endl;
  oDriver << "valarray<double> bUp  ( nB );" << endl;
  oDriver << "valarray<double> bOut ( nB );" << endl;
  if( myIsEstimate )
    {
      oDriver << "double           bObjOut;" << endl;
      oDriver << "valarray<double> bObj_bOut( nB );" << endl;
      oDriver << "valarray<double> bObj_b_bOut( nB * nB );" << endl;
      oDriver << endl;
      oDriver << "const double eps   = " << myIndEpsilon    << ";" << endl;
      oDriver << "const int    mitr  = " << myIndMitr       << ";" << endl;
      oDriver << "const int    trace = " << myIndTraceLevel << ";" << endl;
      oDriver << "Optimizer    opt( eps, mitr, trace );" << endl;
    }
  oDriver << endl;  
  oDriver << "model.getIndPar       ( bIn );" << endl;
  oDriver << "model.getIndParLimits ( bLow, bUp );" << endl;
  oDriver << "model.getIndParStep   ( bStep );" << endl;
  oDriver << endl;

  oDriver << "/*******************************************************************/" << endl;
  oDriver << "/*                                                                 */" << endl;
  oDriver << "/*   Data Initialization                                           */" << endl;
  oDriver << "/*                                                                 */" << endl;
  oDriver << "/*******************************************************************/" << endl;
  if( myIsSimulate )
    {
       oDriver << "valarray<double> y   ( nY );" << endl;
       oDriver << "valarray<double> yOut( nY );" << endl;
       oDriver << "try" << endl;
       oDriver << "{" << endl;
       oDriver << "   simulate( model, nY, bIn, yOut, NonmemPars::seed );" << endl;
       oDriver << "   for( int j=0; j<nY; j++ )" << endl;
       oDriver << "   {" << endl;
       oDriver << "      set.data[0]->SIMDV[j] = yOut[j];" << endl;
       oDriver << "   }" << endl;
       oDriver << "   y   = yOut;" << endl;
       
       oDriver << "   haveCompleteData = true;" << endl;
       oDriver << "}" << endl;
       oDriver << "catch( SpkException& e )" << endl;
       oDriver << "{" << endl;
       oDriver << "   char mess[ SpkError::maxMessageLen() ];" << endl;
       oDriver << "   sprintf( mess, \"Failed in data simulation.\\n\" );" << endl;
       oDriver << "   e.push( SpkError::SPK_SIMULATION_ERR, mess, __LINE__, __FILE__ );" << endl;
       oDriver << "   oRuntimeError << e << endl;  // Printing out to a file." << endl;
       oDriver << "   cerr << e << endl;    // Printing out to the standard error." << endl; 
       oDriver << "   haveCompleteData = false;" << endl;
       oDriver << "}" << endl;
       oDriver << "catch( ... )" << endl;
       oDriver << "{" << endl;
       oDriver << "   char message[] =\"Unknown exception: failed in data simulation!!!\";" << endl;
       oDriver << "   oRuntimeError << message << endl;  // Printing out to a file." << endl;
       oDriver << "   cerr << message << endl;    // Printing out to the standard error." << endl;
       oDriver << "   haveCompleteData = false;" << endl;
       oDriver << "}" << endl;
    }
  else
    {
      oDriver << "valarray<double> y = set.getAllMeasurements();" << endl;
      oDriver << "haveCompleteData = true;" << endl;
      oDriver << endl;
    }

  if( myIsEstimate )
    {
      oDriver << "/*******************************************************************/" << endl;
      oDriver << "/*                                                                 */" << endl;
      oDriver << "/*   Parameter Estimation                                          */" << endl;
      oDriver << "/*                                                                 */" << endl;
      oDriver << "/*******************************************************************/" << endl;
      oDriver << "timeval optBegin, optEnd;" << endl;
      oDriver << "double optTimeSec = 0.0;" << endl;
      oDriver << "if( isOptRequested && haveCompleteData )" << endl;
      oDriver << "{" << endl;
      oDriver << "  gettimeofday( &optBegin, NULL );" << endl;
      
      oDriver << "  try" << endl;
      oDriver << "  {" << endl;
      oDriver << "     fitIndividual( model," << endl;
      oDriver << "                    y," << endl;
      oDriver << "                    opt," << endl;
      oDriver << "                    bLow," << endl;
      oDriver << "                    bUp," << endl;
      oDriver << "                    bIn," << endl;
      oDriver << "                    bStep," << endl;
      oDriver << "                   &bOut," << endl;
      oDriver << "                   &bObjOut," << endl;
      oDriver << "                   &bObj_bOut," << endl;
      oDriver << "                   &bObj_b_bOut," << endl;
      oDriver << "                    false );" << endl;
      oDriver << "     isOptSuccess = true;" << endl;
      oDriver << "  }" << endl;
      oDriver << "  catch( SpkException& e )" << endl;
      oDriver << "  {" << endl;
      oDriver << "     char mess[ SpkError::maxMessageLen() ];" << endl;
      oDriver << "     sprintf( mess, \"Failed in population parameter estimation.\\n\" );" << endl;
      oDriver << "     e.push( SpkError::SPK_OPT_ERR, mess, __LINE__, __FILE__ );" << endl;
      oDriver << "     oRuntimeError << e << endl;" << endl;
      oDriver << "     cerr << e << endl;" << endl; 
      oDriver << "     isOptSuccess = false;" << endl;
      oDriver << "  }" << endl;
      oDriver << "  catch( ... )" << endl;
      oDriver << "  {" << endl;
      oDriver << "     char message[] = \"Unknown exception: failed in parameter estimation!!!\";" << endl;
      oDriver << "     oRuntimeError << message << endl;" << endl;
      oDriver << "     cerr << message << endl;" << endl;
      oDriver << "     isOptSuccess = false;" << endl;
      oDriver << "  }" << endl;
      oDriver << "  gettimeofday( &optEnd, NULL );" << endl;
      oDriver << "  optTimeSec = difftime( optEnd.tv_sec, optBegin.tv_sec );" << endl;
      oDriver << endl;
      oDriver << "  //////////////////////////////////////////////////////////////////////" << endl;
      oDriver << "  //   NONMEM Specific" << endl;
      oDriver << "  if( isOptSuccess || !isOptSuccess )" << endl;
      oDriver << "  {" << endl;
      oDriver << "     valarray<double> ROut( nY * nY );" << endl;
      oDriver << "     model.getTheta( thetaOut );" << endl;
      oDriver << "     model.getOmega( omegaOut );" << endl;
      oDriver << "     model.setIndPar( bOut );" << endl;
      oDriver << "     model.dataVariance( ROut );" << endl;
      oDriver << "     for( int j=0; j<nY; j++ )" << endl;
      oDriver << "        set.data[0]->" << UserStr.RES << "[j] ";
      oDriver << "= y[j] - set.data[0]->" << UserStr.PRED << "[j];" << endl;
      oDriver << "     set.data[0]->" << UserStr.WRES;
      oDriver << " = wres( nY, ROut, set.data[0]->" << UserStr.RES << " ); " << endl;
      oDriver << "  }" << endl;
      oDriver << "  //" << endl;
      oDriver << "  //////////////////////////////////////////////////////////////////////" << endl;    
      oDriver << "}" << endl;

      // Statistics can be only computed when the parameter estimation has been done.
      if( myIsStat )
	{

	  oDriver << "/*******************************************************************/" << endl;
	  oDriver << "/*                                                                 */" << endl;
	  oDriver << "/*   Statistics                                                    */" << endl;
	  oDriver << "/*                                                                 */" << endl;
	  oDriver << "/*******************************************************************/" << endl;
	  oDriver << "timeval statBegin, statEnd;" << endl;
	  oDriver << "double statTimeSec = 0.0;" << endl;
          oDriver << "const int nDegOfFreedom = nY - nB;" << endl;
          oDriver << "valarray<double> bCov( nB * nB );" << endl;
          oDriver << "valarray<double> stdPar( nB );" << endl;
          oDriver << "valarray<double> stdPar_b( nB * nB );" << endl;
	  if( myIsCov || myIsInvCov )
	    oDriver << "valarray<double> stdParCovOut( nB * nB );" << endl;
	  if( myIsStderr )
	    oDriver << "valarray<double> stdParSEOut( nB );" << endl;
	  if( myIsCorrelation )
	    oDriver << "valarray<double> stdParCorrelationOut( nB * nB );" << endl;
	  if( myIsCoefficient )
	    oDriver << "valarray<double> stdParCoefficientOut( nB );" << endl;
	  if( myIsConfidence )
	    oDriver << "valarray<double> stdParConfidenceOut( 2 * nB );" << endl;
	  if( myIsInvCov )
	    oDriver << "valarray<double> stdParInvCovOut( nB * nB );" << endl;
	  
	  oDriver << "valarray<double> f_bOut( nY * nB );" << endl;
	  oDriver << "valarray<double> R_bOut( nY * nY * nB );" << endl;
	  oDriver << "valarray<double> RInvOut( nY * nY );" << endl;
	  oDriver << "if( isStatRequested && haveCompleteData && isOptSuccess )" << endl;
	  oDriver << "{" << endl;
	  oDriver << "   model.setIndPar( bOut );" << endl;
	  oDriver << "   model.dataMean_indPar( f_bOut );" << endl;
	  oDriver << "   model.dataVariance_indPar( R_bOut );" << endl;
	  oDriver << "   model.dataVarianceInv( RInvOut );" << endl;

	  // indStatistics
	  oDriver << "   gettimeofday( &statBegin, NULL );" << endl;
	  oDriver << "   try" << endl;
	  oDriver << "   {" << endl;
	  oDriver << "      model.getStandardPar( stdPar );" << endl;
          oDriver << "      model.getStandardPar_indPar( stdPar_b );" << endl;
	  oDriver << "      indStatistics( bOut, " << endl;
	  oDriver << "                     f_bOut," << endl;
	  oDriver << "                     R_bOut," << endl;
	  oDriver << "                     RInvOut," << endl;
          oDriver << "                    &bCov," << endl;
          oDriver << "                     NULL," << endl;
          oDriver << "                     NULL," << endl;
          oDriver << "                     NULL," << endl;
          oDriver << "                     NULL" << endl;
          oDriver << "                   );" << endl;
          oDriver << "      derParStatistics( bCov," << endl;
          oDriver << "                        stdPar," << endl;
          oDriver << "                        stdPar_b," << endl;
          oDriver << "                        nDegOfFreedom," << endl;
          oDriver << "                        " << ( myIsCov || myIsInvCov? "&stdParCovOut"        : "NULL" ) << "," << endl;
          oDriver << "                        " << ( myIsStderr?            "&stdParSEOut"         : "NULL" ) << "," << endl;
          oDriver << "                        " << ( myIsCorrelation?       "&stdParCorrelationOut": "NULL" ) << "," << endl;
          oDriver << "                        " << ( myIsCoefficient?       "&stdParCoefficientOut": "NULL" ) << "," << endl;
          oDriver << "                        " << ( myIsConfidence?        "&stdParConfidenceOut" : "NULL" ) << endl;
          oDriver << "                      );" << endl;
	  oDriver << "      stdParInvCovOut = inverse( stdParCovOut, nB );" << endl;
	  oDriver << "      isStatSuccess = true;" << endl;
	  oDriver << "   }" << endl;
	  oDriver << "   catch( SpkException& e )" << endl;
	  oDriver << "   {" << endl;
	  oDriver << "      char mess[ SpkError::maxMessageLen() ];" << endl;
	  oDriver << "      sprintf( mess, \"Failed to compute statistics value(s).\\n\" );" << endl;
	  oDriver << "      e.push( SpkError::SPK_STATISTICS_ERR, mess, __LINE__, __FILE__ );" << endl;
	  oDriver << "      oRuntimeError << e << endl;" << endl;
	  oDriver << "      cerr << e << endl;" << endl;
	  oDriver << "      isStatSuccess = false;" << endl;
	  oDriver << "   }" << endl;
	  oDriver << "   catch( ... )" << endl;
	  oDriver << "   {" << endl;
	  oDriver << "      char message[] = \"Unknown exception: failed in statistics calculation!!!\";" << endl;
	  oDriver << "      oRuntimeError << message << endl;" << endl;
	  oDriver << "      cerr << message << endl;" << endl;
	  oDriver << "      isStatSuccess = false;" << endl;
	  oDriver << "   }" << endl;
	  oDriver << endl;

	  oDriver << "   gettimeofday( &statEnd, NULL );" << endl;
	  oDriver << "   statTimeSec = difftime( statEnd.tv_sec, statBegin.tv_sec );" << endl;
	  oDriver << "}" << endl;
	}
    }
  oDriver << endl;
  oDriver << "oRuntimeError.close();" << endl;

  oDriver << "/*******************************************************************/" << endl;
  oDriver << "/*                                                                 */" << endl;
  oDriver << "/*   ReportML Document                                             */" << endl;
  oDriver << "/*                                                                 */" << endl;
  oDriver << "/*******************************************************************/" << endl;
  oDriver << "valarray<double> ROut( nY, nY );" << endl;
  oDriver << "model.setIndPar( bOut );" << endl;
  oDriver << "model.dataVariance( ROut );" << endl;
  oDriver << "for( int j=0; j<nY; j++ )" << endl;
  oDriver << "{" << endl;
  oDriver << "   set.data[0]->" << UserStr.RES << "[j] = y[j] - set.data[0]->" << UserStr.PRED << "[j];" << endl;
  oDriver << "}" << endl;
  oDriver << "set.data[0]->" << UserStr.WRES << " = wres( nY, ROut, set.data[0]->" << UserStr.RES << " ); " << endl;
  
  //oDriver << "set.compWeightedResiduals( ROut );" << endl;
  oDriver << endl;

  oDriver << "ofstream oResults( \"" << fResult_xml << "\" );" << endl;
  oDriver << "if( !oResults.good() )" << endl;
  oDriver << "{" << endl;
  oDriver << "   fprintf( stderr, \"Failed to open a file, %s !!!\", \"" << fResult_xml << "\" );" << endl;
  oDriver << "   return FILE_ACCESS_FAILURE;" << endl;
  oDriver << "}" << endl;

  oDriver << "oResults << \"<?xml version=\\\"1.0\\\"?>\" << endl;" << endl;
  oDriver << "oResults << \"<spkreport>\" << endl;" << endl;
  oDriver << endl;

  // Error messages, if any
  oDriver << "if( !(haveCompleteData && isOptSuccess && isStatSuccess) )" << endl;
  oDriver << "{" << endl;
  oDriver << "   char buf[ SpkError::maxMessageLen() ];" << endl;
  oDriver << "   ifstream iRuntimeError( \"" << fSpkRuntimeError_tmp << "\" );" << endl;
  oDriver << "   oResults << \"<error_message>\" << endl;" << endl;
  oDriver << "   while( iRuntimeError.getline(buf, SpkError::maxMessageLen()) )" << endl;
  oDriver << "   {" << endl;
  oDriver << "      oResults << buf << endl;" << endl;   // Write to the SpkReportML document.
  oDriver << "   }" << endl;
  oDriver << "   oResults << \"</error_message>\" << endl;" << endl;
  oDriver << "   iRuntimeError.close();" << endl;
  oDriver << "}" << endl;
  oDriver << "remove( \"" << fSpkRuntimeError_tmp << "\" );" << endl;
  oDriver << endl;

  oDriver << "if( !haveCompleteData )" << endl;
  oDriver << "{" << endl;
  oDriver << "   oResults << \"</spkreport>\" << endl;" << endl;
  oDriver << "   oResults.close();" << endl;
  oDriver << "   return OTHER_FAILURE;" << endl;
  oDriver << "}" << endl;
  oDriver << endl;

  if( myIsEstimate )
    {
      oDriver << "oResults << \"<ind_analysis_result>\" << endl;" << endl;
      oDriver << endl;
      oDriver << "oResults << \"<ind_opt_result elapsedtime=\\\"\" << optTimeSec << \"\\\">\" << endl;" << endl;
      oDriver << "oResults << \"<ind_obj_out>\" << endl;" << endl;
      oDriver << "oResults << \"<value>\" << bObjOut << \"</value>\" << endl;" << endl;
      oDriver << "oResults << \"</ind_obj_out>\" << endl;" << endl;
      oDriver << endl;

      oDriver << "//////////////////////////////////////////////////////////////////////" << endl;
      oDriver << "//    NONMEM Specific" << endl;
      // theta (b)
      oDriver << "oResults << \"<theta_out length=\\\"\" << NonmemPars::nTheta << \"\\\">\" << endl;" << endl;
      oDriver << "for( int i=0; i<NonmemPars::nTheta; i++ )" << endl;
      oDriver << "{" << endl;
      oDriver << "   oResults << \"<value>\" << thetaOut[i] << \"</value>\" << endl;" << endl;
      oDriver << "}" << endl;
      oDriver << "oResults << \"</theta_out>\" << endl;" << endl;
      // omega 
      oDriver << "oResults << \"<omega_out dimension=\\\"\" << NonmemPars::omegaDim << \"\\\" ";
      if( myOmegaStruct == Symbol::TRIANGLE )
	oDriver << "struct=\\\"diagonal\\\">\" << endl;" << endl;
      else
	oDriver << "struct=\\\"block\\\">\" << endl;" << endl;
      oDriver << "for( int i=0; i<NonmemPars::omegaOrder; i++ )" << endl;
      oDriver << "{" << endl;
      oDriver << "   oResults << \"<value>\" << omegaOut[i] << \"</value>\" << endl;" << endl;
      oDriver << "}" << endl;
      oDriver << "oResults << \"</omega_out>\" << endl;" << endl;
      oDriver << "//" << endl;
      oDriver << "//////////////////////////////////////////////////////////////////////" << endl;
      oDriver << endl;
      oDriver << "oResults << \"</ind_opt_result>\" << endl;" << endl;
      oDriver << endl;

      if( myIsStat )
	{
	  oDriver << "oResults << \"<ind_stat_result elapsedtime=\\\"\" << statTimeSec << \"\\\">\" << endl;" << endl;
	  if( myIsCov )
	    {
	      oDriver << "oResults << \"<ind_covariance_out struct=\\\"block\\\" dimension=\\\"\" << nB << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nB; i++ )" << endl;
	      oDriver << "{" << endl;
              oDriver << "   for( int j=0; j<=i; j++ )" << endl;
              oDriver << "   {" << endl;
	      oDriver << "      oResults << \"   <value>\" << stdParCovOut[i+j*nB] << \"</value>\" << endl;" << endl;
              oDriver << "   }" << endl;
	      oDriver << "}" << endl;

	      oDriver << "oResults << \"</ind_covariance_out>\" << endl;" << endl;
	    }
	  if( myIsInvCov )
	    {
	      oDriver << "oResults << \"<ind_inverse_covariance_out struct=\\\"block\\\" dimension=\\\"\" << nB << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nB; i++ )" << endl;
	      oDriver << "{" << endl;
              oDriver << "   for( int j=0; j<=i; j++ )" << endl;
              oDriver << "   {" << endl;
	      oDriver << "      oResults << \"   <value>\" << stdParInvCovOut[i+j*nB] << \"</value>\" << endl;" << endl;
              oDriver << "   }" << endl;
	      oDriver << "}" << endl;

	      oDriver << "oResults << \"</ind_inverse_covariance_out>\" << endl;" << endl;
	    }
	  if( myIsStderr )
	    {
	      oDriver << "oResults << \"<ind_stderror_out length=\\\"\" << nB << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nB; i++ )" << endl;
	      oDriver << "{" << endl;
	      oDriver << "oResults << \"   <value>\" << stdParSEOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;

	      oDriver << "oResults << \"</ind_stderror_out>\" << endl;" << endl;
	    }
	  if( myIsCorrelation )
	    {
	      oDriver << "oResults << \"<ind_correlation_out struct=\\\"block\\\" dimension=\\\"\" << nB << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nB; i++ )" << endl;
	      oDriver << "{" << endl;
              oDriver << "   for( int j=0; j<=i; j++ )" << endl;
              oDriver << "   {" << endl;
	      oDriver << "      oResults << \"   <value>\" << stdParCorrelationOut[i+j*nB] << \"</value>\" << endl;" << endl;
              oDriver << "   }" << endl;
	      oDriver << "}" << endl;

	      oDriver << "oResults << \"</ind_correlation_out>\" << endl;" << endl;
	    }
	  if( myIsCoefficient )
	    {
	      oDriver << "oResults << \"<ind_coefficient_out length=\\\"\" << nB << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nB; i++ )" << endl;
	      oDriver << "{" << endl;
	      oDriver << "oResults << \"   <value>\" << stdParCoefficientOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;

	      oDriver << "oResults << \"</ind_coefficient_out>\" << endl;" << endl;
	    }
	  if( myIsConfidence )
	    {
	      oDriver << "oResults << \"<ind_confidence_out length=\\\"\" << nB*2 << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nB*2; i++ )" << endl;
	      oDriver << "{" << endl;
	      oDriver << "oResults << \"   <value>\" << stdParConfidenceOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;

	      oDriver << "oResults << \"</ind_confidence_out>\" << endl;" << endl;
	    }
	  oDriver << "oResults << \"</ind_stat_result>\" << endl;" << endl;
	  oDriver << endl;
	}
      oDriver << "oResults << \"</ind_analysis_result>\" << endl;" << endl;
      oDriver << endl;
    }


  //=============================================================================
  // LABELS
  //
  const map<const string, Symbol> * t = table->getTable();
  const Symbol * pID = table->findi(KeyStr.ID);
  if( pID == Symbol::empty() )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "\"ID\" is not defined." );
      SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const int nItems = t->size();
  int nColumns = nItems + myThetaLen-1 + myEtaLen-1 
    - (table->findi(KeyStr.OMEGA) == Symbol::empty()? 0 : 1 )
    - (table->findi(KeyStr.SIGMA) == Symbol::empty()? 0 : 1 )
    - (table->findi(KeyStr.EPS)   == Symbol::empty()? 0 : 1 );

  map<const string, Symbol>::const_iterator pEntry = t->begin();
  const vector<string>::const_iterator pLabelBegin = table->getLabels()->begin();
  const vector<string>::const_iterator pLabelEnd   = table->getLabels()->end();
  vector<string> whatGoesIn;  // will hold those labels in the order that actually go into the data section.
  vector<string>::const_iterator pWhatGoesIn;
  string keyWhatGoesIn;

  oDriver << "oResults << \"<presentation_data rows=\\\"\" << nY << \"\\\" \";" << endl;
  oDriver << "oResults << \"columns=\\\"" << nColumns << "\\\">\" << endl;" << endl;
  oDriver << "oResults << \"<data_labels>\" << endl;" << endl;

  // Put ID first in the sequence
  whatGoesIn.push_back( pID->name );
  oDriver << "oResults << \"<label name=\\\"" << UserStr.ID << "\\\"/>\" << endl;" << endl;
  oDriver << endl;

  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << "//  Data Set Specific" << endl;
  // ...aaand, following ID is, all the left hand side quantities in the model definition.
  // cntColumns is initialized to 1 because the ID column is already printed out.
  int cntColumns = 1;
  for( cntColumns = 1, pEntry = t->begin(); pEntry!=t->end(); pEntry++ )
    {
      if( pEntry->first != KeyStr.ID 
	  /* && ( find( pLabelBegin, pLabelEnd, pEntry->second.name )==pLabelEnd ) */ )
	{
	  // These ones are not stored by Pred::eval() or the data set.
	  if( pEntry->first != KeyStr.OMEGA && pEntry->first != KeyStr.SIGMA )
	    {
	      whatGoesIn.push_back( pEntry->second.name );
	      
	      if( pEntry->first == KeyStr.THETA )
		{
		  for( int cntTheta=0; cntTheta<myThetaLen; cntTheta++ )
		    {
		      oDriver << "oResults << \"<label name=\\\"";
		      oDriver << pEntry->second.name << "(" << cntTheta+1 << ")";
		      oDriver << "\\\"/>\" << endl;" << endl;	
		      cntColumns++;
		    }
		}
	      else if( pEntry->first == KeyStr.ETA )
		{
		  for( int cntEta=0; cntEta<myEtaLen; cntEta++ )
		    {
		      oDriver << "oResults << \"<label name=\\\"";
		      oDriver << pEntry->second.name << "(" << cntEta+1 << ")";
		      oDriver << "\\\"/>\" << endl;" << endl;		      
		      cntColumns++;
		    }
		}
	      else
		{
		  oDriver << "oResults << \"<label name=\\\"";
		  oDriver << pEntry->second.name;
		  oDriver << "\\\"/>\" << endl;" << endl;
		  cntColumns++;
		}
	    }
	}
    }
  if( cntColumns != nColumns )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "The number of data items, %d, does not the number of labels, %d.",
	       cntColumns, nColumns );
      SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  oDriver << "//" << endl;
  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << "oResults << \"</data_labels>\" << endl;" << endl;

  oDriver << "for( int j=0, cnt=1; j<nY; j++, cnt++ )" << endl;
  oDriver << "{" << endl;
  oDriver << "   ///////////////////////////////////////////////////////////////////" << endl;
  oDriver << "   //" << endl;
  oDriver << "   oResults << \"<row position=\\\"\" << cnt << \"\\\">\" << endl;" << endl;

  for( cntColumns=0, pWhatGoesIn = whatGoesIn.begin(); pWhatGoesIn!=whatGoesIn.end(); pWhatGoesIn++ )
    {
      keyWhatGoesIn = SymbolTable::key( *pWhatGoesIn );
      if( keyWhatGoesIn == KeyStr.SIMDV )
	{
	  oDriver << "   oResults << \"<value ref=\\\"" << *pWhatGoesIn << "\\\"" << ">\" << ";
	  oDriver << "yOut[cnt]";
	  oDriver << " << \"</value>\" << endl;" << endl;
	  cntColumns++;
	}
      else if( keyWhatGoesIn == KeyStr.THETA )
	{
	  for( int cntTheta=0; cntTheta<myThetaLen; cntTheta++ )
	    {
	      oDriver << "   oResults << \"<value ref=\\\"";
	      oDriver << *pWhatGoesIn << "(" << cntTheta+1 << ")"<< "\\\"" << ">\" << ";
	      oDriver << "set.data[0]->" << *pWhatGoesIn << "[j][" << cntTheta << "]";
	      oDriver << " << \"</value>\" << endl;" << endl;
	      cntColumns++;
	    }
	}
      else if( keyWhatGoesIn == KeyStr.ETA )
	{
	  for( int cntEta=0; cntEta<myEtaLen; cntEta++ )
	    {
	      oDriver << "   oResults << \"<value ref=\\\"";
	      oDriver << *pWhatGoesIn << "(" << cntEta+1 << ")"<< "\\\"" << ">\" << ";
	      oDriver << "set.data[0]->" << *pWhatGoesIn << "[j][" << cntEta << "]";
	      oDriver << " << \"</value>\" << endl;" << endl;
	      cntColumns++;
	    }
	}
      else
	{
	  oDriver << "   oResults << \"<value ref=\\\"" << *pWhatGoesIn << "\\\"" << ">\" << ";
	  oDriver << "set.data[0]->" << *pWhatGoesIn << "[j]";
	  oDriver << " << \"</value>\" << endl;" << endl;
	  cntColumns++;
	}
    }
  if( cntColumns != nColumns )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "The number of data items, %d, does not the number of labels, %d.",
	       cntColumns, nColumns );
      SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  oDriver << "   oResults << \"</row>\" << endl;" << endl;
  oDriver << "   //" << endl;
  oDriver << "   ///////////////////////////////////////////////////////////////////" << endl;
  oDriver << "}" << endl;

  oDriver << "oResults << \"</presentation_data>\" << endl;" << endl;

  //
  //=============================================================================

  oDriver << "oResults << \"</spkreport>\" << endl;" << endl;

  oDriver << "oResults.close();" << endl;
  oDriver << "if( !haveCompleteData || !isStatSuccess )" << endl;
  oDriver << "   return OTHER_FAILURE;" << endl;
  oDriver << "if( !isOptSuccess )" << endl;
  oDriver << "   return CONVERGENCE_FAILURE;" << endl;
  oDriver << "return SUCCESS;" << endl;
  oDriver << "}" << endl;
  oDriver.close();
}

void NonmemTranslator::generatePopDriver() const
{
  //==================================================================
  // Generate the driver
  //==================================================================
  ofstream oDriver ( fFitDriver_cpp );
  if( !oDriver.good() )
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
  
  oDriver << "// " << myDescription << endl;

  oDriver << "#include <iostream>" << endl;
  oDriver << "#include <fstream>" << endl;
  oDriver << "#include <sys/time.h>" << endl;
  oDriver << endl;

  oDriver << "#include <spk/SpkValarray.h>" << endl;
  oDriver << "#include <spk/SpkException.h>" << endl;
  oDriver << "#include <spk/WarningsManager.h>" << endl;
  oDriver << "#include <CppAD/CppAD.h>" << endl;

  if( myIsEstimate )
    {
      oDriver << "#include <spk/fitPopulation.h>" << endl;
      oDriver << "#include <spk/Optimizer.h>" << endl;
    }
  if( myIsStat )
    {
      oDriver << "#include <spk/derParStatistics.h>" << endl;
      oDriver << "#include <spk/popStatistics.h>" << endl;
      oDriver << "#include <spk/inverse.h>" << endl;
    }
  if( myIsSimulate )
    oDriver << "#include <spk/simulate.h>" << endl;
  oDriver << "#include \"IndData.h\"" << endl;
  oDriver << "#include \"DataSet.h\"" << endl;
  oDriver << endl;

  oDriver << "#include <spk/multiply.h>" << endl;
  oDriver << "#include <spk/cholesky.h>" << endl;

  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << "//   NONMEM PRED specific" << endl;
  oDriver << "#include \"Pred.h\"" << endl;
  oDriver << "#include <spkpred/PopPredModel.h>" << endl;
  oDriver << "#include \"NonmemPars.h\"" << endl;
  oDriver << "//" << endl;
  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << endl;

  oDriver << "using SPK_VA::valarray;" << endl;
  oDriver << "using namespace std;" << endl;
  oDriver << endl;

  oDriver << "namespace{" << endl;
  oDriver << "const vector<CppAD::AD<double> > wres( int n," << endl;
  oDriver << "                                       const valarray<double> & Ri," << endl;
  oDriver << "                                       const vector  < CppAD::AD<double> > & residual )" << endl;
  oDriver << "{" << endl;
  oDriver << "   assert( Ri.size() == n * n );" << endl;
  oDriver << "   assert( residual.size() == n );" << endl;
  oDriver << "   valarray<double> r( n );" << endl;
  oDriver << "   for( int i=0; i<n; i++ ) r[i] = CppAD::Value( residual[i] );" << endl;
  oDriver << "   valarray<double> C( 0.0, n * n );" << endl;
  oDriver << "   C = cholesky( Ri, n );" << endl;
  oDriver << "   valarray<double> w = multiply( C, n, r, 1 );" << endl;
  oDriver << "   vector< CppAD::AD<double> > Cr(n);" << endl;
  oDriver << "   for( int i=0; i<n; i++ ) Cr[i] = w[i];" << endl;
  oDriver << "   return Cr;" << endl;
  oDriver << "}" << endl;
  oDriver << endl;
  oDriver << "};" << endl;

  oDriver << "enum RETURN_CODE { SUCCESS=0, CONVERGENCE_FAILURE=1, FILE_ACCESS_FAILURE=2, OTHER_FAILURE };" << endl;
  oDriver << endl;

  oDriver << "int main( int argc, const char argv[] )" << endl;
  oDriver << "{" << endl;

  oDriver << "ofstream oRuntimeError( \"" << fSpkRuntimeError_tmp << "\" );" << endl;
  oDriver << "if( !oRuntimeError.good() )" << endl;
  oDriver << "{" << endl;
  oDriver << "      fprintf( stderr, \"%s:%d: Failed to create a temporary file, %s.\", ";
  oDriver << " __FILE__, __LINE__, \"" << fSpkRuntimeError_tmp << "\" );" << endl;
  oDriver << "      return FILE_ACCESS_FAILURE;" << endl;
  oDriver << "}" << endl;
  oDriver << endl;

  oDriver << endl;

  oDriver << "const int nPop = " << myPopSize << ";" << endl;
  oDriver << "DataSet< CppAD::AD<double> > set;" << endl;
  oDriver << endl;

  oDriver << "const bool isSimRequested  = " << (myIsSimulate? "true":"false") << ";" << endl;
  oDriver << "bool haveCompleteData      = false;" << endl;
  oDriver << "const bool isSimOnly       = " << (myIsOnlySimulation? "true":"false") << ";" << endl;
  oDriver << endl;

  oDriver << "const bool isOptRequested  = " << (myIsEstimate? "true":"false") << ";" << endl;
  oDriver << "bool isOptSuccess          = " << (myIsEstimate? "false":"true") << ";" << endl;
  if( myIsEstimate )
    {
      oDriver << "Objective objective    = ";
      if( myApproximation == FO )
	oDriver << "FIRST_ORDER;" << endl;
      else if( myApproximation == FOCE )
	oDriver << "EXPECTED_HESSIAN;" << endl;
      else
	oDriver << "MODIFIED_LAPLACE;" << endl;
    }

  oDriver << "const bool isStatRequested = " << (myIsStat? "true":"false") << ";" << endl;
  oDriver << "bool isStatSuccess         = " << (myIsStat? "false":"true") << ";" << endl;
  if( myIsStat )
    oDriver << "enum PopCovForm covForm  = " << myCovForm << ";" << endl;
  oDriver << endl;

  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << "// NONMEM Sepcific" << endl;
  // theta
  oDriver << "valarray<double> thetaOut( NonmemPars::nTheta );" << endl;
  oDriver << endl;

  // Omega
  oDriver << "valarray<double> omegaOut( NonmemPars::omegaOrder );" << endl;
  oDriver << endl;
  
  // Sigma
  oDriver << "valarray<double> sigmaOut( NonmemPars::sigmaOrder );" << endl;
  oDriver << endl;


  oDriver << "valarray<double> etaOut( NonmemPars::nEta );" << endl;
  oDriver << "valarray<double> etaAllOut( NonmemPars::nEta * nPop );" << endl;
  oDriver << endl;
  oDriver << "//" << endl;
  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << endl;

  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << "//   NONMEM PRED Specific" << endl;
  oDriver << "Pred< CppAD::AD<double> > mPred(&set);" << endl;

  oDriver << "PopPredModel model( mPred," << endl;
  oDriver << "                    NonmemPars::nTheta," << endl;
  oDriver << "                    NonmemPars::thetaLow," << endl;
  oDriver << "                    NonmemPars::thetaUp," << endl;
  oDriver << "                    NonmemPars::thetaIn," << endl;
  oDriver << "                    NonmemPars::nEta," << endl;
  oDriver << "                    NonmemPars::etaIn," << endl;
  oDriver << "                    NonmemPars::nEps," << endl;
  oDriver << "                    NonmemPars::omegaStruct," << endl;
  oDriver << "                    NonmemPars::omegaIn," << endl;
  oDriver << "                    NonmemPars::sigmaStruct," << endl;
  oDriver << "                    NonmemPars::sigmaIn );" << endl;
  oDriver << "//" << endl;
  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << endl;

  oDriver << "const int nAlp = model.getNPopPar();" << endl;
  oDriver << "const int nB   = model.getNIndPar();" << endl;
  oDriver << endl;
   
  oDriver << "valarray<double> alpIn  ( nAlp );" << endl;
  oDriver << "valarray<double> alpUp  ( nAlp );" << endl;
  oDriver << "valarray<double> alpLow ( nAlp );" << endl;
  oDriver << "valarray<double> alpStep( nAlp );" << endl;
  oDriver << "valarray<double> alpOut ( nAlp );" << endl;
  if( myIsEstimate )
    {
      oDriver << "double           alpObjOut;" << endl;
      oDriver << "valarray<double> alpObj_alpOut    ( nAlp );" << endl;
      oDriver << "valarray<double> alpObj_alp_alpOut( nAlp * nAlp );" << endl;
    }
  oDriver << "model.getPopPar         ( alpIn );" << endl;
  oDriver << "model.getPopParLimits   ( alpLow, alpUp );" << endl;
  oDriver << "model.getPopParStep     ( alpStep );" << endl;
  oDriver << endl;

  oDriver << "valarray<double> bIn  ( nB * nPop );" << endl;
  oDriver << "valarray<double> biIn ( nB );" << endl;
  oDriver << "valarray<double> bUp  ( nB );" << endl;
  oDriver << "valarray<double> bLow ( nB );" << endl;
  if( myIsEstimate )
    {
      oDriver << "valarray<double> bStep( nB );" << endl;
    }
  oDriver << "valarray<double> bOut ( nB * nPop );" << endl;
  oDriver << "for( int i=0; i<nPop; i++ )" << endl;
  oDriver << "{" << endl;
  oDriver << "   model.selectIndividual( i ); " << endl;
  oDriver << "   model.getIndPar( biIn );" << endl;
  oDriver << "   bIn[ slice(i*nB, nB, 1) ] = biIn;" << endl;
  oDriver << "}" << endl;
  oDriver << "model.getIndParLimits ( bLow, bUp );" << endl;
  if( myIsEstimate )
    {
      oDriver << "model.getIndParStep   ( bStep );" << endl;
    }
  oDriver << endl;

  if( myIsEstimate )
    {
      oDriver << "const double popEps   = " << myPopEpsilon    << ";" << endl;
      oDriver << "const int    popMitr  = " << myPopMitr       << ";" << endl;
      oDriver << "const int    popTrace = " << myPopTraceLevel << ";" << endl;
      oDriver << "Optimizer    popOpt( popEps, popMitr, popTrace );" << endl;
      oDriver << endl;
      oDriver << "const double indEps   = " << myIndEpsilon    << ";" << endl;
      oDriver << "const int    indMitr  = " << myIndMitr       << ";" << endl;
      oDriver << "const int    indTrace = " << myIndTraceLevel << ";" << endl;
      oDriver << "Optimizer    indOpt( indEps, indMitr, indTrace );" << endl;
      oDriver << endl;
    }

  oDriver << "int c_N[nPop] = { ";
  for( int i=0; i<myPopSize; i++ )
    {
      if( i>0 )
	oDriver << ", ";
      oDriver << myRecordNums[i];
    }
  oDriver << " };" << endl;
  oDriver << "const valarray<int> N( c_N, nPop );" << endl; 
  oDriver << "const int nY = N.sum();" << endl; // total number of measurments" << endl;
  oDriver << endl;

  // do data simulation first to replace DV data in IndData objects
  oDriver << "/*******************************************************************/" << endl;
  oDriver << "/*                                                                 */" << endl;
  oDriver << "/*   Data Initialization                                           */" << endl;
  oDriver << "/*                                                                 */" << endl;
  oDriver << "/*******************************************************************/" << endl;
  if( myIsSimulate )
    {
      oDriver << "valarray<double> y   ( nY );" << endl;
      oDriver << "valarray<double> yOut( nY );" << endl;
      oDriver << "try" << endl;
      oDriver << "{" << endl;
      oDriver << "   simulate( model, alpIn, N, bLow, bUp, yOut, bOut, NonmemPars::seed );" << endl;
      //      if( myIsEstimate )
      {
	oDriver << "   bIn = bOut;" << endl;
	oDriver << "   for( int i=0, cnt=0; i<nPop; i++ )" << endl;
	oDriver << "   {" << endl;
	oDriver << "      for( int j=0; j<N[i]; j++ )" << endl;
	oDriver << "      {" << endl;
	oDriver << "         set.data[i]->SIMDV[j] = yOut[cnt];" << endl;
	oDriver << "      }" << endl;
	oDriver << "      cnt+=N[i];" << endl;
	oDriver << "   }" << endl;
	oDriver << "   y   = yOut;" << endl;
      }
      oDriver << "   haveCompleteData = true;" << endl;
      oDriver << "}" << endl;
      oDriver << "catch( SpkException& e )" << endl;
      oDriver << "{" << endl;
      oDriver << "   char mess[ SpkError::maxMessageLen() ];" << endl;
      oDriver << "   sprintf( mess, \"Failed in data simulation.\\n\" );" << endl;
      oDriver << "   e.push( SpkError::SPK_SIMULATION_ERR, mess, __LINE__, __FILE__ );" << endl;
      oDriver << "   oRuntimeError << e << endl;  // Printing out to a file." << endl;
      oDriver << "   cerr << e << endl;    // Printing out to the standard error." << endl; 
      oDriver << "   haveCompleteData = false;" << endl;
      oDriver << "}" << endl;
      oDriver << "catch( ... )" << endl;
      oDriver << "{" << endl;
      oDriver << "   char message[] =\"Unknown exception: failed in data simulation!!!\";" << endl;
      oDriver << "   oRuntimeError << message << endl;  // Printing out to a file." << endl;
      oDriver << "   cerr << message << endl;    // Printing out to the standard error." << endl;
      oDriver << "   haveCompleteData = false;" << endl;
      oDriver << "}" << endl;

      // RETURN HERE!!! 6/23/04
      if( myIsOnlySimulation )
	{
	  oDriver << endl;
	  oDriver << "model.setPopPar( alpIn );" << endl;
	  oDriver << "for( int i=0, cnt=0; i<nPop; i++ )" << endl;
          oDriver << "{" << endl;
	  oDriver << "   model.selectIndividual(i);" << endl;
	  oDriver << "   model.setIndPar( bIn[ slice( cnt, N[i], 1 ) ] );" << endl;
          oDriver << "   valarray<double> tmp_fiOut( N[i] );" << endl;
          oDriver << "   valarray<double> tmp_RiOut( N[i] * N[i] );" << endl;
          oDriver << "   valarray<double> tmp_DOut( nB * nB );" << endl; 
          oDriver << "   model.dataMean( tmp_fiOut );" << endl;
          oDriver << "   model.dataVariance( tmp_RiOut );" << endl;
          oDriver << "   model.indParVariance( tmp_DOut );" << endl;
          oDriver << "}" << endl;
          oDriver << "set.data[i]->" << UserStr.WRES << " = 0.0;" << endl;
	}
    }
  else
    {
      oDriver << "valarray<double> y = set.getAllMeasurements();" << endl;
      oDriver << "haveCompleteData = true;" << endl;
      oDriver << endl;
    }

  if( myIsEstimate )
    {

      oDriver << "/*******************************************************************/" << endl;
      oDriver << "/*                                                                 */" << endl;
      oDriver << "/*   Parameter Estimation                                          */" << endl;
      oDriver << "/*                                                                 */" << endl;
      oDriver << "/*******************************************************************/" << endl;
      oDriver << "timeval optBegin, optEnd;" << endl;
      oDriver << "double optTimeSec = 0.0;" << endl;

      oDriver << "if( isOptRequested && haveCompleteData )" << endl;
      oDriver << "{" << endl;

      oDriver << "   gettimeofday( &optBegin, NULL );" << endl;
      oDriver << "   try" << endl;
      oDriver << "   {" << endl;
      oDriver << "      fitPopulation( model," << endl;
      oDriver << "                     objective, " << endl;
      oDriver << "                     N," << endl;
      oDriver << "                     y," << endl;
      oDriver << "                     popOpt," << endl;
      oDriver << "                     alpLow," << endl;
      oDriver << "                     alpUp," << endl;
      oDriver << "                     alpIn," << endl;
      oDriver << "                     alpStep," << endl;
      oDriver << "                    &alpOut," << endl;
      oDriver << "                     indOpt," << endl;
      oDriver << "                     bLow," << endl;
      oDriver << "                     bUp," << endl;
      oDriver << "                     bIn," << endl;
      oDriver << "                     bStep," << endl;
      oDriver << "                    &bOut," << endl;
      oDriver << "                    &alpObjOut," << endl;
      oDriver << "                    &alpObj_alpOut," << endl;
      oDriver << "                    &alpObj_alp_alpOut );" << endl;
      oDriver << "      isOptSuccess = true;" << endl;
      oDriver << "   }" << endl;
      oDriver << "   catch( SpkException& e )" << endl;
      oDriver << "   {" << endl;
      oDriver << "      char mess[ SpkError::maxMessageLen() ];" << endl;
      oDriver << "      sprintf( mess, \"Failed in population parameter estimation.\\n\" );" << endl;
      oDriver << "      e.push( SpkError::SPK_OPT_ERR, mess, __LINE__, __FILE__ );" << endl;
      oDriver << "      oRuntimeError << e << endl;" << endl;
      oDriver << "      cerr << e << endl;" << endl;
      oDriver << "      isOptSuccess = false;" << endl;
      oDriver << "   }" << endl;
      oDriver << "   catch( ... )" << endl;
      oDriver << "   {" << endl;
      oDriver << "      char message[] = \"Unknown exception: failed in parameter estimation!!!\";" << endl;
      oDriver << "      oRuntimeError << message << endl;" << endl;
      oDriver << "      cerr << message << endl;" << endl;
      oDriver << "      isOptSuccess = false;" << endl;
      oDriver << "   }" << endl;
      oDriver << "   gettimeofday( &optEnd, NULL );" << endl;
      oDriver << "   optTimeSec = difftime( optEnd.tv_sec, optBegin.tv_sec );" << endl;
      oDriver << endl;

      oDriver << "   ///////////////////////////////////////////////////////////////////" << endl;
      oDriver << "   //   NONMEM Specific" << endl;
      oDriver << "   if( isOptSuccess || !isOptSuccess )" << endl;
      oDriver << "   {" << endl;
      oDriver << "      model.getTheta( thetaOut );" << endl;
      oDriver << "      model.getOmega( omegaOut );" << endl;
      oDriver << "      model.getSigma( sigmaOut );" << endl;
      oDriver << "   }" << endl;
      oDriver << "   //" << endl;
      oDriver << "   ///////////////////////////////////////////////////////////////////" << endl;      
      oDriver << "}" << endl;

      // Statistics can be only computed when the parameter estimation has been done.
      if( myIsStat )
	{
	  oDriver << "/*******************************************************************/" << endl;
	  oDriver << "/*                                                                 */" << endl;
	  oDriver << "/*   Statistics                                                    */" << endl;
	  oDriver << "/*                                                                 */" << endl;
	  oDriver << "/*******************************************************************/" << endl;
	  oDriver << "timeval statBegin, statEnd;" << endl;
	  oDriver << "double statTimeSec = 0.0;" << endl;
          oDriver << "valarray<double> alpCov( nAlp * nAlp );" << endl;
          oDriver << "valarray<double> stdPar( nAlp );" << endl;
          oDriver << "valarray<double> stdPar_alp( nAlp * nAlp );" << endl;
          oDriver << "const int nDegOfFreedom = nY - nAlp;" << endl;
	  if( myIsCov || myIsInvCov )
	    oDriver << "valarray<double> stdParCovOut( nAlp * nAlp );" << endl;
	  if( myIsStderr )
	    oDriver << "valarray<double> stdParSEOut( nAlp );" << endl;
	  if( myIsCorrelation )
	    oDriver << "valarray<double> stdParCorrelationOut( nAlp * nAlp );" << endl;
	  if( myIsCoefficient )
	    oDriver << "valarray<double> stdParCoefficientOut( nAlp );" << endl;
	  if( myIsConfidence )
	    oDriver << "valarray<double> stdParConfidenceOut( 2 * nAlp );" << endl;
	  if( myIsInvCov )
	    oDriver << "valarray<double> stdParInvCovOut( nAlp * nAlp );" << endl;
	  
	  oDriver << "if( isStatRequested && haveCompleteData && isOptSuccess )" << endl;
	  oDriver << "{" << endl;

	  oDriver << "   gettimeofday( &statBegin, NULL );" << endl;
	  oDriver << "   try" << endl;
	  oDriver << "   {" << endl;
	  oDriver << "      model.getStandardPar( stdPar );" << endl;
          oDriver << "      model.getStandardPar_popPar( stdPar_alp );" << endl;
	  oDriver << "      popStatistics( model, " << endl;
	  oDriver << "                     objective," << endl;
	  oDriver << "                     N," << endl;
	  oDriver << "                     y," << endl;
	  oDriver << "                     alpOut, " << endl;
	  oDriver << "                     alpObj_alp_alpOut, " << endl;
	  oDriver << "                     bOut," << endl;
	  oDriver << "                     bLow," << endl;
	  oDriver << "                     bUp," << endl;
	  oDriver << "                     bStep," << endl;
	  oDriver << "                     covForm," << endl;
	  oDriver << "                    &alpCov, " << endl;
	  oDriver << "                     NULL, " << endl;
	  oDriver << "                     NULL, " << endl;
	  oDriver << "                     NULL, " << endl;
	  oDriver << "                     NULL );" << endl;
          oDriver << "      derParStatistics( alpCov," << endl;
          oDriver << "                        stdPar," << endl;
          oDriver << "                        stdPar_alp," << endl;
          oDriver << "                        nDegOfFreedom," << endl;
          oDriver << "                        " << ( myIsCov || myIsInvCov? "&stdParCovOut"        : "NULL" ) << "," << endl;
          oDriver << "                        " << ( myIsStderr?            "&stdParSEOut"         : "NULL" ) << "," << endl;
          oDriver << "                        " << ( myIsCorrelation?       "&stdParCorrelationOut": "NULL" ) << "," << endl;
          oDriver << "                        " << ( myIsCoefficient?       "&stdParCoefficientOut": "NULL" ) << "," << endl;
          oDriver << "                        " << ( myIsConfidence?        "&stdParConfidenceOut" : "NULL" ) << endl;
          oDriver << "                      );" << endl;
	  oDriver << "      isStatSuccess = true;" << endl;
	  oDriver << "   }" << endl;
	  oDriver << "   catch( SpkException& e )" << endl;
	  oDriver << "   {" << endl;
	  oDriver << "      char mess[ SpkError::maxMessageLen() ];" << endl;
	  oDriver << "      sprintf( mess, \"Failed to compute statistics value(s).\\n\" );" << endl;
	  oDriver << "      e.push( SpkError::SPK_STATISTICS_ERR, mess, __LINE__, __FILE__ );" << endl;
	  oDriver << "      oRuntimeError << e << endl;" << endl;
	  oDriver << "      cerr << e << endl;" << endl;
	  oDriver << "      isStatSuccess = false;" << endl;
	  oDriver << "   }" << endl;
	  oDriver << "   catch( ... )" << endl;
	  oDriver << "   {" << endl;
	  oDriver << "      char message[] = \"Unknown exception: failed in statistics calculation!!!\";" << endl;
	  oDriver << "      oRuntimeError << message << endl;" << endl;
	  oDriver << "      cerr << message << endl;" << endl;
	  oDriver << "      isStatSuccess = false;" << endl;
	  oDriver << "   }" << endl;

	  if( myIsInvCov )
	    {
	      oDriver << "   try" << endl;
	      oDriver << "   {" << endl;
              oDriver << "      if( isStatSuccess )" << endl;
	      oDriver << "         stdParInvCovOut = inverse( stdParCovOut, nAlp );" << endl;
	      oDriver << "   }" << endl;
	      oDriver << "   catch( SpkException& e )" << endl;
	      oDriver << "   {" << endl;
	      oDriver << "      char mess[ SpkError::maxMessageLen() ];" << endl;
	      oDriver << "      sprintf( mess, \"Failed to invert the covariance of population parameters.\\n\" );" << endl;
	      oDriver << "      e.push( SpkError::SPK_NOT_INVERTABLE_ERR, mess, __LINE__, __FILE__ );" << endl;
	      oDriver << "      oRuntimeError << e << endl;" << endl;
	      oDriver << "      cerr << e << endl;" << endl;
	      oDriver << "      isStatSuccess = false;" << endl;
	      oDriver << "   }" << endl;
	      oDriver << "   catch( ... )" << endl;
	      oDriver << "   {" << endl;
	      oDriver << "      char message[] = \"Unknown exception: failed to invert the covariance of the population parameter!!!\";" << endl;
	      oDriver << "      oRuntimeError << message << endl;" << endl;
	      oDriver << "      cerr << message << endl;" << endl;
	      oDriver << "      isStatSuccess = false;" << endl;
	      oDriver << "   }" << endl;
	    }
	  oDriver << "   gettimeofday( &statEnd, NULL );" << endl;
	  oDriver << "   statTimeSec = difftime( statEnd.tv_sec, statBegin.tv_sec );" << endl;
	  oDriver << "}" << endl;
	}
    }
  oDriver << "oRuntimeError.close();" << endl;
  oDriver << endl;

  oDriver << "/*******************************************************************/" << endl;
  oDriver << "/*                                                                 */" << endl;
  oDriver << "/*   ReportML Document                                             */" << endl;
  oDriver << "/*                                                                 */" << endl;
  oDriver << "/*******************************************************************/" << endl;
  oDriver << "model.setPopPar( alpOut );" << endl;
  oDriver << "for( int i=0; i<nPop; i++ )" << endl;
  oDriver << "{" << endl;
  oDriver << "   valarray<double> RiOut( N[i] * N[i] );" << endl;
  oDriver << "   model.selectIndividual(i);" << endl;
  oDriver << "   model.setIndPar( bOut[ slice( i*nB, nB, 1 ) ] );" << endl;
  oDriver << "   model.dataVariance( RiOut );" << endl;
  oDriver << "   for( int j=0; j<N[i]; j++ )" << endl;
  oDriver << "   {" << endl;
  oDriver << "      set.data[i]->" << UserStr.RES << "[j] = y[j] - set.data[i]->" << UserStr.PRED << "[j] ;" << endl;
  oDriver << "   }" << endl;
  oDriver << "   set.data[i]->" << UserStr.WRES << " = wres( N[i], RiOut, set.data[i]->" << UserStr.RES << " ); " << endl;
  oDriver << "}" << endl;

  oDriver << "ofstream oResults( \"" << fResult_xml << "\" );" << endl;
  oDriver << "if( !oResults.good() )" << endl;
  oDriver << "{" << endl;
  oDriver << "   fprintf( stderr, \"Failed to open a file, %s !!!\", \"" << fResult_xml << "\" );" << endl;
  oDriver << "   return FILE_ACCESS_FAILURE;" << endl;
  oDriver << "}" << endl;

  oDriver << "oResults << \"<?xml version=\\\"1.0\\\"?>\" << endl;" << endl;
  oDriver << "oResults << \"<spkreport>\" << endl;" << endl;
  oDriver << endl;

  oDriver << "if( !(haveCompleteData && isOptSuccess && isStatSuccess) )" << endl;
  oDriver << "{" << endl;
  oDriver << "   char buf[ SpkError::maxMessageLen() ];" << endl;
  oDriver << "   ifstream iRuntimeError( \"" << fSpkRuntimeError_tmp << "\" );" << endl;
  oDriver << "   oResults << \"<error_message>\" << endl;" << endl;
  oDriver << "   while( iRuntimeError.getline(buf, SpkError::maxMessageLen()) )" << endl;
  oDriver << "   {" << endl;
  oDriver << "      oResults << buf << endl;" << endl;   // Write to the SpkReportML document.
  oDriver << "   }" << endl;
  oDriver << "   oResults << \"</error_message>\" << endl;" << endl;
  oDriver << "   iRuntimeError.close();" << endl;
  oDriver << "}" << endl;
  oDriver << "remove( \"" << fSpkRuntimeError_tmp << "\" );" << endl;
  oDriver << endl;

  oDriver << "if( !haveCompleteData )" << endl;
  oDriver << "{" << endl;
  oDriver << "   oResults << \"</spkreport>\" << endl;" << endl;
  oDriver << "   oResults.close();" << endl;
  oDriver << "   return OTHER_FAILURE;" << endl;
  oDriver << "}" << endl;
  oDriver << endl;

  oDriver << "oResults << \"<pop_analysis_result>\" << endl;" << endl;
  oDriver << endl;
  if( myIsEstimate )
    {
      oDriver << "oResults << \"<pop_opt_result elapsedtime=\\\"\" << optTimeSec << \"\\\">\" << endl;" << endl;
      oDriver << "oResults << \"<pop_obj_out>\" << endl;" << endl;
      oDriver << "oResults << \"<value>\" << alpObjOut << \"</value>\" << endl;" << endl;
      oDriver << "oResults << \"</pop_obj_out>\" << endl;" << endl;
      oDriver << endl;

      oDriver << "///////////////////////////////////////////////////////////////////" << endl;
      oDriver << "//   NONMEM Specific" << endl;
      oDriver << "oResults << \"<theta_out length=\\\"\" << NonmemPars::nTheta << \"\\\">\" << endl;" << endl;
      // theta
      oDriver << "for( int i=0; i<NonmemPars::nTheta; i++ )" << endl;
      oDriver << "{" << endl;
      oDriver << "   oResults << \"<value>\" << thetaOut[i] << \"</value>\" << endl;" << endl;
      oDriver << "}" << endl;
      oDriver << "oResults << \"</theta_out>\" << endl;" << endl;
      // Omega 
      oDriver << "oResults << \"<omega_out dimension=\\\"\" << NonmemPars::omegaDim << \"\\\" ";
      if( myOmegaStruct == Symbol::TRIANGLE )
	oDriver << "struct=\\\"block\\\">\" << endl;" << endl;
      else
	oDriver << "struct=\\\"diagonal\\\">\" << endl;" << endl;
      oDriver << "for( int i=0; i<NonmemPars::omegaOrder; i++ )" << endl;
      oDriver << "{" << endl;
      oDriver << "   oResults << \"<value>\" << omegaOut[i] << \"</value>\" << endl;" << endl;
      oDriver << "}" << endl;
      oDriver << "oResults << \"</omega_out>\" << endl;" << endl;
      // Sigma
      oDriver << "oResults << \"<sigma_out dimension=\\\"\" << NonmemPars::sigmaDim << \"\\\" ";
      if( mySigmaStruct == Symbol::TRIANGLE )
	oDriver << "struct=\\\"block\\\">\" << endl;" << endl;
      else
	oDriver << "struct=\\\"diagonal\\\">\" << endl;" << endl;
      oDriver << "for( int i=0; i<NonmemPars::sigmaOrder; i++ )" << endl;
      oDriver << "{" << endl;
      oDriver << "   oResults << \"<value>\" << sigmaOut[i] << \"</value>\" << endl;" << endl;
      oDriver << "}" << endl;
      oDriver << "oResults << \"</sigma_out>\" << endl;" << endl;
      oDriver << "//" << endl;
      oDriver << "///////////////////////////////////////////////////////////////////" << endl;
      oDriver << "oResults << \"</pop_opt_result>\" << endl;" << endl;
      oDriver << endl;

      if( myIsStat )
	{
	  oDriver << "oResults << \"<pop_stat_result elapsedtime=\\\"\" << statTimeSec << \"\\\">\" << endl;" << endl;
	  if( myIsCov )
	    {
	      oDriver << "oResults << \"<pop_covariance_out struct=\\\"block\\\" dimension=\\\"\" << nAlp << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nAlp; i++ )" << endl;
	      oDriver << "{" << endl;
              oDriver << "   for( int j=0; j<=i; j++ )" << endl;
              oDriver << "   {" << endl;
	      oDriver << "      oResults << \"   <value>\" << stdParCovOut[i+j*nAlp] << \"</value>\" << endl;" << endl;
              oDriver << "   }" << endl;
	      oDriver << "}" << endl;
	      
	      oDriver << "oResults << \"</pop_covariance_out>\" << endl;" << endl;
	    }
	  if( myIsInvCov )
	    {
	      oDriver << "oResults << \"<pop_inverse_covariance_out struct=\\\"block\\\" dimension=\\\"\" << nAlp << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nAlp; i++ )" << endl;
	      oDriver << "{" << endl;
              oDriver << "   for( int j=0; j<=i; j++ )" << endl;
              oDriver << "   {" << endl;
	      oDriver << "      oResults << \"   <value>\" << stdParInvCovOut[i+j*nAlp] << \"</value>\" << endl;" << endl;
              oDriver << "   }" << endl;
	      oDriver << "}" << endl;
	      
	      oDriver << "oResults << \"</pop_inverse_covariance_out>\" << endl;" << endl;
	    }
	  if( myIsStderr )
	    {
	      oDriver << "oResults << \"<pop_stderror_out length=\\\"\" << nAlp << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nAlp; i++ )" << endl;
	      oDriver << "{" << endl;
	      oDriver << "oResults << \"   <value>\" << stdParSEOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;
	      
	      oDriver << "oResults << \"</pop_stderror_out>\" << endl;" << endl;
	    }
	  if( myIsCorrelation )
	    {
	      oDriver << "oResults << \"<pop_correlation_out struct=\\\"block\\\" dimension=\\\"\" << nAlp << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nAlp; i++ )" << endl;
	      oDriver << "{" << endl;
              oDriver << "   for( int j=0; j<=i; j++ )" << endl;
              oDriver << "   {" << endl;
	      oDriver << "      oResults << \"   <value>\" << stdParCorrelationOut[i+j*nAlp] << \"</value>\" << endl;" << endl;
              oDriver << "   }" << endl;
	      oDriver << "}" << endl;
	      
	      oDriver << "oResults << \"</pop_correlation_out>\" << endl;" << endl;
	    }
	  if( myIsCoefficient )
	    {
	      oDriver << "oResults << \"<pop_coefficient_out length=\\\"\" << nAlp << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nAlp; i++ )" << endl;
	      oDriver << "{" << endl;
	      oDriver << "oResults << \"   <value>\" << stdParCoefficientOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;
	      
	      oDriver << "oResults << \"</pop_coefficient_out>\" << endl;" << endl;
	    }
	  if( myIsConfidence )
	    {
	      oDriver << "oResults << \"<pop_confidence_out length=\\\"\" << nAlp*2 << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nAlp*2; i++ )" << endl;
	      oDriver << "{" << endl;
	      oDriver << "oResults << \"   <value>\" << stdParConfidenceOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;
	      
	      oDriver << "oResults << \"</pop_confidence_out>\" << endl;" << endl;
	    }
	  oDriver << "oResults << \"</pop_stat_result>\" << endl;" << endl;
	  oDriver << endl;
	}
  
      oDriver << "oResults << \"</pop_analysis_result>\" << endl;" << endl;
    }
  oDriver << endl;


  //=============================================================================
  // LABELS
  //
  const map<const string, Symbol> * t = table->getTable();
  const Symbol * pID = table->findi(KeyStr.ID);
  if( pID == Symbol::empty() )
    {
      char mess [ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "\"ID\" is not defined." );
      SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  const int nItems = t->size();
  int nColumns = nItems + myThetaLen-1 + myEtaLen-1 + myEpsLen-1
    - (table->findi(KeyStr.OMEGA) == Symbol::empty()? 0 : 1 )
    - (table->findi(KeyStr.SIGMA) == Symbol::empty()? 0 : 1 );

  map<const string, Symbol>::const_iterator pEntry = t->begin();
  const vector<string>::const_iterator pLabelBegin = table->getLabels()->begin();
  const vector<string>::const_iterator pLabelEnd   = table->getLabels()->end();
  vector<string> whatGoesIn;  // will hold those labels in the order that actually go into the data section.
  vector<string>::const_iterator pWhatGoesIn;
  string keyWhatGoesIn;

  oDriver << "oResults << \"<presentation_data rows=\\\"\" << N.sum() << \"\\\" \";" << endl;
  oDriver << "oResults << \"columns=\\\"" << nColumns << "\\\">\" << endl;" << endl;
  oDriver << "oResults << \"<data_labels>\" << endl;" << endl;
  
  // Put ID first in the sequence
  whatGoesIn.push_back( pID->name );
  oDriver << "oResults << \"<label name=\\\"" << pID->name << "\\\"/>\" << endl;" << endl;
  
  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << "//   DATA SET Specific" << endl;

  // ...aaand, following ID is, all the left hand side quantities in the model definition.
  // cntColumns is initialized to 1 because the ID column is already printed out.
  int cntColumns = 1;
  for( cntColumns=1,  pEntry = t->begin(); pEntry!=t->end(); pEntry++ )
    {
      if( pEntry->first != KeyStr.ID 
	  /*&& ( find( pLabelBegin, pLabelEnd, pEntry->second.name )==pLabelEnd )*/ )
	{
	  // these three --- theta, omega and sigma --- don't get saved within Pred::eval()
	  // and are already printed out in the reportML earlier during this step.
	  if( pEntry->first != KeyStr.OMEGA && pEntry->first != KeyStr.SIGMA )
	    {
	      whatGoesIn.push_back( pEntry->second.name );
	      
	      if( pEntry->first == KeyStr.THETA )
		{
		  for( int cntTheta=0; cntTheta<myThetaLen; cntTheta++ )
		    {
		      oDriver << "oResults << \"<label name=\\\"";
		      oDriver << pEntry->second.name << "(" << cntTheta+1 << ")";
		      oDriver << "\\\"/>\" << endl;" << endl;		      
		      cntColumns++;
		    }
		}
	      else if( pEntry->first == KeyStr.ETA )
		{
		  for( int cntEta=0; cntEta<myEtaLen; cntEta++ )
		    {
		      oDriver << "oResults << \"<label name=\\\"";
		      oDriver << pEntry->second.name << "(" << cntEta+1 << ")";
		      oDriver << "\\\"/>\" << endl;" << endl;		      
		      cntColumns++;
		    }
		}
	      else if( pEntry->first == KeyStr.EPS )
		{
		  for( int cntEps=0; cntEps<myEpsLen; cntEps++ )
		    {
		      oDriver << "oResults << \"<label name=\\\"";
		      oDriver << pEntry->second.name << "(" << cntEps+1 << ")";
		      oDriver << "\\\"/>\" << endl;" << endl;		      
		      cntColumns++;
		    }
		}
	      else
		{
		  oDriver << "oResults << \"<label name=\\\"";
		  oDriver << pEntry->second.name;
		  oDriver << "\\\"/>\" << endl;" << endl;
		  cntColumns++;
		}
	    }
	}
    }
  if( cntColumns != nColumns )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "The number of data items, %d, does not the number of labels, %d.",
	       cntColumns, nColumns );
      SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  oDriver << "//"  << endl;
  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << "oResults << \"</data_labels>\" << endl;" << endl;
  oDriver << endl;
  
  oDriver << "for( int i=0, position=1; i<nPop; i++ )" << endl;
  oDriver << "{" << endl;
  oDriver << "   for( int j=0; j<N[i]; j++, position++ )" << endl;
  oDriver << "   {" << endl;
  oDriver << "      oResults << \"<row position=\\\"\" << position << \"\\\">\" << endl;" << endl;
  oDriver << "      ///////////////////////////////////////////////////////////////////" << endl;
  oDriver << "      //   DATA SET Specific" << endl;

  for( cntColumns=0, pWhatGoesIn = whatGoesIn.begin(); pWhatGoesIn!=whatGoesIn.end(); pWhatGoesIn++ )
    {
      keyWhatGoesIn = SymbolTable::key( *pWhatGoesIn );
      if( keyWhatGoesIn == KeyStr.SIMDV )
	{
	  oDriver << "   oResults << \"<value ref=\\\"" << *pWhatGoesIn << "\\\"" << ">\" << ";
	  oDriver << "yOut[position]";
	  oDriver << " << \"</value>\" << endl;" << endl;
	  cntColumns++;
	}
      else if( keyWhatGoesIn == KeyStr.THETA )
	{
	  for( int cntTheta=0; cntTheta<myThetaLen; cntTheta++ )
	    {
	      oDriver << "   oResults << \"<value ref=\\\"";
	      oDriver << *pWhatGoesIn << "(" << cntTheta+1 << ")" << "\\\"" << ">\" << ";
	      oDriver << "set.data[i]->" << *pWhatGoesIn << "[j][" << cntTheta << "]";
	      oDriver << " << \"</value>\" << endl;" << endl;
	      cntColumns++;
	    }
	}
      else if( keyWhatGoesIn == KeyStr.ETA )
	{
	  for( int cntEta=0; cntEta<myEtaLen; cntEta++ )
	    {
	      oDriver << "   oResults << \"<value ref=\\\"";
	      oDriver << *pWhatGoesIn << "(" << cntEta+1 << ")"<< "\\\"" << ">\" << ";
	      oDriver << "set.data[i]->" << *pWhatGoesIn << "[j][" << cntEta << "]";
	      oDriver << " << \"</value>\" << endl;" << endl;
	      cntColumns++;
	    }
	}
      else if( keyWhatGoesIn == KeyStr.EPS )
	{
	  for( int cntEps=0; cntEps<myEpsLen; cntEps++ )
	    {
	      oDriver << "   oResults << \"<value ref=\\\"";
	      oDriver << *pWhatGoesIn << "(" << cntEps+1 << ")"<< "\\\"" << ">\" << ";
	      oDriver << "set.data[i]->" << *pWhatGoesIn << "[j][" << cntEps << "]";
	      oDriver << " << \"</value>\" << endl;" << endl;
	      cntColumns++;
	    }
	}
      else if( keyWhatGoesIn == KeyStr.OMEGA || keyWhatGoesIn == KeyStr.SIGMA )
	{
	  // ignore
	}
      else
	{
	  oDriver << "   oResults << \"<value ref=\\\"" << *pWhatGoesIn << "\\\"" << ">\" << ";
	  oDriver << "set.data[i]->" << *pWhatGoesIn << "[j]";
	  oDriver << " << \"</value>\" << endl;" << endl;
	  cntColumns++;
	}
    }
  if( cntColumns != nColumns )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "The number of data items, %d, does not the number of labels, %d.",
	       cntColumns, nColumns );
      SpkCompilerException e( SpkCompilerError::ASPK_PROGRAMMER_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }
  oDriver << "      //" << endl;
  oDriver << "      ///////////////////////////////////////////////////////////////////" << endl;
  oDriver << "      oResults << \"</row>\" << endl;" << endl;
  oDriver << "   }" << endl;
  oDriver << "}" << endl;
  
  oDriver << "oResults << \"</presentation_data>\" << endl;" << endl;
  //
  //=============================================================================
  
  oDriver << "oResults << \"</spkreport>\" << endl;" << endl;
  
  oDriver << "oResults.close();" << endl;
  oDriver << "if( haveCompleteData && isOptSuccess && isStatSuccess )" << endl;
  oDriver << "   remove( \"" << fSpkRuntimeError_tmp << "\" );" << endl;
  oDriver << "if( !haveCompleteData || !isStatSuccess )" << endl;
  oDriver << "   return OTHER_FAILURE;" << endl;
  oDriver << "if( !isOptSuccess )" << endl;
  oDriver << "   return CONVERGENCE_FAILURE;" << endl;
  oDriver << "return SUCCESS;" << endl;
  oDriver << "}" << endl;
  oDriver.close();
}
