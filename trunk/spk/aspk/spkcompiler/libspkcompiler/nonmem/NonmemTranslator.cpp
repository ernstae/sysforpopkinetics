#include <fstream>

#include "NonmemTranslator.h"
#include "explang.h"
#include "../series.h"

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

//========================================
// Local namespace
//----------------------------------------
namespace{

  //
  // Approximation method
  //
  enum APPROX     { FO, FOCE, LAPLACE };
  enum TARGET     { IND, POP};
  enum MODEL_SPEC { PRED };

  //
  // Place holders to hold values/info gathered through parsings.
  // The reason why this is just locally defined, as opposed to
  // defined as a NonmemTranslator class member, is 
  // to hide the existence since it's just an object of convenience.
  //
  enum TARGET       myTarget        = POP;  
  enum MODEL_SPEC   myModelSpec     = PRED;

  bool              myIsEstimate    = true;
  bool              myIsSimulate    = false;
  bool              myIsStat        = false;
 
  bool              myIsOnlySimulation = false;
  unsigned int      mySubproblemsN  = 1; 
  APPROX            myApproximation = FO;
  unsigned int      myPopSize       = 1;
  
  bool              myIsEtaOut      = false;
  bool              myIsRestart     = true;
  unsigned int      myThetaLen      = 0;
  unsigned int      myOmegaDim      = 0;
  unsigned int      myOmegaElemNum  = 0;
  Symbol::Structure myOmegaStruct   = Symbol::TRIANGLE;
  unsigned int      mySigmaDim      = 0;
  int               mySigmaElemNum  = 0;
  Symbol::Structure mySigmaStruct   = Symbol::TRIANGLE;
  int               myEtaLen        = 0;
  int               myEpsLen        = 0;
    
  unsigned int      myPopMitr       = 100;
  unsigned int      myIndMitr       = 100;
  unsigned int      mySigDigits     = 3;
  double            myPopEpsilon    = pow( 10.0, -(mySigDigits+1.0) );
  double            myIndEpsilon    = pow( 10.0, -(mySigDigits+1.0) );
  int               myPopTraceLevel = 1;
  int               myIndTraceLevel = 1;
  unsigned int      mySeed = 0;

  string            myCovForm       = "R";
  bool              myIsStderr      = true;
  bool              myIsCorrelation = true;
  bool              myIsCov         = true;
  bool              myIsInvCov      = true;
  bool              myIsConfidence  = true;
  bool              myIsCoefficient = true;
  valarray<int>     myRecordNums;
};
namespace UserStr{
  string            ID;
  string            EPS;
  string            ETA;
  string            THETA;
  string            OMEGA;
  string            SIGMA;
  string            RES;
  string            WRES;
  string            PRED;
  string            DV;
  string            SIMDV;
  string            MDV;
  string            F;
  string            Y;
};
namespace DefaultStr{
  const string THETA = "THETA";
  const string ETA   = "ETA";
  const string EPS   = "EPS";
  const string OMEGA = "OMEGA";
  const string SIGMA = "SIGMA";
  const string RES   = "RES";
  const string WRES  = "WRES";
  const string PRED  = "PRED";
  const string DV    = "DV";
  const string MDV   = "MDV";
  const string ID    = "ID";
  const string SIMDV = "SIMDV";
  const string F     = "F";
  const string Y     = "Y";
};
namespace KeyStr{
  const string THETA = SymbolTable::key( DefaultStr::THETA );
  const string ETA   = SymbolTable::key( DefaultStr::ETA );
  const string EPS   = SymbolTable::key( DefaultStr::EPS );
  const string OMEGA = SymbolTable::key( DefaultStr::OMEGA );
  const string SIGMA = SymbolTable::key( DefaultStr::SIGMA );
  const string RES   = SymbolTable::key( DefaultStr::RES );
  const string WRES  = SymbolTable::key( DefaultStr::WRES );
  const string PRED  = SymbolTable::key( DefaultStr::PRED );
  const string DV    = SymbolTable::key( DefaultStr::DV );
  const string MDV   = SymbolTable::key( DefaultStr::MDV );
  const string ID    = SymbolTable::key( DefaultStr::ID );
  const string SIMDV = SymbolTable::key( DefaultStr::SIMDV );
  const string F     = SymbolTable::key( DefaultStr::F );
  const string Y     = SymbolTable::key( DefaultStr::Y );
};
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

NonmemTranslator::NonmemTranslator( DOMDocument* sourceIn, DOMDocument* dataIn )
  : ClientTranslator ( sourceIn, dataIn ),
    fMakefile        ( "generatedMakefile" ),
    fIndData_h       ( "IndData.h" ),
    fDataSet_h       ( "DataSet.h" ),
    fPredEqn_fortran ( "predEqn.fortran" ),
    fPredEqn_cpp     ( "predEqn.cpp" ),
    fPred_h          ( "Pred.h" ),
    fOmega_h         ( "Omega.h" ),
    fOmega_cpp       ( "Omega.cpp" ),
    fDriver_cpp      ( "driver.cpp" ),
    fSoftwareError_xml( "software_error.xml" ),
    fSpkRuntimeError_tmp( "spk_error.tmp" ),
    fResult_xml      ( "result.xml" ),
    BURNER           ( "// THIS FILE IS GENERATED BY THE ASPK COMPILER" )

{
  table = ClientTranslator::getSymbolTable();

  X_YES            = XMLString::transcode("yes");
  X_NO             = XMLString::transcode("no");
  X_FIXED          = XMLString::transcode("fixed");
  X_IN             = XMLString::transcode("in");
  X_LOW            = XMLString::transcode("low");
  X_UP             = XMLString::transcode("up");
  X_DIAGONAL       = XMLString::transcode("diagonal");
  X_BLOCK          = XMLString::transcode("block");
  X_VALUE          = XMLString::transcode("value");
  X_STRUCT         = XMLString::transcode("struct");
  X_DIMENSION      = XMLString::transcode("dimension");
  X_LABEL          = XMLString::transcode("label");
  X_COV_R          = XMLString::transcode("R");
  X_COV_RSR        = XMLString::transcode("RSR");
  X_COV_S          = XMLString::transcode("S");
  X_IS_ERR_OUT     = XMLString::transcode("is_stderr_out");
  X_IS_CORR_OUT    = XMLString::transcode("is_correlation_out");
  X_IS_COV_OUT     = XMLString::transcode("is_covariance_out");
  X_IS_INV_COV_OUT = XMLString::transcode("is_inverse_covariance_out");
  X_IS_COEF_OUT    = XMLString::transcode("is_coefficent_out");
  X_IS_CONF_OUT    = XMLString::transcode("is_confidence_out");
  X_NONMEM         = XMLString::transcode("nonmem");
  X_POP_ANALYSIS   = XMLString::transcode("pop_analysis");
  X_IND_ANALYSIS   = XMLString::transcode("ind_analysis");
  X_CONSTRAINT     = XMLString::transcode("constraint");
  X_MODEL          = XMLString::transcode("model");
  X_PRED           = XMLString::transcode("pred");
  X_PRESENTATION   = XMLString::transcode("presentation");
  X_TABLE          = XMLString::transcode("table");
  X_SCATTERPLOT    = XMLString::transcode("scatterplot");
  X_COLUMN         = XMLString::transcode("column");
  X_X              = XMLString::transcode("x");
  X_Y              = XMLString::transcode("y");
  X_BY             = XMLString::transcode("by");
  X_APPROXIMATION  = XMLString::transcode("approximation");
  X_FO             = XMLString::transcode("fo");
  X_FOCE           = XMLString::transcode("foce");
  X_LAPLACE        = XMLString::transcode("laplace");
  X_POP_SIZE       = XMLString::transcode("pop_size" );
  X_IS_ESTIMATION  = XMLString::transcode("is_estimation");
  X_IS_ETA_OUT     = XMLString::transcode("is_eta_out");
  X_IS_RESTART     = XMLString::transcode("is_restart");
  X_DATA_LABELS    = XMLString::transcode("data_labels");
  X_FILENAME       = XMLString::transcode("filename");
  X_NAME           = XMLString::transcode("name");
  X_SYNONYM        = XMLString::transcode("synonym");
  X_THETA          = XMLString::transcode("theta");
  X_LENGTH         = XMLString::transcode("length");
  X_OMEGA          = XMLString::transcode("omega");
  X_ONLYSIMULATION = XMLString::transcode("onlysimulation");
  X_SIGMA          = XMLString::transcode("sigma");
  X_SIMULATION     = XMLString::transcode("simulation");
  X_SEED           = XMLString::transcode("seed");
  X_SUBPROBLEMS    = XMLString::transcode("subproblems");
  X_POP_STAT       = XMLString::transcode("pop_stat");
  X_COVARIANCE_FORM= XMLString::transcode("covariance_form");
  X_MITR           = XMLString::transcode("mitr");
  X_IND_STAT       = XMLString::transcode("ind_stat");
  X_SIG_DIGITS     = XMLString::transcode("sig_digits");
  assert( strcmp( fMakefile                   , "generatedMakefile" ) == 0 );
  assert( strcmp( fIndData_h                  , "IndData.h" ) == 0 );
  assert( strcmp( fDataSet_h                  , "DataSet.h" ) == 0 );
  assert( strcmp( fPredEqn_fortran            , "predEqn.fortran" ) == 0 );
  assert( strcmp( fPredEqn_cpp                , "predEqn.cpp" ) == 0 );
  assert( strcmp( fPred_h                     , "Pred.h" ) == 0 );
  assert( strcmp( fOmega_h                    , "Omega.h" ) == 0 );
  assert( strcmp( fOmega_cpp                  , "Omega.cpp" ) == 0 );
  assert( strcmp( fDriver_cpp                 , "driver.cpp" ) == 0 );
  assert( strcmp( fSoftwareError_xml          , "software_error.xml" ) == 0 );
  assert( strcmp( fSpkRuntimeError_tmp        , "spk_error.tmp" ) == 0 );
  assert( strcmp( fResult_xml                 , "result.xml" ) == 0 );
  assert( strcmp( BURNER                      , "// THIS FILE IS GENERATED BY THE ASPK COMPILER" ) == 0 );
  assert( XMLString::equals( X_YES            , XMLString::transcode("yes") ) );
  assert( XMLString::equals( X_NO             , XMLString::transcode("no") ) );
  assert( XMLString::equals( X_FIXED          , XMLString::transcode("fixed") ) );
  assert( XMLString::equals( X_IN             , XMLString::transcode("in") ) );
  assert( XMLString::equals( X_LOW            , XMLString::transcode("low") ) );
  assert( XMLString::equals( X_UP             , XMLString::transcode("up") ) );
  assert( XMLString::equals( X_DIAGONAL       , XMLString::transcode("diagonal") ) );
  assert( XMLString::equals( X_BLOCK          , XMLString::transcode("block") ) );
  assert( XMLString::equals( X_VALUE          , XMLString::transcode("value") ) );
  assert( XMLString::equals( X_STRUCT         , XMLString::transcode("struct") ) );
  assert( XMLString::equals( X_DIMENSION      , XMLString::transcode("dimension") ) );
  assert( XMLString::equals( X_LABEL          , XMLString::transcode("label") ) );
  assert( XMLString::equals( X_COV_R          , XMLString::transcode("R") ) );
  assert( XMLString::equals( X_COV_RSR        , XMLString::transcode("RSR") ) );
  assert( XMLString::equals( X_COV_S          , XMLString::transcode("S") ) );
  assert( XMLString::equals( X_IS_ERR_OUT     , XMLString::transcode("is_stderr_out") ) );
  assert( XMLString::equals( X_IS_CORR_OUT    , XMLString::transcode("is_correlation_out") ) );
  assert( XMLString::equals( X_IS_COV_OUT     , XMLString::transcode("is_covariance_out") ) );
  assert( XMLString::equals( X_IS_INV_COV_OUT , XMLString::transcode("is_inverse_covariance_out") ) );
  assert( XMLString::equals( X_IS_COEF_OUT    , XMLString::transcode("is_coefficent_out") ) );
  assert( XMLString::equals( X_IS_CONF_OUT    , XMLString::transcode("is_confidence_out") ) );
  assert( XMLString::equals( X_NONMEM         , XMLString::transcode("nonmem") ) );
  assert( XMLString::equals( X_POP_ANALYSIS   , XMLString::transcode("pop_analysis") ) );
  assert( XMLString::equals( X_IND_ANALYSIS   , XMLString::transcode("ind_analysis") ) );
  assert( XMLString::equals( X_CONSTRAINT     , XMLString::transcode("constraint") ) );
  assert( XMLString::equals( X_MODEL          , XMLString::transcode("model") ) );
  assert( XMLString::equals( X_PRED           , XMLString::transcode("pred") ) );
  assert( XMLString::equals( X_PRESENTATION   , XMLString::transcode("presentation") ) );
  assert( XMLString::equals( X_TABLE          , XMLString::transcode("table") ) );
  assert( XMLString::equals( X_SCATTERPLOT    , XMLString::transcode("scatterplot") ) );
  assert( XMLString::equals( X_COLUMN         , XMLString::transcode("column") ) );
  assert( XMLString::equals( X_X              , XMLString::transcode("x") ) );
  assert( XMLString::equals( X_Y              , XMLString::transcode("y") ) );
  assert( XMLString::equals( X_BY             , XMLString::transcode("by") ) );
  assert( XMLString::equals( X_APPROXIMATION  , XMLString::transcode("approximation") ) );
  assert( XMLString::equals( X_FO             , XMLString::transcode("fo") ) );
  assert( XMLString::equals( X_FOCE           , XMLString::transcode("foce") ) );
  assert( XMLString::equals( X_LAPLACE        , XMLString::transcode("laplace") ) );
  assert( XMLString::equals( X_POP_SIZE       , XMLString::transcode("pop_size" ) ) );
  assert( XMLString::equals( X_IS_ESTIMATION  , XMLString::transcode("is_estimation") ) );
  assert( XMLString::equals( X_IS_ETA_OUT     , XMLString::transcode("is_eta_out") ) );
  assert( XMLString::equals( X_IS_RESTART     , XMLString::transcode("is_restart") ) );
  assert( XMLString::equals( X_DATA_LABELS    , XMLString::transcode("data_labels") ) );
  assert( XMLString::equals( X_FILENAME       , XMLString::transcode("filename") ) );
  assert( XMLString::equals( X_NAME           , XMLString::transcode("name") ) );
  assert( XMLString::equals( X_SYNONYM        , XMLString::transcode("synonym") ) );
  assert( XMLString::equals( X_THETA          , XMLString::transcode("theta") ) );
  assert( XMLString::equals( X_LENGTH         , XMLString::transcode("length") ) );
  assert( XMLString::equals( X_OMEGA          , XMLString::transcode("omega") ) );
  assert( XMLString::equals( X_SIGMA          , XMLString::transcode("sigma") ) );
  assert( XMLString::equals( X_SIMULATION     , XMLString::transcode("simulation") ) );
  assert( XMLString::equals( X_SEED           , XMLString::transcode("seed") ) );
  assert( XMLString::equals( X_POP_STAT       , XMLString::transcode("pop_stat") ) );
  assert( XMLString::equals( X_COVARIANCE_FORM, XMLString::transcode("covariance_form") ) );
  assert( XMLString::equals( X_MITR           , XMLString::transcode("mitr") ) );
  assert( XMLString::equals( X_IND_STAT       , XMLString::transcode("ind_stat") ) );
  assert( XMLString::equals( X_SIG_DIGITS     , XMLString::transcode("sig_digits" ) ) );
  assert( XMLString::equals( X_ONLYSIMULATION , XMLString::transcode("onlysimulation") ) );
  assert( XMLString::equals( X_SUBPROBLEMS    , XMLString::transcode("subproblems") ) );
}
NonmemTranslator::NonmemTranslator()
{
}
NonmemTranslator::~NonmemTranslator()
{
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
  assert( table->getLabels()->size() > 0 );

  DOMElement * spksouce = source->getDocumentElement();
  DOMNodeList * nonmems = spksouce->getElementsByTagName( X_NONMEM );
  assert( nonmems->getLength() == 1 );
  DOMElement * nonmem = dynamic_cast<DOMElement*>( nonmems->item(0) );

  //------------------------------------------------------
  // <constraint>
  //------------------------------------------------------
  DOMNodeList * constraints = nonmem->getElementsByTagName( X_CONSTRAINT );
  assert( constraints->getLength() == 1 );
  DOMElement * constraint = dynamic_cast<DOMElement*>( constraints->item(0) );
  assert( constraint->hasChildNodes() );

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
      assert( pop_analysises->getLength() == 1 || ind_analysises->getLength() == 1 );
    }
  //------------------------------------------------------
  // <model>
  // NOTE: only <pred> is allowed under <model> for v0.1.
  //------------------------------------------------------
  DOMNodeList * models = nonmem->getElementsByTagName( X_MODEL );
  assert( models->getLength() == 1 );
  DOMElement * model = dynamic_cast<DOMElement*>( models->item(0) );
  DOMNodeList * preds = model->getElementsByTagName( X_PRED );
  assert( preds->getLength() == 1 );
  DOMElement * pred = dynamic_cast<DOMElement*>( preds->item(0) );
  myModelSpec = PRED;

  bool isPredDone = false;
  parsePred( pred );
  isPredDone = true;
 
  //------------------------------------------------------
  // <presentation>
  //------------------------------------------------------
  // PRED parsing and <xxx_analysis> parsing must have been completed so that the symbol table
  // contains entries for the user defined variables and THETA/OMEGA/SIGMA/ETA.
  assert( isPredDone );
  assert( isAnalysisDone );
  
  DOMNodeList * presentations = nonmem->getElementsByTagName( X_PRESENTATION );
  assert( presentations->getLength() == 1 );
  DOMElement * presentation = dynamic_cast<DOMElement*>( presentations->item(0) );

  myRecordNums.resize( myPopSize );
  Symbol * id = table->findi( KeyStr::ID );
  assert( id != NULL || id != Symbol::empty() );
  for( int i=0; i<myPopSize; i++ )
    {
      myRecordNums[i] = id->initial[i].size();
    }

  if( myIsSimulate )
    {
      Symbol * p = table->insertUserVar(DefaultStr::SIMDV);
    }

  // Keep the user-typed Nonmem Keyword strings
  Symbol * p;
  if( (p = table->findi( KeyStr::ID )) != Symbol::empty() )
     UserStr::ID = p->name;
  else
     UserStr::ID = DefaultStr::ID;

  if( (p = table->findi( KeyStr::THETA )) != Symbol::empty() )
     UserStr::THETA = p->name;
  else
     UserStr::THETA = DefaultStr::THETA;

  if( (p = table->findi( KeyStr::OMEGA )) != Symbol::empty() )
     UserStr::OMEGA = p->name;
  else
     UserStr::OMEGA = DefaultStr::OMEGA;

  if( (p = table->findi( KeyStr::SIGMA )) != Symbol::empty() )
     UserStr::SIGMA = p->name;
  else
     UserStr::SIGMA = DefaultStr::SIGMA;

  if( (p = table->findi( KeyStr::ETA )) != Symbol::empty() )
     UserStr::ETA = p->name;
  else
     UserStr::ETA = DefaultStr::ETA;

  if( (p = table->findi( KeyStr::EPS )) != Symbol::empty() )
     UserStr::EPS = p->name;
  else
     UserStr::EPS = DefaultStr::EPS;

  if( (p = table->findi( KeyStr::PRED )) != Symbol::empty() )
     UserStr::PRED = p->name;
  else
  {
     table->insertUserVar( DefaultStr::PRED );
     UserStr::PRED = DefaultStr::PRED;
  }

  if( (p = table->findi( KeyStr::RES )) != Symbol::empty() )
     UserStr::RES = p->name;
  else
  {
     table->insertUserVar( DefaultStr::RES );
     UserStr::RES = DefaultStr::RES;
  }

  if( (p = table->findi( KeyStr::WRES )) != Symbol::empty() )
     UserStr::WRES = p->name;
  else
  {
     table->insertUserVar( DefaultStr::WRES );
     UserStr::WRES = DefaultStr::WRES;
  }

  if( (p = table->findi( KeyStr::MDV )) != Symbol::empty() )
     UserStr::MDV = p->name;
  else
  {
     Symbol * s = table->insertLabel( DefaultStr::MDV, "", myRecordNums );
     for( int i=0; i<myPopSize; i++ )
	s->initial[i] = "0";
     UserStr::MDV = DefaultStr::MDV;
  }

  if( (p = table->findi( KeyStr::DV )) != Symbol::empty() )
     UserStr::DV = p->name;
  else
     UserStr::DV = DefaultStr::DV;
  
  if( (p = table->findi( KeyStr::SIMDV )) != Symbol::empty() )
     UserStr::SIMDV = p->name;
  else
     UserStr::SIMDV = DefaultStr::SIMDV;
  
  if( (p = table->findi( KeyStr::F )) != Symbol::empty() )
     UserStr::F = p->name;
  else
     UserStr::F = DefaultStr::F;
  
  if( (p = table->findi( KeyStr::Y )) != Symbol::empty() )
     UserStr::Y = p->name;
  else
     UserStr::Y = DefaultStr::Y;
  
  //
  // Generate the headers and definition files for IndData class and
  // DataSet class.
  //
  // The symbol table (ie. the order of data labels in the list) must not change
  // in between the following two routines.
  //
  generateDataSet( );
  generateIndData( );
  generatePred( fPredEqn_cpp );
  if( myTarget == POP )
    generatePopDriver();
  else
    generateIndDriver();
  generateMakefile();
}
void NonmemTranslator::generateMakefile() const
{
  ofstream oMake( fMakefile );
  assert( oMake.good() );
  oMake << "driver : driver.cpp Pred.h DataSet.h IndData.h" << endl;
  oMake << "\tg++ -g driver.cpp -o driver ";
  oMake << "-lspk -lspkopt -lspkpred -latlas_lapack -lcblas -latlas -lpthread -lm";
  oMake << endl;
  oMake << "clean : " << endl;
  oMake << "\trm software_error.xml result.xml driver predEqn.cpp IndData.h DataSet.h Pred.h driver.cpp spk_error.tmp" << endl;
  oMake.close();
  return;
}
void NonmemTranslator::parsePopAnalysis( DOMElement* pop_analysis )
{
  
  //================================================================================
  // Required attributes
  //================================================================================
  // * approximation = {fo, foce, laplace}
  // * pop_size
  // * is_estimation = {yes, no}

  //
  // Finding out the approximation method
  //
  assert( pop_analysis->hasAttribute( X_APPROXIMATION ) );
  const XMLCh * xml_approx = pop_analysis->getAttribute( X_APPROXIMATION );

  assert( XMLString::equals( xml_approx, X_FO ) || XMLString::equals( xml_approx, X_FOCE ) || XMLString::equals( xml_approx, X_LAPLACE ) );
  if( XMLString::equals( xml_approx, X_FO ) )
    myApproximation = FO;
  else if( XMLString::equals( xml_approx, X_FOCE ) )
    myApproximation = FOCE;
  else //( XMLString::equals( xml_approx, X_LAPLACE ) )
    myApproximation = LAPLACE;  

  //
  // Finding out the population size
  //
  assert( pop_analysis->hasAttribute( X_POP_SIZE ) );
  const XMLCh * xml_pop_size = pop_analysis->getAttribute( X_POP_SIZE );
  if( !XMLString::textToBin( xml_pop_size, myPopSize ) )
    {
      fprintf( stderr, 
	       "Failed to evaluate XMLString::textToBin( %s, myPopSize )\n",
	       XMLString::transcode(xml_pop_size) );
      abort();
    }

  //
  // Finding out if parameter estimation is requested.
  //
  assert( pop_analysis->hasAttribute( X_IS_ESTIMATION ) );
  const XMLCh * xml_is_estimation = pop_analysis->getAttribute( X_IS_ESTIMATION );
  myIsEstimate = ( XMLString::equals( xml_is_estimation, X_YES )? true : false );

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
		assert( xml_sig_digits > 0 );
	      assert( mySigDigits > 0 && mySigDigits < 9 );
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
  assert( data_labels_list->getLength() == 1 );
  DOMElement * data_labels = dynamic_cast<DOMElement*>( data_labels_list->item(0) );
  {
     DOMNodeList * labels = data_labels->getElementsByTagName( X_LABEL );
     int nLabels = labels->getLength();
     assert( nLabels > 0 );
     for( int i=0; i<nLabels; i++ )
       {
	 DOMElement * xml_label = dynamic_cast<DOMElement*>( labels->item(i) );
	 // <label> is an empty element

         // required attribute
	 // * name
	 assert( xml_label->hasAttribute( X_NAME ) );
	 const XMLCh* xml_name = xml_label->getAttribute( X_NAME );
	 assert( XMLString::stringLen( xml_name ) > 0 );
         char * c_name = XMLString::transcode( xml_name );
	 Symbol * name = table->find( c_name );

	 assert( name != Symbol::empty() );
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
  assert( theta_list->getLength() == 1 );
  DOMElement * theta = dynamic_cast<DOMElement*>( theta_list->item(0) );
  assert( theta->hasAttribute( X_LENGTH ) );
  const XMLCh* xml_theta_len = theta->getAttribute( X_LENGTH );
  myThetaLen = 0;
  if( !XMLString::textToBin( xml_theta_len, myThetaLen ) )
    {
      fprintf( stderr, 
	       "XMLString::textToBin( %s, myThetaLen ) returned false.\n", 
	       XMLString::transcode( xml_theta_len ) );
      abort();
    }
  Symbol * sym_theta = table->insertNMVector( DefaultStr::THETA, myThetaLen );
  {
    //<in>
    DOMNodeList * theta_in_list = theta->getElementsByTagName( X_IN );
    assert( theta_in_list->getLength() == 1 );
    DOMElement * theta_in = dynamic_cast<DOMElement*>( theta_in_list->item(0) );

    DOMNodeList * value_list = theta_in->getElementsByTagName( X_VALUE );
    assert( myThetaLen == value_list->getLength() );
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
    assert( theta_low_list->getLength() == 1 );
    DOMElement * theta_low = dynamic_cast<DOMElement*>( theta_low_list->item(0) );
    value_list = theta_low->getElementsByTagName( X_VALUE );
    assert( myThetaLen == value_list->getLength() );
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
    assert( theta_up_list->getLength() == 1 );
    DOMElement * theta_up = dynamic_cast<DOMElement*>( theta_up_list->item(0) );
    value_list = theta_up->getElementsByTagName( X_VALUE );
    assert( myThetaLen == value_list->getLength() );
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
  assert( nOmegaSpecs == 1 ); // v0.1 supports only one (full) Omega specification
  DOMElement * omega = dynamic_cast<DOMElement*>( omega_list->item(0) );
  assert( omega->hasAttribute( X_DIMENSION ) );
  const XMLCh* xml_omega_dim = omega->getAttribute( X_DIMENSION );
  if( !XMLString::textToBin( xml_omega_dim, myOmegaDim ) )
    {
      fprintf( stderr, 
	       "XMLString::textToBin( %s, myOmegaDim ) returned false.\n", 
	       XMLString::transcode( xml_omega_dim ) );
      abort();
    }

  assert( omega->hasAttribute( X_STRUCT ) );
  const XMLCh* xml_omega_struct = omega->getAttribute( X_STRUCT );
  assert( XMLString::equals( xml_omega_struct, X_DIAGONAL ) || XMLString::equals( xml_omega_struct, X_BLOCK ) );
  if( XMLString::equals( xml_omega_struct, X_DIAGONAL ) )
    {
      myOmegaStruct = Symbol::DIAGONAL;
      myOmegaElemNum = myOmegaDim;
    }
  else //( XMLString::equals( xml_omega_struct, X_BLOCK ) )
    {
      myOmegaStruct = Symbol::TRIANGLE;
      myOmegaElemNum = series( 1, 1, myOmegaDim );
    }

  Symbol * sym_omega = table->insertNMMatrix( DefaultStr::OMEGA, myOmegaStruct, myOmegaDim );
  {
    //<in>
    DOMNodeList * omega_in_list = omega->getElementsByTagName( X_IN );
    assert( omega_in_list->getLength() == 1 );
    DOMElement * omega_in = dynamic_cast<DOMElement*>( omega_in_list->item(0) );

    DOMNodeList * value_list = omega_in->getElementsByTagName( X_VALUE );
    assert( myOmegaElemNum == value_list->getLength() );
    for( int i=0; i<myOmegaElemNum; i++ )
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
	//	sym_omega->up[0][i]      = 
      }
  }

  DOMNodeList * simga_list = pop_analysis->getElementsByTagName( X_SIGMA );
  int nSigmaSpecs = simga_list->getLength();
  assert( nSigmaSpecs == 1 );// v0.1 supports only one (full) Sigma specification
  DOMElement * sigma = dynamic_cast<DOMElement*>( simga_list->item(0) );
  assert( sigma->hasAttribute( X_DIMENSION ) );
  const XMLCh* xml_sigma_dim = sigma->getAttribute( X_DIMENSION );
  if( !XMLString::textToBin( xml_sigma_dim, mySigmaDim ) )
    {
      fprintf( stderr, 
	       "XMLString::textToBin( %s, mySigmaDim ) returned false.\n", 
	       XMLString::transcode( xml_sigma_dim ) );
      abort();
    }

  assert( sigma->hasAttribute( X_STRUCT ) );
  const XMLCh* xml_sigma_struct = sigma->getAttribute( X_STRUCT );
  assert( XMLString::equals( xml_sigma_struct, X_DIAGONAL ) || XMLString::equals( xml_sigma_struct, X_BLOCK ) );
  if( XMLString::equals( xml_sigma_struct, X_DIAGONAL ) )
    {
      mySigmaStruct = Symbol::DIAGONAL;
      mySigmaElemNum = mySigmaDim;
    }
  else //( XMLString::equals( xml_sigma_struct, X_BLOCK ) )
    {
      mySigmaStruct = Symbol::TRIANGLE;
      mySigmaElemNum = series( 1, 1, mySigmaDim );
    }

  Symbol * sym_sigma = table->insertNMMatrix( DefaultStr::SIGMA, mySigmaStruct, mySigmaDim ); 
  {
    //<in>
    DOMNodeList * sigma_in_list = sigma->getElementsByTagName( X_IN );
    assert( sigma_in_list->getLength() == 1 );
    DOMElement * sigma_in = dynamic_cast<DOMElement*>( sigma_in_list->item(0) );

    DOMNodeList * value_list = sigma_in->getElementsByTagName( X_VALUE );
    assert( mySigmaElemNum == value_list->getLength() );
    for( int i=0; i<mySigmaElemNum; i++ )
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
  Symbol * sym_eta = table->insertNMVector( DefaultStr::ETA, myEtaLen );
  sym_eta->initial[0] = etaDefault;
  sym_eta->fixed[0] = false;

  //----------------------------------------------------------
  // Sigma 
  // Sigma is the covariance of EPS: thus, 
  // the order of Sigma is the length of EPS vector.
  myEpsLen = mySigmaDim;
  char epsDefault[] = "0.0";
  Symbol * sym_eps = table->insertNMVector( DefaultStr::EPS, myEpsLen );
  sym_eps->initial[0] = epsDefault;
  sym_eta->fixed[0] = false;

  //================================================================================
  // (Optional) Statistics elements
  //================================================================================
  // <description>  --- ignore!
  // <simulation>
  // <pop_stat>
  myIsSimulate = false;
  mySeed       = 0;
  DOMNodeList * simulations = pop_analysis->getElementsByTagName( X_SIMULATION );
  if( simulations->getLength() > 0 )
    {
      assert( simulations->getLength() == 1 );
      myIsSimulate = true;
      DOMElement* simulation = dynamic_cast<DOMElement*>( simulations->item(0) );
      assert( simulation->hasAttribute( X_SEED ) );
      const XMLCh* xml_seed = simulation->getAttribute( X_SEED );
      if( !XMLString::textToBin( xml_seed, mySeed ) )
	{
	  fprintf( stderr, "XMLString::textToBin( %s, mySeed ) returned false.\n", 
		   XMLString::transcode(xml_seed) );
	  abort();
	}
      if( simulation->hasAttribute( X_ONLYSIMULATION ) )
      {
         const XMLCh* xml_only_simulation = simulation->getAttribute( X_ONLYSIMULATION );
         if( XMLString::equals( xml_only_simulation, X_YES ) )
           myIsOnlySimulation = true;
         else
           myIsOnlySimulation = false;
      }
      if( simulation->hasAttribute( X_SUBPROBLEMS ) )
      {
         const XMLCh* xml_subproblems = simulation->getAttribute( X_SUBPROBLEMS );
         if( !XMLString::textToBin( xml_subproblems, mySubproblemsN ) )
         {
	  fprintf( stderr, "XMLString::textToBin( %s, mySubproblemsN ) returned false.\n", 
		   XMLString::transcode(xml_subproblems) );
	  abort();
         }
      }
    }
  
  DOMNodeList * pop_stat_list = pop_analysis->getElementsByTagName( X_POP_STAT );
  myCovForm       = "R";  //default
  myIsStderr      = true;//default
  myIsCorrelation = true;//default
  myIsCov         = true;//default
  myIsInvCov      = true;//default
  myIsConfidence  = true;//default
  myIsCoefficient = true;//default
  // Statistics computation can be done only when the parameter estimation
  // is requested.
  if( pop_stat_list->getLength() > 0 && myIsEstimate )
    {
      assert( pop_stat_list->getLength() == 1 );
      DOMElement * pop_stat = dynamic_cast<DOMElement*>( pop_stat_list->item(0) );
      assert( pop_stat->hasAttribute( X_COVARIANCE_FORM ) );
      const XMLCh* cov_form = pop_stat->getAttribute( X_COVARIANCE_FORM ); // r, rsr, s
      if( XMLString::equals( cov_form, X_COV_S ) )
	myCovForm = "S";
      else if( XMLString::equals( cov_form, X_COV_RSR ) )
	myCovForm = "RSR";
      else
	myCovForm = "R";

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
  myIsStat = myIsStderr || myIsCorrelation || myIsCov || myIsInvCov || myIsConfidence || myIsCoefficient;

  return;
}
void NonmemTranslator::parseIndAnalysis( DOMElement* ind_analysis )
{
  //================================================================================
  // Required attributes
  //================================================================================
  // * is_estimation = {yes, no}
  const XMLCh * xml_is_estimation = ind_analysis->getAttribute( X_IS_ESTIMATION );
  assert( XMLString::stringLen( xml_is_estimation ) > 0 );
  myIsEstimate = ( XMLString::equals( xml_is_estimation, X_YES )? true : false );
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
	    assert( xml_mitr != 0 );
	}
      const XMLCh* xml_sig_digits;
      if( ind_analysis->hasAttribute( X_SIG_DIGITS ) )
	{
	  xml_sig_digits = ind_analysis->getAttribute( X_SIG_DIGITS );
	  if( !XMLString::textToBin( xml_sig_digits, mySigDigits ) )
	    assert( xml_sig_digits != 0 );
	  assert( mySigDigits > 0 && mySigDigits < 9 );
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
  assert( data_labels_list->getLength() == 1 );
  DOMElement * data_labels = dynamic_cast<DOMElement*>( data_labels_list->item(0) );
  {

     DOMNodeList * labels = data_labels->getElementsByTagName( X_LABEL );
     int nLabels = labels->getLength();
     assert( nLabels > 0 );
     for( int i=0; i<nLabels; i++ )
       {
	 DOMElement * xml_label = dynamic_cast<DOMElement*>( labels->item(i) );
	 // <label> is an empty element

         // required attribute
	 // * name
	 const XMLCh* xml_name = xml_label->getAttribute( X_NAME );
	 assert( XMLString::stringLen( xml_name ) > 0 );
	 char * c_name = XMLString::transcode( xml_name );

	 // optional attribute
	 // * synonym
         const XMLCh* xml_synonym = xml_label->getAttribute( X_SYNONYM );
	 char * c_synonym = NULL;
	 if( XMLString::stringLen( xml_synonym ) > 0 )
	   {
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
	     assert( c_synonym != NULL );
	     Symbol * synonym = table->findi( c_synonym );
	     assert( synonym != Symbol::empty() );
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
  assert( theta_list->getLength() == 1 );
  DOMElement * theta = dynamic_cast<DOMElement*>( theta_list->item(0) );
  const XMLCh* xml_theta_len = theta->getAttribute( X_LENGTH );
  assert( XMLString::stringLen( xml_theta_len ) > 0 );
  myThetaLen = 0;
  if( !XMLString::textToBin( xml_theta_len, myThetaLen ) )
    {
      fprintf( stderr, "XMLString::textToBin( %s, myThetaLen ) returned false.\n", 
	       XMLString::transcode( xml_theta_len ) );
      abort();
    }
  Symbol * sym_theta = table->insertNMVector( DefaultStr::THETA, myThetaLen );
  {
    //<in>
    DOMNodeList * theta_in_list = theta->getElementsByTagName( X_IN );
    assert( theta_in_list->getLength() == 1 );
    DOMElement * theta_in = dynamic_cast<DOMElement*>( theta_in_list->item(0) );

    DOMNodeList * value_list = theta_in->getElementsByTagName( X_VALUE );
    assert( myThetaLen == value_list->getLength() );
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
    assert( theta_low_list->getLength() == 1 );
    DOMElement * theta_low = dynamic_cast<DOMElement*>( theta_low_list->item(0) );
    value_list = theta_low->getElementsByTagName( X_VALUE );
    assert( myThetaLen == value_list->getLength() );
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
    assert( theta_up_list->getLength() == 1 );
    DOMElement * theta_up = dynamic_cast<DOMElement*>( theta_up_list->item(0) );
    value_list = theta_up->getElementsByTagName( X_VALUE );
    assert( myThetaLen == value_list->getLength() );
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
  assert( nOmegaSpecs == 1 );// v0.1 supports only one Omega specification
  DOMElement * omega = dynamic_cast<DOMElement*>( omega_list->item(0) );
  const XMLCh* xml_omega_dim = omega->getAttribute( X_DIMENSION );
  assert( XMLString::stringLen( xml_omega_dim ) > 0 );
  if( !XMLString::textToBin( xml_omega_dim, myOmegaDim ) )
    {
      fprintf( stderr, "XMLString::textToBin( %s, myOmegaDim ) returned false.\n", 
	       XMLString::transcode(xml_omega_dim) );
      abort();
    }

  // In Individual analysis, Omega is diagonal only.
  const XMLCh* xml_omega_struct = omega->getAttribute( X_STRUCT );
  assert( XMLString::equals( xml_omega_struct, X_DIAGONAL ) );

  assert( XMLString::stringLen( xml_omega_struct ) > 0 );
  myOmegaStruct = Symbol::DIAGONAL;
  myOmegaElemNum = myOmegaDim;

  Symbol * sym_omega = table->insertNMMatrix( DefaultStr::OMEGA, myOmegaStruct, myOmegaDim );
  {
    //<in>
    DOMNodeList * omega_in_list = omega->getElementsByTagName( X_IN );
    assert( omega_in_list->getLength() == 1 );
    DOMElement * omega_in = dynamic_cast<DOMElement*>( omega_in_list->item(0) );

    DOMNodeList * value_list = omega_in->getElementsByTagName( X_VALUE );
    assert( myOmegaElemNum == value_list->getLength() );
    for( int i=0; i<myOmegaElemNum; i++ )
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

  //
  // ETA
  // Eta plays the same role as EPS as in the population analysis.
  // Variance of data?
  myEtaLen = myOmegaElemNum;
  table->insertNMVector( DefaultStr::ETA, myEtaLen );
  
  //================================================================================
  // Optional elements
  //================================================================================
  // <description>  --- ignore!
  // <simulation>
  // <ind_stat>
  myIsSimulate = false;
  mySeed = 0;
  DOMNodeList * simulations = ind_analysis->getElementsByTagName( X_SIMULATION );
  if( simulations->getLength() > 0 )
    {
      assert( simulations->getLength() == 1 );
      myIsSimulate = true;
      DOMElement* simulation = dynamic_cast<DOMElement*>( simulations->item(0) );
      const XMLCh* xml_seed = simulation->getAttribute( X_SEED );
      assert( XMLString::stringLen( xml_seed ) > 0 );
      if( !XMLString::textToBin( xml_seed, mySeed ) )
	{
	  fprintf( stderr, 
		   "XMLString::textToBin( %s, mySeed ) returned false.\n", 
		   XMLString::transcode( xml_seed ) );
	  abort();
	}
    }

  DOMNodeList * ind_stat_list = ind_analysis->getElementsByTagName( X_IND_STAT );
  myIsStderr      = true;//default
  myIsCorrelation = true;//default
  myIsCov         = true;//default
  myIsInvCov      = true;//default
  myIsConfidence  = true;//default
  myIsCoefficient = true;//default

  // Statistics computation can be done only when the parameter estimation
  // is requested.
  if( ind_stat_list->getLength() > 0 && myIsEstimate )
    {
      assert( ind_stat_list->getLength() == 1 );
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
  const Symbol * pID = table->findi( KeyStr::ID );

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
  assert( cnt == 1 );
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
  assert( oIndData_h.good() );
  
  oIndData_h << BURNER << " <" << __FILE__ << ">" << endl;
  
  oIndData_h << "#ifndef INDDATA_H" << endl;
  oIndData_h << "#define INDDATA_H" << endl;
  oIndData_h << "#include <vector>" << endl;
  oIndData_h << endl;
  
  //-----------------------------------------------
  // Declaration
  //-----------------------------------------------
  oIndData_h << "template <class T>" << endl;
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
      oIndData_h << '\t' << "const std::vector<" << (isID? "char*":"T") << ">";
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
      if( type == Symbol::DATALABEL )
	{
	  bool isID = ( varName==pID->name? true : false );
          oIndData_h << "const std::vector<" << (isID? "char *" : "T") << ">";
	  oIndData_h << " " << varName << ";" << endl;
	  if( varAlias != "" )
            {
	      isID = ( varAlias == pID->name? true : false );
	      oIndData_h << "const std::vector<" << (isID? "char" : "T") << ">";
	      oIndData_h << " " << varAlias << ";" << endl;
            }
	}
      else if( type == Symbol::NONMEMDEF )
	{
	  if( keyVarName == KeyStr::THETA 
	      || keyVarName == KeyStr::ETA 
	      || keyVarName == KeyStr::EPS )
	    oIndData_h << "std::vector< std::vector<T> > " << varName << ";" << endl;
	  if( keyVarName == KeyStr::OMEGA 
              || keyVarName == KeyStr::SIGMA )
	    {}

	}
      else // type == Symbol::USERDEF
	{
	  oIndData_h << "std::vector<T> " << varName << ";" << endl;
	}
    }
  string synonym;
  oIndData_h << endl;
  oIndData_h << "~IndData();" << endl;

  // 
  // Protected member declarations.
  //
  // The default and the copy constructors are prohibited.
  // The assignment is also prohibited.
  //
  oIndData_h << "protected:" << endl;
  oIndData_h << "IndData();" << endl;
  oIndData_h << "IndData( const IndData& );" << endl;
  oIndData_h << "IndData& operator=( const IndData& );" << endl;
  oIndData_h << endl;

  //
  // Private member declarations.
  //
  // const int n: #of measurements in this set (ie. individual).
  //
  oIndData_h << "private:" << endl;
  oIndData_h << "const int n; // #of measurements." << endl;

  oIndData_h << "};" << endl;


  //-----------------------------------------------
  // Definition
  //-----------------------------------------------
  //
  // Definition of the constructor that takes a list of
  // valarray objects as arguments.
  // The order must be consistant with the declaration.
  //
  oIndData_h << "template <class T>" << endl;
  oIndData_h << "IndData<T>::IndData( int nIn";
  pLabel = labels->begin();
  for( ; pLabel != labels->end(); pLabel++ )
    {
      bool isID = ( *pLabel == pID->name );
      oIndData_h << "," << endl;

      //
      // If the label string is of "ID", then the data type is char*.
      // Othewise, double.
      //
      oIndData_h << "const std::vector<" << (isID? "char*":"T") << "> ";
      oIndData_h << "& " << *pLabel << "In";
    }
  oIndData_h << ")" << endl;
  oIndData_h << ": n( nIn )";


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
      if( keyLabel == KeyStr::OMEGA || keyLabel == KeyStr::SIGMA )
	{
	  continue;
	}

      //
      // The place holders for completed values.
      //
      if( find( labels->begin(), labels->end(), pRawTable->second.name ) 
	  == labels->end() )
	oIndData_h << "," << endl << label << "( nIn )";
    }
  oIndData_h << "{" << endl;
  oIndData_h << "   for( int i=0; i<nIn; i++ )" << endl;
  oIndData_h << "   {" << endl;
  if( myThetaLen > 0 )
    oIndData_h << "      " << UserStr::THETA << "[i].resize( " << myThetaLen << " );" << endl;
  if( myEtaLen > 0 )
    oIndData_h << "      " << UserStr::ETA   << "[i].resize( " << myEtaLen << " );" << endl;
  if( myEpsLen > 0 )
    oIndData_h << "      " << UserStr::EPS   << "[i].resize( " << myEpsLen << " );" << endl;
  oIndData_h << "   }" << endl;
  oIndData_h << "}" << endl;

  oIndData_h << endl;
  oIndData_h << "template <class T>" << endl;
  oIndData_h << "IndData<T>::~IndData(){}" << endl;

  oIndData_h << "template <class T>" << endl;
  oIndData_h << "IndData<T>::IndData(){}" << endl;

  oIndData_h << "template <class T>" << endl;
  oIndData_h << "IndData<T>::IndData( const IndData<T>& ){}" << endl;

  oIndData_h << "template <class T>" << endl;
  oIndData_h << "IndData<T>& IndData<T>::operator=( const IndData<T>& ){}" << endl;

  oIndData_h << "#endif" << endl;

  oIndData_h.close();
}
void NonmemTranslator::generateDataSet( ) const
{
  const map<const string, Symbol> * t = table->getTable();
  const vector<string> *labels = table->getLabels();
  vector<string>::const_iterator pLabel;
  int nLabels = labels->size();
  const Symbol * pID = table->findi( KeyStr::ID );

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
  //  const Symbol* pID = table->findi(KeyStr::ID);
  //
  ofstream oDataSet_h( fDataSet_h );
  assert( oDataSet_h.good() );

  oDataSet_h << BURNER << " <" << __FILE__ << ">" << endl;

  oDataSet_h << "#ifndef DATASET_H" << endl;
  oDataSet_h << "#define DATASET_H" << endl;

  oDataSet_h << "#include <vector>" << endl;
  oDataSet_h << "#include \"IndData.h\"" << endl;
  oDataSet_h << endl;

  //-----------------------------------------------
  // Declaration
  //-----------------------------------------------
  oDataSet_h << "template <class T>" << endl;
  oDataSet_h << "class DataSet" << endl;
  oDataSet_h << "{" << endl;
      
  //
  // public member declarations
  //
  // The default constructor initializes the entire data set
  // internally.
  //
  // vector<IndData<T>*> data: The entire data set.
  // const int popSize:      : The number of individuals in the population.
  oDataSet_h << "public:" << endl;
  oDataSet_h << "DataSet();" << endl;
  oDataSet_h << "~DataSet();" << endl;
  oDataSet_h << endl;

  oDataSet_h << "std::vector<IndData<T>*> data;" << endl;
  oDataSet_h << "const int popSize;" << endl;
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

  oDataSet_h << "};" << endl;


  //-----------------------------------------------
  // Definition
  //-----------------------------------------------
       
  //
  // The constructor
  //
  // Initialize the class member variables.
  //
  oDataSet_h << "template <class T>" << endl;
  oDataSet_h << "DataSet<T>::DataSet()" << endl;
  oDataSet_h << ": data(" << myPopSize << ")," << endl;
  oDataSet_h << "  popSize( " << myPopSize << " )" << endl;
  oDataSet_h << "{" << endl;
      
  // Initialize the entire data set.
  for( int who=0; who < myPopSize; who++ )
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
	  string valarray_name = s->name + "_" + c_who;

	  oDataSet_h << (isID? "char*":"T") << " " << carray_name << "[] = { ";
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
	  oDataSet_h << "std::vector<" << (isID? "char*":"T") << "> ";
	  oDataSet_h << valarray_name;
	  // oDataSet_h << "(" << carray_name << ", " << nRecords << ");" << endl;
	  oDataSet_h << "( " << nRecords << " );" << endl;
	  oDataSet_h << "copy( " << carray_name << ", " << carray_name << "+" << nRecords;
	  oDataSet_h << ", " << valarray_name << ".begin() );" << endl;
	}

      //
      // Create an IndData object.  The order in which the arguments
      // are passed to the IndData constructor must be strictly
      // compliant to the order in which the label strings are stored
      // in the list returned by SymbolTable::getLabels().
      //
      oDataSet_h << "data[" << who << "] = new IndData<T>";
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

  oDataSet_h << "}" << endl;

  // The destructor
  // Free memory allocated for the entire data set.
  oDataSet_h << "template <class T>" << endl;
  oDataSet_h << "DataSet<T>::~DataSet()" << endl;
  oDataSet_h << "{" << endl;
  oDataSet_h << "   const int n = data.size();" << endl;
  oDataSet_h << "   for( int i=0; i<n; i++ )" << endl;
  oDataSet_h << "   {" << endl;
  oDataSet_h << "      delete data[i];" << endl;
  oDataSet_h << "   }" << endl;
  oDataSet_h << "}" << endl;

  oDataSet_h << "template <class T>" << endl;
  oDataSet_h << "DataSet<T>::DataSet( const DataSet<T>& ){}" << endl;

  oDataSet_h << "template <class T>" << endl;
  oDataSet_h << "DataSet<T>& DataSet<T>::operator=( const DataSet<T>& ){}" << endl;

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
  assert( oPred_h.good() );

  // macros and header includes
  oPred_h << BURNER << endl;
  oPred_h << "#ifndef PRED_H" << endl;
  oPred_h << "#define PRED_H" << endl;
  oPred_h << endl;

  oPred_h << "#include <vector>" << endl;
  oPred_h << "#include <string>" << endl;
  oPred_h << "#include <spkpred/PredBase.h>" << endl;
  oPred_h << "#include <cppad/include/CppAD.h>" << endl;
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
  oPred_h << "template <class Value>" << endl;
  oPred_h << "class Pred : public PredBase<Value>" << endl;
  oPred_h << "{" << endl;
      
  //
  // public interfaces
  //
  oPred_h << "public:" << endl;

  // The legal constructor.
  // This constructor takes a pointer to the DataSet (the set of
  // all individuals' data).
  oPred_h << "Pred( const DataSet<Value>* dataIn );" << endl;

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
  oPred_h << "           const std::vector<Value>& spk_indepVar," << endl;
  oPred_h << "           std::vector<Value>& spk_depVar );" << endl;

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
  // const DataSet<T> *perm: A pointer to the read-only data set.
  // DataSet<T> temp:        The temporary storage for current values
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
  oPred_h << "const DataSet<Value> *perm;" << endl;
  oPred_h << "DataSet<Value> temp;" << endl;
  oPred_h << "mutable bool isIterationCompleted;" << endl;

  // Taking care of the data items (from the data file).
  // Only the "ID" data item values are of type string,
  // otherwise all numeric, T.
  pLabel = labels->begin();
  for( int i=0; i<nLabels, pLabel != labels->end(); i++, pLabel++ )
    {
      bool isID = (SymbolTable::key( *pLabel ) == KeyStr::ID );

      const Symbol* s = table->findi( *pLabel );
      oPred_h << "mutable " << ( isID? "std::string" : "Value" );
      oPred_h << " " << s->name << ";" << endl;
      if( !s->synonym.empty() )
	{
	  oPred_h << "mutable " << ( isID? "std::string" : "Value" );
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
      if( keyLabel != KeyStr::THETA 
	  && keyLabel != KeyStr::ETA 
	  && keyLabel != KeyStr::EPS 
	  && keyLabel != KeyStr::SIGMA
	  && keyLabel != KeyStr::OMEGA )
	{
	  // Ignore if the label is of the data item's.
	  if( find( labels->begin(), labels->end(), label ) 
	      == labels->end() )
	    {
	      oPred_h << "mutable Value " << label;
	      oPred_h << ";" << endl;
	    }
	}
    }

  // footer
  oPred_h << "};" << endl;

  //----------------------------------------------
  // Definition
  //----------------------------------------------
  oPred_h << "template <class Value>" << endl;
  oPred_h << "Pred<Value>::Pred( const DataSet<Value>* dataIn )" << endl;
  oPred_h << ": perm( dataIn )," << endl;
  oPred_h << "  nIndividuals( " << myPopSize << " )," << endl;
  oPred_h << "  isIterationCompleted( true )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "template <class Value>" << endl;
  oPred_h << "Pred<Value>::~Pred()" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "template <class Value>" << endl;
  oPred_h << "int Pred<Value>::getNObservs( int spk_i ) const" << endl;
  oPred_h << "{" << endl;
  oPred_h << "  return perm->data[spk_i]->" << UserStr::ID << ".size();" << endl;
  oPred_h << "}" << endl;


  oPred_h << "template <class Value>" << endl;
  oPred_h << "bool Pred<Value>::eval( int spk_thetaOffset, int spk_thetaLen," << endl;
  oPred_h << "                        int spk_etaOffset,   int spk_etaLen," << endl;
  oPred_h << "                        int spk_epsOffset,   int spk_epsLen," << endl;
  oPred_h << "                        int spk_fOffset,     int spk_fLen," << endl;
  oPred_h << "                        int spk_yOffset,     int spk_yLen," << endl;
  oPred_h << "                        int spk_i," << endl;
  oPred_h << "                        int spk_j," << endl;
  oPred_h << "                        const std::vector<Value>& spk_indepVar," << endl;
  oPred_h << "                        std::vector<Value>& spk_depVar )" << endl;
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
      oPred_h << "typename std::vector<Value>::const_iterator " << UserStr::THETA << i+1;
      oPred_h << " = spk_indepVar.begin() + spk_thetaOffset + " << i << ";" << endl;
    }
  for( int i=0; i<myEtaLen; i++ )
    {
      oPred_h << "typename std::vector<Value>::const_iterator " << UserStr::ETA << i+1;
      oPred_h << " = spk_indepVar.begin() + spk_etaOffset + " << i << ";" << endl;
    }

  // EPS is only apparent in the population analysis.
  // The size of EPS vector is the order of SIGMA which is only apparent in
  // the population analysis.  So, if this is the individual level,
  // "myEpsLen" has been set to zero; thus the following loop loops zero times.
  for( int i=0; i<myEpsLen; i++ )
    {
      oPred_h << "typename std::vector<Value>::const_iterator " << UserStr::EPS << i+1;
      oPred_h << " = spk_indepVar.begin() + spk_epsOffset + " << i << ";" << endl;
    }
  oPred_h << "typename std::vector<Value>::const_iterator " << UserStr::THETA;
  oPred_h << " = spk_indepVar.begin() + spk_thetaOffset;" << endl;
  oPred_h << "typename std::vector<Value>::const_iterator " << UserStr::ETA;
  oPred_h << " = spk_indepVar.begin() + spk_etaOffset;" << endl;
  if( myTarget == POP )
    {
      oPred_h << "typename std::vector<Value>::const_iterator " << UserStr::EPS;
      oPred_h << " = spk_indepVar.begin() + spk_epsOffset;" << endl;
    }

  oPred_h << "Value " << UserStr::F << " = 0.0;" << endl;

  oPred_h << "Value " << UserStr::Y << " = 0.0;" << endl;
  ///////////////////////////////////////////////////////////////////////////////////
      
  oPred_h << "//=========================================" << endl;
  oPred_h << "// Begin User Code                         " << endl;
  oPred_h << "//-----------------------------------------" << endl;
  char ch;
  ifstream iPredEqn( fPredEqn_cpp );
  assert( iPredEqn.good() );
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
  oPred_h << UserStr::PRED << " = " << UserStr::F << ";" << endl;
  oPred_h << UserStr::RES  << " = perm->data[spk_i]->";
  oPred_h << UserStr::DV << "[spk_j] - " << UserStr::PRED << ";" << endl;

  for( pRawTable = rawTable->begin(); pRawTable != rawTable->end(); pRawTable++ )
    {
      // THETA, ETA, EPS are given Pred::eval() as vectors by the caller.
      // So, we have to treat these guys a bit different from the user variables
      // which are scalar values.
      const string label    = pRawTable->second.name;
      const string keyLabel = SymbolTable::key( label );
      if( keyLabel == KeyStr::THETA )
	{
	  oPred_h << "copy( " << label << ", " << label << "+spk_thetaLen, ";
          oPred_h << "temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	}
      else if( keyLabel == KeyStr::ETA )
	{
	  oPred_h << "copy( " << label << ", " << label << "+spk_etaLen, ";
          oPred_h << "temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	}
      else if( keyLabel == KeyStr::EPS )
	{
	  oPred_h << "copy( " << label << ", " << label << "+spk_epsLen, ";
          oPred_h << "temp.data[ spk_i ]->" << label << "[ spk_j ].begin() ); " << endl;
	}
      else if( keyLabel == KeyStr::OMEGA || keyLabel == KeyStr::SIGMA )
	{
	  // ignore.  these don't get used within PRED.
	}
      else if( keyLabel == KeyStr::WRES )
	{
	  // ignore.  This value is only computed outside at the final estimate.
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
  oPred_h << UserStr::ID << ".size()-1 )" << endl;
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
      if( keyLabel == KeyStr::OMEGA || keyLabel == KeyStr::SIGMA )
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
  oPred_h << "spk_depVar[ spk_fOffset+spk_j ] = " << UserStr::F << ";" << endl;
  oPred_h << "spk_depVar[ spk_yOffset+spk_j ] = " << UserStr::Y << ";" << endl;

  // Pred::eval() returns true if MDV(i,j) is 0, which means DV is NOT missing.
  // In this iteration, it is assumed that MDV=true for all, so return true.
  oPred_h << "if( perm->data[ spk_i ]->" << UserStr::MDV << "[ spk_j ] == 0 )" << endl;
  oPred_h << "   return true;" << endl;
  oPred_h << "else return false;" << endl;

  oPred_h << "}" << endl;

  oPred_h << "template <class Value>" << endl;
  oPred_h << "Pred<Value>::Pred()" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "template <class Value>" << endl;
  oPred_h << "Pred<Value>::Pred( const Pred<Value>& )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "template <class Value>" << endl;
  oPred_h << "Pred<Value> & Pred<Value>::operator=( const Pred<Value>& )" << endl;
  oPred_h << "{" << endl;
  oPred_h << "}" << endl;

  oPred_h << "#endif" << endl;
  oPred_h.close();
}

void NonmemTranslator::generateIndDriver( ) const
{
  //==================================================================
  // Generate the driver
  //==================================================================
  ofstream oDriver ( fDriver_cpp );
  assert( oDriver.good() );

  const Symbol* pTheta = table->findi(KeyStr::THETA);
  const Symbol* pEta   = table->findi(KeyStr::ETA);
  const Symbol* pOmega = table->findi(KeyStr::OMEGA);

  oDriver << "#include <iostream>" << endl;
  oDriver << "#include <fstream>" << endl;
  oDriver << "#include <sys/time.h>" << endl;
  oDriver << "#include <vector>" << endl;
  oDriver << endl;

  oDriver << "#include <spk/SpkValarray.h>" << endl;
  oDriver << "#include <spk/SpkException.h>" << endl;
  oDriver << "//#include <spk/FpErrorChecker.h>" << endl;
  if( myIsEstimate )
  {
     oDriver << "#include <spk/fitIndividual.h>" << endl;
     oDriver << "#include <spk/Optimizer.h>" << endl;
  }
  if( myIsEstimate && myIsStat )
    {
      oDriver << "#include <spk/inverse.h>" << endl;
      oDriver << "#include <spk/indStatistics.h>" << endl;
      oDriver << "#include <spk/printInMatrix.h>" << endl;
      oDriver << "#include <spk/isSymmetric.h>" << endl;
      oDriver << "#include <spk/symmetrize.h>" << endl;
    }
  if( myIsSimulate )
     oDriver << "#include <spk/simulate.h>" << endl;
  oDriver << "#include \"IndData.h\"" << endl;
  oDriver << "#include \"DataSet.h\"" << endl;
  oDriver << endl;
  oDriver << "#include <spk/multiply.h>" << endl;
  oDriver << "#include <spk/cholesky.h>" << endl;

  oDriver << "///////////////////////////////////////////////////////////////////////////////////" << endl;
  oDriver << "//   NONMEM PRED SPECIFIC" << endl;
  oDriver << "#include \"Pred.h\"" << endl;
  oDriver << "#include <spkpred/IndPredModel.h>" << endl;
  oDriver << "#include <cppad/include/CppAD.h>" << endl;
  oDriver << "//" << endl;
  oDriver << "///////////////////////////////////////////////////////////////////////////////////" << endl;
  oDriver << endl;

  oDriver << "using SPK_VA::valarray;" << endl;
  oDriver << "using namespace std;" <<endl;
  oDriver << endl;

  oDriver << "enum RETURN_CODE { SUCCESS=0, CONVERGENCE_FAILURE=1, FILE_ACCESS_FAILURE=2, OTHER_FAILURE };" << endl;
  oDriver << endl;

 
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

  oDriver << "//FpErrorChecker fperror;" << endl;
  oDriver << endl;

  oDriver << "const int nY = " << myRecordNums[0] << ";" << endl;
  oDriver << "DataSet< CppAD::AD<double> > set;" << endl;
  oDriver << endl;
  
  oDriver << "const bool isSimRequested = " << (myIsSimulate? "true":"false") << ";" << endl;
  oDriver << "bool haveCompleteData = " << (myIsSimulate? "false":"true") << ";" << endl;
  if( myIsSimulate )
    oDriver << "const unsigned int seed = " << mySeed << ";" << endl;
  oDriver << endl;

  oDriver << "const bool isOptRequested = " << (myIsEstimate? "true":"false") << ";" << endl;
  oDriver << "bool isOptSuccess  = " << (myIsEstimate? "false":"true") << ";" << endl;
  oDriver << endl;

  oDriver << "const bool isStatRequested = " << (myIsStat? "true":"false") << ";" << endl;
  oDriver << "bool isStatSuccess = " << (myIsStat? "false":"true") << ";" << endl;
  oDriver << endl;

  oDriver << "//////////////////////////////////////////////////////////////////////" << endl;
  oDriver << "//   NONMEM Specific" << endl;
  oDriver << endl;

  oDriver << "const int nTheta = " << myThetaLen << ";" << endl;
  oDriver << endl;

  oDriver << "double c_thetaIn[nTheta] = { ";
  for( int j=0; j<myThetaLen; j++ )
    {
      if( j>0 )
	oDriver << ", ";
      oDriver << pTheta->initial[0][j];
    }
  oDriver << " };" << endl;
  if( myIsSimulate )
    {
      oDriver << "valarray<double> thetaIn ( c_thetaIn, nTheta );" << endl;
    }
  else
    {
      oDriver << "const valarray<double> thetaIn ( c_thetaIn, nTheta );" << endl;
    }
  oDriver << endl;
  
  oDriver << "double c_thetaUp[nTheta] = { ";
  for( int j=0; j<myThetaLen; j++ )
  {
     if( j>0 )
        oDriver << ", ";
     oDriver << pTheta->upper[0][j];
  }
  oDriver << " };" << endl;
  oDriver << "const valarray<double> thetaUp  ( c_thetaUp, nTheta );" << endl;
  oDriver << endl;

  oDriver << "double c_thetaLow[nTheta] = { ";
  for( int j=0; j<myThetaLen; j++ )
  {
     if( j>0 )
	oDriver << ", ";
     oDriver << pTheta->lower[0][j];
  }
  oDriver << " };" << endl;
  oDriver << "const valarray<double> thetaLow ( c_thetaLow, nTheta );" << endl;
  oDriver << endl;

  if( myIsEstimate )
    {
      oDriver << "valarray<double> thetaStep( nTheta );" << endl;
    }
  oDriver << endl;
  oDriver << "valarray<double> thetaOut( nTheta );" << endl;
  oDriver << endl;
  
  // eta
  oDriver << "const int nEta     = " << myEtaLen << ";" << endl;

  // Omega
  oDriver << "const int nOmega   = " << myOmegaElemNum;
  oDriver << "; // #of elements in Omega matrix" << endl;
  oDriver << "const int dimOmega = " << myOmegaDim;
  oDriver << "; // dimension of Omeaga matrix" << endl;
  oDriver << "double c_omegaIn[nOmega] = { "; 
  for( int j=0; j<myOmegaElemNum; j++ )
    {
      if( j>0 )
	oDriver << ", ";
      oDriver << pOmega->initial[0][j];
    }
  oDriver << " };" << endl;
  oDriver << "const valarray<double> omegaIn( c_omegaIn, nOmega );" << endl;
  oDriver << "valarray<double> omegaOut( nOmega );" << endl;
  oDriver << endl;
  oDriver << "//" << endl;
  oDriver << "//////////////////////////////////////////////////////////////" << endl;
  oDriver << endl;

  oDriver << "//////////////////////////////////////////////////////////////////////" << endl;
  oDriver << "//   NONMEM PRED Specific" << endl;
  oDriver << "Pred<CppAD::AD<double> > mPred(&set);" << endl;
  if( myOmegaStruct == Symbol::TRIANGLE )
    oDriver << "enum IndPredModel::covStruct omegaStruct = IndPredModel::FULL;" << endl;
  else 
    oDriver << "enum IndPredModel::covStruct omegaStruct = IndPredModel::DIAGONAL;" << endl;
  
  oDriver << "IndPredModel model( mPred, (int)nTheta, thetaLow, thetaUp, thetaIn, (int)nEta, omegaStruct, omegaIn );" << endl;
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
       oDriver << "   simulate( model, nY, bIn, yOut, seed );" << endl;
       //       if( myIsEstimate )
	 {
	   oDriver << "   for( int j=0; j<nY; j++ )" << endl;
	   oDriver << "   {" << endl;
	   oDriver << "      set.data[0]->SIMDV[j] = yOut[j];" << endl;
	   oDriver << "   }" << endl;
	   oDriver << "   y   = yOut;" << endl;
	 }

       oDriver << "   //FpErrorChecker::check( __LINE__, __FILE__ );" << endl;
       oDriver << "   haveCompleteData = true;" << endl;
       oDriver << "}" << endl;
       oDriver << "catch( const SpkException& e )" << endl;
       oDriver << "{" << endl;
       oDriver << "   oRuntimeError << e << endl;  // Printing out to a file." << endl;
       oDriver << "   cerr << e << endl;    // Printing out to the standard error." << endl; 
       oDriver << "   haveCompleteData = false;" << endl;
       oDriver << "   //FpErrorChecker::clear();" << endl;
       oDriver << "}" << endl;
       oDriver << "catch( ... )" << endl;
       oDriver << "{" << endl;
       oDriver << "   char message[] =\"Unknown exception: failed in data simulation!!!\";" << endl;
       oDriver << "   oRuntimeError << message << endl;  // Printing out to a file." << endl;
       oDriver << "   cerr << message << endl;    // Printing out to the standard error." << endl;
       oDriver << "   haveCompleteData = false;" << endl;
       oDriver << "   //FpErrorChecker::clear();" << endl;
       oDriver << "}" << endl;
    }
  else
    {
      const Symbol* pDV = table->findi( KeyStr::DV );
      if( pDV == Symbol::empty() )
	{
	  // "DV" may be registered as a synonym.  
	  // In that case, have to search through the entries
	  // in the map.
	  const map<const string,Symbol> *t = table->getTable();
          map<const string,Symbol>::const_iterator itr = t->begin();
          for( ; itr != t->end(); itr++ )
	    {
	      if( SymbolTable::key( itr->second.synonym ) == KeyStr::DV )
		{
		  pDV = &itr->second;
		  break;
		}
	    }
	}
      assert( pDV != Symbol::empty() );
      oDriver << "double c_y[] = { ";
	  for( int j=0; j<myRecordNums[0]; j++ )
	    {
	      if( j > 0 )
		oDriver << ", ";
	      oDriver << atof( pDV->initial[0][j].c_str() );
	    }
      oDriver << " };" << endl;
      oDriver << "valarray<double> y( c_y, nY );" << endl; 
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
      oDriver << "     //FpErrorChecker::check( __LINE__, __FILE__ );" << endl;
      oDriver << "     isOptSuccess = true;" << endl;
      oDriver << "  }" << endl;
      oDriver << "  catch( const SpkException& e )" << endl;
      oDriver << "  {" << endl;
      oDriver << "     oRuntimeError << e << endl;" << endl;
      oDriver << "     cerr << e << endl;" << endl;
      oDriver << "     isOptSuccess = false;" << endl;
      oDriver << "     //FpErrorChecker::clear();" << endl;
      oDriver << "  }" << endl;
      oDriver << "  catch( ... )" << endl;
      oDriver << "  {" << endl;
      oDriver << "     char message[] = \"Unknown exception: failed in parameter estimation!!!\";" << endl;
      oDriver << "     oRuntimeError << message << endl;" << endl;
      oDriver << "     cerr << message << endl;" << endl;
      oDriver << "     isOptSuccess = false;" << endl;
      oDriver << "     //FpErrorChecker::clear();" << endl;
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
      oDriver << "        set.data[0]->" << UserStr::RES << "[j] ";
      oDriver << "= y[j] - set.data[0]->" << UserStr::PRED << "[j];" << endl;
      oDriver << "     set.data[0]->" << UserStr::WRES;
      oDriver << " = wres( nY, ROut, set.data[0]->" << UserStr::RES << " ); " << endl;
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
          if( myIsCov )
	    oDriver << "valarray<double> covOut( nB * nB );" << endl;
	  if( myIsStderr )
	    oDriver << "valarray<double> seOut( nB );" << endl;
	  if( myIsCorrelation )
	    oDriver << "valarray<double> correlationOut( nB * nB );" << endl;
	  if( myIsCoefficient )
	    oDriver << "valarray<double> coefficientOut( nB );" << endl;
	  if( myIsConfidence )
	    oDriver << "valarray<double> confidenceOut( 2 * nB );" << endl;
	  if( myIsInvCov )
	    oDriver << "valarray<double> invCovOut( nB * nB );" << endl;
	  
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
          oDriver << "      indStatistics( bOut, " << endl;
	  oDriver << "                     f_bOut," << endl;
          oDriver << "                     R_bOut," << endl;
	  oDriver << "                     RInvOut," << endl;
          oDriver << "                     " << (myIsCov?         "&covOut"        :"NULL") << ", " << endl;
          oDriver << "                     " << (myIsStderr?      "&seOut"         :"NULL") << ", " << endl;
          oDriver << "                     " << (myIsCorrelation? "&correlationOut":"NULL") << ", " << endl;
          oDriver << "                     " << (myIsCoefficient? "&coefficientOut" :"NULL") << ", " << endl;
          oDriver << "                     " << (myIsConfidence?  "&confidenceOut" :"NULL") << " );" << endl;
          oDriver << "      //FpErrorChecker::check( __LINE__, __FILE__ );" << endl;
	  oDriver << "      isStatSuccess = true;" << endl;
          oDriver << "   }" << endl;
          oDriver << "   catch( const SpkException& e )" << endl;
          oDriver << "   {" << endl;
          oDriver << "      oRuntimeError << e << endl;" << endl;
          oDriver << "      cerr << e << endl;" << endl;
          oDriver << "      isStatSuccess = false;" << endl;
          oDriver << "      //FpErrorChecker::clear();" << endl;
          oDriver << "   }" << endl;
          oDriver << "   catch( ... )" << endl;
          oDriver << "   {" << endl;
          oDriver << "      char message[] = \"Unknown exception: failed in statistics calculation!!!\";" << endl;
          oDriver << "      oRuntimeError << message << endl;" << endl;
          oDriver << "      cerr << message << endl;" << endl;
          oDriver << "      isStatSuccess = false;" << endl;
          oDriver << "      //FpErrorChecker::clear();" << endl;
          oDriver << "   }" << endl;
          oDriver << endl;

	  if( myIsInvCov )
	    {
	      oDriver << "   try" << endl;
	      oDriver << "   {" << endl;
	      oDriver << "      invCovOut = inverse( covOut, nB );" << endl;
              oDriver << "      //FpErrorChecker::check( __LINE__, __FILE__ );" << endl;
	      oDriver << "   }" << endl;
	      oDriver << "   catch( const SpkException& e )" << endl;
	      oDriver << "   {" << endl;
	      oDriver << "      oRuntimeError << e << endl;" << endl;
              oDriver << "      cerr << e << endl;" << endl;
	      oDriver << "      isStatSuccess = false;" << endl;
              oDriver << "      //FpErrorChecker::clear();" << endl;
	      oDriver << "   }" << endl;
	      oDriver << "   catch( ... )" << endl;
	      oDriver << "   {" << endl;
	      oDriver << "      char message[] = \"Unknown exception: failed to invert the covariance of the final estimate of individual parameter!!!\";" << endl;
              oDriver << "      oRuntimeError << message << endl;" << endl;
              oDriver << "      cerr << message << endl;" << endl;
	      oDriver << "      isStatSuccess = false;" << endl;
              oDriver << "      //FpErrorChecker::clear();" << endl;
	      oDriver << "   }" << endl;
	    }
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
      oDriver << "oResults << \"<theta_out length=\\\"\" << nTheta << \"\\\">\" << endl;" << endl;
      oDriver << "for( int i=0; i<nTheta; i++ )" << endl;
      oDriver << "{" << endl;
      oDriver << "   oResults << \"<value>\" << thetaOut[i] << \"</value>\" << endl;" << endl;
      oDriver << "}" << endl;
      oDriver << "oResults << \"</theta_out>\" << endl;" << endl;
      // omega 
      oDriver << "oResults << \"<omega_out dimension=\\\"\" << dimOmega << \"\\\" ";
      if( myOmegaStruct == Symbol::TRIANGLE )
	oDriver << "struct=\\\"diagonal\\\">\" << endl;" << endl;
      else
	oDriver << "struct=\\\"block\\\">\" << endl;" << endl;
      oDriver << "for( int i=0; i<nOmega; i++ )" << endl;
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
	  oDriver << "int covElemNum = " << series( 1, 1, myThetaLen ) << ";" << endl;
	  if( myIsCov )
	    {
	      oDriver << "oResults << \"<ind_covariance_out struct=\\\"block\\\" dimension=\\\"\" << nB << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<covElemNum; i++ )" << endl;
	      oDriver << "{" << endl;
	         oDriver << "oResults << \"   <value>\" << covOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;

	      oDriver << "oResults << \"</ind_covariance_out>\" << endl;" << endl;
	    }
	  if( myIsInvCov )
	    {
	      oDriver << "oResults << \"<ind_inverse_covariance_out struct=\\\"block\\\" dimension=\\\"\" << nB << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<covElemNum; i++ )" << endl;
	      oDriver << "{" << endl;
	         oDriver << "oResults << \"   <value>\" << invCovOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;

	      oDriver << "oResults << \"</ind_inverse_covariance_out>\" << endl;" << endl;
	    }
	  if( myIsStderr )
	    {
	      oDriver << "oResults << \"<ind_stderror_out length=\\\"\" << nB << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nB; i++ )" << endl;
	      oDriver << "{" << endl;
	         oDriver << "oResults << \"   <value>\" << seOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;

	      oDriver << "oResults << \"</ind_stderror_out>\" << endl;" << endl;
	    }
	  if( myIsCorrelation )
	    {
	      oDriver << "oResults << \"<ind_correlation_out struct=\\\"block\\\" dimension=\\\"\" << nB << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<covElemNum; i++ )" << endl;
	      oDriver << "{" << endl;
	         oDriver << "oResults << \"   <value>\" << correlationOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;

	      oDriver << "oResults << \"</ind_correlation_out>\" << endl;" << endl;
	    }
	  if( myIsCoefficient )
	    {
	      oDriver << "oResults << \"<ind_coefficient_out length=\\\"\" << nB << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nB; i++ )" << endl;
	      oDriver << "{" << endl;
	         oDriver << "oResults << \"   <value>\" << coefficientOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;

	      oDriver << "oResults << \"</ind_coefficient_out>\" << endl;" << endl;
	    }
	  if( myIsConfidence )
	    {
	      oDriver << "oResults << \"<ind_confidence_out length=\\\"\" << nB*2 << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nB*2; i++ )" << endl;
	      oDriver << "{" << endl;
	         oDriver << "oResults << \"   <value>\" << confidenceOut[i] << \"</value>\" << endl;" << endl;
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
  const Symbol * pID = table->findi(KeyStr::ID);
  assert( pID != Symbol::empty() );
  const int nItems = t->size();
  int nColumns = nItems + myThetaLen-1 + myEtaLen-1 
    - (table->findi(KeyStr::OMEGA) == Symbol::empty()? 0 : 1 )
    - (table->findi(KeyStr::SIGMA) == Symbol::empty()? 0 : 1 )
    - (table->findi(KeyStr::EPS)   == Symbol::empty()? 0 : 1 );

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
  oDriver << "oResults << \"<label name=\\\"" << UserStr::ID << "\\\"/>\" << endl;" << endl;
  oDriver << endl;

  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << "//  Data Set Specific" << endl;
  // ...aaand, following ID is, all the left hand side quantities in the model definition.
  // cntColumns is initialized to 1 because the ID column is already printed out.
  int cntColumns = 1;
  for( cntColumns = 1, pEntry = t->begin(); pEntry!=t->end(); pEntry++ )
    {
      if( pEntry->first != KeyStr::ID 
	  /* && ( find( pLabelBegin, pLabelEnd, pEntry->second.name )==pLabelEnd ) */ )
	{
	  // These ones are not stored by Pred::eval() or the data set.
	  if( pEntry->first != KeyStr::OMEGA && pEntry->first != KeyStr::SIGMA )
	    {
	      whatGoesIn.push_back( pEntry->second.name );
	      
	      if( pEntry->first == KeyStr::THETA )
		{
		  for( int cntTheta=0; cntTheta<myThetaLen; cntTheta++ )
		    {
		      oDriver << "oResults << \"<label name=\\\"";
		      oDriver << pEntry->second.name << "(" << cntTheta+1 << ")";
		      oDriver << "\\\"/>\" << endl;" << endl;	
		      cntColumns++;
		    }
		}
	      else if( pEntry->first == KeyStr::ETA )
		{
		  for( int cntEta=0; cntEta<myEtaLen; cntEta++ )
		    {
		      oDriver << "oResults << \"<label name=\\\"";
		      oDriver << pEntry->second.name << "(" << cntEta+1 << ")";
		      oDriver << "\\\"/>\" << endl;" << endl;		      
		      cntColumns++;
		    }
		}
	      /*
	      else if( pEntry->first == KeyStr::EPS )
		{
		  for( int cntEps=0; cntEps<myEpsLen; cntEps++ )
		    {
		      oDriver << "oResults << \"<label name=\\\"";
		      oDriver << pEntry->second.name << "(" << cntEps+1 << ")";
		      oDriver << "\\\"/>\" << endl;" << endl;		      
		    }
		}
	      */
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
  assert( cntColumns == nColumns );
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
      if( keyWhatGoesIn == KeyStr::SIMDV )
	{
	  oDriver << "   oResults << \"<value ref=\\\"" << *pWhatGoesIn << "\\\"" << ">\" << ";
	  oDriver << "yOut[cnt]";
	  oDriver << " << \"</value>\" << endl;" << endl;
	  cntColumns++;
	}
      else if( keyWhatGoesIn == KeyStr::THETA )
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
      else if( keyWhatGoesIn == KeyStr::ETA )
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
      /*
      else if( keyWhatGoesIn == KeyStr::EPS )
	{
	  // EPS is irrevalent in the individual analysis
	  for( int cntEps=0; cntEps<myEpsLen; cntEps++ )
	    {
	      oDriver << "   oResults << \"<value ref=\"" << *pWhatGoesIn << "\"" << ">\" << ";
	      oDriver << "set.data[0]->" << *pWhatGoesIn << "[j][" << cntEps << "]";
	      oDriver << " << \"</value>\" << endl;" << endl;
	    }
	}
      */
      /*
      else if( keyWhatGoesIn == KeyStr::OMEGA || keyWhatGoesIn == KeyStr::SIGMA )
	{
	  // these shouldn't be the whatGoesIn list!
	}
      */
      else
	{
	  oDriver << "   oResults << \"<value ref=\\\"" << *pWhatGoesIn << "\\\"" << ">\" << ";
          oDriver << "set.data[0]->" << *pWhatGoesIn << "[j]";
	  oDriver << " << \"</value>\" << endl;" << endl;
	  cntColumns++;
	}
    }
  assert( cntColumns == nColumns );
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
  ofstream oDriver ( fDriver_cpp );
  assert( oDriver.good() );

  const Symbol* pTheta = table->findi(KeyStr::THETA);
  const Symbol* pOmega = table->findi(KeyStr::OMEGA);
  const Symbol* pSigma = table->findi(KeyStr::SIGMA);
  const Symbol* pEta   = table->findi(KeyStr::ETA);
  
  oDriver << "#include <iostream>" << endl;
  oDriver << "#include <fstream>" << endl;
  oDriver << "#include <sys/time.h>" << endl;
  oDriver << endl;

  oDriver << "#include <spk/SpkValarray.h>" << endl;
  oDriver << "#include <spk/SpkException.h>" << endl;
  oDriver << "//#include <spk/FpErrorChecker.h>" << endl;

   if( myIsEstimate )
  {
     oDriver << "#include <spk/fitPopulation.h>" << endl;
     oDriver << "#include <spk/Optimizer.h>" << endl;
  }
  if( myIsEstimate && myIsStat )
    {
      oDriver << "#include <spk/popStatistics.h>" << endl;
      oDriver << "#include <spk/inverse.h>" << endl;
    }
  if( myIsSimulate )
     oDriver << "#include <spk/simulate.h>" << endl;
  oDriver << "#include \"IndData.h\"" << endl;
  oDriver << "#include \"DataSet.h\"" << endl;
  oDriver << endl;

  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << "//   NONMEM PRED specific" << endl;
  oDriver << "#include \"Pred.h\"" << endl;
  oDriver << "#include <spkpred/PopPredModel.h>" << endl;
  oDriver << "#include <cppad/include/CppAD.h>" << endl;
  oDriver << "//" << endl;
  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << endl;

  oDriver << "using SPK_VA::valarray;" << endl;
  oDriver << "using namespace std;" << endl;
  oDriver << endl;

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

  oDriver << "//FpErrorChecker fperror;" << endl;
  oDriver << endl;

  oDriver << "const int nPop = " << myPopSize << ";" << endl;
  oDriver << "DataSet< CppAD::AD<double> > set;" << endl;
  oDriver << endl;

  oDriver << "const bool isSimRequested  = " << (myIsSimulate? "true":"false") << ";" << endl;
  oDriver << "bool haveCompleteData      = false;" << endl;
  if( myIsSimulate )
    oDriver << "const unsigned int seed  = " << mySeed << ";" << endl;

  oDriver << "const bool isOptRequested  = " << (myIsEstimate? "true":"false") << ";" << endl;
  oDriver << "bool isOptSuccess          = false;" << endl;
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
  oDriver << "bool isStatSuccess         = false;" << endl;
  if( myIsStat )
    oDriver << "enum PopCovForm covForm  = " << myCovForm << ";" << endl;
  oDriver << endl;

  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << "// NONMEM Sepcific" << endl;
  // theta
  oDriver << "const int nTheta          = " << myThetaLen << "; // length of theta vector" << endl;
  oDriver << "double c_thetaIn[nTheta]  = { ";
  for( int j=0; j<myThetaLen; j++ )
    {
      if( j>0 )
	oDriver << ", ";
      oDriver << pTheta->initial[0][j];
    }
  oDriver << " };" << endl;
  oDriver << "const valarray<double> thetaIn( c_thetaIn, nTheta );" << endl;
  oDriver << "double c_thetaUp[nTheta]  = { ";
  for( int j=0; j<myThetaLen; j++ )
    {
      if( j>0 )
	oDriver << ", ";
      oDriver << pTheta->upper[0][j];
    }
  oDriver << " };" << endl;
  oDriver << "const valarray<double> thetaUp( c_thetaUp, nTheta );" << endl;
  oDriver << "double c_thetaLow[nTheta] = { ";
  for( int j=0; j<myThetaLen; j++ )
    {
      if( j>0 )
	oDriver << ", ";
      oDriver << pTheta->lower[0][j];
    }
  oDriver << " };" << endl;
  oDriver << "const valarray<double> thetaLow( c_thetaLow, nTheta );" << endl;
  oDriver << "valarray<double> thetaOut( nTheta );" << endl;
  oDriver << endl;

  // Omega
  oDriver << "const int nOmega         = " << myOmegaElemNum;
  oDriver << "; // #of elements in Omega matrix" << endl;
  oDriver << "const int dimOmega       = " << myOmegaDim;
  oDriver << "; // dimension of Omeaga matrix" << endl;
  oDriver << "double c_omegaIn[nOmega] = { "; 
  for( int j=0; j<myOmegaElemNum; j++ )
    {
      if( j>0 )
	oDriver << ", ";
      oDriver << pOmega->initial[0][j];
    }
  oDriver << " };" << endl;
  oDriver << "const valarray<double> omegaIn( c_omegaIn, nOmega );" << endl;
  oDriver << "valarray<double> omegaOut( nOmega );" << endl;
  oDriver << endl;
  
  // Sigma
  oDriver << "const int nSigma         = " << mySigmaElemNum;
  oDriver << "; // #of elements in Sigma matrix" << endl;
  oDriver << "const int dimSigma = " << mySigmaDim;
  oDriver << "; // order of Sigma matrix" << endl;
  oDriver << "const int nEps           = dimSigma;" << endl;
  oDriver << "double c_sigmaIn[nSigma] = { ";
  for( int j=0; j<mySigmaElemNum; j++ )
    {
      if( j>0 )
	oDriver << ", ";
      oDriver << pSigma->initial[0][j];
    }
  oDriver << " };" << endl;
  oDriver << "const valarray<double> sigmaIn ( c_sigmaIn, nSigma );" << endl;
  oDriver << "valarray<double> sigmaOut( nSigma );" << endl;
  oDriver << endl;

  oDriver << "const int nEta              = " << myEtaLen << ";" << endl;
  oDriver << "double c_etaIn[nEta] = { ";
  for( int i=0; i<myEtaLen; i++ )
    {
      if( i > 0 )
	oDriver << ", ";
      oDriver << pEta->initial[0][i];
    }
  oDriver << " };" << endl;
  oDriver << "const valarray<double> etaIn ( c_etaIn, nEta );" << endl;
  oDriver << "valarray<double> etaOut( nEta );" << endl;
  oDriver << "valarray<double> etaAllOut( nEta * nPop );" << endl;
  oDriver << endl;
  oDriver << "//" << endl;
  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << endl;

  oDriver << "///////////////////////////////////////////////////////////////////" << endl;
  oDriver << "//   NONMEM PRED Specific" << endl;
  oDriver << "Pred< CppAD::AD<double> > mPred(&set);" << endl;
  if( myOmegaStruct == Symbol::TRIANGLE )
    oDriver << "enum PopPredModel::covStruct omegaStruct = PopPredModel::FULL;" << endl;
  else
    oDriver << "enum PopPredModel::covStruct omegaStruct = PopPredModel::DIAGONAL;" << endl;
  if( mySigmaStruct == Symbol::TRIANGLE )
    oDriver << "enum PopPredModel::covStruct sigmaStruct = PopPredModel::FULL;" << endl;
  else
    oDriver << "enum PopPredModel::covStruct sigmaStruct = PopPredModel::DIAGONAL;" << endl;

  oDriver << "PopPredModel model( mPred," << endl;
  oDriver << "                    nTheta," << endl;
  oDriver << "                    thetaLow," << endl;
  oDriver << "                    thetaUp," << endl;
  oDriver << "                    thetaIn," << endl;
  oDriver << "                    nEta," << endl;
  oDriver << "                    etaIn," << endl;
  oDriver << "                    nEps," << endl;
  oDriver << "                    omegaStruct," << endl;
  oDriver << "                    omegaIn," << endl;
  oDriver << "                    sigmaStruct," << endl;
  oDriver << "                    sigmaIn );" << endl;
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
      oDriver << "   simulate( model, alpIn, N, bLow, bUp, yOut, bOut, seed );" << endl;
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
      oDriver << "   //FpErrorChecker::check( __LINE__, __FILE__ );" << endl;
      oDriver << "   haveCompleteData = true;" << endl;
      oDriver << "}" << endl;
      oDriver << "catch( const SpkException& e )" << endl;
      oDriver << "{" << endl;
      oDriver << "   oRuntimeError << e << endl;  // Printing out to a file." << endl;
      oDriver << "   cerr << e << endl;    // Printing out to the standard error." << endl; 
      oDriver << "   haveCompleteData = false;" << endl;
      oDriver << "   //FpErrorChecker::clear();" << endl;
      oDriver << "}" << endl;
      oDriver << "catch( ... )" << endl;
      oDriver << "{" << endl;
      oDriver << "   char message[] =\"Unknown exception: failed in data simulation!!!\";" << endl;
      oDriver << "   oRuntimeError << message << endl;  // Printing out to a file." << endl;
      oDriver << "   cerr << message << endl;    // Printing out to the standard error." << endl;
      oDriver << "   haveCompleteData = false;" << endl;
      oDriver << "   //FpErrorChecker::clear();" << endl;
      oDriver << "}" << endl;
    }
  else
    {
      const Symbol* pDV = table->findi( KeyStr::DV );
      if( pDV == Symbol::empty() )
	{
	  // "DV" may be registered as a synonym.  
	  // In that case, have to search through the entries
	  // in the map.
	  const map<const string,Symbol> *t = table->getTable();
          map<const string,Symbol>::const_iterator itr = t->begin();
          for( ; itr != t->end(); itr++ )
	    {
	      if( SymbolTable::key( itr->second.synonym ) == KeyStr::DV )
		{
		  pDV = &itr->second;
		  break;
		}
	    }
	}
      assert( pDV != Symbol::empty() );
      oDriver << "double c_y[] = { ";
      for( int i=0; i<myPopSize; i++ )
	{
	  for( int j=0; j<myRecordNums[i]; j++ )
	    {
	      if( !(i==0&&j==0) )
		oDriver << ", ";
	      oDriver << atof( pDV->initial[i][j].c_str() );
	    }
	}
      oDriver << " };" << endl;
      oDriver << "valarray<double> y( c_y, nY );" << endl; 
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
      oDriver << "      //FpErrorChecker::check( __LINE__, __FILE__ );" << endl;
      oDriver << "      isOptSuccess = true;" << endl;
      oDriver << "   }" << endl;
      oDriver << "   catch( const SpkException& e )" << endl;
      oDriver << "   {" << endl;
      oDriver << "      oRuntimeError << e << endl;" << endl;
      oDriver << "      cerr << e << endl;" << endl;
      oDriver << "      isOptSuccess = false;" << endl;
      oDriver << "      //FpErrorChecker::clear();" << endl;
      oDriver << "   }" << endl;
      oDriver << "   catch( ... )" << endl;
      oDriver << "   {" << endl;
      oDriver << "      char message[] = \"Unknown exception: failed in parameter estimation!!!\";" << endl;
      oDriver << "      oRuntimeError << message << endl;" << endl;
      oDriver << "      cerr << message << endl;" << endl;
      oDriver << "      isOptSuccess = false;" << endl;
      oDriver << "      //FpErrorChecker::clear();" << endl;
      oDriver << "   }" << endl;
      oDriver << "   gettimeofday( &optEnd, NULL );" << endl;
      oDriver << "   optTimeSec = difftime( optEnd.tv_sec, optBegin.tv_sec );" << endl;
      oDriver << endl;

      oDriver << "   ///////////////////////////////////////////////////////////////////" << endl;
      oDriver << "   //   NONMEM Specific" << endl;
      oDriver << "   if( isOptSuccess )" << endl;
      oDriver << "   {" << endl;
      oDriver << "      valarray<double> biOut(nB);" << endl;
      oDriver << "      model.getTheta( thetaOut );" << endl;
      oDriver << "      model.getOmega( omegaOut );" << endl;
      oDriver << "      model.getSigma( sigmaOut );" << endl;
      oDriver << "      for( int i=0; i<nPop; i++ )" << endl;
      oDriver << "      {" << endl;
      oDriver << "         model.selectIndividual( i ); " << endl;
      oDriver << "         model.getIndPar( biOut );"   << endl;
      oDriver << "         bOut[ slice( i*nB, nB, 1 ) ] = biOut; " << endl;
      oDriver << "      }" << endl;
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
          if( myIsCov )
	    oDriver << "valarray<double> covOut( nAlp * nAlp );" << endl;
	  if( myIsStderr )
	    oDriver << "valarray<double> seOut( nAlp );" << endl;
	  if( myIsCorrelation )
	    oDriver << "valarray<double> correlationOut( nAlp * nAlp );" << endl;
	  if( myIsCoefficient )
	    oDriver << "valarray<double> coefficientOut( nAlp );" << endl;
	  if( myIsConfidence )
	    oDriver << "valarray<double> confidenceOut( 2 * nAlp );" << endl;
	  if( myIsInvCov )
	    oDriver << "valarray<double> invCovOut( nAlp * nAlp );" << endl;
	  
	  oDriver << "if( isStatRequested && haveCompleteData && isOptSuccess )" << endl;
	  oDriver << "{" << endl;

	  oDriver << "   gettimeofday( &statBegin, NULL );" << endl;
          oDriver << "   try" << endl;
          oDriver << "   {" << endl;
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
          oDriver << "                     " << (myIsCov?         "&covOut"        :"NULL") << ", " << endl;
          oDriver << "                     " << (myIsStderr?      "&seOut"         :"NULL") << ", " << endl;
          oDriver << "                     " << (myIsCorrelation? "&correlationOut":"NULL") << ", " << endl;
          oDriver << "                     " << (myIsCoefficient? "&coefficientOut" :"NULL") << ", " << endl;
          oDriver << "                     " << (myIsConfidence?  "&confidenceOut" :"NULL") << " );" << endl;
          oDriver << "      //FpErrorChecker::check( __LINE__, __FILE__ );" << endl;
          oDriver << "      isStatSuccess = true;" << endl;
          oDriver << "   }" << endl;
          oDriver << "   catch( const SpkException& e )" << endl;
          oDriver << "   {" << endl;
          oDriver << "      oRuntimeError << e << endl;" << endl;
          oDriver << "      cerr << e << endl;" << endl;
          oDriver << "      isStatSuccess = false;" << endl;
          oDriver << "      //FpErrorChecker::clear();" << endl;
          oDriver << "   }" << endl;
          oDriver << "   catch( ... )" << endl;
          oDriver << "   {" << endl;
          oDriver << "      char message[] = \"Unknown exception: failed in statistics calculation!!!\";" << endl;
          oDriver << "      oRuntimeError << message << endl;" << endl;
          oDriver << "      cerr << message << endl;" << endl;
          oDriver << "      isStatSuccess = false;" << endl;
          oDriver << "      //FpErrorChecker::clear();" << endl;
          oDriver << "   }" << endl;

	  if( myIsInvCov )
	    {
	      oDriver << "   try" << endl;
	      oDriver << "   {" << endl;
	      oDriver << "      invCovOut = inverse( covOut, nAlp );" << endl;
              oDriver << "      //FpErrorChecker::check( __LINE__, __FILE__ );" << endl;
	      oDriver << "   }" << endl;
	      oDriver << "   catch( const SpkException& e )" << endl;
	      oDriver << "   {" << endl;
	      oDriver << "      oRuntimeError << e << endl;" << endl;
	      oDriver << "      cerr << e << endl;" << endl;
	      oDriver << "      isStatSuccess = false;" << endl;
              oDriver << "      //FpErrorChecker::clear();" << endl;
	      oDriver << "   }" << endl;
	      oDriver << "   catch( ... )" << endl;
	      oDriver << "   {" << endl;
	      oDriver << "      char message[] = \"Unknown exception: failed to invert the covariance of the final estimate of individual parameter!!!\";" << endl;
              oDriver << "      oRuntimeError << message << endl;" << endl;
              oDriver << "      cerr << message << endl;" << endl;
	      oDriver << "      isStatSuccess = false;" << endl;
              oDriver << "      //FpErrorChecker::clear();" << endl;
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
      oDriver << "oResults << \"<theta_out length=\\\"\" << nTheta << \"\\\">\" << endl;" << endl;
      // theta
      oDriver << "for( int i=0; i<nTheta; i++ )" << endl;
      oDriver << "{" << endl;
      oDriver << "   oResults << \"<value>\" << thetaOut[i] << \"</value>\" << endl;" << endl;
      oDriver << "}" << endl;
      oDriver << "oResults << \"</theta_out>\" << endl;" << endl;
      // Omega 
      oDriver << "oResults << \"<omega_out dimension=\\\"\" << dimOmega << \"\\\" ";
      if( myOmegaStruct == Symbol::TRIANGLE )
	oDriver << "struct=\\\"block\\\">\" << endl;" << endl;
      else
	oDriver << "struct=\\\"diagonal\\\">\" << endl;" << endl;
      oDriver << "for( int i=nTheta; i<nTheta+nOmega; i++ )" << endl;
      oDriver << "{" << endl;
      oDriver << "   oResults << \"<value>\" << omegaOut[i] << \"</value>\" << endl;" << endl;
      oDriver << "}" << endl;
      oDriver << "oResults << \"</omega_out>\" << endl;" << endl;
      // Sigma
      oDriver << "oResults << \"<sigma_out dimension=\\\"\" << dimSigma << \"\\\" ";
      if( mySigmaStruct == Symbol::TRIANGLE )
	oDriver << "struct=\\\"block\\\">\" << endl;" << endl;
      else
	oDriver << "struct=\\\"diagonal\\\">\" << endl;" << endl;
      oDriver << "for( int i=nTheta+nOmega; i<nTheta+nOmega+nSigma; i++ )" << endl;
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
	  oDriver << "int covElemNum = " << series( 1, 1, myThetaLen ) << ";" << endl;
	  if( myIsCov )
	    {
	      oDriver << "oResults << \"<pop_covariance_out struct=\\\"block\\\" dimension=\\\"\" << nB << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<covElemNum; i++ )" << endl;
	      oDriver << "{" << endl;
	      oDriver << "oResults << \"   <value>\" << covOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;
	      
	      oDriver << "oResults << \"</pop_covariance_out>\" << endl;" << endl;
	    }
	  if( myIsInvCov )
	    {
	      oDriver << "oResults << \"<pop_inverse_covariance_out struct=\\\"block\\\" dimension=\\\"\" << nB << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<covElemNum; i++ )" << endl;
	      oDriver << "{" << endl;
	      oDriver << "oResults << \"   <value>\" << invCovOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;
	      
	      oDriver << "oResults << \"</pop_inverse_covariance_out>\" << endl;" << endl;
	    }
	  if( myIsStderr )
	    {
	      oDriver << "oResults << \"<pop_stderror_out length=\\\"\" << nB << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nB; i++ )" << endl;
	      oDriver << "{" << endl;
	      oDriver << "oResults << \"   <value>\" << seOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;
	      
	      oDriver << "oResults << \"</pop_stderror_out>\" << endl;" << endl;
	    }
	  if( myIsCorrelation )
	    {
	      oDriver << "oResults << \"<pop_correlation_out struct=\\\"block\\\" dimension=\\\"\" << nB << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<covElemNum; i++ )" << endl;
	      oDriver << "{" << endl;
	      oDriver << "oResults << \"   <value>\" << correlationOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;
	      
	      oDriver << "oResults << \"</pop_correlation_out>\" << endl;" << endl;
	    }
	  if( myIsCoefficient )
	    {
	      oDriver << "oResults << \"<pop_coefficient_out length=\\\"\" << nB << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nB; i++ )" << endl;
	      oDriver << "{" << endl;
	      oDriver << "oResults << \"   <value>\" << coefficientOut[i] << \"</value>\" << endl;" << endl;
	      oDriver << "}" << endl;
	      
	      oDriver << "oResults << \"</pop_coefficient_out>\" << endl;" << endl;
	    }
	  if( myIsConfidence )
	    {
	      oDriver << "oResults << \"<pop_confidence_out length=\\\"\" << nB*2 << \"\\\">\" << endl;" << endl;
	      oDriver << "for( int i=0; i<nB*2; i++ )" << endl;
	      oDriver << "{" << endl;
	      oDriver << "oResults << \"   <value>\" << confidenceOut[i] << \"</value>\" << endl;" << endl;
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
  const Symbol * pID = table->findi(KeyStr::ID);
  assert( pID != Symbol::empty() );
  const int nItems = t->size();
  int nColumns = nItems + myThetaLen-1 + myEtaLen-1 + myEpsLen-1
    - (table->findi(KeyStr::OMEGA) == Symbol::empty()? 0 : 1 )
    - (table->findi(KeyStr::SIGMA) == Symbol::empty()? 0 : 1 );

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
      if( pEntry->first != KeyStr::ID 
	  /*&& ( find( pLabelBegin, pLabelEnd, pEntry->second.name )==pLabelEnd )*/ )
	{
	  // these three --- theta, omega and sigma --- don't get saved within Pred::eval()
	  // and are already printed out in the reportML earlier during this step.
	  if( pEntry->first != KeyStr::OMEGA && pEntry->first != KeyStr::SIGMA )
	    {
	      whatGoesIn.push_back( pEntry->second.name );
	      
	      if( pEntry->first == KeyStr::THETA )
		{
		  for( int cntTheta=0; cntTheta<myThetaLen; cntTheta++ )
		    {
		      oDriver << "oResults << \"<label name=\\\"";
		      oDriver << pEntry->second.name << "(" << cntTheta+1 << ")";
		      oDriver << "\\\"/>\" << endl;" << endl;		      
		      cntColumns++;
		    }
		}
	      else if( pEntry->first == KeyStr::ETA )
		{
		  for( int cntEta=0; cntEta<myEtaLen; cntEta++ )
		    {
		      oDriver << "oResults << \"<label name=\\\"";
		      oDriver << pEntry->second.name << "(" << cntEta+1 << ")";
		      oDriver << "\\\"/>\" << endl;" << endl;		      
		      cntColumns++;
		    }
		}
	      else if( pEntry->first == KeyStr::EPS )
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
  assert( cntColumns == nColumns );
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
      if( keyWhatGoesIn == KeyStr::SIMDV )
	{
	  oDriver << "   oResults << \"<value ref=\\\"" << *pWhatGoesIn << "\\\"" << ">\" << ";
	  oDriver << "yOut[position]";
	  oDriver << " << \"</value>\" << endl;" << endl;
	  cntColumns++;
	}
      else if( keyWhatGoesIn == KeyStr::THETA )
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
      else if( keyWhatGoesIn == KeyStr::ETA )
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
      else if( keyWhatGoesIn == KeyStr::EPS )
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
      else if( keyWhatGoesIn == KeyStr::OMEGA || keyWhatGoesIn == KeyStr::SIGMA )
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
  assert( cntColumns == nColumns );
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
