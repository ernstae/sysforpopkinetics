#include "NonmemTranslator.h"
#include "explang.h"

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

// n!
static unsigned int factorial( unsigned int n )
{
  if( n == 0 )
    return 0;
  else
    return n + factorial( n-1 );
}

extern int gSpkExpErrors;
extern int gSpkExpLines;
extern SymbolTable * gSpkExpSymbolTable;
extern FILE * gSpkExpOutput;
extern FILE * nm_in;
extern int NM_ACCEPT;
extern int NM_ABORT;

extern "C"{
     int nm_parse(void);
};

NonmemTranslator::NonmemTranslator( DOMDocument* sourceIn, DOMDocument* dataIn )
  : ClientTranslator ( sourceIn, dataIn ),
    X_YES            ( XMLString::transcode("yes") ),
    X_NO             ( XMLString::transcode("no") ),
    X_FIXED          ( XMLString::transcode("fixed") ),
    X_IN             ( XMLString::transcode("in") ),
    X_LOW            ( XMLString::transcode("low") ),
    X_UP             ( XMLString::transcode("up") ),
    X_DIAGONAL       ( XMLString::transcode("diagonal") ),
    X_BLOCK          ( XMLString::transcode("block") ),
    X_VALUE          ( XMLString::transcode("value") ),
    X_STRUCT         ( XMLString::transcode("struct") ),
    X_DIMENSION      ( XMLString::transcode("dimension") ),
    X_LABEL          ( XMLString::transcode("label") ),
    X_IS_ERR_OUT     ( XMLString::transcode("is_stderr_out") ),
    X_IS_CORR_OUT    ( XMLString::transcode("is_correlation_out") ),
    X_IS_COV_OUT     ( XMLString::transcode("is_covariance_out") ),
    X_IS_INV_COV_OUT ( XMLString::transcode("is_inverse_covariance_out") ),
    X_IS_COEF_OUT    ( XMLString::transcode("is_coefficent_out") ),
    X_IS_CONF_OUT    ( XMLString::transcode("is_confidence_out") ),
    X_NONMEM         ( XMLString::transcode("nonmem") ),
    X_POP_ANALYSIS   ( XMLString::transcode("pop_analysis") ),
    X_IND_ANALYSIS   ( XMLString::transcode("ind_analysis") ),
    X_CONSTRAINT     ( XMLString::transcode("constraint") ),
    X_MODEL          ( XMLString::transcode("model") ),
    X_PRED           ( XMLString::transcode("pred") ),
    X_PRESENTATION   ( XMLString::transcode("presentation") ),
    X_TABLE          ( XMLString::transcode("table") ),
    X_SCATTERPLOT    ( XMLString::transcode("scatterplot") ),
    X_COLUMN         ( XMLString::transcode("column") ),
    X_X              ( XMLString::transcode("x") ),
    X_Y              ( XMLString::transcode("y") ),
    X_BY             ( XMLString::transcode("by") ),
    X_APPROXIMATION  ( XMLString::transcode("approximation") ),
    X_FO             ( XMLString::transcode("fo") ),
    X_FOCE           ( XMLString::transcode("foce") ),
    X_LAPLACE        ( XMLString::transcode("laplace") ),
    X_POP_SIZE       ( XMLString::transcode("pop_size" ) ),
    X_IS_ESTIMATION  ( XMLString::transcode("is_estimation") ),
    X_IS_ETA_OUT     ( XMLString::transcode("is_eta_out") ),
    X_IS_RESTART     ( XMLString::transcode("is_restart") ),
    X_DATA_LABELS    ( XMLString::transcode("data_labels") ),
    X_FILENAME       ( XMLString::transcode("filename") ),
    X_NAME           ( XMLString::transcode("name") ),
    X_SYNONYM        ( XMLString::transcode("synonym") ),
    X_THETA          ( XMLString::transcode("theta") ),
    X_LENGTH         ( XMLString::transcode("length") ),
    X_OMEGA          ( XMLString::transcode("omega") ),
    X_SIGMA          ( XMLString::transcode("sigma") ),
    X_SIMULATION     ( XMLString::transcode("simulation") ),
    X_SEED           ( XMLString::transcode("seed") ),
    X_POP_STAT       ( XMLString::transcode("pop_stat") ),
    X_COVARIANCE_FORM( XMLString::transcode("covariance_form") ),
    X_MITR           ( XMLString::transcode("mitr") ),
    X_IND_STAT       ( XMLString::transcode("ind_stat") )
{
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
}
NonmemTranslator::NonmemTranslator( const NonmemTranslator& )
{
}
NonmemTranslator& NonmemTranslator::operator=( const NonmemTranslator& )
{
}
void NonmemTranslator::parseSource()
{
  assert( table.getLabels().size() > 0 );

  enum TARGET {IND, POP};
  enum TARGET target;  
  enum MODEL_SPEC { PRED };

  int pop_size = 1;

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
      target = POP;
      analysis = dynamic_cast<DOMElement*>( pop_analysises->item(0) );
      pop_size = parsePopAnalysis( analysis );
      isAnalysisDone = true;
    }
  else if( ind_analysises->getLength() == 1 )
    {
      target = IND;
      analysis = dynamic_cast<DOMElement*>( ind_analysises->item(0) );
      parseIndAnalysis( analysis );
      isAnalysisDone = true;
      pop_size = 1;
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
  MODEL_SPEC model_spec = PRED;

  bool isPredDone = false;
  char pred_cpp[] = "pred.cpp";
  parsePred( pred, table, pred_cpp );
  isPredDone = true;

  //------------------------------------------------------
  // <presentation>
  //------------------------------------------------------
  // PRED parsing and <xxx_analysis> parsing must have been completed so that the symbol table
  // contains entries for the user defined variables and THETA/OMEGA/SIGMA/ETA, respectively.
  assert( isPredDone );
  assert( isAnalysisDone );
  
  DOMNodeList * presentations = nonmem->getElementsByTagName( X_PRESENTATION );
  assert( presentations->getLength() == 1 );
  DOMElement * presentation = dynamic_cast<DOMElement*>( presentations->item(0) );

  vector<int> recordNums( pop_size );
  Symbol * id = table.findi( "id" );
  assert( id != NULL || id != Symbol::empty() );
  for( int i=0; i<pop_size; i++ )
    {
      recordNums[i] = id->initial[i].size();
    }

  // For PRED, a label allowed in table/scatterplot specification is one of:
  // * THETA, OMEGA, (SIGMA), (ETA) --- <xxx_analysis> should have been done by now
  // * data labels  --- parseData() should have been done by now
  // * user defined variables in PRED definition --- PRED parsing should have been done by now
  // * PRED, RES, WRES
  if( table.findi( "pred" ) == Symbol::empty() )
    table.insertLabel( "pred", "", recordNums );
  if( table.findi( "wres" ) == Symbol::empty() )
    table.insertLabel( "wres", "", recordNums );
  if( table.findi( "res" )  == Symbol::empty() )
    table.insertLabel( "res", "", recordNums );
  
  DOMNodeList * display_tables = presentation->getElementsByTagName( X_TABLE );
  parseTables( display_tables, recordNums );

  DOMNodeList * display_plots  = presentation->getElementsByTagName( X_SCATTERPLOT );
  parseScatterplots( display_plots, recordNums );
}
void NonmemTranslator::generateIndData()
{
}
void NonmemTranslator::parseTables( DOMNodeList * display_tables, vector<int>& recordNums )
{
  const int nDisplayTables = display_tables->getLength();
  assert( nDisplayTables <= 10 );
  for( int i=0; i<nDisplayTables; i++ )
    {
      DOMElement* t = dynamic_cast<DOMElement*>( display_tables->item(i) );
      DOMNodeList * columns = t->getElementsByTagName( X_COLUMN );
      int nColumns = columns->getLength();
      assert( nColumns > 0 );
      for( int j=0; j<nColumns; j++ )
	{
	  DOMElement * column = dynamic_cast<DOMElement*>( columns->item(j) );
          assert( column->hasAttribute( X_LABEL ) );

	  const XMLCh* xml_label = column->getAttribute( X_LABEL );
	  char * str_label;
	  XMLCh * xml_sub = XMLString::replicate( xml_label );
	  int openParan = XMLString::indexOf( xml_label, '(' );
	  if( openParan > 0 )
	    {
	      XMLString::subString( xml_sub, xml_label, 0, openParan );
	      str_label = XMLString::transcode( xml_sub );
	    }
	  else
	    {
	      str_label = XMLString::transcode( xml_label );
	    }
	  assert( table.findi( str_label ) != Symbol::empty() );
	  delete str_label;
	}
    }

  generateIndData( );
}
void NonmemTranslator::parseScatterplots( DOMNodeList* display_plots, vector<int>& recordNums )
{
  const int nDisplayPlots = display_plots->getLength();
  assert( nDisplayPlots <= 20 );
  for( int i=0; i<nDisplayPlots; i++ )
    {
      DOMElement* p = dynamic_cast<DOMElement*>( display_plots->item(i) );
      // x+
      DOMNodeList * x_list = p->getElementsByTagName( X_X );
      int nX = x_list->getLength();
      assert( nX > 0 );
      for( int j=0; j<nX; j++ )
	{
	  DOMElement * x = dynamic_cast<DOMElement*>( x_list->item(j) );
	  assert( x->hasAttribute( X_LABEL ) );

	  const XMLCh* xml_label = x->getAttribute( X_LABEL );
          char * str_label = XMLString::transcode( xml_label );
	  if( table.findi( str_label ) == Symbol::empty() )
	    table.insertLabel( str_label, "", recordNums );
	  delete str_label;
	}

      // y+
      DOMNodeList * y_list = p->getElementsByTagName( X_Y );
      int nY = y_list->getLength();
      assert( nY > 0 );
      for( int j=0; j<nY; j++ )
	{
	  DOMElement * y = dynamic_cast<DOMElement*>( y_list->item(j) );
	  assert( y->hasAttribute( X_LABEL ) );

	  const XMLCh* xml_label = y->getAttribute( X_LABEL );
          char * str_label = XMLString::transcode( xml_label );
	  if( table.findi( str_label ) == Symbol::empty() )
	    table.insertLabel( str_label, "", recordNums );
	  delete str_label;
	}

      // by*
      DOMNodeList * by_list = p->getElementsByTagName( X_BY );
      int nBy = by_list->getLength();
      for( int j=0; j<nBy; j++ )
	{
	  DOMElement * by = dynamic_cast<DOMElement*>( by_list->item(j) );
	  assert( by->hasAttribute( X_LABEL ) );

	  const XMLCh* xml_label = by->getAttribute( X_LABEL );
          char * str_label = XMLString::transcode( xml_label );
	  if( table.findi( str_label ) == Symbol::empty() )
	    table.insertLabel( str_label, "", recordNums );
	  delete str_label;
	}    
    }
}
int NonmemTranslator::parsePopAnalysis( DOMElement* pop_analysis )
{
  enum APPROX {FO, FOCE, LAPLACE};
  
  //================================================================================
  // Required attributes
  //================================================================================
  // * approximation = {fo, foce, laplace}
  // * pop_size
  // * is_estimation = {yes, no}
  assert( pop_analysis->hasAttribute( X_APPROXIMATION ) );
  const XMLCh * xml_approx = pop_analysis->getAttribute( X_APPROXIMATION );

  APPROX approximation;
  if( XMLString::equals( xml_approx, X_FO ) )
    approximation = FO;
  else if( XMLString::equals( xml_approx, X_FOCE ) )
    approximation = FOCE;
  else if( XMLString::equals( xml_approx, X_LAPLACE ) )
    approximation = LAPLACE;
  else
    {
      assert( false );
    }

  assert( pop_analysis->hasAttribute( X_POP_SIZE ) );
  const XMLCh * xml_pop_size = pop_analysis->getAttribute( X_POP_SIZE );
  unsigned int pop_size = 0;
  if( !XMLString::textToBin( xml_pop_size, pop_size ) )
    {
      assert( false );
    }

  assert( pop_analysis->hasAttribute( X_IS_ESTIMATION ) );
  const XMLCh * xml_is_estimation = pop_analysis->getAttribute( X_IS_ESTIMATION );
  bool is_estimation = ( XMLString::equals( xml_is_estimation, X_YES )? true : false );

  //================================================================================
  // Optional attributes
  //================================================================================
  // * is_eta_out = {yes, "no"}
  // * is_restart = {"yes", no}
  bool is_eta_out = false;
  const XMLCh * xml_is_eta_out;
  if( pop_analysis->hasAttribute( X_IS_ETA_OUT ) )
    {
      xml_is_eta_out = pop_analysis->getAttribute( X_IS_ETA_OUT );
      is_eta_out = ( XMLString::equals( xml_is_eta_out, X_YES )? true : false );
    }

  bool is_restart = true;
  const XMLCh * xml_is_restart;
  if( pop_analysis->hasAttribute( X_IS_RESTART ) )
    {
      xml_is_restart = pop_analysis->getAttribute( X_IS_RESTART );
      is_restart = ( XMLString::equals( xml_is_restart, X_YES )? true : false );
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
     // Required <data_labels> attributes
     // * filename = CDATA
    assert( data_labels->hasAttribute( X_FILENAME ) );
     const XMLCh* xml_data_filename = data_labels->getAttribute( X_FILENAME );
     assert( XMLString::stringLen( xml_data_filename ) > 0 );

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
	 Symbol * name = table.findi( c_name );
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
  unsigned int theta_len = 0;
  if( !XMLString::textToBin( xml_theta_len, theta_len ) )
    {
      assert( theta_len > 0 );
    }
  Symbol * sym_theta = table.insertNMVector( "theta", theta_len );
  {
    //<in>
    DOMNodeList * theta_in_list = theta->getElementsByTagName( X_IN );
    assert( theta_in_list->getLength() == 1 );
    DOMElement * theta_in = dynamic_cast<DOMElement*>( theta_in_list->item(0) );

    DOMNodeList * value_list = theta_in->getElementsByTagName( X_VALUE );
    assert( theta_len == value_list->getLength() );
    for( int i=0; i<theta_len; i++ )
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
    assert( theta_len == value_list->getLength() );
    for( int i=0; i<theta_len; i++ )
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
    assert( theta_len == value_list->getLength() );
    for( int i=0; i<theta_len; i++ )
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
  }

  DOMNodeList * omega_list = pop_analysis->getElementsByTagName( X_OMEGA );
  int nOmegaSpecs = omega_list->getLength();
  assert( nOmegaSpecs == 1 );// v0.1 supports only one (full) Omega specification
  DOMElement * omega = dynamic_cast<DOMElement*>( omega_list->item(0) );
  assert( omega->hasAttribute( X_DIMENSION ) );
  const XMLCh* xml_omega_dim = omega->getAttribute( X_DIMENSION );
  unsigned int omegaDim = 0;
  if( !XMLString::textToBin( xml_omega_dim, omegaDim ) )
    {
      assert( omegaDim > 0 );
    }

  assert( omega->hasAttribute( X_STRUCT ) );
  const XMLCh* xml_omega_struct = omega->getAttribute( X_STRUCT );
  enum Symbol::Structure omega_structure;
  int omegaElemNum = 0;
  if( XMLString::equals( xml_omega_struct, X_DIAGONAL ) )
    {
      omega_structure = Symbol::DIAGONAL;
      omegaElemNum = omegaDim;
    }
  else if( XMLString::equals( xml_omega_struct, X_BLOCK ) )
    {
      omega_structure = Symbol::TRIANGLE;
      omegaElemNum = factorial( omegaDim );
    }
  else
    assert( false );
  Symbol * sym_omega = table.insertNMMatrix( "omega", omega_structure, omegaDim );
  {
    //<in>
    DOMNodeList * omega_in_list = omega->getElementsByTagName( X_IN );
    assert( omega_in_list->getLength() == 1 );
    DOMElement * omega_in = dynamic_cast<DOMElement*>( omega_in_list->item(0) );

    DOMNodeList * value_list = omega_in->getElementsByTagName( X_VALUE );
    assert( omegaElemNum == value_list->getLength() );
    for( int i=0; i<omegaElemNum; i++ )
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

  DOMNodeList * simga_list = pop_analysis->getElementsByTagName( X_SIGMA );
  int nSigmaSpecs = simga_list->getLength();
  assert( nSigmaSpecs == 1 );// v0.1 supports only one (full) Sigma specification
  DOMElement * sigma = dynamic_cast<DOMElement*>( simga_list->item(0) );
  assert( sigma->hasAttribute( X_DIMENSION ) );
  const XMLCh* xml_sigma_dim = sigma->getAttribute( X_DIMENSION );
  unsigned int sigmaDim = 0;
  if( !XMLString::textToBin( xml_sigma_dim, sigmaDim ) )
    {
      assert( sigmaDim > 0 );
    }

  assert( sigma->hasAttribute( X_STRUCT ) );
  const XMLCh* xml_sigma_struct = sigma->getAttribute( X_STRUCT );
  enum Symbol::Structure sigma_structure;
  int sigmaElemNum = 0;
  if( XMLString::equals( xml_sigma_struct, X_DIAGONAL ) )
    {
      sigma_structure = Symbol::DIAGONAL;
      sigmaElemNum = sigmaDim;
    }
  else if( XMLString::equals( xml_sigma_struct, X_BLOCK ) )
    {
      sigma_structure = Symbol::TRIANGLE;
      sigmaElemNum = factorial( sigmaDim );
    }
  else
    assert( false );
  Symbol * sym_sigma = table.insertNMMatrix( "sigma", sigma_structure, sigmaDim ); 
  {
    //<in>
    DOMNodeList * sigma_in_list = sigma->getElementsByTagName( X_IN );
    assert( sigma_in_list->getLength() == 1 );
    DOMElement * sigma_in = dynamic_cast<DOMElement*>( sigma_in_list->item(0) );

    DOMNodeList * value_list = sigma_in->getElementsByTagName( X_VALUE );
    assert( sigmaElemNum == value_list->getLength() );
    for( int i=0; i<sigmaElemNum; i++ )
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
  // REVISIT - Sachiko 01/22/04
  // The boundary values must be computed automatically too.
  //-----------------------------------------------------------
  const int eta_len = theta_len;
  char etaDefault[] = "0.0";
  Symbol * sym_eta = table.insertNMVector( "eta", eta_len );
  sym_eta->initial[0] = etaDefault;
  sym_eta->fixed[0] = false;

  //================================================================================
  // Optional elements
  //================================================================================
  // <description>  --- ignore!
  // <simulation>
  // <pop_stat>
  bool isSimulate = false;
  unsigned int seed = 0;
  DOMNodeList * simulations = pop_analysis->getElementsByTagName( X_SIMULATION );
  if( simulations->getLength() > 0 )
    {
      assert( simulations->getLength() == 1 );
      isSimulate = true;
      DOMElement* simulation = dynamic_cast<DOMElement*>( simulations->item(0) );
      assert( simulation->hasAttribute( X_SEED ) );
      const XMLCh* xml_seed = simulation->getAttribute( X_SEED );
      if( !XMLString::textToBin( xml_seed, seed ) )
	{
	  assert( false );
	}
    }

  DOMNodeList * pop_stat_list = pop_analysis->getElementsByTagName( X_POP_STAT );
  if( pop_stat_list->getLength() > 0 )
    {
      assert( pop_stat_list->getLength() == 1 );
      DOMElement * pop_stat = dynamic_cast<DOMElement*>( pop_stat_list->item(0) );
      bool is_stderr_out             = false;//default
      bool is_correlation_out        = false;//default
      bool is_covariance_out         = false;//default
      bool is_inverse_covariance_out = false;//default
      bool is_confidence_out         = false;//default
      bool is_coefficient_out        = false;//default
      assert( pop_stat->hasAttribute( X_COVARIANCE_FORM ) );
      const XMLCh* cov_form = pop_stat->getAttribute( X_COVARIANCE_FORM ); // r, rsr, s

      if( pop_stat->hasAttribute( X_IS_ERR_OUT ) )
	{
	  const XMLCh* xml_stderr = pop_stat->getAttribute( X_IS_ERR_OUT );
	  is_stderr_out = (XMLString::equals( xml_stderr, X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( X_IS_CORR_OUT ) )
	{
	  const XMLCh* xml_correlation = pop_stat->getAttribute( X_IS_CORR_OUT );
	  is_correlation_out = (XMLString::equals( xml_correlation, X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( X_IS_COV_OUT ) )
	{
	  const XMLCh* xml_cov = pop_stat->getAttribute( X_IS_COV_OUT );
	  is_covariance_out = (XMLString::equals( xml_cov, X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( X_IS_INV_COV_OUT ) )
	{
	  const XMLCh* xml_inv_cov = pop_stat->getAttribute( X_IS_INV_COV_OUT );
	  is_inverse_covariance_out = (XMLString::equals( xml_inv_cov, X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( X_IS_CONF_OUT ) )
	{
	  const XMLCh* xml_conf = pop_stat->getAttribute( X_IS_CONF_OUT );
	  is_confidence_out = (XMLString::equals( xml_conf, X_YES )? true : false );
	}

      if( pop_stat->hasAttribute( X_IS_COEF_OUT ) )
	{
	  const XMLCh* xml_coef = pop_stat->getAttribute( X_IS_COEF_OUT );
	  is_coefficient_out = (XMLString::equals( xml_coef, X_YES )? true : false );
	}
    }
  return pop_size;
}
void NonmemTranslator::parseIndAnalysis( DOMElement* ind_analysis )
{
  //================================================================================
  // Required attributes
  //================================================================================
  // * is_estimation = {yes, no}
  const XMLCh * xml_is_estimation = ind_analysis->getAttribute( X_IS_ESTIMATION );
  assert( XMLString::stringLen( xml_is_estimation ) > 0 );
  bool is_estimation = ( XMLString::equals( xml_is_estimation, X_YES )? true : false );

  //================================================================================
  // Optional attributes
  //================================================================================
  // * mitr   --- required when is_estimation == "yes"
  // * is_restart = {"yes", no}
  bool is_restart = true;
  unsigned int mitr = 0;
  const XMLCh * xml_is_restart = ind_analysis->getAttribute( X_IS_RESTART );
  if( XMLString::stringLen( xml_is_restart ) )
      is_restart = ( XMLString::equals( xml_is_restart, X_YES )? true : false );
  if( is_estimation )
    {
      const XMLCh* xml_mitr = ind_analysis->getAttribute( X_MITR );
      assert( XMLString::stringLen( xml_mitr ) > 0 );
      if( !XMLString::textToBin( xml_mitr, mitr ) )
	assert( xml_mitr > 0 );
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
     // Required <data_labels> attributes
     // * filename = CDATA
     const XMLCh* xml_data_filename = data_labels->getAttribute( X_FILENAME );
     assert( XMLString::stringLen( xml_data_filename ) > 0 );

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
	 Symbol * name = table.findi( c_name );
	 assert( name != Symbol::empty() );
	 delete c_name;

	 // optional attribute
	 // * synonym
         const XMLCh* xml_synonym = xml_label->getAttribute( X_SYNONYM );
	 if( XMLString::stringLen( xml_synonym ) > 0 )
	   {
	     char * c_synonym = XMLString::transcode( xml_synonym );
	     // register the synonym to the symbol table
	     name->synonym = c_synonym;
	     delete c_synonym;
	   }
       }
  }

  DOMNodeList * theta_list = ind_analysis->getElementsByTagName( X_THETA );
  assert( theta_list->getLength() == 1 );
  DOMElement * theta = dynamic_cast<DOMElement*>( theta_list->item(0) );
  const XMLCh* xml_theta_len = theta->getAttribute( X_LENGTH );
  assert( XMLString::stringLen( xml_theta_len ) > 0 );
  unsigned int theta_len = 0;
  if( !XMLString::textToBin( xml_theta_len, theta_len ) )
    {
      assert( theta_len > 0 );
    }
  Symbol * sym_theta = table.insertNMVector( "theta", theta_len );
  {
    //<in>
    DOMNodeList * theta_in_list = theta->getElementsByTagName( X_IN );
    assert( theta_in_list->getLength() == 1 );
    DOMElement * theta_in = dynamic_cast<DOMElement*>( theta_in_list->item(0) );

    DOMNodeList * value_list = theta_in->getElementsByTagName( X_VALUE );
    assert( theta_len == value_list->getLength() );
    for( int i=0; i<theta_len; i++ )
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
    assert( theta_len == value_list->getLength() );
    for( int i=0; i<theta_len; i++ )
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
    assert( theta_len == value_list->getLength() );
    for( int i=0; i<theta_len; i++ )
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
  }

  DOMNodeList * omega_list = ind_analysis->getElementsByTagName( X_OMEGA );
  int nOmegaSpecs = omega_list->getLength();
  assert( nOmegaSpecs == 1 );// v0.1 supports only one (full) Omega specification
  DOMElement * omega = dynamic_cast<DOMElement*>( omega_list->item(0) );
  const XMLCh* xml_omega_dim = omega->getAttribute( X_DIMENSION );
  assert( XMLString::stringLen( xml_omega_dim ) > 0 );
  unsigned int omegaDim = 0;
  if( !XMLString::textToBin( xml_omega_dim, omegaDim ) )
    {
      assert( omegaDim > 0 );
    }
  const XMLCh* xml_omega_struct = omega->getAttribute( X_STRUCT );
  assert( XMLString::stringLen( xml_omega_struct ) > 0 );
  enum Symbol::Structure omega_structure;
  int omegaElemNum = 0;
  if( XMLString::equals( xml_omega_struct, X_DIAGONAL ) )
    {
      omega_structure = Symbol::DIAGONAL;
      omegaElemNum = omegaDim;
    }
  else if( XMLString::equals( xml_omega_struct, X_BLOCK ) )
    {
      omega_structure = Symbol::TRIANGLE;
      omegaElemNum = factorial( omegaDim );
    }
  else
    assert( false );
  Symbol * sym_omega = table.insertNMMatrix( "omega", omega_structure, omegaDim );
  {
    //<in>
    DOMNodeList * omega_in_list = omega->getElementsByTagName( X_IN );
    assert( omega_in_list->getLength() == 1 );
    DOMElement * omega_in = dynamic_cast<DOMElement*>( omega_in_list->item(0) );

    DOMNodeList * value_list = omega_in->getElementsByTagName( X_VALUE );
    assert( omegaElemNum == value_list->getLength() );
    for( int i=0; i<omegaElemNum; i++ )
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

  //================================================================================
  // Optional elements
  //================================================================================
  // <description>  --- ignore!
  // <simulation>
  // <ind_stat>
  bool isSimulate = false;
  unsigned int seed = 0;
  DOMNodeList * simulations = ind_analysis->getElementsByTagName( X_SIMULATION );
  if( simulations->getLength() > 0 )
    {
      assert( simulations->getLength() == 1 );
      isSimulate = true;
      DOMElement* simulation = dynamic_cast<DOMElement*>( simulations->item(0) );
      const XMLCh* xml_seed = simulation->getAttribute( X_SEED );
      assert( XMLString::stringLen( xml_seed ) > 0 );
      if( !XMLString::textToBin( xml_seed, seed ) )
	{
	  assert( false );
	}
    }

  DOMNodeList * ind_stat_list = ind_analysis->getElementsByTagName( X_IND_STAT );
  if( ind_stat_list->getLength() > 0 )
    {
      assert( ind_stat_list->getLength() == 1 );
      DOMElement * ind_stat = dynamic_cast<DOMElement*>( ind_stat_list->item(0) );
      bool is_stderr_out             = false;//default
      bool is_correlation_out        = false;//default
      bool is_covariance_out         = false;//default
      bool is_inverse_covariance_out = false;//default
      bool is_confidence_out         = false;//default
      bool is_coefficient_out        = false;//default

      const XMLCh* xml_stderr = ind_stat->getAttribute( X_IS_ERR_OUT );
      if( XMLString::stringLen( xml_stderr ) > 0 )
      {
	is_stderr_out = (XMLString::equals( xml_stderr, X_YES )? true : false );
      }
      const XMLCh* xml_correlation = ind_stat->getAttribute( X_IS_CORR_OUT );
      if( XMLString::stringLen( xml_correlation ) > 0 )
      {
	is_correlation_out = (XMLString::equals( xml_correlation, X_YES )? true : false );
      }
      const XMLCh* xml_cov = ind_stat->getAttribute( X_IS_COV_OUT );
      if( XMLString::stringLen( xml_cov ) > 0 )
      {
	is_covariance_out = (XMLString::equals( xml_cov, X_YES )? true : false );
      }
      const XMLCh* xml_inv_cov = ind_stat->getAttribute( X_IS_INV_COV_OUT );
      if( XMLString::stringLen( xml_inv_cov ) > 0 )
      {
	is_inverse_covariance_out = (XMLString::equals( xml_inv_cov, X_YES )? true : false );
      }
      const XMLCh* xml_conf = ind_stat->getAttribute( X_IS_CONF_OUT );
      if( XMLString::stringLen( xml_conf ) > 0 )
      {
	is_confidence_out = (XMLString::equals( xml_conf, X_YES )? true : false );
      }
      const XMLCh* xml_coef = ind_stat->getAttribute( X_IS_COEF_OUT );
      if( XMLString::stringLen( xml_coef ) > 0 )
      {
	is_coefficient_out = (XMLString::equals( xml_coef, X_YES )? true : false );
      }
    }
}
void NonmemTranslator::parsePred( DOMElement * pred, SymbolTable & table, char pred_cpp[] )
{
  char pred_stored[] = "pred.fortran";
  
  char * c_equations = NULL;
  const XMLCh* xml_equations = pred->getFirstChild()->getNodeValue();
  int size = XMLString::stringLen( xml_equations );
  if( size > 0 )
    c_equations = XMLString::transcode( xml_equations );

  nm_in = fopen( pred_stored, "w" );
  fprintf( nm_in, "%s", c_equations );
  fclose( nm_in );
  delete c_equations;

  nm_in = fopen( pred_stored, "r" );
  gSpkExpOutput = fopen( pred_cpp, "w" );
  gSpkExpSymbolTable = &table;

  try{
    nm_parse();
  }
  catch( ... )
    {
      assert( false );
    }

  fclose( nm_in );
  fclose( gSpkExpOutput );
  remove( pred_stored );
}
