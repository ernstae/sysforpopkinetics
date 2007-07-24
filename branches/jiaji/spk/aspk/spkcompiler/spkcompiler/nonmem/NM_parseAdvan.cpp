/**
 * @file NM_parseAdvan.cpp
 * Define NonmemTranslator::parseAdvan().
 */
#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"
#include "explang.h"
#include "countStrInLhs.h"

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;



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
  if( advan != ADVAN6 )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"ADVAN%d is not supported!", (int)advan );
      throw SpkCompilerException( SpkCompilerError::ASPK_USER_ERR, m, __LINE__, __FILE__ );
    }

  DOMNodeList * comp_models = model->getElementsByTagName( XML.X_COMP_MODEL );
  DOMNodeList * pks         = model->getElementsByTagName( XML.X_PK );
  DOMNodeList * errors      = model->getElementsByTagName( XML.X_ERROR );
  DOMNodeList * diffeqns    = model->getElementsByTagName( XML.X_DIFFEQN );
  DOMElement  * comp_model  = NULL;
  DOMElement  * diffeqn     = NULL;
  DOMElement  * pk          = NULL;
  DOMElement  * error       = NULL;

  //
  // Get <pk> and <error> are needed for any ADVAN.
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
      snprintf( mess, 
		SpkCompilerError::maxMessageLen(),
		"Missing <pk> and/or <error>!" );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
			      mess, __LINE__, __FILE__ );
      throw e;
    }
  
  pk      = dynamic_cast<DOMElement*>( pks->item(0) );
  if( !pk )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"<%s> block is missing!", XML.C_PK );
      throw SpkCompilerException( SpkCompilerError::ASPK_SOURCEML_ERR,
				  m, __LINE__, __FILE__ );
    }
  error   = dynamic_cast<DOMElement*>( errors->item(0) );
  if( !error )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"<%s> block is missing!", XML.C_ERROR );
      throw SpkCompilerException( SpkCompilerError::ASPK_SOURCEML_ERR,
				  m, __LINE__, __FILE__ );
    }
  
  // ADVAN 5-9 needs an additional model, the compartmental model definition <comp_model>.
  if( advan >= ADVAN5 && advan <= ADVAN9 )
    {
      if( comp_models->getLength() < 1 )
	{
	  char mess[ SpkCompilerError::maxMessageLen() ];
	  snprintf( mess, 
		    SpkCompilerError::maxMessageLen(),
		    "<comp_model> must be specified when ADVAN 5-9 is used!" );
	  SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
				  mess, __LINE__, __FILE__ );
	  throw e;
	}
      comp_model = dynamic_cast<DOMElement*>( comp_models->item(0) );

      // further, ADVAN 6, 8 and 9 needs the differential equations, <diffeqn>.
      if( advan == 6 || advan == 8 || advan == 9 )
	{
	  if( diffeqns->getLength() < 1 )
	    {
	      char mess[ SpkCompilerError::maxMessageLen() ];
	      snprintf( mess, 
			SpkCompilerError::maxMessageLen(),
			"<diffeqn> must be specified when ADVAN 6, 8 or 9 is used!" );
	      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR,
				      mess, __LINE__, __FILE__ );
	      throw e;
	    }
	  diffeqn = dynamic_cast<DOMElement*>( diffeqns->item(0) );
	}
    }

  //assert( table->find( nonmem::EVID ) );
  assert( table->find( nonmem::AMT ) );
  assert( table->find( nonmem::TIME ) );
  //assert( table->find( nonmem::"RATE" ) );       // optional
  //assert( table->find( nonmem::CMT ) );  // optional
  //assert( table->find( nonmem::PCMT ) ); // optional

  // These may/should appear on the left hand side of assignment in the user's equations.
  // In that case, the variable would be registered to the symbol table
  // as a user variable automatically.  But, we want it to be a system
  // variable and also read-write.  So, we register in the way
  // we require in advance.
  table->insertScalar( nonmem::F, Symbol::SYSTEM, Symbol::READWRITE );
  table->insertScalar( nonmem::Y, Symbol::SYSTEM, Symbol::READWRITE );
 
  double relTol;
  if( advan == 6 || advan == 8 || advan == 9 )
    {
      unsigned sig_digits = 0;
      if( model->hasAttribute( XML.X_TOLERANCE ) )
	{
	  const XMLCh *x_tol = model->getAttribute( XML.X_TOLERANCE );
	  if( !XMLString::textToBin( x_tol, sig_digits ) )
	    {
	      char m[ SpkCompilerError::maxMessageLen() ];
	      snprintf( m, 
			SpkCompilerError::maxMessageLen(),
			"Invalid %s: %s", XML.C_TOLERANCE, XMLString::transcode( x_tol ) );
	      throw SpkCompilerException( SpkCompilerError::ASPK_USER_ERR, m, __LINE__, __FILE__ );
	    }
	  relTol = pow( 10.0, -(sig_digits+1.0) );
	}

      parseCompModel( comp_model, relTol );
      assert( myCompModel != NULL );
      
      // P(n), A(m) and DADT(m) may appear on the left hand side of
      // assignment statements in model equations. 
      // n = NPARAMETERS (assumed so for now), m = NCOMPARTMENTS+1.
      // Currently our expression compiler cannot figure out
      // the size of a vector as it parses through.
      // So, provide the vectors before hand. 
      //  
      table->insertVector( nonmem::P,    myCompModel->getNParameters(),   Symbol::SYSTEM, Symbol::READWRITE );
      table->insertVector( nonmem::DADT, myCompModel->getNCompartments(), Symbol::SYSTEM, Symbol::READWRITE );
      table->insertVector( nonmem::A,    myCompModel->getNCompartments(), Symbol::SYSTEM, Symbol::READONLY );

      // REVISIT - Sachiko 08/09/2005
      // For ODE models (ADVAN 6, 8 and 9),
      // T (the continuous time) is required.
      // The values is set by the ODE solver.
      table->insertScalar( nonmem::T,      Symbol::SYSTEM, Symbol::READONLY );
      table->insertScalar( nonmem::TSCALE, Symbol::SYSTEM, Symbol::READWRITE );
      table->insertScalar( nonmem::FO,     Symbol::SYSTEM, Symbol::READWRITE ); // ef-oh
      table->insertScalar( nonmem::F0,     Symbol::SYSTEM, Symbol::READWRITE ); // ef-zero
      table->insertScalar( nonmem::S0,     Symbol::SYSTEM, Symbol::READWRITE ); // es-zero
      for( int i=0; i<myCompModel->getNCompartments(); i++ )
	{
	  char tmp_s[ 32 ], tmp_f[ 32 ], tmp_r[ 32 ], tmp_d[ 32 ], tmp_alag[ 32 ];
	  snprintf( tmp_r,    
		    SpkCompilerError::maxMessageLen(),
		    "%s%d", nonmem::R.c_str(), i+1 );    // infusion rate for i-th comp
	  snprintf( tmp_d,    
		    SpkCompilerError::maxMessageLen(),
		    "%s%d", nonmem::D.c_str(), i+1 );    // infusion duration for i-th comp
	  snprintf( tmp_alag, 
		    SpkCompilerError::maxMessageLen(),
		    "%s%d", nonmem::ALAG.c_str(), i+1 ); // absorption lag time for i-th comp
	  snprintf( tmp_s,    
		    SpkCompilerError::maxMessageLen(),
		    "%s%d", nonmem::S.c_str(), i+1 );    // scale for i-th comp
	  snprintf( tmp_f,    
		    SpkCompilerError::maxMessageLen(),
		    "%s%d", nonmem::F.c_str(), i+1 ); 

	  table->insertScalar( tmp_r,    Symbol::SYSTEM, Symbol::READWRITE );
	  table->insertScalar( tmp_d,    Symbol::SYSTEM, Symbol::READWRITE ); 
	  table->insertScalar( tmp_alag, Symbol::SYSTEM, Symbol::READWRITE ); 
	  table->insertScalar( tmp_s,    Symbol::SYSTEM, Symbol::READWRITE );
	  table->insertScalar( tmp_f,    Symbol::SYSTEM, Symbol::READWRITE ); 
	}

      parsePK( pk );
      parseDiffEqn( diffeqn );
      parseError( error );

    }
}
//
// Parse <comp_model> which defines a compartmental model.
// #of compartments, #of equilibrim compartments, #of basic PK parameters
// are determined.  For each compartment, a number of attributes will be
// determined.
// The return value is the number of compartments defined by the user
// plus the output compartment.
// 
// Post-condition:
// myCompModel->getNCompartments() > 0
// myCompModel->getNEquilibrim() >= 0
// myCompModel->getNParameters() >= 0
//
// For each compartment, 
// 
int NonmemTranslator::parseCompModel( const DOMElement* comp_model, double relTol )
{
  const DOMNodeList * compartment_list = comp_model->getElementsByTagName( XML.X_COMPARTMENT ); 

  // #of compartments other than the ouput compartment.
  // This value must match the number of <compartment> sub-entries.
  unsigned int nUserCompartments; /* User specified #of compartments (this doesn't include the output comp)*/
  if( comp_model->hasAttribute( XML.X_NCOMPARTMENTS ) )
    {
      const XMLCh* x_ncompartments = comp_model->getAttribute( XML.X_NCOMPARTMENTS );
      if( ! XMLString::textToBin( x_ncompartments, nUserCompartments ) )
	{
          char mess[ SpkCompilerError::maxMessageLen() ];
          snprintf( mess,
		    SpkCompilerError::maxMessageLen(),
		    "Invalid <%s::%s> attribute value: \"%s\"", XML.C_COMP_MODEL, XML.C_NCOMPARTMENTS,
		    XMLString::transcode(x_ncompartments) );
          SpkCompilerException e( SpkCompilerError::ASPK_USER_ERR, mess, __LINE__, __FILE__);
          throw e;
	} 
      if( nUserCompartments != compartment_list->getLength() )
	{
          char mess[ SpkCompilerError::maxMessageLen() ];
          snprintf( mess,
		    SpkCompilerError::maxMessageLen(),
		    "The number of compartments specified in <%s::%s> does not match with the number of <%s> incidents!",
		    XML.C_COMP_MODEL, XML.C_NCOMPARTMENTS, XML.C_COMPARTMENT );
          throw SpkCompilerException( SpkCompilerError::ASPK_USER_ERR, mess, __LINE__, __FILE__ );
	}
    }
  else
    {
      nUserCompartments = compartment_list->getLength();
    }

  // #of equilibrim compartments. Default = 0.
  unsigned int nEquilibrims = 0;
  if( comp_model->hasAttribute( XML.X_NEQUILIBRIMS ) )
    {
      const XMLCh* x_nequilibrims = comp_model->getAttribute( XML.X_NEQUILIBRIMS );
      if( ! XMLString::textToBin( x_nequilibrims, nEquilibrims ) )
	{
          char mess[ SpkCompilerError::maxMessageLen() ];
          snprintf( mess,
		    SpkCompilerError::maxMessageLen(),
		    "Invalid <%s::%s> attribute value: \"%s\"", XML.C_COMP_MODEL, XML.C_NEQUILIBRIMS,
		    XMLString::transcode(x_nequilibrims) );
          SpkCompilerException e( SpkCompilerError::ASPK_USER_ERR, mess, __LINE__, __FILE__);
          throw e;
	} 
    }

  unsigned int nParameters = 0; 
  if( comp_model->hasAttribute( XML.X_NPARAMETERS ) )
    {
      const XMLCh* x_nparameters = comp_model->getAttribute( XML.X_NPARAMETERS );
      if( ! XMLString::textToBin( x_nparameters, nParameters ) )
	{
          char mess[ SpkCompilerError::maxMessageLen() ];
          snprintf( mess,
		    SpkCompilerError::maxMessageLen(),
		    "Invalid <%s::%s> attribute value: \"%s\"", XML.C_COMP_MODEL, XML.C_NPARAMETERS,
		    XMLString::transcode(x_nparameters) );
          SpkCompilerException e( SpkCompilerError::ASPK_USER_ERR, mess, __LINE__, __FILE__);
          throw e;
	} 
    }

  // Compartments
  bool tempVal = false;
  myCompModel = new CompModelInfo( nUserCompartments+1 /* Including the output compartment */, 
				   nParameters, 
				   nEquilibrims,
				   relTol );
  
  for( int i=0; i<nUserCompartments; i++ )
    {
      DOMElement* compartment = dynamic_cast<DOMElement*>( compartment_list->item(i) );
      if( compartment->hasAttribute( XML.X_NAME ) )
	myCompModel->getCompartment(i).setName( XMLString::transcode( compartment->getAttribute( XML.X_NAME ) ) );
      if( compartment->hasAttribute( XML.X_INITIAL_OFF ) )
	{
	  const XMLCh * yn = compartment->getAttribute( XML.X_INITIAL_OFF );
	  if( XMLString::equals( yn, XML.X_YES ) )
	    tempVal = true;
	  else
	    tempVal = false;
	  myCompModel->getCompartment(i).set_initial_off( tempVal );
	}
      if( compartment->hasAttribute( XML.X_NO_OFF ) )
	{
	  const XMLCh * yn = compartment->getAttribute( XML.X_NO_OFF );
	  if( XMLString::equals( yn, XML.X_YES ) )
	    tempVal = true;
	  else
	    tempVal = false;
	  myCompModel->getCompartment(i).set_no_off( tempVal );
	}
      if( compartment->hasAttribute( XML.X_NO_DOSE ) )
	{
	  const XMLCh * yn = compartment->getAttribute( XML.X_NO_DOSE );
	  if( XMLString::equals( yn, XML.X_YES ) )
	    tempVal = true;
	  else
	    tempVal = false;
	  myCompModel->getCompartment(i).set_no_dose( tempVal );
	}
      if( compartment->hasAttribute( XML.X_EQUILIBRIM ) )
	{
	  const XMLCh * yn = compartment->getAttribute( XML.X_EQUILIBRIM );
	  if( XMLString::equals( yn, XML.X_YES ) )
	    tempVal = true;
	  else
	    tempVal = false;
	  myCompModel->getCompartment(i).set_equilibrim( tempVal );
	}
      if( compartment->hasAttribute( XML.X_EXCLUDE ) )
	{
	  const XMLCh * yn = compartment->getAttribute( XML.X_EXCLUDE );
	  if( XMLString::equals( yn, XML.X_YES ) )
	    tempVal = true;
	  else
	    tempVal = false;
	  myCompModel->getCompartment(i).set_exclude( tempVal );
	}
      if( compartment->hasAttribute( XML.X_DEFAULT_OBSERVATION ) )
	{
	  const XMLCh * yn = compartment->getAttribute( XML.X_DEFAULT_OBSERVATION );
	  if( XMLString::equals( yn, XML.X_YES ) )
	    tempVal = true;
	  else
	    tempVal = false;
	  myCompModel->getCompartment(i).set_default_observation( tempVal );
	}
      if( compartment->hasAttribute( XML.X_DEFAULT_DOSE ) )
	{
	  const XMLCh * yn = compartment->getAttribute( XML.X_DEFAULT_DOSE );
	  if( XMLString::equals( yn, XML.X_YES ) )
	    tempVal = true;
	  else
	    tempVal = false;
	  myCompModel->getCompartment(i).set_default_dose( tempVal );
	}  
    }
  return nUserCompartments + 1;
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

  // REVISIT Sachiko 08/05/2005
  //
  // This value is assumed to be the same as NPARAMETERS given in $MODEL.
  // Maybe wrong...
  int nP_elements = countStrInLhs( nonmem::P.c_str(), c_pk_def );

  nm_in = fopen( fPkEqn_fortran, "w" );
  fprintf( nm_in, "%s", c_pk_def );
  fclose( nm_in );
  delete c_pk_def;

  nm_in = fopen( fPkEqn_fortran, "r" );
  gSpkExpOutput = fopen( fPkEqn_cpp, "w" );
  gSpkExpSymbolTable = table;
  gSpkExpErrors = 0;
  gSpkExpLines  = 0;
  gSpkIsTInRhs  = false;
  gSpkExpErrorMessages = new char[ SpkCompilerError::maxMessageLen()-50 ];
  strcpy( gSpkExpErrorMessages, "" );

  nm_parse();

  fclose( nm_in );
  fclose( gSpkExpOutput );
  if( gSpkExpErrors > 0 )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"Syntax error(s) found in <pk> definition.\n%s", 
		gSpkExpErrorMessages );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, 
			      m, __LINE__, __FILE__ );
      throw e;
    }
  // Find out if PK block is a function of the continuous time.
  // (T appears on the right hand side?)
  //
  myCompModel->setPkFunctionOfT( gSpkIsTInRhs );

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

  int nCompsInDES = countStrInLhs( nonmem::DADT.c_str(), c_des_def );
  if( nCompsInDES != myCompModel->getNCompartments()-1 )
    {
      char m[ SpkCompilerError::maxMessageLen() ];
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"The number of DADT elements appear on the left hand side of equations does not match with the number given in SpkSourceML document." );
      throw SpkCompilerException( SpkCompilerError::ASPK_USER_ERR, m, __LINE__, __FILE__ );
    }
  
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
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"Syntax error(s) found in <diffeqn> definition.\n%s", 
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
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"Syntax error(s) found in <error> definition.\n%s", 
		gSpkExpErrorMessages );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, 
			      m, __LINE__, __FILE__ );
      throw e;
    }
  remove( fErrorEqn_fortran );
}
