#include <iostream>
#include <fstream>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/framework/StdOutFormatTarget.hpp>

#include "read_nonmem_model.h"
#include "NonmemTranslator.h"
#include "../SymbolTable.h"
#include "../SpkCompilerUtil.h"
#include "../ExpTreeGenerator.h"
#include "explang.h"

using namespace std;
using namespace xercesc;

static void pred(   enum nonmem::TRANS trans,
		    xercesc::DOMElement* modelNode,
		    int nIndividuals,
		    SymbolTable * table );
static void advan2( enum nonmem::TRANS trans,
		    xercesc::DOMElement* modelNode,
		    int nIndividuals,
		    SymbolTable * table );

std::pair<enum nonmem::MODEL,
	  enum nonmem::TRANS>
   read_nonmem_model( xercesc::DOMElement * modelNode, 
		      int nIndividuals, 
		      SymbolTable * table )
{
  DOMDocument * doc = modelNode->getOwnerDocument();
  DOMTreeWalker * walker = doc->createTreeWalker(  modelNode,
                                                   DOMNodeFilter::SHOW_ELEMENT,
                                                   NULL,
                                                   false
						   );


  //
  // <model> has two optional attributes:
  // 
  // base={advan1:advan12}
  // tolerance={1:n}, where n could be any integer.
  //
  const XMLCh* xml_base = modelNode->getAttribute( X("base") );
  const XMLCh* xml_parameterization = modelNode->getAttribute( X("parameterization") );
  const XMLCh* xml_tol  = modelNode->getAttribute( X("tolerance") );

  pair<enum nonmem::MODEL, 
       enum nonmem::TRANS>
       model_type;
  enum nonmem::MODEL base = nonmem::NONE;
  enum nonmem::TRANS tran  = nonmem::DEFAULT;
  int tol = 1;

  if( !XMLString::isAllWhiteSpace(xml_base) && xml_base != NULL )
    {
      const char * c_base = C(xml_base);
      base = nonmem::toEnumMODEL( c_base );
    }
  if( !XMLString::isAllWhiteSpace( xml_parameterization ) && xml_parameterization != NULL )
    { 
      const char * c_tran = C(xml_parameterization);
      tran = nonmem::toEnumTRANS( c_tran );
    }
  if( xml_tol != NULL )
    {
      tol = atoi( C(xml_tol) );
    }
  model_type.first  = base;
  model_type.second = tran;

  if( base == nonmem::NONE )
    {
      pred( tran, modelNode, nIndividuals, table );
    }
  else if( base == nonmem::ADVAN2 )
    {
      advan2( tran, modelNode, nIndividuals, table );
    }
  else
    {
      fprintf( stderr, "%s is not supported!", 
	       nonmem::toStringMODEL( base ) );
      exit(-1);
    }

  return model_type;
}
void pred(   enum nonmem::TRANS,
	     xercesc::DOMElement* modelNode,
	     int nIndividuals,
	     SymbolTable * table )
{
}
void advan2( enum nonmem::TRANS trans,
	     xercesc::DOMElement* modelNode,
	     int nIndividuals,
	     SymbolTable * table )
{
  //
  // <pk> and <error> pair or <pred> must follow.
  //
  DOMImplementation * impl 
    = DOMImplementationRegistry::getDOMImplementation(XMLString::transcode("Core"));
  
  DOMDocument * doc = modelNode->getOwnerDocument();
  DOMTreeWalker * walker = doc->createTreeWalker(  modelNode,
                                                   DOMNodeFilter::SHOW_ELEMENT,
                                                   NULL,
                                                   false

						   );
  ExpTreeGenerator expUtils;

  bool isPk    = false;
  bool isError = false;
  bool isPred  = false;
  DOMElement * pk_error_pred = dynamic_cast<DOMElement*>( walker->firstChild() );
  while( pk_error_pred != NULL )
    {
      const XMLCh* nodeName = pk_error_pred->getNodeName();
      if( XMLString::equals( nodeName, X( "pk" ) ) )
	{
	  isPk = true;
	  const DOMElement * pk = pk_error_pred;

	  //
	  // Get the set of equations defining the model for estimating DV (y).
	  //
	  char pk_filename[] = "pk.eqn";
	  const XMLCh* pk_def = pk->getFirstChild()->getNodeValue();
	  XMLString::lowerCase( const_cast<XMLCh*>( pk_def ) );
	  ofstream o( pk_filename );
	  if( !o )
	    {
	      cerr << "Error opening a file <" << pk_filename << ">" << endl;
	    }
	  o << C(pk_def) << endl;
	  o.close();

	  FILE * pPK_handler = fopen( pk_filename, "r" );
	  assert( pPK_handler );
	  yyin = pPK_handler;

          FILE * pPK_output = fopen( "pk_output_filename", "w" );
	  gSpkExpErrors = 0;
	  gSpkExpLines  = 0;
          gSpkExpSymbolTable = table;
          gSpkExpOutput = pPK_output;
	  if( trans == nonmem::TRANS1 )
	    {
	      //
	      //  This is the default parameterization for ADVAN2
	      //
	      // Required PK parameters:
	      // K, KA
	      //
	      Symbol k ( "k",  Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol ka( "ka", Symbol::SCALAR, Symbol::DOUBLE, true );
              table->insert( k );
              table->insert( ka );
	    
	      //
	      // Parse the equations.
	      // The parser must cross check if the required parameters
	      // are defined (appear on the left hand side of equations).
	      //             
	    }
	  else if( trans == nonmem::TRANS2 || trans == nonmem::DEFAULT )
	    {
	      //
	      // Required PK Parameters:
	      //
	      // CL:        clearance, 
	      // V:         volume of distribution, 
	      // K = CL/V:  rate constant of elimination
	      // KA:        rate constant of absorption
	      //
              Symbol cl    ( "cl",     Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol v     ( "v",      Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol k     ( "k",      Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol ka    ( "ka",     Symbol::SCALAR, Symbol::DOUBLE, true );
              table->insert( cl );
              table->insert( v );
	      table->insert( k );
	      table->insert( ka );
	      
	      //
	      // Additional PK Parameters:
	      // (numbers correspond to compartments)
	      // (1: input/Absorption compartment)
	      // (2: Central/observation compartment)
	      // (3: Output/Elimiation compartment)
	      //
	      // S1, S2=SC, S3=SO, 
	      // F1, F2, 
	      // R1, R2,
	      // D1, D2, 
	      // ALAG1, ALAG2, 
	      // F0=F3=FO, 
	      // XSCALE
	      // 
  	      Symbol s1    ( "s1",     Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol s2    ( "s2",     Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol s3    ( "s3",     Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol sO    ( "sO",     Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol f1    ( "f1",     Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol f2    ( "f2",     Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol r1    ( "r1",     Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol r2    ( "r2",     Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol d1    ( "d1",     Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol d2    ( "d2",     Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol alag1 ( "alag1",  Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol alag2 ( "alag2",  Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol f0    ( "f0",     Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol f3    ( "f3",     Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol fO    ( "fO",     Symbol::SCALAR, Symbol::DOUBLE, true );
              Symbol xscale( "xscale", Symbol::SCALAR, Symbol::DOUBLE, true );
	      table->insert( s1 );
	      table->insert( s2 );
	      table->insert( s3 );
	      table->insert( sO );
	      table->insert( f1 );
	      table->insert( f2 );
	      table->insert( r1 );
	      table->insert( r2 );
	      table->insert( d1 );
	      table->insert( d2 );
	      table->insert( alag1 );
	      table->insert( alag2 );
	      table->insert( f0 );
	      table->insert( f3 );
	      table->insert( fO );
	      table->insert( xscale );
	    }
	  else
	    {
	      cerr << "Invalid TRANS: ";
	      cerr << nonmem::toStringTRANS( trans );
	      cerr << "." << endl;
	      exit(-1);
	    }

	  yyparse();

	  fclose(pPK_handler);
          fclose(pPK_output);
	    
          /*   
	  try{
	      remove( pk_filename );
	  }
	  catch( ... )
	    {
	      clog << "Warning: Failed to remove a temporary file <";
	      clog << pk_filename << ">" << endl;
	    }
	  */
	}
      else if( XMLString::equals( nodeName, X( "error" ) ) )
	{
	  isError = true;
	  // Y = { F, ETA }
	}
      else if( XMLString::equals( nodeName, X( "pred" ) ) )
	{
	  isPred = true;
	  pred( trans, modelNode,  nIndividuals, table );
	}
      else
        {
	  cerr << "Unrecoginized tag <" << C(nodeName) << ">" << endl;
	  exit(-1);
        }

      pk_error_pred = dynamic_cast<DOMElement*>( walker->nextSibling() );
    }
    if( !(isPk && isError) || isPred )
    {
      cerr << "Specification incomplete." << endl;
      cerr << " If not <pred>, the pair of <pk> and <error> must appear." << endl;
      exit(-1);
    }
}
