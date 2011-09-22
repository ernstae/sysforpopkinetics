/**
 * @file NM_parsePred.cpp
 * Define NonmemTranslator::parsePred().
 */
#include <fstream>

#include "NonmemTranslator.h"
#include "SpkCompilerException.h"
#include "explang.h"

#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

void NonmemTranslator::parsePred( const DOMElement * pred )
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
      snprintf( m, 
		SpkCompilerError::maxMessageLen(),
		"Syntax error(s) found in PRED definition.\n%s", 
		gSpkExpErrorMessages );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, 
			      m, __LINE__, __FILE__ );
      throw e;
    }

  fclose( nm_in );
  fclose( gSpkExpOutput );
  remove( fPredEqn_fortran );
}
