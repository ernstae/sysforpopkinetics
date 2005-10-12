#include <fstream>
#include "NonmemTranslator.h"
#include "SpkCompilerException.h"
#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

enum NonmemTranslator::MODEL_SPEC NonmemTranslator::detModelType()
{
  DOMElement  * spksouce = getSourceTree()->getDocumentElement();
  DOMNodeList * nonmems  = spksouce->getElementsByTagName( X_NONMEM );
  if( !nonmems->getLength() > 0 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s> element.", C_NONMEM );
      SpkCompilerException e ( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  DOMElement * nonmem = dynamic_cast<DOMElement*>( nonmems->item(0) );

  DOMNodeList * models = nonmem->getElementsByTagName( X_MODEL );
  if( models->getLength() != 1 )
    {
      char mess[ SpkCompilerError::maxMessageLen() ];
      sprintf( mess, "Missing <%s> element.", C_MODEL );
      SpkCompilerException e( SpkCompilerError::ASPK_SOURCEML_ERR, mess, __LINE__, __FILE__ );
      throw e;
    }

  DOMElement  * model = dynamic_cast<DOMElement*>( models->item(0) );

  enum MODEL_SPEC modelSpec;
  unsigned int advan;

  //
  // All ADVANs fit to the compartmental modeling framework.
  //
  if( model->hasAttribute( X_ADVAN ) )
    {
      XMLString::textToBin( model->getAttribute( X_ADVAN ), advan );
      assert( advan > 0 );

      modelSpec = static_cast<MODEL_SPEC>( advan );
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

      modelSpec = PRED;
    }
  return modelSpec;
}
